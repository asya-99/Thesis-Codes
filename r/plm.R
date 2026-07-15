# =============================================================================
# PUSH & PULL — Trade Flow Evidence of Carbon Leakage
# Using plm package (two-way fixed effects, clustered SE via coeftest)
#
# PUSH: EPS countries — does domestic EXPORT fall as OBS_VALUE rises?
# PULL: non-EPS countries — do foreign IMPORTS rise as EPI score is lower?
# =============================================================================

# install.packages(c("readxl","plm","lmtest","sandwich","openxlsx"))
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(openxlsx)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
df_exp <- read_excel("./COMPLETE.xlsx",     sheet = "Sheet1")
df_imp <- read_excel("./COMPLETE_IMP.xlsx", sheet = "Sheet1")

# Numeric conversions
to_num <- function(df, cols) {
  for (col in cols) if (col %in% names(df)) df[[col]] <- as.numeric(df[[col]])
  df
}

num_cols <- c("OBS_VALUE","EPI_Mitigation_Score","GDP","TRADE","TIME_PERIOD")
df_exp <- to_num(df_exp, num_cols)
df_imp <- to_num(df_imp, num_cols)

# Detect industry columns
exp_cols <- names(df_exp)[grepl("_(Domestic|Foreign)$", names(df_exp))]
imp_cols <- names(df_imp)[grepl("_(Domestic|Foreign)$", names(df_imp))]

df_exp <- to_num(df_exp, exp_cols)
df_imp <- to_num(df_imp, imp_cols)

cat("Export file:", nrow(df_exp), "rows |",
    length(unique(df_exp$REF_AREA)), "countries |",
    length(exp_cols), "industry columns\n")
cat("Import file:", nrow(df_imp), "rows |",
    length(unique(df_imp$REF_AREA)), "countries |",
    length(imp_cols), "industry columns\n")

# =============================================================================
# 2. SPLIT INTO PUSH (EPS countries) AND PULL (rest)
# =============================================================================
eps_countries <- c(
  "AUS","AUT","BEL","BRA","CAN","CHE","CHL","CHN","CZE","DEU",
  "DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IDN","IND",
  "IRL","ISL","ISR","ITA","JPN","KOR","LUX","MEX","NLD","NOR",
  "NZL","POL","PRT","RUS","SVK","SVN","SWE","TUR","USA","ZAF"
)

push_countries <- intersect(unique(df_exp$REF_AREA), eps_countries)
pull_countries <- setdiff(unique(df_imp$REF_AREA),   eps_countries)

df_push <- df_exp[df_exp$REF_AREA %in% push_countries, ]
df_pull <- df_imp[df_imp$REF_AREA %in% pull_countries, ]

cat("\nPush countries (", length(push_countries), "):",
    paste(sort(push_countries), collapse = ", "), "\n")
cat("\nPull countries (", length(pull_countries), "):",
    paste(sort(pull_countries), collapse = ", "), "\n")

# =============================================================================
# 3. CREATE LAGS WITHIN EACH SAMPLE (same logic as your reference code)
# =============================================================================
lag_within <- function(x, group) {
  out <- rep(NA, length(x))
  for (g in unique(group)) {
    idx      <- which(group == g)
    out[idx] <- c(NA, x[idx[-length(idx)]])
  }
  out
}

df_push <- df_push[order(df_push$REF_AREA, df_push$TIME_PERIOD), ]
df_pull <- df_pull[order(df_pull$REF_AREA, df_pull$TIME_PERIOD), ]

# Lag the TRADE VALUES (exports/imports) within each country
# This tests whether policy this year affects trade NEXT year
# We create lagged versions of every industry column in each dataset
for (col in exp_cols) {
  df_push[[paste0(col, "_lag1")]] <- lag_within(df_push[[col]], df_push$REF_AREA)
}
for (col in imp_cols) {
  df_pull[[paste0(col, "_lag1")]] <- lag_within(df_pull[[col]], df_pull$REF_AREA)
}

# Create matching lag column name lists
exp_cols_lag <- paste0(exp_cols, "_lag1")
imp_cols_lag <- paste0(imp_cols, "_lag1")

cat("\nLagged export columns created:", length(exp_cols_lag), "\n")
cat("Lagged import columns created: ", length(imp_cols_lag), "\n")

# =============================================================================
# 4. INDUSTRY LABELS
# =============================================================================
industry_labels <- c(
  A01T03="Agriculture, forestry & fishing", B05T09="Mining & quarrying",
  C10T12="Food, beverages & tobacco", C13T15="Textiles & apparel",
  C16="Wood & wood products", C17T18="Paper & printing",
  C19="Coke & refined petroleum", C20="Chemicals & chemical products",
  C21="Pharmaceuticals", C22="Rubber & plastics",
  C23="Non-metallic mineral products", C24="Basic metals",
  C25="Fabricated metal products", C26="Computers & electronics",
  C27="Electrical equipment", C28="Machinery & equipment",
  C29="Motor vehicles", C30="Other transport equipment",
  C31T33="Other manufacturing", D35_E36T39="Electricity, gas, steam & water",
  F41T43="Construction", G45T47="Wholesale & retail trade",
  H49="Land transport", H50="Water transport", H51="Air transport",
  H52="Warehousing & support", H53="Postal & courier",
  I55T56="Accommodation & food services", J58T60="Publishing, TV & broadcasting",
  J61="Telecommunications", J62T63="IT & information services",
  K64T66="Financial & insurance", L68="Real estate",
  M69T75="Professional & technical services", N77T82="Admin & support services",
  O84="Public admin & defence", P85="Education",
  Q86T88="Health & social work", R90T93="Arts & entertainment",
  S94T96="Other service activities", T97T98="Households as employers"
)

# =============================================================================
# 5. PLM MODEL FUNCTION
# Mirrors your reference code:
#   pdata.frame(data, index = c("country","year"))
#   plm(y ~ policy + log(GDP) + TRADE, model="within", effect="twoways")
#   coeftest(model, vcov = vcovHC(model, cluster="group"))  <- clustered SE
# =============================================================================
run_plm <- function(df_sub, y_col, policy_col) {

  # Keep only rows with all required variables
  keep <- !is.na(df_sub[[y_col]]) &
          !is.na(df_sub[[policy_col]]) &
          !is.na(df_sub$GDP) &
          !is.na(df_sub$TRADE) &
          df_sub[[y_col]] > 0      # log() requires positive values

  d <- df_sub[keep, ]
  if (nrow(d) < 30)                       return(NULL)
  if (length(unique(d$REF_AREA)) < 3)     return(NULL)

  # Log-transform outcome and GDP
  d$ln_y    <- log(d[[y_col]])
  d$ln_GDP  <- log(d$GDP + 1)

  # Declare panel structure — mirrors pdata.frame(data, index=c("borough","year"))
  pdata <- tryCatch(
    pdata.frame(d, index = c("REF_AREA", "TIME_PERIOD")),
    error = function(e) NULL
  )
  if (is.null(pdata)) return(NULL)

  # Build formula — mirrors plm(ln_houseprice ~ cg_numbers + ln_income + ...)
  fml <- as.formula(paste("ln_y ~", policy_col, "+ ln_GDP + TRADE"))

  # Two-way fixed effects — mirrors model="within", effect="twoways"
  model <- tryCatch(
    plm(fml, data = pdata, model = "within", effect = "twoways"),
    error = function(e) NULL
  )
  if (is.null(model)) return(NULL)

  # Clustered standard errors by country group
  # mirrors coeftest(model, vcov=vcovHC(model, cluster="group"))
  ct <- tryCatch(
    coeftest(model, vcov = vcovHC(model, method = "arellano", cluster = "group")),
    error = function(e) tryCatch(coeftest(model), error = function(e) NULL)
  )
  if (is.null(ct)) return(NULL)

  # Extract policy coefficient row
  policy_row <- tryCatch(ct[policy_col, ], error = function(e) NULL)
  if (is.null(policy_row)) return(NULL)

  # Extract GDP and TRADE rows
  gdp_row   <- tryCatch(ct["ln_GDP", ], error = function(e) rep(NA, 4))
  trade_row <- tryCatch(ct["TRADE",   ], error = function(e) rep(NA, 4))

  stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                       ifelse(p < 0.05,  "*",   ifelse(p < 0.10, ".", "ns"))))

  list(
    coef_policy  = round(policy_row["Estimate"],    4),
    se_policy    = round(policy_row["Std. Error"],  4),
    t_policy     = round(policy_row["t value"],     3),
    p_policy     = round(policy_row["Pr(>|t|)"],    4),
    sig_policy   = stars(policy_row["Pr(>|t|)"]),
    coef_GDP     = round(gdp_row["Estimate"],        4),
    sig_GDP      = stars(gdp_row["Pr(>|t|)"]),
    coef_TRADE   = round(trade_row["Estimate"],      4),
    sig_TRADE    = stars(trade_row["Pr(>|t|)"]),
    r2           = round(summary(model)$r.squared["rsq"], 3),
    n            = nobs(model),
    G            = length(unique(d$REF_AREA))
  )
}

# =============================================================================
# 6. LOOP OVER INDUSTRIES
# =============================================================================
run_all_industries <- function(df_sub, industry_col_names,
                                policy_col, trade_type) {
  # Loop directly over column names — handles both C19_Domestic and C19_Domestic_lag1
  out <- data.frame()

  for (y_col in industry_col_names) {
    # Only process columns for the requested trade type
    pattern <- paste0("_", trade_type, "(_lag1)?$")
    if (!grepl(pattern, y_col)) next
    if (!y_col %in% names(df_sub)) next

    # Extract industry code by stripping _Domestic/_Foreign and optional _lag1
    ind   <- sub("_(Domestic|Foreign)(_lag1)?$", "", y_col)
    label <- ifelse(ind %in% names(industry_labels), industry_labels[ind], ind)
    res   <- run_plm(df_sub, y_col, policy_col)
    if (is.null(res)) next

    out <- rbind(out, data.frame(
      Industry    = ind,
      Description = label,
      coef_policy = res$coef_policy,  se_policy  = res$se_policy,
      t_policy    = res$t_policy,     p_policy   = res$p_policy,
      sig_policy  = res$sig_policy,
      coef_GDP    = res$coef_GDP,     sig_GDP    = res$sig_GDP,
      coef_TRADE  = res$coef_TRADE,   sig_TRADE  = res$sig_TRADE,
      R2          = res$r2,
      n_obs       = res$n,            n_countries = res$G,
      stringsAsFactors = FALSE
    ))
  }
  out
}

# =============================================================================
# 7. BUILD SIDE-BY-SIDE TABLE (t vs t-1)
# =============================================================================
build_table <- function(res_t, res_l1) {
  if (nrow(res_t) == 0 && nrow(res_l1) == 0) return(data.frame())
  inds <- union(res_t$Industry, res_l1$Industry)
  out  <- data.frame()

  for (ind in inds) {
    t_row <- res_t[res_t$Industry == ind, ]
    l_row <- res_l1[res_l1$Industry == ind, ]
    label <- if (nrow(t_row) > 0) t_row$Description[1] else l_row$Description[1]

    out <- rbind(out, data.frame(
      Industry        = ind,
      Description     = label,
      coef_t          = if (nrow(t_row) > 0) t_row$coef_policy else NA,
      sig_t           = if (nrow(t_row) > 0) t_row$sig_policy  else NA,
      n_t             = if (nrow(t_row) > 0) t_row$n_obs       else NA,
      coef_l1         = if (nrow(l_row) > 0) l_row$coef_policy else NA,
      sig_l1          = if (nrow(l_row) > 0) l_row$sig_policy  else NA,
      n_l1            = if (nrow(l_row) > 0) l_row$n_obs       else NA,
      coef_GDP_t      = if (nrow(t_row) > 0) t_row$coef_GDP    else NA,
      sig_GDP_t       = if (nrow(t_row) > 0) t_row$sig_GDP     else NA,
      coef_GDP_l1     = if (nrow(l_row) > 0) l_row$coef_GDP    else NA,
      sig_GDP_l1      = if (nrow(l_row) > 0) l_row$sig_GDP     else NA,
      coef_TRADE_t    = if (nrow(t_row) > 0) t_row$coef_TRADE  else NA,
      sig_TRADE_t     = if (nrow(t_row) > 0) t_row$sig_TRADE   else NA,
      coef_TRADE_l1   = if (nrow(l_row) > 0) l_row$coef_TRADE  else NA,
      sig_TRADE_l1    = if (nrow(l_row) > 0) l_row$sig_TRADE   else NA,
      stringsAsFactors = FALSE
    ))
  }

  if (nrow(out) > 0 && "coef_l1" %in% names(out)) {
    sort_key <- suppressWarnings(as.numeric(out$coef_l1))
    out <- out[order(sort_key, na.last = TRUE), ]
  }
  out
}

# =============================================================================
# 8. RUN PUSH MODELS (domestic exports, EPS countries, policy = OBS_VALUE)
# =============================================================================
# PUSH: policy -> exports THIS year (contemporaneous)
cat("\n=== PUSH: Export(t) ~ OBS_VALUE(t) ===\n")
push_t  <- run_all_industries(df_push, exp_cols,     "OBS_VALUE",            "Domestic")
cat("Results:", nrow(push_t), "industries\n")

# PUSH: policy -> exports NEXT year (lagged trade value)
cat("\n=== PUSH: Export(t+1) ~ OBS_VALUE(t) i.e. lag on export ===\n")
push_l1 <- run_all_industries(df_push, exp_cols_lag, "OBS_VALUE",            "Domestic")
cat("Results:", nrow(push_l1), "industries\n")

push_table <- build_table(push_t, push_l1)

# =============================================================================
# 9. RUN PULL MODELS (foreign imports, non-EPS countries, policy = EPI score)
# =============================================================================
# PULL: policy -> imports THIS year (contemporaneous)
cat("\n=== PULL: Import(t) ~ EPI_score(t) ===\n")
pull_t  <- run_all_industries(df_pull, imp_cols,     "EPI_Mitigation_Score", "Foreign")
cat("Results:", nrow(pull_t), "industries\n")

# PULL: policy -> imports NEXT year (lagged trade value)
cat("\n=== PULL: Import(t+1) ~ EPI_score(t) i.e. lag on import ===\n")
pull_l1 <- run_all_industries(df_pull, imp_cols_lag, "EPI_Mitigation_Score", "Foreign")
cat("Results:", nrow(pull_l1), "industries\n")

pull_table <- build_table(pull_t, pull_l1)

# =============================================================================
# 10. PUSH vs PULL COMPARISON TABLE
# =============================================================================
build_comparison <- function(push_table, pull_table) {
  if (nrow(push_table) == 0 && nrow(pull_table) == 0) return(data.frame())
  inds <- union(push_table$Industry, pull_table$Industry)
  out  <- data.frame()

  for (ind in inds) {
    p <- push_table[push_table$Industry == ind, ]
    l <- pull_table[pull_table$Industry == ind, ]
    label <- if (nrow(p) > 0) p$Description[1] else l$Description[1]

    out <- rbind(out, data.frame(
      Industry          = ind, Description = label,
      Push_coef_t       = if (nrow(p) > 0) p$coef_t       else NA,
      Push_sig_t        = if (nrow(p) > 0) p$sig_t        else NA,
      Push_n_t          = if (nrow(p) > 0) p$n_t          else NA,
      Push_coef_l1      = if (nrow(p) > 0) p$coef_l1      else NA,
      Push_sig_l1       = if (nrow(p) > 0) p$sig_l1       else NA,
      Push_n_l1         = if (nrow(p) > 0) p$n_l1         else NA,
      Push_coef_GDP_t   = if (nrow(p) > 0) p$coef_GDP_t   else NA,
      Push_sig_GDP_t    = if (nrow(p) > 0) p$sig_GDP_t    else NA,
      Push_coef_TRADE_t = if (nrow(p) > 0) p$coef_TRADE_t else NA,
      Push_sig_TRADE_t  = if (nrow(p) > 0) p$sig_TRADE_t  else NA,
      Pull_coef_t       = if (nrow(l) > 0) l$coef_t       else NA,
      Pull_sig_t        = if (nrow(l) > 0) l$sig_t        else NA,
      Pull_n_t          = if (nrow(l) > 0) l$n_t          else NA,
      Pull_coef_l1      = if (nrow(l) > 0) l$coef_l1      else NA,
      Pull_sig_l1       = if (nrow(l) > 0) l$sig_l1       else NA,
      Pull_n_l1         = if (nrow(l) > 0) l$n_l1         else NA,
      Pull_coef_GDP_t   = if (nrow(l) > 0) l$coef_GDP_t   else NA,
      Pull_sig_GDP_t    = if (nrow(l) > 0) l$sig_GDP_t    else NA,
      Pull_coef_TRADE_t = if (nrow(l) > 0) l$coef_TRADE_t else NA,
      Pull_sig_TRADE_t  = if (nrow(l) > 0) l$sig_TRADE_t  else NA,
      stringsAsFactors  = FALSE
    ))
  }

  if (nrow(out) > 0) {
    sort_key <- suppressWarnings(as.numeric(out$Push_coef_l1))
    out <- out[order(sort_key, na.last = TRUE), ]
  }
  out
}

comparison   <- build_comparison(push_table, pull_table)
fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")

# Console preview
cat("\n--- Push vs Pull (fossil industries) ---\n")
if (nrow(comparison) > 0) {
  fc <- comparison[comparison$Industry %in% fossil_codes, ]
  show <- intersect(c("Industry","Description",
                      "Push_coef_t","Push_sig_t","Push_n_t",
                      "Push_coef_l1","Push_sig_l1","Push_n_l1",
                      "Pull_coef_t","Pull_sig_t","Pull_n_t",
                      "Pull_coef_l1","Pull_sig_l1","Pull_n_l1"),
                    names(fc))
  if (nrow(fc) > 0) print(fc[, show])
}

# =============================================================================
# 11. EXPORT TO EXCEL
# =============================================================================
wb  <- createWorkbook()
hs  <- createStyle(fontColour = "#FFFFFF", bgFill = "#2F4F4F",
                   textDecoration = "bold", halign = "center")
neg <- createStyle(fontColour = "#C0392B", textDecoration = "bold")
pos <- createStyle(fontColour = "#1D6A40", textDecoration = "bold")

colour_col <- function(wb, sheet, data, col_name) {
  ci <- which(names(data) == col_name)
  if (length(ci) == 0) return()
  for (i in seq_len(nrow(data))) {
    val <- suppressWarnings(as.numeric(data[i, ci]))
    if (!is.na(val))
      addStyle(wb, sheet, if (val < 0) neg else pos, rows = i + 1, cols = ci)
  }
}

add_sheet <- function(wb, name, data, coef_cols) {
  addWorksheet(wb, name)
  if (is.null(data) || nrow(data) == 0) {
    writeData(wb, name, data.frame(Note = "No results — check sample size and column names"))
    return()
  }
  writeData(wb, name, data, headerStyle = hs)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
  for (cc in coef_cols) colour_col(wb, name, data, cc)
}

trade_coef_cols <- c("coef_t","coef_l1",
                     "coef_GDP_t","coef_GDP_l1",
                     "coef_TRADE_t","coef_TRADE_l1")
comp_coef_cols  <- c("Push_coef_t","Push_coef_l1",
                     "Pull_coef_t","Pull_coef_l1",
                     "Push_coef_GDP_t","Pull_coef_GDP_t",
                     "Push_coef_TRADE_t","Pull_coef_TRADE_t")

add_sheet(wb, "Push (Dom Exports)", push_table, trade_coef_cols)
add_sheet(wb, "Pull (For Imports)", pull_table, trade_coef_cols)
add_sheet(wb, "Push vs Pull",       comparison, comp_coef_cols)
add_sheet(wb, "Fossil Only",
          if (nrow(comparison) > 0) comparison[comparison$Industry %in% fossil_codes, ]
          else data.frame(),
          comp_coef_cols)

# Country reference
addWorksheet(wb, "Country Groups")
writeData(wb, "Country Groups",
          data.frame(
            Country = c(sort(push_countries), sort(pull_countries)),
            Group   = c(rep("Push — EPS countries (domestic exports)", length(push_countries)),
                        rep("Pull — non-EPS countries (foreign imports)", length(pull_countries)))
          ), headerStyle = hs)
setColWidths(wb, "Country Groups", cols = 1:2, widths = "auto")

out_path <- "./Push_Pull_Trade_lagged.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# MODEL STRUCTURE (mirrors your reference code):
#   pdata.frame(d, index = c("REF_AREA", "TIME_PERIOD"))
#   plm(ln_y ~ policy + ln_GDP + TRADE,
#       model = "within", effect = "twoways")
#   coeftest(model, vcov = vcovHC(model, method="arellano", cluster="group"))
#
# PUSH (t):   ln(Export_it)   ~ OBS_VALUE_it + ln(GDP_it) + TRADE_it
# PUSH (t+1): ln(Export_it+1) ~ OBS_VALUE_it + ln(GDP_it) + TRADE_it
#   [EPS countries] — negative coef = stricter EPS reduces exports
#   t+1 lag tests delayed trade response (e.g. after contracts/shipments)
#
# PULL (t):   ln(Import_it)   ~ EPI_Score_it + ln(GDP_it) + TRADE_it
# PULL (t+1): ln(Import_it+1) ~ EPI_Score_it + ln(GDP_it) + TRADE_it
#   [non-EPS countries] — negative coef = lower EPI = more imports next year
#   t+1 lag tests whether import surge takes time to materialise
#
# LEAKAGE SIGNATURE:
#   Push_coef < 0 (exports fall in strict countries)
#   Pull_coef < 0 (imports rise in lax countries — lower EPI = more imports)
#   Both significant for same fossil industry = carbon leakage confirmed
# =============================================================================