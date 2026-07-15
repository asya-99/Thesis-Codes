# =============================================================================
# PUSH & PULL — Trade Flow Evidence of Carbon Leakage
#
# PUSH (OECD/EPS countries):
#   Does domestic EXPORT fall as OBS_VALUE (EPS) rises?
#   Data: COMPLETE.xlsx — _Domestic columns = domestic exports by industry
#   Policy var: OBS_VALUE (EPS)
#   Sample: your 40 EPS countries
#
# PULL (non-OECD countries):
#   Do foreign IMPORTS rise as EPI score is lower?
#   Data: COMPLETE_IMP.xlsx — _Foreign columns = foreign imports by industry
#   Policy var: EPI_Mitigation_Score
#   Sample: countries NOT in the EPS list
#
# Both models: Two-Way FE, controls GDP + TRADE, t and t-1
# Output: Push_Pull_Trade.xlsx
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. LOAD BOTH FILES
# =============================================================================
df_exp <- read_excel("R_codes/COMPLETE.xlsx",     sheet = "Sheet1")
df_imp <- read_excel("R_codes/COMPLETE_IMP.xlsx", sheet = "Sheet1")

# Numeric conversions — export file
df_exp$OBS_VALUE            <- as.numeric(df_exp$OBS_VALUE)
df_exp$EPI_Mitigation_Score <- as.numeric(df_exp$EPI_Mitigation_Score)
df_exp$GDP                  <- as.numeric(df_exp$GDP)
df_exp$TRADE                <- as.numeric(df_exp$TRADE)
df_exp$TIME_PERIOD          <- as.numeric(df_exp$TIME_PERIOD)

# Numeric conversions — import file
df_imp$OBS_VALUE            <- as.numeric(df_imp$OBS_VALUE)
df_imp$EPI_Mitigation_Score <- as.numeric(df_imp$EPI_Mitigation_Score)
df_imp$GDP                  <- as.numeric(df_imp$GDP)
df_imp$TRADE                <- as.numeric(df_imp$TRADE)
df_imp$TIME_PERIOD          <- as.numeric(df_imp$TIME_PERIOD)

# Detect industry columns
exp_cols <- names(df_exp)[grepl("_(Domestic|Foreign)$", names(df_exp))]
imp_cols <- names(df_imp)[grepl("_(Domestic|Foreign)$", names(df_imp))]

for (col in exp_cols) df_exp[[col]] <- as.numeric(df_exp[[col]])
for (col in imp_cols) df_imp[[col]] <- as.numeric(df_imp[[col]])

cat("Export file: ", nrow(df_exp), "rows |", length(unique(df_exp$REF_AREA)),
    "countries |", length(exp_cols), "industry columns\n")
cat("Import file: ", nrow(df_imp), "rows |", length(unique(df_imp$REF_AREA)),
    "countries |", length(imp_cols), "industry columns\n")

# =============================================================================
# 2. SPLIT INTO PUSH (EPS countries) AND PULL (rest)
# =============================================================================
eps_countries <- c(
  "AUS","AUT","BEL","BRA","CAN","CHE","CHL","CHN","CZE","DEU",
  "DNK","ESP","EST","FIN","FRA","GBR","GRC","HUN","IDN","IND",
  "IRL","ISL","ISR","ITA","JPN","KOR","LUX","MEX","NLD","NOR",
  "NZL","POL","PRT","RUS","SVK","SVN","SWE","TUR","USA","ZAF"
)

# PUSH sample: EPS countries from export file, domestic columns only
push_countries <- intersect(unique(df_exp$REF_AREA), eps_countries)
df_push        <- df_exp[df_exp$REF_AREA %in% push_countries, ]

# PULL sample: non-EPS countries from import file, foreign columns only
pull_countries <- setdiff(unique(df_imp$REF_AREA), eps_countries)
df_pull        <- df_imp[df_imp$REF_AREA %in% pull_countries, ]

cat("\nPush sample:", length(push_countries), "countries:",
    paste(sort(push_countries), collapse=", "), "\n")
cat("\nPull sample:", length(pull_countries), "countries:",
    paste(sort(pull_countries), collapse=", "), "\n")

# =============================================================================
# 3. CREATE LAGS WITHIN EACH SAMPLE
# =============================================================================
lag_within <- function(x, group) {
  out <- rep(NA, length(x))
  for (g in unique(group)) {
    idx      <- which(group == g)
    out[idx] <- c(NA, x[idx[-length(idx)]])
  }
  out
}

add_lags <- function(d, policy_col) {
  d <- d[order(d$REF_AREA, d$TIME_PERIOD), ]
  d[[paste0(policy_col, "_lag1")]] <- lag_within(d[[policy_col]], d$REF_AREA)
  d
}

df_push <- add_lags(df_push, "OBS_VALUE")
df_pull <- add_lags(df_pull, "EPI_Mitigation_Score")

cat("\nPush rows with lagged OBS_VALUE:", sum(!is.na(df_push$OBS_VALUE_lag1)), "\n")
cat("Pull rows with lagged EPI score: ", sum(!is.na(df_pull$EPI_Mitigation_Score_lag1)), "\n")

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
# 5. TWO-WAY FIXED EFFECTS FUNCTION
# =============================================================================
twoway_fe <- function(y, policy, gdp, trade, country, year) {

  keep <- !is.na(y) & !is.na(policy) & !is.na(gdp) & !is.na(trade) &
          !is.na(country) & !is.na(year) & y > 0
  y       <- y[keep];       policy  <- policy[keep]
  gdp     <- gdp[keep];     trade   <- trade[keep]
  country <- country[keep]; year    <- year[keep]
  n       <- length(y)

  if (n < 30)                        return(NULL)
  if (length(unique(country)) < 3)   return(NULL)

  y   <- log(y)
  gdp <- log(gdp + 1)

  within_transform <- function(x) {
    grand  <- mean(x)
    c_mean <- ave(x, country, FUN = mean)
    y_mean <- ave(x, year,    FUN = mean)
    x - c_mean - y_mean + grand
  }

  y_w      <- within_transform(y)
  policy_w <- within_transform(policy)
  gdp_w    <- within_transform(gdp)
  trade_w  <- within_transform(trade)

  fit <- tryCatch(lm(y_w ~ policy_w + gdp_w + trade_w - 1), error = function(e) NULL)
  if (is.null(fit)) return(NULL)

  X       <- cbind(policy_w, gdp_w, trade_w)
  e       <- residuals(fit)
  XtX_inv <- tryCatch(solve(t(X) %*% X), error = function(e) NULL)
  if (is.null(XtX_inv)) return(NULL)

  k <- ncol(X)
  B <- matrix(0, k, k)
  for (cc in unique(country)) {
    idx <- country == cc
    Xc  <- X[idx, , drop = FALSE]
    ec  <- e[idx]
    sc  <- t(Xc) %*% ec
    B   <- B + sc %*% t(sc)
  }
  G   <- length(unique(country))
  adj <- (G / (G - 1)) * ((n - 1) / (n - k))
  V   <- adj * XtX_inv %*% B %*% XtX_inv

  stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                       ifelse(p < 0.05,  "*",   ifelse(p < 0.10, ".", "ns"))))

  get_coef <- function(i, name) {
    cv <- coef(fit)[name]
    se <- sqrt(V[i, i])
    tv <- cv / se
    pv <- 2 * pt(-abs(tv), df = G - 1)
    list(coef = round(cv, 4), se = round(se, 4),
         t = round(tv, 3), p = round(pv, 4), sig = stars(pv))
  }

  list(
    policy = get_coef(1, "policy_w"),
    gdp    = get_coef(2, "gdp_w"),
    trade  = get_coef(3, "trade_w"),
    r2     = round(summary(fit)$r.squared, 3),
    n      = n, G = G
  )
}

# =============================================================================
# 6. RUN MODEL FUNCTION
#    trade_type: "Domestic" for push (exports), "Foreign" for pull (imports)
# =============================================================================
run_model <- function(df_sub, industry_col_names, policy_var, trade_type) {
  industry_codes <- unique(sub("_(Domestic|Foreign)$", "", industry_col_names))
  out <- data.frame()

  for (ind in industry_codes) {
    col_name <- paste0(ind, "_", trade_type)
    if (!col_name %in% names(df_sub)) next

    y       <- df_sub[[col_name]]
    policy  <- df_sub[[policy_var]]
    gdp     <- df_sub$GDP
    trade   <- df_sub$TRADE
    country <- df_sub$REF_AREA
    year    <- df_sub$TIME_PERIOD
    label   <- ifelse(ind %in% names(industry_labels), industry_labels[ind], ind)

    res <- twoway_fe(y, policy, gdp, trade, country, year)
    if (is.null(res)) next

    out <- rbind(out, data.frame(
      Industry    = ind, Description = label,
      coef_policy = res$policy$coef, se_policy = res$policy$se,
      t_policy    = res$policy$t,    p_policy  = res$policy$p,
      sig_policy  = res$policy$sig,
      coef_GDP    = res$gdp$coef,    sig_GDP   = res$gdp$sig,
      coef_TRADE  = res$trade$coef,  sig_TRADE = res$trade$sig,
      R2_within   = res$r2, n_obs = res$n, n_countries = res$G,
      stringsAsFactors = FALSE
    ))
  }
  out
}

# =============================================================================
# 7. BUILD SIDE-BY-SIDE TABLE (t vs t-1)
# =============================================================================
build_table <- function(res_t, res_l1, label_suffix) {
  if (nrow(res_t) == 0 && nrow(res_l1) == 0) return(data.frame())
  inds <- union(res_t$Industry, res_l1$Industry)
  out  <- data.frame()
  for (ind in inds) {
    t_row  <- res_t [res_t$Industry  == ind, ]
    l_row  <- res_l1[res_l1$Industry == ind, ]
    label  <- if (nrow(t_row) > 0) t_row$Description[1] else l_row$Description[1]
    row <- data.frame(
      Industry                        = ind,
      Description                     = label,
      # Policy coefficient
      coef_t                          = if (nrow(t_row) > 0) t_row$coef_policy else NA,
      sig_t                           = if (nrow(t_row) > 0) t_row$sig_policy  else NA,
      n_t                             = if (nrow(t_row) > 0) t_row$n_obs       else NA,
      coef_l1                         = if (nrow(l_row) > 0) l_row$coef_policy else NA,
      sig_l1                          = if (nrow(l_row) > 0) l_row$sig_policy  else NA,
      n_l1                            = if (nrow(l_row) > 0) l_row$n_obs       else NA,
      # GDP control
      coef_GDP_t                      = if (nrow(t_row) > 0) t_row$coef_GDP    else NA,
      sig_GDP_t                       = if (nrow(t_row) > 0) t_row$sig_GDP     else NA,
      coef_GDP_l1                     = if (nrow(l_row) > 0) l_row$coef_GDP    else NA,
      sig_GDP_l1                      = if (nrow(l_row) > 0) l_row$sig_GDP     else NA,
      # TRADE control
      coef_TRADE_t                    = if (nrow(t_row) > 0) t_row$coef_TRADE  else NA,
      sig_TRADE_t                     = if (nrow(t_row) > 0) t_row$sig_TRADE   else NA,
      coef_TRADE_l1                   = if (nrow(l_row) > 0) l_row$coef_TRADE  else NA,
      sig_TRADE_l1                    = if (nrow(l_row) > 0) l_row$sig_TRADE   else NA,
      stringsAsFactors                = FALSE
    )
    out <- rbind(out, row)
  }
  if (nrow(out) > 0 && "coef_l1" %in% names(out)) {
    sort_key <- suppressWarnings(as.numeric(out$coef_l1))
    out <- out[order(sort_key, na.last = TRUE), ]
  }
  out
}

# =============================================================================
# 8. RUN PUSH MODEL (EPS countries, domestic exports, policy = OBS_VALUE)
# =============================================================================
cat("\n=== PUSH MODEL (EPS countries, domestic exports) ===\n")
cat("Running OBS_VALUE(t)...\n")
push_t  <- run_model(df_push, exp_cols, "OBS_VALUE",      "Domestic")
cat("Running OBS_VALUE(t-1)...\n")
push_l1 <- run_model(df_push, exp_cols, "OBS_VALUE_lag1", "Domestic")
push_table <- build_table(push_t, push_l1, "EPS")

cat("Industries with results:", nrow(push_table), "\n")

# =============================================================================
# 9. RUN PULL MODEL (non-EPS countries, foreign imports, policy = EPI score)
# =============================================================================
cat("\n=== PULL MODEL (non-EPS countries, foreign imports) ===\n")
cat("Running EPI_score(t)...\n")
pull_t  <- run_model(df_pull, imp_cols, "EPI_Mitigation_Score",      "Foreign")
cat("Running EPI_score(t-1)...\n")
pull_l1 <- run_model(df_pull, imp_cols, "EPI_Mitigation_Score_lag1", "Foreign")
pull_table <- build_table(pull_t, pull_l1, "EPI")

cat("Industries with results:", nrow(pull_table), "\n")

# =============================================================================
# 10. COMBINED PUSH vs PULL COMPARISON TABLE
# =============================================================================
build_comparison <- function(push_table, pull_table) {
  inds <- union(push_table$Industry, pull_table$Industry)
  out  <- data.frame()
  for (ind in inds) {
    p_row  <- push_table[push_table$Industry == ind, ]
    l_row  <- pull_table[pull_table$Industry == ind, ]
    label  <- if (nrow(p_row) > 0) p_row$Description[1] else l_row$Description[1]
    row <- data.frame(
      Industry          = ind,
      Description       = label,
      # PUSH: EPS effect on domestic exports
      Push_coef_t       = if (nrow(p_row) > 0) p_row$coef_t   else NA,
      Push_sig_t        = if (nrow(p_row) > 0) p_row$sig_t    else NA,
      Push_n_t          = if (nrow(p_row) > 0) p_row$n_t      else NA,
      Push_coef_l1      = if (nrow(p_row) > 0) p_row$coef_l1  else NA,
      Push_sig_l1       = if (nrow(p_row) > 0) p_row$sig_l1   else NA,
      Push_n_l1         = if (nrow(p_row) > 0) p_row$n_l1     else NA,
      Push_coef_GDP_t   = if (nrow(p_row) > 0) p_row$coef_GDP_t  else NA,
      Push_sig_GDP_t    = if (nrow(p_row) > 0) p_row$sig_GDP_t   else NA,
      Push_coef_TRADE_t = if (nrow(p_row) > 0) p_row$coef_TRADE_t else NA,
      Push_sig_TRADE_t  = if (nrow(p_row) > 0) p_row$sig_TRADE_t  else NA,
      # PULL: EPI effect on foreign imports
      Pull_coef_t       = if (nrow(l_row) > 0) l_row$coef_t   else NA,
      Pull_sig_t        = if (nrow(l_row) > 0) l_row$sig_t    else NA,
      Pull_n_t          = if (nrow(l_row) > 0) l_row$n_t      else NA,
      Pull_coef_l1      = if (nrow(l_row) > 0) l_row$coef_l1  else NA,
      Pull_sig_l1       = if (nrow(l_row) > 0) l_row$sig_l1   else NA,
      Pull_n_l1         = if (nrow(l_row) > 0) l_row$n_l1     else NA,
      Pull_coef_GDP_t   = if (nrow(l_row) > 0) l_row$coef_GDP_t  else NA,
      Pull_sig_GDP_t    = if (nrow(l_row) > 0) l_row$sig_GDP_t   else NA,
      Pull_coef_TRADE_t = if (nrow(l_row) > 0) l_row$coef_TRADE_t else NA,
      Pull_sig_TRADE_t  = if (nrow(l_row) > 0) l_row$sig_TRADE_t  else NA,
      stringsAsFactors  = FALSE
    )
    out <- rbind(out, row)
  }
  if (nrow(out) > 0) {
    sort_key <- suppressWarnings(as.numeric(out$Push_coef_l1))
    out <- out[order(sort_key, na.last = TRUE), ]
  }
  out
}

comparison <- build_comparison(push_table, pull_table)

cat("\n--- Push vs Pull comparison (fossil industries) ---\n")
fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")
if (nrow(comparison) > 0) {
  fossil_comp <- comparison[comparison$Industry %in% fossil_codes, ]
  show_cols <- intersect(
    c("Industry","Description",
      "Push_coef_t","Push_sig_t","Push_n_t",
      "Push_coef_l1","Push_sig_l1","Push_n_l1",
      "Pull_coef_t","Pull_sig_t","Pull_n_t",
      "Pull_coef_l1","Pull_sig_l1","Pull_n_l1"),
    names(fossil_comp))
  if (nrow(fossil_comp) > 0) print(fossil_comp[, show_cols])
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
    if (!is.na(val)) addStyle(wb, sheet, if (val < 0) neg else pos, rows = i+1, cols = ci)
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

push_coef_cols <- c("coef_t","coef_l1","coef_GDP_t","coef_GDP_l1",
                    "coef_TRADE_t","coef_TRADE_l1")
pull_coef_cols <- push_coef_cols
comp_coef_cols <- c("Push_coef_t","Push_coef_l1","Pull_coef_t","Pull_coef_l1",
                    "Push_coef_GDP_t","Pull_coef_GDP_t",
                    "Push_coef_TRADE_t","Pull_coef_TRADE_t")

add_sheet(wb, "Push (Dom Exports)",    push_table,  push_coef_cols)
add_sheet(wb, "Pull (For Imports)",    pull_table,  pull_coef_cols)
add_sheet(wb, "Push vs Pull",          comparison,  comp_coef_cols)
add_sheet(wb, "Fossil Only",
          if (nrow(comparison) > 0) comparison[comparison$Industry %in% fossil_codes, ]
          else data.frame(),
          comp_coef_cols)

# Country reference sheet
country_ref <- data.frame(
  Country = c(sort(push_countries), sort(pull_countries)),
  Group   = c(rep("Push — EPS countries (domestic exports)", length(push_countries)),
              rep("Pull — non-EPS countries (foreign imports)", length(pull_countries)))
)
addWorksheet(wb, "Country Groups")
writeData(wb, "Country Groups", country_ref, headerStyle = hs)
setColWidths(wb, "Country Groups", cols = 1:2, widths = "auto")

out_path <- "/Users/asyadachi/gent/R_codes/Push_Pull_Trade.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# KEY:
# PUSH model: log(Domestic_Export_it) ~ OBS_VALUE_it + log(GDP_it) + TRADE_it
#             + country_FE + year_FE   [EPS countries only]
#   Negative coef = stricter EPS associated with falling domestic exports
#   = evidence industry is being pushed out of strict-regulator countries
#
# PULL model: log(Foreign_Import_it) ~ EPI_Score_it + log(GDP_it) + TRADE_it
#             + country_FE + year_FE   [non-EPS countries only]
#   Negative coef = lower EPI score associated with more foreign imports
#   = evidence lax-regulator countries are pulling fossil industries in
#   (remember: lower EPI = weaker performance, so negative coef on EPI
#    means less strict = more imports = pull effect confirmed)
#
# LEAKAGE SIGNATURE:
#   Push_coef < 0 AND Pull_coef < 0 for same fossil industry
#   = exports fall in strict countries, imports rise in lax countries
#   = carbon leakage confirmed in that industry
# =============================================================================