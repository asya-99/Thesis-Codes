# =============================================================================
# PPML (Poisson Pseudo Maximum Likelihood) with Two-Way Fixed Effects
# PUSH model: Domestic Exports ~ EPS + controls (OECD sample)
# PULL model: Foreign Imports  ~ EPI + controls (Full 77-country sample)
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. COUNTRY LISTS FOR ROBUSTNESS CHECKS
# =============================================================================

oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

eu_iso3 <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)

cat("OECD countries:", length(oecd_iso3), "\n")
cat("EU countries:",   length(eu_iso3),   "\n")

# =============================================================================
# 2. LOAD DATA
# NOTE: change filename to COMPLETE_EXPORT or COMPLETE_IMPORT as needed
# =============================================================================
df_export <- read_excel("COMPLETE_EXPORT.xlsx", sheet = "Sheet1")
df_import <- read_excel("COMPLETE_IMPORT.xlsx", sheet = "Sheet1")

# Standardise column names (trim whitespace)
names(df_export) <- trimws(names(df_export))
names(df_import) <- trimws(names(df_import))

# Force numeric
to_numeric_cols <- function(df) {
  skip <- c("REF_AREA", "TIME_PERIOD")
  for (col in names(df)[!names(df) %in% skip]) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
  df
}

df_export <- to_numeric_cols(df_export)
df_import <- to_numeric_cols(df_import)

cat("\nExport data — Rows:", nrow(df_export),
    "| Countries:", length(unique(df_export$REF_AREA)), "\n")
cat("Import data — Rows:", nrow(df_import),
    "| Countries:", length(unique(df_import$REF_AREA)), "\n")

# Get GVA industry codes
gva_cols    <- names(df_export)[grepl("_(Domestic|Foreign)$", names(df_export))]
industry_codes <- unique(sub("_(Domestic|Foreign)$", "", gva_cols))
cat("Industries:", length(industry_codes), "\n")

# =============================================================================
# 3. INDUSTRY LABELS
# =============================================================================
industry_labels <- c(
  A01T03="Agriculture, forestry & fishing", B05T09="Mining & quarrying",
  C10T12="Food, beverages & tobacco",       C13T15="Textiles & apparel",
  C16="Wood & wood products",               C17T18="Paper & printing",
  C19="Coke & refined petroleum",           C20="Chemicals & chemical products",
  C21="Pharmaceuticals",                    C22="Rubber & plastics",
  C23="Non-metallic mineral products",      C24="Basic metals",
  C25="Fabricated metal products",          C26="Computers & electronics",
  C27="Electrical equipment",               C28="Machinery & equipment",
  C29="Motor vehicles",                     C30="Other transport equipment",
  C31T33="Other manufacturing",             D35_E36T39="Electricity, gas, steam & water",
  F41T43="Construction",                    G45T47="Wholesale & retail trade",
  H49="Land transport",                     H50="Water transport",
  H51="Air transport",                      H52="Warehousing & support",
  H53="Postal & courier",                   I55T56="Accommodation & food services",
  J58T60="Publishing, TV & broadcasting",   J61="Telecommunications",
  J62T63="IT & information services",       K64T66="Financial & insurance",
  L68="Real estate",                        M69T75="Professional & technical services",
  N77T82="Admin & support services",        O84="Public admin & defence",
  P85="Education",                          Q86T88="Health & social work",
  R90T93="Arts & entertainment",            S94T96="Other service activities",
  T97T98="Households as employers"
)

# =============================================================================
# 4. PPML FUNCTION WITH TWO-WAY FIXED EFFECTS
#
# Two-way FE implemented via dummy variables for country and year
# (PPML cannot use the within/demeaning trick used in linear FE because
#  the log-link means demeaning does not cancel out the fixed effects)
#
# Standard errors clustered by country using sandwich estimator
#
# Returns: coefficient on policy variable, SE, z-stat, p-value, significance
# =============================================================================
ppml_twfe <- function(y, policy, gdp, trade, interest, oil_rents, fin_dev,
                      country, year) {

  keep <- !is.na(y) & !is.na(policy) & !is.na(gdp) & !is.na(trade) &
          !is.na(interest) & !is.na(oil_rents) & !is.na(fin_dev) &
          !is.na(country) & !is.na(year) & y >= 0

  y        <- y[keep];        policy   <- policy[keep]
  gdp      <- gdp[keep];      trade    <- trade[keep]
  interest <- interest[keep]; oil_rents<- oil_rents[keep]
  fin_dev  <- fin_dev[keep];  country  <- country[keep]
  year     <- year[keep]
  n        <- length(y)

  if (n < 20) return(NULL)
  if (length(unique(country)) < 3) return(NULL)

  # Scale continuous controls to avoid numerical issues
  gdp       <- log(gdp + 1)
  trade     <- scale(trade)[, 1]
  interest  <- scale(interest)[, 1]
  oil_rents <- scale(oil_rents)[, 1]
  fin_dev   <- scale(fin_dev)[, 1]

  # Build country and year dummies (drop one of each for identification)
  country_f <- factor(country)
  year_f    <- factor(year)
  country_f <- relevel(country_f, ref = levels(country_f)[1])
  year_f    <- relevel(year_f,    ref = levels(year_f)[1])

  X_controls <- model.matrix(~ gdp + trade + interest + oil_rents + fin_dev +
                               country_f + year_f - 1)
  X_policy   <- matrix(policy, ncol = 1, dimnames = list(NULL, "policy"))
  X          <- cbind(X_policy, X_controls)

  # PPML: Poisson GLM (log link, Poisson family)
  fit <- tryCatch(
    glm.fit(x = X, y = y, family = poisson(link = "log"),
            control = glm.control(maxit = 200, epsilon = 1e-8)),
    error = function(e) NULL
  )

  if (is.null(fit) || !fit$converged) return(NULL)

  # Clustered standard errors by country
  coefs   <- fit$coefficients
  resids  <- y - fit$fitted.values          # raw residuals (y - mu)
  score   <- X * resids                     # score matrix n x k
  XtWX    <- t(X) %*% diag(fit$fitted.values) %*% X  # Hessian approx
  XtWX_inv <- tryCatch(solve(XtWX), error = function(e) NULL)
  if (is.null(XtWX_inv)) return(NULL)

  k <- ncol(X)
  B <- matrix(0, k, k)
  for (ctry in unique(country)) {
    idx <- country == ctry
    sc  <- colSums(score[idx, , drop = FALSE])
    B   <- B + outer(sc, sc)
  }
  G   <- length(unique(country))
  adj <- G / (G - 1)
  V   <- adj * XtWX_inv %*% B %*% XtWX_inv

  coef_p <- coefs["policy"]
  se_p   <- sqrt(max(V[1, 1], 0))
  z_p    <- coef_p / se_p
  p_p    <- 2 * pnorm(-abs(z_p))

  stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                       ifelse(p < 0.05,  "*",   ifelse(p < 0.10, ".", "ns"))))

  list(
    coef   = round(coef_p, 4),
    se     = round(se_p,   4),
    z      = round(z_p,    3),
    p      = round(p_p,    4),
    sig    = stars(p_p),
    n      = n,
    G      = G
  )
}

# =============================================================================
# 5. RUN MODELS FUNCTION
# sample_filter: NULL = full sample, "OECD" = OECD only, "EU" = EU only
# =============================================================================
run_ppml <- function(df, policy_col, gva_type, sample_filter = NULL) {

  # Apply sample filter
  if (!is.null(sample_filter)) {
    country_list <- if (sample_filter == "OECD") oecd_iso3 else eu_iso3
    df <- df[df$REF_AREA %in% country_list, ]
  }

  out <- data.frame()
  for (ind in industry_codes) {

    dom_col <- paste0(ind, "_Domestic")
    for_col <- paste0(ind, "_Foreign")
    if (!dom_col %in% names(df) | !for_col %in% names(df)) next

    y <- if (gva_type == "Domestic") df[[dom_col]]
         else if (gva_type == "Foreign") df[[for_col]]
         else df[[dom_col]] + df[[for_col]]

    res <- ppml_twfe(
      y         = y,
      policy    = df[[policy_col]],
      gdp       = df$GDP,
      trade     = df$TRADE,
      interest  = df$INTEREST_PAYMENTS,
      oil_rents = df$OIL_RENTS,
      fin_dev   = df$FINANCIAL_DEVELOPMENT,
      country   = df$REF_AREA,
      year      = df$TIME_PERIOD
    )

    label <- ifelse(ind %in% names(industry_labels), industry_labels[ind], ind)

    if (!is.null(res)) {
      out <- rbind(out, data.frame(
        Industry    = ind,
        Description = label,
        GVA_type    = gva_type,
        Sample      = ifelse(is.null(sample_filter), "Full", sample_filter),
        coef_policy = res$coef,
        se_policy   = res$se,
        z_stat      = res$z,
        p_value     = res$p,
        sig         = res$sig,
        n_obs       = res$n,
        n_countries = res$G,
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# =============================================================================
# 6. BUILD RESULTS TABLE: PUSH (EPS, Domestic exports, OECD-based)
#    Three sample cuts for robustness
# =============================================================================
cat("\n--- PUSH MODEL: EPS x Domestic Exports ---\n")

push_full <- run_ppml(df_export, "EPS", "Domestic", sample_filter = NULL)
cat("Full sample done\n")

push_oecd <- run_ppml(df_export, "EPS", "Domestic", sample_filter = "OECD")
cat("OECD sample done\n")

push_eu   <- run_ppml(df_export, "EPS", "Domestic", sample_filter = "EU")
cat("EU sample done\n")

# Combine into one wide table per industry
build_wide <- function(full, oecd, eu, coef_label) {
  inds <- unique(full$Industry)
  out  <- data.frame()
  for (ind in inds) {
    label  <- full$Description[full$Industry == ind][1]
    f_row  <- full[full$Industry == ind, ]
    o_row  <- oecd[oecd$Industry == ind, ]
    e_row  <- eu  [eu$Industry   == ind, ]
    row <- data.frame(
      Industry    = ind,
      Description = label,
      # Full sample
      coef_Full   = if (nrow(f_row) > 0) f_row$coef_policy else NA,
      sig_Full    = if (nrow(f_row) > 0) f_row$sig         else NA,
      n_Full      = if (nrow(f_row) > 0) f_row$n_obs       else NA,
      # OECD only
      coef_OECD   = if (nrow(o_row) > 0) o_row$coef_policy else NA,
      sig_OECD    = if (nrow(o_row) > 0) o_row$sig         else NA,
      n_OECD      = if (nrow(o_row) > 0) o_row$n_obs       else NA,
      # EU only
      coef_EU     = if (nrow(e_row) > 0) e_row$coef_policy else NA,
      sig_EU      = if (nrow(e_row) > 0) e_row$sig         else NA,
      n_EU        = if (nrow(e_row) > 0) e_row$n_obs       else NA,
      stringsAsFactors = FALSE
    )
    out <- rbind(out, row)
  }
  out[order(out$coef_Full), ]   # most negative first
}

push_table <- build_wide(push_full, push_oecd, push_eu, "EPS")

# =============================================================================
# 7. BUILD RESULTS TABLE: PULL (EPI, Foreign imports, all countries)
# =============================================================================
cat("\n--- PULL MODEL: EPI x Foreign Imports ---\n")

pull_full <- run_ppml(df_import, "EPI", "Foreign", sample_filter = NULL)
cat("Full sample done\n")

pull_oecd <- run_ppml(df_import, "EPI", "Foreign", sample_filter = "OECD")
cat("OECD sample done\n")

pull_eu   <- run_ppml(df_import, "EPI", "Foreign", sample_filter = "EU")
cat("EU sample done\n")

pull_table <- build_wide(pull_full, pull_oecd, pull_eu, "EPI")

# =============================================================================
# 8. EXPORT TO EXCEL
# =============================================================================
wb  <- createWorkbook()
hs  <- createStyle(fontColour = "#FFFFFF", bgFill = "#2F4F4F",
                   textDecoration = "bold", halign = "center")
neg <- createStyle(fontColour = "#C0392B", textDecoration = "bold")
pos <- createStyle(fontColour = "#1D6A40", textDecoration = "bold")

colour_coef <- function(wb, sheet, data, col_names) {
  for (cn in col_names) {
    ci <- which(names(data) == cn)
    if (length(ci) == 0) next
    for (i in seq_len(nrow(data))) {
      val <- suppressWarnings(as.numeric(data[i, ci]))
      if (!is.na(val))
        addStyle(wb, sheet, if (val < 0) neg else pos, rows = i+1, cols = ci)
    }
  }
}

add_sheet <- function(wb, name, data, coef_cols) {
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle = hs)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
  colour_coef(wb, name, data, coef_cols)
}

fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")
coef_cols    <- c("coef_Full","coef_OECD","coef_EU")

# Push sheets
add_sheet(wb, "PUSH All Industries",    push_table,                            coef_cols)
add_sheet(wb, "PUSH Fossil Industries", push_table[push_table$Industry %in% fossil_codes, ], coef_cols)

# Pull sheets
add_sheet(wb, "PULL All Industries",    pull_table,                            coef_cols)
add_sheet(wb, "PULL Fossil Industries", pull_table[pull_table$Industry %in% fossil_codes, ], coef_cols)

# Full detail for appendix
add_sheet(wb, "PUSH Full Detail", push_full, "coef_policy")
add_sheet(wb, "PULL Full Detail", pull_full, "coef_policy")

out_path <- "PPML_PUSH_PULL_results.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# KEY:
#
# PUSH model:
#   y       = Domestic exports (from COMPLETE_EXPORT.xlsx)
#   policy  = EPS (environmental policy stringency)
#   sample  = Full / OECD-only / EU-only (robustness)
#   coef < 0 + significant => stricter EPS associated with lower domestic
#             export activity => PUSH effect (firms moving production abroad)
#
# PULL model:
#   y       = Foreign imports (from COMPLETE_IMPORT.xlsx)
#   policy  = EPI (overall environmental performance index)
#   sample  = Full / OECD-only / EU-only (robustness)
#   coef > 0 + significant => better EPI score associated with more foreign
#             import activity => PULL effect (production attracted from
#             lower-EPI countries into higher-EPI countries)
#   coef < 0 => lower EPI (weaker env. policy) attracts fossil activity
#
# Controls: log(GDP), TRADE, INTEREST_PAYMENTS, OIL_RENTS, FINANCIAL_DEVELOPMENT
# Fixed effects: country + year (two-way, via dummy variables in PPML)
# SE: clustered by country
# Sig: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10  ns not significant
# =============================================================================