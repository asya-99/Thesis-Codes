# =============================================================================
# PPML with Two-Way Fixed Effects — EPI for BOTH Push and Pull
# PUSH: Domestic Exports ~ EPI(t) + EPI(t-1) + EPI(t-2) + controls
# PULL: Foreign  Imports ~ EPI(t) + EPI(t-1) + EPI(t-2) + controls
#
# Using the same policy measure (EPI) for both sides allows direct comparison:
# Push coef < 0: higher EPI (better env. performance) = fewer domestic exports
# Pull coef < 0: higher EPI (better env. performance) = fewer foreign imports
#                i.e. low-EPI countries attracting fossil-fuel production
#
# If push is negative AND pull is negative for the same fossil industry:
# => carbon leakage confirmed: strict countries export less, lax countries
#    import more of the same dirty production
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. COUNTRY LISTS FOR ROBUSTNESS
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

# =============================================================================
# 2. LOAD DATA
# =============================================================================
df_export <- read_excel("COMPLETE_EXPORT.xlsx", sheet = "Sheet1")
df_import <- read_excel("COMPLETE_IMPORT.xlsx", sheet = "Sheet1")

names(df_export) <- trimws(names(df_export))
names(df_import) <- trimws(names(df_import))

to_numeric_cols <- function(df) {
  skip <- c("REF_AREA", "TIME_PERIOD")
  for (col in names(df)[!names(df) %in% skip])
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
  df
}

df_export <- to_numeric_cols(df_export)
df_import <- to_numeric_cols(df_import)

cat("Export data — Rows:", nrow(df_export),
    "| Countries:", length(unique(df_export$REF_AREA)), "\n")
cat("Import data — Rows:", nrow(df_import),
    "| Countries:", length(unique(df_import$REF_AREA)), "\n")

# Check EPI is present in both
if (!"EPI" %in% names(df_export)) stop("EPI column not found in COMPLETE_EXPORT.xlsx")
if (!"EPI" %in% names(df_import)) stop("EPI column not found in COMPLETE_IMPORT.xlsx")

cat("Non-missing EPI in export data:", sum(!is.na(df_export$EPI)), "\n")
cat("Non-missing EPI in import data:", sum(!is.na(df_import$EPI)), "\n")

# =============================================================================
# 3. CREATE LAGS (t-1 and t-2) WITHIN COUNTRY
# =============================================================================
lag_within <- function(x, group, lag = 1) {
  out <- rep(NA, length(x))
  for (g in unique(group)) {
    idx      <- which(group == g)
    vals     <- x[idx]
    lagged   <- c(rep(NA, lag), vals[seq_len(length(vals) - lag)])
    out[idx] <- lagged
  }
  out
}

df_export <- df_export[order(df_export$REF_AREA, df_export$TIME_PERIOD), ]
df_import <- df_import[order(df_import$REF_AREA, df_import$TIME_PERIOD), ]

df_export$EPI_lag1 <- lag_within(df_export$EPI, df_export$REF_AREA, 1)
df_export$EPI_lag2 <- lag_within(df_export$EPI, df_export$REF_AREA, 2)

df_import$EPI_lag1 <- lag_within(df_import$EPI, df_import$REF_AREA, 1)
df_import$EPI_lag2 <- lag_within(df_import$EPI, df_import$REF_AREA, 2)

cat("\nExport EPI lag1:", sum(!is.na(df_export$EPI_lag1)),
    "| lag2:", sum(!is.na(df_export$EPI_lag2)), "\n")
cat("Import EPI lag1:", sum(!is.na(df_import$EPI_lag1)),
    "| lag2:", sum(!is.na(df_import$EPI_lag2)), "\n")

# =============================================================================
# 4. INDUSTRY LABELS
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

gva_cols       <- names(df_export)[grepl("_(Domestic|Foreign)$", names(df_export))]
industry_codes <- unique(sub("_(Domestic|Foreign)$", "", gva_cols))

# =============================================================================
# 5. PPML FUNCTION — THREE LAGS SIMULTANEOUSLY
# =============================================================================
ppml_twfe_lag <- function(y, pol_t, pol_l1, pol_l2,
                           gdp, trade, interest, oil_rents, fin_dev,
                           country, year) {

  keep <- !is.na(y) & !is.na(pol_t) & !is.na(pol_l1) & !is.na(pol_l2) &
          !is.na(gdp) & !is.na(trade) & !is.na(interest) &
          !is.na(oil_rents) & !is.na(fin_dev) &
          !is.na(country) & !is.na(year) & y >= 0

  y        <- y[keep];        pol_t    <- pol_t[keep]
  pol_l1   <- pol_l1[keep];   pol_l2   <- pol_l2[keep]
  gdp      <- gdp[keep];      trade    <- trade[keep]
  interest <- interest[keep]; oil_rents<- oil_rents[keep]
  fin_dev  <- fin_dev[keep];  country  <- country[keep]
  year     <- year[keep]
  n        <- length(y)

  if (n < 20 || length(unique(country)) < 3) return(NULL)

  gdp       <- log(gdp + 1)
  trade     <- scale(trade)[, 1]
  interest  <- scale(interest)[, 1]
  oil_rents <- scale(oil_rents)[, 1]
  fin_dev   <- scale(fin_dev)[, 1]

  country_f <- relevel(factor(country), ref = levels(factor(country))[1])
  year_f    <- relevel(factor(year),    ref = levels(factor(year))[1])

  X_controls <- model.matrix(~ gdp + trade + interest + oil_rents + fin_dev +
                               country_f + year_f - 1)
  X_policy   <- cbind(pol_t = pol_t, pol_l1 = pol_l1, pol_l2 = pol_l2)
  X          <- cbind(X_policy, X_controls)

  fit <- tryCatch(
    glm.fit(x = X, y = y, family = poisson(link = "log"),
            control = glm.control(maxit = 200, epsilon = 1e-8)),
    error = function(e) NULL
  )

  if (is.null(fit) || !fit$converged) return(NULL)

  resids   <- y - fit$fitted.values
  score    <- X * resids
  XtWX     <- t(X) %*% diag(fit$fitted.values) %*% X
  XtWX_inv <- tryCatch(solve(XtWX), error = function(e) NULL)
  if (is.null(XtWX_inv)) return(NULL)

  k <- ncol(X)
  B <- matrix(0, k, k)
  for (ctry in unique(country)) {
    idx <- country == ctry
    sc  <- colSums(score[idx, , drop = FALSE])
    B   <- B + outer(sc, sc)
  }
  G <- length(unique(country))
  V <- (G / (G - 1)) * XtWX_inv %*% B %*% XtWX_inv

  stars <- function(p) ifelse(p < 0.001, "***", ifelse(p < 0.01, "**",
                       ifelse(p < 0.05,  "*",   ifelse(p < 0.10, ".", "ns"))))

  extract <- function(col_name, vi) {
    coef_v <- fit$coefficients[col_name]
    se_v   <- sqrt(max(V[vi, vi], 0))
    z_v    <- coef_v / se_v
    p_v    <- 2 * pnorm(-abs(z_v))
    list(coef = round(coef_v, 4), se = round(se_v, 4),
         z = round(z_v, 3), p = round(p_v, 4), sig = stars(p_v))
  }

  list(t = extract("pol_t", 1), l1 = extract("pol_l1", 2),
       l2 = extract("pol_l2", 3), n = n, G = G)
}

# =============================================================================
# 6. RUN MODELS FOR PUSH AND PULL
# =============================================================================
run_model <- function(df, gva_type, sample_filter = NULL) {

  if (!is.null(sample_filter)) {
    country_list <- if (sample_filter == "OECD") oecd_iso3 else eu_iso3
    df <- df[df$REF_AREA %in% country_list, ]
  }

  out <- data.frame()
  for (ind in industry_codes) {
    dom_col <- paste0(ind, "_Domestic")
    for_col <- paste0(ind, "_Foreign")
    if (!dom_col %in% names(df) | !for_col %in% names(df)) next

    y <- if (gva_type == "Domestic")     df[[dom_col]]
         else if (gva_type == "Foreign") df[[for_col]]
         else                            df[[dom_col]] + df[[for_col]]

    res <- ppml_twfe_lag(
      y         = y,
      pol_t     = df$EPI,
      pol_l1    = df$EPI_lag1,
      pol_l2    = df$EPI_lag2,
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
        Industry    = ind,  Description = label,
        GVA_type    = gva_type,
        Sample      = ifelse(is.null(sample_filter), "Full", sample_filter),
        coef_t      = res$t$coef,  se_t  = res$t$se,  sig_t  = res$t$sig,
        coef_l1     = res$l1$coef, se_l1 = res$l1$se, sig_l1 = res$l1$sig,
        coef_l2     = res$l2$coef, se_l2 = res$l2$se, sig_l2 = res$l2$sig,
        n_obs       = res$n,       n_countries = res$G,
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# PUSH — domestic exports
cat("\n--- PUSH: EPI x Domestic Exports ---\n")
push_full <- run_model(df_export, "Domestic", NULL);   cat("Full |")
push_oecd <- run_model(df_export, "Domestic", "OECD"); cat(" OECD |")
push_eu   <- run_model(df_export, "Domestic", "EU");   cat(" EU done\n")

# PULL — foreign imports
cat("\n--- PULL: EPI x Foreign Imports ---\n")
pull_full <- run_model(df_import, "Foreign", NULL);    cat("Full |")
pull_oecd <- run_model(df_import, "Foreign", "OECD");  cat(" OECD |")
pull_eu   <- run_model(df_import, "Foreign", "EU");    cat(" EU done\n")

# =============================================================================
# 7. BUILD SUMMARY TABLE
# =============================================================================
build_summary <- function(full, oecd, eu) {
  inds <- unique(full$Industry)
  out  <- data.frame()

  fmt <- function(df, cc, sc) {
    if (nrow(df) == 0 || is.na(df[[cc]])) return(NA)
    paste0(df[[cc]], " ", df[[sc]])
  }

  for (ind in inds) {
    label <- full$Description[full$Industry == ind][1]
    f <- full[full$Industry == ind, ]
    o <- oecd[oecd$Industry == ind, ]
    e <- eu  [eu$Industry   == ind, ]

    row <- data.frame(
      Industry    = ind, Description = label,
      t_Full      = fmt(f,"coef_t","sig_t"),
      t_OECD      = fmt(o,"coef_t","sig_t"),
      t_EU        = fmt(e,"coef_t","sig_t"),
      l1_Full     = fmt(f,"coef_l1","sig_l1"),
      l1_OECD     = fmt(o,"coef_l1","sig_l1"),
      l1_EU       = fmt(e,"coef_l1","sig_l1"),
      l2_Full     = fmt(f,"coef_l2","sig_l2"),
      l2_OECD     = fmt(o,"coef_l2","sig_l2"),
      l2_EU       = fmt(e,"coef_l2","sig_l2"),
      n_Full      = if (nrow(f)>0) f$n_obs       else NA,
      G_Full      = if (nrow(f)>0) f$n_countries else NA,
      stringsAsFactors = FALSE
    )
    out <- rbind(out, row)
  }
  sort_key <- as.numeric(sub(" .*", "", out$l1_Full))
  out[order(sort_key), ]
}

push_summary <- build_summary(push_full, push_oecd, push_eu)
pull_summary <- build_summary(pull_full, pull_oecd, pull_eu)

# =============================================================================
# 8. EXPORT TO EXCEL
# =============================================================================
wb  <- createWorkbook()
hs  <- createStyle(fontColour = "#FFFFFF", bgFill = "#2F4F4F",
                   textDecoration = "bold", halign = "center")
neg <- createStyle(fontColour = "#C0392B", textDecoration = "bold")
pos <- createStyle(fontColour = "#1D6A40", textDecoration = "bold")

colour_fmt_col <- function(wb, sheet, data, col_name) {
  ci <- which(names(data) == col_name)
  if (length(ci) == 0) return()
  for (i in seq_len(nrow(data))) {
    val_str <- data[i, ci]
    if (!is.na(val_str)) {
      r_val <- suppressWarnings(as.numeric(sub(" .*", "", val_str)))
      if (!is.na(r_val))
        addStyle(wb, sheet, if (r_val < 0) neg else pos, rows = i+1, cols = ci)
    }
  }
}

fmt_cols <- c("t_Full","t_OECD","t_EU",
              "l1_Full","l1_OECD","l1_EU",
              "l2_Full","l2_OECD","l2_EU")

add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle = hs)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
  for (cc in fmt_cols) colour_fmt_col(wb, name, data, cc)
}

fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")

add_sheet(wb, "PUSH All Industries",    push_summary)
add_sheet(wb, "PUSH Fossil Industries", push_summary[push_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PULL All Industries",    pull_summary)
add_sheet(wb, "PULL Fossil Industries", pull_summary[pull_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PUSH Full Detail",       push_full)
add_sheet(wb, "PULL Full Detail",       pull_full)

out_path <- "PPML_EPI_PUSH_PULL_LAG.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# KEY:
# Same EPI measure used for both push and pull — cleaner comparison
#
# PUSH (domestic exports x EPI):
#   coef < 0: higher EPI = fewer domestic exports of that industry
#   => countries performing better on climate are exporting less dirty output
#
# PULL (foreign imports x EPI):
#   coef < 0: higher EPI = fewer foreign imports of that industry
#   => better-performing countries are importing less dirty production too
#   (this would mean the industry is genuinely shrinking globally)
#
#   coef > 0 for PULL: higher EPI = MORE foreign imports
#   => high-EPI countries are importing dirty production from elsewhere
#   => CARBON LEAKAGE confirmed
#
# The most informative comparison:
#   Push negative + Pull positive for fossil industries
#   => high-EPI countries are exporting less AND importing more dirty goods
#   => clear leakage: production shifted abroad, not eliminated
#
# Sig: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10  ns not significant
# =============================================================================