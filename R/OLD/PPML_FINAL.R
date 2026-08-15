# =============================================================================
# PPML with Two-Way Fixed Effects — Final Push/Pull Framework
#
# PUSH models (OECD countries only):
#   P1: Domestic Exports ~ EPS(t, t-1, t-2) + controls
#   P2: Domestic Exports ~ EPI(t, t-1, t-2) + controls
#
# PULL model (non-OECD countries only):
#   PL: Foreign Imports ~ EPI(t, t-1, t-2) + controls
#
# Logic:
#   PUSH — OECD countries have the strictest regulations. As EPS/EPI tightens,
#          do their domestic firms export less? (production leaving)
#   PULL — Non-OECD countries have weaker regulations. As their EPI lags
#          behind OECD levels, do they attract more foreign-owned imports?
#          (production arriving, owned by multinationals from strict countries)
#
# Using EPI for both push and pull allows direct comparison of coefficients
# across the OECD/non-OECD divide.
# Using EPS for push gives the most precise policy stringency measure
# for the OECD countries where it is available.
#
# Output: PPML_FINAL_results.xlsx (6 main sheets + 3 detail sheets)
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. COUNTRY LISTS
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

# Split by OECD membership
df_export_oecd    <- df_export[df_export$REF_AREA %in% oecd_iso3, ]
df_export_eu      <- df_export[df_export$REF_AREA %in% eu_iso3, ]
df_import_nonoecd <- df_import[!df_import$REF_AREA %in% oecd_iso3, ]
df_import_noneu   <- df_import[!df_import$REF_AREA %in% eu_iso3, ]

cat("=== SAMPLE SIZES ===\n")
cat("PUSH — Export OECD countries:     ", length(unique(df_export_oecd$REF_AREA)),
    "countries |", nrow(df_export_oecd), "rows\n")
cat("PUSH — Export EU countries:       ", length(unique(df_export_eu$REF_AREA)),
    "countries |", nrow(df_export_eu), "rows\n")
cat("PULL — Import non-OECD countries: ", length(unique(df_import_nonoecd$REF_AREA)),
    "countries |", nrow(df_import_nonoecd), "rows\n")
cat("PULL — Import non-EU countries:   ", length(unique(df_import_noneu$REF_AREA)),
    "countries |", nrow(df_import_noneu), "rows\n")
cat("PULL — Import all countries:      ", length(unique(df_import$REF_AREA)),
    "countries |", nrow(df_import), "rows\n")

# =============================================================================
# 3. CREATE LAGS WITHIN COUNTRY (t-1 and t-2)
# =============================================================================
lag_within <- function(x, group, lag = 1) {
  out <- rep(NA, length(x))
  for (g in unique(group)) {
    idx      <- which(group == g)
    vals     <- x[idx]
    out[idx] <- c(rep(NA, lag), vals[seq_len(length(vals) - lag)])
  }
  out
}

add_lags <- function(df, policy_cols) {
  df <- df[order(df$REF_AREA, df$TIME_PERIOD), ]
  for (col in policy_cols) {
    df[[paste0(col, "_lag1")]] <- lag_within(df[[col]], df$REF_AREA, 1)
    df[[paste0(col, "_lag2")]] <- lag_within(df[[col]], df$REF_AREA, 2)
  }
  df
}

df_export_oecd    <- add_lags(df_export_oecd,    c("EPS", "EPI"))
df_export_eu      <- add_lags(df_export_eu,      c("EPS", "EPI"))
df_import_nonoecd <- add_lags(df_import_nonoecd, "EPI")
df_import_noneu   <- add_lags(df_import_noneu,   "EPI")
df_import         <- add_lags(df_import,          "EPI")

cat("\n=== LAG AVAILABILITY ===\n")
cat("EPS lag1 (OECD export):", sum(!is.na(df_export_oecd$EPS_lag1)), "\n")
cat("EPI lag1 (OECD export):", sum(!is.na(df_export_oecd$EPI_lag1)), "\n")
cat("EPI lag1 (non-OECD import):", sum(!is.na(df_import_nonoecd$EPI_lag1)), "\n")

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
# 5. PPML FUNCTION — THREE LAGS SIMULTANEOUSLY, CLUSTERED SE
# =============================================================================
ppml_twfe_lag <- function(y, pol_t, pol_l1, pol_l2,
                           gdp, trade, interest, oil_rents, fin_dev,
                           country, year) {

  keep <- !is.na(y) & !is.na(pol_t) & !is.na(pol_l1) & !is.na(pol_l2) &
          !is.na(gdp) & !is.na(trade) & !is.na(interest) &
          !is.na(oil_rents) & !is.na(fin_dev) &
          !is.na(country) & !is.na(year) & y >= 0

  y         <- y[keep];        pol_t    <- pol_t[keep]
  pol_l1    <- pol_l1[keep];   pol_l2   <- pol_l2[keep]
  gdp       <- gdp[keep];      trade    <- trade[keep]
  interest  <- interest[keep]; oil_rents<- oil_rents[keep]
  fin_dev   <- fin_dev[keep];  country  <- country[keep]
  year      <- year[keep]
  n         <- length(y)

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

  extract <- function(name, vi) {
    cv <- fit$coefficients[name]
    sv <- sqrt(max(V[vi, vi], 0))
    zv <- cv / sv
    pv <- 2 * pnorm(-abs(zv))
    list(coef = round(cv, 4), se = round(sv, 4),
         z = round(zv, 3),   p = round(pv, 4), sig = stars(pv))
  }

  list(t  = extract("pol_t",  1),
       l1 = extract("pol_l1", 2),
       l2 = extract("pol_l2", 3),
       n  = n, G = G)
}

# =============================================================================
# 6. RUN MODEL — generic wrapper
# df:         dataset to use
# policy_t/l1/l2: column names for the three lags
# gva_type:   "Domestic" or "Foreign"
# model_name: label for output
# =============================================================================
run_model <- function(df, policy_t, policy_l1, policy_l2,
                      gva_type, model_name) {
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
      pol_t     = df[[policy_t]],
      pol_l1    = df[[policy_l1]],
      pol_l2    = df[[policy_l2]],
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
        Industry    = ind,  Description = label, Model = model_name,
        coef_t      = res$t$coef,  se_t   = res$t$se,  sig_t  = res$t$sig,
        coef_l1     = res$l1$coef, se_l1  = res$l1$se, sig_l1 = res$l1$sig,
        coef_l2     = res$l2$coef, se_l2  = res$l2$se, sig_l2 = res$l2$sig,
        n_obs       = res$n,       n_countries = res$G,
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# =============================================================================
# 7. RUN ALL MODELS
# =============================================================================

# --- PUSH: OECD domestic exports ---
cat("\n=== PUSH MODELS (OECD domestic exports) ===\n")

cat("P1: EPS x Domestic Exports (OECD)... ")
push_eps_oecd <- run_model(df_export_oecd,
                            "EPS","EPS_lag1","EPS_lag2",
                            "Domestic", "P1: EPS x Dom.Export (OECD)")
cat(nrow(push_eps_oecd), "rows\n")

cat("P2: EPS x Domestic Exports (EU)... ")
push_eps_eu   <- run_model(df_export_eu,
                            "EPS","EPS_lag1","EPS_lag2",
                            "Domestic", "P2: EPS x Dom.Export (EU)")
cat(nrow(push_eps_eu), "rows\n")

cat("P3: EPI x Domestic Exports (OECD)... ")
push_epi_oecd <- run_model(df_export_oecd,
                            "EPI","EPI_lag1","EPI_lag2",
                            "Domestic", "P3: EPI x Dom.Export (OECD)")
cat(nrow(push_epi_oecd), "rows\n")

cat("P4: EPI x Domestic Exports (EU)... ")
push_epi_eu   <- run_model(df_export_eu,
                            "EPI","EPI_lag1","EPI_lag2",
                            "Domestic", "P4: EPI x Dom.Export (EU)")
cat(nrow(push_epi_eu), "rows\n")

# --- PULL: non-OECD foreign imports ---
cat("\n=== PULL MODELS (non-OECD foreign imports) ===\n")

cat("PL1: EPI x Foreign Imports (non-OECD)... ")
pull_nonoecd  <- run_model(df_import_nonoecd,
                            "EPI","EPI_lag1","EPI_lag2",
                            "Foreign", "PL1: EPI x For.Import (non-OECD)")
cat(nrow(pull_nonoecd), "rows\n")

cat("PL2: EPI x Foreign Imports (non-EU)... ")
pull_noneu    <- run_model(df_import_noneu,
                            "EPI","EPI_lag1","EPI_lag2",
                            "Foreign", "PL2: EPI x For.Import (non-EU)")
cat(nrow(pull_noneu), "rows\n")

cat("PL3: EPI x Foreign Imports (all countries)... ")
pull_all      <- run_model(df_import,
                            "EPI","EPI_lag1","EPI_lag2",
                            "Foreign", "PL3: EPI x For.Import (all)")
cat(nrow(pull_all), "rows\n")

# =============================================================================
# 8. BUILD SUMMARY TABLES
# One row per industry, models as columns
# =============================================================================
build_summary <- function(model_list) {
  # model_list: named list of result data frames
  inds <- unique(model_list[[1]]$Industry)
  out  <- data.frame()

  fmt <- function(df, cc, sc) {
    if (is.null(df) || nrow(df) == 0 || is.na(df[[cc]])) return(NA)
    paste0(df[[cc]], " ", df[[sc]])
  }

  for (ind in inds) {
    label <- model_list[[1]]$Description[model_list[[1]]$Industry == ind][1]
    row   <- data.frame(Industry = ind, Description = label,
                        stringsAsFactors = FALSE)

    for (mname in names(model_list)) {
      sub <- model_list[[mname]]
      sub <- sub[sub$Industry == ind, ]
      row[[paste0(mname, "_t")]]  <- fmt(sub, "coef_t",  "sig_t")
      row[[paste0(mname, "_l1")]] <- fmt(sub, "coef_l1", "sig_l1")
      row[[paste0(mname, "_l2")]] <- fmt(sub, "coef_l2", "sig_l2")
    }
    out <- rbind(out, row)
  }

  # Sort by first model's l1 coefficient (most negative first)
  first_l1  <- paste0(names(model_list)[1], "_l1")
  sort_key  <- as.numeric(sub(" .*", "", out[[first_l1]]))
  out[order(sort_key), ]
}

push_summary <- build_summary(list(
  "EPS_OECD" = push_eps_oecd,
  "EPS_EU"   = push_eps_eu,
  "EPI_OECD" = push_epi_oecd,
  "EPI_EU"   = push_epi_eu
))

pull_summary <- build_summary(list(
  "nonOECD"  = pull_nonoecd,
  "nonEU"    = pull_noneu,
  "All"      = pull_all
))

# =============================================================================
# 9. EXPORT TO EXCEL
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

add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle = hs)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
  # Colour all columns that contain coefficients (identified by _t, _l1, _l2 suffix)
  coef_cols <- names(data)[grepl("_(t|l1|l2)$", names(data))]
  for (cc in coef_cols) colour_fmt_col(wb, name, data, cc)
}

fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")

# Main summary sheets
add_sheet(wb, "PUSH Summary",
          push_summary)
add_sheet(wb, "PUSH Fossil",
          push_summary[push_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PULL Summary",
          pull_summary)
add_sheet(wb, "PULL Fossil",
          pull_summary[pull_summary$Industry %in% fossil_codes, ])

# Full detail sheets (all stats for appendix)
all_push_detail <- rbind(push_eps_oecd, push_eps_eu,
                          push_epi_oecd, push_epi_eu)
all_pull_detail <- rbind(pull_nonoecd, pull_noneu, pull_all)

add_sheet(wb, "PUSH Full Detail", all_push_detail)
add_sheet(wb, "PULL Full Detail", all_pull_detail)

out_path <- "PPML_FINAL_results.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# OUTPUT STRUCTURE:
#
# Sheet: PUSH Summary — one row per industry, columns:
#   EPS_OECD_t / _l1 / _l2  — EPS effect on OECD domestic exports
#   EPS_EU_t   / _l1 / _l2  — EPS effect on EU domestic exports (robustness)
#   EPI_OECD_t / _l1 / _l2  — EPI effect on OECD domestic exports
#   EPI_EU_t   / _l1 / _l2  — EPI effect on EU domestic exports (robustness)
#
# Sheet: PULL Summary — one row per industry, columns:
#   nonOECD_t / _l1 / _l2   — EPI effect on non-OECD foreign imports (main)
#   nonEU_t   / _l1 / _l2   — EPI effect on non-EU foreign imports (robustness)
#   All_t     / _l1 / _l2   — EPI effect on all-country foreign imports
#
# KEY:
# Each cell: coefficient + significance e.g. "-0.18 **"
# Red = negative, Green = positive
#
# PUSH: coef < 0 => stricter policy = fewer domestic exports (production leaving)
# PULL: coef < 0 => lower EPI = more foreign imports (production arriving in
#                   weak-regulation countries — carbon leakage confirmed)
#
# COMBINED LEAKAGE SIGNAL:
#   PUSH negative + PULL negative for same fossil industry = carbon leakage
#   Production is leaving OECD countries AND landing in non-OECD countries
#   The lag structure (t-2 > t-1 > t) confirms the investment adjustment channel
#
# Sig: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10  ns not significant
# =============================================================================