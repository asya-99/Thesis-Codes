# =============================================================================
# PPML with Two-Way Fixed Effects — GVA-based Push/Pull Framework
# Main dependent variables: Domestic GVA, Foreign GVA, Foreign/Domestic ratio
# Secondary (robustness): Domestic Exports, Foreign Imports
#
# PUSH (OECD countries):
#   Domestic GVA   ~ EPS/EPI + controls  — is domestic production shrinking?
#   Foreign GVA    ~ EPS/EPI + controls  — are foreign firms filling the gap?
#   For/Dom ratio  ~ EPS/EPI + controls  — is ownership shifting to foreign?
#
# PULL (non-OECD resource-rich countries):
#   Foreign GVA    ~ EPI + controls      — are foreign firms expanding?
#   Domestic GVA   ~ EPI + controls      — are local firms also growing?
#   For/Dom ratio  ~ EPI + controls      — is foreign ownership share rising?
#
# Industries O84 and T97T98 dropped (non-traded, degenerate PPML fit)
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

fossil_producers_nonoecd <- c(
  "NGA","AGO","MOZ","SEN","ZAF","GHA","COG","GAB","CMR",
  "SAU","ARE","IRQ","IRN","KWT","QAT","OMN","DZA","LBY","EGY",
  "RUS","KAZ","AZE","TKM","UZB",
  "IDN","MYS","VNM","PNG","BGD",
  "BRA","TTO","VEN","ECU","BOL",
  "PAK","IND"
)

# Industries to DROP — non-traded, produce degenerate PPML fits
drop_industries <- c("O84", "T97T98")

# Fossil-fuel-intensive industries for focused output
fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")

# =============================================================================
# 2. LOAD DATA
# =============================================================================
df_gva    <- read_excel("COMPLETE_GVA.xlsx",    sheet = "Sheet1")
df_export <- read_excel("COMPLETE_EXPORT.xlsx", sheet = "Sheet1")
df_import <- read_excel("COMPLETE_IMPORT.xlsx", sheet = "Sheet1")

names(df_gva)    <- trimws(names(df_gva))
names(df_export) <- trimws(names(df_export))
names(df_import) <- trimws(names(df_import))

to_numeric_cols <- function(df) {
  skip <- c("REF_AREA","TIME_PERIOD")
  for (col in names(df)[!names(df) %in% skip])
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
  df
}

df_gva    <- to_numeric_cols(df_gva)
df_export <- to_numeric_cols(df_export)
df_import <- to_numeric_cols(df_import)

cat("GVA data:   ", nrow(df_gva),    "rows |",
    length(unique(df_gva$REF_AREA)),    "countries\n")
cat("Export data:", nrow(df_export), "rows |",
    length(unique(df_export$REF_AREA)), "countries\n")
cat("Import data:", nrow(df_import), "rows |",
    length(unique(df_import$REF_AREA)), "countries\n")

# =============================================================================
# 3. GET INDUSTRY CODES — EXCLUDING DROPPED INDUSTRIES
# =============================================================================
gva_cols       <- names(df_gva)[grepl("_(Domestic|Foreign)$", names(df_gva))]
industry_codes <- unique(sub("_(Domestic|Foreign)$", "", gva_cols))
industry_codes <- industry_codes[!industry_codes %in% drop_industries]

cat("\nIndustries after dropping O84 and T97T98:", length(industry_codes), "\n")

# =============================================================================
# 4. DEFINE RESOURCE-RICH SUBSETS
# =============================================================================
nonoecd_in_data <- unique(df_import$REF_AREA[
  !df_import$REF_AREA %in% oecd_iso3])

oil_rents_avg <- tapply(
  df_import$OIL_RENTS[!df_import$REF_AREA %in% oecd_iso3],
  df_import$REF_AREA[!df_import$REF_AREA %in% oecd_iso3],
  mean, na.rm = TRUE
)
oil_rents_df <- data.frame(
  REF_AREA       = names(oil_rents_avg),
  mean_oil_rents = round(as.numeric(oil_rents_avg), 2),
  stringsAsFactors = FALSE
)
oil_rents_df <- oil_rents_df[order(-oil_rents_df$mean_oil_rents), ]

subset_5pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 5]
subset_2pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 2]
subset_1pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 1]
subset_known    <- nonoecd_in_data[nonoecd_in_data %in% fossil_producers_nonoecd]
subset_intersect<- intersect(subset_2pct, subset_known)

# =============================================================================
# 5. DIAGNOSTIC TABLE
# =============================================================================
country_diag <- data.frame(
  REF_AREA       = oil_rents_df$REF_AREA,
  Oil_Rents_Avg  = oil_rents_df$mean_oil_rents,
  In_Rents_5pct  = oil_rents_df$REF_AREA %in% subset_5pct,
  In_Rents_2pct  = oil_rents_df$REF_AREA %in% subset_2pct,
  In_Rents_1pct  = oil_rents_df$REF_AREA %in% subset_1pct,
  Known_Producer = oil_rents_df$REF_AREA %in% subset_known,
  In_Intersect   = oil_rents_df$REF_AREA %in% subset_intersect,
  stringsAsFactors = FALSE
)

cat("\n=================================================================\n")
cat("  NON-OECD COUNTRY DIAGNOSTIC — Oil Rents & Subset Membership\n")
cat("=================================================================\n")
cat(sprintf("%-8s  %11s  %5s  %5s  %5s  %7s  %9s\n",
    "Country","Oil Rents%",">5%",">2%",">1%","Known","Intersect"))
cat(paste(rep("-", 60), collapse=""), "\n")
for (i in seq_len(nrow(country_diag))) {
  cat(sprintf("%-8s  %11.2f  %5s  %5s  %5s  %7s  %9s\n",
    country_diag$REF_AREA[i],
    country_diag$Oil_Rents_Avg[i],
    ifelse(country_diag$In_Rents_5pct[i],  "YES", "-"),
    ifelse(country_diag$In_Rents_2pct[i],  "YES", "-"),
    ifelse(country_diag$In_Rents_1pct[i],  "YES", "-"),
    ifelse(country_diag$Known_Producer[i], "YES", "-"),
    ifelse(country_diag$In_Intersect[i],   "YES", "-")
  ))
}
cat(paste(rep("-", 60), collapse=""), "\n")
cat(sprintf("  All non-OECD : %d  |  >5%%: %d  |  >2%%: %d  |  >1%%: %d  |  Known: %d  |  Intersect: %d\n",
    length(nonoecd_in_data), length(subset_5pct), length(subset_2pct),
    length(subset_1pct), length(subset_known), length(subset_intersect)))
cat("=================================================================\n\n")

if (length(subset_5pct) < 3) {
  cat("WARNING: Rents >5% has only", length(subset_5pct),
      "countries. Using >2% as fallback.\n\n")
  subset_5pct <- subset_2pct
}

# =============================================================================
# 6. CREATE LAGS WITHIN COUNTRY
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

add_lags <- function(df, cols) {
  df <- df[order(df$REF_AREA, df$TIME_PERIOD), ]
  for (col in cols) {
    if (!col %in% names(df)) next
    df[[paste0(col,"_lag1")]] <- lag_within(df[[col]], df$REF_AREA, 1)
    df[[paste0(col,"_lag2")]] <- lag_within(df[[col]], df$REF_AREA, 2)
  }
  df
}

df_gva    <- add_lags(df_gva,    c("EPS","EPI"))
df_export <- add_lags(df_export, c("EPS","EPI"))
df_import <- add_lags(df_import, "EPI")

# =============================================================================
# 7. INDUSTRY LABELS
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
  C31T33="Other manufacturing",             D35_E36T39="Electricity, gas & water",
  F41T43="Construction",                    G45T47="Wholesale & retail trade",
  H49="Land transport",                     H50="Water transport",
  H51="Air transport",                      H52="Warehousing & support",
  H53="Postal & courier",                   I55T56="Accommodation & food services",
  J58T60="Publishing, TV & broadcasting",   J61="Telecommunications",
  J62T63="IT & information services",       K64T66="Financial & insurance",
  L68="Real estate",                        M69T75="Professional & technical services",
  N77T82="Admin & support services",        P85="Education",
  Q86T88="Health & social work",            R90T93="Arts & entertainment",
  S94T96="Other service activities"
)

# =============================================================================
# 8. PPML FUNCTION — for Domestic GVA, Foreign GVA, and Exports/Imports
# Standard PPML, handles zeros correctly
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
  year      <- year[keep];     n        <- length(y)

  if (n < 10 || length(unique(country)) < 3) return(NULL)

  gdp       <- log(gdp + 1)
  trade     <- scale(trade)[, 1]
  interest  <- scale(interest)[, 1]
  oil_rents <- scale(oil_rents)[, 1]
  fin_dev   <- scale(fin_dev)[, 1]

  country_f <- relevel(factor(country), ref = levels(factor(country))[1])
  year_f    <- relevel(factor(year),    ref = levels(factor(year))[1])

  X_controls <- model.matrix(~ gdp + trade + interest + oil_rents + fin_dev +
                               country_f + year_f - 1)
  X <- cbind(cbind(pol_t=pol_t, pol_l1=pol_l1, pol_l2=pol_l2), X_controls)

  fit <- tryCatch(
    glm.fit(x=X, y=y, family=poisson(link="log"),
            control=glm.control(maxit=200, epsilon=1e-8)),
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
    sc  <- colSums(score[idx,,drop=FALSE])
    B   <- B + outer(sc, sc)
  }
  G <- length(unique(country))
  V <- (G/(G-1)) * XtWX_inv %*% B %*% XtWX_inv

  stars <- function(p) ifelse(p<0.001,"***",ifelse(p<0.01,"**",
                       ifelse(p<0.05,"*",  ifelse(p<0.10,".","ns"))))
  extract <- function(name, vi) {
    cv <- fit$coefficients[name]
    sv <- sqrt(max(V[vi,vi], 0))
    zv <- cv/sv; pv <- 2*pnorm(-abs(zv))
    list(coef=round(cv,4), se=round(sv,4),
         z=round(zv,3), p=round(pv,4), sig=stars(pv))
  }
  list(t=extract("pol_t",1), l1=extract("pol_l1",2),
       l2=extract("pol_l2",3), n=n, G=G)
}

# =============================================================================
# 9. OLS FUNCTION — for the Foreign/Domestic GVA RATIO
# Ratio is continuous and can be negative in edge cases so OLS is appropriate
# Includes the same two-way FE via within transformation
# =============================================================================
ols_twfe_lag <- function(y, pol_t, pol_l1, pol_l2,
                          gdp, trade, interest, oil_rents, fin_dev,
                          country, year) {

  keep <- !is.na(y) & !is.na(pol_t) & !is.na(pol_l1) & !is.na(pol_l2) &
          !is.na(gdp) & !is.na(trade) & !is.na(interest) &
          !is.na(oil_rents) & !is.na(fin_dev) &
          !is.na(country) & !is.na(year) & is.finite(y)

  y         <- y[keep];        pol_t    <- pol_t[keep]
  pol_l1    <- pol_l1[keep];   pol_l2   <- pol_l2[keep]
  gdp       <- log(gdp[keep] + 1)
  trade     <- scale(trade[keep])[,1]
  interest  <- scale(interest[keep])[,1]
  oil_rents <- scale(oil_rents[keep])[,1]
  fin_dev   <- scale(fin_dev[keep])[,1]
  country   <- country[keep];  year <- year[keep]
  n         <- length(y)

  if (n < 10 || length(unique(country)) < 3) return(NULL)

  # Within transformation (two-way FE via demeaning)
  within_transform <- function(x) {
    grand  <- mean(x)
    c_mean <- ave(x, country, FUN=mean)
    y_mean <- ave(x, year,    FUN=mean)
    x - c_mean - y_mean + grand
  }

  y_w      <- within_transform(y)
  pt_w     <- within_transform(pol_t)
  pl1_w    <- within_transform(pol_l1)
  pl2_w    <- within_transform(pol_l2)
  gdp_w    <- within_transform(gdp)
  trade_w  <- within_transform(trade)
  int_w    <- within_transform(interest)
  oil_w    <- within_transform(oil_rents)
  fin_w    <- within_transform(fin_dev)

  fit <- lm(y_w ~ pt_w + pl1_w + pl2_w + gdp_w + trade_w +
                  int_w + oil_w + fin_w - 1)

  # Clustered SE by country
  X       <- model.matrix(fit)
  e       <- residuals(fit)
  XtX_inv <- tryCatch(solve(t(X) %*% X), error=function(e) NULL)
  if (is.null(XtX_inv)) return(NULL)

  k <- ncol(X)
  B <- matrix(0, k, k)
  for (ctry in unique(country)) {
    idx <- country == ctry
    Xc  <- X[idx,,drop=FALSE]
    ec  <- e[idx]
    sc  <- t(Xc) %*% ec
    B   <- B + sc %*% t(sc)
  }
  G   <- length(unique(country))
  adj <- (G/(G-1)) * ((n-1)/(n-k))
  V   <- adj * XtX_inv %*% B %*% XtX_inv

  stars <- function(p) ifelse(p<0.001,"***",ifelse(p<0.01,"**",
                       ifelse(p<0.05,"*",  ifelse(p<0.10,".","ns"))))
  extract_ols <- function(name, vi) {
    cv <- coef(fit)[name]
    sv <- sqrt(max(V[vi,vi], 0))
    tv <- cv/sv; pv <- 2*pt(-abs(tv), df=G-1)
    list(coef=round(cv,4), se=round(sv,4),
         z=round(tv,3), p=round(pv,4), sig=stars(pv))
  }
  list(t=extract_ols("pt_w",1), l1=extract_ols("pl1_w",2),
       l2=extract_ols("pl2_w",3), n=n, G=G)
}

# =============================================================================
# 10. GENERIC RUN FUNCTION
# dep_type: "Domestic", "Foreign", or "Ratio" (Foreign/Domestic)
# estimator: "ppml" or "ols" (ratio uses ols automatically)
# =============================================================================
run_model <- function(df, policy_t, policy_l1, policy_l2,
                      dep_type, sample_label) {
  out <- data.frame()
  for (ind in industry_codes) {
    dom_col <- paste0(ind, "_Domestic")
    for_col <- paste0(ind, "_Foreign")
    if (!dom_col %in% names(df) | !for_col %in% names(df)) next

    dom <- df[[dom_col]]
    frn <- df[[for_col]]

    if (dep_type == "Domestic") {
      y         <- dom
      estimator <- "ppml"
    } else if (dep_type == "Foreign") {
      y         <- frn
      estimator <- "ppml"
    } else {  # Ratio
      # Foreign / Domestic — winsorise extreme values at 99th percentile
      ratio     <- frn / (dom + 1)   # +1 avoids division by zero
      p99       <- quantile(ratio, 0.99, na.rm=TRUE)
      y         <- pmin(ratio, p99)
      estimator <- "ols"
    }

    res <- if (estimator == "ppml") {
      ppml_twfe_lag(y, df[[policy_t]], df[[policy_l1]], df[[policy_l2]],
                    df$GDP, df$TRADE, df$INTEREST_PAYMENTS,
                    df$OIL_RENTS, df$FINANCIAL_DEVELOPMENT,
                    df$REF_AREA, df$TIME_PERIOD)
    } else {
      ols_twfe_lag(y, df[[policy_t]], df[[policy_l1]], df[[policy_l2]],
                   df$GDP, df$TRADE, df$INTEREST_PAYMENTS,
                   df$OIL_RENTS, df$FINANCIAL_DEVELOPMENT,
                   df$REF_AREA, df$TIME_PERIOD)
    }

    label <- ifelse(ind %in% names(industry_labels), industry_labels[ind], ind)

    if (!is.null(res)) {
      out <- rbind(out, data.frame(
        Industry    = ind,  Description = label, Sample = sample_label,
        Estimator   = estimator,
        coef_t      = res$t$coef,  se_t  = res$t$se,  sig_t  = res$t$sig,
        coef_l1     = res$l1$coef, se_l1 = res$l1$se, sig_l1 = res$l1$sig,
        coef_l2     = res$l2$coef, se_l2 = res$l2$se, sig_l2 = res$l2$sig,
        n_obs       = res$n, n_countries = res$G,
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# Subset wrapper
run_subset <- function(df, country_subset, policy_t, policy_l1, policy_l2,
                       dep_type, sample_label) {
  df_sub      <- df[df$REF_AREA %in% country_subset, ]
  n_countries <- length(unique(df_sub$REF_AREA))
  if (n_countries < 3) {
    cat("  Skipping", sample_label, dep_type,
        "— only", n_countries, "countries\n")
    return(NULL)
  }
  cat(" ", sample_label, "|", dep_type, "| countries:", n_countries, "\n")
  run_model(df_sub, policy_t, policy_l1, policy_l2, dep_type, sample_label)
}

# =============================================================================
# 11. RUN ALL MODELS
# =============================================================================

# ---- PUSH: OECD — GVA domestic, foreign, ratio ----
cat("\n=== PUSH: EPS x GVA (OECD) ===\n")
push_dom_eps_oecd   <- run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS OECD")
push_dom_eps_eu     <- run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS EU")
push_for_eps_oecd   <- run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2", "Foreign",  "EPS OECD")
push_for_eps_eu     <- run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2", "Foreign",  "EPS EU")
push_rat_eps_oecd   <- run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2", "Ratio",    "EPS OECD")
push_rat_eps_eu     <- run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2", "Ratio",    "EPS EU")

cat("\n=== PUSH: EPI x GVA (OECD) ===\n")
push_dom_epi_oecd   <- run_subset(df_gva, oecd_iso3, "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI OECD")
push_dom_epi_eu     <- run_subset(df_gva, eu_iso3,   "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI EU")
push_for_epi_oecd   <- run_subset(df_gva, oecd_iso3, "EPI","EPI_lag1","EPI_lag2", "Foreign",  "EPI OECD")
push_for_epi_eu     <- run_subset(df_gva, eu_iso3,   "EPI","EPI_lag1","EPI_lag2", "Foreign",  "EPI EU")
push_rat_epi_oecd   <- run_subset(df_gva, oecd_iso3, "EPI","EPI_lag1","EPI_lag2", "Ratio",    "EPI OECD")
push_rat_epi_eu     <- run_subset(df_gva, eu_iso3,   "EPI","EPI_lag1","EPI_lag2", "Ratio",    "EPI EU")

# ---- PULL: non-OECD resource-rich — GVA domestic, foreign, ratio ----
pull_subsets <- list(
  "All nonOECD" = nonoecd_in_data,
  "Rents >5%"   = subset_5pct,
  "Rents >2%"   = subset_2pct,
  "Rents >1%"   = subset_1pct,
  "Known"       = subset_known,
  "Intersect"   = subset_intersect
)

cat("\n=== PULL: EPI x Foreign GVA (resource-rich) ===\n")
pull_for_results <- lapply(names(pull_subsets), function(nm)
  run_subset(df_gva, pull_subsets[[nm]], "EPI","EPI_lag1","EPI_lag2",
             "Foreign", nm))
names(pull_for_results) <- names(pull_subsets)

cat("\n=== PULL: EPI x Domestic GVA (resource-rich) ===\n")
pull_dom_results <- lapply(names(pull_subsets), function(nm)
  run_subset(df_gva, pull_subsets[[nm]], "EPI","EPI_lag1","EPI_lag2",
             "Domestic", nm))
names(pull_dom_results) <- names(pull_subsets)

cat("\n=== PULL: EPI x Foreign/Domestic Ratio (resource-rich) ===\n")
pull_rat_results <- lapply(names(pull_subsets), function(nm)
  run_subset(df_gva, pull_subsets[[nm]], "EPI","EPI_lag1","EPI_lag2",
             "Ratio", nm))
names(pull_rat_results) <- names(pull_subsets)

# ---- ROBUSTNESS: exports and imports ----
cat("\n=== ROBUSTNESS: EPS x Domestic Exports (OECD) ===\n")
rob_exp_eps_oecd <- run_subset(df_export, oecd_iso3, "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS OECD")
rob_exp_eps_eu   <- run_subset(df_export, eu_iso3,   "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS EU")

cat("\n=== ROBUSTNESS: EPI x Foreign Imports (non-OECD) ===\n")
rob_imp_all      <- run_subset(df_import, nonoecd_in_data, "EPI","EPI_lag1","EPI_lag2", "Foreign", "All nonOECD")
rob_imp_known    <- run_subset(df_import, subset_known,    "EPI","EPI_lag1","EPI_lag2", "Foreign", "Known")

# =============================================================================
# 12. BUILD SUMMARY TABLES
# =============================================================================
build_summary <- function(results_list) {
  non_null <- results_list[!sapply(results_list, is.null)]
  if (length(non_null) == 0) return(NULL)
  base <- non_null[[1]]
  inds <- unique(base$Industry)
  out  <- data.frame()

  fmt <- function(df_m, ind, cc, sc) {
    if (is.null(df_m)) return(NA)
    sub <- df_m[df_m$Industry == ind, ]
    if (nrow(sub) == 0 || is.na(sub[[cc]])) return(NA)
    paste0(sub[[cc]], " ", sub[[sc]])
  }

  for (ind in inds) {
    label <- base$Description[base$Industry == ind][1]
    row   <- data.frame(Industry=ind, Description=label,
                        stringsAsFactors=FALSE)
    for (mname in names(results_list)) {
      df_m <- results_list[[mname]]
      row[[paste0(mname,"_t")]]  <- fmt(df_m, ind, "coef_t",  "sig_t")
      row[[paste0(mname,"_l1")]] <- fmt(df_m, ind, "coef_l1", "sig_l1")
      row[[paste0(mname,"_l2")]] <- fmt(df_m, ind, "coef_l2", "sig_l2")
    }
    out <- rbind(out, row)
  }
  sort_col <- paste0(names(results_list)[1], "_l1")
  sort_key <- as.numeric(sub(" .*","", out[[sort_col]]))
  out[order(sort_key), ]
}

# Push summaries
push_dom_summary <- build_summary(list(
  "EPS_OECD"=push_dom_eps_oecd, "EPS_EU"=push_dom_eps_eu,
  "EPI_OECD"=push_dom_epi_oecd, "EPI_EU"=push_dom_epi_eu))

push_for_summary <- build_summary(list(
  "EPS_OECD"=push_for_eps_oecd, "EPS_EU"=push_for_eps_eu,
  "EPI_OECD"=push_for_epi_oecd, "EPI_EU"=push_for_epi_eu))

push_rat_summary <- build_summary(list(
  "EPS_OECD"=push_rat_eps_oecd, "EPS_EU"=push_rat_eps_eu,
  "EPI_OECD"=push_rat_epi_oecd, "EPI_EU"=push_rat_epi_eu))

# Pull summaries
pull_for_summary <- build_summary(pull_for_results)
pull_dom_summary <- build_summary(pull_dom_results)
pull_rat_summary <- build_summary(pull_rat_results)

# Robustness summary
rob_summary <- build_summary(list(
  "EPS_Export_OECD"=rob_exp_eps_oecd, "EPS_Export_EU"=rob_exp_eps_eu,
  "EPI_Import_All" =rob_imp_all,      "EPI_Import_Known"=rob_imp_known))

# =============================================================================
# 13. EXPORT TO EXCEL
# =============================================================================
wb  <- createWorkbook()
hs  <- createStyle(fontColour="#FFFFFF", bgFill="#2F4F4F",
                   textDecoration="bold", halign="center")
neg <- createStyle(fontColour="#C0392B", textDecoration="bold")
pos <- createStyle(fontColour="#1D6A40", textDecoration="bold")

colour_fmt_col <- function(wb, sheet, data, col_name) {
  ci <- which(names(data) == col_name)
  if (length(ci) == 0) return()
  for (i in seq_len(nrow(data))) {
    val_str <- data[i, ci]
    if (!is.na(val_str)) {
      r_val <- suppressWarnings(as.numeric(sub(" .*","", val_str)))
      if (!is.na(r_val))
        addStyle(wb, sheet, if(r_val<0) neg else pos, rows=i+1, cols=ci)
    }
  }
}

add_sheet <- function(wb, name, data) {
  if (is.null(data) || nrow(data) == 0) return()
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle=hs)
  setColWidths(wb, name, cols=1:ncol(data), widths="auto")
  freezePane(wb, name, firstRow=TRUE)
  coef_cols <- names(data)[grepl("_(t|l1|l2)$", names(data))]
  for (cc in coef_cols) colour_fmt_col(wb, name, data, cc)
}

# PUSH sheets — OECD countries, policy = EPS or EPI
# Columns: EPS_OECD / EPS_EU / EPI_OECD / EPI_EU, each with _t / _l1 / _l2
add_sheet(wb, "PUSH Dom GVA (OECD)",    push_dom_summary)
add_sheet(wb, "PUSH Dom GVA Fossil",
          push_dom_summary[push_dom_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PUSH For GVA (OECD)",    push_for_summary)
add_sheet(wb, "PUSH For GVA Fossil",
          push_for_summary[push_for_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PUSH Ratio (OECD)",      push_rat_summary)
add_sheet(wb, "PUSH Ratio Fossil",
          push_rat_summary[push_rat_summary$Industry %in% fossil_codes, ])

# PULL sheets — non-OECD resource-rich countries, policy = EPI
# Columns: All nonOECD / Rents>5% / Rents>2% / Rents>1% / Known / Intersect
add_sheet(wb, "PULL For GVA (nonOECD)", pull_for_summary)
add_sheet(wb, "PULL For GVA Fossil",
          pull_for_summary[pull_for_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PULL Dom GVA (nonOECD)", pull_dom_summary)
add_sheet(wb, "PULL Dom GVA Fossil",
          pull_dom_summary[pull_dom_summary$Industry %in% fossil_codes, ])
add_sheet(wb, "PULL Ratio (nonOECD)",   pull_rat_summary)
add_sheet(wb, "PULL Ratio Fossil",
          pull_rat_summary[pull_rat_summary$Industry %in% fossil_codes, ])

# Robustness — exports (OECD) and imports (non-OECD)
add_sheet(wb, "ROB Export Import",   rob_summary)
add_sheet(wb, "ROB Fossil",
          rob_summary[rob_summary$Industry %in% fossil_codes, ])

# Country diagnostic
addWorksheet(wb, "Country Subsets")
writeData(wb, "Country Subsets", country_diag, headerStyle=hs)
setColWidths(wb, "Country Subsets", cols=1:ncol(country_diag), widths="auto")
freezePane(wb, "Country Subsets", firstRow=TRUE)

out_path <- "GVA_oilrent_final_results.xlsx"
saveWorkbook(wb, out_path, overwrite=TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# EXCEL STRUCTURE (15 main sheets):
#
# PUSH Dom GVA All/Fossil  — Domestic GVA ~ EPS/EPI, OECD+EU
# PUSH For GVA All/Fossil  — Foreign GVA  ~ EPS/EPI, OECD+EU
# PUSH Ratio All/Fossil    — For/Dom ratio ~ EPS/EPI, OECD+EU (OLS+FE)
# PULL For GVA All/Fossil  — Foreign GVA  ~ EPI, 6 resource subsets
# PULL Dom GVA All/Fossil  — Domestic GVA ~ EPI, 6 resource subsets
# PULL Ratio All/Fossil    — For/Dom ratio ~ EPI, 6 resource subsets (OLS+FE)
# ROB Export Import/Fossil — Exports+imports robustness check
# Country Subsets          — oil rents ranking and subset membership
#
# RATIO INTERPRETATION:
#   PUSH ratio coef > 0: EPS rise => foreign share of GVA rises in OECD
#     => domestic firms retreating, foreign firms holding position
#   PULL ratio coef < 0: lower EPI => foreign share rises in non-OECD
#     => multinational firms expanding in weak-regulation destinations
#   Both together = ownership substitution leakage channel confirmed
#
# Sig: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10  ns not significant
# Dropped: O84 (Public admin), T97T98 (Households) — non-traded industries
# =============================================================================