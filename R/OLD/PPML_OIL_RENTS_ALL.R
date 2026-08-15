# =============================================================================
# PPML — Resource-Rich Country Subset Analysis
# Full push/pull framework matching ppml_final.R structure
#
# PUSH models (OECD countries — domestic exports + domestic GVA):
#   EPS x Domestic Exports (OECD, OECD+EU robustness)
#   EPI x Domestic Exports (OECD, OECD+EU robustness)
#   EPS x Domestic GVA     (OECD, OECD+EU robustness)
#   EPI x Domestic GVA     (OECD, OECD+EU robustness)
#
# PULL models (non-OECD countries — foreign imports + foreign GVA):
#   EPI x Foreign Imports  (All non-OECD / rents>5% / rents>2% /
#                           known producers / intersection)
#   EPI x Foreign GVA      (same subsets)
#
# Resource-rich subsets for pull side:
#   Method 1 — OIL_RENTS > threshold (data-driven, already in your data)
#   Method 2 — Known fossil producer list (theory-driven)
#   Method 3 — Intersection of both (most conservative)
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

# =============================================================================
# 2. LOAD DATA
# =============================================================================
df_export <- read_excel("COMPLETE_EXPORT.xlsx", sheet = "Sheet1")
df_import <- read_excel("COMPLETE_IMPORT.xlsx", sheet = "Sheet1")
df_gva    <- read_excel("COMPLETE_GVA.xlsx",    sheet = "Sheet1")

names(df_export) <- trimws(names(df_export))
names(df_import) <- trimws(names(df_import))
names(df_gva)    <- trimws(names(df_gva))

to_numeric_cols <- function(df) {
  skip <- c("REF_AREA","TIME_PERIOD")
  for (col in names(df)[!names(df) %in% skip])
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
  df
}

df_export <- to_numeric_cols(df_export)
df_import <- to_numeric_cols(df_import)
df_gva    <- to_numeric_cols(df_gva)

cat("Export data:", nrow(df_export), "rows |",
    length(unique(df_export$REF_AREA)), "countries\n")
cat("Import data:", nrow(df_import), "rows |",
    length(unique(df_import$REF_AREA)), "countries\n")
cat("GVA data:   ", nrow(df_gva),    "rows |",
    length(unique(df_gva$REF_AREA)),    "countries\n")

# =============================================================================
# 3. DEFINE RESOURCE-RICH SUBSETS FROM IMPORT DATA
# (OIL_RENTS is country-level so we use import file for subsetting)
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
  mean_oil_rents = round(as.numeric(oil_rents_avg), 2)
)[order(-as.numeric(oil_rents_avg)), ]

subset_5pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 5]
subset_2pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 2]
subset_1pct     <- oil_rents_df$REF_AREA[oil_rents_df$mean_oil_rents > 1]
subset_known    <- nonoecd_in_data[nonoecd_in_data %in% fossil_producers_nonoecd]
subset_intersect<- intersect(subset_2pct, subset_known)

# =============================================================================
# COUNTRY DIAGNOSTIC TABLE
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
cat(sprintf("%-8s  %12s  %7s  %7s  %7s  %7s  %10s\n",
    "Country", "Oil Rents %", ">5%", ">2%", ">1%", "Known", "Intersect"))
cat(paste(rep("-", 68), collapse=""), "\n")
for (i in seq_len(nrow(country_diag))) {
  cat(sprintf("%-8s  %12.2f  %7s  %7s  %7s  %7s  %10s\n",
    country_diag$REF_AREA[i],
    country_diag$Oil_Rents_Avg[i],
    ifelse(country_diag$In_Rents_5pct[i],  "YES", "-"),
    ifelse(country_diag$In_Rents_2pct[i],  "YES", "-"),
    ifelse(country_diag$In_Rents_1pct[i],  "YES", "-"),
    ifelse(country_diag$Known_Producer[i], "YES", "-"),
    ifelse(country_diag$In_Intersect[i],   "YES", "-")
  ))
}
cat(paste(rep("-", 68), collapse=""), "\n")

cat("\nSUMMARY:\n")
cat(sprintf("  All non-OECD in data : %d countries\n", length(nonoecd_in_data)))
cat(sprintf("  Oil rents > 5%% GDP   : %d countries  [%s]\n",
    length(subset_5pct), paste(sort(subset_5pct), collapse=", ")))
cat(sprintf("  Oil rents > 2%% GDP   : %d countries  [%s]\n",
    length(subset_2pct), paste(sort(subset_2pct), collapse=", ")))
cat(sprintf("  Oil rents > 1%% GDP   : %d countries  [%s]\n",
    length(subset_1pct), paste(sort(subset_1pct), collapse=", ")))
cat(sprintf("  Known producers      : %d countries  [%s]\n",
    length(subset_known), paste(sort(subset_known), collapse=", ")))
cat(sprintf("  Rents>2%% AND Known   : %d countries  [%s]\n",
    length(subset_intersect), paste(sort(subset_intersect), collapse=", ")))
cat("=================================================================\n\n")

# Auto-fallback if rents>5% has fewer than 3 countries
if (length(subset_5pct) < 3) {
  cat("WARNING: Oil rents >5% has only", length(subset_5pct),
      "countries — too few for PPML.\n")
  cat("Automatically using >2% cut for the Rents >5% column.\n\n")
  subset_5pct <- subset_2pct
}

# =============================================================================
# 4. CREATE LAGS WITHIN COUNTRY (t-1 and t-2)
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

df_export <- add_lags(df_export, c("EPS","EPI"))
df_import <- add_lags(df_import, "EPI")
df_gva    <- add_lags(df_gva,    c("EPS","EPI"))

# =============================================================================
# 5. INDUSTRY LABELS
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
  N77T82="Admin & support services",        O84="Public admin & defence",
  P85="Education",                          Q86T88="Health & social work",
  R90T93="Arts & entertainment",            S94T96="Other service activities",
  T97T98="Households as employers"
)

gva_cols       <- names(df_export)[grepl("_(Domestic|Foreign)$", names(df_export))]
industry_codes <- unique(sub("_(Domestic|Foreign)$", "", gva_cols))

# =============================================================================
# 6. PPML FUNCTION — three lags simultaneously, clustered SE by country
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

  stars <- function(p) ifelse(p < 0.001,"***", ifelse(p < 0.01,"**",
                       ifelse(p < 0.05, "*",   ifelse(p < 0.10,".","ns"))))

  extract <- function(name, vi) {
    cv <- fit$coefficients[name]
    sv <- sqrt(max(V[vi,vi], 0))
    zv <- cv / sv
    pv <- 2 * pnorm(-abs(zv))
    list(coef=round(cv,4), se=round(sv,4),
         z=round(zv,3), p=round(pv,4), sig=stars(pv))
  }

  list(t=extract("pol_t",1), l1=extract("pol_l1",2),
       l2=extract("pol_l2",3), n=n, G=G)
}

# =============================================================================
# 7. GENERIC RUN FUNCTION
# df:       source data (export or import file)
# policy_t/l1/l2: column names for the three lags
# gva_type: "Domestic" or "Foreign"
# country_subset: vector of ISO3 codes to keep
# sample_label: name for output
# =============================================================================
run_model <- function(df, policy_t, policy_l1, policy_l2,
                      gva_type, sample_label) {

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
        Industry    = ind,  Description = label,
        Sample      = sample_label,
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

# Subset wrapper — filters df then calls run_model
run_subset <- function(df, country_subset, policy_t, policy_l1, policy_l2,
                        gva_type, sample_label) {
  df_sub <- df[df$REF_AREA %in% country_subset, ]
  n_countries <- length(unique(df_sub$REF_AREA))
  if (n_countries < 3) {
    cat("  Skipping", sample_label, "— only", n_countries, "countries (need 3+)\n")
    return(NULL)
  }
  if (n_countries < 5) {
    cat("  NOTE:", sample_label, "has only", n_countries,
        "countries — interpret results with caution\n")
  }
  cat(" ", sample_label, "| countries:", length(unique(df_sub$REF_AREA)), "\n")
  run_model(df_sub, policy_t, policy_l1, policy_l2, gva_type, sample_label)
}

# =============================================================================
# 8. RUN ALL MODELS
# =============================================================================

# ---- PUSH: OECD domestic exports ----
cat("\n=== PUSH: EPS x Domestic Exports (OECD) ===\n")
push_exp_eps_oecd <- run_subset(df_export, oecd_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS OECD")
push_exp_eps_eu   <- run_subset(df_export, eu_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS EU")

cat("\n=== PUSH: EPI x Domestic Exports (OECD) ===\n")
push_exp_epi_oecd <- run_subset(df_export, oecd_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI OECD")
push_exp_epi_eu   <- run_subset(df_export, eu_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI EU")

# ---- PUSH: OECD domestic GVA (from COMPLETE_GVA) ----
cat("\n=== PUSH: EPS x Domestic GVA (OECD) ===\n")
push_gva_dom_eps_oecd <- run_subset(df_gva, oecd_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS OECD")
push_gva_dom_eps_eu   <- run_subset(df_gva, eu_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Domestic", "EPS EU")

cat("\n=== PUSH: EPI x Domestic GVA (OECD) ===\n")
push_gva_dom_epi_oecd <- run_subset(df_gva, oecd_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI OECD")
push_gva_dom_epi_eu   <- run_subset(df_gva, eu_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "EPI EU")

# ---- PUSH: OECD foreign GVA (from COMPLETE_GVA — ownership substitution) ----
cat("\n=== PUSH: EPS x Foreign GVA (OECD) ===\n")
push_gva_for_eps_oecd <- run_subset(df_gva, oecd_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Foreign", "EPS OECD")
push_gva_for_eps_eu   <- run_subset(df_gva, eu_iso3,
  "EPS","EPS_lag1","EPS_lag2", "Foreign", "EPS EU")

cat("\n=== PUSH: EPI x Foreign GVA (OECD) ===\n")
push_gva_for_epi_oecd <- run_subset(df_gva, oecd_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "EPI OECD")
push_gva_for_epi_eu   <- run_subset(df_gva, eu_iso3,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "EPI EU")

# ---- PULL: non-OECD foreign imports — resource-rich subsets ----
cat("\n=== PULL: EPI x Foreign Imports (resource-rich subsets) ===\n")
pull_imp_all       <- run_subset(df_import, nonoecd_in_data,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "All non-OECD")
pull_imp_rents5    <- run_subset(df_import, subset_5pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >5%")
pull_imp_rents2    <- run_subset(df_import, subset_2pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >2%")
pull_imp_rents1    <- run_subset(df_import, subset_1pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >1%")
pull_imp_known     <- run_subset(df_import, subset_known,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Known producers")
pull_imp_intersect <- run_subset(df_import, subset_intersect,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents>2% & Known")

# ---- PULL: non-OECD domestic GVA (local firms in resource-rich countries) ----
cat("\n=== PULL: EPI x Domestic GVA — non-OECD resource-rich ===\n")
pull_gva_dom_all       <- run_subset(df_gva, nonoecd_in_data,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "All non-OECD")
pull_gva_dom_rents5    <- run_subset(df_gva, subset_5pct,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "Rents >5%")
pull_gva_dom_rents2    <- run_subset(df_gva, subset_2pct,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "Rents >2%")
pull_gva_dom_rents1    <- run_subset(df_gva, subset_1pct,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "Rents >1%")
pull_gva_dom_known     <- run_subset(df_gva, subset_known,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "Known producers")
pull_gva_dom_intersect <- run_subset(df_gva, subset_intersect,
  "EPI","EPI_lag1","EPI_lag2", "Domestic", "Rents>2% & Known")

# ---- PULL: non-OECD foreign GVA (multinationals in resource-rich countries) ----
cat("\n=== PULL: EPI x Foreign GVA — non-OECD resource-rich ===\n")
pull_gva_for_all       <- run_subset(df_gva, nonoecd_in_data,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "All non-OECD")
pull_gva_for_rents5    <- run_subset(df_gva, subset_5pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >5%")
pull_gva_for_rents2    <- run_subset(df_gva, subset_2pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >2%")
pull_gva_for_rents1    <- run_subset(df_gva, subset_1pct,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents >1%")
pull_gva_for_known     <- run_subset(df_gva, subset_known,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Known producers")
pull_gva_for_intersect <- run_subset(df_gva, subset_intersect,
  "EPI","EPI_lag1","EPI_lag2", "Foreign", "Rents>2% & Known")

# =============================================================================
# 9. BUILD SUMMARY TABLES
# One row per industry, each sample as a set of columns (t, l1, l2)
# =============================================================================
build_summary <- function(results_list) {
  base   <- results_list[[which(!sapply(results_list, is.null))[1]]]
  inds   <- unique(base$Industry)
  out    <- data.frame()

  fmt <- function(df, ind, cc, sc) {
    if (is.null(df)) return(NA)
    sub <- df[df$Industry == ind, ]
    if (nrow(sub) == 0 || is.na(sub[[cc]])) return(NA)
    paste0(sub[[cc]], " ", sub[[sc]])
  }

  for (ind in inds) {
    label <- base$Description[base$Industry == ind][1]
    row   <- data.frame(Industry=ind, Description=label, stringsAsFactors=FALSE)
    for (mname in names(results_list)) {
      df_m <- results_list[[mname]]
      row[[paste0(mname,"_t")]]  <- fmt(df_m, ind, "coef_t",  "sig_t")
      row[[paste0(mname,"_l1")]] <- fmt(df_m, ind, "coef_l1", "sig_l1")
      row[[paste0(mname,"_l2")]] <- fmt(df_m, ind, "coef_l2", "sig_l2")
    }
    out <- rbind(out, row)
  }

  # Sort by first available l1 column (most negative first)
  l1_cols  <- grep("_l1$", names(out), value=TRUE)
  sort_key <- as.numeric(sub(" .*","", out[[l1_cols[1]]]))
  out[order(sort_key), ]
}

# Push summaries
push_exp_summary <- build_summary(list(
  "EPS_OECD" = push_exp_eps_oecd,
  "EPS_EU"   = push_exp_eps_eu,
  "EPI_OECD" = push_exp_epi_oecd,
  "EPI_EU"   = push_exp_epi_eu
))

push_gva_dom_summary <- build_summary(list(
  "EPS_OECD" = push_gva_dom_eps_oecd,
  "EPS_EU"   = push_gva_dom_eps_eu,
  "EPI_OECD" = push_gva_dom_epi_oecd,
  "EPI_EU"   = push_gva_dom_epi_eu
))

push_gva_for_summary <- build_summary(list(
  "EPS_OECD" = push_gva_for_eps_oecd,
  "EPS_EU"   = push_gva_for_eps_eu,
  "EPI_OECD" = push_gva_for_epi_oecd,
  "EPI_EU"   = push_gva_for_epi_eu
))

# Pull summaries — resource-rich subsets
pull_imp_summary <- build_summary(list(
  "All_nonOECD" = pull_imp_all,
  "Rents_5pct"  = pull_imp_rents5,
  "Rents_2pct"  = pull_imp_rents2,
  "Rents_1pct"  = pull_imp_rents1,
  "Known"       = pull_imp_known,
  "Intersect"   = pull_imp_intersect
))

pull_gva_dom_summary <- build_summary(list(
  "All_nonOECD" = pull_gva_dom_all,
  "Rents_5pct"  = pull_gva_dom_rents5,
  "Rents_2pct"  = pull_gva_dom_rents2,
  "Rents_1pct"  = pull_gva_dom_rents1,
  "Known"       = pull_gva_dom_known,
  "Intersect"   = pull_gva_dom_intersect
))

pull_gva_for_summary <- build_summary(list(
  "All_nonOECD" = pull_gva_for_all,
  "Rents_5pct"  = pull_gva_for_rents5,
  "Rents_2pct"  = pull_gva_for_rents2,
  "Rents_1pct"  = pull_gva_for_rents1,
  "Known"       = pull_gva_for_known,
  "Intersect"   = pull_gva_for_intersect
))

# =============================================================================
# 10. EXPORT TO EXCEL
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
        addStyle(wb, sheet, if(r_val < 0) neg else pos, rows=i+1, cols=ci)
    }
  }
}

add_sheet <- function(wb, name, data) {
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle=hs)
  setColWidths(wb, name, cols=1:ncol(data), widths="auto")
  freezePane(wb, name, firstRow=TRUE)
  coef_cols <- names(data)[grepl("_(t|l1|l2)$", names(data))]
  for (cc in coef_cols) colour_fmt_col(wb, name, data, cc)
}

fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")

# PUSH sheets
add_sheet(wb, "PUSH Export All",
          push_exp_summary)
add_sheet(wb, "PUSH Export Fossil",
          push_exp_summary[push_exp_summary$Industry %in% fossil_codes, ])

add_sheet(wb, "PUSH GVA Dom All",
          push_gva_dom_summary)
add_sheet(wb, "PUSH GVA Dom Fossil",
          push_gva_dom_summary[push_gva_dom_summary$Industry %in% fossil_codes, ])

add_sheet(wb, "PUSH GVA For All",
          push_gva_for_summary)
add_sheet(wb, "PUSH GVA For Fossil",
          push_gva_for_summary[push_gva_for_summary$Industry %in% fossil_codes, ])

# PULL sheets — resource-rich subsets
add_sheet(wb, "PULL Import All",
          pull_imp_summary)
add_sheet(wb, "PULL Import Fossil",
          pull_imp_summary[pull_imp_summary$Industry %in% fossil_codes, ])

add_sheet(wb, "PULL GVA Dom All",
          pull_gva_dom_summary)
add_sheet(wb, "PULL GVA Dom Fossil",
          pull_gva_dom_summary[pull_gva_dom_summary$Industry %in% fossil_codes, ])

add_sheet(wb, "PULL GVA For All",
          pull_gva_for_summary)
add_sheet(wb, "PULL GVA For Fossil",
          pull_gva_for_summary[pull_gva_for_summary$Industry %in% fossil_codes, ])

# Country composition sheet
country_info <- data.frame(
  REF_AREA      = nonoecd_in_data,
  Rents_5pct    = nonoecd_in_data %in% subset_5pct,
  Rents_2pct    = nonoecd_in_data %in% subset_2pct,
  Rents_1pct    = nonoecd_in_data %in% subset_1pct,
  Known_producer= nonoecd_in_data %in% subset_known,
  Intersect     = nonoecd_in_data %in% subset_intersect,
  stringsAsFactors = FALSE
)
country_info <- merge(country_info, oil_rents_df, by="REF_AREA", all.x=TRUE)
country_info <- country_info[order(-country_info$mean_oil_rents), ]

addWorksheet(wb, "Country Subsets")
writeData(wb, "Country Subsets", country_info, headerStyle=hs)
setColWidths(wb, "Country Subsets", cols=1:ncol(country_info), widths="auto")
freezePane(wb, "Country Subsets", firstRow=TRUE)

out_path <- "PPML_ResourceRich_results.xlsx"
saveWorkbook(wb, out_path, overwrite=TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# EXCEL OUTPUT STRUCTURE (8 main sheets + country sheet):
#
# PUSH Export All/Fossil  — EPS/EPI x Domestic Exports, OECD+EU samples
# PUSH GVA All/Fossil     — EPS/EPI x Domestic GVA,     OECD+EU samples
# PULL Import All/Fossil  — EPI x Foreign Imports, 6 resource-rich subsets
# PULL GVA All/Fossil     — EPI x Foreign GVA,     6 resource-rich subsets
# Country Subsets         — which non-OECD countries are in each subset
#
# COLUMN GROUPS (push tables):
#   EPS_OECD_t/l1/l2 — EPS effect on OECD domestic exports/GVA
#   EPS_EU_t/l1/l2   — EPS effect on EU domestic exports/GVA (robustness)
#   EPI_OECD_t/l1/l2 — EPI effect on OECD domestic exports/GVA
#   EPI_EU_t/l1/l2   — EPI effect on EU domestic exports/GVA (robustness)
#
# COLUMN GROUPS (pull tables):
#   All_nonOECD_t/l1/l2  — baseline (all non-OECD, likely diluted)
#   Rents_5pct_t/l1/l2   — oil rents > 5% of GDP
#   Rents_2pct_t/l1/l2   — oil rents > 2% of GDP
#   Rents_1pct_t/l1/l2   — oil rents > 1% of GDP
#   Known_t/l1/l2         — known fossil producers in data
#   Intersect_t/l1/l2     — rents>2% AND known producer (most conservative)
#
# KEY SIGNAL TO LOOK FOR:
#   Pull coefficients becoming more negative and significant as you move
#   from All_nonOECD → Rents_5pct → Known → Intersect confirms that
#   composition bias was diluting the leakage signal in the full sample.
#   This progression IS the finding for resource-rich leakage.
#
# Sig: *** p<0.001  ** p<0.01  * p<0.05  . p<0.10  ns not significant
# =============================================================================