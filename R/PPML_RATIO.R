# =============================================================================
# Model 3 — Pull-Side Foreign/Domestic GVA Ratio
# Tests the ownership substitution mechanism on the pull side only
# Ratio = Foreign GVA / (Domestic GVA + 1), winsorised at 99th percentile
#
# PULL: EPI x Ratio (non-OECD + Rents>2% + Rents>1%)
#       estimated at t, t-1, t-2 separately
#
# Estimator: PPML with two-way fixed effects (consistent with level models)
# Controls: GDP, Trade, Oil Rents
#
# Saved to: PPML_Ratio_results.xlsx
# Data: COMPLETE_GVA.xlsx only
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
fossil_codes    <- c("B05T09","C19","C20","C23","C24","C17T18","D35_E36T39")
drop_industries <- c("O84","T97T98")

# =============================================================================
# 2. LOAD DATA
# =============================================================================
df <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(df) <- trimws(names(df))
for (col in names(df)[!names(df) %in% c("REF_AREA","TIME_PERIOD")])
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
df <- df[order(df$REF_AREA, df$TIME_PERIOD), ]
cat("GVA rows:", nrow(df),
    "| Countries:", length(unique(df$REF_AREA)),
    "| Years:", paste(range(df$TIME_PERIOD, na.rm=TRUE), collapse="-"), "\n")

# =============================================================================
# 3. INDUSTRY CODES
# =============================================================================
gva_cols  <- names(df)[grepl("_(Domestic|Foreign)$", names(df))]
ind_codes <- unique(sub("_(Domestic|Foreign)$","", gva_cols))
ind_codes <- ind_codes[!ind_codes %in% drop_industries]
cat("Industries:", length(ind_codes), "\n")

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
# 4. LAGS
# =============================================================================
lag_within <- function(x, grp, k = 1) {
  out <- rep(NA, length(x))
  for (g in unique(grp)) {
    idx <- which(grp == g); v <- x[idx]
    out[idx] <- c(rep(NA, k), v[seq_len(length(v) - k)])
  }
  out
}

if (!"EPI" %in% names(df)) stop("EPI column not found")
df$EPI_lag1 <- lag_within(df$EPI, df$REF_AREA, 1)
df$EPI_lag2 <- lag_within(df$EPI, df$REF_AREA, 2)
cat("EPI lags created\n")

# =============================================================================
# 5. PULL-SIDE SUBSETS ONLY
# =============================================================================
df_nonoecd <- df[!df$REF_AREA %in% oecd_iso3, ]

oil_avg <- tapply(df_nonoecd$OIL_RENTS, df_nonoecd$REF_AREA,
                  mean, na.rm=TRUE)
oil_df  <- data.frame(
  REF_AREA = names(oil_avg),
  mean_oil = round(as.numeric(oil_avg), 2),
  stringsAsFactors = FALSE)
oil_df  <- oil_df[order(-oil_df$mean_oil), ]
sub_2   <- oil_df$REF_AREA[oil_df$mean_oil > 2]
sub_1   <- oil_df$REF_AREA[oil_df$mean_oil > 1]

df_rent2 <- df_nonoecd[df_nonoecd$REF_AREA %in% sub_2, ]
df_rent1 <- df_nonoecd[df_nonoecd$REF_AREA %in% sub_1, ]

cat("\nnonOECD:", length(unique(df_nonoecd$REF_AREA)),
    "| Rents>2%:", length(sub_2), "|", paste(sort(sub_2), collapse=", "),
    "\n| Rents>1%:", length(sub_1), "|", paste(sort(sub_1), collapse=", "), "\n")

# =============================================================================
# 6. PPML ESTIMATOR — for ratio dependent variable
# Ratio is non-negative so PPML is valid and consistent with level models
# Controls: log(GDP), scaled Trade, scaled Oil Rents
# =============================================================================
ppml_ratio <- function(y, pol, gdp, trade, oil, country, year) {
  keep <- !is.na(y) & !is.na(pol) &
          !is.na(gdp) & !is.na(trade) & !is.na(oil) &
          !is.na(country) & !is.na(year) & y >= 0
  y<-y[keep]; pol<-pol[keep]; gdp<-gdp[keep]
  trade<-trade[keep]; oil<-oil[keep]
  country<-country[keep]; year<-year[keep]
  n <- length(y)
  if (n < 10 || length(unique(country)) < 3) return(NULL)
  gdp   <- log(gdp + 1)
  trade <- scale(trade)[, 1]
  oil   <- scale(oil)[, 1]
  cf <- relevel(factor(country), ref=levels(factor(country))[1])
  yf <- relevel(factor(year),    ref=levels(factor(year))[1])
  Xc <- model.matrix(~gdp + trade + oil + cf + yf - 1)
  X  <- cbind(pol=pol, Xc)
  fit <- tryCatch(
    glm.fit(x=X, y=y, family=poisson(link="log"),
            control=glm.control(maxit=200, epsilon=1e-8)),
    error=function(e) NULL)
  if (is.null(fit) || !fit$converged) return(NULL)
  r  <- y - fit$fitted.values; sc <- X * r
  H  <- t(X) %*% diag(fit$fitted.values) %*% X
  Hi <- tryCatch(solve(H), error=function(e) NULL)
  if (is.null(Hi)) return(NULL)
  k <- ncol(X); B <- matrix(0, k, k)
  for (ct in unique(country)) {
    i <- country==ct; s <- colSums(sc[i,,drop=FALSE])
    B <- B + outer(s, s) }
  G <- length(unique(country))
  V <- (G/(G-1)) * Hi %*% B %*% Hi
  cv <- fit$coefficients["pol"]; sv <- sqrt(max(V[1,1], 0))
  zv <- cv/sv; pv <- 2*pnorm(-abs(zv))
  st <- function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",
                   ifelse(p<.05,"*",  ifelse(p<.10,".", "ns"))))
  list(coef=round(cv,4), se=round(sv,4), sig=st(pv),
       z=round(zv,3), p=round(pv,4), n=n, G=G)
}

# =============================================================================
# 7. RUN ONE RATIO MODEL
# =============================================================================
run_ratio <- function(df_sub, pol_col, label) {
  if (!pol_col %in% names(df_sub)) {
    cat("  Column", pol_col, "not found — skipping", label, "\n")
    return(NULL)
  }
  out <- data.frame()
  for (ind in ind_codes) {
    dc <- paste0(ind,"_Domestic"); fc <- paste0(ind,"_Foreign")
    if (!dc %in% names(df_sub) | !fc %in% names(df_sub)) next
    dom <- df_sub[[dc]]; frn <- df_sub[[fc]]
    r   <- frn / (dom + 1)
    # Values below 0.01 treated as zero — foreign ownership < 1% of domestic
    # is economically negligible and treated as zero foreign ownership.
    # This produces a zero-heavy distribution appropriate for PPML.
    r[!is.na(r) & r < 0.01] <- 0
    p99 <- quantile(r, 0.99, na.rm=TRUE)
    y   <- pmin(r, p99)
    res <- ppml_ratio(y, df_sub[[pol_col]],
                      df_sub$GDP, df_sub$TRADE, df_sub$OIL_RENTS,
                      df_sub$REF_AREA, df_sub$TIME_PERIOD)
    lbl <- ifelse(ind %in% names(industry_labels),
                  industry_labels[ind], ind)
    if (!is.null(res))
      out <- rbind(out, data.frame(
        Industry=ind, Description=lbl, Sample=label,
        Coef=res$coef, SE=res$se, Sig=res$sig,
        Z_stat=res$z, P_value=res$p,
        N_obs=res$n, N_countries=res$G,
        stringsAsFactors=FALSE))
  }
  if (nrow(out)==0) {
    cat("  WARNING: no results for", label, "\n")
    return(NULL) }
  cat(" ", label, "| industries:", nrow(out),
      "| obs:", min(out$N_obs), "-", max(out$N_obs), "\n")
  out
}

# =============================================================================
# 8. BUILD WIDE TABLE — fossil industries on top
# =============================================================================
make_table <- function(results_list) {
  non_null <- results_list[!sapply(results_list, is.null)]
  if (length(non_null)==0) return(NULL)
  base <- non_null[[1]]
  inds <- unique(base$Industry)
  out  <- data.frame()
  for (ind in inds) {
    lbl <- base$Description[base$Industry==ind][1]
    row <- data.frame(Industry=ind, Description=lbl,
                      stringsAsFactors=FALSE)
    for (nm in names(results_list)) {
      dm  <- results_list[[nm]]
      get <- function(col) {
        if (is.null(dm)) return(NA)
        r <- dm[dm$Industry==ind,]
        if (nrow(r)==0) return(NA); r[[col]][1] }
      cv <- get("Coef"); sg <- get("Sig")
      row[[paste0(nm,"_coef")]] <- if(!is.na(cv)) paste0(cv," ",sg) else NA
      row[[paste0(nm,"_se")]]   <- get("SE")
      row[[paste0(nm,"_N")]]    <- get("N_obs")
      row[[paste0(nm,"_Ncty")]] <- get("N_countries")
    }
    out <- rbind(out, row)
  }
  out$is_fossil <- out$Industry %in% fossil_codes
  out <- out[order(-out$is_fossil, out$Industry),]
  out$is_fossil <- NULL
  out
}

run_lag_ratio <- function(pol_col) {
  cat("\n---", pol_col, "---\n")
  make_table(list(
    All_nonOECD = run_ratio(df_nonoecd, pol_col, "All_nonOECD"),
    Rents_2pct  = run_ratio(df_rent2,   pol_col, "Rents_2pct"),
    Rents_1pct  = run_ratio(df_rent1,   pol_col, "Rents_1pct")))
}

# =============================================================================
# 9. RUN ALL MODELS
# =============================================================================
cat("\n=== PULL RATIO: EPI t, t-1, t-2 ===\n")
pull_rat_t  <- run_lag_ratio("EPI")
pull_rat_l1 <- run_lag_ratio("EPI_lag1")
pull_rat_l2 <- run_lag_ratio("EPI_lag2")

# =============================================================================
# 10. EXCEL EXPORT
# =============================================================================
wb <- createWorkbook()
hs <- createStyle(fontColour="#FFFFFF", fgFill="#2F4F4F",
                  textDecoration="bold", halign="center")
fos      <- createStyle(fgFill="#FFF3CD")
neg_norm <- createStyle(fontColour="#C0392B", textDecoration="bold")
pos_norm <- createStyle(fontColour="#1D6A40", textDecoration="bold")
neg_fos  <- createStyle(fontColour="#C0392B", textDecoration="bold",
                        fgFill="#FFF3CD")
pos_fos  <- createStyle(fontColour="#1D6A40", textDecoration="bold",
                        fgFill="#FFF3CD")

add_sheet <- function(wb, name, data) {
  if (is.null(data)||nrow(data)==0) {
    cat("SKIPPING", name, "— no data\n"); return() }
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle=hs)
  setColWidths(wb, name, cols=1:ncol(data), widths="auto")
  freezePane(wb, name, firstRow=TRUE)
  coef_cols <- names(data)[grepl("_coef$", names(data))]
  for (i in seq_len(nrow(data))) {
    is_fos <- data$Industry[i] %in% fossil_codes
    if (is_fos)
      addStyle(wb, name, fos, rows=i+1,
               cols=1:ncol(data), gridExpand=TRUE, stack=FALSE)
    for (cc in coef_cols) {
      ci  <- which(names(data)==cc)
      vs  <- data[i,ci]; if (is.na(vs)) next
      rv  <- suppressWarnings(as.numeric(sub(" .*","",vs)))
      if (is.na(rv)) next
      sty <- if(is_fos){ if(rv<0) neg_fos else pos_fos } else
                        { if(rv<0) neg_norm else pos_norm }
      addStyle(wb, name, sty, rows=i+1, cols=ci, stack=FALSE)
    }
  }
}

add_sheet(wb, "PULL EPI Ratio t",  pull_rat_t)
add_sheet(wb, "PULL EPI Ratio l1", pull_rat_l1)
add_sheet(wb, "PULL EPI Ratio l2", pull_rat_l2)

saveWorkbook(wb, "PPML_Ratio_results.xlsx", overwrite=TRUE)
cat("\nSaved: PPML_Ratio_results.xlsx\n")

# =============================================================================
# COUNTRY COVERAGE REPORT
# =============================================================================
controls <- c("GDP","TRADE","OIL_RENTS")
countries_with_data <- function(df_sub, pol_vars) {
  all_vars      <- c(pol_vars, controls)
  all_vars      <- all_vars[all_vars %in% names(df_sub)]
  complete_rows <- df_sub[complete.cases(df_sub[, all_vars]), ]
  counts        <- table(complete_rows$REF_AREA)
  names(counts)[counts >= 3]
}

cat("\n================================================================\n")
cat("  COUNTRY COVERAGE — Model 3 (Pull Ratio, PPML)\n")
cat("================================================================\n\n")

noecd_tested <- countries_with_data(df_nonoecd, c("EPI","EPI_lag1","EPI_lag2"))
r2_tested    <- countries_with_data(df_rent2,   c("EPI","EPI_lag1","EPI_lag2"))
r1_tested    <- countries_with_data(df_rent1,   c("EPI","EPI_lag1","EPI_lag2"))

cat("All nonOECD  :", length(noecd_tested), "countries entering\n")
cat("Rents >2%    :", length(r2_tested),
    "|", paste(sort(r2_tested), collapse=", "), "\n")
cat("Rents >1%    :", length(r1_tested),
    "|", paste(sort(r1_tested), collapse=", "), "\n")
cat("================================================================\n\n")

# =============================================================================
# INTERPRETATION GUIDE:
# Ratio = Foreign GVA / (Domestic GVA + 1)
# Values below 0.01 set to zero (foreign ownership < 1% of domestic = zero)
# This zero-heavy distribution is appropriate for PPML estimation.
#
# Negative EPI coefficient = as EPI falls (worse environmental performance),
# the Foreign/Domestic GVA ratio rises in resource-rich non-OECD countries
# — meaning foreign firms gain ownership share relative to domestic firms
# as the regulatory gap widens.
# This is the direct test of the ownership substitution channel of
# investment-led carbon leakage.
#
# Columns per sample: _coef (coef + sig), _se, _N, _Ncty
# Fossil industries amber at top | red=negative | green=positive
# Sig: *** p<.001  ** p<.01  * p<.05  . p<.10  ns=not significant
# =============================================================================