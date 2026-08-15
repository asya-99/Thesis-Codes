# =============================================================================
# Specification 2 — EPI Push (OECD) + EPI Pull (non-OECD)
# Each model uses ONE policy variable at a time (t, l1, or l2 separately)
#
# Changes from Spec 1:
#   - EPI replaces EPS on push side (broader country coverage)
#   - Financial Development dropped from controls (recovers more countries)
#   - Interest Payments already dropped (same as Spec 1)
#   - Controls: GDP, Trade, Oil Rents only
#   - Full AAMNE time span used: 2014-2022
#
# PUSH: EPI x Domestic GVA (OECD + EU)
# PULL: EPI x Foreign GVA  (non-OECD + Rents>2% + Rents>1%)
# PUSH: EPI x Ratio        (OECD + EU)
# PULL: EPI x Ratio        (non-OECD + Rents>2% + Rents>1%)
#
# 12 output sheets + country coverage report printed to console
# Saved to: PPML_Spec2_bylags.xlsx
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
eu_iso3 <- c(
  "AUT","BEL","BGR","HRV","CYP","CZE","DNK","EST","FIN","FRA",
  "DEU","GRC","HUN","IRL","ITA","LVA","LTU","LUX","MLT","NLD",
  "POL","PRT","ROU","SVK","SVN","ESP","SWE"
)
fossil_codes    <- c("B05T09","C19","C20","C23","C24","C17T18","D35_E36T39")
drop_industries <- c("O84","T97T98")

# =============================================================================
# 2. LOAD AND PREPARE DATA
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
cat("Industries after dropping O84 and T97T98:", length(ind_codes), "\n")

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

if (!"EPI" %in% names(df)) stop("EPI column not found in COMPLETE_GVA.xlsx")
df$EPI_lag1 <- lag_within(df$EPI, df$REF_AREA, 1)
df$EPI_lag2 <- lag_within(df$EPI, df$REF_AREA, 2)
cat("EPI lags created\n")

# =============================================================================
# 5. COUNTRY AND RESOURCE-RICH SUBSETS
# =============================================================================
df_oecd    <- df[df$REF_AREA %in% oecd_iso3, ]
df_eu      <- df[df$REF_AREA %in% eu_iso3, ]
df_nonoecd <- df[!df$REF_AREA %in% oecd_iso3, ]

oil_avg <- tapply(df_nonoecd$OIL_RENTS, df_nonoecd$REF_AREA,
                  mean, na.rm=TRUE)
oil_df  <- data.frame(
  REF_AREA = names(oil_avg),
  mean_oil = round(as.numeric(oil_avg), 2),
  stringsAsFactors = FALSE)
oil_df  <- oil_df[order(-oil_df$mean_oil), ]

sub_2    <- oil_df$REF_AREA[oil_df$mean_oil > 2]
sub_1    <- oil_df$REF_AREA[oil_df$mean_oil > 1]

# Filter from df_nonoecd to prevent OECD countries entering pull side
df_rent2 <- df_nonoecd[df_nonoecd$REF_AREA %in% sub_2, ]
df_rent1 <- df_nonoecd[df_nonoecd$REF_AREA %in% sub_1, ]

cat("\nOECD:", length(unique(df_oecd$REF_AREA)),
    "| EU:", length(unique(df_eu$REF_AREA)),
    "| nonOECD:", length(unique(df_nonoecd$REF_AREA)),
    "| Rents>2%:", length(sub_2),
    "| Rents>1%:", length(sub_1), "\n")
cat("Rents >2%:", paste(sort(sub_2), collapse=", "), "\n")
cat("Rents >1%:", paste(sort(sub_1), collapse=", "), "\n")

# =============================================================================
# 6. PPML ESTIMATOR — single EPI variable, no Interest Payments, no FinDev
# Controls: log(GDP), scaled Trade, scaled Oil Rents
# =============================================================================
ppml_single <- function(y, pol, gdp, trade, oil, country, year) {
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
                   ifelse(p<.05, "*",  ifelse(p<.10, ".", "ns"))))
  list(coef=round(cv,4), se=round(sv,4), sig=st(pv),
       z=round(zv,3), p=round(pv,4), n=n, G=G)
}

# =============================================================================
# 7. OLS WITH WITHIN FE — for ratio (continuous, can be negative)
# =============================================================================
ols_single <- function(y, pol, gdp, trade, oil, country, year) {
  keep <- !is.na(y) & !is.na(pol) &
          !is.na(gdp) & !is.na(trade) & !is.na(oil) &
          !is.na(country) & !is.na(year) & is.finite(y)
  y<-y[keep]; pol<-pol[keep]
  gdp   <- log(gdp[keep] + 1)
  trade <- scale(trade[keep])[, 1]
  oil   <- scale(oil[keep])[, 1]
  country<-country[keep]; year<-year[keep]
  n <- length(y)
  if (n < 10 || length(unique(country)) < 3) return(NULL)
  dm <- function(x) {
    grand <- mean(x); cm <- ave(x, country, FUN=mean)
    ym    <- ave(x, year, FUN=mean); x - cm - ym + grand }
  yw <- dm(y); pw <- dm(pol); gw <- dm(gdp)
  tw <- dm(trade); ow <- dm(oil)
  fit <- lm(yw ~ pw + gw + tw + ow - 1)
  X   <- model.matrix(fit); e <- residuals(fit)
  Xi  <- tryCatch(solve(t(X)%*%X), error=function(e) NULL)
  if (is.null(Xi)) return(NULL)
  k <- ncol(X); B <- matrix(0, k, k)
  for (ct in unique(country)) {
    i <- country==ct; Xc <- X[i,,drop=FALSE]; ec <- e[i]
    s <- t(Xc)%*%ec; B <- B + s%*%t(s) }
  G   <- length(unique(country))
  adj <- (G/(G-1)) * ((n-1)/(n-k))
  V   <- adj * Xi %*% B %*% Xi
  cv  <- coef(fit)["pw"]; sv <- sqrt(max(V[1,1], 0))
  tv  <- cv/sv; pv <- 2*pt(-abs(tv), df=G-1)
  st  <- function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",
                    ifelse(p<.05, "*",  ifelse(p<.10, ".", "ns"))))
  list(coef=round(cv,4), se=round(sv,4), sig=st(pv),
       z=round(tv,3), p=round(pv,4), n=n, G=G)
}

# =============================================================================
# 8. RUN ONE MODEL
# =============================================================================
run_one <- function(df_sub, pol_col, dep, label) {
  if (!pol_col %in% names(df_sub)) {
    cat("  Column", pol_col, "not found — skipping", label, "\n")
    return(NULL)
  }
  out <- data.frame()
  for (ind in ind_codes) {
    dc <- paste0(ind,"_Domestic"); fc <- paste0(ind,"_Foreign")
    if (!dc %in% names(df_sub) | !fc %in% names(df_sub)) next
    dom <- df_sub[[dc]]; frn <- df_sub[[fc]]
    if (dep == "Domestic")     { y <- dom; est <- "ppml" }
    else if (dep == "Foreign") { y <- frn; est <- "ppml" }
    else {
      r   <- frn/(dom+1)
      p99 <- quantile(r, 0.99, na.rm=TRUE)
      y   <- pmin(r, p99); est <- "ols" }
    res <- if (est == "ppml")
      ppml_single(y, df_sub[[pol_col]],
                  df_sub$GDP, df_sub$TRADE, df_sub$OIL_RENTS,
                  df_sub$REF_AREA, df_sub$TIME_PERIOD)
    else
      ols_single(y, df_sub[[pol_col]],
                 df_sub$GDP, df_sub$TRADE, df_sub$OIL_RENTS,
                 df_sub$REF_AREA, df_sub$TIME_PERIOD)
    lbl  <- ifelse(ind %in% names(industry_labels),
                   industry_labels[ind], ind)
    y_sd <- round(sd(y[is.finite(y)], na.rm=TRUE), 4)
    if (!is.null(res))
      out <- rbind(out, data.frame(
        Industry=ind, Description=lbl, Sample=label,
        Coef=res$coef, SE=res$se, Sig=res$sig,
        Z_stat=res$z, P_value=res$p,
        N_obs=res$n, N_countries=res$G, SD_y=y_sd,
        stringsAsFactors=FALSE))
  }
  if (nrow(out) == 0) {
    cat("  WARNING: no results for", label, dep, "\n")
    return(NULL)
  }
  cat(" ", label, "|", dep,
      "| industries:", nrow(out),
      "| obs:", min(out$N_obs), "-", max(out$N_obs), "\n")
  out
}

# =============================================================================
# 9. BUILD WIDE TABLE — fossil industries sorted to top
# =============================================================================
make_table <- function(results_list) {
  non_null <- results_list[!sapply(results_list, is.null)]
  if (length(non_null) == 0) return(NULL)
  base <- non_null[[1]]
  inds <- unique(base$Industry)
  out  <- data.frame()
  for (ind in inds) {
    lbl <- base$Description[base$Industry == ind][1]
    row <- data.frame(Industry=ind, Description=lbl,
                      stringsAsFactors=FALSE)
    for (nm in names(results_list)) {
      dm  <- results_list[[nm]]
      get <- function(col) {
        if (is.null(dm)) return(NA)
        r <- dm[dm$Industry == ind, ]
        if (nrow(r) == 0) return(NA); r[[col]][1] }
      cv <- get("Coef"); sg <- get("Sig")
      row[[paste0(nm,"_coef")]] <- if(!is.na(cv)) paste0(cv," ",sg) else NA
      row[[paste0(nm,"_se")]]   <- get("SE")
      row[[paste0(nm,"_N")]]    <- get("N_obs")
      row[[paste0(nm,"_Ncty")]] <- get("N_countries")
    }
    out <- rbind(out, row)
  }
  out$is_fossil <- out$Industry %in% fossil_codes
  out <- out[order(-out$is_fossil, out$Industry), ]
  out$is_fossil <- NULL
  out
}

# =============================================================================
# 10. RUN ALL 12 MODELS
# =============================================================================
run_lag <- function(pol_col, dep, side) {
  cat("\n---", pol_col, dep, "---\n")
  if (side == "push") {
    make_table(list(
      OECD = run_one(df_oecd, pol_col, dep, "OECD"),
      EU   = run_one(df_eu,   pol_col, dep, "EU")))
  } else {
    make_table(list(
      All_nonOECD = run_one(df_nonoecd, pol_col, dep, "All_nonOECD"),
      Rents_2pct  = run_one(df_rent2,   pol_col, dep, "Rents_2pct"),
      Rents_1pct  = run_one(df_rent1,   pol_col, dep, "Rents_1pct")))
  }
}

cat("\n=== PUSH: EPI x Domestic GVA ===\n")
push_dom_t  <- run_lag("EPI",      "Domestic", "push")
push_dom_l1 <- run_lag("EPI_lag1", "Domestic", "push")
push_dom_l2 <- run_lag("EPI_lag2", "Domestic", "push")

cat("\n=== PUSH: EPI x Ratio ===\n")
push_rat_t  <- run_lag("EPI",      "Ratio", "push")
push_rat_l1 <- run_lag("EPI_lag1", "Ratio", "push")
push_rat_l2 <- run_lag("EPI_lag2", "Ratio", "push")

cat("\n=== PULL: EPI x Foreign GVA ===\n")
pull_for_t  <- run_lag("EPI",      "Foreign", "pull")
pull_for_l1 <- run_lag("EPI_lag1", "Foreign", "pull")
pull_for_l2 <- run_lag("EPI_lag2", "Foreign", "pull")

cat("\n=== PULL: EPI x Ratio ===\n")
pull_rat_t  <- run_lag("EPI",      "Ratio", "pull")
pull_rat_l1 <- run_lag("EPI_lag1", "Ratio", "pull")
pull_rat_l2 <- run_lag("EPI_lag2", "Ratio", "pull")

# =============================================================================
# 11. EXCEL EXPORT
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
  if (is.null(data) || nrow(data) == 0) {
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
      ci  <- which(names(data) == cc)
      vs  <- data[i, ci]; if (is.na(vs)) next
      rv  <- suppressWarnings(as.numeric(sub(" .*","", vs)))
      if (is.na(rv)) next
      sty <- if(is_fos){ if(rv<0) neg_fos else pos_fos } else
                        { if(rv<0) neg_norm else pos_norm }
      addStyle(wb, name, sty, rows=i+1, cols=ci, stack=FALSE)
    }
  }
}

add_sheet(wb, "PUSH EPI Dom GVA t",  push_dom_t)
add_sheet(wb, "PUSH EPI Dom GVA l1", push_dom_l1)
add_sheet(wb, "PUSH EPI Dom GVA l2", push_dom_l2)
add_sheet(wb, "PUSH EPI Ratio t",    push_rat_t)
add_sheet(wb, "PUSH EPI Ratio l1",   push_rat_l1)
add_sheet(wb, "PUSH EPI Ratio l2",   push_rat_l2)
add_sheet(wb, "PULL EPI For GVA t",  pull_for_t)
add_sheet(wb, "PULL EPI For GVA l1", pull_for_l1)
add_sheet(wb, "PULL EPI For GVA l2", pull_for_l2)
add_sheet(wb, "PULL EPI Ratio t",    pull_rat_t)
add_sheet(wb, "PULL EPI Ratio l1",   pull_rat_l1)
add_sheet(wb, "PULL EPI Ratio l2",   pull_rat_l2)

saveWorkbook(wb, "PPML_Spec2_bylags.xlsx", overwrite=TRUE)
cat("\nSaved: PPML_Spec2_bylags.xlsx\n")

# =============================================================================
# 12. COUNTRY COVERAGE REPORT
# =============================================================================
controls <- c("GDP","TRADE","OIL_RENTS")

countries_with_data <- function(df_sub, pol_vars) {
  all_vars      <- c(pol_vars, controls)
  all_vars      <- all_vars[all_vars %in% names(df_sub)]
  complete_rows <- df_sub[complete.cases(df_sub[, all_vars]), ]
  counts        <- table(complete_rows$REF_AREA)
  names(counts)[counts >= 3]
}

cat("\n")
cat("================================================================\n")
cat("  COUNTRY COVERAGE REPORT — Specification 2\n")
cat("  Policy: EPI (push + pull) | Controls: GDP, Trade, Oil Rents\n")
cat("  No Interest Payments | No Financial Development\n")
cat("================================================================\n\n")

oecd_tested   <- countries_with_data(df_oecd,
                   c("EPI","EPI_lag1","EPI_lag2"))
eu_tested     <- countries_with_data(df_eu,
                   c("EPI","EPI_lag1","EPI_lag2"))
noecd_tested  <- countries_with_data(df_nonoecd,
                   c("EPI","EPI_lag1","EPI_lag2"))
r2_tested     <- countries_with_data(df_rent2,
                   c("EPI","EPI_lag1","EPI_lag2"))
r1_tested     <- countries_with_data(df_rent1,
                   c("EPI","EPI_lag1","EPI_lag2"))

oecd_dropped  <- setdiff(unique(df_oecd$REF_AREA),    oecd_tested)
eu_dropped    <- setdiff(unique(df_eu$REF_AREA),      eu_tested)
noecd_dropped <- setdiff(unique(df_nonoecd$REF_AREA), noecd_tested)
r2_dropped    <- setdiff(unique(df_rent2$REF_AREA),   r2_tested)
r1_dropped    <- setdiff(unique(df_rent1$REF_AREA),   r1_tested)

cat("PUSH — OECD sample:\n")
cat(sprintf("  Entering : %d\n", length(oecd_tested)))
cat(sprintf("  Excluded : %d", length(oecd_dropped)))
if (length(oecd_dropped)>0)
  cat(" |", paste(sort(oecd_dropped), collapse=", "))
cat("\n\n")

cat("PUSH — EU subsample:\n")
cat(sprintf("  Entering : %d\n", length(eu_tested)))
cat(sprintf("  Excluded : %d", length(eu_dropped)))
if (length(eu_dropped)>0)
  cat(" |", paste(sort(eu_dropped), collapse=", "))
cat("\n  Note: EU tested separately — EU countries also in OECD model\n\n")

cat("PULL — All non-OECD:\n")
cat(sprintf("  Entering : %d\n", length(noecd_tested)))
cat(sprintf("  Excluded : %d", length(noecd_dropped)))
if (length(noecd_dropped)>0)
  cat(" |", paste(sort(noecd_dropped), collapse=", "))
cat("\n\n")

cat("PULL — Rents >2% subset:\n")
cat(sprintf("  In subset: %d | %s\n",
    length(unique(df_rent2$REF_AREA)),
    paste(sort(unique(df_rent2$REF_AREA)), collapse=", ")))
cat(sprintf("  Entering : %d", length(r2_tested)))
if (length(r2_tested)>0)
  cat(" |", paste(sort(r2_tested), collapse=", "))
cat("\n")
cat(sprintf("  Excluded : %d", length(r2_dropped)))
if (length(r2_dropped)>0)
  cat(" |", paste(sort(r2_dropped), collapse=", "))
cat("\n\n")

cat("PULL — Rents >1% subset:\n")
cat(sprintf("  In subset: %d | %s\n",
    length(unique(df_rent1$REF_AREA)),
    paste(sort(unique(df_rent1$REF_AREA)), collapse=", ")))
cat(sprintf("  Entering : %d", length(r1_tested)))
if (length(r1_tested)>0)
  cat(" |", paste(sort(r1_tested), collapse=", "))
cat("\n")
cat(sprintf("  Excluded : %d", length(r1_dropped)))
if (length(r1_dropped)>0)
  cat(" |", paste(sort(r1_dropped), collapse=", "))
cat("\n\n")

all_tested <- unique(c(oecd_tested, noecd_tested))
cat("----------------------------------------------------------------\n")
cat(sprintf("TOTAL unique countries: OECD (%d) + nonOECD (%d) = %d\n",
    length(oecd_tested), length(noecd_tested), length(all_tested)))
cat("Full list:", paste(sort(all_tested), collapse=", "), "\n")
cat("================================================================\n\n")