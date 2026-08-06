# =============================================================================
# PPML — GVA Push/Pull Framework (Clean Version)
#
# PUSH (OECD): Domestic GVA, Foreign GVA, Ratio ~ EPS/EPI/CCPI + controls
# PULL (non-OECD resource-rich): same ~ EPI/CCPI + controls
#
# Output: one Excel file, 6 sheets (no export/import, no separate fossil sheets)
# Fossil-fuel industries sorted to TOP of each table
# =============================================================================

library(readxl)
library(openxlsx)

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
  "BRA","TTO","VEN","ECU","BOL","PAK","IND"
)

fossil_codes <- c("B05T09","C19","C20","C23","C24","C17T18","D35_E36T39")
drop_industries <- c("O84","T97T98")

# =============================================================================
# 1. LOAD DATA
# =============================================================================
df_gva <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(df_gva) <- trimws(names(df_gva))

to_num <- function(df) {
  for (col in names(df)[!names(df) %in% c("REF_AREA","TIME_PERIOD")])
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
  df
}
df_gva <- to_num(df_gva)

cat("GVA rows:", nrow(df_gva),
    "| Countries:", length(unique(df_gva$REF_AREA)), "\n")

# =============================================================================
# 2. RESOURCE-RICH SUBSETS
# =============================================================================
nonoecd <- unique(df_gva$REF_AREA[!df_gva$REF_AREA %in% oecd_iso3])

oil_avg <- tapply(
  df_gva$OIL_RENTS[!df_gva$REF_AREA %in% oecd_iso3],
  df_gva$REF_AREA[!df_gva$REF_AREA %in% oecd_iso3],
  mean, na.rm=TRUE)

oil_df <- data.frame(
  REF_AREA=names(oil_avg),
  mean_oil=round(as.numeric(oil_avg),2),
  stringsAsFactors=FALSE)
oil_df <- oil_df[order(-oil_df$mean_oil),]

sub_5  <- oil_df$REF_AREA[oil_df$mean_oil > 5]
sub_2  <- oil_df$REF_AREA[oil_df$mean_oil > 2]
sub_1  <- oil_df$REF_AREA[oil_df$mean_oil > 1]
sub_kn <- nonoecd[nonoecd %in% fossil_producers_nonoecd]
sub_ix <- intersect(sub_2, sub_kn)

cat("\nOil rents ranking:\n"); print(oil_df)
cat("\nRents >5%:", length(sub_5), "|", paste(sort(sub_5), collapse=", "))
cat("\nRents >2%:", length(sub_2), "|", paste(sort(sub_2), collapse=", "))
cat("\nRents >1%:", length(sub_1), "|", paste(sort(sub_1), collapse=", "))
cat("\nKnown:    ", length(sub_kn),"| ", paste(sort(sub_kn), collapse=", "))
cat("\nIntersect:", length(sub_ix),"| ", paste(sort(sub_ix), collapse=", "), "\n")

if (length(sub_5) < 3) {
  cat("\nWARNING: Rents >5% has only", length(sub_5),
      "countries — using >2% as fallback\n")
  sub_5 <- sub_2
}

# =============================================================================
# 3. LAGS
# =============================================================================
lag_within <- function(x, grp, k=1) {
  out <- rep(NA, length(x))
  for (g in unique(grp)) {
    idx <- which(grp==g); v <- x[idx]
    out[idx] <- c(rep(NA,k), v[seq_len(length(v)-k)])
  }
  out
}

df_gva <- df_gva[order(df_gva$REF_AREA, df_gva$TIME_PERIOD),]
for (pol in c("EPS","EPI","CCPI_SCORE")) {
  if (!pol %in% names(df_gva)) next
  df_gva[[paste0(pol,"_lag1")]] <- lag_within(df_gva[[pol]], df_gva$REF_AREA, 1)
  df_gva[[paste0(pol,"_lag2")]] <- lag_within(df_gva[[pol]], df_gva$REF_AREA, 2)
}

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

gva_cols  <- names(df_gva)[grepl("_(Domestic|Foreign)$", names(df_gva))]
all_codes <- unique(sub("_(Domestic|Foreign)$","",gva_cols))
ind_codes <- all_codes[!all_codes %in% drop_industries]

# =============================================================================
# 5. SORT FUNCTION — fossil industries on top
# =============================================================================
sort_fossil_top <- function(df) {
  df$is_fossil <- df$Industry %in% fossil_codes
  df <- df[order(-df$is_fossil, df$Industry),]
  df$is_fossil <- NULL
  df
}

# =============================================================================
# 6. PPML (for Domestic and Foreign GVA)
# =============================================================================
ppml_twfe <- function(y, pt, pl1, pl2,
                       gdp, trade, interest, oil, fin, country, year) {
  keep <- !is.na(y)&!is.na(pt)&!is.na(pl1)&!is.na(pl2)&
          !is.na(gdp)&!is.na(trade)&!is.na(interest)&
          !is.na(oil)&!is.na(fin)&!is.na(country)&!is.na(year)&y>=0
  y<-y[keep];pt<-pt[keep];pl1<-pl1[keep];pl2<-pl2[keep]
  gdp<-gdp[keep];trade<-trade[keep];interest<-interest[keep]
  oil<-oil[keep];fin<-fin[keep];country<-country[keep];year<-year[keep]
  n<-length(y)
  if(n<10||length(unique(country))<3) return(NULL)

  gdp      <- log(gdp+1)
  trade    <- scale(trade)[,1]
  interest <- scale(interest)[,1]
  oil      <- scale(oil)[,1]
  fin      <- scale(fin)[,1]

  cf <- relevel(factor(country), ref=levels(factor(country))[1])
  yf <- relevel(factor(year),    ref=levels(factor(year))[1])
  Xc <- model.matrix(~gdp+trade+interest+oil+fin+cf+yf-1)
  X  <- cbind(pt=pt,pl1=pl1,pl2=pl2, Xc)

  fit <- tryCatch(
    glm.fit(x=X,y=y,family=poisson(link="log"),
            control=glm.control(maxit=200,epsilon=1e-8)),
    error=function(e) NULL)
  if(is.null(fit)||!fit$converged) return(NULL)

  r <- y-fit$fitted.values; sc <- X*r
  H <- t(X)%*%diag(fit$fitted.values)%*%X
  Hi <- tryCatch(solve(H),error=function(e) NULL)
  if(is.null(Hi)) return(NULL)

  k<-ncol(X); B<-matrix(0,k,k)
  for(ct in unique(country)){
    i<-country==ct; s<-colSums(sc[i,,drop=FALSE]); B<-B+outer(s,s)}
  G<-length(unique(country)); V<-(G/(G-1))*Hi%*%B%*%Hi

  st<-function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",
              ifelse(p<.05,"*",ifelse(p<.10,".","ns"))))
  ex<-function(nm,vi){
    cv<-fit$coefficients[nm]; sv<-sqrt(max(V[vi,vi],0))
    zv<-cv/sv; pv<-2*pnorm(-abs(zv))
    list(coef=round(cv,4),sig=st(pv))}
  list(t=ex("pt",1),l1=ex("pl1",2),l2=ex("pl2",3),n=n,G=G)
}

# =============================================================================
# 7. OLS with within FE (for Ratio)
# =============================================================================
ols_twfe <- function(y, pt, pl1, pl2,
                      gdp, trade, interest, oil, fin, country, year) {
  keep <- !is.na(y)&!is.na(pt)&!is.na(pl1)&!is.na(pl2)&
          !is.na(gdp)&!is.na(trade)&!is.na(interest)&
          !is.na(oil)&!is.na(fin)&!is.na(country)&!is.na(year)&is.finite(y)
  y<-y[keep];pt<-pt[keep];pl1<-pl1[keep];pl2<-pl2[keep]
  gdp<-log(gdp[keep]+1);trade<-scale(trade[keep])[,1]
  interest<-scale(interest[keep])[,1];oil<-scale(oil[keep])[,1]
  fin<-scale(fin[keep])[,1];country<-country[keep];year<-year[keep]
  n<-length(y)
  if(n<10||length(unique(country))<3) return(NULL)

  dm<-function(x){
    grand<-mean(x); cm<-ave(x,country,FUN=mean); ym<-ave(x,year,FUN=mean)
    x-cm-ym+grand}
  yw<-dm(y);pw<-dm(pt);lw<-dm(pl1);l2w<-dm(pl2)
  gw<-dm(gdp);tw<-dm(trade);iw<-dm(interest);ow<-dm(oil);fw<-dm(fin)

  fit<-lm(yw~pw+lw+l2w+gw+tw+iw+ow+fw-1)
  X<-model.matrix(fit); e<-residuals(fit)
  Xi<-tryCatch(solve(t(X)%*%X),error=function(e) NULL)
  if(is.null(Xi)) return(NULL)

  k<-ncol(X); B<-matrix(0,k,k)
  for(ct in unique(country)){
    i<-country==ct; Xc<-X[i,,drop=FALSE]; ec<-e[i]
    s<-t(Xc)%*%ec; B<-B+s%*%t(s)}
  G<-length(unique(country))
  V<-(G/(G-1))*((n-1)/(n-k))*Xi%*%B%*%Xi

  st<-function(p) ifelse(p<.001,"***",ifelse(p<.01,"**",
              ifelse(p<.05,"*",ifelse(p<.10,".","ns"))))
  ex<-function(nm,vi){
    cv<-coef(fit)[nm]; sv<-sqrt(max(V[vi,vi],0))
    tv<-cv/sv; pv<-2*pt(-abs(tv),df=G-1)
    list(coef=round(cv,4),sig=st(pv))}
  list(t=ex("pw",1),l1=ex("lw",2),l2=ex("l2w",3),n=n,G=G)
}

# =============================================================================
# 8. RUN ONE POLICY / ONE DEP VAR / ONE SAMPLE
# =============================================================================
run_one <- function(df, pol_t, pol_l1, pol_l2, dep, label) {
  out <- data.frame()
  for (ind in ind_codes) {
    dc<-paste0(ind,"_Domestic"); fc<-paste0(ind,"_Foreign")
    if(!dc%in%names(df)|!fc%in%names(df)) next

    dom<-df[[dc]]; frn<-df[[fc]]

    if (dep=="Domestic") { y<-dom; est<-"ppml" }
    else if (dep=="Foreign") { y<-frn; est<-"ppml" }
    else {
      r<-frn/(dom+1); p99<-quantile(r,0.99,na.rm=TRUE)
      y<-pmin(r,p99); est<-"ols"
    }

    res <- if(est=="ppml")
      ppml_twfe(y,df[[pol_t]],df[[pol_l1]],df[[pol_l2]],
                df$GDP,df$TRADE,df$INTEREST_PAYMENTS,
                df$OIL_RENTS,df$FINANCIAL_DEVELOPMENT,
                df$REF_AREA,df$TIME_PERIOD)
    else
      ols_twfe(y,df[[pol_t]],df[[pol_l1]],df[[pol_l2]],
               df$GDP,df$TRADE,df$INTEREST_PAYMENTS,
               df$OIL_RENTS,df$FINANCIAL_DEVELOPMENT,
               df$REF_AREA,df$TIME_PERIOD)

    lbl<-ifelse(ind%in%names(industry_labels),industry_labels[ind],ind)
    if(!is.null(res))
      out<-rbind(out,data.frame(
        Industry=ind, Description=lbl, Sample=label,
        coef_t =res$t$coef,  sig_t =res$t$sig,
        coef_l1=res$l1$coef, sig_l1=res$l1$sig,
        coef_l2=res$l2$coef, sig_l2=res$l2$sig,
        n_obs=res$n, n_cty=res$G, stringsAsFactors=FALSE))
  }
  out
}

run_subset <- function(df, cty, pol_t, pol_l1, pol_l2, dep, label) {
  ds <- df[df$REF_AREA %in% cty,]
  n  <- length(unique(ds$REF_AREA))
  if(n<3){cat("  Skip",label,dep,"— only",n,"countries\n"); return(NULL)}
  cat(" ",label,"|",dep,"|",n,"countries\n")
  run_one(ds, pol_t, pol_l1, pol_l2, dep, label)
}

# =============================================================================
# 9. BUILD WIDE SUMMARY TABLE
# Columns: one set per sample (t, l1, l2)
# Rows sorted: fossil on top, then all others alphabetically
# =============================================================================
make_wide <- function(res_list) {
  non_null <- res_list[!sapply(res_list,is.null)]
  if(length(non_null)==0) return(NULL)
  base <- non_null[[1]]
  inds <- unique(base$Industry)
  out  <- data.frame()

  fmt<-function(df,ind,cc,sc){
    if(is.null(df)) return(NA)
    r<-df[df$Industry==ind,]
    if(nrow(r)==0||is.na(r[[cc]])) return(NA)
    paste0(r[[cc]]," ",r[[sc]])}

  for(ind in inds){
    lbl<-base$Description[base$Industry==ind][1]
    row<-data.frame(Industry=ind,Description=lbl,stringsAsFactors=FALSE)
    for(nm in names(res_list)){
      dm<-res_list[[nm]]
      row[[paste0(nm,"_t") ]]<-fmt(dm,ind,"coef_t", "sig_t")
      row[[paste0(nm,"_l1")]]<-fmt(dm,ind,"coef_l1","sig_l1")
      row[[paste0(nm,"_l2")]]<-fmt(dm,ind,"coef_l2","sig_l2")
    }
    out<-rbind(out,row)
  }
  sort_fossil_top(out)
}

# =============================================================================
# 10. RUN ALL MODELS
# =============================================================================

# ---- PUSH: OECD, EPS ----
cat("\n=== PUSH EPS — Domestic GVA ===\n")
push_dom_eps <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2","Domestic","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2","Domestic","EU")))

cat("\n=== PUSH EPS — Foreign GVA ===\n")
push_for_eps <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2","Foreign","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2","Foreign","EU")))

cat("\n=== PUSH EPS — Ratio ===\n")
push_rat_eps <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "EPS","EPS_lag1","EPS_lag2","Ratio","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "EPS","EPS_lag1","EPS_lag2","Ratio","EU")))

# ---- PUSH: OECD, CCPI ----
cat("\n=== PUSH CCPI — Domestic GVA ===\n")
push_dom_ccpi <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Domestic","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Domestic","EU")))

cat("\n=== PUSH CCPI — Foreign GVA ===\n")
push_for_ccpi <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Foreign","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Foreign","EU")))

cat("\n=== PUSH CCPI — Ratio ===\n")
push_rat_ccpi <- make_wide(list(
  OECD = run_subset(df_gva, oecd_iso3, "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Ratio","OECD"),
  EU   = run_subset(df_gva, eu_iso3,   "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Ratio","EU")))

# ---- PULL: non-OECD resource-rich, EPI ----
pull_subs <- list(
  "All_nonOECD" = nonoecd,
  "Rents_5pct"  = sub_5,
  "Rents_2pct"  = sub_2,
  "Rents_1pct"  = sub_1,
  "Known"       = sub_kn,
  "Intersect"   = sub_ix)

cat("\n=== PULL EPI — Foreign GVA ===\n")
pull_for_epi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "EPI","EPI_lag1","EPI_lag2","Foreign",nm)) |>
  setNames(names(pull_subs)))

cat("\n=== PULL EPI — Domestic GVA ===\n")
pull_dom_epi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "EPI","EPI_lag1","EPI_lag2","Domestic",nm)) |>
  setNames(names(pull_subs)))

cat("\n=== PULL EPI — Ratio ===\n")
pull_rat_epi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "EPI","EPI_lag1","EPI_lag2","Ratio",nm)) |>
  setNames(names(pull_subs)))

# ---- PULL: non-OECD resource-rich, CCPI ----
cat("\n=== PULL CCPI — Foreign GVA ===\n")
pull_for_ccpi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Foreign",nm)) |>
  setNames(names(pull_subs)))

cat("\n=== PULL CCPI — Domestic GVA ===\n")
pull_dom_ccpi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Domestic",nm)) |>
  setNames(names(pull_subs)))

cat("\n=== PULL CCPI — Ratio ===\n")
pull_rat_ccpi <- make_wide(lapply(names(pull_subs), function(nm)
  run_subset(df_gva, pull_subs[[nm]], "CCPI_SCORE","CCPI_SCORE_lag1","CCPI_SCORE_lag2","Ratio",nm)) |>
  setNames(names(pull_subs)))

# =============================================================================
# 11. EXPORT TO EXCEL — 12 sheets, fossil on top of each
# =============================================================================
wb  <- createWorkbook()
hs  <- createStyle(fontColour="#FFFFFF", bgFill="#65dbdb",
                   textDecoration="bold", halign="center")
fos <- createStyle(bgFill="#ede62d")   
neg <- createStyle(fontColour="#C0392B", textDecoration="bold")
pos <- createStyle(fontColour="#4dcc86", textDecoration="bold")

colour_cell <- function(wb, sheet, data, col_name) {
  ci <- which(names(data)==col_name); if(!length(ci)) return()
  for(i in seq_len(nrow(data))){
    vs <- data[i,ci]; if(is.na(vs)) next
    rv <- suppressWarnings(as.numeric(sub(" .*","",vs)))
    if(!is.na(rv))
      addStyle(wb,sheet,if(rv<0)neg else pos,rows=i+1,cols=ci,stack=TRUE)
  }
}

add_sheet <- function(wb, name, data) {
  if(is.null(data)||nrow(data)==0) return()
  addWorksheet(wb, name)
  writeData(wb, name, data, headerStyle=hs)
  setColWidths(wb, name, cols=1:ncol(data), widths="auto")
  freezePane(wb, name, firstRow=TRUE)
  # Highlight fossil rows amber
  for(i in seq_len(nrow(data)))
    if(data$Industry[i] %in% fossil_codes)
      addStyle(wb,name,fos,rows=i+1,cols=1:ncol(data),gridExpand=TRUE,stack=TRUE)
  # Colour coefficient cells red/green
  coef_cols <- names(data)[grepl("_(t|l1|l2)$",names(data))]
  for(cc in coef_cols) colour_cell(wb, name, data, cc)
}

# PUSH sheets
add_sheet(wb, "PUSH EPS Dom GVA",    push_dom_eps)
add_sheet(wb, "PUSH EPS For GVA",    push_for_eps)
add_sheet(wb, "PUSH EPS Ratio",      push_rat_eps)
add_sheet(wb, "PUSH CCPI Dom GVA",   push_dom_ccpi)
add_sheet(wb, "PUSH CCPI For GVA",   push_for_ccpi)
add_sheet(wb, "PUSH CCPI Ratio",     push_rat_ccpi)

# PULL sheets
add_sheet(wb, "PULL EPI For GVA",    pull_for_epi)
add_sheet(wb, "PULL EPI Dom GVA",    pull_dom_epi)
add_sheet(wb, "PULL EPI Ratio",      pull_rat_epi)
add_sheet(wb, "PULL CCPI For GVA",   pull_for_ccpi)
add_sheet(wb, "PULL CCPI Dom GVA",   pull_dom_ccpi)
add_sheet(wb, "PULL CCPI Ratio",     pull_rat_ccpi)

# Country diagnostic sheet
addWorksheet(wb, "Country Info")
diag <- merge(
  data.frame(REF_AREA=nonoecd,
             Rents_5pct  =nonoecd%in%sub_5,
             Rents_2pct  =nonoecd%in%sub_2,
             Rents_1pct  =nonoecd%in%sub_1,
             Known       =nonoecd%in%sub_kn,
             Intersect   =nonoecd%in%sub_ix,
             stringsAsFactors=FALSE),
  oil_df, by="REF_AREA", all.x=TRUE)
diag <- diag[order(-diag$mean_oil),]
writeData(wb,"Country Info",diag,headerStyle=hs)
setColWidths(wb,"Country Info",cols=1:ncol(diag),widths="auto")
freezePane(wb,"Country Info",firstRow=TRUE)

out_path <- "PPML_GVA_clean_results.xlsx"
saveWorkbook(wb, out_path, overwrite=TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# SHEET STRUCTURE (12 sheets + Country Info):
#
# PUSH EPS Dom/For GVA / Ratio   — EPS x GVA in OECD (OECD + EU columns)
# PUSH CCPI Dom/For GVA / Ratio  — CCPI x GVA in OECD (OECD + EU columns)
# PULL EPI Dom/For GVA / Ratio   — EPI x GVA in non-OECD resource subsets
# PULL CCPI Dom/For GVA / Ratio  — CCPI x GVA in non-OECD resource subsets
#
# Each cell: coefficient + significance e.g. "-0.18 **"
# Fossil industries highlighted AMBER at the TOP of every table
# Negative = red, Positive = green
# Sig: *** p<.001  ** p<.01  * p<.05  . p<.10  ns = not significant
# =============================================================================