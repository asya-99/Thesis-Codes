# =============================================================================
# CCPI_SCORE x GVA — Two-Way Fixed Effects Panel Regression
# Controls: GDP, TRADE
# Includes contemporaneous (t) and 1-year lag (t-1) coefficients
# Output: CCPI_TWFE_results.xlsx with one sheet
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
df <- read_excel("R_codes/COMPLETE.xlsx", sheet = "Sheet1")

df$CCPI_SCORE   <- as.numeric(df$CCPI_SCORE)
df$GDP         <- as.numeric(df$GDP)
df$TRADE       <- as.numeric(df$TRADE)
df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)

gva_cols <- names(df)[grepl("_(Domestic|Foreign)$", names(df))]
for (col in gva_cols) df[[col]] <- as.numeric(df[[col]])

cat("Rows:", nrow(df), "| Countries:", length(unique(df$REF_AREA)),
    "| Years:", length(unique(df$TIME_PERIOD)), "\n")

# =============================================================================
# 2. CREATE 1-YEAR LAG OF CCPI_SCORE (within country)
# =============================================================================
df <- df[order(df$REF_AREA, df$TIME_PERIOD), ]

lag_within <- function(x, group) {
  out <- rep(NA, length(x))
  for (g in unique(group)) {
    idx      <- which(group == g)
    out[idx] <- c(NA, x[idx[-length(idx)]])
  }
  out
}

df$CCPI_lag1 <- lag_within(df$CCPI_SCORE, df$REF_AREA)
cat("Rows with lagged CCPI_SCORE:", sum(!is.na(df$CCPI_lag1)), "\n")

# =============================================================================
# 3. INDUSTRY LABELS
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
# 4. TWO-WAY FIXED EFFECTS FUNCTION
# Model: log(GVA_it) ~ policy_it + log(GDP_it) + TRADE_it + country_FE + year_FE
# Country and year fixed effects removed via within (demeaning) transformation
# Standard errors clustered by country
# =============================================================================
twoway_fe <- function(y, policy, gdp, trade, country, year) {

  keep <- !is.na(y) & !is.na(policy) & !is.na(gdp) & !is.na(trade) &
          !is.na(country) & !is.na(year)
  y       <- y[keep];       policy  <- policy[keep]
  gdp     <- gdp[keep];     trade   <- trade[keep]
  country <- country[keep]; year    <- year[keep]
  n       <- length(y)

  if (n < 30) return(NULL)

  y   <- log(y   + 1)
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

  fit <- lm(y_w ~ policy_w + gdp_w + trade_w - 1)

  # Clustered SE by country (cluster-robust sandwich estimator)
  X       <- cbind(policy_w, gdp_w, trade_w)
  e       <- residuals(fit)
  XtX_inv <- solve(t(X) %*% X)
  k       <- ncol(X)
  B       <- matrix(0, k, k)
  for (c in unique(country)) {
    idx <- country == c
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

  get_coef <- function(idx, name) {
    coef_v <- coef(fit)[name]
    se_v   <- sqrt(V[idx, idx])
    t_v    <- coef_v / se_v
    p_v    <- 2 * pt(-abs(t_v), df = G - 1)
    list(coef = round(coef_v, 4), se = round(se_v, 4),
         t = round(t_v, 3), p = round(p_v, 4), sig = stars(p_v))
  }

  list(
    policy = get_coef(1, "policy_w"),
    gdp    = get_coef(2, "gdp_w"),
    trade  = get_coef(3, "trade_w"),
    r2     = round(summary(fit)$r.squared, 3),
    n      = n,
    G      = G
  )
}

# =============================================================================
# 5. LOOP OVER INDUSTRIES — run for t and t-1, for Domestic/Foreign/Total
# =============================================================================
industry_codes <- unique(sub("_(Domestic|Foreign)$", "", gva_cols))

run_model <- function(policy_var) {
  out <- data.frame()
  for (ind in industry_codes) {
    dom_col <- paste0(ind, "_Domestic")
    for_col <- paste0(ind, "_Foreign")
    if (!dom_col %in% names(df) | !for_col %in% names(df)) next

    dom     <- df[[dom_col]]
    frn     <- df[[for_col]]
    tot     <- dom + frn
    policy  <- df[[policy_var]]
    gdp     <- df$GDP
    trade   <- df$TRADE
    country <- df$REF_AREA
    year    <- df$TIME_PERIOD
    label   <- ifelse(ind %in% names(industry_labels), industry_labels[ind], ind)

    make_row <- function(res, gva_type) {
      if (is.null(res)) return(NULL)
      data.frame(
        Industry    = ind, Description = label, GVA_type = gva_type,
        coef_policy = res$policy$coef, se_policy = res$policy$se,
        t_policy    = res$policy$t,    p_policy  = res$policy$p,
        sig_policy  = res$policy$sig,
        coef_GDP    = res$gdp$coef,    sig_GDP   = res$gdp$sig,
        coef_TRADE  = res$trade$coef,  sig_TRADE = res$trade$sig,
        R2_within   = res$r2, n_obs = res$n, n_countries = res$G,
        stringsAsFactors = FALSE
      )
    }

    out <- rbind(out,
                 make_row(twoway_fe(dom, policy, gdp, trade, country, year), "Domestic"),
                 make_row(twoway_fe(frn, policy, gdp, trade, country, year), "Foreign"),
                 make_row(twoway_fe(tot, policy, gdp, trade, country, year), "Total"))
  }
  out
}

cat("\nRunning contemporaneous model: CCPI_SCORE(t)...\n")
res_t  <- run_model("CCPI_SCORE")

cat("Running lagged model: CCPI_SCORE(t-1)...\n")
res_l1 <- run_model("CCPI_lag1")

# =============================================================================
# 6. BUILD SIDE-BY-SIDE TABLE: one row per industry, t and t-1 coefficients
#    for Domestic, Foreign, Total
# =============================================================================
build_table <- function(res_t, res_l1) {
  inds <- unique(res_t$Industry)
  out  <- data.frame()
  for (ind in inds) {
    label <- res_t$Description[res_t$Industry == ind][1]
    row   <- data.frame(Industry = ind, Description = label, stringsAsFactors = FALSE)
    for (gt in c("Domestic", "Foreign", "Total")) {
      t_row  <- res_t [res_t$Industry  == ind & res_t$GVA_type  == gt, ]
      l_row  <- res_l1[res_l1$Industry == ind & res_l1$GVA_type == gt, ]
      row[[paste0("coef_t_",  gt)]] <- if (nrow(t_row)  > 0) t_row$coef_policy  else NA
      row[[paste0("sig_t_",   gt)]] <- if (nrow(t_row)  > 0) t_row$sig_policy   else NA
      row[[paste0("coef_l1_", gt)]] <- if (nrow(l_row) > 0) l_row$coef_policy   else NA
      row[[paste0("sig_l1_",  gt)]] <- if (nrow(l_row) > 0) l_row$sig_policy    else NA
    }
    out <- rbind(out, row)
  }
  # Sort by Total t-1 coefficient (most negative = strongest delayed push effect)
  out[order(out$coef_l1_Total), ]
}

side_by_side <- build_table(res_t, res_l1)

cat("\n--- CCPI_SCORE(t) vs CCPI_SCORE(t-1) coefficients ---\n")
print(side_by_side[, c("Industry","Description",
                       "coef_t_Total","sig_t_Total",
                       "coef_l1_Total","sig_l1_Total")])

# =============================================================================
# 7. EXPORT TO EXCEL
# =============================================================================
wb <- createWorkbook()
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
  writeData(wb, name, data, headerStyle = hs)
  setColWidths(wb, name, cols = 1:ncol(data), widths = "auto")
  freezePane(wb, name, firstRow = TRUE)
  for (cc in coef_cols) colour_col(wb, name, data, cc)
}

# Main sheet: side-by-side t vs t-1 for all three GVA types
coef_cols_main <- c("coef_t_Domestic","coef_l1_Domestic",
                    "coef_t_Foreign","coef_l1_Foreign",
                    "coef_t_Total","coef_l1_Total")
add_sheet(wb, "CCPI TWFE", side_by_side, coef_cols_main)

# Fossil-fuel industries only
fossil_codes <- c("C19","C20","C24","B05T09","D35_E36T39","C23","C17T18")
fossil_out   <- side_by_side[side_by_side$Industry %in% fossil_codes, ]
add_sheet(wb, "CCPI Fossil", fossil_out, coef_cols_main)

# Full detail (every stat, every model) for reference/appendix
full_detail <- rbind(
  cbind(Lag = "t",   res_t),
  cbind(Lag = "t-1", res_l1)
)
add_sheet(wb, "CCPI Full Detail", full_detail, "coef_policy")

out_path <- "/Users/asyadachi/gent/R_codes/CCPI_PUSH_results.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved to:", out_path, "\n")

# =============================================================================
# KEY:
# Model: log(GVA_it) ~ CCPI_SCORE_it + log(GDP_it) + TRADE_it + country_FE + year_FE
# coef_t      — effect of CCPI_SCORE this year on GVA this year
# coef_l1     — effect of CCPI_SCORE last year on GVA this year
# Negative + significant = push effect (red). Positive = green.
# Country and year fixed effects absorb time-invariant country differences
# and global shocks — what remains is the within-country policy effect.
# =============================================================================