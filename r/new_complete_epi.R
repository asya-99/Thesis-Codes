# =============================================================================
# Climate Change Mitigation EPI Score — Weighted Aggregation
# Combines 11 sub-indicators into a single composite score using their
# official within-category weights from the Yale EPI 2024 Technical Appendix
# =============================================================================

library(readxl)
library(openxlsx)

# =============================================================================
# 1. LOAD DATA
# =============================================================================
df <- read_excel("/Users/asyadachi/gent/R_codes/COMPLETE_IMP.xlsx")

# Indicator columns — make sure all are numeric
indicator_cols <- c("BCA_VALUE","CBP_VALUE","CDA_VALUE","CDF_VALUE","CHA_VALUE",
                    "FGA_VALUE","GHN_VALUE","GTI_VALUE","GTP_VALUE",
                    "LUF_VALUE","NDA_VALUE")

for (col in indicator_cols) df[[col]] <- as.numeric(df[[col]])

cat("Rows:", nrow(df), "\n")
for (col in indicator_cols) {
  cat(col, "- non-missing:", sum(!is.na(df[[col]])), "\n")
}

# =============================================================================
# 2. OFFICIAL WEIGHTS (within Climate Change Mitigation category)
# Source: Yale EPI 2024 Technical Appendix, p.5
# These sum to 100
# =============================================================================
weights <- c(
  CDA_VALUE = 25.0,    # CO2 growth rate (standard)
  GTI_VALUE = 20.0,    # GHG growth rate adjusted by emissions intensity
  GTP_VALUE = 20.0,    # GHG growth rate adjusted by per capita emissions
  CHA_VALUE = 10.0,    # Adjusted emissions growth rate for methane
  FGA_VALUE = 6.67,    # Adjusted emissions growth rate for F-gases
  BCA_VALUE = 5.0,     # Adjusted emissions growth rate for black carbon
  NDA_VALUE = 3.33,    # Adjusted emissions growth rate for nitrous oxide
  LUF_VALUE = 3.33,    # Net carbon fluxes due to land cover change
  GHN_VALUE = 3.33,    # Projected emissions in 2050
  CDF_VALUE = 1.67,    # CO2 growth rate (country-specific targets)
  CBP_VALUE = 1.67     # Projected cumulative emissions to 2050 vs carbon budget
)

cat("\nWeights sum to:", sum(weights), "(should be 100)\n")

# =============================================================================
# 3. COMPUTE WEIGHTED COMPOSITE SCORE
# Standard EPI approach: weighted average, renormalizing weights for any
# missing indicators in a given country-year (so missing data doesn't
# unfairly penalize a country - this matches Yale's own methodology)
# =============================================================================
compute_weighted_score <- function(row_values, weights) {
  available <- !is.na(row_values)
  if (sum(available) == 0) return(NA)

  w_avail <- weights[available]
  v_avail <- row_values[available]

  # Renormalize weights to sum to 100 among available indicators
  w_norm <- w_avail / sum(w_avail) * 100

  sum(v_avail * w_norm) / 100
}

df$EPI_Mitigation_Score <- NA
for (i in seq_len(nrow(df))) {
  row_vals <- as.numeric(df[i, indicator_cols])
  names(row_vals) <- indicator_cols
  df$EPI_Mitigation_Score[i] <- compute_weighted_score(row_vals, weights)
}

cat("\nComposite score computed for", sum(!is.na(df$EPI_Mitigation_Score)), "rows\n")
cat("Range:", round(min(df$EPI_Mitigation_Score, na.rm=TRUE), 2), "to",
            round(max(df$EPI_Mitigation_Score, na.rm=TRUE), 2), "\n")
cat("Mean:", round(mean(df$EPI_Mitigation_Score, na.rm=TRUE), 2), "\n")

# =============================================================================
# 4. SAVE UPDATED DATA WITH NEW COLUMN
# =============================================================================
wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, "Sheet1", df)
setColWidths(wb, "Sheet1", cols = 1:ncol(df), widths = "auto")
freezePane(wb, "Sheet1", firstRow = TRUE)

out_path <- "/Users/asyadachi/gent/R_codes/COMPLETE_IMP.xlsx"
saveWorkbook(wb, out_path, overwrite = TRUE)
cat("\nSaved full dataset with EPI_Mitigation_Score column to:\n", out_path, "\n")

# =============================================================================
# WEIGHTS USED (within Climate Change Mitigation category, sum to 100)
# Source: Official Yale EPI 2024 weights.csv ("epi2024weights")
# CDA_VALUE  25.0  CO2 growth rate (standard)
# GTI_VALUE  20.0  GHG growth rate adjusted by emissions intensity
# GTP_VALUE  20.0  GHG growth rate adjusted by per capita emissions
# CHA_VALUE  10.0  Adjusted emissions growth rate for methane
# FGA_VALUE   6.7  Adjusted emissions growth rate for F-gases
# BCA_VALUE   5.0  Adjusted emissions growth rate for black carbon
# NDA_VALUE   3.3  Adjusted emissions growth rate for nitrous oxide
# LUF_VALUE   3.3  Net carbon fluxes due to land cover change
# GHN_VALUE   3.3  Projected emissions in 2050
# CDF_VALUE   1.7  CO2 growth rate (country-specific targets)
# CBP_VALUE   1.7  Projected cumulative emissions to 2050 vs carbon budget
# =============================================================================