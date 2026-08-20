# =============================================================================
# Variance Inflation Factor (VIF) Test for Multicollinearity
# Tests control variables used in Spec 1 and Spec 2
# =============================================================================

library(readxl)
library(car)

oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

# Load data
gva <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(gva) <- trimws(names(gva))

# Convert to numeric
for (col in names(gva)[!names(col) %in% c("REF_AREA","TIME_PERIOD")])
  gva[[col]] <- suppressWarnings(as.numeric(gva[[col]]))
gva$TIME_PERIOD <- as.numeric(gva$TIME_PERIOD)
gva$log_GDP <- log(gva$GDP + 1)

cat("=== SPEC 2 — VIF Test (OECD push side) ===\n")
cat("Controls: log GDP per capita, Oil Rents\n\n")

spec2_oecd <- gva[gva$REF_AREA %in% oecd_iso3 &
                  complete.cases(gva[, c("EPI","log_GDP","OIL_RENTS")]), ]

vif_spec2 <- lm(EPI ~ log_GDP + OIL_RENTS, data = spec2_oecd)
cat("VIF results:\n")
print(vif(vif_spec2))

cat("\n=== SPEC 2 — VIF Test (non-OECD pull side) ===\n")
cat("Controls: log GDP per capita, Oil Rents\n\n")

spec2_nonoecd <- gva[!gva$REF_AREA %in% oecd_iso3 &
                     !gva$REF_AREA %in% c("CHN","RUS") &
                     complete.cases(gva[, c("EPI","log_GDP","OIL_RENTS")]), ]

vif_spec2_pull <- lm(EPI ~ log_GDP + OIL_RENTS, data = spec2_nonoecd)
cat("VIF results:\n")
print(vif(vif_spec2_pull))

cat("\n=== SPEC 1 — VIF Test (OECD push side) ===\n")
cat("Controls: log GDP per capita, Oil Rents, Financial Development\n\n")

spec1_oecd <- gva[gva$REF_AREA %in% oecd_iso3 &
                  complete.cases(gva[, c("EPS","log_GDP",
                                         "OIL_RENTS",
                                         "FINANCIAL_DEVELOPMENT")]), ]

if (nrow(spec1_oecd) > 10) {
  vif_spec1 <- lm(EPS ~ log_GDP + OIL_RENTS + FINANCIAL_DEVELOPMENT,
                  data = spec1_oecd)
  cat("VIF results:\n")
  print(vif(vif_spec1))
} else {
  cat("Insufficient complete observations for Spec 1 VIF test\n")
}

cat("\n=== INTERPRETATION GUIDE ===\n")
cat("VIF < 5    : No multicollinearity concern\n")
cat("VIF 5-10   : Moderate concern, investigate further\n")
cat("VIF > 10   : Serious multicollinearity, consider dropping variable\n")