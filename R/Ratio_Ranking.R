# =============================================================================
# Foreign to Domestic GVA Ratio Ranking
# Average ratio per country across all fossil-fuel-intensive industries
# Ranked highest to lowest for OECD and non-OECD separately
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

fossil_codes <- c("B05T09","C19","C20","C23","C24","C17T18","D35_E36T39")

# Load data
df <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(df) <- trimws(names(df))
df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
df$group <- ifelse(df$REF_AREA %in% oecd_iso3, "OECD", "Non-OECD")

# Get fossil domestic and foreign columns
fossil_dom_cols <- paste0(fossil_codes, "_Domestic")
fossil_for_cols <- paste0(fossil_codes, "_Foreign")
fossil_dom_cols <- fossil_dom_cols[fossil_dom_cols %in% names(df)]
fossil_for_cols <- fossil_for_cols[fossil_for_cols %in% names(df)]

# Convert to numeric
for (col in c(fossil_dom_cols, fossil_for_cols))
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))

# Sum across all fossil industries per country-year
df$total_dom <- rowSums(df[, fossil_dom_cols], na.rm = TRUE)
df$total_for <- rowSums(df[, fossil_for_cols], na.rm = TRUE)
df$ratio     <- df$total_for / (df$total_dom + 1)

# Average ratio per country across all years
country_avg <- df %>%
  group_by(REF_AREA, group) %>%
  summarise(
    avg_ratio    = round(mean(ratio, na.rm = TRUE), 4),
    avg_for_gva  = round(mean(total_for, na.rm = TRUE), 2),
    avg_dom_gva  = round(mean(total_dom, na.rm = TRUE), 2),
    years        = n(),
    .groups = "drop"
  ) %>%
  arrange(group, desc(avg_ratio))

# Split into OECD and non-OECD
oecd_rank    <- country_avg %>%
  filter(group == "OECD") %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = REF_AREA,
         Avg_Ratio = avg_ratio,
         Avg_Foreign_GVA = avg_for_gva,
         Avg_Domestic_GVA = avg_dom_gva,
         Years = years)

nonoecd_rank <- country_avg %>%
  filter(group == "Non-OECD") %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = REF_AREA,
         Avg_Ratio = avg_ratio,
         Avg_Foreign_GVA = avg_for_gva,
         Avg_Domestic_GVA = avg_dom_gva,
         Years = years)

cat("=== OECD COUNTRIES — Foreign/Domestic GVA Ratio (High to Low) ===\n")
print(as.data.frame(oecd_rank))

cat("\n=== NON-OECD COUNTRIES — Foreign/Domestic GVA Ratio (High to Low) ===\n")
print(as.data.frame(nonoecd_rank))

# Export to Excel
wb <- createWorkbook()
hs <- createStyle(fontColour = "#FFFFFF", fgFill = "#2F4F4F",
                  textDecoration = "bold", halign = "center")

addWorksheet(wb, "OECD Ranking")
writeData(wb, "OECD Ranking", oecd_rank, headerStyle = hs)
setColWidths(wb, "OECD Ranking", cols = 1:ncol(oecd_rank), widths = "auto")
freezePane(wb, "OECD Ranking", firstRow = TRUE)

addWorksheet(wb, "NonOECD Ranking")
writeData(wb, "NonOECD Ranking", nonoecd_rank, headerStyle = hs)
setColWidths(wb, "NonOECD Ranking", cols = 1:ncol(nonoecd_rank), widths = "auto")
freezePane(wb, "NonOECD Ranking", firstRow = TRUE)

saveWorkbook(wb, "GVA_Ratio_Ranking.xlsx", overwrite = TRUE)
cat("\nSaved: GVA_Ratio_Ranking.xlsx\n")

cat("\nKey stats:\n")
cat("Highest OECD ratio:", oecd_rank$Country[1],
    "-", oecd_rank$Avg_Ratio[1], "\n")
cat("Lowest OECD ratio:", oecd_rank$Country[nrow(oecd_rank)],
    "-", oecd_rank$Avg_Ratio[nrow(oecd_rank)], "\n")
cat("Highest non-OECD ratio:", nonoecd_rank$Country[1],
    "-", nonoecd_rank$Avg_Ratio[1], "\n")
cat("Lowest non-OECD ratio:", nonoecd_rank$Country[nrow(nonoecd_rank)],
    "-", nonoecd_rank$Avg_Ratio[nrow(nonoecd_rank)], "\n")