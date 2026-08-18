# =============================================================================
# Export to Import Ratio Ranking
# Average export/import ratio per country across fossil-fuel-intensive industries
# Ranked highest to lowest for OECD and non-OECD separately
# Uses Domestic + Foreign columns combined for total exports and imports
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
df_exp <- read_excel("COMPLETE_EXPORT.xlsx", sheet = "Sheet1")
df_imp <- read_excel("COMPLETE_IMPORT.xlsx", sheet = "Sheet1")
names(df_exp) <- trimws(names(df_exp))
names(df_imp) <- trimws(names(df_imp))

# Remove index column if present
if (names(df_exp)[1] == "" | is.na(names(df_exp)[1]))
  df_exp <- df_exp[, -1]
if (names(df_imp)[1] == "" | is.na(names(df_imp)[1]))
  df_imp <- df_imp[, -1]

df_exp$TIME_PERIOD <- as.numeric(df_exp$TIME_PERIOD)
df_imp$TIME_PERIOD <- as.numeric(df_imp$TIME_PERIOD)

# Get fossil export and import columns (domestic + foreign combined)
fossil_dom_exp <- paste0(fossil_codes, "_Domestic")
fossil_for_exp <- paste0(fossil_codes, "_Foreign")
fossil_dom_exp <- fossil_dom_exp[fossil_dom_exp %in% names(df_exp)]
fossil_for_exp <- fossil_for_exp[fossil_for_exp %in% names(df_exp)]
fossil_dom_imp <- paste0(fossil_codes, "_Domestic")
fossil_for_imp <- paste0(fossil_codes, "_Foreign")
fossil_dom_imp <- fossil_dom_imp[fossil_dom_imp %in% names(df_imp)]
fossil_for_imp <- fossil_for_imp[fossil_for_imp %in% names(df_imp)]

# Convert to numeric
for (col in c(fossil_dom_exp, fossil_for_exp))
  df_exp[[col]] <- suppressWarnings(as.numeric(df_exp[[col]]))
for (col in c(fossil_dom_imp, fossil_for_imp))
  df_imp[[col]] <- suppressWarnings(as.numeric(df_imp[[col]]))

# Total fossil exports and imports per country-year
df_exp$total_exp <- rowSums(df_exp[, c(fossil_dom_exp, fossil_for_exp)],
                             na.rm = TRUE)
df_imp$total_imp <- rowSums(df_imp[, c(fossil_dom_imp, fossil_for_imp)],
                             na.rm = TRUE)

# Merge exports and imports
df_merged <- df_exp %>%
  select(REF_AREA, TIME_PERIOD, total_exp) %>%
  left_join(df_imp %>% select(REF_AREA, TIME_PERIOD, total_imp),
            by = c("REF_AREA","TIME_PERIOD"))

df_merged$group <- ifelse(df_merged$REF_AREA %in% oecd_iso3,
                           "OECD", "Non-OECD")

# Export to import ratio per country-year
df_merged$exp_imp_ratio <- df_merged$total_exp /
                            (df_merged$total_imp + 1)

# Average per country
country_avg <- df_merged %>%
  group_by(REF_AREA, group) %>%
  summarise(
    avg_ratio  = round(mean(exp_imp_ratio, na.rm = TRUE), 4),
    avg_export = round(mean(total_exp, na.rm = TRUE), 2),
    avg_import = round(mean(total_imp, na.rm = TRUE), 2),
    years      = n(),
    .groups    = "drop"
  ) %>%
  arrange(group, desc(avg_ratio))

# Split
oecd_rank <- country_avg %>%
  filter(group == "OECD") %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = REF_AREA,
         Avg_Exp_Imp_Ratio = avg_ratio,
         Avg_Fossil_Exports = avg_export,
         Avg_Fossil_Imports = avg_import,
         Years = years)

nonoecd_rank <- country_avg %>%
  filter(group == "Non-OECD") %>%
  mutate(Rank = row_number()) %>%
  select(Rank, Country = REF_AREA,
         Avg_Exp_Imp_Ratio = avg_ratio,
         Avg_Fossil_Exports = avg_export,
         Avg_Fossil_Imports = avg_import,
         Years = years)

cat("=== OECD — Average Fossil Export/Import Ratio (High to Low) ===\n")
print(as.data.frame(oecd_rank))
cat("\n=== NON-OECD — Average Fossil Export/Import Ratio (High to Low) ===\n")
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

saveWorkbook(wb, "Export_Import_Ratio_Ranking.xlsx", overwrite = TRUE)
cat("\nSaved: Export_Import_Ratio_Ranking.xlsx\n")

cat("\nKey stats:\n")
cat("Highest OECD ratio:    ", oecd_rank$Country[1],
    "-", oecd_rank$Avg_Exp_Imp_Ratio[1], "\n")
cat("Lowest OECD ratio:     ", oecd_rank$Country[nrow(oecd_rank)],
    "-", oecd_rank$Avg_Exp_Imp_Ratio[nrow(oecd_rank)], "\n")
cat("Highest non-OECD ratio:", nonoecd_rank$Country[1],
    "-", nonoecd_rank$Avg_Exp_Imp_Ratio[1], "\n")
cat("Lowest non-OECD ratio: ", nonoecd_rank$Country[nrow(nonoecd_rank)],
    "-", nonoecd_rank$Avg_Exp_Imp_Ratio[nrow(nonoecd_rank)], "\n")

# =============================================================================
# NOTE ON INTERPRETATION:
# Ratio > 1 = country exports more fossil industry output than it imports
#             (net fossil exporter — typically resource-rich producers)
# Ratio < 1 = country imports more fossil industry output than it exports
#             (net fossil importer — typically consuming/processing economies)
# Ratio ~ 1 = roughly balanced trade in fossil industries
#
# High ratio in non-OECD = resource-rich country exporting fossil production
# Low ratio in OECD = importing fossil goods from non-OECD producers
# This pattern is consistent with the pull mechanism of carbon leakage
# =============================================================================