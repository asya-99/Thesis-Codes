# =============================================================================
# Descriptive Statistics and Visualisation
# China (CHN) and Russia (RUS) excluded from non-OECD group
# =============================================================================

library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

df <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(df) <- trimws(names(df))
for (col in names(df)[!names(df) %in% c("REF_AREA","TIME_PERIOD")])
  df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)

oecd_iso3 <- c(
  "AUS","AUT","BEL","CAN","CHL","COL","CRI","CZE","DNK","EST",
  "FIN","FRA","DEU","GRC","HUN","ISL","IRL","ISR","ITA","JPN",
  "KOR","LVA","LTU","LUX","MEX","NLD","NZL","NOR","POL","PRT",
  "SVK","SVN","ESP","SWE","CHE","TUR","GBR","USA"
)

fossil_codes <- c("B05T09","C19","C20","C23","C24","C17T18","D35_E36T39")

# Exclude China and Russia from non-OECD group
df <- df[!(df$REF_AREA %in% c("CHN","RUS") & !df$REF_AREA %in% oecd_iso3), ]
cat("Excluded CHN and RUS from non-OECD sample\n")
cat("Remaining countries:", length(unique(df$REF_AREA)), "\n")

# Get fossil columns
fossil_dom_cols <- paste0(fossil_codes, "_Domestic")
fossil_for_cols <- paste0(fossil_codes, "_Foreign")
fossil_dom_cols <- fossil_dom_cols[fossil_dom_cols %in% names(df)]
fossil_for_cols <- fossil_for_cols[fossil_for_cols %in% names(df)]

df$group     <- ifelse(df$REF_AREA %in% oecd_iso3, "OECD", "Non-OECD")
df$fossil_dom <- rowSums(df[, fossil_dom_cols], na.rm=TRUE)
df$fossil_for <- rowSums(df[, fossil_for_cols], na.rm=TRUE)

# Aggregate by year and group
annual <- df %>%
  group_by(TIME_PERIOD, group) %>%
  summarise(
    total_dom   = sum(fossil_dom, na.rm=TRUE),
    total_for   = sum(fossil_for, na.rm=TRUE),
    ratio       = sum(fossil_for, na.rm=TRUE) /
                  (sum(fossil_dom, na.rm=TRUE) + 1),
    n_countries = n_distinct(REF_AREA),
    .groups = "drop"
  )

cat("\n=== ANNUAL TOTALS BY GROUP ===\n")
print(as.data.frame(annual))

# Percentage change from 2014
base_2014 <- annual %>%
  filter(TIME_PERIOD == 2014) %>%
  select(group, base_dom=total_dom, base_for=total_for, base_ratio=ratio)

annual_pct <- annual %>%
  left_join(base_2014, by="group") %>%
  mutate(
    pct_change_dom   = (total_dom - base_dom)   / base_dom   * 100,
    pct_change_for   = (total_for - base_for)   / base_for   * 100,
    pct_change_ratio = (ratio     - base_ratio) / base_ratio * 100
  )

cat("\n=== PERCENTAGE CHANGE FROM 2014 BASELINE ===\n")
print(as.data.frame(annual_pct %>%
  select(TIME_PERIOD, group,
         pct_change_dom, pct_change_for, pct_change_ratio)))

cat("\n=== KEY STATISTICS ===\n")
for (grp in c("OECD","Non-OECD")) {
  sub   <- annual_pct[annual_pct$group == grp, ]
  first <- sub[sub$TIME_PERIOD == min(sub$TIME_PERIOD), ]
  last  <- sub[sub$TIME_PERIOD == max(sub$TIME_PERIOD), ]
  cat("\n", grp, ":\n")
  cat("  Domestic GVA change 2014-", max(sub$TIME_PERIOD), ": ",
      round(last$pct_change_dom, 1), "%\n", sep="")
  cat("  Foreign GVA change 2014-",  max(sub$TIME_PERIOD), ": ",
      round(last$pct_change_for, 1), "%\n", sep="")
  cat("  Ratio in 2014:", round(first$ratio, 4), "\n")
  cat("  Ratio in", max(sub$TIME_PERIOD), ":", round(last$ratio, 4), "\n")
}

# =============================================================================
# PLOT 1 — Foreign/Domestic GVA Ratio over time
# =============================================================================
p1 <- ggplot(annual, aes(x=TIME_PERIOD, y=ratio,
                          colour=group, group=group)) +
  geom_line(linewidth=1.2) +
  geom_point(size=3) +
  scale_colour_manual(values=c("OECD"="#2166ac","Non-OECD"="#d73027")) +
  scale_x_continuous(breaks=seq(2014, 2022, 1)) +
  labs(
    title    = "Foreign-to-Domestic GVA Ratio in Fossil-Fuel-Intensive Industries",
    subtitle = "Aggregated across OECD and non-OECD country groups, 2014 to 2022\n(China and Russia excluded from non-OECD group)",
    x = "Year", y = "Foreign GVA / Domestic GVA",
    colour = "Country Group",
    caption = "Source: OECD AAMNE database. Industries: B05T09, C17T18, C19, C20, C23, C24, D35_E36T39\nChina (CHN) and Russia (RUS) excluded from non-OECD group."
  ) +
  theme_minimal(base_size=13) +
  theme(
    plot.title      = element_text(face="bold", size=13),
    plot.subtitle   = element_text(size=10, colour="grey40"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle=45, hjust=1)
  )

ggsave("GVA_ratio_trend_excl.png", p1, width=9, height=6, dpi=300)
cat("\nSaved: GVA_ratio_trend_excl.png\n")

# =============================================================================
# PLOT 2 — GVA growth relative to 2014 baseline
# =============================================================================
annual_long <- annual_pct %>%
  select(TIME_PERIOD, group,
         `Domestic GVA` = pct_change_dom,
         `Foreign GVA`  = pct_change_for) %>%
  pivot_longer(cols=c(`Domestic GVA`,`Foreign GVA`),
               names_to="Type", values_to="pct_change")

annual_long$series <- paste0(annual_long$Type, " — ", annual_long$group)

p2 <- ggplot(annual_long,
             aes(x=TIME_PERIOD, y=pct_change,
                 colour=series, linetype=series, group=series)) +
  geom_line(linewidth=1.1) +
  geom_point(size=2.5) +
  geom_hline(yintercept=0, linetype="dashed", colour="grey50", alpha=0.5) +
  scale_colour_manual(values=c(
    "Domestic GVA — OECD"     = "#0c71e49d",
    "Domestic GVA — Non-OECD" = "#d73027bb",
    "Foreign GVA — OECD"      = "#0c71e4",
    "Foreign GVA — Non-OECD"  = "#d73027")) +
  scale_linetype_manual(values=c(
    "Domestic GVA — OECD"     = "solid",
    "Domestic GVA — Non-OECD" = "solid",
    "Foreign GVA — OECD"      = "dashed",
    "Foreign GVA — Non-OECD"  = "dashed")) +
  scale_x_continuous(breaks=seq(2014, 2022, 1)) +
  labs(
    title    = "Growth in Fossil-Fuel-Intensive GVA Relative to 2014 Baseline",
    subtitle = "Percentage change from 2014, by ownership type and country group\n(China and Russia excluded from non-OECD group)",
    x = "Year", y = "Percentage change from 2014 (%)",
    colour = NULL, linetype = NULL,
    caption = "Source: OECD AAMNE database. Industries: B05T09, C17T18, C19, C20, C23, C24, D35_E36T39\nChina (CHN) and Russia (RUS) excluded from non-OECD group.\nSolid = Domestic GVA | Dashed = Foreign GVA | Blue = OECD | Red = Non-OECD"
  ) +
  theme_minimal(base_size=13) +
  theme(
    plot.title      = element_text(face="bold", size=13),
    plot.subtitle   = element_text(size=10, colour="grey40"),
    legend.position = "bottom",
    legend.text     = element_text(size=10),
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle=45, hjust=1)
  ) +
  guides(colour=guide_legend(ncol=2), linetype=guide_legend(ncol=2))

ggsave("GVA_growth_trend_excl.png", p2, width=9, height=6, dpi=300)
cat("Saved: GVA_growth_trend_excl.png\n")

cat("\n=== DONE ===\n")
cat("Saved with China and Russia excluded:\n")
cat("  GVA_ratio_trend_excl.png\n")
cat("  GVA_growth_trend_excl.png\n")