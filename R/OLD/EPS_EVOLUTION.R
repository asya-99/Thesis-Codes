# =============================================================================
# Plot 1: EPI evolution — spaghetti plot with group mean highlighted
#         Individual country lines + thick group mean
#         OECD vs non-OECD, 2014-2023
#
# Plot 2: EPS evolution — box plots over time
#         EU vs rest of OECD, 2000-2020
# =============================================================================

library(readxl)
library(ggplot2)
library(dplyr)

# =============================================================================
# COUNTRY LISTS
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

# =============================================================================
# LOAD DATA
# =============================================================================
df <- read_excel("COMPLETE_GVA.xlsx", sheet = "Sheet1")
names(df) <- trimws(names(df))
df$TIME_PERIOD <- as.numeric(df$TIME_PERIOD)
df$EPI         <- suppressWarnings(as.numeric(df$EPI))
df$EPS         <- suppressWarnings(as.numeric(df$EPS))

df$group_epi <- ifelse(df$REF_AREA %in% oecd_iso3, "OECD", "Non-OECD")
df$group_eps <- case_when(
  df$REF_AREA %in% eu_iso3                                 ~ "EU",
  df$REF_AREA %in% oecd_iso3 & !df$REF_AREA %in% eu_iso3 ~ "OECD (non-EU)",
  TRUE                                                       ~ NA_character_
)

# Colour palette
col_oecd    <- "#2166ac"
col_nonoecd <- "#d73027"
col_eu      <- "#1a9641"
col_rest    <- "#2166ac"

# =============================================================================
# PLOT 1 — EPI spaghetti plot
# Thin lines = individual countries
# Thick lines = group mean
# =============================================================================
epi_df <- df %>%
  filter(!is.na(EPI), !is.na(TIME_PERIOD),
         TIME_PERIOD >= 2014, TIME_PERIOD <= 2023)

epi_mean <- epi_df %>%
  group_by(TIME_PERIOD, group_epi) %>%
  summarise(mean_epi = mean(EPI, na.rm = TRUE), .groups = "drop")

p1 <- ggplot() +
  # Individual country lines — thin and transparent
  geom_line(data = epi_df,
            aes(x = TIME_PERIOD, y = EPI,
                group = REF_AREA, colour = group_epi),
            alpha = 0.18, linewidth = 0.4) +
  # Group mean lines — thick and opaque
  geom_line(data = epi_mean,
            aes(x = TIME_PERIOD, y = mean_epi,
                colour = group_epi),
            linewidth = 2) +
  geom_point(data = epi_mean,
             aes(x = TIME_PERIOD, y = mean_epi,
                 colour = group_epi),
             size = 3.5) +
  scale_colour_manual(
    values = c("OECD" = col_oecd, "Non-OECD" = col_nonoecd)
  ) +
  scale_x_continuous(breaks = seq(2014, 2023, 1)) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 20)) +
  labs(
    title    = "Evolution of Yale EPI Climate Change Mitigation Scores",
    subtitle = "Individual country trajectories (thin lines) and group mean (thick lines), 2014 to 2023",
    x        = "Year",
    y        = "EPI Climate Change Mitigation Score",
    colour   = "Country Group",
    caption  = paste0("Source: Yale Environmental Performance Index (2024).\n",
                      "Thin lines = individual countries. Thick lines = group mean.\n",
                      "OECD = 37 countries | Non-OECD = 35 countries.")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("EPI_spaghetti.png", p1, width = 10, height = 6.5, dpi = 300)
cat("Saved: EPI_spaghetti.png\n")

# =============================================================================
# PLOT 2 — EPS box plots over time
# One box per group per year showing median, IQR, and outliers
# =============================================================================
eps_df <- df %>%
  filter(!is.na(EPS), !is.na(TIME_PERIOD),
         !is.na(group_eps),
         TIME_PERIOD >= 2000, TIME_PERIOD <= 2020)

# Use every 2 years to avoid overcrowding
eps_df_filtered <- eps_df %>%
  filter(TIME_PERIOD %in% seq(2000, 2020, 2))

p2 <- ggplot(eps_df_filtered,
             aes(x = factor(TIME_PERIOD),
                 y = EPS,
                 fill = group_eps,
                 colour = group_eps)) +
  geom_boxplot(alpha = 0.5, outlier.size = 1.5,
               outlier.alpha = 0.6,
               position = position_dodge(width = 0.8),
               width = 0.6) +
  scale_fill_manual(values   = c("EU" = col_eu,
                                  "OECD (non-EU)" = col_rest)) +
  scale_colour_manual(values = c("EU" = col_eu,
                                  "OECD (non-EU)" = col_rest)) +
  labs(
    title    = "Distribution of OECD Environmental Policy Stringency Scores",
    subtitle = "Box plots showing median, interquartile range, and outliers by country group, 2000 to 2020",
    x        = "Year",
    y        = "EPS Index Score",
    fill     = "Country Group",
    colour   = "Country Group",
    caption  = paste0("Source: OECD Environmental Policy Stringency Index (2024).\n",
                      "Box = 25th to 75th percentile | Line = median | Points = outliers.\n",
                      "EU = 20 countries | OECD non-EU = 17 countries.")
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, colour = "grey40"),
    legend.position  = "bottom",
    legend.title     = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("EPS_boxplot.png", p2, width = 10, height = 6.5, dpi = 300)
cat("Saved: EPS_boxplot.png\n")

cat("\nDone. Two plots saved:\n")
cat("  EPI_spaghetti.png — individual country EPI lines + group mean\n")
cat("  EPS_boxplot.png   — box plots of EPS distribution by group over time\n")

# Print summary stats for caption verification
cat("\nEPI group means by year:\n")
print(epi_mean)
cat("\nEPS medians by group and year:\n")
eps_df_filtered %>%
  group_by(TIME_PERIOD, group_eps) %>%
  summarise(median = round(median(EPS, na.rm=TRUE), 2),
            n = n(), .groups="drop") %>%
  print()