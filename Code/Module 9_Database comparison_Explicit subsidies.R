#Module 9: Cross-database comparison of explicit fossil fuel subsidies
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
#
# Goal: Compare country-level explicit fossil fuel subsidy estimates across:
#   (A) IMF 2023  –  EXTERNALfuelsubsidiestemplate2023new.xlsx  / 'data' sheet
#       Variable:  mit.sub.texpprod.tot.all.1  (texp + tpro; million 2021 USD)
#       Consistent with Module 3 Subsidy_Clean construction.
#   (B) IMF 2025  –  EXTERNALfuelsubsidiestemplate2025.csv
#       Variable:  mit.expsub.con.all.all.1  (consumer explicit only; billion 2021 USD)
#   (C) IEA 2025  –  IEA Subsidies 2010-2024.xlsx  / 'Subsidies by country' sheet
#       Product = "Total" (price-gap, all fuels; million real 2024 USD)
#
# Unit harmonisation → ALL converted to BILLION 2017 USD via US GDP deflator
#   Deflator base: GDPDEF.csv  (quarterly; annual average; index 2017 = 100)
#   DEFL_2021_to_2017 = 0.9078  (consistent with Module 3)
#   DEFL_2024_to_2017 = 0.7973
#
# Module 3 consistency note:
#   Module 3 computes Subsidy_Clean from the SAME 'data' sheet using:
#   texp (residential + industrial/power) + tpro (direct producer transfers)
#   → mit.sub.texpprod.tot.all.1 captures this identical aggregation.
#   Module 9 therefore reproduces the SAME explicit subsidy total used in the
#   paper's main analysis, making the cross-database comparison directly
#   interpretable alongside the paper's results.
#
# Figures:
#   Fig9a – Time-series facet for ALL study countries (targetcounty) + key extras
#   Fig9b – 2022 bar chart (all study countries + others with data, sorted by IMF2023)
#   Fig9c – Scatter: IMF 2023 vs IMF 2025 (log scale, 2022)
#   Fig9d – Scatter: IMF 2023 vs IEA (log scale, 2022)
#   Fig9e – Income-group aggregated time series (requires Reg_corr)

library(readxl)
library(tidyverse)
library(ggrepel)
library(scales)
library(ggpubr)
library(RColorBrewer)

# ── paths ─────────────────────────────────────────────────────────────────────
pathdata_others <- "C:/Users/xiang/OneDrive/Energy subsidy and distributional issue/others"

# =============================================================================
# 0. GDP deflator  (quarterly → annual; normalise so 2017 = 1)
# =============================================================================
gdpdef_raw <- read_csv(file.path(pathdata_others, "GDPDEF.csv"),
                       show_col_types = FALSE) %>%
  mutate(year = as.integer(str_sub(observation_date, 1, 4))) %>%
  group_by(year) %>%
  summarise(GDPDEF_ann = mean(GDPDEF, na.rm = TRUE), .groups = "drop")

base_2017 <- gdpdef_raw %>% filter(year == 2017) %>% pull(GDPDEF_ann)

gdpdef <- gdpdef_raw %>%
  mutate(defl = base_2017 / GDPDEF_ann)   # multiply data × defl → 2017 USD

# Scalar deflators used as constants
DEFL_2021_to_2017 <- gdpdef %>% filter(year == 2021) %>% pull(defl)  # ≈ 0.9078
DEFL_2024_to_2017 <- gdpdef %>% filter(year == 2024) %>% pull(defl)  # ≈ 0.7973

message(sprintf("Deflators: 2021→2017 = %.4f | 2024→2017 = %.4f",
                DEFL_2021_to_2017, DEFL_2024_to_2017))

# =============================================================================
# 1.  Study-country reference list  (from Module 7 / targetcounty)
# =============================================================================
# These must ALL appear in every figure.
study_countries_display <- c(
  # individually modelled in paper
  "Brazil","China","India","Mexico","Russian Federation",
  "Indonesia","South Africa","Turkiye",
  "Belgium","Denmark","Spain","Finland","France","United Kingdom","Ireland",
  "Luxembourg","Germany","Italy","Japan","United States"
)

# ── shared theme ──────────────────────────────────────────────────────────────
db_colors <- c("IMF 2023" = "#2166AC", "IMF 2025" = "#D6604D", "IEA 2025" = "#1B7837")
db_shapes <- c("IMF 2023" = 16,        "IMF 2025" = 17,        "IEA 2025" = 15)
db_lty    <- c("IMF 2023" = "solid",   "IMF 2025" = "dashed",  "IEA 2025" = "dotted")

theme_m9 <- theme_classic(base_size = 13) +
  theme(panel.grid.major  = element_line(color = "grey90", linewidth = 0.25),
        axis.line         = element_line(color = "black",  linewidth = 0.3),
        axis.ticks        = element_line(color = "black",  linewidth = 0.3),
        axis.ticks.length = unit(2, "pt"),
        axis.text         = element_text(color = "black"),
        axis.title        = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key.size   = unit(12, "pt"),
        strip.background  = element_rect(fill = "grey92", color = NA),
        strip.text        = element_text(face = "bold", size = 9),
        plot.margin       = margin(8, 12, 8, 8))

unit_caption <- paste0(
  "Unit: billion 2017 USD (harmonised via US GDP deflator from GDPDEF.csv; 2017 = 100).\n",
  "IMF 2023: million 2021 USD × 0.9078 / 1000.  ",
  "IMF 2025: billion 2021 USD × 0.9078.\n",
  "IEA: million real 2024 USD × 0.7973 / 1000.\n",
  "IMF 2023 = texp (price-gap) + tpro (producer transfers), consistent with Module 3.\n",
  "IMF 2025 = consumer-side explicit only (no producer transfers).\n",
  "IEA = price-gap approach, consumption-side, ~48 non-OECD economies."
)

# =============================================================================
# 2. Load IMF 2023
# =============================================================================
message("Loading IMF 2023...")
imf23_raw <- read_excel(
  file.path(pathdata_others, "EXTERNALfuelsubsidiestemplate2023new.xlsx"),
  sheet = "data", col_names = TRUE
)

# Build country name → ISO3 lookup from this file (used for IEA/IMF25 matching)
iso_lookup <- imf23_raw %>%
  select(country = countryname, iso3 = countrycode) %>%
  distinct()

imf23 <- imf23_raw %>%
  rename(country = countryname, iso3 = countrycode) %>%
  filter(scenario == "U1", year %in% 2015:2022) %>%  # 2023+ are IMF projections; excluded
  mutate(texpprod = replace_na(as.numeric(`mit.sub.texpprod.tot.all.1`), 0),
         # million 2021 USD → billion 2017 USD
         explicit_bn_2017 = texpprod * DEFL_2021_to_2017 / 1000,
         source = "IMF 2023") %>%
  select(country, iso3, year, explicit_bn_2017, source)

# =============================================================================
# 3. Load IMF 2025
# =============================================================================
message("Loading IMF 2025...")
imf25_raw <- read_csv(
  file.path(pathdata_others, "EXTERNALfuelsubsidiestemplate2025.csv"),
  show_col_types = FALSE
) %>%
  filter(MTCode == "mit.expsub.con.all.all.1", SubScenario == 1)

# IMF 2025 year columns — include 2023 and 2024 actual data
yr_cols <- as.character(2015:2024)
yr_cols_present <- yr_cols[yr_cols %in% names(imf25_raw)]

imf25 <- imf25_raw %>%
  select(country = Country, all_of(yr_cols_present)) %>%
  pivot_longer(-country, names_to = "year", values_to = "val") %>%
  mutate(year             = as.integer(year),
         val              = suppressWarnings(as.numeric(val)),
         val              = replace_na(val, 0),
         # billion 2021 USD → billion 2017 USD
         explicit_bn_2017 = val * DEFL_2021_to_2017,
         source           = "IMF 2025") %>%
  # join iso3 (match on country name; harmonise a few spellings)
  left_join(iso_lookup %>%
              mutate(country = recode(country,
                "Türkiye" = "Turkiye", "Russian Federation" = "Russia",
                .default = country)),
            by = c("country" = "country")) %>%
  select(country, iso3, year, explicit_bn_2017, source)

# =============================================================================
# 4. Load IEA
# =============================================================================
message("Loading IEA...")
iea_all <- read_excel(
  file.path(pathdata_others, "IEA Subsidies 2010-2024.xlsx"),
  sheet = "Subsidies by country", col_names = FALSE
)

# Row 5 (index 4) contains years; data starts at row 12 (index 11)
year_vec   <- suppressWarnings(as.integer(unlist(iea_all[5, ])))
year_cols_iea <- which(!is.na(year_vec))    # column positions of year numbers
iea_years     <- year_vec[year_cols_iea]

iea_df <- iea_all[12:nrow(iea_all), ]
# Rename: col1=country, col2=product, then year columns
names(iea_df)[1:2] <- c("country","product")
for (j in seq_along(year_cols_iea)) {
  names(iea_df)[year_cols_iea[j]] <- paste0("yr_", iea_years[j])
}

iea <- iea_df %>%
  filter(product == "Total") %>%
  select(country, starts_with("yr_")) %>%
  pivot_longer(-country, names_to = "year", values_to = "val") %>%
  mutate(year = as.integer(str_remove(year, "yr_")),
         val  = suppressWarnings(as.numeric(val)),
         val  = replace_na(val, 0),
         # million real 2024 USD → billion 2017 USD
         explicit_bn_2017 = val * DEFL_2024_to_2017 / 1000,
         source = "IEA 2025") %>%
  filter(year %in% 2015:2024) %>%
  left_join(iso_lookup %>%
              mutate(country = recode(country,
                "Russian Federation" = "Russia",
                "Türkiye" = "Turkiye", .default = country)),
            by = c("country" = "country")) %>%
  select(country, iso3, year, explicit_bn_2017, source)

# =============================================================================
# 5. Combine
# =============================================================================
message("Combining...")
all3 <- bind_rows(imf23, imf25, iea) %>%
  mutate(source = factor(source, levels = c("IMF 2023", "IMF 2025", "IEA 2025")),
         # flag study countries (as they appear in each database)
         is_study = country %in% study_countries_display |
           country %in% c("Russia","Türkiye","Turkey","Türkiye"))

write.csv(all3,
          file.path(pathout5, "Module9_ExplicitSubsidy_Comparison_2017USD.csv"),
          row.names = FALSE)

# =============================================================================
# 6. Fig 9a – Time-series: ALL study countries (facet)
# =============================================================================
message("Plotting Fig 9a...")

# Harmonise country names to display labels
name_recode <- c("Russian Federation" = "Russia",
                 "Türkiye"            = "Turkiye")

# The set of countries to show: study_countries_display (using whichever name
# exists in each database)
study_name_variants <- c(study_countries_display,
                         "Russia","Türkiye","Turkey")

ts_data <- all3 %>%
  filter(country %in% study_name_variants) %>%
  mutate(country_disp = recode(country, !!!name_recode)) %>%
  group_by(country_disp, year, source) %>%
  summarise(explicit_bn_2017 = sum(explicit_bn_2017, na.rm = TRUE),
            .groups = "drop") %>%
  # force factor order: BRICS-like big emitters first, then EU, then others
  mutate(country_disp = factor(country_disp, levels = c(
    "China","India","Russia","Indonesia","Brazil","Mexico","South Africa","Turkiye",
    "United States","Japan",
    "Germany","France","United Kingdom","Italy","Spain",
    "Belgium","Denmark","Finland","Ireland","Luxembourg"
  )))

Fig.9a <- ts_data %>%
  filter(!is.na(country_disp)) %>%
  ggplot(aes(x = year, y = explicit_bn_2017,
             colour = source, shape = source, linetype = source)) +
  geom_line(linewidth = 0.65, alpha = 0.9) +
  geom_point(size = 1.8) +
  facet_wrap(~ country_disp, scales = "free_y", ncol = 5) +
  scale_color_manual(values = db_colors) +
  scale_shape_manual(values = db_shapes) +
  scale_linetype_manual(values = db_lty) +
  geom_vline(xintercept = 2022.5, colour = "grey50", linetype = "longdash",
             linewidth = 0.4) +
  # annotate("text", x = 2022.6, y = -Inf, label = "IMF 2023\nends",
  #          hjust = 0, vjust = -0.3, size = 2.2, colour = "grey45",
  #          family = "sans") +
  scale_x_continuous(breaks = c(2015, 2017, 2019, 2021, 2023),
                     limits = c(2015, 2024)) +
  scale_y_continuous(labels = number_format(accuracy = 1)) +
  labs(x = NULL, y = "Explicit subsidy (billion 2017 USD)",
       colour = "Database", shape = "Database", linetype = "Database",
       title = "Explicit fossil fuel subsidies — cross-database comparison (study countries, 2015-2024)",
       subtitle = "IMF 2023 shown for 2015-2022 only (2023+ are IMF projections). IMF 2025 & IEA include 2023-2024 actuals.",
       caption = unit_caption) +
  theme_m9 +
  theme(legend.position = "top",
        legend.title    = element_text(face = "bold"),
        plot.subtitle   = element_text(size = 9, colour = "grey40"),
        axis.text.x     = element_text(size = 7))

ggsave(file.path(pathout5, "Fig9a_ExplicitSubsidy_TimeSeries_StudyCountries.jpg"),
       plot = Fig.9a, width = 9, height = 8, dpi = 300)
message("  Fig9a saved.")

# =============================================================================
# 7. Fig 9b – 2022 bar: study countries (guaranteed) + other top countries
# =============================================================================
message("Plotting Fig 9b...")

# Determine top-N non-study countries by IMF 2023 (2022)
top_other <- all3 %>%
  filter(year == 2022, source == "IMF 2023",
         !country %in% study_name_variants,
         !is.na(explicit_bn_2017)) %>%
  arrange(desc(explicit_bn_2017)) %>%
  slice_head(n = 15) %>%
  pull(country)

bar_countries <- union(study_name_variants, top_other)

bar_data <- all3 %>%
  filter(year == 2022, country %in% bar_countries,
         !is.na(explicit_bn_2017)) %>%
  mutate(country_disp = recode(country, !!!name_recode),
         is_study_flag = country_disp %in%
           recode(study_countries_display, !!!name_recode)) %>%
  group_by(country_disp, source, is_study_flag) %>%
  summarise(explicit_bn_2017 = sum(explicit_bn_2017, na.rm = TRUE),
            .groups = "drop")

# order by IMF 2023 descending
order_bar <- bar_data %>%
  filter(source == "IMF 2023") %>%
  arrange(explicit_bn_2017) %>%
  pull(country_disp)

Fig.9b <- bar_data %>%
  mutate(country_disp = factor(country_disp, levels = order_bar),
         alpha_val = ifelse(is_study_flag, 1.0, 0.65)) %>%
  ggplot(aes(x = country_disp, y = explicit_bn_2017,
             fill = source, alpha = alpha_val)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  coord_flip() +
  scale_fill_manual(values = db_colors) +
  scale_alpha_identity() +
  scale_y_continuous(labels = comma_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.07))) +
  annotate("text", x = Inf, y = Inf,
           label = "Darker bars = study countries",
           hjust = 1.05, vjust = 1.5, size = 3, color = "grey40") +
  labs(x = NULL, y = "Explicit subsidy (billion 2017 USD, 2022)",
       fill = "Database",
       title = "Explicit fossil fuel subsidies, 2022 — study countries + top others",
       caption = unit_caption) +
  theme_m9 +
  theme(legend.position = "top",
        legend.title    = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.25))

ggsave(file.path(pathout5, "Fig9b_ExplicitSubsidy_Bar2022.jpg"),
       plot = Fig.9b, width = 7, height = 9, dpi = 500)
message("  Fig9b saved.")

# =============================================================================
# 8. Fig 9c – Scatter: IMF 2023 vs IMF 2025 (2022)
# =============================================================================
message("Plotting Fig 9c...")

cmp_imf <- all3 %>%
  filter(year == 2022,
         source %in% c("IMF 2023","IMF 2025"),
         !is.na(explicit_bn_2017), explicit_bn_2017 >= 0) %>%
  mutate(country_disp = recode(country, !!!name_recode)) %>%
  group_by(country_disp, source) %>%
  summarise(v = sum(explicit_bn_2017, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = source, values_from = v) %>%
  filter(!is.na(`IMF 2023`), !is.na(`IMF 2025`), `IMF 2023` > 0.001) %>%
  mutate(is_study = country_disp %in%
           recode(study_countries_display, !!!name_recode),
         label_show = is_study | `IMF 2023` > 5)

Fig.9c <- cmp_imf %>%
  ggplot(aes(x = `IMF 2023`, y = `IMF 2025`)) +
  geom_abline(color = "grey55", linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(fill = is_study, size = `IMF 2023`),
             shape = 21, stroke = 0.3, alpha = 0.85,
             color = "black") +
  scale_fill_manual(values = c("TRUE" = db_colors["IMF 2025"],
                               "FALSE" = "grey70"),
                    labels = c("TRUE" = "Study country",
                               "FALSE" = "Other country"),
                    name = NULL) +
  geom_text_repel(data = . %>% filter(label_show),
                  aes(label = country_disp),
                  size = 2.7, family = "sans", max.overlaps = 25,
                  box.padding = 0.3, segment.size = 0.2, segment.alpha = 0.5) +
  scale_x_log10(labels = number_format(accuracy = 0.01)) +
  scale_y_log10(labels = number_format(accuracy = 0.01)) +
  scale_size_continuous(range = c(1.5, 9), guide = "none") +
  labs(x = "IMF 2023 (billion 2017 USD, log)",
       y = "IMF 2025 (billion 2017 USD, log)",
       title = "IMF 2023 vs IMF 2025 — explicit subsidies, 2022",
       caption = paste0(unit_caption, "\n",
         "Points BELOW 45° line: IMF 2025 < IMF 2023, i.e., producer transfers (tpro) are important.")) +
  theme_m9 +
  theme(legend.position = c(0.18, 0.88))

ggsave(file.path(pathout5, "Fig9c_ExplicitSubsidy_Scatter_IMF23vsIMF25.jpg"),
       plot = Fig.9c, width = 8, height = 7, dpi = 500)
message("  Fig9c saved.")

# =============================================================================
# 9. Fig 9d – Scatter: IMF 2023 vs IEA (2022)
# =============================================================================
message("Plotting Fig 9d...")

cmp_iea <- all3 %>%
  filter(year == 2022,
         source %in% c("IMF 2023","IEA 2025"),
         !is.na(explicit_bn_2017), explicit_bn_2017 >= 0) %>%
  mutate(country_disp = recode(country, !!!name_recode)) %>%
  group_by(country_disp, source) %>%
  summarise(v = sum(explicit_bn_2017, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = source, values_from = v) %>%
  filter(!is.na(`IMF 2023`), !is.na(`IEA 2025`), `IMF 2023` > 0.001) %>%
  mutate(is_study  = country_disp %in%
           recode(study_countries_display, !!!name_recode),
         label_show = is_study | `IEA 2025` > 2)

Fig.9d <- cmp_iea %>%
  ggplot(aes(x = `IMF 2023`, y = `IEA 2025`)) +
  geom_abline(color = "grey55", linetype = "dashed", linewidth = 0.5) +
  geom_point(aes(fill = is_study, size = `IMF 2023`),
             shape = 21, stroke = 0.3, alpha = 0.85, color = "black") +
  scale_fill_manual(values = c("TRUE" = db_colors["IEA 2025"],
                               "FALSE" = "grey70"),
                    labels = c("TRUE" = "Study country",
                               "FALSE" = "Other country"),
                    name = NULL) +
  geom_text_repel(data = . %>% filter(label_show),
                  aes(label = country_disp),
                  size = 2.7, family = "sans", max.overlaps = 25,
                  box.padding = 0.3, segment.size = 0.2, segment.alpha = 0.5) +
  scale_x_log10(labels = number_format(accuracy = 0.01)) +
  scale_y_log10(labels = number_format(accuracy = 0.01)) +
  scale_size_continuous(range = c(1.5, 9), guide = "none") +
  labs(x = "IMF 2023 (billion 2017 USD, log)",
       y = "IEA 2025 (billion 2017 USD, log)",
       title = "IMF 2023 vs IEA — explicit subsidies, 2022",
       caption = paste0(unit_caption, "\n",
         "IEA coverage: ~48 non-OECD economies with price gaps; OECD shown as zero/missing.\n",
         "Points ABOVE 45° line: IEA > IMF 2023 — typically where IEA captures\n",
         "broader price-gap scope (e.g. natural gas for Russia, electricity for Middle East).")) +
  theme_m9 +
  theme(legend.position = c(0.18, 0.88))

ggsave(file.path(pathout5, "Fig9d_ExplicitSubsidy_Scatter_IMF23vsIEA.jpg"),
       plot = Fig.9d, width = 8, height = 7, dpi = 500)
message("  Fig9d saved.")

# =============================================================================
# 10. Fig 9e – Income-group aggregated time series (requires Reg_corr)
# =============================================================================
if (exists("Reg_corr")) {
  message("Plotting Fig 9e...")

  ig_lookup <- Reg_corr %>%
    select(country = WBGDPreg, WB_IncomeGroup) %>%
    mutate(WB_IncomeGroup = recode(WB_IncomeGroup,
      "Low income"          = "Other LICs",
      "Lower middle income" = "Other LMICs",
      "Upper middle income" = "Other UMICs",
      "High income"         = "Other HICs"
    ))

  agg_ts <- all3 %>%
    mutate(country_disp = recode(country, !!!name_recode)) %>%
    left_join(ig_lookup, by = c("country_disp" = "country")) %>%
    filter(!is.na(WB_IncomeGroup), !is.na(explicit_bn_2017)) %>%
    group_by(WB_IncomeGroup, year, source) %>%
    summarise(explicit_bn_2017 = sum(explicit_bn_2017, na.rm = TRUE),
              .groups = "drop")

  Fig.9e <- agg_ts %>%
    ggplot(aes(x = year, y = explicit_bn_2017,
               colour = source, shape = source, linetype = source)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.2) +
    facet_wrap(~ WB_IncomeGroup, scales = "free_y", ncol = 2) +
    scale_color_manual(values = db_colors) +
    scale_shape_manual(values = db_shapes) +
    scale_linetype_manual(values = db_lty) +
    scale_y_continuous(labels = comma_format(accuracy = 1)) +
    labs(x = "Year", y = "Explicit subsidy (billion 2017 USD)",
         colour = "Database", shape = "Database", linetype = "Database",
         title = "Aggregate explicit subsidies by income group — cross-database comparison",
         caption = unit_caption) +
    theme_m9 +
    theme(legend.position = "top",
          legend.title    = element_text(face = "bold"))

  ggsave(file.path(pathout5, "Fig9e_ExplicitSubsidy_IncomeGroup_TimeSeries.jpg"),
         plot = Fig.9e, width = 10, height = 7, dpi = 500)
  message("  Fig9e saved.")
} else {
  message("  Reg_corr not found in environment — skipping Fig9e. Run after Module 7.")
}

message("Module 9 complete. Output saved to pathout5.")

# ── Clean up ──────────────────────────────────────────────────────────────────
rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                   "pathdata3","pathdata4","pathcode"))])
gc()
