#Module 8: Additional Visualization
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
#
# Produces Module-7-style poverty / inequality / CO2 figures
# for the 80% recycling share scenario.
# (Poverty-neutral threshold analysis removed; only 80% and 100% are simulated.)
#
# Input:  pathout3 / "Poverty, Ineq, Emission outcome by subsidy, sp, recy80percent.Rdata"
#         pathout5 / "Results summary100percent.Rdata" (for Reg_corr, Population)
# Output: Fig_recy80_*.jpg  /  recy80_*.csv

# -------------------------------------------------------
# 0. Setup – recycling grid and ordering (match Module 5 V6 / Module 7 V9)
# -------------------------------------------------------
Recy    <- c(0.80, 1.00)
Recynam <- c("80percent", "100percent")

# SP scenario ordering (no Universal – Module 4 V5)
order.sp     <- c("Null","Cash","SP_current","SP_covid")
order.spname <- c("No social\nassistance",
                  "(S1) Cash-based\nprograms",
                  "(S2) Current social\nassistance",
                  "(S3) Social assistance\nCOVID-19 expansion")

targetcounty <- c("Belgium","Denmark","Spain","Finland","France","United Kingdom","Ireland",
                  "Indonesia","South Africa","Turkiye","Japan",
                  "Luxembourg","Germany","Italy","United States",
                  "Brazil","China","India","Mexico","Russian Federation")

order.reg <- c("Brazil","China","India","Mexico","Russian Federation",
               "Indonesia","South Africa","Turkiye",
               "Belgium","Denmark","Spain","Finland","France","Ireland",
               "Luxembourg","Germany","Italy",
               "Japan","United Kingdom","United States",
               "Other LICs","Other LMICs","Other UMICs","Other HICs")

# Income-group aggregation labels (same as Module 7 V9)
# -------------------------------------------------------
# Load Results summary (contains Reg_corr, Population, etc.)
# If already in memory from Module 7, this is a no-op for those objects.
# Using 100percent version to get Reg_corr in its pre-mutated or post-mutated form.
# -------------------------------------------------------
load(str_c(pathout5, "/Results summary100percent.Rdata"))
# Ensure 4-category WB_IncomeGroup — robust to BOTH raw WB labels ("Low income" etc.)
# AND already-collapsed labels ("Other LICs" etc.) from a prior Module 7 run.
Reg_corr <- Reg_corr %>% mutate(WB_IncomeGroup = ifelse(grepl("Low income", WB_IncomeGroup),"Other LICs",
                                            ifelse(grepl("Lower middle income", WB_IncomeGroup),"Other LMICs",
                                                   ifelse(grepl("Upper middle income", WB_IncomeGroup),"Other UMICs",
                                                          "Other HICs")))) 


# Section 8: Module-7-style figures at 80% recycling share
# =============================================================================
# Loads the pre-computed results for Recy = 0.80 ("80percent") and reproduces
# the main poverty / inequality / CO2 figures from Module 7 with identical
# styling, so that 100% and 80% results can be compared side-by-side.
# Figures are saved with the suffix "_recy80" in pathout5.
# =============================================================================

library(ggrepel)
library(ggpubr)
library(RColorBrewer)
library(wesanderson)
library(viridis)
library(scales)
library(stringr)

Recy80_nam <- "80percent"
load(str_c(pathout3, "/Poverty, Ineq, Emission outcome by subsidy, sp, recy", Recy80_nam, ".Rdata"))
load(str_c(pathout2, "/Subsidy scenarios_price and CO2 response_v5.Rdata"))

# ── shared theme (identical to Module 7) ──────────────────────────────────────
scheme80 <- theme_test(base_line_size = 1, base_size = 11) +
  theme(legend.position   = "top",
        legend.title       = element_text(size = 14, face = "bold"),
        legend.box         = "vertical",
        legend.text        = element_text(size = 11),
        legend.background  = element_blank(),
        legend.key.height  = unit(8,  "pt"),
        legend.key.width   = unit(45, "pt"),
        legend.spacing.y   = unit(10, "pt"),
        axis.title         = element_text(size = 16, face = "bold"))

theme_scatter80 <- list(
  theme_classic(base_size = 12),
  theme(panel.grid.major  = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.minor  = element_blank(),
        axis.line         = element_line(color = "black", linewidth = 0.3),
        axis.ticks        = element_line(color = "black", linewidth = 0.1),
        axis.ticks.length = unit(2, "pt"),
        axis.text         = element_text(color = "black"),
        axis.title        = element_text(size = 12, face = "bold"),
        legend.position   = c(0.85, 0.8),
        legend.title      = element_text(size = 10, face = "bold"),
        legend.text       = element_text(size = 9),
        plot.margin       = margin(5.5, 10, 5.5, 5.5))
)

# helper: build aggregated + country-specific Data4 (no-SP baseline scatter)
make_data4 <- function(arr_slice) {
  df <- as.data.frame(arr_slice)
  df$Country <- rownames(df)

  agg <- df %>%
    filter(!Country %in% targetcounty) %>%
    filter(Country %in% Reg_corr$WBGDPreg) %>%
    left_join(Reg_corr %>% select(WBGDPreg, WB_IncomeGroup, Pop_2017_WB),
              by = c("Country" = "WBGDPreg")) %>%
    group_by(WB_IncomeGroup) %>%
    summarise(npl_chg      = sum(PPop_npl_sub - PPop_npl_ori),
              npl_chgrate  = sum(PPop_npl_sub - PPop_npl_ori) / sum(PPop_npl_ori) * 100,
              Gini_chg     = weighted.mean(Gini_sub - Gini_ori, Pop),
              CO2_chg      = sum(CO2_sub - CO2_ori),
              CO2_chgrate  = sum(CO2_sub - CO2_ori) / sum(CO2_ori) * 100)

  ctry <- df %>%
    filter(Country %in% targetcounty) %>%
    group_by(Country) %>%
    summarise(npl_chg      = PPop_npl_sub - PPop_npl_ori,
              npl_chgrate  = (PPop_npl_sub - PPop_npl_ori) / PPop_npl_ori * 100,
              Gini_chg     = Gini_sub - Gini_ori,
              CO2_chg      = CO2_sub - CO2_ori,
              CO2_chgrate  = (CO2_sub - CO2_ori) / CO2_ori * 100)

  ctry %>% bind_rows(setNames(agg, names(ctry))) %>%
    mutate(Country = factor(Country, levels = order.reg))
}

# helper: build SP-scenario long Data6
make_data6 <- function(arr_sp) {
  INT <- arr_sp
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(arr_sp)[[2]], dim(arr_sp)[3]),
                         rep(dimnames(arr_sp)[[3]], each = dim(arr_sp)[2]), sep = "::")
  INT <- as.data.frame(INT)
  INT$Country <- rownames(arr_sp)

  INT %>% pivot_longer(-Country, names_to = "Vari") %>%
    separate(col = Vari, "::", into = c("Variable", "SPScenario")) %>%
    pivot_wider(id_cols = c(Country, SPScenario),
                names_from = Variable, values_from = value) -> INT_long

  agg <- INT_long %>%
    filter(!Country %in% targetcounty) %>%
    filter(Country %in% Reg_corr$WBGDPreg) %>%
    left_join(Reg_corr %>% select(WBGDPreg, WB_IncomeGroup, Pop_2017_WB),
              by = c("Country" = "WBGDPreg")) %>%
    group_by(WB_IncomeGroup, SPScenario) %>%
    summarise(npl_chg      = sum(PPop_npl_sub - PPop_npl_ori),
              npl_chgrate  = sum(PPop_npl_sub - PPop_npl_ori) / sum(PPop_npl_ori) * 100,
              Gini_chg     = weighted.mean(Gini_sub - Gini_ori, Pop),
              CO2_chg      = sum(CO2_sub - CO2_ori),
              CO2_chgrate  = sum(CO2_sub - CO2_ori) / sum(CO2_ori) * 100,
              Ineq_chg     = weighted.mean(IneqRat_sub - IneqRat_ori, Pop),
              Ineq_chgrate = weighted.mean(IneqRat_sub - IneqRat_ori, Pop) /
                              weighted.mean(IneqRat_ori, Pop) * 100)

  ctry <- INT_long %>%
    filter(Country %in% targetcounty) %>%
    group_by(Country, SPScenario) %>%
    summarise(npl_chg      = PPop_npl_sub - PPop_npl_ori,
              npl_chgrate  = (PPop_npl_sub - PPop_npl_ori) / PPop_npl_ori * 100,
              Gini_chg     = Gini_sub - Gini_ori,
              CO2_chg      = CO2_sub - CO2_ori,
              CO2_chgrate  = (CO2_sub - CO2_ori) / CO2_ori * 100,
              Ineq_chg     = IneqRat_sub - IneqRat_ori,
              Ineq_chgrate = (IneqRat_sub - IneqRat_ori) / IneqRat_ori * 100)

  ctry %>% bind_rows(setNames(agg, names(ctry))) %>%
    mutate(Country = factor(Country, levels = order.reg))
}


# ── 8-A  No-SP baseline scatter: Explicit (mirrors Fig 2b in Module 7) ────────
D4_exp80 <- make_data4(OUT_SUBSP[,,1,1])

Fig.8A_exp <- D4_exp80 %>% ggplot() +
  geom_point(aes(npl_chgrate, CO2_chgrate, colour = Gini_chg),
             size = 5, alpha = 0.7) +
  geom_text_repel(aes(npl_chgrate, CO2_chgrate, label = Country),
                  size = 3, force = 5, family = "sans",
                  box.padding = 0.3, segment.size = 0.2, point.padding = 0.2,
                  max.overlaps = Inf) +
  geom_hline(yintercept = 0, color = "blue", linetype = "dotted") +
  geom_vline(xintercept = 0, color = "blue", linetype = "dotted") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_x_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_gradient2(low = "grey44", mid = "grey88", high = "#B24745", midpoint = 0) +
  labs(y = "% change in carbon emissions",
       x = "% change in poverty headcount",
       colour = "Δ Gini\n coefficient",
       title = "Explicit subsidy reform — 80% recycling (no SP)") +
  guides(color = guide_colorbar(barwidth = 1, barheight = 5,
                                frame.colour = "black", ticks.colour = "black")) +
  theme_scatter80

# poverty bar
Fig.8A_povbar <- D4_exp80 %>%
  mutate(Country = reorder(Country, npl_chg)) %>%
  ggplot(aes(x = Country, y = npl_chg / 1000)) +
  geom_col(fill = "#1b9e77", width = 0.7) +
  geom_text(aes(label = comma(round(npl_chg / 1000, 0))),
            hjust = -0.1, size = 3, family = "sans") +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Δ poverty headcount (thousand)",
       title = "Poverty headcount change — Explicit, 80% recycling") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_size = 11) +
  theme(panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
        axis.line   = element_line(linewidth = 0.3, colour = "black"),
        axis.ticks  = element_line(linewidth = 0.3, colour = "black"),
        axis.ticks.length = unit(2, "pt"),
        axis.text   = element_text(color = "black"),
        axis.title  = element_text(size = 10, face = "bold"),
        plot.margin = margin(5.5, 30, 5.5, 5.5))

# CO2 bar
Fig.8A_co2bar <- D4_exp80 %>%
  mutate(Country = reorder(Country, -CO2_chg)) %>%
  ggplot(aes(x = Country, y = CO2_chg)) +
  geom_col(fill = "#D95F02", width = 0.7) +
  geom_text(aes(label = comma(round(CO2_chg, 0))),
            hjust = -0.1, size = 3, family = "sans") +
  coord_flip(clip = "off") +
  labs(x = NULL, y = "Δ carbon emissions (MT)",
       title = "CO2 change — Explicit, 80% recycling") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_classic(base_size = 11) +
  theme(panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
        axis.line   = element_line(linewidth = 0.3, colour = "black"),
        axis.ticks  = element_line(linewidth = 0.3, colour = "black"),
        axis.ticks.length = unit(2, "pt"),
        axis.text   = element_text(color = "black"),
        axis.title  = element_text(size = 10, face = "bold"),
        plot.margin = margin(5.5, 30, 5.5, 5.5))

ggarrange(ggarrange(Fig.8A_exp, Fig.8A_co2bar, Fig.8A_povbar,
                    nrow = 1, widths = c(2, 1, 1),
                    labels = c("(a)", "(b)", "(c)"),
                    font.label = list(size = 12, face = "bold")))

ggsave(str_c(pathout5, "/Fig_recy80_Explicit_scatter_bar.jpg"),
       width = 12, height = 6, dpi = 500)
write.csv(D4_exp80, str_c(pathout5, "/recy80_pov_ineq_CO2_Explicit.csv"))


# ── 8-B  SP-scenario scatter by subsidy type (mirrors Module 7 lines 770–1000) ─

sp_scatter_theme <- list(
  theme_classic(base_size = 12),
  theme(panel.grid.major  = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.minor  = element_blank(),
        axis.line         = element_line(color = "black", linewidth = 0.2),
        axis.ticks        = element_line(color = "black", linewidth = 0.2),
        axis.ticks.length = unit(1, "pt"),
        legend.title      = element_text(face = "bold", size = 10),
        legend.text       = element_text(size = 10),
        axis.text         = element_text(size = 7),
        legend.key.height = unit(8,  "pt"),
        legend.key.width  = unit(14, "pt"),
        strip.background  = element_rect(fill = "grey92", color = NA),
        strip.text        = element_text(size = 9),
        panel.spacing     = unit(6, "pt"),
        plot.margin       = margin(5.5, 10, 5.5, 5.5)),
  theme(legend.position    = "top",
        legend.box         = "vertical",
        legend.background  = element_blank(),
        legend.key.height  = unit(8,  "pt"),
        legend.key.width   = unit(45, "pt"),
        legend.spacing.y   = unit(10, "pt"),
        axis.title         = element_text(face = "bold"))
)

make_sp_scatter <- function(data6, sub_label) {
  data6 %>%
    mutate(SPScenario = factor(SPScenario, levels = order.sp)) %>%
    ggplot() +
    geom_point(aes(round(CO2_chgrate, 0), round(npl_chgrate, 0),
                   fill = Gini_chg, shape = SPScenario),
               size = 5, alpha = 0.9, color = "black", stroke = 0.25) +
    geom_hline(yintercept = 0, color = "grey60", linetype = "dashed", linewidth = 0.3) +
    geom_vline(xintercept = 0, color = "grey60", linetype = "dashed", linewidth = 0.3) +
    facet_wrap(. ~ Country, scales = "free", nrow = 4) +
    labs(x = "% change in carbon emissions",
         y = "% change in poverty headcount",
         shape = "Social assistance\nscenarios",
         title = str_c(sub_label, " — 80% recycling, by SP scenario")) +
    scale_x_continuous(labels = function(x) paste0(x, "%"), expand = c(0.1, 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), expand = c(0.1, 0.1)) +
    scale_fill_gradient2(name = "Δ Gini coefficient",
                         high    = brewer.pal(3, "Dark2")[1],
                         mid     = "gray77",
                         low     = brewer.pal(4, "Dark2")[4],
                         midpoint = 0) +
    scale_shape_manual(values  = c(21:24),
                       breaks  = order.sp,
                       labels  = order.spname) +
    guides(shape = guide_legend(nrow = 1)) +
    sp_scatter_theme
}

# Explicit
D6_exp80 <- make_data6(OUT_SUBSP[,,1,])
make_sp_scatter(D6_exp80, "Explicit subsidy reform")
ggsave(str_c(pathout5, "/Fig_recy80_SP_Explicit.jpg"),
       width = 9, height = 6, dpi = 500)
write.csv(D6_exp80, str_c(pathout5, "/recy80_SP_Explicit.csv"))

# Implicit_prod
D6_impprod80 <- make_data6(OUT_SUBSP[,,2,])
make_sp_scatter(D6_impprod80, "Implicit subsidy — production tax")
ggsave(str_c(pathout5, "/Fig_recy80_SP_ImpProd.jpg"),
       width = 9, height = 6, dpi = 500)
write.csv(D6_impprod80, str_c(pathout5, "/recy80_SP_ImpProd.csv"))

# Implicit_cons
D6_impcons80 <- make_data6(OUT_SUBSP[,,3,])
make_sp_scatter(D6_impcons80, "Implicit subsidy — consumption tax")
ggsave(str_c(pathout5, "/Fig_recy80_SP_ImpCons.jpg"),
       width = 9, height = 6, dpi = 500)
write.csv(D6_impcons80, str_c(pathout5, "/recy80_SP_ImpCons.csv"))


# ── 8-C  No-SP baseline scatters for Implicit_prod and Implicit_cons ──────────
D4_impprod80 <- make_data4(OUT_SUBSP[,,2,1])
D4_impcons80 <- make_data4(OUT_SUBSP[,,3,1])

make_nosp_scatter <- function(data4, sub_label, col_hi = "#B24745") {
  data4 %>% ggplot() +
    geom_point(aes(npl_chgrate, CO2_chgrate, colour = Gini_chg),
               size = 5, alpha = 0.7) +
    geom_text_repel(aes(npl_chgrate, CO2_chgrate, label = Country),
                    size = 3, force = 5, family = "sans",
                    box.padding = 0.3, segment.size = 0.2,
                    point.padding = 0.2, max.overlaps = Inf) +
    geom_hline(yintercept = 0, color = "blue", linetype = "dotted") +
    geom_vline(xintercept = 0, color = "blue", linetype = "dotted") +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_color_gradient2(low = "grey44", mid = "grey88",
                          high = col_hi, midpoint = 0) +
    labs(y = "% change in carbon emissions",
         x = "% change in poverty headcount",
         colour = "Δ Gini\n coefficient",
         title = str_c(sub_label, " — 80% recycling (no SP)")) +
    guides(color = guide_colorbar(barwidth = 1, barheight = 5,
                                  frame.colour = "black", ticks.colour = "black")) +
    theme_scatter80
}

ggarrange(
  make_nosp_scatter(D4_impprod80, "Implicit — production tax"),
  make_nosp_scatter(D4_impcons80, "Implicit — consumption tax"),
  nrow = 1, labels = c("(a)", "(b)"),
  font.label = list(size = 12, face = "bold")
)
ggsave(str_c(pathout5, "/Fig_recy80_ImpProd_ImpCons_scatter.jpg"),
       width = 12, height = 6, dpi = 500)

message("Section 8 (80% recycling figures) complete.")


# -------------------------------------------------------
# Clean up
# -------------------------------------------------------
rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                   "pathdata3","pathdata4","pathcode"))])
gc()

