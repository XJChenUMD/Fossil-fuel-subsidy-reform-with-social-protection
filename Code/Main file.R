

#Main file: run the full analysis pipeline (Modules 1-9)
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

#Set working directory and working environment-----
library(openxlsx)
library(stringr)
library(tidyverse)
library(zoo)
library(ineq)
library(cowplot)
library(RColorBrewer)
library(viridis)
library(ggpubr)
library(ggh4x)
library(wesanderson)
library(ggrepel)

setwd("C:\\Users\\xiang\\OneDrive\\C_Database\\GTAP 11\\GTAP_11b")
path <- getwd()
pathdata <- str_c(path,"/Data/Global SAM blocks_GTAP 11b_2017_160reg65secs")
pathdata2 <- str_c(path,"/Data/CSV files for 2017_GTAP 11b raw data")
pathdata3 <- str_c(path,"/Data/GTAP 2017_CO2 account")
pathdata4 <- str_c("C:/Users/xiang/OneDrive/Draft_Carbon taxation and revenue recycling/Carbon Price Project/Data")

#For IMF 2022 IMF Subsidy data
pathout <- str_c(path,"/Analysis_GTAP SAM_EnergySubsidy_v2");dir.create(pathout)
pathout2 <- str_c(pathout,"/Subsidy scenarios and revenue");dir.create(pathout2)
pathout3 <- str_c(pathout,"/Effect_Subsidy+Social assistance");dir.create(pathout3)
pathout5 <- str_c(pathout,"/Results summary and figs");dir.create(pathout5)
pathcode <- str_c(path,"/Code/R code_Energy Subsidy_v2")

# source(str_c(pathcode,"/Module 1_Read and clean basic blocks_v2.R"))
# source(str_c(pathcode,"/Module 2_Labor income and transfer payments_v5.R"))
source(str_c(pathcode,"/Module 3_Subsidy scenarios_PriceData_v5.R"))
source(str_c(pathcode,"/Module 4_Social assistance scenarios_v6.R"))
source(str_c(pathcode,"/Module 5_Effects_Subsidy+SA_v5_recyshares_v6.R"))
source(str_c(pathcode,"/Module 6_Results summary_v5_recyshares.R"))
source(str_c(pathcode,"/Module 7_Visualization_v9_recyshares100.R"))
source(str_c(pathcode,"/Module 8_Additional Visualization.R"))
source(str_c(pathcode,"/Module 9_Database comparison_Explicit subsidies.R"))



#Robustness check SI2021: rerun with 2021 subsidy data instead of 2022-----
# pathout stays as the main-run base (Module 4 SP_Scenarios is year-invariant)
pathout2 <- str_c(pathout, "/Subsidy scenarios and revenue/SI2021");dir.create(pathout2)
pathout3 <- str_c(pathout, "/Effect_Subsidy+Social assistance/SI2021");dir.create(pathout3)
pathout5 <- str_c(pathout, "/Results summary and figs/SI2021");dir.create(pathout5)

YEAR_USE_2023_OVERRIDE <- 2021L  # switches Module 3's reference year to 2021
source(str_c(pathcode,"/Module 3_Subsidy scenarios_PriceData_v5.R"))
rm(YEAR_USE_2023_OVERRIDE)
message("SI2021 Module 3 complete (YEAR_USE_2023 = 2021 was used as override).")

source(str_c(pathcode,"/Module 5_Effects_Subsidy+SA_v5_recyshares_v6.R"))
source(str_c(pathcode,"/Module 6_Results summary_v5_recyshares.R"))
source(str_c(pathcode,"/Module 7_Visualization_v9_recyshares100.R"))

#Robustness check SI_IMF25: rerun with the IMF 2025 database, reference year 2024-----
pathout2_imf25 <- str_c(pathout, "/Subsidy scenarios and revenue/SI_IMF25");dir.create(pathout2_imf25)
pathout3_imf25 <- str_c(pathout, "/Effect_Subsidy+Social assistance/SI_IMF25");dir.create(pathout3_imf25)
pathout5_imf25 <- str_c(pathout, "/Results summary and figs/SI_IMF25");dir.create(pathout5_imf25)
pathcode_imf25 <- str_c(path, "/Code/R code_Energy Subsidy_v2/SI_IMF25")

source(str_c(pathcode_imf25, "/Module 3_Subsidy scenarios_PriceData_v5_IMF25.R"))
source(str_c(pathcode_imf25, "/Module 5_Effects_Subsidy+SA_v5_recyshares_v6_IMF25.R"))
source(str_c(pathcode_imf25, "/Module 6_Results summary_v5_recyshares_IMF25.R"))
source(str_c(pathcode_imf25, "/Module 7_Visualization_v9_recyshares100_IMF25.R"))
