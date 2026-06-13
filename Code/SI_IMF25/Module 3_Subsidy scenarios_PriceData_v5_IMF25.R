# Module 3 SI_IMF25: Subsidy scenarios — IMF 2025 database, reference year 2024
# Xiangjie Chen
#
# DATA SOURCE: IMF (2025) Fossil Fuel Subsidies Database
#   File: EXTERNALfuelsubsidiestemplate2025.csv
#   Format: long panel; Scenario=ISO3; SubScenario=1 (baseline); years as columns
#   Units: billion 2021 USD
#   Reference year: 2024 (actual data available through 2024)
#
# KEY DIFFERENCES vs Module 3 v5 (IMF 2023):
#   1. Data format: long CSV (MTCode rows, year columns) vs wide Excel
#   2. No sector-level price gap rates -> use monetary/demand ratio directly
#   3. Unit: billion USD (x1000 -> M USD) vs million USD in IMF 2023
#   4. Deflator: same DEFL_2021_to_2017 = 0.90787 (both DBs in 2021 prices)
#   5. tpro (producer transfers) NOT available in IMF 2025 -> explicit = consumer-side only
#
# GTAP sector mapping (same logic as v5):
#   s15 coal  : Exp_Cons=coa.res,             Exp_Prod=coa.ind+coa.pow
#   s17 gas   : Exp_Cons=0,                   Exp_Prod=nga.ind+nga.pow
#   s32 petro : Exp_Cons=die+gso+lpg+ker+oop, Exp_Prod=die+gso+lpg+ker+oop
#   s46 elec  : Exp_Cons=ecy.res,             Exp_Prod=ecy.ind
#   s47 gasdt : Exp_Cons=nga.res,             Exp_Prod=0
#   Implicit : same sector mapping using impsub codes
#
# Outputs to: pathout2_imf25, pathout3_imf25, pathout5_imf25

# =============================================================================
# 0. Load base data (shared with main run)
# =============================================================================
load(str_c(pathout, "/GTAP_11b_2017_Elements for MRIO.Rdata"))
load(str_c(pathout, "/Expenditure,Footprint,Pop,Emission.Rdata"))
regnam <- toupper(regnam)

# =============================================================================
# 1. Expenditure elasticity (identical to v5)
# =============================================================================
Elasticity_raw <- array(0, dim = c(N, length(Reg_Inclu)),
                        dimnames = list(secnam, Reg_Inclu))
for (r in 1:length(Reg_Inclu)) {
  Exp_Sec <- Exp_perCap_detail[,,r]
  Exp     <- colSums(Exp_perCap_detail[,,r], na.rm = TRUE)
  for (j in 1:N) {
    if (sum(Exp_Sec[j,], na.rm = TRUE) != 0) {
      dat  <- data.frame(cbind(log(Exp_Sec[j,]), log(Exp)))
      dat[is.na(dat) | dat == "Inf" | dat == "-Inf"] <- NaN
      eq   <- lm(data = dat, X1 ~ X2)
      Elasticity_raw[j, r] <- eq$coefficients[2]
    }
  }
}
write.csv(Elasticity_raw, file = str_c(pathout2_imf25, "/Elasticity_raw_IMF25.csv"))
Elasticity <- Elasticity_raw
Elasticity[Elasticity <= 0] <- 0.1

# =============================================================================
# 2. LES summary statistics (identical to v5)
# =============================================================================
FinalDemand_ori <- apply(Expenditure_detail_global, c(1,3), sum) +
  Gov_detail_global + Inv_detail_global

DemandTot_ori <- array(0, dim = c(203, length(Reg_Inclu)),
                       dimnames = list(c(Gnam,"Firms","Gov"), Reg_Inclu))
Minumum     <- array(0, dim = dim(FinalDemand_ori), dimnames = dimnames(FinalDemand_ori))
Minumum_reg <- array(0, dim = c(203, length(Reg_Inclu)))
for (r in 1:length(Reg_Inclu)) {
  a <- which(colSums(Expenditure_detail_global[,,r]) > 0)[1]
  Minumum[,r]     <- Expenditure_detail_global[,a,r] / Population[a,r] * sum(Population[,r])
  Minumum_reg[1:201,r] <- sum(Expenditure_detail_global[,a,r]) / Population[a,r] * Population[,r]
}
Mimunum_sum <- colSums(Minumum)

INT2 <- array(0, dim = c(GN, 203, length(Reg_Inclu)),
              dimnames = list(regsecnam, c(Gnam,"Firms","Gov"), Reg_Inclu))
for (r in 1:length(Reg_Inclu)) {
  int  <- cbind(Expenditure_detail_global[,,r], Inv_detail_global[,r], Gov_detail_global[,r])
  xx   <- array(0, dim = c(GN, length(Gnam)+2))
  xx[,1:length(Gnam)] <- t(pracma::repmat((Minumum[,r]/sum(Population[,r])),
                                          length(Gnam),1)) %*% diag(Population[,r])
  INT2[,,r]         <- t(t(int - xx) / (colSums(int) - Minumum_reg[,r]))
  DemandTot_ori[,r] <- colSums(int)
}
INT2[is.nan(INT2)] <- 0;  INT2[is.infinite(INT2)] <- 0

OutputMAT <- TotOutput;  dim(OutputMAT) <- c(N, G)
FD_reg <- array(0, dim = c(N,G), dimnames = list(secnam,regnam))
Z_reg  <- array(0, dim = c(N,G), dimnames = list(secnam,regnam))
for (r in 1:G) {
  fac <- rep(secnam, G)
  m = (r-1)*3+1;  n = r*3;  p = (r-1)*N+1;  q = r*N
  FD_reg[,r] <- rowSums(rowsum(FD[,m:n], fac, reorder=FALSE))
  Z_reg[,r]  <- rowSums(rowsum(Inter_Trade[,p:q], fac, reorder=FALSE))
}
Demand_reg <- FD_reg + Z_reg

# =============================================================================
# 3. Read IMF 2025 data and pivot to wide format
# =============================================================================
YEAR_USE_IMF25   <- 2024L
DEFL_2021_to_2017 <- 0.90787   # same as v5: both DBs in 2021 constant prices
# Unit conversion: IMF 2025 in billion 2021 USD -> M 2017 USD
BN_TO_M_DEFL <- 1000 * DEFL_2021_to_2017   # billion 2021 -> million 2017

path_csv_2025 <- "C:/Users/xiang/OneDrive/Energy subsidy and distributional issue/others/EXTERNALfuelsubsidiestemplate2025.csv"
message("Reading IMF 2025 data...")
library(readr)
IMF25_raw <- read_csv(path_csv_2025, col_types = cols(.default = col_character()))

# Convert year columns to numeric
yr_cols <- as.character(2015:2035)
IMF25_raw <- IMF25_raw %>%
  mutate(across(all_of(yr_cols), ~ suppressWarnings(as.numeric(.))))

# Filter: SubScenario == 1 (baseline)
IMF25 <- IMF25_raw %>% filter(SubScenario == "1" | SubScenario == 1)
message("  Countries in IMF 2025 (SubScenario=1): ", n_distinct(IMF25$Scenario))
message("  Reference year: ", YEAR_USE_IMF25)

# Pivot: one row per country, one column per MTCode (using reference year value)
yr_str <- as.character(YEAR_USE_IMF25)
IMF25_wide <- IMF25 %>%
  select(Scenario, MTCode, all_of(yr_str)) %>%
  rename(value = all_of(yr_str)) %>%
  pivot_wider(names_from = MTCode, values_from = value, values_fn = first)

message("  Countries after pivot: ", nrow(IMF25_wide))

# Helper: safe extract from wide table
safe_col <- function(df, iso, col) {
  row <- df[df$Scenario == iso, ]
  if (nrow(row) == 0 || !col %in% names(row)) return(0)
  v <- row[[col]]
  if (is.null(v) || length(v) == 0 || is.na(v)) return(0)
  v <- suppressWarnings(as.numeric(v))
  if (is.na(v) || v < 0) return(0)
  v
}

# =============================================================================
# 4. Match IMF 2025 countries to GTAP Reg_Inclu
# =============================================================================
# Reg_corr$WBGDPcode = ISO3 matching Scenario in IMF25_wide
n_matched <- 0
for (r in 1:length(Reg_Inclu)) {
  iso <- toupper(Reg_corr$WBGDPcode[r])
  if (iso %in% IMF25_wide$Scenario) n_matched <- n_matched + 1
}
message(n_matched, " of ", length(Reg_Inclu), " countries matched to IMF 2025 data")
message("Computing sector-level price change rates from IMF 2025 price data...")

# =============================================================================
# 5.  Price change rates — IDENTICAL methodology to IMF 2023 Module 3 v5
# =============================================================================
# IMF 2025 provides, for every fuel × sector combination:
#   mit.rp.{fuel}.{sector}.1         retail price (USD/GJ)
#   mit.sup.cost.{fuel}.{sector}.1   supply / benchmark cost (USD/GJ)
#   mit.eff.price.{fuel}.{sector}.1  effective price incl. externalities (USD/GJ)
#
# Explicit rate: max(0, sup_cost / rp - 1)   [same as safe_expl() in v5]
# Implicit rate: max(0, eff_price / max(rp, sup_cost) - 1)  [same as safe_impl()]
#
# After computing raw rates, an alpha calibration anchors aggregate subsidy amounts
# to IMF monetary totals — exactly as in IMF 2023 v5 (Section 6b).
# Petroleum products use physical consumption volumes as averaging weights
# (same role as mit.sub.timp.tot.* in IMF 2023).

# ── Helper functions ─────────────────────────────────────────────────────────
sc_val <- function(iso, fuel) safe_col(IMF25_wide, iso, str_c("mit.sup.cost.", fuel, ".1"))
rp_val <- function(iso, fuel) safe_col(IMF25_wide, iso, str_c("mit.rp.",       fuel, ".1"))
ep_val <- function(iso, fuel) safe_col(IMF25_wide, iso, str_c("mit.eff.price.",fuel, ".1"))
con_val<- function(iso, fuel) safe_col(IMF25_wide, iso, str_c("mit.con.",      fuel, ".1"))

safe_expl_25 <- function(iso, fuel) {
  rp <- rp_val(iso, fuel); sc <- sc_val(iso, fuel)
  if (rp < 1e-9) return(0)
  max(0, sc / rp - 1)
}
safe_impl_25 <- function(iso, fuel) {
  rp <- rp_val(iso, fuel); sc <- sc_val(iso, fuel); ep <- ep_val(iso, fuel)
  fl <- max(rp, sc)
  if (fl < 1e-9) return(0)
  max(0, ep / fl - 1)
}
wt_avg_25 <- function(iso, rate_fn, fuels, wt_fuels = fuels) {
  rates <- sapply(fuels,    function(f) rate_fn(iso, f))
  wts   <- sapply(wt_fuels, function(f) con_val(iso, f))
  wts[is.na(wts)] <- 0
  if (sum(wts) > 1e-12) sum(rates * wts) / sum(wts)
  else mean(rates)
}

calib_log_25 <- matrix(NA_real_, nrow = length(Reg_Inclu), ncol = 4,
                       dimnames = list(Reg_Inclu, c("IMF_expl","raw_expl","alpha_expl","alpha_impl")))

tarsec <- c(15, 17, 32, 46, 47)   # GTAP sector indices (1-based)
PriceChg_expl_cons <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_expl_prod <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_impl_cons <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_impl_prod <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))

scennam <- c("Explicit","Implicit_prod","Implicit_cons","Null")
Sub_Scenarios  <- array(0, dim = c(N*4, length(Reg_Inclu), 4),
                        dimnames = list(
                          str_c(rep(c("Expli_Cons_","Expli_Prod_","Impli_Cons_","Impli_Prod_"), each=N),
                                rep(secnam,4)), Reg_Inclu, scennam))
Sub_revenue    <- array(0, dim = c(length(Reg_Inclu), 5),
                        dimnames = list(Reg_Inclu, c(scennam,"GDP")))
Price_Response <- array(0, dim = c(GN, length(Reg_Inclu), 4),
                        dimnames = list(regsecnam, Reg_Inclu, scennam))
CO2_Reg_Response <- array(0, dim = c(length(Reg_Inclu), 4), dimnames = list(Reg_Inclu, scennam))
DemandTot_new    <- array(0, dim = c(203, length(Reg_Inclu), 4),
                          dimnames = list(c(Gnam,"Firms","Gov"), Reg_Inclu, scennam))
VA_Tot_Chg <- array(0, dim = c(3, length(Reg_Inclu), 4),
                    dimnames = list(c("Lab","Capital","Tax"), Reg_Inclu, scennam))

Sub_revenue[,5] <- Reg_corr$GDP_2017_WB / 10^6

for (r in 1:length(Reg_Inclu)) {
  iso    <- toupper(Reg_corr$WBGDPcode[r])
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))

  if (!iso %in% IMF25_wide$Scenario) next

  # ── Per-fuel explicit rates ─────────────────────────────────────────────
  expl_coa_res <- safe_expl_25(iso, "coa.res")
  expl_coa_ind <- safe_expl_25(iso, "coa.ind")
  expl_coa_pow <- safe_expl_25(iso, "coa.pow")

  expl_nga_res <- safe_expl_25(iso, "nga.res")
  expl_nga_ind <- safe_expl_25(iso, "nga.ind")
  expl_nga_pow <- safe_expl_25(iso, "nga.pow")

  expl_ecy_res <- safe_expl_25(iso, "ecy.res")
  expl_ecy_ind <- safe_expl_25(iso, "ecy.ind")

  # Petroleum: weighted avg across products (weights = physical consumption volume)
  pet_fuels <- c("die.all", "gso.all", "lpg.all", "ker.all")
  expl_pet  <- wt_avg_25(iso, safe_expl_25, pet_fuels)

  # ── Per-fuel implicit rates ─────────────────────────────────────────────
  impl_coa_res <- safe_impl_25(iso, "coa.res")
  impl_coa_ind <- safe_impl_25(iso, "coa.ind")
  impl_coa_pow <- safe_impl_25(iso, "coa.pow")

  impl_nga_res <- safe_impl_25(iso, "nga.res")
  impl_nga_ind <- safe_impl_25(iso, "nga.ind")
  impl_nga_pow <- safe_impl_25(iso, "nga.pow")

  impl_ecy_res <- safe_impl_25(iso, "ecy.res")
  impl_ecy_ind <- safe_impl_25(iso, "ecy.ind")

  impl_pet <- wt_avg_25(iso, safe_impl_25, pet_fuels)

  # ── Aggregate to GTAP sectors — same mapping as IMF 2023 v5 ────────────
  # Coal consumption weights: physical consumption by use type
  wt_coa_ind <- con_val(iso, "coa.ind") + 1e-12   # avoid zero
  wt_coa_pow <- con_val(iso, "coa.pow") + 1e-12

  # Consumer channel [RESIDENTIAL]
  PriceChg_expl_cons[15, tarreg] <- expl_coa_res
  PriceChg_expl_cons[17, tarreg] <- 0               # bridged from s47 below
  PriceChg_expl_cons[32, tarreg] <- expl_pet
  PriceChg_expl_cons[46, tarreg] <- expl_ecy_res
  PriceChg_expl_cons[47, tarreg] <- expl_nga_res    # residential gas distribution

  # Producer channel [INDUSTRY + POWER, consumption-weighted]
  wt_s <- wt_coa_ind + wt_coa_pow
  PriceChg_expl_prod[15, tarreg] <- (expl_coa_ind * wt_coa_ind +
                                      expl_coa_pow * wt_coa_pow) / wt_s

  wt_nga_ind <- con_val(iso, "nga.ind") + 1e-12
  wt_nga_pow <- con_val(iso, "nga.pow") + 1e-12
  wt_ng <- wt_nga_ind + wt_nga_pow
  PriceChg_expl_prod[17, tarreg] <- (expl_nga_ind * wt_nga_ind +
                                      expl_nga_pow * wt_nga_pow) / wt_ng
  PriceChg_expl_prod[32, tarreg] <- expl_pet        # same as consumer (no prod split)
  PriceChg_expl_prod[46, tarreg] <- expl_ecy_ind
  PriceChg_expl_prod[47, tarreg] <- 0               # bridged from s17 below

  # Implicit consumer [RESIDENTIAL]
  PriceChg_impl_cons[15, tarreg] <- impl_coa_res
  PriceChg_impl_cons[17, tarreg] <- 0
  PriceChg_impl_cons[32, tarreg] <- impl_pet
  PriceChg_impl_cons[46, tarreg] <- impl_ecy_res
  PriceChg_impl_cons[47, tarreg] <- impl_nga_res

  # Implicit producer [INDUSTRY + POWER]
  PriceChg_impl_prod[15, tarreg] <- (impl_coa_ind * wt_coa_ind +
                                      impl_coa_pow * wt_coa_pow) / wt_s
  PriceChg_impl_prod[17, tarreg] <- (impl_nga_ind * wt_nga_ind +
                                      impl_nga_pow * wt_nga_pow) / wt_ng
  PriceChg_impl_prod[32, tarreg] <- impl_pet
  PriceChg_impl_prod[46, tarreg] <- impl_ecy_ind
  PriceChg_impl_prod[47, tarreg] <- 0

  # ── Gas bridging: s17 ↔ s47 (same as IMF 2023 v5 lines 603-606) ────────
  PriceChg_expl_cons[17, tarreg] <- PriceChg_expl_cons[47, tarreg]
  PriceChg_expl_prod[47, tarreg] <- PriceChg_expl_prod[17, tarreg]
  PriceChg_impl_cons[17, tarreg] <- PriceChg_impl_cons[47, tarreg]
  PriceChg_impl_prod[47, tarreg] <- PriceChg_impl_prod[17, tarreg]

  # ── Alpha calibration — anchor to IMF monetary totals (same as IMF 2023 v5) ──
  g <- function(code) safe_col(IMF25_wide, iso, code) * BN_TO_M_DEFL

  IMF_expl_r <- g("mit.expsub.con.all.all.1")
  IMF_impl_r <- g("mit.impsub.con.all.all.1")

  raw_expl_r <- sum(PriceChg_expl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg] +
                    PriceChg_expl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg],
                    na.rm = TRUE)
  raw_impl_r <- sum(PriceChg_impl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg] +
                    PriceChg_impl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg],
                    na.rm = TRUE)

  alpha_expl <- if (raw_expl_r > 1e-6 && IMF_expl_r >  0) IMF_expl_r / raw_expl_r else
                if (raw_expl_r > 1e-6 && IMF_expl_r == 0) 0 else 1
  alpha_impl <- if (raw_impl_r > 1e-6 && IMF_impl_r >  0) IMF_impl_r / raw_impl_r else
                if (raw_impl_r > 1e-6 && IMF_impl_r == 0) 0 else 1

  PriceChg_expl_cons[tarsec, tarreg] <- PriceChg_expl_cons[tarsec, tarreg] * alpha_expl
  PriceChg_expl_prod[tarsec, tarreg] <- PriceChg_expl_prod[tarsec, tarreg] * alpha_expl
  PriceChg_impl_cons[tarsec, tarreg] <- PriceChg_impl_cons[tarsec, tarreg] * alpha_impl
  PriceChg_impl_prod[tarsec, tarreg] <- PriceChg_impl_prod[tarsec, tarreg] * alpha_impl

  Sub_revenue[r, 1] <- IMF_expl_r
  Sub_revenue[r, 2] <- IMF_impl_r
  Sub_revenue[r, 3] <- IMF_impl_r

  calib_log_25[r, ] <- c(IMF_expl_r, raw_expl_r, alpha_expl, alpha_impl)
}

message(sprintf("  alpha_expl (IMF25): min=%.2f  median=%.2f  max=%.2f",
  min(calib_log_25[,3], na.rm=TRUE), median(calib_log_25[,3], na.rm=TRUE),
  max(calib_log_25[,3], na.rm=TRUE)))
message(sprintf("  Explicit rate summary: median=%.3f  max=%.3f",
  median(PriceChg_expl_cons[tarsec,], na.rm=TRUE),
  max(PriceChg_expl_cons[tarsec,],    na.rm=TRUE)))
message(sprintf("  Sub_revenue[Explicit] total: %.1f B 2017 USD",
  sum(Sub_revenue[,1], na.rm=TRUE) / 1000))

# Subsidy_Clean: build a Module-7-compatible structure from IMF 2025 monetary amounts.
# Format mirrors IMF 2023 Subsidy_Clean: one row per country-year,
# columns = Coal_Exp_Cons, Gas_Exp_Cons, Petro_Exp_Cons, Elec_Exp_Cons, etc.
year_cols_sc <- as.character(2015:2024)
get_yr <- function(mtcode) {
  IMF25 %>%
    filter(MTCode == mtcode) %>%
    select(Scenario, all_of(year_cols_sc)) %>%
    pivot_longer(all_of(year_cols_sc), names_to = "year", values_to = mtcode) %>%
    mutate(year = as.integer(year))
}
gdp_long   <- get_yr("mit.gdp.pre.lvl.1")
expc_coa_l <- get_yr("mit.expsub.con.coa.res.1")
expc_nga_l <- get_yr("mit.expsub.con.nga.res.1")
expc_die_l <- get_yr("mit.expsub.con.die.all.1")
expc_gso_l <- get_yr("mit.expsub.con.gso.all.1")
expc_lpg_l <- get_yr("mit.expsub.con.lpg.all.1")
expc_ker_l <- get_yr("mit.expsub.con.ker.all.1")
expc_oop_l <- get_yr("mit.expsub.con.oop.all.1")
expc_ecy_l <- get_yr("mit.expsub.con.ecy.res.1")
expp_coa_l <- get_yr("mit.expsub.con.coa.ind.1"); expp_coa_pow <- get_yr("mit.expsub.con.coa.pow.1")
expp_nga_l <- get_yr("mit.expsub.con.nga.ind.1"); expp_nga_pow <- get_yr("mit.expsub.con.nga.pow.1")
expp_ecy_l <- get_yr("mit.expsub.con.ecy.ind.1")
imp_coa_l  <- get_yr("mit.impsub.con.coa.all.1")
imp_nga_l  <- get_yr("mit.impsub.con.nga.all.1")
imp_die_l  <- get_yr("mit.impsub.con.die.all.1")
imp_gso_l  <- get_yr("mit.impsub.con.gso.all.1")
imp_ecy_l  <- get_yr("mit.impsub.con.ecy.res.1")

# Build ISO3 -> full country name lookup from IMF25 raw data
# (Column 'Country' in the CSV = full English name; 'Scenario' = ISO3)
country_names_25 <- IMF25 %>%
  select(Scenario, Country) %>%
  distinct(Scenario, .keep_all = TRUE)

Subsidy_Clean <- gdp_long %>%
  left_join(expc_coa_l, by=c("Scenario","year")) %>%
  left_join(expc_nga_l, by=c("Scenario","year")) %>%
  left_join(expc_die_l, by=c("Scenario","year")) %>%
  left_join(expc_gso_l, by=c("Scenario","year")) %>%
  left_join(expc_lpg_l, by=c("Scenario","year")) %>%
  left_join(expc_ker_l, by=c("Scenario","year")) %>%
  left_join(expc_oop_l, by=c("Scenario","year")) %>%
  left_join(expc_ecy_l, by=c("Scenario","year")) %>%
  left_join(expp_coa_l, by=c("Scenario","year")) %>%
  left_join(expp_coa_pow, by=c("Scenario","year")) %>%
  left_join(expp_nga_l, by=c("Scenario","year")) %>%
  left_join(expp_nga_pow, by=c("Scenario","year")) %>%
  left_join(expp_ecy_l, by=c("Scenario","year")) %>%
  left_join(imp_coa_l,  by=c("Scenario","year")) %>%
  left_join(imp_nga_l,  by=c("Scenario","year")) %>%
  left_join(imp_die_l,  by=c("Scenario","year")) %>%
  left_join(imp_gso_l,  by=c("Scenario","year")) %>%
  left_join(imp_ecy_l,  by=c("Scenario","year")) %>%
  rename(countrycode = Scenario) %>%
  left_join(country_names_25, by = c("countrycode" = "Scenario")) %>%
  rename(countryname = Country) %>%
  filter(countrycode %in% Reg_corr$WBGDPcode) %>%
  mutate(
    GDP          = .data[["mit.gdp.pre.lvl.1"]] * DEFL_2021_to_2017,
    # Consumer-side explicit (M 2017 USD)
    Coal_Exp_Cons  = replace_na(.data[["mit.expsub.con.coa.res.1"]], 0) * BN_TO_M_DEFL,
    Gas_Exp_Cons   = replace_na(.data[["mit.expsub.con.nga.res.1"]], 0) * BN_TO_M_DEFL,
    Petro_Exp_Cons = (replace_na(.data[["mit.expsub.con.die.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.gso.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.lpg.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.ker.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.oop.all.1"]], 0)) * 0.5 * BN_TO_M_DEFL,
    Elec_Exp_Cons  = replace_na(.data[["mit.expsub.con.ecy.res.1"]], 0) * BN_TO_M_DEFL,
    # Producer-side explicit (M 2017 USD)
    Coal_Exp_Prod  = (replace_na(.data[["mit.expsub.con.coa.ind.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.coa.pow.1"]], 0)) * BN_TO_M_DEFL,
    Gas_Exp_Prod   = (replace_na(.data[["mit.expsub.con.nga.ind.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.nga.pow.1"]], 0)) * BN_TO_M_DEFL,
    Petro_Exp_Prod = (replace_na(.data[["mit.expsub.con.die.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.gso.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.lpg.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.ker.all.1"]], 0) +
                     replace_na(.data[["mit.expsub.con.oop.all.1"]], 0)) * 0.5 * BN_TO_M_DEFL,
    Elec_Exp_Prod  = replace_na(.data[["mit.expsub.con.ecy.ind.1"]], 0) * BN_TO_M_DEFL,
    # Implicit subsidies
    Coal_Imp  = replace_na(.data[["mit.impsub.con.coa.all.1"]], 0) * BN_TO_M_DEFL,
    Gas_Imp   = replace_na(.data[["mit.impsub.con.nga.all.1"]], 0) * BN_TO_M_DEFL,
    Petro_Imp = (replace_na(.data[["mit.impsub.con.die.all.1"]], 0) +
                 replace_na(.data[["mit.impsub.con.gso.all.1"]], 0)) * BN_TO_M_DEFL,
    Elec_Imp  = replace_na(.data[["mit.impsub.con.ecy.res.1"]], 0) * BN_TO_M_DEFL
  ) %>%
  select(countryname, countrycode, year, mit.gdp.pre.lvl.1, GDP,
         Coal_Exp_Cons, Gas_Exp_Cons, Petro_Exp_Cons, Elec_Exp_Cons,
         Coal_Exp_Prod, Gas_Exp_Prod, Petro_Exp_Prod, Elec_Exp_Prod,
         Coal_Imp, Gas_Imp, Petro_Imp, Elec_Imp)

subsidy_cols_sc <- c("Coal_Exp_Cons","Gas_Exp_Cons","Petro_Exp_Cons","Elec_Exp_Cons",
                     "Coal_Exp_Prod","Gas_Exp_Prod","Petro_Exp_Prod","Elec_Exp_Prod",
                     "Coal_Imp","Gas_Imp","Petro_Imp","Elec_Imp")
Subsidy_Clean$GDPshare <- rowSums(Subsidy_Clean[, subsidy_cols_sc], na.rm = TRUE) /
  Subsidy_Clean$GDP / 1000
message("  Subsidy_Clean (IMF25): ", nrow(Subsidy_Clean), " country-year obs")

# Subsidy_Year: single reference year slice
Subsidy_Year_IMF25 <- IMF25_wide %>%
  rename(countrycode = Scenario) %>%
  filter(countrycode %in% Reg_Inclu)

# =============================================================================
# 6. Leontief cost-push helpers (domestic only, identical to v5)
# =============================================================================
leontief_push_dom <- function(rate_mat_NxG, phi = 0.7) {
  result <- numeric(N * G)
  for (r_idx in 1:G) {
    p <- (r_idx - 1) * N + 1;  q <- r_idx * N
    shock_r <- rate_mat_NxG[, r_idx]
    L_block  <- Leontief[p:q, p:q]
    result[p:q] <- phi * as.vector(t(shock_r) %*% L_block)
  }
  result
}
phi_cons <- .7

# =============================================================================
# 7. Scenario calculator (identical to v5)
# =============================================================================
run_scenario_calc <- function(Pr_mat) {
  FD_new <- Minumum + (FinalDemand_ori - Minumum) / Pr_mat
  FD_chg <- rowSums(FD_new) - rowSums(FinalDemand_ori)
  INT <- rbind(rowSums(VA_multiplier_Lab %*% FD_chg),
               rowSums(VA_multiplier_Cap %*% FD_chg),
               rowSums(VA_multiplier_Tax %*% FD_chg))
  VA_chg_r <- VA_mat_detail
  DTnew    <- array(0, dim=c(203, length(Reg_Inclu)))
  CO2_vec  <- numeric(length(Reg_Inclu))
  for (r in 1:length(Reg_Inclu)) {
    y = (Reg_corr$GTAP.NO[r]-1)*N + 1;  z = Reg_corr$GTAP.NO[r]*N
    VA_chg_r[,,r] <- INT[, y:z]
    DTnew[,r] <- DemandTot_ori[,r] +
      rowSums(Transfer_mat[,,r] %*% diag(rowSums(VA_chg_r[,,r])))
    CO2_vec[r] <- sum((Minumum[,r] +
                         INT2[,,r] %*% (DTnew[,r] - Minumum_reg[,r]) / Pr_mat[,r]) *
                        CF_IncluDirect[,r])
  }
  list(VA_Chg=apply(VA_chg_r,3,rowSums), DemandNew=DTnew, CO2=CO2_vec)
}

# =============================================================================
# 8. Scenario 1: Explicit removal
# =============================================================================
int <- pracma::repmat(PriceChg_expl_cons, G, 1)
Pchg1_expl_cons <- int[, match(Reg_Inclu, toupper(regnam))]
Pchg1_expl_prod_dom <- leontief_push_dom(PriceChg_expl_prod)

Price_Response[,,1] <- 1 + phi_cons * Pchg1_expl_cons
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  p <- (tarreg - 1) * N + 1;  q <- tarreg * N
  Price_Response[p:q, r, 1] <- Price_Response[p:q, r, 1] + Pchg1_expl_prod_dom[p:q]
  Sub_Scenarios[tarsec,     r, 1] <- PriceChg_expl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
  Sub_Scenarios[tarsec + N, r, 1] <- PriceChg_expl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}
res1 <- run_scenario_calc(Price_Response[,,1])
VA_Tot_Chg[,,1]      <- res1$VA_Chg
DemandTot_new[,,1]   <- res1$DemandNew
CO2_Reg_Response[,1] <- res1$CO2

# =============================================================================
# 9. Scenario 2: Implicit via production tax
# =============================================================================
Pchg2_impl_prod_dom <- leontief_push_dom(PriceChg_impl_prod)
Price_Response[,,2] <- 1
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  p <- (tarreg - 1) * N + 1;  q <- tarreg * N
  Price_Response[p:q, r, 2] <- 1 + Pchg2_impl_prod_dom[p:q]
  Sub_Scenarios[tarsec + 3*N, r, 2] <- PriceChg_impl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}
res2 <- run_scenario_calc(Price_Response[,,2])
VA_Tot_Chg[,,2]      <- res2$VA_Chg
DemandTot_new[,,2]   <- res2$DemandNew
CO2_Reg_Response[,2] <- res2$CO2

# =============================================================================
# 10. Scenario 3: Implicit via consumer tax
# =============================================================================
int <- pracma::repmat(PriceChg_impl_cons, G, 1)
Pchg3_impl_cons <- int[, match(Reg_Inclu, toupper(regnam))]
Price_Response[,,3] <- phi_cons * Pchg3_impl_cons + 1
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  Sub_Scenarios[tarsec + 2*N, r, 3] <- PriceChg_impl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}
res3 <- run_scenario_calc(Price_Response[,,3])
VA_Tot_Chg[,,3]      <- res3$VA_Chg
DemandTot_new[,,3]   <- res3$DemandNew
CO2_Reg_Response[,3] <- res3$CO2

# =============================================================================
# 11. Scenario 4: No policy
# =============================================================================
Sub_Scenarios[,,4]   <- 0
Sub_revenue[, 4]     <- 0
Price_Response[,,4]  <- 1
DemandTot_new[,,4]   <- DemandTot_ori
VA_Tot_Chg[,,4]      <- 0
CO2_Reg_Response[,4] <- colSums(FinalDemand_ori * CF_IncluDirect)

# =============================================================================
# 12. Save results
# =============================================================================
save(Subsidy_Clean, Sub_Scenarios, Sub_revenue, CO2_Reg_Response, Price_Response,
     DemandTot_new, DemandTot_ori, VA_Tot_Chg,
     Mimunum_sum, Minumum_reg, Minumum, Elasticity, Elasticity_raw,
     PriceChg_expl_cons, PriceChg_expl_prod,
     PriceChg_impl_cons, PriceChg_impl_prod,
     file = str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5_IMF25.Rdata"))

message("Module 3 IMF25 complete. Results saved to: ", pathout2_imf25)

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                   "pathout2_imf25","pathout3_imf25","pathout5_imf25",
                                   "pathdata3","pathdata4","pathcode","pathcode_si2021",
                                   "pathcode_imf25"))])
gc()
