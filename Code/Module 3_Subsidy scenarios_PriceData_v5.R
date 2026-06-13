#Module 3 (Price-Data Version v4): Energy subsidy scenarios
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
#
# REVISION (v3, April 2025):
#  DATA SOURCE: IMF (2023) Fossil Fuel Subsidies Database.
#  We retain the 2023 edition in preference to the 2025 update for the following
#  documented reasons (see IMF_2023_vs_2025_Methodology_Note.md):
#
#  (1) ELECTRICITY SUPPLY COST: The 2025 database replaces the marginal generation
#      cost benchmark with a narrow fossil-fuel-input-cost measure that produces
#      physically impossible negative values (e.g., Finland sc_res = -$0.013/kWh,
#      Denmark sc_res = -$0.026/kWh) and sets EU explicit electricity subsidies to
#      zero during the 2021-2023 energy crisis — directly contradicting >€700 billion
#      in documented government interventions (IEA, 2023).
#
#  (2) COAL PRODUCER SUBSIDIES: The 2025 database reduces German coal producer
#      subsidies by 70-100% for the same historical years (e.g., 2020: $2.80B→$0).
#
#  (3) VAT UNDERPRICING: Both versions report energy VAT rates, but pvat is excluded
#      from our analysis. VAT rate design is general tax policy (applies similarly to
#      food, medicine, etc.) rather than a targeted energy subsidy. Additionally,
#      IMF pvat figures show anomalies inconsistent with observed VAT rate gaps
#      (e.g., Germany 2018 pvat.nga = $1.3B despite vatrate.nga = standard rate).
#      We therefore exclude pvat from both price rates and monetary totals.
#
# Reference year: 2022 (last year with actual, non-projected data in 2023 database).
# Units in 2023 database: subsidies in million 2021 USD; GDP in billion 2021 USD.
# Deflator: 2021→2017 = 0.90787 (GDP deflator, from FRED GDPDEF series).
#
# PRICE RATE IMPROVEMENT (vs old LES-based approach):
#  Old approach: rate ≈ subsidy_amount_M$ / sector_output_M$ (crude approximation)
#  V3 approach: rate = max(0, supply_cost / retail_price - 1)  (direct price gap)
#  This correctly measures the proportional price increase when the subsidy is removed.
#
# KEY METHODOLOGICAL FRAMEWORK (unchanged from v2):
#   CONSUMER CHANNEL: residential price rates → direct final demand multiplier
#   PRODUCER CHANNEL: industry+power rates (consumption-weighted) → Leontief cost-push
#
#   s15 coa: cons=coa.res,             prod=wt(coa.ind, coa.pow)
#   s17 gas: cons=0,                   prod=wt(nga.ind, nga.pow)
#   s32 p_c: cons=wt(gso,die,lpg,ker), prod=same
#   s46 ely: cons=ecy.res,             prod=ecy.ind
#   s47 gdt: cons=nga.res,             prod=0
#
# Formulas:
#   Explicit: pchg = max(0, sup_cost / rp - 1)
#   Implicit: pchg = max(0, eff_price / max(rp, sup_cost) - 1)
#
# SCENARIOS (v5 — domestic-only Leontief for producer channels):
#  S1: Explicit removal  → consumer channel (res, direct) + producer channel (ind+pow, DOMESTIC Leontief)
#  S2: Implicit via production tax → producer channel only (DOMESTIC Leontief)
#  S3: Implicit via consumer tax  → consumer channel only (direct, unchanged)
#  S4: No policy (reference)
#
# KEY CHANGE vs v4 (domestic-only price propagation for producer channel):
#  v4 used the full global MRIO Leontief → cross-border price spillovers included.
#  v5 uses only the diagonal block Leontief[p:q, p:q] for each country r → small-country
#  assumption: removing Country r's production subsidy raises Country r's domestic sector
#  prices only; world market prices (and thus import prices for all countries) are unchanged.
#  This is consistent with IMF Implicit_prod data which targets DOMESTIC industrial/power users.


# =============================================================================
# 0. Load base data
# =============================================================================
load(str_c(pathout, "/GTAP_11b_2017_Elements for MRIO.Rdata"))
load(str_c(pathout, "/Expenditure,Footprint,Pop,Emission.Rdata"))
regnam <- toupper(regnam)


# =============================================================================
# 1. Expenditure elasticity (unchanged)
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
write.csv(Elasticity_raw, file = str_c(pathout2, "/Elasticity_raw.csv"))
Elasticity <- Elasticity_raw
Elasticity[Elasticity <= 0] <- 0.1


# =============================================================================
# 2. LES summary statistics (unchanged)
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
# 3. Read IMF 2023 price data & compute price change rates
# =============================================================================
# Source: IMF (2023) Fossil Fuel Subsidies database
#   File:   EXTERNALfuelsubsidiestemplate2023new.xlsx, sheet "data"
#   Format: panel (one row per country × year), variables as columns
#   scenario = "U1" (baseline/current policy)
#   year     = 2022 (reference year; last actual, non-projected data year)
#
# Units: retail/supply prices in USD/GJ; consumption in EJ; monetary subsidies in M USD (2021).
# DEFL_2021_to_2017 = 0.90787 (GDP deflator: GDPDEF Q4-2021 / Q4-2017 ≈ 110.14/99.995^-1)
#
# WHY 2023 DATA — see header and IMF_2023_vs_2025_Methodology_Note.md for full justification.

path_xls_2023 <- "C:/Users/xiang/OneDrive/Energy subsidy and distributional issue/others/EXTERNALfuelsubsidiestemplate2023new.xlsx"
# Default: 2022 = last year with actual (non-projected) data in IMF 2023 database.
# Override: set YEAR_USE_2023_OVERRIDE before source()-ing this file (e.g. SI2021 run uses 2021).
YEAR_USE_2023 <- if (exists("YEAR_USE_2023_OVERRIDE")) {
  message(sprintf("  [OVERRIDE] YEAR_USE_2023 set to %d (via YEAR_USE_2023_OVERRIDE)", YEAR_USE_2023_OVERRIDE))
  YEAR_USE_2023_OVERRIDE
} else {
  2022L   # default: main run
}
DEFL_2021_to_2017 <- 0.90787   # GDP deflator: 2021 prices → 2017 prices
# Note: IMF 2023 database reports ALL years in 2021 constant prices, so this
# deflator is correct regardless of which year row is selected by YEAR_USE_2023.

message("Reading IMF 2023 data (sheet='data')...")
library(readxl)
IMF_2023 <- read_excel(path_xls_2023, sheet = "data")
IMF_2023_U1 <- IMF_2023 %>% filter(scenario == "U1")
message("  Countries in 2023 database (U1): ", n_distinct(IMF_2023_U1$countrycode))
message("  Years available: ", paste(sort(unique(IMF_2023_U1$year)), collapse = ", "))

# ---- 3a. Filter reference year for price_wide ----------------------------------
# price_ref: one row per country for YEAR_USE_2023
price_ref <- IMF_2023_U1 %>%
  filter(year == YEAR_USE_2023) %>%
  select(countryname, countrycode, everything(), -c(scenario, year, region, incomelevel))

message("  Countries with ", YEAR_USE_2023, " data: ", nrow(price_ref))

# Join with Reg_corr to get one row per GTAP included country
# Reg_corr$WBGDPcode = ISO3 code that matches IMF countrycode
price_wide <- Reg_corr %>%
  left_join(price_ref, by = c("WBGDPcode" = "countrycode"))


# ---- 3b. Helper functions  --------------------------------------------------
# safe_expl: explicit rate = max(0, sup_cost / rp - 1)
safe_expl <- function(sup_c, rp_c, d = price_wide) {
  sup <- suppressWarnings(as.numeric(d[[sup_c]]))
  rp  <- suppressWarnings(as.numeric(d[[rp_c]]))
  rp[is.na(rp) | rp < 1e-9] <- NA
  pmax(0, sup/rp - 1, na.rm = FALSE) %>% tidyr::replace_na(0)
}

# safe_impl: implicit rate = max(0, eff_price / max(rp, sup_cost) - 1)
safe_impl <- function(eff_c, rp_c, sup_c, d = price_wide) {
  eff   <- suppressWarnings(as.numeric(d[[eff_c]]))
  rp    <- suppressWarnings(as.numeric(d[[rp_c]]))
  sup   <- suppressWarnings(as.numeric(d[[sup_c]]))
  floor <- pmax(rp, sup, na.rm = TRUE)
  floor[is.na(floor) | floor < 1e-9] <- NA
  pmax(0, eff/floor - 1, na.rm = FALSE) %>% tidyr::replace_na(0)
}

# wt_avg: consumption-volume-weighted average of price change rates
# Fallback to simple mean when no consumption data available.
wt_avg <- function(rate_cols, wt_cols, d = price_wide) {
  rates <- as.matrix(d[, rate_cols, drop = FALSE])
  wts   <- as.matrix(d[, wt_cols,   drop = FALSE])
  rates[is.na(rates)] <- 0
  wts[is.na(wts)]     <- 0
  tot_wt <- rowSums(wts)
  ifelse(tot_wt > 1e-12,
         rowSums(rates * wts) / tot_wt,
         rowMeans(rates, na.rm = TRUE))
}




# ---- 3d. Per-fuel explicit price change rates --------------------------------
# COAL (ind, res, pow use types available in 2023 data)
price_wide$expl_coa_ind <- safe_expl("mit.sup.cost.coa.ind.1", "mit.rp.coa.ind.1")
price_wide$expl_coa_res <- safe_expl("mit.sup.cost.coa.res.1", "mit.rp.coa.res.1")
price_wide$expl_coa_pow <- safe_expl("mit.sup.cost.coa.pow.1", "mit.rp.coa.pow.1")

# NATURAL GAS (ind, res, pow use types available in 2023 data)
price_wide$expl_nga_ind <- safe_expl("mit.sup.cost.nga.ind.1", "mit.rp.nga.ind.1")
price_wide$expl_nga_res <- safe_expl("mit.sup.cost.nga.res.1", "mit.rp.nga.res.1")
price_wide$expl_nga_pow <- safe_expl("mit.sup.cost.nga.pow.1", "mit.rp.nga.pow.1")

# LIQUID FUELS (no use-type split in 2023 data; all = all uses combined)
price_wide$expl_gso <- safe_expl("mit.sup.cost.gso.all.1", "mit.rp.gso.all.1")
price_wide$expl_die <- safe_expl("mit.sup.cost.die.all.1", "mit.rp.die.all.1")
price_wide$expl_lpg <- safe_expl("mit.sup.cost.lpg.all.1", "mit.rp.lpg.all.1")
price_wide$expl_ker <- safe_expl("mit.sup.cost.ker.all.1", "mit.rp.ker.all.1")
price_wide$expl_oop <- safe_expl("mit.sup.cost.oop.all.1", "mit.rp.oop.all.1")

# ELECTRICITY (ind, res use types in 2023 data; no pow for electricity)
# KEY: 2023 electricity supply costs use MARGINAL GENERATION COST benchmark
# → correctly captures EU electricity subsidies during 2019-2022 energy market conditions
price_wide$expl_ecy_ind <- safe_expl("mit.sup.cost.ecy.ind.1", "mit.rp.ecy.ind.1")
price_wide$expl_ecy_res <- safe_expl("mit.sup.cost.ecy.res.1", "mit.rp.ecy.res.1")


# ---- 3e. Per-fuel implicit price change rates --------------------------------
price_wide$impl_coa_ind <- safe_impl("mit.eff.price.coa.ind.1", "mit.rp.coa.ind.1", "mit.sup.cost.coa.ind.1")
price_wide$impl_coa_res <- safe_impl("mit.eff.price.coa.res.1", "mit.rp.coa.res.1", "mit.sup.cost.coa.res.1")
price_wide$impl_coa_pow <- safe_impl("mit.eff.price.coa.pow.1", "mit.rp.coa.pow.1", "mit.sup.cost.coa.pow.1")

price_wide$impl_nga_ind <- safe_impl("mit.eff.price.nga.ind.1", "mit.rp.nga.ind.1", "mit.sup.cost.nga.ind.1")
price_wide$impl_nga_res <- safe_impl("mit.eff.price.nga.res.1", "mit.rp.nga.res.1", "mit.sup.cost.nga.res.1")
price_wide$impl_nga_pow <- safe_impl("mit.eff.price.nga.pow.1", "mit.rp.nga.pow.1", "mit.sup.cost.nga.pow.1")

price_wide$impl_gso <- safe_impl("mit.eff.price.gso.all.1", "mit.rp.gso.all.1", "mit.sup.cost.gso.all.1")
price_wide$impl_die <- safe_impl("mit.eff.price.die.all.1", "mit.rp.die.all.1", "mit.sup.cost.die.all.1")
price_wide$impl_lpg <- safe_impl("mit.eff.price.lpg.all.1", "mit.rp.lpg.all.1", "mit.sup.cost.lpg.all.1")
price_wide$impl_ker <- safe_impl("mit.eff.price.ker.all.1", "mit.rp.ker.all.1", "mit.sup.cost.ker.all.1")

price_wide$impl_ecy_ind <- safe_impl("mit.eff.price.ecy.ind.1", "mit.rp.ecy.ind.1", "mit.sup.cost.ecy.ind.1")
price_wide$impl_ecy_res <- safe_impl("mit.eff.price.ecy.res.1", "mit.rp.ecy.res.1", "mit.sup.cost.ecy.res.1")


# ---- 3f. Aggregate to GTAP sector × transmission channel --------------------
#
# COAL CONSUMPTION WEIGHTS: mit.ener.gj.{use}.coa.e.1 (EJ, by sector)
# GAS CONSUMPTION WEIGHTS:  mit.ener.gj.{use}.nga.e.1 (EJ, by sector)
# PETROLEUM WEIGHTS: mit.ener.ltr.tot.{product}.e.1 (litres, total by product)
#   Only total-level consumption available; no ind/res/pow split for petroleum.
#   Using physical consumption volumes directly — independent of subsidy magnitude,
#   avoiding the circular logic of using subsidy amounts as their own proxy.
# ELECTRICITY: only ind and res rates; no gap to average (each rate used directly).

# Consumer channel [RESIDENTIAL]
price_wide$expl_cons_s15 <- price_wide$expl_coa_res
price_wide$expl_cons_s17 <- 0                        # producer channel only for s17
price_wide$expl_cons_s32 <- wt_avg(
  c("expl_gso","expl_die","expl_lpg","expl_ker"),
  c("mit.ener.ltr.tot.gso.e.1","mit.ener.ltr.tot.die.e.1",
    "mit.ener.ltr.tot.lpg.e.1","mit.ener.ltr.tot.ker.e.1"))
price_wide$expl_cons_s46 <- price_wide$expl_ecy_res
price_wide$expl_cons_s47 <- price_wide$expl_nga_res  # consumer channel for gas dist.

# Producer channel [INDUSTRY + POWER]
# Coal weights: consumption-weighted avg(ind, pow)
price_wide$expl_prod_s15 <- wt_avg(
  c("expl_coa_ind","expl_coa_pow"),
  c("mit.ener.gj.ind.coa.e.1","mit.ener.gj.pow.coa.e.1"))
# Gas weights: consumption-weighted avg(ind, pow)
price_wide$expl_prod_s17 <- wt_avg(
  c("expl_nga_ind","expl_nga_pow"),
  c("mit.ener.gj.ind.nga.e.1","mit.ener.gj.pow.nga.e.1"))
# Petroleum: same as consumer (no use-type split)
price_wide$expl_prod_s32 <- wt_avg(
  c("expl_gso","expl_die","expl_lpg","expl_ker"),
  c("mit.ener.ltr.tot.gso.e.1","mit.ener.ltr.tot.die.e.1",
    "mit.ener.ltr.tot.lpg.e.1","mit.ener.ltr.tot.ker.e.1"))
# Electricity: industrial rate only
price_wide$expl_prod_s46 <- price_wide$expl_ecy_ind
price_wide$expl_prod_s47 <- 0                        # consumer channel for gas dist.

# Implicit consumer channel [RESIDENTIAL]
price_wide$impl_cons_s15 <- price_wide$impl_coa_res
price_wide$impl_cons_s17 <- 0
price_wide$impl_cons_s32 <- wt_avg(
  c("impl_gso","impl_die","impl_lpg","impl_ker"),
  c("mit.ener.ltr.tot.gso.e.1","mit.ener.ltr.tot.die.e.1",
    "mit.ener.ltr.tot.lpg.e.1","mit.ener.ltr.tot.ker.e.1"))
price_wide$impl_cons_s46 <- price_wide$impl_ecy_res
price_wide$impl_cons_s47 <- price_wide$impl_nga_res

# Implicit producer channel [INDUSTRY + POWER]
price_wide$impl_prod_s15 <- wt_avg(
  c("impl_coa_ind","impl_coa_pow"),
  c("mit.ener.gj.ind.coa.e.1","mit.ener.gj.pow.coa.e.1"))
price_wide$impl_prod_s17 <- wt_avg(
  c("impl_nga_ind","impl_nga_pow"),
  c("mit.ener.gj.ind.nga.e.1","mit.ener.gj.pow.nga.e.1"))
price_wide$impl_prod_s32 <- wt_avg(
  c("impl_gso","impl_die","impl_lpg","impl_ker"),
  c("mit.ener.ltr.tot.gso.e.1","mit.ener.ltr.tot.die.e.1",
    "mit.ener.ltr.tot.lpg.e.1","mit.ener.ltr.tot.ker.e.1"))
price_wide$impl_prod_s46 <- price_wide$impl_ecy_ind
price_wide$impl_prod_s47 <- 0


# NOTE: VAT underpricing (pvat) is excluded from price change rates.
# VAT rate design is general tax policy rather than a targeted energy subsidy,
# and IMF pvat figures show anomalies (e.g. Germany 2018 pvat.nga = $1.3B despite
# vatrate.nga = standard 19%). Explicit rates = price-gap (sc/rp-1) only.


# ---- 3h. Map to GTAP [N × G] matrices  -----------------------------------
tarsec <- c(15, 17, 32, 46, 47)

cons_cols_expl <- c("expl_cons_s15","expl_cons_s17","expl_cons_s32","expl_cons_s46","expl_cons_s47")
prod_cols_expl <- c("expl_prod_s15","expl_prod_s17","expl_prod_s32","expl_prod_s46","expl_prod_s47")
cons_cols_impl <- c("impl_cons_s15","impl_cons_s17","impl_cons_s32","impl_cons_s46","impl_cons_s47")
prod_cols_impl <- c("impl_prod_s15","impl_prod_s17","impl_prod_s32","impl_prod_s46","impl_prod_s47")

PriceChg_expl_cons <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_expl_prod <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_impl_cons <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))
PriceChg_impl_prod <- array(0, dim=c(N,G), dimnames=list(secnam,regnam))

n_matched <- 0
for (r in 1:length(Reg_Inclu)) {
  iso    <- toupper(Reg_corr$WBGDPcode[r])
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  idx    <- which(toupper(price_wide$WBGDPcode) == iso)
  if (length(idx) >= 1) {
    pw <- price_wide[idx[1], ]
    n_matched <- n_matched + 1
    for (k in 1:length(tarsec)) {
      e_c <- suppressWarnings(as.numeric(pw[[cons_cols_expl[k]]]))
      e_p <- suppressWarnings(as.numeric(pw[[prod_cols_expl[k]]]))
      i_c <- suppressWarnings(as.numeric(pw[[cons_cols_impl[k]]]))
      i_p <- suppressWarnings(as.numeric(pw[[prod_cols_impl[k]]]))
      if (!is.na(e_c) && is.finite(e_c)) PriceChg_expl_cons[tarsec[k], tarreg] <- e_c
      if (!is.na(e_p) && is.finite(e_p)) PriceChg_expl_prod[tarsec[k], tarreg] <- e_p
      if (!is.na(i_c) && is.finite(i_c)) PriceChg_impl_cons[tarsec[k], tarreg] <- i_c
      if (!is.na(i_p) && is.finite(i_p)) PriceChg_impl_prod[tarsec[k], tarreg] <- i_p
    }

    # ---- Add tpro (direct producer transfer) rates to PriceChg_expl_prod ---------
    # tpro is captured in Subsidy_Clean monetary amounts (Coal/Gas/Petro/Elec_Exp_Prod)
    # but NOT in safe_expl() which only measures retail price vs supply cost gaps.
    # We add tpro_rate_s = tpro_monetary_s / demand_s on top of the price-gap rate,
    # for ALL countries where tpro > 0 (not just USA).
    # Constraint:  tpro_rate_s × demand_s = tpro_monetary_s  (satisfied by construction)
    # After this, raw_expl_r = texp_monetary_equivalent + tpro_monetary,
    # and alpha = IMF_expl_r(=texp+tpro) / raw_expl_r → ≈ 1 for tpro-only countries.
    tpro_coa <- replace_na(suppressWarnings(as.numeric(pw[["mit.sub.tpro.tot.coa.1"]])), 0) * DEFL_2021_to_2017
    tpro_nga <- replace_na(suppressWarnings(as.numeric(pw[["mit.sub.tpro.tot.nga.1"]])), 0) * DEFL_2021_to_2017
    tpro_oil <- replace_na(suppressWarnings(as.numeric(pw[["mit.sub.tpro.tot.oil.1"]])), 0) * DEFL_2021_to_2017
    tpro_ecy <- replace_na(suppressWarnings(as.numeric(pw[["mit.sub.tpro.tot.ecy.1"]])), 0) * DEFL_2021_to_2017
    tpro_sec <- c(tpro_coa, tpro_nga, tpro_oil, tpro_ecy, tpro_nga)  # s15,s17,s32,s46,s47
    for (k in 1:length(tarsec)) {
      s <- tarsec[k]
      if (tpro_sec[k] > 0 && Demand_reg[s, tarreg] > 1e-6)
        PriceChg_expl_prod[s, tarreg] <- PriceChg_expl_prod[s, tarreg] +
                                         tpro_sec[k] / Demand_reg[s, tarreg]
    }
    # -------------------------------------------------------------------------------
  }
}
message(n_matched, " of ", length(Reg_Inclu), " countries matched to IMF 2022 price data.")
unmatched_iso <- Reg_corr$WBGDPcode[!toupper(Reg_corr$WBGDPcode) %in% toupper(price_wide$WBGDPcode)]
unmatched_reg <- Reg_Inclu[!toupper(Reg_corr$WBGDPcode) %in% toupper(price_wide$WBGDPcode[!is.na(price_wide$countryname)])]
if (length(unmatched_reg) > 0)
  message("  No IMF data (rates set to 0): ",
          paste(str_c(unmatched_reg, " (", unmatched_iso, ")"), collapse = ", "))
# =============================================================================


# =============================================================================
# 4. Build Subsidy_Clean time series from IMF 2023 monetary amounts
# =============================================================================
# Data: IMF (2023) panel, years 2015-2030
#   2015-2022: ACTUAL data (used for main analysis)
#   2023-2030: IMF projections (included for Module 7 visualization only;
#              dashed vertical line at 2022 in charts separates actual from projected)
# Units: subsidy amounts in MILLION 2021 USD; GDP in BILLION 2021 USD
# Deflator: × DEFL_2021_to_2017 → 2017 constant prices
#
# Column structure (identical to downstream Module 7 expectations):
#   col 1: countryname  col 2: countrycode (ISO3)  col 3: year
#   col 4: mit.gdp.pre.lvl.1 (raw)  col 5: GDP (billion 2017 USD)
#   col 6-9:  Coal_Exp_Cons, Gas_Exp_Cons, Petro_Exp_Cons, Elec_Exp_Cons  (M 2017 USD)
#   col 10-13: Coal_Exp_Prod, Gas_Exp_Prod, Petro_Exp_Prod, Elec_Exp_Prod (M 2017 USD)
#   col 14-17: Petro_Imp, Gas_Imp, Coal_Imp, Elec_Imp                     (M 2017 USD)
#
# GDPshare note: rowSums(subsidy_cols) / GDP / 1000 = M$ / (Bn$ × 1000) = fraction ✓
# Years > 2022 are IMF projections; downstream code uses year > 2022 to identify them.
#
# CONSUMER vs PRODUCER split (consistent with price channel logic above):
#   _Cons: residential use (texp.res.*) → direct household impact channel
#   _Prod: industry + power use (texp.ind.* + texp.pow.*) + direct producer transfers
#          (tpro.*) → Leontief propagation channel
#
# NOTE: pvat (VAT underpricing) is NOT included — excluded due to conceptual ambiguity
# (VAT is general tax policy) and anomalous IMF pvat figures for some countries.
# Explicit subsidies = price-gap subsidies (texp) + direct transfers (tpro) only.

Subsidy_Clean <- IMF_2023_U1 %>%
  filter(year %in% 2015:2030,
         # Match by ISO3 countrycode (robust; avoids name-spelling mismatches)
         # This ensures countries like "Russian Federation" are not silently dropped
         # when Reg_corr$IMF_reg uses a different spelling.
         countrycode %in% Reg_corr$WBGDPcode) %>%
  mutate(
    GDP = mit.gdp.pre.lvl.1 * DEFL_2021_to_2017,  # billion 2017 USD

    # --- EXPLICIT CONSUMER SIDE (residential) ---
    # Price-gap subsidy received by residential users (direct final demand channel)
    Coal_Exp_Cons  = replace_na(mit.sub.texp.res.coa.1, 0) * DEFL_2021_to_2017,
    Gas_Exp_Cons   = replace_na(mit.sub.texp.res.nga.1, 0) * DEFL_2021_to_2017,
    Elec_Exp_Cons  = replace_na(mit.sub.texp.res.ecy.1, 0) * DEFL_2021_to_2017,
    # Petroleum consumer = total petroleum explicit minus producer portion
    Petro_Exp_Cons = pmax(0,
      (replace_na(mit.sub.texp.tot.gso.1, 0) + replace_na(mit.sub.texp.tot.die.1, 0) +
       replace_na(mit.sub.texp.tot.lpg.1, 0) + replace_na(mit.sub.texp.tot.ker.1, 0) +
       replace_na(mit.sub.texp.tot.oop.1, 0)) -
       replace_na(mit.sub.tpro.tot.oil.1, 0)) * DEFL_2021_to_2017,

    # --- EXPLICIT PRODUCER SIDE ---
    # = industrial/power use-type price-gap subsidies (texp.ind + texp.pow)
    # + direct government transfers to producers (tpro)
    # pvat (VAT underpricing) is EXCLUDED — see header note.
    Coal_Exp_Prod  = (
      replace_na(mit.sub.texp.ind.coa.1, 0) +
      replace_na(mit.sub.texp.pow.coa.1, 0) +
      replace_na(mit.sub.tpro.tot.coa.1, 0)   # coal producer subsidies (mining support)
    ) * DEFL_2021_to_2017,

    Gas_Exp_Prod   = (
      replace_na(mit.sub.texp.ind.nga.1, 0) +
      replace_na(mit.sub.texp.pow.nga.1, 0) +
      replace_na(mit.sub.tpro.tot.nga.1, 0)   # gas producer subsidies (royalty discounts etc.)
    ) * DEFL_2021_to_2017,

    Petro_Exp_Prod = (
      replace_na(mit.sub.tpro.tot.oil.1, 0)   # petroleum producer subsidies
    ) * DEFL_2021_to_2017,

    Elec_Exp_Prod  = (
      replace_na(mit.sub.texp.ind.ecy.1, 0) +   # industrial electricity price-gap
      replace_na(mit.sub.tpro.tot.ecy.1, 0)     # electricity producer subsidies
    ) * DEFL_2021_to_2017,

    # --- IMPLICIT SUBSIDIES (externality under-pricing, unchanged definition) ---
    Petro_Imp = (
      replace_na(mit.sub.timp.tot.gso.1, 0) + replace_na(mit.sub.timp.tot.die.1, 0) +
      replace_na(mit.sub.timp.tot.lpg.1, 0) + replace_na(mit.sub.timp.tot.ker.1, 0) +
      replace_na(mit.sub.timp.tot.oop.1, 0)
    ) * DEFL_2021_to_2017,

    Gas_Imp  = replace_na(mit.sub.timp.tot.nga.1, 0) * DEFL_2021_to_2017,
    Coal_Imp = replace_na(mit.sub.timp.tot.coa.1, 0) * DEFL_2021_to_2017,
    Elec_Imp = replace_na(mit.sub.timp.tot.ecy.1, 0) * DEFL_2021_to_2017
  ) %>%
  select(countryname, countrycode, year, mit.gdp.pre.lvl.1, GDP,
         Coal_Exp_Cons, Gas_Exp_Cons, Petro_Exp_Cons, Elec_Exp_Cons,
         Coal_Exp_Prod, Gas_Exp_Prod, Petro_Exp_Prod, Elec_Exp_Prod,
         Petro_Imp, Gas_Imp, Coal_Imp, Elec_Imp)
         # NOTE: is_projected NOT included here — Module 7 uses (year > 2022) directly
         # to distinguish actual (<=2022) from projected (>2022) in its era variable.

# GDPshare: M$ subsidy / (Bn$ GDP × 1000) = M$/M$ = dimensionless share
# GDPshare: use named subsidy columns to avoid position-dependent bugs
subsidy_cols <- c("Coal_Exp_Cons","Gas_Exp_Cons","Petro_Exp_Cons","Elec_Exp_Cons",
                  "Coal_Exp_Prod","Gas_Exp_Prod","Petro_Exp_Prod","Elec_Exp_Prod",
                  "Petro_Imp","Gas_Imp","Coal_Imp","Elec_Imp")
Subsidy_Clean$GDPshare <- rowSums(Subsidy_Clean[, subsidy_cols], na.rm = TRUE) /
  Subsidy_Clean$GDP / 1000

message("  Subsidy_Clean: ", nrow(Subsidy_Clean), " country-year obs (2015-2030; actual≤2022, projected>2022)")
message("  Subsidy_Clean countries: ", n_distinct(Subsidy_Clean$countrycode))

# Reference year slice for scenario calculations (same year as price rates)
Subsidy_Year <- Subsidy_Clean %>%
  filter(year == YEAR_USE_2023,
         countrycode %in% Reg_Inclu)
# =============================================================================


# =============================================================================
# 5. Output arrays (identical structure to v2)
# =============================================================================
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


# =============================================================================
# 6. Leontief cost-push helpers
# =============================================================================

# leontief_push_dom: DOMESTIC-ONLY cost-push propagation (v5)
# For each GTAP region r, extracts the diagonal block Leontief[p:q, p:q] and
# applies it to the direct cost shock for that country's sectors only.
# Off-diagonal positions (foreign sectors) remain 0 → small-country assumption:
# no cross-border price propagation from domestic production subsidy removal.
leontief_push_dom <- function(rate_mat_NxG, phi = 0.7) {
  result <- numeric(N * G)                         # initialise: all zeros (no foreign effect)
  for (r in 1:G) {
    p <- (r - 1) * N + 1;  q <- r * N
    shock_r <- rate_mat_NxG[, r]                   # [N] direct price shock for GTAP region r
    L_block <- Leontief[p:q, p:q]                  # [N x N] domestic diagonal block of global L
    result[p:q] <- phi * as.vector(t(shock_r) %*% L_block)  # domestic propagation only
  }
  result   # [N*G] vector; foreign positions are 0
}

# leontief_push kept for reference only (no longer called in v5)
# leontief_push <- function(rate_mat_NxG, phi = 0.7) {
#   rate_GN <- as.vector(rate_mat_NxG)
#   phi * as.vector(rate_GN %*% Leontief)
# }

# Pass-through rate for the consumer channel.
# Subsidy benefits reach households via retailers/distributors; intermediaries
# absorb approx. 30% of the fiscal transfer, so effective price transmission = 70%.
phi_cons <- .7

# =============================================================================
# 6b. IMF monetary calibration of price change rates (national-level alpha)
# =============================================================================
# RATIONALE: GTAP sectors are broader than the specific energy products IMF covers.
# (e.g. s17 "Gas manufacture & distribution" includes pipeline services, admin, etc.)
# Applying the raw IMF price-gap rate directly to GTAP sector demand therefore
# overestimates total subsidy amounts.
#
# SOLUTION: compute a national-level calibration factor alpha_r:
#   alpha_r = IMF_reported_amount / (rate_raw × GTAP_demand_sum)
# This preserves the relative rate structure across sectors while anchoring the
# aggregate subsidy amount to the IMF fiscal total.
#
# Sub_revenue = FULL IMF monetary amounts (government fiscal savings, no phi).
# phi_cons enters ONLY in Price_Response (single application, no double-counting).
#
message("Calibrating price rates to IMF monetary amounts (national alpha per country)...")

calib_log <- matrix(NA_real_, nrow = length(Reg_Inclu), ncol = 4,
                    dimnames = list(Reg_Inclu, c("IMF_expl","raw_expl","alpha_expl","alpha_impl")))

for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))

  sy_r <- Subsidy_Year[Subsidy_Year$countrycode == Reg_Inclu[r], ]
  if (nrow(sy_r) == 0) next   # no IMF data: leave rates & Sub_revenue at 0

  IMF_expl_r <- sum(c(sy_r$Coal_Exp_Cons, sy_r$Gas_Exp_Cons,
                       sy_r$Petro_Exp_Cons, sy_r$Elec_Exp_Cons,
                       sy_r$Coal_Exp_Prod,  sy_r$Gas_Exp_Prod,
                       sy_r$Petro_Exp_Prod, sy_r$Elec_Exp_Prod), na.rm = TRUE)
  IMF_impl_r <- sum(c(sy_r$Petro_Imp, sy_r$Gas_Imp,
                       sy_r$Coal_Imp,  sy_r$Elec_Imp), na.rm = TRUE)

  # GAS sector bridging: s17 (gas extraction) and s47 (gas distribution) together
  # -------------------------------------------------------------------------
  # IMF codes gas consumption subsidies as "nga.res" → mapped to s47 (residential gas dist.)
  # IMF codes gas producer subsidies as "nga.ind/pow" → mapped to s17 (gas extraction).
  # But both subsidies span the entire gas supply chain (extraction → distribution).
  # Treating only s47 for consumer and only s17 for producer causes alpha to blow up when
  # GTAP's s47 demand is tiny relative to the reported IMF subsidy amount.
  #
  # FIX: before computing alpha, copy gas rates across both sectors so that
  #   - Consumer rate:  s17 ← s47  (both sectors share the residential gas rate)
  #   - Producer rate:  s47 ← s17  (both sectors share the industrial/power gas rate)
  # raw_expl_r now includes (s17+s47) demand in the denominator → alpha stays reasonable.
  # After applying alpha, both s17 and s47 carry the calibrated gas price shock.
  PriceChg_expl_cons[17, tarreg] <- PriceChg_expl_cons[47, tarreg]  # bridge cons: s47→s17
  PriceChg_expl_prod[47, tarreg] <- PriceChg_expl_prod[17, tarreg]  # bridge prod: s17→s47
  PriceChg_impl_cons[17, tarreg] <- PriceChg_impl_cons[47, tarreg]  # implicit cons bridge
  PriceChg_impl_prod[47, tarreg] <- PriceChg_impl_prod[17, tarreg]  # implicit prod bridge

  # Raw model amounts: pre-calibration rate × GTAP demand, summed over energy sectors
  raw_expl_r <- sum(PriceChg_expl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg] +
                    PriceChg_expl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg], na.rm = TRUE)
  raw_impl_r <- sum(PriceChg_impl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg] +
                    PriceChg_impl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg], na.rm = TRUE)

  # National-level scale factors (fully data-driven, no cap)
  # NOTE: raw_expl_r now includes tpro rates (added in the price mapping loop above),
  # so alpha ≈ 1 for tpro-only countries (USA etc.) and adjusts texp measurement
  # error for other countries. The constraint rate×demand = IMF_amount holds after
  # applying alpha to the combined (texp+tpro) rate.
  alpha_expl_r <- if (raw_expl_r > 1e-6 && IMF_expl_r > 0) IMF_expl_r / raw_expl_r else
                  if (raw_expl_r > 1e-6 && IMF_expl_r == 0) 0 else 1
  alpha_impl_r <- if (raw_impl_r > 1e-6 && IMF_impl_r > 0) IMF_impl_r / raw_impl_r else
                  if (raw_impl_r > 1e-6 && IMF_impl_r == 0) 0 else 1

  # Apply uniformly to all energy-sector rates for this country
  PriceChg_expl_cons[tarsec, tarreg] <- PriceChg_expl_cons[tarsec, tarreg] * alpha_expl_r
  PriceChg_expl_prod[tarsec, tarreg] <- PriceChg_expl_prod[tarsec, tarreg] * alpha_expl_r
  PriceChg_impl_cons[tarsec, tarreg] <- PriceChg_impl_cons[tarsec, tarreg] * alpha_impl_r
  PriceChg_impl_prod[tarsec, tarreg] <- PriceChg_impl_prod[tarsec, tarreg] * alpha_impl_r

  # Sub_revenue: FULL IMF amounts (government fiscal savings, no phi)
  # S4 = Null (reference), no revenue to record
  Sub_revenue[r, 1] <- IMF_expl_r
  Sub_revenue[r, 2] <- IMF_impl_r
  Sub_revenue[r, 3] <- IMF_impl_r

  calib_log[r, ] <- c(IMF_expl_r, raw_expl_r, alpha_expl_r, alpha_impl_r)
}

message(sprintf("  alpha_expl: min=%.2f  median=%.2f  max=%.2f  (uncapped)",
  min(calib_log[, 3], na.rm = TRUE), median(calib_log[, 3], na.rm = TRUE),
  max(calib_log[, 3], na.rm = TRUE)))
message(sprintf("  Sub_revenue[Explicit] total: %.1f B 2017 USD",
  sum(calib_log[, 1], na.rm = TRUE) / 1000))

# =============================================================================
# 7. Shared scenario calculator
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
# 8. Scenario 1: Remove EXPLICIT subsidies (S1)
# =============================================================================
# Consumer channel: direct residential price effect (unchanged from v4)
int <- pracma::repmat(PriceChg_expl_cons, G, 1)
Pchg1_expl_cons <- int[, match(Reg_Inclu, toupper(regnam))]

# Producer channel: domestic-only Leontief (v5 change)
# Each country's industrial/power energy cost shock propagates only through
# that country's own supply chain. Foreign sector prices are unaffected.
Pchg1_expl_prod_dom <- leontief_push_dom(PriceChg_expl_prod)  # [N*G], foreign positions = 0

# Build Price_Response column-by-column (per Reg_Inclu country)
# - Consumer channel applies to ALL N*G rows (domestic energy price replicated across GN)
# - Producer channel applies ONLY to country r's own rows [p:q]
Price_Response[,,1] <- 1 + phi_cons * Pchg1_expl_cons   # consumer channel baseline
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  p <- (tarreg - 1) * N + 1;  q <- tarreg * N
  # Add domestic producer channel only to this country's own sector rows
  Price_Response[p:q, r, 1] <- Price_Response[p:q, r, 1] + Pchg1_expl_prod_dom[p:q]
  Sub_Scenarios[tarsec,     r, 1] <- PriceChg_expl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
  Sub_Scenarios[tarsec + N, r, 1] <- PriceChg_expl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}

res1 <- run_scenario_calc(Price_Response[,,1])
VA_Tot_Chg[,,1]      <- res1$VA_Chg
DemandTot_new[,,1]   <- res1$DemandNew
CO2_Reg_Response[,1] <- res1$CO2


# =============================================================================
# 9. Scenario 2: Remove IMPLICIT subsidies via PRODUCTION TAX (S2)
# =============================================================================
# Domestic-only Leontief (v5 key change):
# IMF Implicit_prod covers industrial/power energy inputs — domestic producers only.
# Under small-country assumption, world energy prices are unchanged.
# → Only country r's own sector prices rise; all foreign sector prices remain 1.
Pchg2_impl_prod_dom <- leontief_push_dom(PriceChg_impl_prod)  # [N*G], foreign = 0

Price_Response[,,2] <- 1   # initialise: no change anywhere
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  p <- (tarreg - 1) * N + 1;  q <- tarreg * N
  Price_Response[p:q, r, 2] <- 1 + Pchg2_impl_prod_dom[p:q]   # domestic sectors only
  Sub_Scenarios[tarsec + 3*N, r, 2] <- PriceChg_impl_prod[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}

res2 <- run_scenario_calc(Price_Response[,,2])
VA_Tot_Chg[,,2]      <- res2$VA_Chg
DemandTot_new[,,2]   <- res2$DemandNew
CO2_Reg_Response[,2] <- res2$CO2


# =============================================================================
# 10. Scenario 3: Remove IMPLICIT subsidies via CONSUMER TAX (S3)
# =============================================================================
int <- pracma::repmat(PriceChg_impl_cons, G, 1)
Pchg3_impl_cons <- int[, match(Reg_Inclu, toupper(regnam))]

Price_Response[,,3] <- phi_cons * Pchg3_impl_cons + 1
# Sub_revenue[,3] already set in calibration block (phi × IMF_impl, same as S2)
for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(toupper(regnam) == toupper(Reg_Inclu[r]))
  Sub_Scenarios[tarsec + 2*N, r, 3] <- PriceChg_impl_cons[tarsec, tarreg] * Demand_reg[tarsec, tarreg]
}

res3 <- run_scenario_calc(Price_Response[,,3])
VA_Tot_Chg[,,3]      <- res3$VA_Chg
DemandTot_new[,,3]   <- res3$DemandNew
CO2_Reg_Response[,3] <- res3$CO2


# =============================================================================
# 11. Scenario 4: No policy (reference)
# =============================================================================
Sub_Scenarios[,,4]  <- 0
Sub_revenue[, 4]    <- 0
Price_Response[,,4] <- 1
DemandTot_new[,,4]  <- DemandTot_ori
VA_Tot_Chg[,,4]     <- 0
CO2_Reg_Response[,4] <- colSums(FinalDemand_ori * CF_IncluDirect)


# =============================================================================
# 12. Save results
# =============================================================================
save(Subsidy_Clean, Sub_Scenarios, Sub_revenue, CO2_Reg_Response, Price_Response,
     DemandTot_new, DemandTot_ori, VA_Tot_Chg,
     Mimunum_sum, Minumum_reg, Minumum, Elasticity, Elasticity_raw,
     PriceChg_expl_cons, PriceChg_expl_prod,
     PriceChg_impl_cons, PriceChg_impl_prod,
     file = str_c(pathout2, "/Subsidy scenarios_price and CO2 response_v5.Rdata"))


rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                   "pathdata3","pathdata4","pathcode","pathcode_si2021",
                                   "pathcode_imf25","pathout2_imf25","pathout3_imf25","pathout5_imf25"))])
gc()
