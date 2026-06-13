#Module 5 (v6): The poverty and inequality effect of combinations of subsidy scenarios (3) and assistance scenarios (4)
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu
#
# v5 changes vs v4: load _v5/_v4 files; pre-compute NPL/Poor/Gini baselines; remove gc()
# v6 speedup vs v5: moved Cost_perCap/Expen_sub/Pop_tar computation to j-loop level (once per (i,j,z)).
# v6b: recycling share reduced to 2 values only: 80% and 100%.


load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios_v6.Rdata"))
load(str_c(pathout2,"/Subsidy scenarios_price and CO2 response_v5.Rdata"))

#Prepare nesscessy variable----------
INT2 <- array(0,dim = c(GN,201,length(Reg_Inclu)), dimnames = list(regsecnam,Gnam,Reg_Inclu))
for (r in 1:length(Reg_Inclu)) {
  INT2[,,r] <- t(t(Expenditure_detail_global[,,r])/colSums(Expenditure_detail_global[,,r]))
}
INT2[is.nan(INT2)] <- 0

rm(Saving_mat_detail,VA_mat_detail,Td_vec,Transfer_mat,
   Footprint_detail_global,
   Footprint_detail,Exp_perCap_detail,
   Gov_detail_global,
   Emission_CB,Emission_PB,Reg_corr_Full, Reg_WBCD,Leontief, EMc)
gc()
#------------


#Set recycling shares: 80% and 100% only
Recy    <- c(0.80, 1.00)
Recynam <- c("80percent", "100percent")

# =============================================================
# PRE-COMPUTE: baseline quantities invariant across all loops
# These depend only on Expenditure_perCap & Population, NOT on
# subsidy scenario (i), SP scenario (j), or recycling rate (z).
# Without this, the computation inside the loops is repeated
# 21 × 3 × 5 × 2 = 630 times unnecessarily.
# =============================================================
NPL_baseline  <- list()   # NPL per poverty line setting
Poor_baseline <- list()   # Poor counts per poverty line setting
Gini_ori_vec     <- numeric(length(Reg_Inclu))
IneqRat_ori_vec  <- numeric(length(Reg_Inclu))

for (npl_setting in 1:2) {
  if (npl_setting == 1) PR_2017 <- Reg_corr$WB_PR/100
  if (npl_setting == 2) PR_2017 <- Reg_corr$WB_2.15/100
  pr  <- pracma::repmat(PR_2017, 201, 1)
  pop <- apply(t(t(Population)/colSums(Population)), 2, cumsum)
  position <- apply(abs(pr-pop), 2, which.min)
  NPL_2017 <- c()
  for (r in 1:length(Reg_Inclu)) {
    xx <- pr[,r]-pop[,r]
    if (xx[position[r]] > 0) {
      share <- xx[position[r]]/(xx[position[r]]-xx[position[r]+1])
      NPL_2017[r] <- Expenditure_perCap[position[r],r]+
        share*(Expenditure_perCap[position[r]+1,r]-Expenditure_perCap[position[r],r])
    } else {
      share <- xx[position[r]-1]/(xx[position[r]-1]-xx[position[r]])
      if (sum(share) != 0) {
        NPL_2017[r] <- Expenditure_perCap[position[r]-1,r]+
          share*(Expenditure_perCap[position[r],r]-Expenditure_perCap[position[r]-1,r])
      } else {
        NPL_2017[r] <- Expenditure_perCap[position[r],r]
      }
    }
  }
  npl_pre <- round(pracma::repmat(NPL_2017, 201, 1), 5)
  NPL_baseline[[npl_setting]] <- npl_pre

  Poor_pre <- array(0, dim=dim(Population))
  for (r in 1:length(Reg_Inclu)) {
    expend_col <- Expenditure_perCap[, r]
    pop_col    <- Population[, r]
    order_idx  <- order(expend_col)
    expend_sorted <- expend_col[order_idx]
    pop_sorted    <- pop_col[order_idx]
    expend_sorted[expend_sorted == 0] <- NA
    xx       <- npl_pre[, r] - expend_sorted
    position <- which.min(abs(xx))
    if (xx[position] > 0) {
      share <- xx[position]/(xx[position]-xx[position+1])
      ps <- rep(0, length(expend_col))
      ps[1:position] <- pop_sorted[1:position]
      ps[position+1] <- pop_sorted[position+1]*share
      ps[is.na(ps)]  <- 0
    } else if (position > 1) {
      share <- xx[position-1]/(xx[position-1]-xx[position])
      ps <- rep(0, length(expend_col))
      ps[1:(position-1)] <- pop_sorted[1:(position-1)]
      ps[position] <- pop_sorted[position]*share
      ps[is.na(ps)] <- 0
    } else {
      ps <- rep(0, length(expend_col))
    }
    Poor_pre[, r] <- ps[order(order_idx)]
  }
  Poor_baseline[[npl_setting]] <- Poor_pre
}

# Baseline Gini and IneqRat (invariant across all scenarios)
for (r in 1:length(Reg_Inclu)) {
  CumShareEXP <- cumsum(Expenditure_perCap[,r]*Population[,r])/
                   sum(Expenditure_perCap[,r]*Population[,r])
  SharePop    <- Population[,r]/sum(Population[,r])
  Gini_ori_vec[r] <- (1-2*sum(CumShareEXP*SharePop))*(length(SharePop)/(length(SharePop)-1))
  CumSharePop  <- cumsum(Population[,r])/sum(Population[,r])
  loca_bottom  <- which.min(abs(CumSharePop - 0.1))
  loca_top     <- which.min(abs(CumSharePop - 0.9))
  IneqRat_ori_vec[r] <- ((1-CumShareEXP[loca_top])/(1-CumSharePop[loca_top]))/
                          (CumShareEXP[loca_bottom]/CumSharePop[loca_bottom])
}
message("Pre-computation complete (NPL, Poor, Gini baselines).")
# =============================================================

for (z in 1:length(Recy)) {#loop for various recycling share
  #Define aggregated output variable----------
  CO2_Response_Recy <- array(0,dim = c(dim(Sub_Scenarios)[3],dim(SP_Scenarios)[3]),
                             dimnames = list(dimnames(Sub_Scenarios)[[3]],
                                             dimnames(SP_Scenarios)[[3]]))
  
  OUT_SUBSP <- array(NA, dim = c(length(Reg_Inclu),15,
                                 dim(Sub_Scenarios)[3],dim(SP_Scenarios)[3]),
                     dimnames = list(Reg_corr$WBGDPreg, 
                                     c("Pop",
                                       "PPop_npl_ori","PR_npl_ori","PPop_ipl_ori","PR_ipl_ori",
                                       "Gini_ori",
                                       "PPop_npl_sub", "PR_npl_sub","PPop_ipl_sub", "PR_ipl_sub",
                                       "Gini_sub",
                                       "CO2_ori","CO2_sub","IneqRat_ori","IneqRat_sub"),
                                     dimnames(Sub_Scenarios)[[3]],dimnames(SP_Scenarios)[[3]]))
  #------------
  
  for (i in 1:dim(Sub_Scenarios)[3]) {
    t2 <- Sys.time()
    
    # (PerfectTargeted scenario removed)
    
    for (j in 1:dim(SP_Scenarios)[3]) {
      #Here we calculate the emission as consumption-based emission.

      # ---------------------------------------------------------------
      # ONCE PER (i,j,z): subsidy effect + social assistance
      # These do NOT depend on npl_setting — compute once, not twice.
      # ---------------------------------------------------------------
      #Subsidy cost effect
      Cost_perCap <- array(0, dim = dim(Population))
      for (r in 1:length(Reg_Inclu)) {
        Cost_perCap[,r] <- (Price_Response[,r,i]-1)%*%Expenditure_detail_global[,,r]/Population[,r]*10^6
      }
      Expen_sub  <- DemandTot_new[1:length(Gnam),,i]*10^6/Population - Cost_perCap
      Inc_effect <- (DemandTot_ori[1:length(Gnam), ] - DemandTot_new[1:length(Gnam),,i])*10^6/Population

      #Social assistance targeting
      Pop_tar   <- Population * SP_Scenarios[,,j]
      Pop_untar <- Population - Pop_tar
      Benifit_tar <- Sub_revenue[,i]*Recy[z]/colSums(Pop_tar)*10^6  #$
      Benifit_tar[is.nan(Benifit_tar)]      <- 0
      Benifit_tar[is.infinite(Benifit_tar)] <- 0

      #Export benefit per capita (once per j, not once per npl_setting)
      write.csv(Benifit_tar,
                file = str_c(pathout3,"/",dimnames(Sub_Scenarios)[[3]][i],"-",
                             dimnames(SP_Scenarios)[[3]][j],Recynam[z],"Benifit per capita.csv"))

      Expen_perCap_untar <- Expen_sub
      Expen_perCap_tar   <- Expen_sub + pracma::repmat(Benifit_tar, 201, 1)
      Expen_perCap_tar[is.na(Expen_perCap_tar)]     <- 0
      Expen_perCap_untar[is.na(Expen_perCap_untar)] <- 0
      # ---------------------------------------------------------------

      #loop for different poverty line setting
      NPL_setting <- c("WB_PR","WB_2.15")
      for (npl_setting in 1:length(NPL_setting)){
        OUT_SUBSP[,1,i,j] <- colSums(Population)  #Population

        # Use pre-computed NPL and Poor baseline (invariant across i, j, z)
        npl  <- NPL_baseline[[npl_setting]]
        Poor <- Poor_baseline[[npl_setting]]

        #Baseline poverty calculation
        if (npl_setting == 1) {
          OUT_SUBSP[,2,i,j] <- colSums(Poor)  #Poverty headcount under npl
          OUT_SUBSP[,3,i,j] <- colSums(Poor)/colSums(Population)  #Poverty rate under npl
        } else {
          OUT_SUBSP[,4,i,j] <- colSums(Poor)  #Poverty headcount under ipl
          OUT_SUBSP[,5,i,j] <- colSums(Poor)/colSums(Population)  #Poverty rate under ipl
        }

        #poverty calculation under subsidy+SP
        Poor_SP_untar <- array(0, dim = dim(Expenditure_perCap))
        
        for (r in 1 :length(Reg_Inclu)) {
          expend_col <- Expen_perCap_untar[, r]
          pop_col <- Pop_untar[, r]
          
          order_idx <- order(expend_col)
          expend_sorted <- expend_col[order_idx]
          pop_sorted <- pop_col[order_idx]
          expend_sorted[expend_sorted == 0] <- NA
          xx <- npl[, r] - expend_sorted
          position <- which.min(abs(xx))
          
          if (xx[position] > 0) {
            share <- xx[position] / (xx[position] - xx[position + 1])
            Poor_SP_sorted <- rep(0, length(expend_col))
            Poor_SP_sorted[1:position] <- pop_sorted[1:position]
            Poor_SP_sorted[position + 1] <- pop_sorted[position + 1] * share
            Poor_SP_sorted[is.na(Poor_SP_sorted)] <- 0
          } else if (position > 1) {
            share <- xx[position - 1] / (xx[position - 1] - xx[position])
            Poor_SP_sorted <- rep(0, length(expend_col))
            Poor_SP_sorted[1:(position - 1)] <- pop_sorted[1:(position - 1)]
            Poor_SP_sorted[position] <- pop_sorted[position] * share
            Poor_SP_sorted[is.na(Poor_SP_sorted)] <- 0
          } else {
            Poor_SP_sorted <- rep(0, length(expend_col))  # fallback for rare edge cases
          }
          Poor_SP_untar[, r] <- Poor_SP_sorted[order(order_idx)]
        }
        
        Poor_SP_tar <- array(0,dim = dim(Expenditure_perCap))
        
        for (r in 1 :length(Reg_Inclu)) {
          expend_col <- Expen_perCap_tar[, r]
          pop_col <- Pop_tar[, r]
          
          order_idx <- order(expend_col)
          expend_sorted <- expend_col[order_idx]
          pop_sorted <- pop_col[order_idx]
          expend_sorted[expend_sorted == 0] <- NA
          xx <- npl[, r] - expend_sorted
          position <- which.min(abs(xx))
          
          if (xx[position] > 0) {
            share <- xx[position] / (xx[position] - xx[position + 1])
            Poor_SP_sorted <- rep(0, length(expend_col))
            Poor_SP_sorted[1:position] <- pop_sorted[1:position]
            Poor_SP_sorted[position + 1] <- pop_sorted[position + 1] * share
            Poor_SP_sorted[is.na(Poor_SP_sorted)] <- 0
          } else if (position > 1) {
            share <- xx[position - 1] / (xx[position - 1] - xx[position])
            if (is.na(share)) {share = 0}
            Poor_SP_sorted <- rep(0, length(expend_col))
            Poor_SP_sorted[1:(position - 1)] <- pop_sorted[1:(position - 1)]
            Poor_SP_sorted[position] <- pop_sorted[position] * share
            Poor_SP_sorted[is.na(Poor_SP_sorted)] <- 0
          } else {
            Poor_SP_sorted <- rep(0, length(expend_col))  # fallback for rare edge cases
          }
          Poor_SP_tar[, r] <- Poor_SP_sorted[order(order_idx)]
        }
        
        
        if (npl_setting == 1) {#npl
          OUT_SUBSP[,7,i,j] <- colSums(Poor_SP_untar + Poor_SP_tar)
          OUT_SUBSP[,8,i,j] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
        }else{#ipl
          OUT_SUBSP[,9,i,j] <- colSums(Poor_SP_untar + Poor_SP_tar)
          OUT_SUBSP[,10,i,j] <- colSums(Poor_SP_untar + Poor_SP_tar)/colSums(Population)
        }
        #------------
      }
      
      #Inequality
      #-------------
      # Use pre-computed baseline Gini and IneqRat (invariant across all scenarios)
      OUT_SUBSP[,6,i,j]  <- Gini_ori_vec
      OUT_SUBSP[,14,i,j] <- IneqRat_ori_vec
      
      
      
      #inequality under subsidy+SP
      for (r in 1:length(Reg_Inclu)) {
        X <- order(c(Expen_perCap_tar[,r],Expen_perCap_untar[,r]))
        exp.order <- c(Expen_perCap_tar[,r],Expen_perCap_untar[,r])[X]
        pop.order <- c(Pop_tar[,r],Pop_untar[,r])[X]
        CumShareEXP <- cumsum(exp.order*pop.order)/sum(exp.order*pop.order)
        CumShareEXP[is.nan(CumShareEXP)] <- 0
        SharePop <- pop.order/sum(pop.order)
        OUT_SUBSP[r,11,i,j] <- (1-2*sum(CumShareEXP*SharePop))*
          (length(SharePop)/(length(SharePop)-1))
        
        CumSharePop <- cumsum(pop.order)/sum(pop.order)
        loca_bottom <- which.min(abs(CumSharePop - 0.1))
        loca_top <- which.min(abs(CumSharePop - 0.9))
        OUT_SUBSP[r,15,i,j] <- ((1-CumShareEXP[loca_top])/(1-CumSharePop[loca_top]))/
          (CumShareEXP[loca_bottom]/CumSharePop[loca_bottom])
      }
      
      rm(X,CumShareEXP,SharePop,exp.order,pop.order)
      # gc() removed — R manages memory automatically; calling gc() in tight loops is slow
      #-------------
      
      #Emission effect 
      #-------------
      #original emission and emission under subsidy+SP
      OUT_SUBSP[,12,i,j] <- CO2_Reg_Response[,4]  # Null scenario = index 4 in Module 3 v4
      
      RecyAmount <- pracma::repmat(Benifit_tar,201,1)*Pop_tar/10^6
      
      CO2_Recycle <- c()
      for (r in 1:length(Reg_Inclu)) {
        CO2_Recycle[r] <- t(INT2[,,r]%*%RecyAmount[,r]/
                              Price_Response[,r,i])%*%CF_IncluDirect[,r]
      }
      
      OUT_SUBSP[,13,i,j]  <- CO2_Reg_Response[,i]+CO2_Recycle
      
      CO2_Response_Recy[i,j] <- sum(OUT_SUBSP[,13,i,j])
      #-------------
      
      save(Expen_perCap_untar,Expen_perCap_tar,
           Expen_sub,Cost_perCap,Inc_effect,CO2_Recycle,
           Benifit_tar,Pop_untar,Pop_tar,Poor_SP_tar,Poor_SP_untar,
           file = str_c(pathout3,"/",dimnames(Sub_Scenarios)[[3]][i],"-",
                        dimnames(SP_Scenarios)[[3]][j],Recynam[z],".Rdata"))
    }
    
    print(str_c(Recynam[z],"--",dimnames(Sub_Scenarios)[[3]][i],"--",round(Sys.time()-t2,2)))
  }
  
  save(OUT_SUBSP, CO2_Response_Recy,
       file = str_c(pathout3,"/Poverty, Ineq, Emission outcome by subsidy, sp, recy",Recynam[z],".Rdata"))
}

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                  "pathdata3","pathdata4","pathcode","pathcode_si2021",
                                  "pathcode_imf25","pathout2_imf25","pathout3_imf25","pathout5_imf25",
                                  "pathout2_main_backup","pathout3_main_backup","pathout5_main_backup"))])
gc()
