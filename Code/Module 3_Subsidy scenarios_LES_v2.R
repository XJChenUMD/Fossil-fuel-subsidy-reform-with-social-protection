#Module 3: Energy subsidy scenarios
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu


# 1: Removal all explicit subsidies
# 2: Removal all implicit subsidies with production tax
# 3: Removal all implicit subsidies with consumer tax
# 4: Combine 1 & 2
# 5: Combine 1 & 3
# 6: No additional policy (reference scenarios).

# Calculate the price changes by products and demand, CO2 response to removal fossil fuel subsidies.
# Also, calculate the revenue saving under each scenario.
# total emission in 2017 fr GTAP 11b, 32971.25 MT CO2.


#Load the data from the results in Module 2----
load(str_c(str_c(pathout,"/GTAP_11b_2017_Elements for MRIO.Rdata")))
load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
regnam <- toupper(regnam)
#--------------

# estimate the expenditure elasticity
# --------------
Elasticity_raw <- array(0,dim = c(N,length(Reg_Inclu)))
rownames(Elasticity_raw) <- secnam;colnames(Elasticity_raw) <- Reg_Inclu

for (r in 1:length(Reg_Inclu)) {
  Exp_Sec <- Exp_perCap_detail[,,r]
  Exp <- colSums(Exp_perCap_detail[,,r],na.rm = T)
  for (j in 1:N) {
    if (sum(Exp_Sec[j,],na.rm = T) != 0){
      a <- Exp_Sec[j,]
      data <- data.frame(cbind(log(a),log(Exp)))
      data[is.na(data) | data=="Inf"| data=="-Inf"] = NaN
      equation <- lm(data = data, X1 ~ X2)
      Elasticity_raw[j,r] <- equation$coefficients[2]
    }
  }
}
write.csv(Elasticity_raw,file = str_c(pathout2,"/Elasticity_raw.csv"))

Elasticity <- Elasticity_raw
Elasticity[Elasticity<=0] <- 0.1#treatment for negative elasticity
#--------------


#calculate some summary statistics to save time---------
FinalDemand_ori <- apply(Expenditure_detail_global, c(1,3), sum) + Gov_detail_global + Inv_detail_global

DemandTot_ori <- array(0,dim = c(203,length(Reg_Inclu)),
                       dimnames = list(c(Gnam,"Firms","Gov"),Reg_Inclu))

#Locate the "minimum demand" (committed consumption)
#people live below the extreme poverty line ware unable to adjust their consumption structure
#not the extreme one, but the bottom one in each country
Minumum <- array(0,dim = dim(FinalDemand_ori),dimnames = dimnames(FinalDemand_ori))
Minumum_reg <- array(0,dim = c(203,length(Reg_Inclu)))
for (r in 1:length(Reg_Inclu)) {
  a <- which(colSums(Expenditure_detail_global[,,r]) > 0)[1]
  Minumum[,r] <- Expenditure_detail_global[,a,r]/Population[a,r]*sum(Population[,r])
  Minumum_reg[1:201,r] <- sum(Expenditure_detail_global[,a,r])/Population[a,r]*Population[,r]
}
Mimunum_sum <- colSums(Minumum)

INT2 <- array(0,dim = c(GN,203,length(Reg_Inclu)),
              dimnames = list(regsecnam,c( Gnam,"Firms","Gov"),Reg_Inclu))

for (r in 1:length(Reg_Inclu)) {
  int <- cbind(Expenditure_detail_global[,,r],Inv_detail_global[,r],Gov_detail_global[,r])
  xx <- array(0,dim = c(GN, length(Gnam)+2))
  xx[,1:length(Gnam)] <- t(pracma::repmat((Minumum[,r]/sum(Population[,r])),length(Gnam),1))%*%diag(Population[,r])
  INT2[,,r] <- t(t(int-xx)/(colSums(int)-Minumum_reg[,r]))
  DemandTot_ori[,r] <- colSums(int)
}
INT2[is.nan(INT2)] <- 0
INT2[is.infinite(INT2)] <- 0
#---------


#Load and clean energy subsidy data----
IMFSubsidy <- read.csv("C:/Users/xiang/OneDrive/Energy subsidy and distributional issue/IMF_fuelsubsidies2023new.csv")  

IMFSubsidy %>% 
  filter(scenario %in% "U1") %>% 
  select(countryname,countrycode,year,mit.gdp.pre.lvl.1,
         mit.sub.texp.tot.gso.1,mit.sub.texp.tot.die.1,mit.sub.texp.tot.lpg.1,mit.sub.texp.tot.ker.1,mit.sub.texp.tot.oop.1,
         mit.sub.tpro.tot.oil.1,mit.sub.texp.tot.nga.1,mit.sub.tpro.tot.nga.1,mit.sub.texp.tot.coa.1,mit.sub.tpro.tot.coa.1,
         mit.sub.texp.tot.ecy.1,mit.sub.tpro.tot.ecy.1,mit.sub.timp.tot.gso.1,mit.sub.timp.tot.die.1,mit.sub.timp.tot.lpg.1,	
         mit.sub.timp.tot.ker.1,mit.sub.timp.tot.oop.1,mit.sub.timp.tot.nga.1,mit.sub.timp.tot.coa.1,mit.sub.timp.tot.ecy.1) %>% 
  mutate(GDP = mit.gdp.pre.lvl.1*.90787,#Million$ in 2017, 2021 to 2017: 99.995/110.14225 = 0.9078714 https://fred.stlouisfed.org/series/GDPDEF
         Petro_Exp_Cons = (mit.sub.texp.tot.gso.1+mit.sub.texp.tot.die.1+mit.sub.texp.tot.lpg.1+mit.sub.texp.tot.ker.1+mit.sub.texp.tot.oop.1)*.90787,
         Gas_Exp_Cons = (mit.sub.texp.tot.nga.1)*.90787, Coal_Exp_Cons = (mit.sub.texp.tot.coa.1)*.90787, Elec_Exp_Cons = (mit.sub.texp.tot.ecy.1)*.90787,
         Petro_Exp_Prod = (mit.sub.tpro.tot.oil.1)*.90787, Gas_Exp_Prod = (mit.sub.tpro.tot.nga.1)*.90787, 
         Coal_Exp_Prod = (mit.sub.tpro.tot.coa.1)*.90787, Elec_Exp_Prod = (mit.sub.tpro.tot.ecy.1)*.90787,
         Petro_Imp = (mit.sub.timp.tot.gso.1+mit.sub.timp.tot.die.1+mit.sub.timp.tot.lpg.1+mit.sub.timp.tot.ker.1+mit.sub.timp.tot.oop.1)*.90787,
         Gas_Imp = (mit.sub.timp.tot.nga.1)*.90787, Coal_Imp = (mit.sub.timp.tot.coa.1)*.90787, Elec_Imp = (mit.sub.timp.tot.ecy.1)*.90787) %>% 
  select(-c(mit.sub.texp.tot.gso.1,mit.sub.texp.tot.die.1,mit.sub.texp.tot.lpg.1,mit.sub.texp.tot.ker.1,mit.sub.texp.tot.oop.1,
            mit.sub.tpro.tot.oil.1,mit.sub.texp.tot.nga.1,mit.sub.tpro.tot.nga.1,mit.sub.texp.tot.coa.1,mit.sub.tpro.tot.coa.1,
            mit.sub.texp.tot.ecy.1,mit.sub.tpro.tot.ecy.1,mit.sub.timp.tot.gso.1,mit.sub.timp.tot.die.1,mit.sub.timp.tot.lpg.1,	
            mit.sub.timp.tot.ker.1,mit.sub.timp.tot.oop.1,mit.sub.timp.tot.nga.1,mit.sub.timp.tot.coa.1,mit.sub.timp.tot.ecy.1)) -> Subsidy_Clean

Subsidy_Clean$GDPshare <- rowSums(Subsidy_Clean[,6:17])/Subsidy_Clean[,5]/1000
# Subsidy_Clean %>% filter(year %in% 2017) %>% filter(countryname %in% Reg_corr$IMF_reg) -> Subsidy_Year
Subsidy_Clean %>% filter(year %in% 2022) %>% filter(countryname %in% Reg_corr$IMF_reg) -> Subsidy_Year#Use data for the latest year
#---------


#define output variable
#-----------
scennam <- c("Explicit","Implicit_prod","Implicit_cons",
             "Explicit_Implicit_prod","Explicit_Implicit_cons","Null")

Sub_Scenarios <- array(0,dim = c(N*4,length(Reg_Inclu),6), 
                       dimnames = list(str_c(rep(c("Expli_Cons_","Expli_Prod_","Impli_Cons_","Impli_Prod_"),each = N), 
                                                                               rep(secnam,4)), Reg_Inclu,scennam))
#"Explicit" include both consumer and production subsidy
# All tax scenarios transit to a consumption tax to improve the code efficiency for calculating tax burden

Sub_revenue <- array(0,dim = c(length(Reg_Inclu),7), dimnames = list(Reg_Inclu,c(scennam,"GDP")))

Price_Response <- array(0,dim = c(GN,length(Reg_Inclu),6), dimnames = list(regsecnam, Reg_Inclu,scennam))

CO2_Reg_Response <- array(0,dim = c(length(Reg_Inclu),6), dimnames = list(Reg_Inclu,scennam))

DemandTot_new <- array(0,dim = c(203,length(Reg_Inclu),6), dimnames = list(c(Gnam,"Firms","Gov"),Reg_Inclu,scennam))

VA_Tot_Chg <- array(0,dim = c(3,length(Reg_Inclu),6), dimnames = list(c("Lab","Capital","Tax"),Reg_Inclu,scennam))
#-----------

Sub_revenue[,7] <- Reg_corr$GDP_2017_WB/10^6#M$

#Part 1: Removal all explicit subsidies
#--------------
tarsec <- c(15:17,32,46:47)

OutputMAT <- TotOutput
dim(OutputMAT) <- c(N,G)

FD_reg <- array(0,dim = c(N,G),dimnames = list(secnam,regnam))
Z_reg <- array(0,dim = c(N,G),dimnames = list(secnam,regnam))

for (r in 1:G) {
  fac <- rep(secnam,G)
  
  m = (r-1)*3+1;n = r*3 
  FD_reg[,r] <- rowSums(rowsum(FD[,m:n],fac, reorder = F))
  
  p = (r-1)*N+1;q = r*N 
  Z_reg[,r] <- rowSums(rowsum(Inter_Trade[,p:q],fac,reorder = F))
}

Demand_reg <- FD_reg + Z_reg

Sub_S1 <- array(0,dim = c(N*4,length(Reg_Inclu)),
                dimnames = list(str_c(rep(c("Expli_Cons_","Expli_Prod_","Impli_Cons_","Impli_Prod_"),each = N), 
                                      rep(secnam,4)),Reg_corr$GTAP.reg))

for (r in 1:length(Reg_Inclu)) {
  tarreg <- which(regnam %in% Reg_Inclu[r])
  
  ConSub <- as.numeric(Subsidy_Year %>% filter(countrycode %in% Reg_Inclu[r]) %>% 
                         select(Coal_Exp_Cons,Petro_Exp_Cons,Gas_Exp_Cons,Elec_Exp_Cons))
  b <- rep(0,6)
  b[c(1,4)] <- ConSub[1]*(Demand_reg[tarsec,tarreg][c(1,4)])/sum(Demand_reg[tarsec,tarreg][c(1,4)])
  b[c(2,4)] <- b[c(2,4)]+ConSub[2]*(Demand_reg[tarsec,tarreg][c(2,4)])/sum(Demand_reg[tarsec,tarreg][c(2,4)])
  b[c(3,6)] <- ConSub[3]*(Demand_reg[tarsec,tarreg][c(3,6)])/sum(Demand_reg[tarsec,tarreg][c(3,6)])
  b[5] <- ConSub[4]
  Sub_S1[tarsec,r] <- b
  
  ProdSub <- as.numeric(Subsidy_Year %>% filter(countrycode %in% Reg_Inclu[r]) %>% 
                          select(Coal_Exp_Prod,Petro_Exp_Prod,Gas_Exp_Prod,Elec_Exp_Prod))
  a <- rep(0,6)
  a[c(1,4)] <- ProdSub[1]*(OutputMAT[tarsec,tarreg][c(1,4)])/sum(OutputMAT[tarsec,tarreg][c(1,4)])
  a[c(2,4)] <- a[c(2,4)]+ProdSub[2]*(OutputMAT[tarsec,tarreg][c(2,4)])/sum(OutputMAT[tarsec,tarreg][c(2,4)])
  a[c(3,6)] <- ProdSub[3]*(OutputMAT[tarsec,tarreg][c(3,6)])/sum(OutputMAT[tarsec,tarreg][c(3,6)])
  a[5] <- ProdSub[4]
  Sub_S1[tarsec+N,r] <-  a
}

Sub_Scenarios[,,1] <- Sub_S1
Sub_revenue[,1] <- colSums(Sub_S1)

#Match with GTAP region
a <- Sub_S1[1:N,]#Consumer
Cons_Exp <- a[,match(regnam,colnames(a))]
Cons_Exp[is.na(Cons_Exp)] <- 0
int <- pracma::repmat(Cons_Exp/Demand_reg,G,1)
Pchg1 <- int[,match(colnames(a),regnam)]

a2 <- Sub_S1[(N+1):(2*N),]#Producer
Prod_Exp <- a2[,match(regnam,colnames(a2))]
Prod_Exp[is.na(Prod_Exp)] <- 0
dim(Prod_Exp) <- c(GN,1)
Pchg2 <- as.vector(as.vector((Prod_Exp/(TotOutput)))%*%Leontief)

Price_Response[,,1] <- t(pracma::repmat(Pchg2+1,length(Reg_Inclu),1))+Pchg1
FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(Price_Response[,,1])

FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
             rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
             rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

VA_mat_detail_chg <- VA_mat_detail
DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
CO2_Reg_Response_int <- c()

for (r in 1:length(Reg_Inclu)) {
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail_chg[,,r] <- INT[,y:z]

  DemandTot_new_int[,r] <- DemandTot_ori[,r] +
    rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
  CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                    INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                    (Price_Response[,r,1]))*CF_IncluDirect[,r])
}

VA_Tot_Chg[,,1] <- apply(VA_mat_detail_chg,3,rowSums)
DemandTot_new[,,1] <- DemandTot_new_int
CO2_Reg_Response[,1] <- CO2_Reg_Response_int
#--------------

#Part 2: Removal all implicit subsidies with production tax
#--------------
Sub_S2 <- array(0,dim = c(N*4,length(Reg_Inclu)),
                dimnames = list(str_c(rep(c("Expli_Cons_","Expli_Prod_","Impli_Cons_","Impli_Prod_"),each = N), 
                                      rep(secnam,4)),Reg_corr$GTAP.reg))

for (r in 1:length(Reg_Inclu)) {
  ProdSub <- as.numeric(Subsidy_Year %>% filter(countrycode %in% Reg_Inclu[r]) %>% 
                          select(Coal_Imp,Petro_Imp,Gas_Imp,Elec_Imp))
  a <- rep(0,6)
  a[c(1,4)] <- ProdSub[1]*(OutputMAT[tarsec,tarreg][c(1,4)])/sum(OutputMAT[tarsec,tarreg][c(1,4)])
  a[c(2,4)] <- a[c(2,4)]+ProdSub[2]*(OutputMAT[tarsec,tarreg][c(2,4)])/sum(OutputMAT[tarsec,tarreg][c(2,4)])
  a[c(3,6)] <- ProdSub[3]*(OutputMAT[tarsec,tarreg][c(3,6)])/sum(OutputMAT[tarsec,tarreg][c(3,6)])
  a[5] <- ProdSub[4]
  Sub_S2[tarsec+3*N,r] <-  a
}

Sub_Scenarios[,,2] <- Sub_S2
Sub_revenue[,2] <- colSums(Sub_S2)#M$

a3 <- Sub_S2[(3*N+1):(4*N),]#Producer
Prod_Imp <- a3[,match(regnam,colnames(a3))]
Prod_Imp[is.na(Prod_Imp)] <- 0
dim(Prod_Imp) <- c(GN,1)
Pchg3 <- as.vector(as.vector((Prod_Imp/TotOutput))%*%Leontief)
# Pchg3 <- as.vector(as.vector(Prod_Imp/(TotOutput+Prod_Imp))%*%Leontief)

Price_Response[,,2] <- t(pracma::repmat(Pchg3+1,length(Reg_Inclu),1))
FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(Price_Response[,,2])

FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
             rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
             rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

VA_mat_detail_chg <- VA_mat_detail
DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
CO2_Reg_Response_int <- c()

for (r in 1:length(Reg_Inclu)) {
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail_chg[,,r] <- INT[,y:z]

  DemandTot_new_int[,r] <- DemandTot_ori[,r] +
    rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
  CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                    INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                    (Price_Response[,r,2]))*CF_IncluDirect[,r])
}

VA_Tot_Chg[,,2] <- apply(VA_mat_detail_chg,3,rowSums)
DemandTot_new[,,2] <- DemandTot_new_int
CO2_Reg_Response[,2] <- CO2_Reg_Response_int
#--------------

#Part 3: Removal all implicit subsidies with consumer tax
#--------------
Sub_S3 <- array(0,dim = c(N*4,length(Reg_Inclu)),
                dimnames = list(str_c(rep(c("Expli_Cons_","Expli_Prod_","Impli_Cons_","Impli_Prod_"),each = N), 
                                      rep(secnam,4)),Reg_corr$GTAP.reg))

for (r in 1:length(Reg_Inclu)) {
  ConSub <- as.numeric(Subsidy_Year %>% filter(countrycode %in% Reg_Inclu[r]) %>% 
                         select(Coal_Imp,Petro_Imp,Gas_Imp,Elec_Imp))
  b <- rep(0,6)
  b[c(1,4)] <- ConSub[1]*(Demand_reg[tarsec,tarreg][c(1,4)])/sum(Demand_reg[tarsec,tarreg][c(1,4)])
  b[c(2,4)] <- b[c(2,4)]+ConSub[2]*(Demand_reg[tarsec,tarreg][c(2,4)])/sum(Demand_reg[tarsec,tarreg][c(2,4)])
  b[c(3,6)] <- ConSub[3]*(Demand_reg[tarsec,tarreg][c(3,6)])/sum(Demand_reg[tarsec,tarreg][c(3,6)])
  b[5] <- ConSub[4]
  Sub_S3[tarsec+2*N ,r] <- b
}

Sub_Scenarios[,,3] <- Sub_S3
Sub_revenue[,3] <- colSums(Sub_S3)#M$


#Match with GTAP region
a4 <- Sub_S3[(2*N+1):(3*N),]#Consumer
Cons_Imp <- a4[,match(regnam,colnames(a4))]
Cons_Imp[is.na(Cons_Imp)] <- 0
int <- pracma::repmat(Cons_Imp/Demand_reg,G,1)
Pchg4 <- int[,match(colnames(a4),regnam)]

Price_Response[,,3] <- Pchg4+1
FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(Price_Response[,,3])

FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
             rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
             rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

VA_mat_detail_chg <- VA_mat_detail
DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
CO2_Reg_Response_int <- c()

for (r in 1:length(Reg_Inclu)) {
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail_chg[,,r] <- INT[,y:z]
  
  DemandTot_new_int[,r] <- DemandTot_ori[,r] +
    rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
  CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                    INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                    (Price_Response[,r,3]))*CF_IncluDirect[,r])
}

VA_Tot_Chg[,,3] <- apply(VA_mat_detail_chg,3,rowSums)
DemandTot_new[,,3] <- DemandTot_new_int
CO2_Reg_Response[,3] <- CO2_Reg_Response_int
#--------------

#Part 4: Combine 1 & 2
#--------------
Sub_S4 <- Sub_S1+Sub_S2

Sub_Scenarios[,,4] <- Sub_S4
Sub_revenue[,4] <- colSums(Sub_S4)#M$

Price_Response[,,4] <- t(pracma::repmat(Pchg2+Pchg3+1,length(Reg_Inclu),1))+Pchg1
FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(Price_Response[,,4])

FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
             rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
             rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

VA_mat_detail_chg <- VA_mat_detail
DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
CO2_Reg_Response_int <- c()
for (r in 1:length(Reg_Inclu)) {
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail_chg[,,r] <- INT[,y:z]
  
  DemandTot_new_int[,r] <- DemandTot_ori[,r] +
    rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
  CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                    INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                    (Price_Response[,r,4]))*CF_IncluDirect[,r])
}

VA_Tot_Chg[,,4] <- apply(VA_mat_detail_chg,3,rowSums)
DemandTot_new[,,4] <- DemandTot_new_int
CO2_Reg_Response[,4] <- CO2_Reg_Response_int
#--------------

#Part 5: Combine 1 & 3
#--------------
Sub_S5 <- Sub_S1+Sub_S3

Sub_Scenarios[,,5] <- Sub_S5
Sub_revenue[,5] <- colSums(Sub_S5)#M$

Price_Response[,,5] <- t(pracma::repmat(Pchg2+1,length(Reg_Inclu),1))+Pchg1+Pchg4
FinalDemand_New <- Minumum + (FinalDemand_ori-Minumum)/(Price_Response[,,5])

FinalDemand_chg <- rowSums(FinalDemand_New)-rowSums(FinalDemand_ori)
INT <- rbind(rowSums(VA_multiplier_Lab%*%FinalDemand_chg),
             rowSums(VA_multiplier_Cap%*%FinalDemand_chg),
             rowSums(VA_multiplier_Tax%*%FinalDemand_chg))

VA_mat_detail_chg <- VA_mat_detail
DemandTot_new_int <- array(0,dim = c(203,length(Reg_Inclu)))
CO2_Reg_Response_int <- c()

for (r in 1:length(Reg_Inclu)) {
  y = (Reg_corr$GTAP.NO[r]-1)*N+1;z = Reg_corr$GTAP.NO[r]*N
  VA_mat_detail_chg[,,r] <- INT[,y:z]

  DemandTot_new_int[,r] <- DemandTot_ori[,r] +
    rowSums(Transfer_mat[,,r]%*%diag(rowSums(VA_mat_detail_chg[,,r])))
  CO2_Reg_Response_int[r] <- sum((Minumum[,r]+
                                    INT2[,,r]%*%(DemandTot_new_int[,r]-Minumum_reg[,r])/
                                    (Price_Response[,r,5]))*CF_IncluDirect[,r])
}

VA_Tot_Chg[,,5] <- apply(VA_mat_detail_chg,3,rowSums)
DemandTot_new[,,5] <- DemandTot_new_int
CO2_Reg_Response[,5] <- CO2_Reg_Response_int
#--------------

#Part 6: No tax
#--------------
Sub_Scenarios[,,6] <- 0
Sub_revenue[,6] <- 0
Price_Response[,,6] <- 1

FinalDemand_New <- FinalDemand_ori
DemandTot_new[,,6] <- DemandTot_ori
VA_Tot_Chg[,,6] <- 0
CO2_Reg_Response[,6] <- colSums(FinalDemand_New*CF_IncluDirect)
#--------------


save(Subsidy_Clean, Sub_Scenarios, Sub_revenue, CO2_Reg_Response,Price_Response,
     DemandTot_new, DemandTot_ori,VA_Tot_Chg,
     Mimunum_sum,Minumum_reg,Minumum,Elasticity,Elasticity_raw,
     file = str_c(pathout2,"/Subsidy scenarios_price and CO2 response.Rdata"))

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()
