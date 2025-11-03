#Module 6: Results summary
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

load(str_c(pathout,"/Expenditure,Footprint,Pop,Emission.Rdata"))
load(str_c(pathout,"/Social assistance scenarios.Rdata"))
load(str_c(pathout2,"/Subsidy scenarios_price and CO2 response.Rdata"))

Recy <- c(.8,1)
Recynam <- c("80percent","100percent")

for (z in 1:length(Recy)) {#loop for various recycling share
  load(str_c(pathout3,"/Poverty, Ineq, Emission outcome by subsidy, sp, recy",Recynam[z],".Rdata"))
  
  #Figure: Uneven burden by deciles under different subsidy scenarios.
  #===========================
  #Subsidy burden by decile
  DEC_name <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
  DEC_num <- seq(0.1,1,0.1)
  
  #National level
  #-------------
  Population_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu)),
                             dimnames = list(DEC_name,Reg_corr$WBGDPreg))
  Expenditure_decile <- Population_decile
  Subsidy_cost_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu),dim(Sub_Scenarios)[3]),
                              dimnames = list(DEC_name,Reg_corr$WBGDPreg,dimnames(Sub_Scenarios)[[3]]))
  Cost_perEXP_decile <- Subsidy_cost_decile
  Cost_perCap_decile <- Subsidy_cost_decile
  Income_cost_decile <- Subsidy_cost_decile
  Income_cost_perEXP_decile <- Subsidy_cost_decile
  Income_cost_perCap_decile <- Subsidy_cost_decile
  
  Transfer_Str_Decile <- array(NA,dim = c(length(DEC_name),3,length(Reg_Inclu)),
                               dimnames = list(DEC_name,c("Lab","Cap","Tax"),Reg_corr$WBGDPreg))
  
  Subsidy_Benifit_Bins <- array(NA,dim = c(length(Gnam),length(Reg_Inclu),dim(Sub_Scenarios)[3]),
                                dimnames = list(Gnam,Reg_corr$WBGDPreg,dimnames(Sub_Scenarios)[[3]]))
  
  SP_Benifit_Bins <- array(NA,dim = c(length(Gnam),length(Reg_Inclu),dim(Sub_Scenarios)[3],dim(SP_Scenarios)[[3]]),
                           dimnames = list(Gnam,Reg_corr$WBGDPreg,dimnames(Sub_Scenarios)[[3]],dimnames(SP_Scenarios)[[3]]))
  
  for (i in 1:(dim(Sub_Scenarios)[3]-1)) {
    load(str_c(pathout3,"/",dimnames(Sub_Scenarios)[[3]][i],"-",
               dimnames(SP_Scenarios)[[3]][1],Recynam[z],".Rdata"))
    
    Subsidy_Benifit_Bins[,,i] <- Cost_perCap*Population#Subsidy benefit at bins level,unit: $
    
    for (q in 1:length(Reg_Inclu)) {
      a <- trunc(sum(trunc(Population[,q]/100))/10)#Population for each decile
      B <- rep.int(Expenditure_perCap[,q],trunc(Population[,q]/100))#change the unit for pop to boost calculation
      C <- rep.int(Cost_perCap[,q],trunc(Population[,q]/100))#Subsidy Cost
      D <- rep.int(Inc_effect[,q],trunc(Population[,q]/100))#Income Cost
      E1 <- rep.int(Transfer_mat[1:length(Gnam),1,q],trunc(Population[,q]/100))
      E2 <- rep.int(Transfer_mat[1:length(Gnam),2,q],trunc(Population[,q]/100))
      E3 <- rep.int(Transfer_mat[1:length(Gnam),3,q],trunc(Population[,q]/100))
      
      for (p in 1:length(DEC_name)){
        Population_decile[p,q] <- a*100
        Expenditure_decile[p,q] <- sum(B[((p-1)*a+1):(p*a)])*100
        Subsidy_cost_decile[p,q,i] <- sum(C[((p-1)*a+1):(p*a)])*100
        Income_cost_decile[p,q,i] <- sum(D[((p-1)*a+1):(p*a)])*100
        Transfer_Str_Decile[p,1,q] <- sum(E1[((p-1)*a+1):(p*a)])*100/length(E1)
        Transfer_Str_Decile[p,2,q] <- sum(E2[((p-1)*a+1):(p*a)])*100/length(E2)
        Transfer_Str_Decile[p,3,q] <- sum(E3[((p-1)*a+1):(p*a)])*100/length(E3)
      }
    }
    Cost_perEXP_decile[,,i] <-  Subsidy_cost_decile[,,i]/Expenditure_decile#unit: 1
    Cost_perCap_decile[,,i] <- Subsidy_cost_decile[,,i] /Population_decile#unit: $
    Income_cost_perEXP_decile[,,i] <- Income_cost_decile[,,i]/Expenditure_decile#unit: 1
    Income_cost_perCap_decile[,,i] <- Income_cost_decile[,,i] /Population_decile#unit: $
    
    #Uneven distribution of subsidy and social protection (bins level)
    for (j in 1:(dim(SP_Scenarios)[[3]])) {
      load(str_c(pathout3,"/",dimnames(Sub_Scenarios)[[3]][i],"-",
                 dimnames(SP_Scenarios)[[3]][j],Recynam[z],".Rdata"))
      SP_Benifit_Bins[,,i,j] <- pracma::repmat(Benifit_tar,201,1)*Pop_tar#Social protection benefit at bins level,unit: $
    }
  }
  
  Tot_cost_perEXP_decile <- Income_cost_perEXP_decile+Cost_perEXP_decile
  
  
  a <- SP_Benifit_Bins[,,4,]
  INT <- a
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(a)[[2]],dim(a)[3]),
                         rep(dimnames(a)[[3]], each = dim(a)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(a)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_SP") %>%
    separate(col = Reg_SP, "::", into = c("Country","Scenario")) -> SP_Benifit
  
  Pop <- as.data.frame(Population)
  Pop$Group <- rownames(Pop)
  Pop %>% pivot_longer(-Group,names_to = "Country",values_to = "Poplation") -> Pop_bins
  
  
  SP_Benifit_Lorenz <- left_join(SP_Benifit %>% filter(!Scenario %in% c("Null","Universal")),Pop_bins) %>% 
    group_by(Country,Scenario) %>% 
    mutate(Benefit_cum = cumsum(value)/sum(value), Pop_cum = cumsum(Poplation)/sum(Poplation))
 
  
  INT <- Subsidy_Benifit_Bins
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Subsidy_Benifit_Bins)[[2]],dim(Subsidy_Benifit_Bins)[3]),
                         rep(dimnames(Subsidy_Benifit_Bins)[[3]], each = dim(Subsidy_Benifit_Bins)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Subsidy_Benifit_Bins)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_Sub") %>%
    separate(col = Reg_Sub, "::", into = c("Country","Scenario")) -> Sub_Benifit
  
  Sub_Benifit_Lorenz <- left_join(Sub_Benifit %>% filter(!Scenario %in% "Null"),Pop_bins) %>% 
    mutate(value = ifelse(is.na(value),0,value)) %>% 
    group_by(Country,Scenario) %>% 
    mutate(Benefit_cum = cumsum(value)/sum(value), Pop_cum = cumsum(Poplation)/sum(Poplation))
  #-------------
  
  
  #Prepare national fig data
  #------------
  INT <- Tot_cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Tot_cost_perEXP_decile)[[2]],dim(Tot_cost_perEXP_decile)[3]),
                         rep(dimnames(Tot_cost_perEXP_decile)[[3]], each = dim(Tot_cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Tot_cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","SubScenario")) %>% 
    mutate(Vari = "Total_Effect") -> Tot_cost_PerExp_Figdata
  
  
  INT <- Income_cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Income_cost_perEXP_decile)[[2]],dim(Income_cost_perEXP_decile)[3]),
                         rep(dimnames(Income_cost_perEXP_decile)[[3]], each = dim(Income_cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Income_cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","SubScenario"))  %>% 
    mutate(Vari = "Income_Effect") -> Inc_cost_PerExp_Figdata
  
  
  INT <- Cost_perEXP_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Cost_perEXP_decile)[[2]],dim(Cost_perEXP_decile)[3]),
                         rep(dimnames(Cost_perEXP_decile)[[3]], each = dim(Cost_perEXP_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Cost_perEXP_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","SubScenario"))  %>% 
    mutate(Vari = "Price_Effect") -> Subsidy_cost_PerExp_Figdata
  
  Data_Unevenburden <- rbind(Tot_cost_PerExp_Figdata,Inc_cost_PerExp_Figdata,Subsidy_cost_PerExp_Figdata)
  #-------------
  #================
  
  
  #Figure: Social protection rate
  #================
  Cover_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu),dim(SP_Scenarios)[3]),
                            dimnames = list(DEC_name,Reg_corr$WBGDPreg,c(dimnames(SP_Scenarios)[[3]])))

  for (i in 1:(dim(SP_Scenarios)[3])){
    if(i <=5){
      Pop_tar <- Population*SP_Scenarios[,,i]
    }
    if (i == 6) {
      Pop_tar <- Population*read.csv(str_c(pathout3,"/Poverty focused Coverage rate--Explicit_Implicit_prod-100percent.csv"),row.names = 1)
    }

    Cover <- Pop_tar/Population
    Pop_tar_decile <- array(NA,dim = c(length(DEC_name),length(Reg_Inclu)),
                                dimnames = list(DEC_name,Reg_corr$WBGDPreg))

    for (q in 1:length(Reg_Inclu)) {
      a <- trunc(sum(trunc(Population[,q]/100))/10)#Population for each decile
      B <- rep.int(Cover[,q],trunc(Population[,q]/100))#change the unit for pop to boost calculation
      for (p in 1:length(DEC_name)) {Pop_tar_decile[p,q] <- sum(B[((p-1)*a+1):(p*a)],na.rm = T)*100}
    }
    rm(a,B);gc()

    Cover_decile[,,i] <- Pop_tar_decile/Population_decile*100
  }


  INT <- Cover_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Cover_decile)[[2]],dim(Cover_decile)[3]),
                         rep(dimnames(Cover_decile)[[3]], each = dim(Cover_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Cover_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Reg_SP") %>%
    separate(col = Reg_SP, "::", into = c("Reg","SPScenario")) -> SPCover_Decile
  #================
  
  
  
  #Figure: National level domestic best recycling policy
  #================
  X_IPL <- OUT_SUBSP[,which(dimnames(OUT_SUBSP)[[2]] %in% "PR_npl_sub"),
                     which(dimnames(OUT_SUBSP)[[3]] %in% "Explicit_Implicit_prod"),
                     which(dimnames(OUT_SUBSP)[[4]]%in% c("Cash","SP_current","SP_covid"))]
  
  write.csv(X_IPL,str_c(pathout5,"/","National best policy mix_raw results_Explicit_Implicit_prod",Recynam[z],".csv"),row.names = F)
  
  Nation_BestRecy <- as.data.frame(array(NA,dim = c(length(Reg_Inclu),2)))
  colnames(Nation_BestRecy) <- c("Country","BestRec")
  Nation_BestRecy$Country <- Reg_corr$WBGDPreg
  Nation_BestRecy$BestRec <- colnames(X_IPL)[apply(X_IPL, 1, which.min)]
  #================
  
  
  #Figure: revenue and gdp
  #================
  #revenue-amount--------
  Revenue_nation <- Sub_revenue[,1:6]
  write.csv(Revenue_nation,
            str_c(pathout5,"/", "Revenue_nation.csv"))
  
  
  #revenue-share of GDP
  RevenuetoGDP <- Revenue_nation/Sub_revenue[,7]
  #================
  
  
  INT <- Subsidy_cost_decile
  dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
  colnames(INT) <- str_c(rep(dimnames(Subsidy_cost_decile)[[2]],dim(Subsidy_cost_decile)[3]),
                         rep(dimnames(Subsidy_cost_decile)[[3]], each = dim(Subsidy_cost_decile)[2]),
                         sep = "::")
  INT <- as.data.frame(INT)
  INT$Group <- dimnames(Subsidy_cost_decile)[[1]]
  INT %>% pivot_longer(-Group, names_to = "Country_Tax") %>% 
    separate(col = Country_Tax, "::", into = c("Country","SubScenario")) -> Subsidy_cost_decile
  
  Subsidy_Clean <- Subsidy_Clean %>% mutate(countryname = ifelse(grepl("Russia", countryname),"Russian Federation",countryname))
  
  
  save(DEC_name,DEC_num,
       Transfer_Str_Decile,VA_Tot_Chg,
       Data_Unevenburden, #Uneven burden across decile at country level
       Revenue_nation,RevenuetoGDP,
       OUT_SUBSP,#raw full results by country, scenarios,
       CO2_Response_Recy,#CO2 response 
       SP_Scenarios,#Social protection
       Subsidy_Clean,#IMF Subsidy time series, All countries
       Subsidy_cost_decile,
       Nation_BestRecy,#Best recycling policy by country
       Gnam,Reg_Inclu,secnam,Reg_corr,
       Subsidy_Benifit_Bins,SP_Benifit_Bins,SP_Benifit_Lorenz, Sub_Benifit_Lorenz,
       Population,
       SPCover_Decile,Pop_bins,
       file = str_c(pathout5,"/Results summary",Recynam[z],".Rdata"))
}

rm(list = ls()[-which(ls() %in% c("path","pathout","pathout2","pathout3","pathout5",
                                  "pathdata3","pathdata4","pathcode"))])
gc()

