#Module 7: Results summary and visualization
#Xiangjie Chen, GEOG, UMD
#xjchen@terpmail.umd.edu

library(ggpubr)

Recy <- c(0.8)
Recynam <- c("80percent")
z=1#for 80percent

#Load data and set order
#======================
load(str_c(pathout5,"/Results summary",Recynam[z],".Rdata"))

order.sub <- c("Explicit","Implicit_prod","Implicit_cons",
               "Explicit_Implicit_prod","Explicit_Implicit_cons","Null")
order.subname <- c("(R1) Explicit subsidy","(R2) Implicit subsidy\n(production tax)","(R3) Implicit subsidy\n(consumption tax)",
                   "(R4) Explicit & Implicit subsidy\n(production tax)","(R5) Explicit & Implicit subsidy\n(consumption tax)")
order.sp <- c("Null","Cash","SP_current","SP_covid","Universal","PerfectTargeted")
order.spname <- c("No social\nassistance",
                  "(S1) Cash-based\nprograms", "(S2) Current social\nassistance",
                  "(S3) Social assistance\nCOVID-19 expansion","(S4) Universal dividend","(S4) Perfectly\ntargeted")

order.reg <- c("Brazil","China","India","Mexico","Russian Federation","Indonesia","South Africa","Turkiye",
               "Belgium",  "Denmark", "Spain", "Finland", "France", "Ireland", "Luxembourg","Germany", "Italy",
               "Japan","United Kingdom", "United States",
               "Other LICs","Other LMICs","Other UMICs","Other HICs")

targetcounty <- c("Belgium",  "Denmark", "Spain", "Finland", "France", "United Kingdom", "Ireland", 
                  "Indonesia","South Africa","Turkiye","Japan",
                  "Luxembourg","Germany", "Italy", "United States",
                  "Brazil","China","India","Mexico","Russian Federation")#, "Colombia",,"South Africa"
targetISO <- c("BEL","DNK","ESP","FIN","FRA","GBR","IRL",
               "IDN","ZAF","TUR","JPN",
               "LUX","DEU","ITA","USA","BRA","CHN","IND","MEX","RUS")#"COL",,"ZAF"

Reg_corr <- Reg_corr %>% mutate(WB_IncomeGroup = ifelse(grepl("Low income", WB_IncomeGroup),"Other LICs",
                                     ifelse(grepl("Lower middle income", WB_IncomeGroup),"Other LMICs",
                                            ifelse(grepl("Upper middle income", WB_IncomeGroup),"Other UMICs",
                                                   "Other HICs")))) 
#======================



#Figure: Global fossil fuel Subsidy trend
#===========================
#Share of GDP
Subsidy_2022 <- Subsidy_Clean %>% filter(year %in% 2022) %>% 
  mutate(TotExpSub = Coal_Exp_Cons+Coal_Exp_Prod+Gas_Exp_Cons+Gas_Exp_Prod+
           Petro_Exp_Cons+Petro_Exp_Prod+Elec_Exp_Cons+Elec_Exp_Prod,
         TotImpSub = Petro_Imp+Coal_Imp+Gas_Imp+Elec_Imp) %>% 
  filter(countryname %in% Reg_corr$IMF_reg) %>% 
  select(countryname,countrycode,year,GDP,TotExpSub,TotImpSub) %>% 
  mutate(TotSub = TotExpSub+TotImpSub,
         Exp_gdpshare = TotExpSub/GDP/1000,
         Imp_gdpshare = TotImpSub/GDP/1000,
         Tot_gdpshare = (TotExpSub+TotImpSub)/GDP/1000)
  
Subsidy_2022 %>% 
  filter(!countrycode %in% targetISO) %>% 
  select(-c(countrycode,year)) %>% 
  left_join(Reg_corr %>% select(IMF_reg,WB_IncomeGroup), by = c("countryname" = "IMF_reg")) %>% 
  group_by(WB_IncomeGroup) %>% 
  summarise(GDP = sum(GDP),
            TotExpSub = sum (TotExpSub),
            TotImpSub = sum (TotImpSub),
            TotSub = sum (TotSub)) %>% 
  mutate(Exp_gdpshare = TotExpSub/GDP/1000,
         Imp_gdpshare = TotImpSub/GDP/1000,
         Tot_gdpshare = (TotExpSub+TotImpSub)/GDP/1000) -> Subsidy_agg

Subsidy_2022 %>% 
  filter(countrycode %in% targetISO) %>% 
  select(countryname,GDP,TotExpSub,TotImpSub,TotSub,Exp_gdpshare,Imp_gdpshare,Tot_gdpshare) -> Subsidy_2022_clean

Subsidy_2022_clean %>% bind_rows(setNames(Subsidy_agg,names(Subsidy_2022_clean))) %>% 
  mutate(countryname = factor(countryname, levels = rev(order.reg))) -> Subsidy_2022_clean

write.csv(Subsidy_2022_clean, str_c(pathout5,"/", "Subsidy to GDP_2022",".csv"))


scheme <-   theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = c(.85,.5),legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(20,"pt"),legend.key.width = unit(15,"pt"),
        axis.title = element_text(size = 12,face = "bold"))

Fig.1c <- Subsidy_2022_clean %>% 
  ggplot()+
  geom_col(aes(countryname,Tot_gdpshare*100,fill = TotSub/1000))+
  coord_flip()+
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(y = "Subsidies as a share of GDP", x = NULL,fill = "Amount\n(billion $)",title = "Total")+
  scheme


Fig.1a <- Subsidy_2022_clean %>% 
  ggplot()+
  geom_col(aes(countryname,Exp_gdpshare*100,fill = TotExpSub/1000))+
  coord_flip()+
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(y = "Subsidies as a share of GDP", x = NULL,fill = "Amount\n(billion $)",title = "Explicit")+
  scheme


Fig.1b <- Subsidy_2022_clean %>% 
  ggplot()+
  geom_col(aes(countryname,Imp_gdpshare*100,fill = TotImpSub/1000))+
  coord_flip()+
  scale_fill_viridis_c(option = "plasma", direction = -1) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  labs(y = "Subsidies as a share of GDP", x = NULL,fill = "Amount\n(billion $)",title = "Implicit")+
  scheme

ggarrange(Fig.1a, Fig.1b, Fig.1c, ncol = 3, 
          labels = c("(a)", "(b)", "(c)"), font.label = list(size = 12, face = "bold"))
# ggsave(str_c(pathout5,"/", "FigS1_Fossil fuel subsidies as a share of GDP",".jpg"),
#        width = 12,height = 6,dpi = 500)


#Subsidy distribution
Subsidy_Clean %>% 
  filter(!countrycode %in% targetISO) %>% 
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP)) %>% 
  filter(countryname %in% Reg_corr$IMF_reg) %>% 
  left_join(Reg_corr %>% select(IMF_reg,WB_IncomeGroup), by = c("countryname" = "IMF_reg")) %>% 
  pivot_longer(-c(year,countryname,WB_IncomeGroup),values_to = "Value",names_to = "Class") %>% 
  mutate(Group = ifelse(grepl("Imp", Class),"Implicit subsidy",
                        ifelse(grepl("Cons", Class),"Explicit subsidy (Consumer)","Explicit subsidy (Producer)"))) %>% 
  group_by(year,WB_IncomeGroup,Group) %>% summarise(TotalSub = sum(Value)) -> Subsidy_Clean_agg

Subsidy_Clean %>% 
  filter(countrycode %in% targetISO) %>%
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP)) %>% 
  pivot_longer(-c(year,countryname),values_to = "Value",names_to = "Class") %>% 
  mutate(Group = ifelse(grepl("Imp", Class),"Implicit subsidy",
                        ifelse(grepl("Cons", Class),"Explicit subsidy (Consumer)","Explicit subsidy (Producer)"))) %>% 
  group_by(year,countryname,Group) %>% summarise(TotalSub = sum(Value)) -> Data1

Data1 %>% bind_rows(setNames(Subsidy_Clean_agg,names(Data1))) %>% 
  mutate(countryname = factor(countryname, levels = order.reg)) -> Data1

write.csv(Data1, str_c(pathout5,"/", "Fossil fuel subsidy by country_trend",".csv"))

scheme <- theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.title = element_text(size = 14,face = "bold"))

Data1 %>% ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,3)],
                      breaks = c("Explicit subsidy (Consumer)","Explicit subsidy (Producer)","Implicit subsidy"))+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend_imp and exp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)


Data1 %>% filter(Group %in% c("Implicit subsidy")) %>% 
  ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,3)],
                    breaks = c("Explicit subsidy (Consumer)","Explicit subsidy (Producer)","Implicit subsidy"))+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend_imp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)


Data1 %>% filter(Group %in% c("Explicit subsidy (Consumer)","Explicit subsidy (Producer)")) %>% 
  ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,3)],
                    breaks = c("Explicit subsidy (Consumer)","Explicit subsidy (Producer)","Implicit subsidy"))+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend_exp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)


#Explicit and implicit
Subsidy_Clean %>% 
  filter(!countrycode %in% targetISO) %>% 
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  filter(countryname %in% Reg_corr$IMF_reg) %>% 
  left_join(Reg_corr %>% select(IMF_reg,WB_IncomeGroup), by = c("countryname" = "IMF_reg")) %>% 
  pivot_longer(-c(year,countryname,WB_IncomeGroup),values_to = "Value",names_to = "Class") %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity"))))  %>% 
  group_by(year,WB_IncomeGroup,Group) %>% summarise(TotalSub = sum(Value)) -> Subsidy_Clean_agg

Subsidy_Clean %>% 
  filter(countrycode %in% targetISO) %>%
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  pivot_longer(-c(year,countryname),values_to = "Value",names_to = "Class") %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity")))) %>% 
  group_by(year,countryname,Group) %>% summarise(TotalSub = sum(Value)) -> Data1

Data1 %>% bind_rows(setNames(Subsidy_Clean_agg,names(Data1))) %>% 
  mutate(countryname = factor(countryname, levels = order.reg)) -> Data1

Data1 %>% ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,4,3)])+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_imp and exp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)
# write.csv(Data1, str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_imp and exp",".csv"))


#Explicit
Subsidy_Clean %>% 
  filter(!countrycode %in% targetISO) %>% 
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  filter(countryname %in% Reg_corr$IMF_reg) %>% 
  left_join(Reg_corr %>% select(IMF_reg,WB_IncomeGroup), by = c("countryname" = "IMF_reg")) %>% 
  pivot_longer(-c(year,countryname,WB_IncomeGroup),values_to = "Value",names_to = "Class") %>% 
  filter(!str_detect(Class, "Imp")) %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity"))))  %>% 
  group_by(year,WB_IncomeGroup,Group) %>% summarise(TotalSub = sum(Value)) -> Subsidy_Clean_agg

Subsidy_Clean %>% 
  filter(countrycode %in% targetISO) %>%
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  pivot_longer(-c(year,countryname),values_to = "Value",names_to = "Class") %>% 
  filter(!str_detect(Class, "Imp")) %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity")))) %>% 
  group_by(year,countryname,Group) %>% summarise(TotalSub = sum(Value)) -> Data1

Data1 %>% bind_rows(setNames(Subsidy_Clean_agg,names(Data1))) %>% 
  mutate(countryname = factor(countryname, levels = order.reg)) -> Data1

Data1 %>% ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,4,3)])+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_exp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)
# write.csv(Data1, str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_exp",".csv"))


#Implicit
Subsidy_Clean %>% 
  filter(!countrycode %in% targetISO) %>% 
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  filter(countryname %in% Reg_corr$IMF_reg) %>% 
  left_join(Reg_corr %>% select(IMF_reg,WB_IncomeGroup), by = c("countryname" = "IMF_reg")) %>% 
  pivot_longer(-c(year,countryname,WB_IncomeGroup),values_to = "Value",names_to = "Class") %>% 
  filter(str_detect(Class, "Imp")) %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity"))))  %>% 
  group_by(year,WB_IncomeGroup,Group) %>% summarise(TotalSub = sum(Value)) -> Subsidy_Clean_agg

Subsidy_Clean %>% 
  filter(countrycode %in% targetISO) %>%
  select(-c(countrycode,mit.gdp.pre.lvl.1,GDP, GDPshare)) %>% 
  pivot_longer(-c(year,countryname),values_to = "Value",names_to = "Class") %>% 
  filter(str_detect(Class, "Imp")) %>% 
  mutate(Group = ifelse(grepl("Petro", Class),"Petroleum",
                        ifelse(grepl("Gas", Class),"Natural gas",
                               ifelse(grepl("Coal", Class),"Coal","Fossil-based electricity")))) %>% 
  group_by(year,countryname,Group) %>% summarise(TotalSub = sum(Value)) -> Data1

Data1 %>% bind_rows(setNames(Subsidy_Clean_agg,names(Data1))) %>% 
  mutate(countryname = factor(countryname, levels = order.reg)) -> Data1

Data1 %>% ggplot()+
  geom_bar(aes(year,TotalSub/1000, fill = Group),stat = "identity", position = "stack")+
  geom_vline(xintercept = 2022.5, linetype = "dotted", color =  "blue")+
  labs(x = NULL,y = "Fossil fuel subsidies (billion USD, 2017constant price)", fill = NULL)+
  facet_wrap(.~countryname,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,5,4,3)])+
  scheme
# ggsave(str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_imp",".jpg"),
#        width = 10,height = 6.5,dpi = 500)
# write.csv(Data1, str_c(pathout5,"/", "Fossil fuel subsidy by country_trend by sector_imp",".csv"))
#===========================



#Figure: The undesirable benefits distribution
#===========================
Subsidy_cost_decile %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup), by = c("Country" = "WBGDPreg")) %>% 
  mutate(ReGroup = ifelse(Group %in% c("D1","D2","D3","D4","D5"), "Bottom 50%",
                          ifelse(Group %in% c("D6","D7","D8","D9"),"Next 40%", "Top 10%")))   %>%  
  group_by(WB_IncomeGroup,ReGroup,SubScenario) %>% summarise(Amount = sum(value)/10^6) %>% #Million $
  group_by(WB_IncomeGroup,SubScenario) %>% mutate(Share = Amount/sum(Amount)) -> Subsidy_cost_decile_agg


Subsidy_cost_decile %>% filter(Country %in% targetcounty) %>% #$
  mutate(ReGroup = ifelse(Group %in% c("D1","D2","D3","D4","D5"), "Bottom 50%",
                          ifelse(Group %in% c("D6","D7","D8","D9"),"Next 40%", "Top 10%")))   %>%  
  group_by(Country,ReGroup,SubScenario) %>% summarise(Amount = sum(value)/10^6) %>% #Million $
  group_by(Country,SubScenario) %>% mutate(Share = Amount/sum(Amount)) -> Data2

Data2 %>% bind_rows(setNames(Subsidy_cost_decile_agg,names(Data2))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data2

# write.csv(Data2,str_c(pathout5,"/", "Undesirable benefits distribution",".csv"))

Data2 %>% 
  filter(SubScenario %in% "Explicit") %>% 
  mutate(ReGroup = factor(ReGroup,level = c("Top 10%","Next 40%","Bottom 50%"))) %>% 
  ggplot()+
  geom_bar(aes(Country,Share*100,fill = ReGroup),stat = "identity", position = "stack")+
  geom_text(aes(Country,Share*100,fill = ReGroup,label = str_c(round(Share*100,0),"%")),                    
            position = position_stack(vjust = 0.4), 
            color = "black",   size = 3) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,3,2)])+
  labs(fill = "Income group", x = NULL, 
       y = "Proportion of benefits from\nfossil fuel subsidies")+
  scale_y_continuous(breaks = seq(0,100,20),labels = str_c( seq(0,100,20), "%"),expand = c(0,0))+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))
# ggsave(str_c(pathout5,"/", "Undesirable benefits distribution_exp",".jpg"),
#        width = 7,height = 5,dpi = 500)

scheme <- theme_test(base_line_size = 1,base_size = 10)+
  theme(legend.position = "top",legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -325, vjust = 1,hjust=1),
        axis.title = element_text(size = 12,face = "bold"))

Fig.5a_impcons <- Data2 %>% 
  filter(SubScenario %in% "Implicit_cons") %>% 
  mutate(ReGroup = factor(ReGroup,level = c("Top 10%","Next 40%","Bottom 50%"))) %>% 
  ggplot()+
  geom_bar(aes(Country,Share*100,fill = ReGroup),stat = "identity", position = "stack")+
  geom_text(aes(Country,Share*100,fill = ReGroup,label = str_c(round(Share*100,0),"%")),                    
            position = position_stack(vjust = 0.4), 
            color = "black",   size = 2.5) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,3,2)])+
  labs(fill = "Income group", x = NULL, 
       y = "Proportion of benefits from\nfossil fuel subsidies")+
  scale_y_continuous(breaks = seq(0,100,20),labels = str_c( seq(0,100,20), "%"),expand = c(0,0))+
  scheme

Fig.5a_impprod <- Data2 %>% 
  filter(SubScenario %in% "Implicit_prod") %>%
  mutate(ReGroup = factor(ReGroup,level = c("Top 10%","Next 40%","Bottom 50%"))) %>% 
  ggplot()+
  geom_bar(aes(Country,Share*100,fill = ReGroup),stat = "identity", position = "stack")+
  geom_text(aes(Country,Share*100,fill = ReGroup,label = str_c(round(Share*100,0),"%")),                    
            position = position_stack(vjust = 0.4), 
            color = "black",   size = 2.5) +
  scale_fill_manual(values = wes_palette("Rushmore1", n = 5)[c(1,3,2)])+
  labs(fill = "Income group", x = NULL, 
       y = "Proportion of benefits from\nfossil fuel subsidies")+
  scale_y_continuous(breaks = seq(0,100,20),labels = str_c( seq(0,100,20), "%"),expand = c(0,0))+
  scheme
#===========================



#Figure: The impacts of subsidy removal  #Incidence rate by decile
#=========================== 
Data_Unevenburden %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(Group,WB_IncomeGroup,SubScenario,Vari) %>% 
  summarise(value = weighted.mean(value,Pop_2017_WB)) -> Data_Unevenburden_agg# weighted mean based on population

Data_Unevenburden %>% filter(Country %in% targetcounty) %>% 
  select(Group,Country,SubScenario,Vari,value) -> Data3

Data3 %>% bind_rows(setNames(Data_Unevenburden_agg,names(Data3))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data3
# write.csv(Data3,
#           str_c(pathout5,"/", "Incidence rate by decile_Total,Income and price effects",".csv"))

#Explicit subsidies
Fig.2a <- Data3 %>% filter(SubScenario %in% "Explicit") %>%
  filter(!Vari %in% "Total_Effect") %>% 
  ggplot()+
  geom_bar(aes(Group,value*100,group = Vari,fill = Vari),stat = "identity", alpha=.7, size=0.8)+
  facet_wrap(.~Country,ncol = 4,scales = "free_y") +
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_fill_manual(values = wes_palette("Royal1", n = 4)[c(2,4)],
                    labels = c("Income_Effect" = "Income Effect", 
                               "Price_Effect" = "Price Effect"))+
  scale_x_discrete(limits = DEC_name)+
  labs(y = "Impacts on household expenditure",x = "Income decile", fill = NULL)+
  theme_test(base_line_size = 1,base_size = 10)+
  theme(legend.position = "top",legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -270, vjust = .5,hjust=1),
        axis.title = element_text(size = 12,face = "bold"))


scheme <- theme_test(base_line_size = 1,base_size = 10)+
  theme(legend.position = "top",legend.title = element_text(face = "bold"),
        legend.background = element_blank(),legend.text = element_text(size = 13),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -270, vjust = .5,hjust=1),
        axis.title = element_text(size = 12,face = "bold"))
#Implicit subsidies
Fig.5b_impcons <-  Data3 %>% 
  filter(SubScenario %in% "Implicit_cons") %>%
  filter(!Vari %in% "Total_Effect") %>% 
  ggplot()+
  geom_bar(aes(Group,value*100,group = Vari,fill = Vari),stat = "identity", alpha=.7, size=0.8)+
  facet_wrap(.~Country,ncol = 6,scales = "free_y") +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)[c(2,4)],
                    labels = c("Income_Effect" = "Income Effect", 
                               "Price_Effect" = "Price Effect"))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = DEC_name)+
  labs(y = "Impacts on household expenditure",x = "Income decile", fill = NULL)+
  scheme


Fig.5b_impprod <-  Data3 %>% 
  filter(SubScenario %in% "Implicit_prod") %>%
  filter(!Vari %in% "Total_Effect") %>% 
  ggplot()+
  geom_bar(aes(Group,value*100,group = Vari,fill = Vari),stat = "identity", alpha=.7, size=0.8)+
  facet_wrap(.~Country,ncol = 6,scales = "free_y",nrow = 4) +
  scale_fill_manual(values = wes_palette("Royal1", n = 4)[c(2,4)],
                    labels = c("Income_Effect" = "Income Effect", 
                               "Price_Effect" = "Price Effect"))+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_discrete(limits = DEC_name)+
  labs(y = "Impacts on household expenditure",x = "Income decile", fill = NULL)+
  scheme
#==========================



#Figure: The impacts of subsidy removal #Ineq, CO2, and poverty outcome
#==========================
#Explicit
Data4 <- as.data.frame(OUT_SUBSP[,,1,1])#Explicit
Data4$Country <- rownames(Data4)

Data4 %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100) -> Data4_agg

Data4 %>% filter(Country %in% targetcounty) %>% 
  group_by(Country) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
                 npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
                 Gini_chg = Gini_sub - Gini_ori,
                 Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
                 CO2_chg = CO2_sub - CO2_ori,
                 CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100)  -> Data4

Data4 %>% bind_rows(setNames(Data4_agg,names(Data4))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data4

scheme  <-  theme_test(base_line_size = 1,base_size = 10)+
  theme(legend.position = c(0.85,0.8),legend.title = element_text(face = "bold"),
        legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.title = element_text(size = 12,face = "bold"))

Fig.2b <- Data4 %>% ggplot()+
  geom_point(aes(npl_chgrate,CO2_chgrate,colour = Gini_chg),size = 5, alpha = 0.7)+
  geom_text_repel(aes(npl_chgrate,CO2_chgrate,label = Country),size = 3,force = 5,max.overlaps = Inf)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_gradient2(high = wes_palette("Royal1", n = 4)[2], mid = "grey77",
                        low = "orange", midpoint = 0) +
  labs(y = "Changes rate of carbon emissions", x = "Changes rate of national poverty headcount", 
       colour  = "Changes in Gini\n coefficient")+
  guides(color = guide_colorbar(barwidth = 1, barheight = 5))+
  scheme
  
# write.csv(Data4, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_exp",".csv"))

ggarrange(Fig.2a, Fig.2b, ncol = 2, widths = c(2.5, 2),  
          labels = c("(a)", "(b)"), font.label = list(size = 12, face = "bold"))
# ggsave(str_c(pathout5,"/", "Fig2_Impact of subsidies removal_exp",".jpg"),
#        width = 12,height = 12*2/4.5,dpi = 500)


#Implicit_cons
Data4 <- as.data.frame(OUT_SUBSP[,,3,1])#Implicit_cons
Data4$Country <- rownames(Data4)

Data4 %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100) -> Data4_agg

Data4 %>% filter(Country %in% targetcounty) %>% 
  group_by(Country) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
            npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
            Gini_chg = Gini_sub - Gini_ori,
            Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
            CO2_chg = CO2_sub - CO2_ori,
            CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100)  -> Data4

Data4 %>% bind_rows(setNames(Data4_agg,names(Data4))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data4


Fig.5c_impcons <-  Data4 %>% ggplot()+
  geom_point(aes(npl_chgrate,CO2_chgrate,colour = Gini_chg),size = 5, alpha = 0.7)+
  geom_text_repel(aes(npl_chgrate,CO2_chgrate,label = Country),size = 3,force = 5,max.overlaps = Inf)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_gradient2(high = wes_palette("Royal1", n = 4)[2], mid = "grey77",
                        low = "orange", midpoint = 0) +
  labs(y = "Changes rate of carbon emissions", x = "Changes rate of national poverty headcount", 
       colour  = "Changes in Gini\n coefficient")+
  guides(color = guide_colorbar(barwidth = 1, barheight = 5))+
  scheme

# write.csv(Data4, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_ImpCons",".csv"))



#Implicit_prod
Data4 <- as.data.frame(OUT_SUBSP[,,2,1])#Implicit_prod
Data4$Country <- rownames(Data4)

Data4 %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100) -> Data4_agg

Data4 %>% filter(Country %in% targetcounty) %>% 
  group_by(Country) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
            npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
            Gini_chg = Gini_sub - Gini_ori,
            Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
            CO2_chg = CO2_sub - CO2_ori,
            CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100)  -> Data4

Data4 %>% bind_rows(setNames(Data4_agg,names(Data4))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data4


Fig.5c_impprod <-  Data4 %>% ggplot()+
  geom_point(aes(npl_chgrate,CO2_chgrate,colour = Gini_chg),size = 5, alpha = 0.7)+
  geom_text_repel(aes(npl_chgrate,CO2_chgrate,label = Country),size = 3,force = 5,max.overlaps = Inf)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  scale_color_gradient2(high = wes_palette("Royal1", n = 4)[2], mid = "grey77",
                        low = "orange", midpoint = 0) +
  labs(y = "Changes rate of carbon emissions", x = "Changes rate of national poverty headcount", 
       colour  = "Changes in Gini\n coefficient")+
  guides(color = guide_colorbar(barwidth = 1, barheight = 5))+
  scheme

# write.csv(Data4, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_ImpProd",".csv"))
#===========================



#New income compensation policy based on social assistance
#===========================
scheme <- theme_test(base_line_size = 1,base_size = 11)+
  theme(legend.position = "top",legend.title = element_text(size = 14,face = "bold"),
        legend.box = "vertical",
        legend.text = element_text(size = 11),legend.background = element_blank(),
        legend.key.height = unit(8,"pt"),legend.key.width = unit(45,"pt"),
        legend.spacing.y = unit(10,"pt"),
        axis.title = element_text(size = 16,face = "bold"))

#Ineq, CO2, and poverty outcome
Data6 <- OUT_SUBSP[,,1,]#Explicit

INT <- Data6
dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
colnames(INT) <- str_c(rep(dimnames(Data6)[[2]],dim(Data6)[3]),
                       rep(dimnames(Data6)[[3]], each = dim(Data6)[2]),sep = "::")

INT <- as.data.frame(INT)
INT$Country <- rownames(Data6)

INT %>% pivot_longer(-Country, names_to = "Vari") %>% 
  separate(col = Vari, "::", into = c("Variable","SPScenario")) %>% 
  pivot_wider(id_cols = c(Country,SPScenario),names_from = Variable, values_from = value) -> INT

INT %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup,SPScenario) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100,
            Ineq_chg = weighted.mean(IneqRat_sub - IneqRat_ori,Pop),#population weighted
            Ineq_chgrate = weighted.mean(IneqRat_sub - IneqRat_ori,Pop)/weighted.mean(IneqRat_ori,Pop)*100
            ) -> Data6_agg

INT %>% 
  filter(Country %in% targetcounty) %>%
  group_by(Country,SPScenario) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
         npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
         Gini_chg = Gini_sub - Gini_ori,
         Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
         CO2_chg = CO2_sub - CO2_ori,
         CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100,
         Ineq_chg = IneqRat_sub - IneqRat_ori,
         Ineq_chgrate = (IneqRat_sub - IneqRat_ori)/IneqRat_ori*100
         ) -> Data6

Data6 %>% bind_rows(setNames(Data6_agg,names(Data6))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data6

Data6 %>% filter(!SPScenario %in% c("Universal","PerfectTargeted")) %>% 
  mutate(SPScenario = factor(SPScenario,levels = order.sp[-5])) %>% 
  ggplot()+
  geom_point(aes(round(CO2_chgrate,0),round(npl_chgrate,0),fill = Gini_chg,shape = SPScenario),size = 5,alpha = 0.8)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  facet_wrap(.~Country,scales = "free",nrow = 4)+
  labs(x = "Changes rate of carbon emissions", y = "Changes rate of national poverty headcount", 
       shape = "Social assistance\nscenarios")+
  scale_x_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_fill_gradient2(name="Changes in Gini coefficient", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "gray77", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0)+
  scale_shape_manual(values = c(21:25),breaks = order.sp[-5],labels = order.spname[-5])+
  guides(shape = guide_legend(nrow = 1))+
  scheme

ggsave(str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_exp 80 percent",".jpg"),
       width = 10,height = 7,dpi = 500)
write.csv(Data6, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_exp 80 percent",".csv"))


Data6 %>% filter(!SPScenario %in% c("Universal","PerfectTargeted")) %>%
  filter(Country %in% c("Belgium","Germany","Finland","Italy",
                        "China","Indonesia","Turkiye","India",
                        "Other HICs","Other LICs","Other LMICs","Other UMICs")) %>% 
  mutate(SPScenario = factor(SPScenario,levels = order.sp[-5])) %>% 
  ggplot()+
  geom_point(aes(round(CO2_chgrate,0),round(npl_chgrate,0),fill = Gini_chg,shape = SPScenario),size = 5,alpha = 0.8)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  facet_wrap(.~Country,scales = "free",nrow = 3)+
  labs(x = "Changes rate of carbon emissions", y = "Changes rate of national poverty headcount", 
       shape = "Social assistance\nscenarios")+
  scale_x_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_fill_gradient2(name="Changes in Gini coefficient", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "gray77", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0)+
  scale_shape_manual(values = c(21:25),breaks = order.sp[-5],labels = order.spname[-5])+
  guides(shape = guide_legend(nrow = 1))+
  scheme

ggsave(str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_exp_for slides 80 percent",".jpg"),
       width = 10,height = 7.5,dpi = 500)



#Implicit_prod
Data6 <- OUT_SUBSP[,,2,]#Implicit_prod

INT <- Data6
dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
colnames(INT) <- str_c(rep(dimnames(Data6)[[2]],dim(Data6)[3]),
                       rep(dimnames(Data6)[[3]], each = dim(Data6)[2]),sep = "::")

INT <- as.data.frame(INT)
INT$Country <- rownames(Data6)

INT %>% pivot_longer(-Country, names_to = "Vari") %>% 
  separate(col = Vari, "::", into = c("Variable","SPScenario")) %>% 
  pivot_wider(id_cols = c(Country,SPScenario),names_from = Variable, values_from = value) -> INT

INT %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup,SPScenario) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100,
            Ineq_chg = weighted.mean(IneqRat_sub - IneqRat_ori,Pop),#population weighted
            Ineq_chgrate = weighted.mean(IneqRat_sub - IneqRat_ori,Pop)/weighted.mean(IneqRat_ori,Pop)*100
  ) -> Data6_agg

INT %>% 
  filter(Country %in% targetcounty) %>%
  group_by(Country,SPScenario) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
            npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
            Gini_chg = Gini_sub - Gini_ori,
            Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
            CO2_chg = CO2_sub - CO2_ori,
            CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100,
            Ineq_chg = IneqRat_sub - IneqRat_ori,
            Ineq_chgrate = (IneqRat_sub - IneqRat_ori)/IneqRat_ori*100
  ) -> Data6

Data6 %>% bind_rows(setNames(Data6_agg,names(Data6))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data6

Data6 %>% filter(!SPScenario %in% c("Universal","PerfectTargeted")) %>% 
  mutate(SPScenario = factor(SPScenario,levels = order.sp[-5])) %>% 
  ggplot()+
  geom_point(aes(round(CO2_chgrate,0),round(npl_chgrate,0),fill = Gini_chg,shape = SPScenario),size = 5,alpha = 0.8)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  facet_wrap(.~Country,scales = "free",nrow = 4)+
  labs(x = "Changes rate of carbon emissions", y = "Changes rate of national poverty headcount", 
       shape = "Social assistance\nscenarios")+
  scale_x_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_fill_gradient2(name="Changes in Gini coefficient", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "gray77", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0)+
  scale_shape_manual(values = c(21:25),breaks = order.sp[-5],labels = order.spname[-5])+
  guides(shape = guide_legend(nrow = 1))+
  scheme

ggsave(str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_ImpProd 80 percent",".jpg"),
       width = 10,height = 7,dpi = 500)
write.csv(Data6, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_ImpProd 80 percent",".csv"))


Data6 %>% filter(!SPScenario %in% c("Universal","PerfectTargeted"))  %>%
  filter(Country %in% c("Belgium","Germany","Finland","Italy",
                        "China","Indonesia","Turkiye","India",
                        "Other HICs","Other LICs","Other LMICs","Other UMICs")) %>% 
  mutate(SPScenario = factor(SPScenario,levels = order.sp[-5])) %>% 
  ggplot()+
  geom_point(aes(round(CO2_chgrate,0),round(npl_chgrate,0),fill = Gini_chg,shape = SPScenario),size = 5,alpha = 0.8)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  facet_wrap(.~Country,scales = "free",nrow = 3)+
  labs(x = "Changes rate of carbon emissions", y = "Changes rate of national poverty headcount", 
       shape = "Social assistance\nscenarios")+
  scale_x_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_fill_gradient2(name="Changes in Gini coefficient", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "gray77", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0)+
  scale_shape_manual(values = c(21:25),breaks = order.sp[-5],labels = order.spname[-5])+
  guides(shape = guide_legend(nrow = 1))+
  scheme

ggsave(str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_ImpProd_for slides 80 percent",".jpg"),
       width = 10,height = 7.5,dpi = 500)


#"Implicit_cons"
Data6 <- OUT_SUBSP[,,3,]#"Implicit_cons"

INT <- Data6
dim(INT) <- c(dim(INT)[1], prod(dim(INT)[2:3]))
colnames(INT) <- str_c(rep(dimnames(Data6)[[2]],dim(Data6)[3]),
                       rep(dimnames(Data6)[[3]], each = dim(Data6)[2]),sep = "::")

INT <- as.data.frame(INT)
INT$Country <- rownames(Data6)

INT %>% pivot_longer(-Country, names_to = "Vari") %>% 
  separate(col = Vari, "::", into = c("Variable","SPScenario")) %>% 
  pivot_wider(id_cols = c(Country,SPScenario),names_from = Variable, values_from = value) -> INT

INT %>% 
  filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  group_by(WB_IncomeGroup,SPScenario) %>% 
  summarise(npl_chg = sum(PPop_npl_sub - PPop_npl_ori),
            npl_chgrate = sum(PPop_npl_sub - PPop_npl_ori)/sum(PPop_npl_ori)*100,
            Gini_chg = weighted.mean(Gini_sub - Gini_ori,Pop),#population weighted
            Gini_chgrate = weighted.mean(Gini_sub - Gini_ori,Pop)/weighted.mean(Gini_ori,Pop)*100,
            CO2_chg = sum(CO2_sub - CO2_ori),
            CO2_chgrate = sum(CO2_sub - CO2_ori)/sum(CO2_ori)*100,
            Ineq_chg = weighted.mean(IneqRat_sub - IneqRat_ori,Pop),#population weighted
            Ineq_chgrate = weighted.mean(IneqRat_sub - IneqRat_ori,Pop)/weighted.mean(IneqRat_ori,Pop)*100
  ) -> Data6_agg

INT %>% 
  filter(Country %in% targetcounty) %>%
  group_by(Country,SPScenario) %>% 
  summarise(npl_chg = PPop_npl_sub - PPop_npl_ori,
            npl_chgrate = (PPop_npl_sub - PPop_npl_ori)/PPop_npl_ori*100,
            Gini_chg = Gini_sub - Gini_ori,
            Gini_chgrate = (Gini_sub - Gini_ori)/Gini_ori*100,
            CO2_chg = CO2_sub - CO2_ori,
            CO2_chgrate = (CO2_sub - CO2_ori)/CO2_ori*100,
            Ineq_chg = IneqRat_sub - IneqRat_ori,
            Ineq_chgrate = (IneqRat_sub - IneqRat_ori)/IneqRat_ori*100
  ) -> Data6

Data6 %>% bind_rows(setNames(Data6_agg,names(Data6))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data6

Data6 %>% filter(!SPScenario %in% c("Universal","PerfectTargeted")) %>% 
  mutate(SPScenario = factor(SPScenario,levels = order.sp[-5])) %>% 
  ggplot()+
  geom_point(aes(round(CO2_chgrate,0),round(npl_chgrate,0),fill = Gini_chg,shape = SPScenario),size = 5,alpha = 0.8)+
  geom_hline(yintercept = 0,color = "blue",linetype = "dotted")+
  geom_vline(xintercept = 0,color = "blue",linetype = "dotted")+
  facet_wrap(.~Country,scales = "free",nrow = 4)+
  labs(x = "Changes rate of carbon emissions", y = "Changes rate of national poverty headcount", 
       shape = "Social assistance\nscenarios")+
  scale_x_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_y_continuous(labels = function(x) paste0(x, "%"),expand = c(0.1,0.1))+
  scale_fill_gradient2(name="Changes in Gini coefficient", 
                       high=brewer.pal(3,"Dark2")[1],
                       mid = "gray77", low=brewer.pal(4,"Dark2")[4],
                       midpoint = 0)+
  scale_shape_manual(values = c(21:25),breaks = order.sp[-5],labels = order.spname[-5])+
  guides(shape = guide_legend(nrow = 1))+
  scheme

ggsave(str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_ImpCons 80 percent",".jpg"),
       width = 10,height = 7,dpi = 500)
write.csv(Data6, str_c(pathout5,"/", "Ineq, CO2, and poverty outcome_SP_Cons 80 percent",".csv"))
#===========================



#Benefit distribution comparison
#===========================
Lorenze <- bind_rows(SP_Benifit_Lorenz, Sub_Benifit_Lorenz)

Lorenze %>% filter(!Country %in% targetcounty) %>% 
  filter(Country %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Country" = "WBGDPreg")) %>% 
  filter(Scenario %in% c("Explicit_Implicit_prod","SP_covid","Cash","SP_current" )) %>% 
  group_by(Group,WB_IncomeGroup,Scenario) %>% 
  summarise(Benefit_cum = weighted.mean(Benefit_cum,Pop_2017_WB),
            Pop_cum = weighted.mean(Pop_cum,Pop_2017_WB)) -> Lorenze_agg
  
Lorenze %>% filter(Country %in% targetcounty) %>% 
  filter(Scenario %in% c("Explicit_Implicit_prod","SP_covid","Cash","SP_current" )) %>% 
  select(Group,Country ,Scenario,Benefit_cum,Pop_cum)-> Data7

Data7 %>% bind_rows(setNames(Lorenze_agg,names(Data7))) %>% 
  mutate(Country = factor(Country, levels = order.reg)) -> Data7

Data7 %>% ggplot()+
  geom_abline(color = "grey")+
  geom_line(aes(Pop_cum,Benefit_cum,group = Scenario,colour = Scenario, linetype = Scenario),
            alpha = 0.6,linewidth = .9)+
  facet_wrap(.~Country,nrow = 4)+
  scale_x_continuous(breaks = seq(0,1,0.2),labels = str_c(seq(0,1,0.2)*100, "%"),expand = c(0,0))+
  scale_y_continuous(breaks = seq(0,1,0.2),labels = str_c(seq(0,1,0.2)*100, "%"),expand = c(0,0))+
  scale_color_manual(values = wes_palette("Rushmore1", n = 5)[c(1,3,5,4)],
                    limits = c("Cash","SP_current","SP_covid","Explicit_Implicit_prod"),
                    labels = c("(S1) Cash-based\nprograms","(S2) Current social\nassistance",
                               "(S3) Social assistance\nCOVID-19 expansion","Fossil fuel subsidies"))+
  scale_linetype_discrete(guide = NULL)+
  labs(y = "Cumulative share of benefit",x = "Cumulative share of population from lowest to highest incomes", color = NULL)+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(legend.position = "top",legend.title = element_text(size = 12,face = "bold"),
        legend.text = element_text(size = 12),legend.background = element_blank(),
        legend.key.height = unit(10,"pt"),legend.key.width = unit(25,"pt"),
        axis.text.x = element_text(angle = -270, vjust = .5,hjust=1),
        axis.title = element_text(size = 14,face = "bold"))

ggsave(str_c(pathout5,"/", "Lorenze curve of benefit",".jpg"),
       width = 10,height = 7.5,dpi = 500)
write.csv(Data7, str_c(pathout5,"/", "Lorenze curve of benefit",".csv"))
#===========================


#SP coverage by decile
#===========================
SPCover_Decile %>% filter(!Reg %in% targetcounty) %>% 
  filter(Reg %in% Reg_corr$WBGDPreg) %>% 
  left_join(Reg_corr %>% select(WBGDPreg,WB_IncomeGroup,Pop_2017_WB), by = c("Reg" = "WBGDPreg")) %>% 
  group_by(Group,WB_IncomeGroup,SPScenario) %>% 
  summarise(value = weighted.mean(value,Pop_2017_WB)) %>% 
  filter(!SPScenario %in% c("Null","Universal","PerfectTargeted")) -> SPCover_Decile_agg

SPCover_Decile %>% filter(Reg %in% targetcounty) %>% 
  filter(!SPScenario %in% c("Null","Universal","PerfectTargeted")) -> Data8

Data8 %>% bind_rows(setNames(SPCover_Decile_agg,names(Data8))) %>% 
  mutate(Reg = factor(Reg, levels = order.reg)) -> Data8

Data8  %>%  ggplot()+
  geom_line(aes(Group, value, group = SPScenario, color = SPScenario),linewidth = 0.3)+
  geom_point(aes(Group, value, color = SPScenario), alpha=.8, size=0.8)+
  facet_wrap(.~Reg, nrow = 4)+
  scale_x_discrete(limits = DEC_name)+
  scale_y_continuous(limits = c(0,100),breaks = seq(0,100,20),labels = str_c(seq(0,100,20),"%"))+
  scale_color_manual(name = "", values = wes_palette("Royal1", n = 4)[c(2,1,4)],
                     breaks = order.sp,
                     labels = order.spname)+
  labs(x = "Income decile", y = "Coverage of social assistance")+
  theme_test(base_line_size = 1,base_size = 12)+
  theme(axis.text.x = element_text(angle = -270, vjust = .5,hjust=1),
        legend.position = "top",legend.key.height = unit(22,"pt"),
        legend.key.width = unit(2,"pt"),
        axis.title = element_text(size = 14,face = "bold"))

ggsave(str_c(pathout5,"/", "Social protection coverage",".jpg"),
       width = 10,height = 7.5,dpi = 500)
write.csv(Data8, str_c(pathout5,"/", "Social protection coverage",".csv"))
#===========================



#Pirce response
#============================
load(str_c(pathout2,"/Subsidy scenarios_price and CO2 response.Rdata"))

GTAPsec <- read.csv(str_c(pathdata4,"/GTAP 11 sector names.csv"))

#Explicit
Price_data <- as.data.frame(Price_Response[1:65,,1]*100-100)#  "Explicit"
colnames(Price_data) <- Reg_Inclu
Price_data$Sec <- secnam

Price_data %>% pivot_longer(-Sec,names_to = "Reg",values_to = "PriceChg") %>%
  left_join(Reg_corr %>% select(GTAP.reg,WBGDPreg,WB_IncomeGroup), by = c("Reg" = "GTAP.reg")) %>% 
  left_join(GTAPsec, by = c("Sec" = "Code")) -> Price_data


Price_data %>% 
  filter(!Reg %in% targetISO) %>% 
  group_by(Sec,Description, WB_IncomeGroup) %>% summarise(PriceChg = mean(PriceChg)) -> Price_agg

Price_data %>% 
  filter(Reg %in% targetISO) %>% 
  select(Sec,Description, WBGDPreg, PriceChg) -> Price_data


Price_data %>% bind_rows(setNames(Price_agg,names(Price_data))) %>% 
  mutate(WBGDPreg = factor(WBGDPreg , levels = order.reg),
         Description = factor(Description, levels = GTAPsec$Description)) -> Price_data


Price_data %>% filter(Sec %in% c("frs", "coa", "oil", "gas", "p_c", "chm", "ely", "gdt", "otp", "wtp", "atp")) %>% 
  ggplot() +
  geom_tile(aes(x = WBGDPreg, y = Description, fill = PriceChg),color = "grey77") + 
  scale_fill_viridis_c(option = "cividis", direction = -1, labels = function(x) paste0("+",x, "%")) +
  geom_text(aes(x = WBGDPreg, y = Description, label = round(PriceChg,1)), size = 3, color = "red")+
  theme_bw(base_line_size = 1,base_size = 12)+
  labs(x = NULL, y = NULL, fill = "Price changes caused by explicit subsidy removal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  theme(legend.position = "top",legend.key.height = unit(10,"pt"),
        legend.key.width = unit(40,"pt"),
        axis.title = element_text(size = 14,face = "bold"))

ggsave(str_c(pathout5,"/", "Price changes_exp",".jpg"),
       width = 10,height = 6,dpi = 500)
write.csv(Price_data, str_c(pathout5,"/", "Price changes_exp",".csv"))
#=============




combined_plot <- ggarrange(ggarrange(Fig.5a_impcons, Fig.5c_impcons, nrow = 1,widths = c(2.4,2),
                    labels = c("(a)", "(b)"), font.label = list(size = 12, face = "bold")), 
  Fig.5b_impcons, nrow = 2,heights = c(2,2.4),labels = c("", "(c)"), font.label = list(size = 12, face = "bold"))

annotate_figure(combined_plot,
                top = text_grob("Impact ofthe removal of implicit subsidies by taxing consumers\n", face = "bold", size = 14))

ggsave(str_c(pathout5,"/", "FigS4_Impact of subsidies removal_ImpCons",".jpg"),
       width = 10,height = 10,dpi = 500)

combined_plot <- ggarrange(ggarrange(Fig.5a_impprod, Fig.5c_impprod, nrow = 1,widths = c(2.4,2),
                    labels = c("(a)", "(b)"), font.label = list(size = 12, face = "bold")), 
          Fig.5b_impprod, nrow = 2,heights = c(2,2.4),labels = c("", "(c)"), font.label = list(size = 12, face = "bold"))
annotate_figure(combined_plot,
                top = text_grob("Impact ofthe removal of implicit subsidies by taxing producers\n", face = "bold", size = 14))


ggsave(str_c(pathout5,"/", "FigS4_Impact of subsidies removal_ImpProd",".jpg"),
       width = 10,height = 10,dpi = 500)


