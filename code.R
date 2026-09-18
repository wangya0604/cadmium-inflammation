#### cancer survivors####
library(foreign)
library(dplyr)

MCQ_1<-read.xport("MCQ.XPT")
str(MCQ_1)
write.csv(MCQ_1,file = "MCQ_1.csv")
MCQ_2<-MCQ_1[,c("SEQN","MCQ220","MCQ230A","MCQ053","MCQ160B","MCQ160C"
                ,"MCQ160D","MCQ160E","MCQ160F","MCQ240O","MCQ240L","MCQ240B")]
####1==: Example/key-step code ####
####2007-2008####
library(foreign)
library(dplyr)
MCQE_1<-read.xport("MCQ_E.XPT")
str(MCQE_1)
write.csv(MCQE_1,file = "MCQE_1.csv")
MCQE_2<-MCQE_1[,c("SEQN","MCQ220","MCQ230A")]
MCQE_2$year<-"2007-2008"
str(MCQE_2)
head(MCQE_2)

MCQE_3<-rename(.data=MCQE_2,
               ca=MCQ220,
               type=MCQ230A)
table(MCQE_3$ca,useNA = "ifany") 
MCQE_4 <- subset(MCQE_3,MCQE_3$ca==1)
write.csv(MCQE_4,file = "MCQE_4.csv")
####Variables####
####2007-2008####
####——CBC####
library(foreign)
library(dplyr)
CBC_E<-read.xport("CBC_E.XPT")
str(L25_B)
CBC_E_1<-CBC_E[,c("SEQN","LBXWBCSI","LBDLYMNO","LBDMONO","LBDNENO","LBXRBCSI",
                  "LBXHGB","LBXPLTSI")]

str(L25_B_2)
CBC_E_2<-rename(.data=CBC_E_1,
                WBC=LBXWBCSI,
                L=LBDLYMNO,
                M=LBDMONO,
                N=LBDNENO,
                RBC=LBXRBCSI,
                HGB=LBXHGB,
                PLT=LBXPLTSI,)
LABDATAE3<-merge(MCQE_4,CBC_E_2,by="SEQN", all.x = T)
####——Cd####
library(foreign)
library(dplyr)

PBCD_E<-read.xport("PBCD_E.XPT")
str(PBCD_E)
PBCD_E_1<-PBCD_E[,c("SEQN","LBDBPBSI","LBDBCDSI","LBDTHGSI")]

str(L06_B_1)
PBCD_E_2<-rename(.data=PBCD_E_1,
                 Pb=LBDBPBSI,
                 Cd=LBDBCDSI,
                 Hg=LBDTHGSI,)
LABDATAE4<-merge(LABDATAE3,PBCD_E_2,by="SEQN", all.x = T)
####——BMI####
library(foreign)
library(dplyr)

BMX_E<-read.xport("BMX_E.XPT")

BMX_E_1<-BMX_E[,c("SEQN","BMXBMI")]

BMX_E_2<-rename(.data=BMX_E_1,
                BMI=BMXBMI,)
LABDATAE5-merge(LABDATAE4,BMX_E_2,by="SEQN", all.x = T)
####——Smoke####
library(foreign)
library(dplyr)
SMQ_E<-read.xport("SMQ_E.XPT")

SMQ_E_1<-SMQ_E[,c("SEQN","SMQ020","SMQ040")]

SMQ_E_2<-rename(.data=SMQ_E_1,
                Smoke_history=SMQ020, 
                Smoke_now=SMQ040,)
table(SMQ_E_2$Smoke_history,useNA = "ifany") 
SMQ_E_2$Smoke_history <- ifelse(is.na(SMQ_E_2$Smoke_history)|SMQ_E_2$Smoke_history%in% c(7, 9),999,SMQ_E_2$Smoke_history)
table(SMQ_E_2$Smoke_history,useNA = "ifany") 
# Smoke_history
# 1	Yes
# 2	No
# 7	Refused	
# 9	Don't know
# .	Missing
table(SMQ_E_2$Smoke_now,useNA = "ifany") 

SMQ_E_2$Smoke_now <- ifelse(SMQ_E_2$Smoke_now %in% c(7, 9) | is.na(SMQ_E_2$Smoke_now), 999, SMQ_E_2$Smoke_now)
table(SMQ_E_2$Smoke_now,useNA = "ifany") 
# smoke_now
# 1	Every day
# 2	Some days
# 3	Not at all
# 7	Refused
# 9	Don't know
# .	Missing

SMQ_E_2$Smoke <- ifelse(SMQ_E_2$Smoke_history == 2, 0,  # Never
                        ifelse(SMQ_E_2$Smoke_history == 1 & SMQ_E_2$Smoke_now == 3, 1,  # Former
                               ifelse((SMQ_E_2$Smoke_history == 1 & SMQ_E_2$Smoke_now %in% c(1, 2)) |
                                        (SMQ_E_2$Smoke_history == 2 & SMQ_E_2$Smoke_now %in% c(1, 2)), 2, 999)))

LABDATAE6<-merge(LABDATAE5,SMQ_E_2,by="SEQN", all.x = T)


####——Alcohol####
library(foreign)
library(dplyr)
alq_e<-read.xport("alq_e.XPT")
alq_e <- alq_e[,c("SEQN","ALQ110","ALQ120Q")]
LABDATAE7<-merge(LABDATAE6,alq_e,by="SEQN", all.x = T)

#####sleepdis####
library(foreign)
library(dplyr)

DPQ_E<-read.xport("DPQ_E.XPT")

DPQ_E_1<-DPQ_E[,c("SEQN","DPQ030")]

DPQ_E_2<-rename(.data=DPQ_E_1,sleepdis=DPQ030)
DPQ_E_2<- DPQ_E_2 %>%
  mutate(across(everything(), ~ ifelse(. %in% c(7, 9, NA), NA, .))) # 将值为 "7"、"9" 或缺失值（NA）的数据替换为 NA，保留其他值。

DPQ_E_3 <- DPQ_E_2 %>%
  mutate(sleep_disorder = ifelse(sleepdis == 0, 0, 
                                 ifelse(sleepdis %in% c(1, 2, 3), 1, 999)))
#0为“0”Not at all，1为“1/2/3”有睡眠障碍
table(DPQ_E_3$sleep_disorder)      

LABDATAE8<-merge(LABDATAE7,DPQ_E_3,by="SEQN", all.x = T)


####——Hypertension####
library(foreign)
library(dplyr)

BPQ_E<-read.xport("BPQ_E.XPT")

BPQ_E_1<-BPQ_E[,c("SEQN","BPQ020")]

BPQ_E_2<-rename(.data=BPQ_E_1,hypter=BPQ020)

LABDATAE13<-merge(LABDATAE12,BPQ_E_2,by="SEQN", all.x = T)


library(foreign)
library(dplyr)

BPQ_1<-BPQ_E[,c("SEQN","BPQ020","BPQ040A","BPQ080","BPQ090D")]
BPQ_2<-rename(.data=BPQ_1,
              hypter=BPQ020,
              Taking_hypter=BPQ040A,
              high_cho=BPQ080,
              Taking_cho=BPQ090D,)
table(BPQ_2$hypter,useNA = "ifany") 
BPQ_2$hypter <- ifelse(BPQ_2$hypter %in% c(7, 9) | is.na(BPQ_2$hypter), 999, BPQ_2$hypter)
table(BPQ_2$hypter,useNA = "ifany") 

table(BPQ_2$Taking_hypter,useNA = "ifany") 
BPQ_2$Taking_hypter <- ifelse(BPQ_2$Taking_hypter %in% c(7, 9) | is.na(BPQ_2$Taking_hypter), 999, BPQ_2$Taking_hypter)
table(BPQ_2$Taking_hypter,useNA = "ifany") 

table(BPQ_2$high_cho,useNA = "ifany") 
BPQ_2$high_cho <- ifelse(BPQ_2$high_cho %in% c(7, 9) | is.na(BPQ_2$high_cho), 999, BPQ_2$high_cho)
table(BPQ_2$high_cho,useNA = "ifany") 

table(BPQ_2$Taking_cho,useNA = "ifany") 
BPQ_2$Taking_cho <- ifelse(BPQ_2$Taking_cho %in% c(7, 9) | is.na(BPQ_2$Taking_cho), 999, BPQ_2$Taking_cho)
table(BPQ_2$Taking_cho,useNA = "ifany") 
J2<-merge(LABDATAE7,BPQ_2,by="SEQN", all.x = T)

#血压测量
library(foreign)
library(dplyr)
bpx_e<-read.xport("bpx_e.XPT")
bpx_1<-bpx_e[,c("SEQN","BPXSY1","BPXSY2","BPXSY3","BPXSY4","BPXDI1","BPXDI2","BPXDI3","BPXDI4")]

####__________SBP ####
bpx_1$SBP1 <- ifelse(is.na(bpx_1$BPXSY1),0,bpx_1$BPXSY1)
bpx_1$SBP2 <- ifelse(is.na(bpx_1$BPXSY2),0,bpx_1$BPXSY2)
bpx_1$SBP3 <- ifelse(is.na(bpx_1$BPXSY3),0,bpx_1$BPXSY3)
bpx_1$SBP4 <- ifelse(is.na(bpx_1$BPXSY4),0,bpx_1$BPXSY4)
#
bpx_1$SBPn1 <- ifelse(is.na(bpx_1$BPXSY1),0,1)
bpx_1$SBPn2 <- ifelse(is.na(bpx_1$BPXSY2),0,1)
bpx_1$SBPn3 <- ifelse(is.na(bpx_1$BPXSY3),0,1)
bpx_1$SBPn4 <- ifelse(is.na(bpx_1$BPXSY4),0,1)
bpx_1$SBPn <- bpx_1$SBPn1 + bpx_1$SBPn2 + bpx_1$SBPn3 + bpx_1$SBPn4
#
bpx_1$SBP <- (bpx_1$SBP1 + bpx_1$SBP2 + bpx_1$SBP3 + bpx_1$SBP4)/bpx_1$SBPn
summary(bpx_1$SBP) #

sbp_temp <- subset(bpx_1,is.na(bpx_1$BPXSY1) & 
                     is.na(bpx_1$BPXSY2) & 
                     is.na(bpx_1$BPXSY3) & 
                     is.na(bpx_1$BPXSY4))
nrow(sbp_temp) #
#
bpx_1 <- subset(bpx_1, select=-c(SBP1,SBP2,SBP3,SBP4,
                                 BPXSY1,BPXSY2,BPXSY3,BPXSY4,
                                 SBPn1,SBPn2,SBPn3,SBPn4,SBPn
))

####__________DBP ####

#
bpx_1$DBP1 <- ifelse(is.na(bpx_1$BPXDI1),0,bpx_1$BPXDI1)
bpx_1$DBP2 <- ifelse(is.na(bpx_1$BPXDI2),0,bpx_1$BPXDI2)
bpx_1$DBP3 <- ifelse(is.na(bpx_1$BPXDI3),0,bpx_1$BPXDI3)
bpx_1$DBP4 <- ifelse(is.na(bpx_1$BPXDI4),0,bpx_1$BPXDI4)
#
bpx_1$DBPn1 <- ifelse(is.na(bpx_1$BPXDI1),0,1)
bpx_1$DBPn2 <- ifelse(is.na(bpx_1$BPXDI2),0,1)
bpx_1$DBPn3 <- ifelse(is.na(bpx_1$BPXDI3),0,1)
bpx_1$DBPn4 <- ifelse(is.na(bpx_1$BPXDI4),0,1)
bpx_1$DBPn <- bpx_1$DBPn1 + bpx_1$DBPn2 + bpx_1$DBPn3 + bpx_1$DBPn4
#
bpx_1$DBP <- (bpx_1$DBP1 + bpx_1$DBP2 + bpx_1$DBP3 + bpx_1$DBP4)/bpx_1$DBPn

summary(bpx_1$DBP) #

dbp_temp <- subset(bpx_1,is.na(bpx_1$BPXDI1) & 
                     is.na(bpx_1$BPXDI2) & 
                     is.na(bpx_1$BPXDI3) & 
                     is.na(bpx_1$BPXDI4))
nrow(dbp_temp) #
bpx_1 <- subset(bpx_1, 
                select=-c(DBP1,DBP2,DBP3,DBP4,
                          BPXDI1,BPXDI2,BPXDI3,BPXDI4,
                          DBPn1,DBPn2,DBPn3,DBPn4,DBPn
                ))

#

J2<-merge(J1,bpx_1,by="SEQN", all.x = T)


####__####
table(J2$hypter,useNA = "ifany")

summary(J2$SBP) #

summary(J2$DBP) #


J2$high_bp <- ifelse(J2$SBP>=140|J2$DBP>=90, 1, 0)

J2$Hypertension <- ifelse(J2$high_bp==1 | J2$hypter== 1| J2$Taking_hypter== 1, 1, 0)

table(J2$Hypertension ,useNA = "ifany")
#
write.csv(J2,file = "J2.csv")

J2$hyptersion <- factor(J2$hyptersion,levels = c(1,2),labels = c("Yes","No"))

####——Diabetes####
library(foreign)
library(dplyr)

diq_e<-read.xport("diq_e.XPT")
diq <- diq_e[,c("SEQN","DIQ010","DIQ050","DID070")]

diq_1 <- rename(.data=diq,Diabetes1=DIQ010,#
                TInsulin=DIQ050,  #
                Sugar=DID070) #
head(diq_1)

LABDATAE24<-merge(J2,diq_1,by="SEQN", all.x = T)

table(LABDATAE24$Diabetes1,useNA = "ifany")
LABDATAE24$Diabetes1 <- ifelse(is.na(LABDATAE24$Diabetes1) | LABDATAE24$Diabetes1 == 9| LABDATAE24$Diabetes1== 3, 999, LABDATAE24$Diabetes1)
table(LABDATAE24$Diabetes1,useNA = "ifany")
#  
table(LABDATAE24$TInsulin,useNA = "ifany")
LABDATAE24$TInsulin <- ifelse(is.na(LABDATAE24$TInsulin) ,
                              999,LABDATAH20$TInsulin)
table(LABDATAI23$TInsulin,useNA = "ifany")
#  
table(LABDATAE24$Sugar,useNA = "ifany") #
LABDATAE24$Sugar <- ifelse(is.na(LABDATAE24$Sugar) | LABDATAE24$Sugar == 9, 999, LABDATAE24$Sugar)
table(LABDATAE24$Sugar,useNA = "ifany")

table(LABDATAE24$GlyHb,useNA = "ifany")

table(LABDATAE24$Glu,useNA = "ifany")
LABDATAE24$diabetes <- ifelse(LABDATAE24$Diabetes1==1 |
                                LABDATAE24$TInsulin==1 | 
                                LABDATAE24$Sugar==1|
                                LABDATAE24$GlyHb>=6.5|
                                LABDATAE24$Glu>=7,1,2)
table(LABDATAE24$diabetes,useNA = "ifany")
write.csv(LABDATAE24,file = "LABDATAE24.csv")


####2==: Analysis code####
#### imputation####
library(foreign)
library(dplyr)
library(survey)
library(arsenal)
library(mitools)
library(mice)
CBC <- read.csv(
  "C:/Desktop/CBC.csv",
  header = TRUE,        
  na.strings = c("NA")  
)

study_design <- svydesign(data=CBC, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)

summary(CBC)

CBC <- CBC %>%
  mutate(
    Smoke = as.factor(Smoke),
    Alcohol = as.factor(Alcohol),
    sleep_disorder = as.factor(sleep_disorder)
  )
summary(CBC)
continuous_vars <- c("PIR","BMI")
ordinal_vars <- c( "Smoke", "Alcohol")
non_imputed_vars <- c("SDMVSTRA", "SDMVPSU","WT")
binary_vars <- c("sleep_disorder") # 
summary(CBC)
md.pattern(CBC)
methods <- make.method(CBC)
methods[continuous_vars] <- "pmm"
methods[non_imputed_vars] <- ""
methods[binary_vars] <- "logreg"      
methods[ordinal_vars] <- "polr"        

pred_matrix <- make.predictorMatrix(CBC)
pred_matrix[, non_imputed_vars] <- 0 

imputed_data <- mice(
  CBC,
  method = methods,
  predictorMatrix = pred_matrix,
  m = 5,
  maxit = 10,
  seed = 123
)

complete_CBC <- complete(imputed_data, action = "all")

temp_dir <- tempdir()


for (i in 1:5) {
  write.csv(
    complete_CBC[[i]],
    file.path(temp_dir, paste0("imputed_CBC_", i, ".csv")),
    row.names = FALSE
  )
}


zip("imputed_CBC.zip", files = list.files(temp_dir, full.names = TRUE))




####DATA####

library(foreign)
library(dplyr)

DATA <- read.csv(
  "C:/Users/wy/Desktop/DATA.csv")
library(survey)
library(arsenal)

options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median####
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value

####Cd_median=3.2####
Cd_median_value <- 3.2

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.2,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")
#### PIV_median#### 
PIV_median <- svyquantile(
  ~PIV,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PIV_median_value <- coef(PIV_median)
PIV_median_value
####PIV_median=276.25 ####
PIV_median_value <- 276.25

DATA$PIV_binary <- ifelse(
  DATA$PIV < PIV_median_value,
  "Low",
  "High"
)

DATA$PIV_binary <- factor(
  DATA$PIV_binary,
  levels = c("Low", "High")
)
table(DATA$PIV_binary, useNA = "ifany")

#### SII_median#### 
SII_median <- svyquantile(
  ~SII,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SII_median_value <- coef(SII_median)
SII_median_value
####SII_median=501.28 ####
SII_median_value <- 501.28

DATA$SII_binary <- ifelse(
  DATA$SII < SII_median_value,
  "Low",
  "High"
)

DATA$SII_binary <- factor(
  DATA$SII_binary,
  levels = c("Low", "High")
)
table(DATA$SII_binary, useNA = "ifany")

#### NLR_median#### 
NLR_median <- svyquantile(
  ~NLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

NLR_median_value <- coef(NLR_median)
NLR_median_value
####NLR_median=2.19 ####
NLR_median_value <- 2.19 

DATA$NLR_binary <- ifelse(
  DATA$NLR < NLR_median_value,
  "Low",
  "High"
)

DATA$NLR_binary <- factor(
  DATA$NLR_binary,
  levels = c("Low", "High")
)
table(DATA$NLR_binary, useNA = "ifany")
#### MLR_median#### 
MLR_median <- svyquantile(
  ~MLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

MLR_median_value <- coef(MLR_median)
MLR_median_value
####MLR_median=0.3 ####
MLR_median_value <- 0.3

DATA$MLR_binary <- ifelse(
  DATA$MLR < MLR_median_value,
  "Low",
  "High"
)

DATA$MLR_binary <- factor(
  DATA$MLR_binary,
  levels = c("Low", "High")
)
table(DATA$MLR_binary, useNA = "ifany")

#### PLR_median#### 
PLR_median <- svyquantile(
  ~PLR,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

PLR_median_value <- coef(PLR_median)
PLR_median_value
####PLR_median=125.6 ####
PLR_median_value <- 125.6

DATA$PLR_binary <- ifelse(
  DATA$PLR < PLR_median_value,
  "Low",
  "High"
)

DATA$PLR_binary <- factor(
  DATA$PLR_binary,
  levels = c("Low", "High")
)
table(DATA$PLR_binary, useNA = "ifany")
#### SIRI_median#### 
SIRI_median <- svyquantile(
  ~SIRI,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

SIRI_median_value <- coef(SIRI_median)
SIRI_median_value
####SIRI_median=1.2####
SIRI_median_value <- 1.2

DATA$SIRI_binary <- ifelse(
  DATA$SIRI < SIRI_median_value,
  "Low",
  "High"
)

DATA$SIRI_binary <- factor(
  DATA$SIRI_binary,
  levels = c("Low", "High")
)
table(DATA$SIRI_binary, useNA = "ifany")

####Table 2####

library(survey)
library(dplyr)
library(tidyr)
library(stringr)

covars_model2 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1"
)

covars_model3 <- c(
  "Gender",
  "Race",
  "Edu",
  "Marital1",
  "Age2",
  "PIR1",
  "BMI1",
  "Smoke",
  "Alcohol",
  "sleepdisorder",
  "Diabetes",
  "Hypertension"
)


extract_cox <- function(model, indicator){
  
  coef_table <- summary(model)$coefficients
  
  ci <- suppressWarnings(confint(model))
  
  
  row_id <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  
  coef_table <- coef_table[row_id, , drop = FALSE]
  ci <- ci[row_id, , drop = FALSE]
  
  result <- data.frame(
    Variable = rownames(coef_table),
    HR = exp(coef_table[, "coef"]),
    Lower95 = exp(ci[, 1]),
    Upper95 = exp(ci[, 2]),
    P = coef_table[, "Pr(>|z|)"],
    stringsAsFactors = FALSE
  )
  
  result
}



get_p_trend <- function(indicator){
  
  trend_var <- paste0(indicator, "_trend")
  
  # Model 1
  formula1 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var
    )
  )
  
  trend_model1 <- svycoxph(
    formula1,
    design = study_design
  )
  
  p1 <- summary(trend_model1)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 2
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
  
  trend_model2 <- svycoxph(
    formula2,
    design = study_design
  )
  
  p2 <- summary(trend_model2)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  # Model 3
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      trend_var,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
  
  trend_model3 <- svycoxph(
    formula3,
    design = study_design
  )
  
  p3 <- summary(trend_model3)$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  data.frame(
    P_trend_Model1 = p1,
    P_trend_Model2 = p2,
    P_trend_Model3 = p3
  )
}



Table2_final

####Table2_final-output####
Indicator     Variable   Model          HR_95CI P_value
CdPIVGroup2...1       CdPIV  CdPIVGroup2 Model 1 1.47 (1.09–1.98)   0.012
CdPIVGroup3...2       CdPIV  CdPIVGroup3 Model 1 1.65 (1.24–2.21)  <0.001
CdPIVGroup4...3       CdPIV  CdPIVGroup4 Model 1 2.38 (1.82–3.12)  <0.001
CdPIVGroup2...4       CdPIV  CdPIVGroup2 Model 2 1.44 (1.03–2.00)   0.030
CdPIVGroup3...5       CdPIV  CdPIVGroup3 Model 2 1.64 (1.22–2.21)   0.001
CdPIVGroup4...6       CdPIV  CdPIVGroup4 Model 2 2.03 (1.53–2.68)  <0.001
CdPIVGroup2...7       CdPIV  CdPIVGroup2 Model 3 1.26 (0.92–1.73)   0.142
CdPIVGroup3...8       CdPIV  CdPIVGroup3 Model 3 1.56 (1.16–2.10)   0.003
CdPIVGroup4...9       CdPIV  CdPIVGroup4 Model 3 1.87 (1.41–2.48)  <0.001
CdPLRGroup2...10      CdPLR  CdPLRGroup2 Model 1 1.03 (0.72–1.46)   0.879
CdPLRGroup3...11      CdPLR  CdPLRGroup3 Model 1 1.35 (1.00–1.81)   0.048
CdPLRGroup4...12      CdPLR  CdPLRGroup4 Model 1 2.10 (1.53–2.89)  <0.001
CdPLRGroup2...13      CdPLR  CdPLRGroup2 Model 2 0.99 (0.70–1.38)   0.933
CdPLRGroup3...14      CdPLR  CdPLRGroup3 Model 2 1.27 (0.96–1.68)   0.100
CdPLRGroup4...15      CdPLR  CdPLRGroup4 Model 2 1.82 (1.31–2.52)  <0.001
CdPLRGroup2...16      CdPLR  CdPLRGroup2 Model 3 0.95 (0.66–1.35)   0.763
CdPLRGroup3...17      CdPLR  CdPLRGroup3 Model 3 1.23 (0.90–1.68)   0.197
CdPLRGroup4...18      CdPLR  CdPLRGroup4 Model 3 1.77 (1.24–2.52)   0.002
CdMLRGroup2...19      CdMLR  CdMLRGroup2 Model 1 2.74 (1.79–4.19)  <0.001
CdMLRGroup3...20      CdMLR  CdMLRGroup3 Model 1 1.78 (1.22–2.58)   0.003
CdMLRGroup4...21      CdMLR  CdMLRGroup4 Model 1 4.50 (3.02–6.72)  <0.001
CdMLRGroup2...22      CdMLR  CdMLRGroup2 Model 2 2.36 (1.57–3.56)  <0.001
CdMLRGroup3...23      CdMLR  CdMLRGroup3 Model 2 1.74 (1.21–2.49)   0.003
CdMLRGroup4...24      CdMLR  CdMLRGroup4 Model 2 3.30 (2.23–4.87)  <0.001
CdMLRGroup2...25      CdMLR  CdMLRGroup2 Model 3 2.28 (1.50–3.45)  <0.001
CdMLRGroup3...26      CdMLR  CdMLRGroup3 Model 3 1.73 (1.19–2.53)   0.004
CdMLRGroup4...27      CdMLR  CdMLRGroup4 Model 3 3.14 (2.11–4.67)  <0.001
CdNLRGroup2...28      CdNLR  CdNLRGroup2 Model 1 1.71 (1.13–2.60)   0.012
CdNLRGroup3...29      CdNLR  CdNLRGroup3 Model 1 1.58 (1.06–2.37)   0.025
CdNLRGroup4...30      CdNLR  CdNLRGroup4 Model 1 3.06 (2.13–4.40)  <0.001
CdNLRGroup2...31      CdNLR  CdNLRGroup2 Model 2 1.47 (0.99–2.19)   0.059
CdNLRGroup3...32      CdNLR  CdNLRGroup3 Model 2 1.43 (0.97–2.12)   0.071
CdNLRGroup4...33      CdNLR  CdNLRGroup4 Model 2 2.43 (1.70–3.47)  <0.001
CdNLRGroup2...34      CdNLR  CdNLRGroup2 Model 3 1.32 (0.87–1.99)   0.191
CdNLRGroup3...35      CdNLR  CdNLRGroup3 Model 3 1.38 (0.92–2.06)   0.118
CdNLRGroup4...36      CdNLR  CdNLRGroup4 Model 3 2.20 (1.50–3.24)  <0.001
CdSIIGroup2...37      CdSII  CdSIIGroup2 Model 1 1.17 (0.85–1.61)   0.333
CdSIIGroup3...38      CdSII  CdSIIGroup3 Model 1 1.48 (1.08–2.02)   0.015
CdSIIGroup4...39      CdSII  CdSIIGroup4 Model 1 2.14 (1.60–2.85)  <0.001
CdSIIGroup2...40      CdSII  CdSIIGroup2 Model 2 1.12 (0.80–1.57)   0.509
CdSIIGroup3...41      CdSII  CdSIIGroup3 Model 2 1.41 (1.02–1.96)   0.040
CdSIIGroup4...42      CdSII  CdSIIGroup4 Model 2 1.83 (1.35–2.50)  <0.001
CdSIIGroup2...43      CdSII  CdSIIGroup2 Model 3 0.99 (0.72–1.37)   0.953
CdSIIGroup3...44      CdSII  CdSIIGroup3 Model 3 1.34 (0.97–1.84)   0.076
CdSIIGroup4...45      CdSII  CdSIIGroup4 Model 3 1.69 (1.24–2.30)  <0.001
CdSIRIGroup2...46    CdSIRI CdSIRIGroup2 Model 1 2.15 (1.47–3.14)  <0.001
CdSIRIGroup3...47    CdSIRI CdSIRIGroup3 Model 1 1.61 (1.20–2.17)   0.002
CdSIRIGroup4...48    CdSIRI CdSIRIGroup4 Model 1 3.40 (2.45–4.72)  <0.001
CdSIRIGroup2...49    CdSIRI CdSIRIGroup2 Model 2 1.91 (1.29–2.81)   0.001
CdSIRIGroup3...50    CdSIRI CdSIRIGroup3 Model 2 1.58 (1.19–2.11)   0.002
CdSIRIGroup4...51    CdSIRI CdSIRIGroup4 Model 2 2.66 (1.90–3.70)  <0.001
CdSIRIGroup2...52    CdSIRI CdSIRIGroup2 Model 3 1.73 (1.17–2.56)   0.006
CdSIRIGroup3...53    CdSIRI CdSIRIGroup3 Model 3 1.57 (1.14–2.16)   0.006
CdSIRIGroup4...54    CdSIRI CdSIRIGroup4 Model 3 2.47 (1.73–3.53)  <0.001
####Schoenfeld test####
library(survival)
library(dplyr)

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)

ph_test_indicator <- function(indicator) {
  
  # Model 1

  
  formula1 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
  
  cox1 <- coxph(
    formula1,
    data = DATA
  )
  
  ph1 <- cox.zph(cox1)
  
  # Model 2
  
  formula2 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
  
  cox2 <- coxph(
    formula2,
    data = DATA
  )
  
  ph2 <- cox.zph(cox2)
  
  # Model 3
  
  formula3 <- as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
  
  cox3 <- coxph(
    formula3,
    data = DATA
  )
  
  ph3 <- cox.zph(cox3)
  
  
  
  list(
    Model1 = ph1,
    Model2 = ph2,
    Model3 = ph3
  )
}


extract_global_ph <- function(ph_object) {
  
  tab <- ph_object$table
  
  global_row <- which(
    rownames(tab) == "GLOBAL"
  )
  
  if (length(global_row) == 0) {
    return(NA_real_)
  }
  
  tab[global_row, "p"]
}


PH_summary <- data.frame()

for (indicator in indicators) {
  
  ph <- ph_test_indicator(indicator)
  
  temp <- data.frame(
    Indicator = indicator,
    Model1_Global_P = extract_global_ph(ph$Model1),
    Model2_Global_P = extract_global_ph(ph$Model2),
    Model3_Global_P = extract_global_ph(ph$Model3)
  )
  
  PH_summary <- bind_rows(
    PH_summary,
    temp
  )
}


PH_summary
#### Schoenfeld test-outputt####
Indicator Model1_Global_P Model2_Global_P Model3_Global_P
1     CdPIV       0.9804032       0.4744239       0.6399042
2     CdPLR       0.4830471       0.2397258       0.3409411
3     CdMLR       0.5975582       0.3094101       0.4909272
4     CdNLR       0.6740822       0.3249545       0.5345441
5     CdSII       0.8261789       0.4243797       0.5928955
6    CdSIRI       0.8352984       0.3778788       0.6135674


####Table S1####
# Table S1
# Outcome: CVD
# Survey-weighted Cox regression#

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

DATA$CdPIV_trend <- as.numeric(
  factor(
    DATA$CdPIV,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdPLR_trend <- as.numeric(
  factor(
    DATA$CdPLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdMLR_trend <- as.numeric(
  factor(
    DATA$CdMLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdNLR_trend <- as.numeric(
  factor(
    DATA$CdNLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSII_trend <- as.numeric(
  factor(
    DATA$CdSII,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSIRI_trend <- as.numeric(
  factor(
    DATA$CdSIRI,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)



study_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = DATA
)



formula_model1 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator
    )
  )
}

formula_model2 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}

formula_model3 <- function(indicator) {
  as.formula(
    paste(
      "Surv(permthint, CVD) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  s <- summary(model)
  
  coef_table <- s$coefficients
  
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[rows, , drop = FALSE]
  
  # confint
  ci <- suppressWarnings(confint(model))
  
  ci2 <- ci[rows, , drop = FALSE]
  
  result <- data.frame(
    Variable = rownames(coef_table2),
    HR = exp(coef_table2[, "coef"]),
    Lower95 = exp(ci2[, 1]),
    Upper95 = exp(ci2[, 2]),
    P = coef_table2[, "Pr(>|z|)"],
    Model = model_name,
    stringsAsFactors = FALSE
  )
  
  result
}
####Model 1–3####
TableS1_Cox <- TableS1_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )
cat("\n==============================\n")
cat("P for trend\n")
cat("==============================\n\n")

print(P_trend_table)

cat("\n==============================\n")
cat("Table S1 Cox results\n")
cat("==============================\n\n")

print(TableS1_Cox)
#### TableS1-output####
Indicator     Variable   Model           HR_95CI P_value
CdPIVGroup2...1       CdPIV  CdPIVGroup2 Model 1  2.11 (0.92–4.85)   0.079
CdPIVGroup3...2       CdPIV  CdPIVGroup3 Model 1  2.07 (0.96–4.47)   0.064
CdPIVGroup4...3       CdPIV  CdPIVGroup4 Model 1  3.51 (1.70–7.27)  <0.001
CdPIVGroup2...4       CdPIV  CdPIVGroup2 Model 2  2.04 (0.86–4.82)   0.105
CdPIVGroup3...5       CdPIV  CdPIVGroup3 Model 2  2.16 (1.00–4.64)   0.050
CdPIVGroup4...6       CdPIV  CdPIVGroup4 Model 2  3.00 (1.46–6.16)   0.003
CdPIVGroup2...7       CdPIV  CdPIVGroup2 Model 3  1.71 (0.73–4.03)   0.217
CdPIVGroup3...8       CdPIV  CdPIVGroup3 Model 3  2.11 (0.94–4.73)   0.070
CdPIVGroup4...9       CdPIV  CdPIVGroup4 Model 3  2.93 (1.31–6.55)   0.009
CdPLRGroup2...10      CdPLR  CdPLRGroup2 Model 1  0.91 (0.46–1.79)   0.788
CdPLRGroup3...11      CdPLR  CdPLRGroup3 Model 1  1.24 (0.67–2.28)   0.496
CdPLRGroup4...12      CdPLR  CdPLRGroup4 Model 1  2.43 (1.43–4.13)   0.001
CdPLRGroup2...13      CdPLR  CdPLRGroup2 Model 2  0.85 (0.43–1.70)   0.654
CdPLRGroup3...14      CdPLR  CdPLRGroup3 Model 2  1.18 (0.66–2.13)   0.570
CdPLRGroup4...15      CdPLR  CdPLRGroup4 Model 2  2.13 (1.22–3.73)   0.008
CdPLRGroup2...16      CdPLR  CdPLRGroup2 Model 3  0.83 (0.43–1.59)   0.571
CdPLRGroup3...17      CdPLR  CdPLRGroup3 Model 3  1.22 (0.65–2.29)   0.542
CdPLRGroup4...18      CdPLR  CdPLRGroup4 Model 3  2.26 (1.24–4.14)   0.008
CdMLRGroup2...19      CdMLR  CdMLRGroup2 Model 1  3.02 (1.32–6.89)   0.009
CdMLRGroup3...20      CdMLR  CdMLRGroup3 Model 1  1.76 (0.97–3.21)   0.063
CdMLRGroup4...21      CdMLR  CdMLRGroup4 Model 1 5.82 (3.13–10.82)  <0.001
CdMLRGroup2...22      CdMLR  CdMLRGroup2 Model 2  2.48 (1.10–5.60)   0.028
CdMLRGroup3...23      CdMLR  CdMLRGroup3 Model 2  1.78 (0.96–3.32)   0.068
CdMLRGroup4...24      CdMLR  CdMLRGroup4 Model 2  4.18 (2.20–7.93)  <0.001
CdMLRGroup2...25      CdMLR  CdMLRGroup2 Model 3  2.26 (0.98–5.20)   0.055
CdMLRGroup3...26      CdMLR  CdMLRGroup3 Model 3  1.84 (0.98–3.46)   0.057
CdMLRGroup4...27      CdMLR  CdMLRGroup4 Model 3  4.11 (2.11–7.99)  <0.001
CdNLRGroup2...28      CdNLR  CdNLRGroup2 Model 1  2.21 (0.94–5.21)   0.071
CdNLRGroup3...29      CdNLR  CdNLRGroup3 Model 1  1.60 (0.71–3.60)   0.256
CdNLRGroup4...30      CdNLR  CdNLRGroup4 Model 1 4.65 (2.12–10.21)  <0.001
CdNLRGroup2...31      CdNLR  CdNLRGroup2 Model 2  1.83 (0.77–4.33)   0.171
CdNLRGroup3...32      CdNLR  CdNLRGroup3 Model 2  1.49 (0.68–3.27)   0.317
CdNLRGroup4...33      CdNLR  CdNLRGroup4 Model 2  3.61 (1.62–8.02)   0.002
CdNLRGroup2...34      CdNLR  CdNLRGroup2 Model 3  1.61 (0.67–3.86)   0.290
CdNLRGroup3...35      CdNLR  CdNLRGroup3 Model 3  1.53 (0.64–3.63)   0.336
CdNLRGroup4...36      CdNLR  CdNLRGroup4 Model 3  3.40 (1.39–8.32)   0.007
CdSIIGroup2...37      CdSII  CdSIIGroup2 Model 1  1.40 (0.72–2.73)   0.327
CdSIIGroup3...38      CdSII  CdSIIGroup3 Model 1  1.56 (0.77–3.16)   0.215
CdSIIGroup4...39      CdSII  CdSIIGroup4 Model 1  2.90 (1.51–5.56)   0.001
CdSIIGroup2...40      CdSII  CdSIIGroup2 Model 2  1.29 (0.65–2.56)   0.460
CdSIIGroup3...41      CdSII  CdSIIGroup3 Model 2  1.55 (0.79–3.05)   0.207
CdSIIGroup4...42      CdSII  CdSIIGroup4 Model 2  2.46 (1.28–4.73)   0.007
CdSIIGroup2...43      CdSII  CdSIIGroup2 Model 3  1.11 (0.58–2.14)   0.747
CdSIIGroup3...44      CdSII  CdSIIGroup3 Model 3  1.52 (0.75–3.08)   0.242
CdSIIGroup4...45      CdSII  CdSIIGroup4 Model 3  2.41 (1.15–5.05)   0.020
CdSIRIGroup2...46    CdSIRI CdSIRIGroup2 Model 1  2.37 (1.03–5.49)   0.044
CdSIRIGroup3...47    CdSIRI CdSIRIGroup3 Model 1  1.70 (0.80–3.60)   0.166
CdSIRIGroup4...48    CdSIRI CdSIRIGroup4 Model 1  4.28 (2.10–8.72)  <0.001
CdSIRIGroup2...49    CdSIRI CdSIRIGroup2 Model 2  1.97 (0.84–4.61)   0.117
CdSIRIGroup3...50    CdSIRI CdSIRIGroup3 Model 2  1.72 (0.80–3.66)   0.163
CdSIRIGroup4...51    CdSIRI CdSIRIGroup4 Model 2  3.26 (1.60–6.64)   0.001
CdSIRIGroup2...52    CdSIRI CdSIRIGroup2 Model 3  1.74 (0.71–4.26)   0.223
CdSIRIGroup3...53    CdSIRI CdSIRIGroup3 Model 3  1.79 (0.76–4.22)   0.184
CdSIRIGroup4...54    CdSIRI CdSIRIGroup4 Model 3  3.18 (1.39–7.25)   0.006
####Table S2####
# Table S2
# Outcome: Cancer mortality (CA)
# Survey-weighted Cox regression

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

DATA$CdPIV_trend <- as.numeric(
  factor(
    DATA$CdPIV,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdPLR_trend <- as.numeric(
  factor(
    DATA$CdPLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdMLR_trend <- as.numeric(
  factor(
    DATA$CdMLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdNLR_trend <- as.numeric(
  factor(
    DATA$CdNLR,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSII_trend <- as.numeric(
  factor(
    DATA$CdSII,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

DATA$CdSIRI_trend <- as.numeric(
  factor(
    DATA$CdSIRI,
    levels = c("Group1", "Group2", "Group3", "Group4")
  )
)

study_design <- svydesign(
  ids = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = DATA
)

formula_model1 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator
    )
  )
}


formula_model2 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}


formula_model3 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, CA) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  coef_table <- summary(model)$coefficients
  
  #
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[
    rows,
    ,
    drop = FALSE
  ]
  
  # 95% CI
  ci <- suppressWarnings(
    confint(model)
  )
  
  ci2 <- ci[
    rows,
    ,
    drop = FALSE
  ]
  
  result <- data.frame(
    
    Variable = rownames(coef_table2),
    
    HR = exp(
      coef_table2[, "coef"]
    ),
    
    Lower95 = exp(
      ci2[, 1]
    ),
    
    Upper95 = exp(
      ci2[, 2]
    ),
    
    P = coef_table2[
      ,
      "Pr(>|z|)"
    ],
    
    Model = model_name,
    
    stringsAsFactors = FALSE
  )
  
  result
}

get_p_trend <- function(indicator) {
  
  trend_var <- paste0(
    indicator,
    "_trend"
  )
  

  
  trend_model1 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var
      )
    ),
    
    design = study_design
  )
  
  p1 <- summary(
    trend_model1
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  

  
  trend_model2 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
      )
    ),
    
    design = study_design
  )
  
  p2 <- summary(
    trend_model2
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  trend_model3 <- svycoxph(
    
    as.formula(
      paste(
        "Surv(permthint, CA) ~",
        trend_var,
        "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
        "+ BMI1 + Smoke + Alcohol + sleepdisorder",
        "+ Diabetes + Hypertension"
      )
    ),
    
    design = study_design
  )
  
  p3 <- summary(
    trend_model3
  )$coefficients[
    trend_var,
    "Pr(>|z|)"
  ]
  
  
  data.frame(
    
    P_trend_Model1 = p1,
    
    P_trend_Model2 = p2,
    
    P_trend_Model3 = p3
    
  )
}

indicators <- c(
  "CdPIV",
  "CdPLR",
  "CdMLR",
  "CdNLR",
  "CdSII",
  "CdSIRI"
)


####Model 1–3####

all_results <- list()


for (indicator in indicators) {
  
  cat(
    "\nRunning:",
    indicator,
    "\n"
  )
  
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  
  # Model 2
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )
  
  
  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )
  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS2_results <- bind_rows(
  all_results
)

TableS2_results <- TableS2_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf("%.3f", P)
    )
    
  )


P_trend_table <- data.frame()


for (indicator in indicators) {
  
  trend <- get_p_trend(
    indicator
  )
  
  temp <- data.frame(
    
    Indicator = indicator,
    
    P_trend_Model1 =
      trend$P_trend_Model1,
    
    P_trend_Model2 =
      trend$P_trend_Model2,
    
    P_trend_Model3 =
      trend$P_trend_Model3
    
  )
  
  P_trend_table <- bind_rows(
    P_trend_table,
    temp
  )
}


P_trend_table <- P_trend_table %>%
  
  mutate(
    
    Model1 = ifelse(
      P_trend_Model1 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model1
      )
    ),
    
    Model2 = ifelse(
      P_trend_Model2 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model2
      )
    ),
    
    Model3 = ifelse(
      P_trend_Model3 < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P_trend_Model3
      )
    )
    
  ) %>%
  
  select(
    Indicator,
    Model1,
    Model2,
    Model3
  )


TableS2_Cox <- TableS2_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )
cat(
  "\n====================================\n"
)

cat(
  "Table S2: P for trend\n"
)

cat(
  "====================================\n\n"
)

print(
  P_trend_table
)
Indicator Model1 Model2 Model3
1     CdPIV  0.155  0.314  0.455
2     CdPLR  0.052  0.102  0.151
3     CdMLR  0.001  0.011  0.016
4     CdNLR  0.027  0.112  0.197
5     CdSII  0.283  0.463  0.650
6    CdSIRI  0.027  0.116  0.186
cat(
  "\n====================================\n"
)

cat(
  "Table S2: Cox regression results\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS2_Cox
)

TableS2_wide <- TableS2_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  ) %>%
  
  pivot_wider(
    names_from = Model,
    values_from = c(
      HR_95CI,
      P_value
    )
  )



TableS2_final <- TableS2_wide %>%
  
  left_join(
    P_trend_table,
    by = "Indicator"
  )

cat(
  "\n====================================\n"
)

cat(
  "FINAL Table S2\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS2_final
)
####TableS2-output####
Indicator Variable   `HR_95CI_Model 1` `HR_95CI_Model 2` `HR_95CI_Model 3` `P_value_Model 1` `P_value_Model 2` `P_value_Model 3` Model1 Model2 Model3
<chr>     <chr>      <chr>             <chr>             <chr>             <chr>             <chr>             <chr>             <chr>  <chr>  <chr> 
  1 CdPIV     CdPIVGrou… 0.87 (0.49–1.53)  0.84 (0.47–1.51)  0.79 (0.44–1.41)  0.627             0.565             0.423             0.155  0.314  0.455 
2 CdPIV     CdPIVGrou… 1.07 (0.61–1.87)  1.11 (0.63–1.95)  1.06 (0.61–1.85)  0.823             0.730             0.839             0.155  0.314  0.455 
3 CdPIV     CdPIVGrou… 1.38 (0.83–2.31)  1.23 (0.72–2.08)  1.13 (0.67–1.90)  0.218             0.450             0.644             0.155  0.314  0.455 
4 CdPLR     CdPLRGrou… 1.22 (0.71–2.09)  1.19 (0.70–2.01)  1.16 (0.67–2.02)  0.479             0.519             0.597             0.052  0.102  0.151 
5 CdPLR     CdPLRGrou… 1.28 (0.73–2.24)  1.25 (0.71–2.21)  1.19 (0.68–2.07)  0.397             0.438             0.547             0.052  0.102  0.151 
6 CdPLR     CdPLRGrou… 1.67 (0.99–2.82)  1.54 (0.91–2.61)  1.49 (0.85–2.61)  0.056             0.106             0.165             0.052  0.102  0.151 
7 CdMLR     CdMLRGrou… 1.91 (1.04–3.53)  1.60 (0.87–2.95)  1.58 (0.84–2.97)  0.038             0.131             0.155             0.001  0.011  0.016 
8 CdMLR     CdMLRGrou… 0.97 (0.53–1.77)  0.97 (0.53–1.80)  0.94 (0.52–1.70)  0.915             0.934             0.841             0.001  0.011  0.016 
9 CdMLR     CdMLRGrou… 2.93 (1.67–5.14)  2.24 (1.26–3.96)  2.13 (1.18–3.84)  <0.001            0.006             0.012             0.001  0.011  0.016 
10 CdNLR     CdNLRGrou… 1.30 (0.69–2.44)  1.09 (0.59–2.02)  1.02 (0.54–1.91)  0.421             0.776             0.961             0.027  0.112  0.197 
11 CdNLR     CdNLRGrou… 1.23 (0.70–2.18)  1.17 (0.67–2.05)  1.11 (0.65–1.87)  0.467             0.587             0.710             0.027  0.112  0.197 
12 CdNLR     CdNLRGrou… 1.81 (1.08–3.03)  1.48 (0.89–2.47)  1.36 (0.80–2.31)  0.024             0.129             0.261             0.027  0.112  0.197 
13 CdSII     CdSIIGrou… 0.78 (0.46–1.34)  0.75 (0.43–1.31)  0.70 (0.40–1.25)  0.374             0.318             0.230             0.283  0.463  0.650 
14 CdSII     CdSIIGrou… 1.12 (0.64–1.96)  1.12 (0.64–1.98)  1.06 (0.61–1.83)  0.680             0.694             0.835             0.283  0.463  0.650 
15 CdSII     CdSIIGrou… 1.21 (0.71–2.04)  1.10 (0.64–1.87)  1.01 (0.59–1.72)  0.483             0.737             0.964             0.283  0.463  0.650 
16 CdSIRI    CdSIRIGro… 1.30 (0.71–2.38)  1.13 (0.61–2.10)  1.05 (0.56–1.99)  0.389             0.704             0.870             0.027  0.116  0.186 
17 CdSIRI    CdSIRIGro… 1.05 (0.64–1.74)  1.08 (0.65–1.78)  1.04 (0.63–1.72)  0.845             0.774             0.877             0.027  0.116  0.186 
18 CdSIRI    CdSIRIGro… 1.91 (1.13–3.25)  1.55 (0.89–2.68)  1.43 (0.81–2.53)  0.016             0.119             0.218             0.027  0.116  0.186 
####Table S3####
# Table S3
# Outcome: All-cause mortality
# Survey-weighted Cox regression
# Indicators: PIV, PLR, MLR, NLR, SII, SIRI, Cd
# Reference: Low

library(survey)
library(dplyr)
library(tidyr)

options(survey.lonely.psu = "adjust")

indicators <- c(
  "PIVm",
  "PLRm",
  "MLRm",
  "NLRm",
  "SIIm",
  "SIRIm",
  "Cdm"
)

for (indicator in indicators) {
  
  study_design$variables[[indicator]] <- relevel(
    factor(study_design$variables[[indicator]]),
    ref = "Low"
  )
}

formula_model1 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator
    )
  )
}


formula_model2 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1"
    )
  )
}


formula_model3 <- function(indicator) {
  
  as.formula(
    paste(
      "Surv(permthint, mortstat) ~",
      indicator,
      "+ Gender + Race + Edu + Marital1 + Age2 + PIR1",
      "+ BMI1 + Smoke + Alcohol + sleepdisorder",
      "+ Diabetes + Hypertension"
    )
  )
}

extract_cox <- function(model, indicator, model_name) {
  
  coef_table <- summary(model)$coefficients
  
  # 
  rows <- grep(
    paste0("^", indicator),
    rownames(coef_table)
  )
  
  coef_table2 <- coef_table[
    rows,
    ,
    drop = FALSE
  ]
  
  # 95% CI
  ci <- suppressWarnings(
    confint(model)
  )
  
  ci2 <- ci[
    rows,
    ,
    drop = FALSE
  ]
  
  result <- data.frame(
    
    Variable = rownames(coef_table2),
    
    HR = exp(
      coef_table2[, "coef"]
    ),
    
    Lower95 = exp(
      ci2[, 1]
    ),
    
    Upper95 = exp(
      ci2[, 2]
    ),
    
    P = coef_table2[
      ,
      "Pr(>|z|)"
    ],
    
    Model = model_name,
    
    stringsAsFactors = FALSE
  )
  
  return(result)
}

all_results <- list()


for (indicator in indicators) {
  
  cat(
    "\n====================================\n"
  )
  
  cat(
    "Running:",
    indicator,
    "\n"
  )
  
  cat(
    "====================================\n"
  )
  
  # Model 1
  model1 <- svycoxph(
    formula_model1(indicator),
    design = study_design
  )
  
  # Model 2
  
  model2 <- svycoxph(
    formula_model2(indicator),
    design = study_design
  )

  # Model 3
  model3 <- svycoxph(
    formula_model3(indicator),
    design = study_design
  )

  
  res1 <- extract_cox(
    model1,
    indicator,
    "Model 1"
  )
  
  res2 <- extract_cox(
    model2,
    indicator,
    "Model 2"
  )
  
  res3 <- extract_cox(
    model3,
    indicator,
    "Model 3"
  )
  
  result <- bind_rows(
    res1,
    res2,
    res3
  )
  
  
  result$Indicator <- indicator
  
  all_results[[indicator]] <- result
}


TableS3_results <- bind_rows(
  all_results
)

TableS3_results <- TableS3_results %>%
  
  mutate(
    
    HR_95CI = sprintf(
      "%.2f (%.2f–%.2f)",
      HR,
      Lower95,
      Upper95
    ),
    
    P_value = ifelse(
      P < 0.001,
      "<0.001",
      sprintf(
        "%.3f",
        P
      )
    )
  )

TableS3_Cox <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  )


cat(
  "\n====================================\n"
)

cat(
  "Table S3: Cox regression results\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_Cox
)


TableS3_wide <- TableS3_results %>%
  
  select(
    Indicator,
    Variable,
    Model,
    HR_95CI,
    P_value
  ) %>%
  
  pivot_wider(
    
    names_from = Model,
    
    values_from = c(
      HR_95CI,
      P_value
    )
  )

TableS3_final <- TableS3_wide

cat(
  "\n====================================\n"
)

cat(
  "FINAL Table S3\n"
)

cat(
  "====================================\n\n"
)

print(
  TableS3_final
)
####TableS3-output####
   Indicator Variable  `HR_95CI_Model 1` `HR_95CI_Model 2` `HR_95CI_Model 3` `P_value_Model 1` `P_value_Model 2` `P_value_Model 3`
<chr>     <chr>     <chr>             <chr>             <chr>             <chr>             <chr>             <chr>            
  1 PIVm      PIVmHigh  1.50 (1.21–1.85)  1.36 (1.12–1.66)  1.24 (1.03–1.51)  <0.001            0.002             0.024            
2 PLRm      PLRmHigh  1.30 (1.03–1.64)  1.23 (0.98–1.54)  1.20 (0.95–1.53)  0.027             0.076             0.126            
3 MLRm      MLRmHigh  2.60 (2.07–3.27)  2.09 (1.67–2.62)  2.00 (1.60–2.50)  <0.001            <0.001            <0.001           
4 NLRm      NLRmHigh  1.82 (1.45–2.27)  1.59 (1.27–2.00)  1.46 (1.16–1.84)  <0.001            <0.001            0.001            
5 SIIm      SIImHigh  1.35 (1.10–1.66)  1.25 (1.00–1.55)  1.15 (0.93–1.43)  0.004             0.046             0.182            
6 SIRIm     SIRImHigh 2.19 (1.74–2.75)  1.83 (1.46–2.29)  1.68 (1.35–2.09)  <0.001            <0.001            <0.001           
7 Cdm       CdmHigh   1.67 (1.39–2.02)  1.55 (1.27–1.88)  1.53 (1.26–1.86)  <0.001            <0.001            <0.001 
#### Table S4 ####
options(survey.lonely.psu = "adjust")
study_design <- svydesign(data=DATA, 
                          id=~SDMVPSU, 
                          strata=~SDMVSTRA, 
                          weights=~WT, nest=TRUE)
Cd_median <- svyquantile(
  ~Cd,
  design = study_design,
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_median_value <- coef(Cd_median)
Cd_median_value
####Cd_median=3.2####
Cd_median_value <- 3.2

DATA$Cd_binary <- ifelse(
  DATA$Cd < Cd_median_value,
  "Low",
  "High"
)

DATA$Cd_binary <- factor(
  DATA$Cd_binary,
  levels = c("Low", "High")
)
# quantile
Cd_Q1_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd < 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

# 
Cd_Q3_new <- svyquantile(
  ~Cd,
  design = subset(study_design, Cd >= 3.2),
  quantiles = 0.50,
  na.rm = TRUE
)

Cd_Q1_new <- as.numeric(coef(Cd_Q1_new))
Cd_Q3_new <- as.numeric(coef(Cd_Q3_new))

Cd_Q1_new
Cd_Q3_new
DATA$Cd_quartile <- cut(
  DATA$Cd,
  breaks = c(
    -Inf,
    Cd_Q1_new,
    3.2,
    Cd_Q3_new,
    Inf
  ),
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,
  right = FALSE
)
table(DATA$Cd_binary, DATA$Cd_quartile, useNA = "ifany")

#Q4
model1 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile , design = study_design)
model2 <- svycoxph(Surv(permthint, mortstat) ~  Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1 , design = study_design)
model3 <- svycoxph(Surv(permthint, mortstat) ~ Cd_quartile+Gender+Race+Edu+Marital1+Age2+PIR1+BMI1+Smoke+Alcohol+sleepdisorder+Diabetes+Hypertension , design = study_design)
summary(model1)
summary(model2)
summary(model3)
####TableS4-output####
n= 2450, number of events= 608 

coef exp(coef) se(coef) robust se      z Pr(>|z|)    
Cd_quartileQ2    0.16157   1.17535  0.16723   0.21903  0.738 0.460729    
Cd_quartileQ3    0.51418   1.67227  0.16019   0.15590  3.298 0.000974 ***
  Cd_quartileQ4    0.54568   1.72578  0.16885   0.15851  3.443 0.000576 ***
  
####Deaths/Participants####
death_table <- DATA %>%
  filter(!is.na(Cd_quartile),
         !is.na(mortstat)) %>%
  group_by(Cd_quartile) %>%
  summarise(
    Participants = n(),
    Deaths = sum(mortstat == 1, na.rm = TRUE),
    Death_percent = Deaths / Participants * 100,
    .groups = "drop"
  )

death_table
####______P for trend####
DATA$Cd_quartile_trend <- as.numeric(DATA$Cd_quartile)

study_design <- update(
  study_design,
  Cd_quartile_trend = DATA$Cd_quartile_trend
)

model1_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend,
  design = study_design
)

summary(model1_trend)

model2_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1,
  design = study_design
)

summary(model2_trend)

model3_trend <- svycoxph(
  Surv(permthint, mortstat) ~ Cd_quartile_trend+Gender + Race + Edu + Marital1 + Age2 + PIR1
  + BMI1 + Smoke + Alcohol + sleepdisorder +
    Diabetes + Hypertension,
  design = study_design
)

summary(model3_trend)
####Table S11####
####1year landmark####
library(survey)
library(survival)


# 
landmark1 <- DATA %>%
  filter(
    mortstat == 0 | permthint > 12
  )


# 
landmark1$time_landmark <- landmark1$permthint - 12


# 
landmark1$death_landmark <- ifelse(
  landmark1$mortstat == 1 &
    landmark1$permthint > 12,
  1,
  0
)


design_landmark1 <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = landmark1
)

#COX1
cox1_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR,
  design = design_landmark1
)
summary(cox1_landmark1)
#
extract_HR <- function(model){
  result <- summary(cox1_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
summary(cox1_landmark1)
extract_HR(cox1_landmark1)
####modle1-output####
              HR        CI        P
CdMLRGroup2 2.75 1.76-4.29 8.70e-06
CdMLRGroup3 1.66 1.11-2.49 1.38e-02
CdMLRGroup4 4.32 2.84-6.58 8.37e-12

cox2_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu,
  design = design_landmark1
)
#
extract_HR <- function(model){
  result <- summary(cox2_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox2_landmark1)
summary(cox2_landmark1)
####modle2-output####
              HR         CI        P
CdMLRGroup2 2.33  1.49-3.65 2.28e-04
CdMLRGroup3 1.68  1.13-2.49 9.87e-03
CdMLRGroup4 3.30  2.16-5.05 3.49e-08
  
cox3_landmark1 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu+
    BMI1+
    Marital1+
    Smoke+
    Alcohol+
    Diabetes+
    Hypertension+
    sleepdisorder,
  design = design_landmark1
)
summary(cox3_landmark1)
#
extract_HR <- function(model){
  result <- summary(cox3_landmark1)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox3_landmark1)
                  HR         CI        P
CdMLRGroup2     2.28  1.47-3.53 2.35e-04
CdMLRGroup3     1.64  1.09-2.46 1.75e-02
CdMLRGroup4     3.05     2-4.65 2.02e-07
####2years ####
landmark2 <- DATA %>%
  filter(mortstat == 0 | permthint > 24) %>%
  mutate(
    time_landmark = permthint - 24,
    death_landmark = ifelse(mortstat == 1 & permthint > 24, 1, 0)
  )

design_landmark2 <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~WT,
  nest = TRUE,
  data = landmark2
)

cox1_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark) ~ CdMLR,
  design = design_landmark2
)
summary(cox1_landmark2)

#
extract_HR <- function(cox1_landmark2){
  result <- summary(cox1_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}

extract_HR(cox1_landmark2)
####modle1-output####
             HR        CI        P
CdMLRGroup2 2.64 1.65-4.21 4.69e-05
CdMLRGroup3 1.60 1.05-2.44 2.71e-02
CdMLRGroup4 4.16 2.71-6.38 6.75e-11

cox2_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu,
  design = design_landmark2
)
summary(cox2_landmark2)
#
extract_HR <- function(cox2_landmark2){
  result <- summary(cox2_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox2_landmark2)
####modle2-output####
               HR        CI        P
CdMLRGroup2  2.24 1.38-3.62 1.00e-03
CdMLRGroup3  1.64  1.1-2.44 1.52e-02
CdMLRGroup4  3.17 2.06-4.87 1.52e-07

cox3_landmark2 <- svycoxph(
  Surv(time_landmark, death_landmark)~
    CdMLR+
    Age2+
    Gender+
    Race+
    PIR1+
    Edu+
    BMI1+
    Marital1+
    Smoke+
    Alcohol+
    Diabetes+
    Hypertension+
    sleepdisorder,
  design = design_landmark2
)
summary(cox3_landmark2)
#
extract_HR <- function(cox3_landmark2){
  result <- summary(cox3_landmark2)$coefficients
  HR <- exp(result[,1])
  CI_low <- exp(result[,1] - 1.96*result[,4])
  CI_high <- exp(result[,1] + 1.96*result[,4])
  P <-result[,6]
  data.frame(
    HR = round(HR,2),
    CI = paste0(
      round(CI_low,2),
      "-",
      round(CI_high,2)
    ),
    P = signif(P,3)
  )
}
extract_HR(cox3_landmark2)

####modle3-output####
                  HR         CI        P
CdMLRGroup2      2.17  1.36-3.46 1.23e-03
CdMLRGroup3      1.56  1.03-2.39 3.79e-02
CdMLRGroup4      2.89  1.89-4.43 1.08e-06

####
library(dplyr)

event_table_1yr <- landmark1 %>%
  group_by(CdMLR) %>%
  summarise(
    Participants = n(),
    Deaths = sum(death_landmark == 1, na.rm = TRUE),
    Death_percent = round(
      Deaths / Participants * 100,
      1
    )
  )

event_table_1yr

CdMLR  Participants Deaths Death_percent
<chr>         <int>  <int>         <dbl>
  1 Group1          561     67          11.9
2 Group2          545    121          22.2
3 Group3          638    127          19.9
4 Group4          648    235          36.3
#2years
event_table_2yr <- landmark2 %>%
  group_by(CdMLR) %>%
  summarise(
    Participants = n(),
    Deaths = sum(death_landmark == 1, na.rm = TRUE),
    Death_percent = round(
      Deaths / Participants * 100,
      1
    )
  )

event_table_2yr

CdMLR  Participants Deaths Death_percent
<chr>         <int>  <int>         <dbl>
  1 Group1          552     58          10.5
2 Group2          529    105          19.8
3 Group3          625    114          18.2
4 Group4          615    202          32.8
####Fig S4-Spearman correlation heatmap####

inflammation_markers <- DATA[, c(
  "PIV",
  "SII",
  "PLR",
  "MLR",
  "NLR",
  "SIRI"
)]
# Spearman correlation matrix

cor_matrix <- cor(
  inflammation_markers,
  method = "spearman",
  use = "complete.obs"
)

####output####
round(cor_matrix, 3)
      PIV   SII   PLR   MLR   NLR  SIRI
PIV  1.000 0.857 0.492 0.630 0.714 0.898
SII  0.857 1.000 0.729 0.471 0.846 0.727
PLR  0.492 0.729 1.000 0.501 0.584 0.356
MLR  0.630 0.471 0.501 1.000 0.637 0.789
NLR  0.714 0.846 0.584 0.637 1.000 0.820
SIRI 0.898 0.727 0.356 0.789 0.820 1.000
library(corrplot)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "original",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.8
)


####RCS-output####
#PIV
Name	Chi_Square	          Df  	Pvalue
PIV	46.2564898719847	      2	  9.02673491509631e-11
Nonlinear	1.06096029434321	1  	0.302996266258382
#PLR	
Name	Chi_Square          	Df	 Pvalue
PLR	20.095347969634	        2	   4.32863166839326e-05
Nonlinear	10.2044049949634	1  	 0.00140105589192152
#MLR
Name	Chi_Square          	Df	Pvalue
MLR	65.2388239253202       	2	   6.77236045021346e-15
Nonlinear	2.82223306959806	1	   0.0929669825644589
#NLR
Name	Chi_Square	          Df	Pvalue
NLR	41.431807225643	        2  	1.00738939412537e-09
Nonlinear	0.95756215787096	1 	0.327801855565601
#SII
Name	Chi_Square	          Df	Pvalue
SII	37.7404506251367	      2	  6.379186179295e-09
Nonlinear	6.62723980226937	1	  0.0100430772280765
#SIRI
Name	Chi_Square	          Df	Pvalue
SIRI	58.8104967240313    	2	  1.69642078162724e-13
Nonlinear	5.45521339960888	1	  0.0195100210552246
