
# packages ------
library(dplyr)
library(ggplot2)
library(rms)

# data -----
load("1 data prep/working data.RData")


### run multiple imputation ------
#we have missing data in BMI, smoking, and MEDEA
sum(is.na(covid.data$bmi.all_time))/nrow(covid.data)
sum(is.na(covid.data$smoke.all_time))/nrow(covid.data)
sum(is.na(covid.data$medea))/nrow(covid.data)

r.healthy.diagnosis$medea<-droplevels(r.healthy.diagnosis$medea)
r.healthy.hospitalised$medea<-droplevels(r.healthy.hospitalised$medea)
r.diagnosis.hospitalised$medea<-droplevels(r.diagnosis.hospitalised$medea)
r.diagnosis.death$medea<-droplevels(r.diagnosis.death$medea)
r.hospitalised.death$medea<-droplevels(r.hospitalised.death$medea)

# 
print("Getting r.healthy.diagnosis.imp")
start<-Sys.time()
r.healthy.diagnosis.imp<-aregImpute(~
    time+ status+ 
          gender+age+
        bmi.all_time+
           smoke.all_time+ medea+charlson+
           Myocardial_infarction+ Congestive_heart_failure+Peripheral_vascular_disease+
        Cerebrovascular_disease+Dementia+Chronic_pulmonary_disease+
        Rheumatologic_disease+Peptic_ulcer_disease+Mild_liver_disease+
        Diabetes_with_chronic_complications+ Hemoplegia_or_paralegia+Renal_disease+
        Any_malignancy+ Moderate_to_severe_liver_disease+Metastatic_solid_tumor+
        AIDS+ a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
        a_malignant_neoplasm+ a_t2_diabetes, 
           r.healthy.diagnosis,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.healthy.diagnosis.imp"),
     file = "1 data prep/r.healthy.diagnosis.imp.RData")
Sys.time()-start

print("Getting r.healthy.hospitalised.imp")
start<-Sys.time()
r.healthy.hospitalised.imp<-aregImpute(~
    time+ status+ 
          gender+age+
        bmi.all_time+
           smoke.all_time+ medea+charlson+
           Myocardial_infarction+ Congestive_heart_failure+Peripheral_vascular_disease+
        Cerebrovascular_disease+Dementia+Chronic_pulmonary_disease+
        Rheumatologic_disease+Peptic_ulcer_disease+Mild_liver_disease+
        Diabetes_with_chronic_complications+ Hemoplegia_or_paralegia+Renal_disease+
        Any_malignancy+ Moderate_to_severe_liver_disease+Metastatic_solid_tumor+
        AIDS+ a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
        a_malignant_neoplasm+ a_t2_diabetes, 
           r.healthy.hospitalised,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.healthy.hospitalised.imp"),
     file = "1 data prep/r.healthy.hospitalised.imp.RData")
Sys.time()-start

print("Getting r.diagnosis.hospitalised.imp")
start<-Sys.time()
r.diagnosis.hospitalised.imp<-aregImpute(~
    time+ status+ 
          gender+age+
        bmi.all_time+
           smoke.all_time+ medea+charlson+
           Myocardial_infarction+ Congestive_heart_failure+Peripheral_vascular_disease+
        Cerebrovascular_disease+Dementia+Chronic_pulmonary_disease+
        Rheumatologic_disease+Peptic_ulcer_disease+Mild_liver_disease+
        Diabetes_with_chronic_complications+ Hemoplegia_or_paralegia+Renal_disease+
        Any_malignancy+ Moderate_to_severe_liver_disease+Metastatic_solid_tumor+
        AIDS+ a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
        a_malignant_neoplasm+ a_t2_diabetes, 
           r.diagnosis.hospitalised,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.diagnosis.hospitalised.imp"),
     file = "1 data prep/r.diagnosis.hospitalised.imp.RData")
Sys.time()-start

print("Getting r.diagnosis.death.imp")
start<-Sys.time()
r.diagnosis.death.imp<-aregImpute(~
    time+ status+ 
          gender+age+
        bmi.all_time+
           smoke.all_time+ medea+charlson+
           Myocardial_infarction+ Congestive_heart_failure+Peripheral_vascular_disease+
        Cerebrovascular_disease+Dementia+Chronic_pulmonary_disease+
        Rheumatologic_disease+Peptic_ulcer_disease+Mild_liver_disease+
        Diabetes_with_chronic_complications+ Hemoplegia_or_paralegia+Renal_disease+
        Any_malignancy+ Moderate_to_severe_liver_disease+Metastatic_solid_tumor+
        AIDS+ a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
        a_malignant_neoplasm+ a_t2_diabetes, 
           r.diagnosis.death,
           n.impute=5,  
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.diagnosis.death.imp"),
     file = "1 data prep/r.diagnosis.death.imp.RData")
Sys.time()-start

print("Getting r.hospitalised.death.imp")
start<-Sys.time()
r.hospitalised.death.imp<-aregImpute(~
    time+ status+ 
          gender+age+
        bmi.all_time+
           smoke.all_time+ medea+charlson+
           Myocardial_infarction+ Congestive_heart_failure+Peripheral_vascular_disease+
        Cerebrovascular_disease+Dementia+Chronic_pulmonary_disease+
        Rheumatologic_disease+Peptic_ulcer_disease+Mild_liver_disease+
        Diabetes_with_chronic_complications+ Hemoplegia_or_paralegia+Renal_disease+
        Any_malignancy+ Moderate_to_severe_liver_disease+Metastatic_solid_tumor+
        AIDS+ a_autoimmune_condition+
        a_chronic_kidney_disease+a_copd+
        a_dementia+ a_heart_disease+
        a_hyperlipidemia+ a_hypertension+
        a_malignant_neoplasm+ a_t2_diabetes, 
           r.hospitalised.death,
           n.impute=5, 
           B=5, nk=0,
           match='closest',
           type="pmm")
save(list=c("r.hospitalised.death.imp"),
     file = "1 data prep/r.hospitalised.death.imp.RData")
Sys.time()-start























