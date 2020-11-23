---
title: 'Descriptive statistics for BMI'
output:
  html_document:
    code_folding: hide
    highlight: tango
    theme: united
    toc: yes
    toc_depth: 4
    toc_float: yes
editor_options:
  chunk_output_type: console
---

```{r options, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE, message=FALSE, comment = FALSE,
                      cache.lazy=FALSE)
options(scipen=999)
options(knitr.kable.NA = '')
```

```{r packages}
# packages -----
library(dplyr)
library(tableone)
library(kableExtra)
library(stringr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(cmprsk)
```

```{r functions}
nice.num<-function(x){
  prettyNum(x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE)}
nice.num2<-function(x){
  prettyNum(x, big.mark=",", nsmall = 2, digits=2,scientific = FALSE)}
```

```{r data, include=FALSE}

load("~/1 data prep/working data.RData")
#put obesity in just one category
levels(covid.data$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                        "overweight" ="overweight",
                                        "obese"=c("obese class I", "obese class II", "obese class III"))

```

## Table 1: Patient characteristics at baseline
Patient characteristics (overall population, and by BMI group)
```{r}

# variables for table 1
vars<-c("bmi.all_time",
        "age",
        "age_gr",
        "gender",
        "smoke.all_time",
        "medea",
        "region",
        "charlson",
        # components of charlson
        "Myocardial_infarction",
        "Congestive_heart_failure",
        "Peripheral_vascular_disease",
        "Cerebrovascular_disease",
        "Dementia",
        "Chronic_pulmonary_disease",
        "Rheumatologic_disease",
        "Peptic_ulcer_disease",
        "Mild_liver_disease",
        "Diabetes_with_chronic_complications",
        "Hemoplegia_or_paralegia",
        "Renal_disease",
        "Any_malignancy",
        "Moderate_to_severe_liver_disease",
        "Metastatic_solid_tumor",
        "AIDS",
        # atlas cohorts
        "a_autoimmune_condition",
        "a_chronic_kidney_disease",
        "a_copd",
        "a_dementia",
        "a_heart_disease",
        "a_hyperlipidemia",
        "a_hypertension",
        "a_malignant_neoplasm",
        "a_t2_diabetes")
factor.vars<- c("age_gr",
                "gender",   
                "smoke.all_time",
                "medea",
                "region",
                "charlson",
                "Myocardial_infarction",
                "Congestive_heart_failure",
                "Peripheral_vascular_disease",
                "Cerebrovascular_disease",
                "Dementia",
                "Chronic_pulmonary_disease",
                "Rheumatologic_disease",
                "Peptic_ulcer_disease",
                "Mild_liver_disease",
                "Diabetes_with_chronic_complications",
                "Hemoplegia_or_paralegia",
                "Renal_disease",
                "Any_malignancy",
                "Moderate_to_severe_liver_disease",
                "Metastatic_solid_tumor",
                "AIDS",
                "a_autoimmune_condition",
                "a_chronic_kidney_disease",
                "a_copd",
                "a_dementia",
                "a_heart_disease",
                "a_hyperlipidemia",
                "a_hypertension",
                "a_malignant_neoplasm",
                "a_t2_diabetes")
# overall
summary.characteristics.overall<-print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data%>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time)),
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE)
# by bmi
summary.characteristics.bmi<-print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data%>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time)),
  strata=c("bmi.all_time_gr"),
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE)

summary.characteristics<-cbind(summary.characteristics.overall,
                               summary.characteristics.bmi)
rm(summary.characteristics.overall,summary.characteristics.bmi)
for(i in 1:ncol(summary.characteristics)) {
  # tidy up 
  cur_column <- summary.characteristics[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
    as.numeric() 
  cur_column <-nice.num(cur_column)
  # add back in
  summary.characteristics[, i] <- str_replace(string=summary.characteristics[, i], 
                                              pattern='[0-9.]+\\b', 
                                              replacement=cur_column)    
}
# names
#rownames(summary.characteristics)
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "charlson", "charlson comorbidity index")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "obesity.5y", "obesity")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), " = 1", "")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "a_", "")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "_", " ")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "_", " ")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "t2", "Type 2")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "Na", "Missing")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "Copd", "COPD")
rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))


```


```{r}
kable(summary.characteristics)%>%
  kable_styling(bootstrap_options = c("striped", "bordered")) %>%
  add_header_above(c(" "=2,
                     "BMI group"= 3)) 
```

## Table 2: Patient characteristics at baseline stratified by missing VS not missing BMI measurement
Patient characteristics by BMI
```{r, cache=TRUE}
vars<-c(  "age",
          "age_gr",
          "gender",
          "region",
          "charlson",
          # components of charlson
          "Myocardial_infarction",
          "Congestive_heart_failure",
          "Peripheral_vascular_disease",
          "Cerebrovascular_disease",
          "Dementia",
          "Chronic_pulmonary_disease",
          "Rheumatologic_disease",
          "Peptic_ulcer_disease",
          "Mild_liver_disease",
          "Diabetes_with_chronic_complications",
          "Hemoplegia_or_paralegia",
          "Renal_disease",
          "Any_malignancy",
          "Moderate_to_severe_liver_disease",
          "Metastatic_solid_tumor",
          "AIDS",
          # atlas cohorts
          "a_autoimmune_condition",
          "a_chronic_kidney_disease",
          "a_copd",
          "a_dementia",
          "a_heart_disease",
          "a_hyperlipidemia",
          "a_hypertension",
          "a_malignant_neoplasm",
          "a_t2_diabetes")
factor.vars<- c("age_gr",
                "gender",   
                "region",
                "charlson",
                "Myocardial_infarction",
                "Congestive_heart_failure",
                "Peripheral_vascular_disease",
                "Cerebrovascular_disease",
                "Dementia",
                "Chronic_pulmonary_disease",
                "Rheumatologic_disease",
                "Peptic_ulcer_disease",
                "Mild_liver_disease",
                "Diabetes_with_chronic_complications",
                "Hemoplegia_or_paralegia",
                "Renal_disease",
                "Any_malignancy",
                "Moderate_to_severe_liver_disease",
                "Metastatic_solid_tumor",
                "AIDS",
                "a_autoimmune_condition",
                "a_chronic_kidney_disease",
                "a_copd",
                "a_dementia",
                "a_heart_disease",
                "a_hyperlipidemia",
                "a_hypertension",
                "a_malignant_neoplasm",
                "a_t2_diabetes")
summary.characteristics2<-cbind(print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data,
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE),
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      filter(!is.na(bmi.all_time))%>%
      filter(!is.na(medea))%>%
      filter(!is.na(smoke.all_time)),
    test = F), 
    showAllLevels=F,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE),
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      filter(is.na(bmi.all_time)),
    test = F), 
    showAllLevels=F,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE),
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      filter(!is.na(bmi.all_time))%>%
      filter(is.na(medea)|is.na(smoke.all_time)),
    test = F), 
    showAllLevels=F,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE),
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      filter(is.na(bmi.all_time)|is.na(medea)|is.na(smoke.all_time)),
    test = F), 
    showAllLevels=F,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE))

for(i in 1:ncol(summary.characteristics2)) {
  # tidy up 
  cur_column <- summary.characteristics2[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
    as.numeric() 
  cur_column <- format(cur_column,big.mark = ',', decimal.mark='.')
  # add back in
  summary.characteristics2[, i] <- str_replace(string=summary.characteristics2[, i], 
                                               pattern='[0-9.]+\\b', 
                                               replacement=cur_column)    
}
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "charlson", "charlson comorbidity index")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "obesity.5y", "obesity")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), " = 1", "")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "a_", "")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "_", " ")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "_", " ")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "t2", "Type 2")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "Copd", "COPD")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "Na", "Missing")
rownames(summary.characteristics2)<-str_to_sentence(rownames(summary.characteristics2))
```


```{r}
kable(summary.characteristics2,
      col.names=c("Overall", "With BMI recorded and complete information on covariates",
                  "Without BMI", "With BMI and without smoking status or MEDEA",
                  "Without BMI nor smoking status nor MEDEA"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

```

## Supplementary tables 
```{r, cache=TRUE}
### Overall

vars<-c( "bmi.all_time",
         "smoke.all_time",
         "medea")
factor.vars<- c("smoke.all_time",
                "medea")
summary.characteristics6<-cbind(
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      mutate(medea=ifelse(is.na(medea), "Missing MEDEA", as.character(medea)),
             smoke.all_time=ifelse(is.na(smoke.all_time), "Missing smoke", smoke.all_time)),
    test = F), 
    showAllLevels=T,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE)
)
summary.characteristics6[1:2,1]<-c("N","bmi.all_time (median [IQR])")

```
```{r, cache=TRUE}
### With BMI and complete smoking and medea

vars<-c( "bmi.all_time",
         "smoke.all_time",
         "medea")
factor.vars<- c("smoke.all_time",
                "medea")
summary.characteristics3<-cbind(
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = covid.data%>%
      filter(!is.na(bmi.all_time))%>%
      filter(!is.na(medea))%>%
      filter(!is.na(smoke.all_time)),
    test = F), 
    showAllLevels=T,smd=F,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE)
)
summary.characteristics3[1:2,1]<-c("N", "bmi.all_time (median [IQR])")
colnames(summary.characteristics3)[2]<-"With BMI and complete smoking and medea"

```
```{r, cache=TRUE}
### Without BMI

vars<-c( "smoke.all_time",
         "medea")
factor.vars<- c("smoke.all_time",
                "medea")
summary.characteristics4<-cbind(print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data%>%
    mutate(medea=ifelse(is.na(medea), "Missing MEDEA", as.character(medea)),
           smoke.all_time=ifelse(is.na(smoke.all_time), "Missing smoke", smoke.all_time))%>%
    filter(is.na(bmi.all_time)),
  test = F), 
  showAllLevels=T,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE
))
summary.characteristics4[1,1]<-"N"
colnames(summary.characteristics4)[2]<-"Without BMI"


```

```{r, cache=TRUE}
### With BMI and without smoking and medea

vars<-c( "bmi.all_time")
factor.vars<- c("")
summary.characteristics5<-cbind(print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data%>%
    mutate(medea=ifelse(is.na(medea), "Missing MEDEA", as.character(medea)),
           smoke.all_time=ifelse(is.na(smoke.all_time), "Missing smoke", smoke.all_time))%>%
    filter(!is.na(bmi.all_time))%>%
    filter(medea=="Missing MEDEA"|smoke.all_time=="Missing smoke"),
  test = F), 
  showAllLevels=T,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE))
summary.characteristics5[1:2,1]<-c("N","bmi.all_time (median [IQR])")
colnames(summary.characteristics5)[2]<-"With BMI and without smoking and medea"
```

```{r, cache=TRUE}
### Without  BMI nor smoking nor medea

vars<-c( "bmi.all_time",
         "smoke.all_time",
         "medea")
factor.vars<- c("smoke.all_time",
                "medea")
summary.characteristics7<-cbind(print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = covid.data%>%
    mutate(medea=ifelse(is.na(medea), "Missing MEDEA", as.character(medea)),
           smoke.all_time=ifelse(is.na(smoke.all_time), "Missing smoke", smoke.all_time))%>%
    filter(is.na(bmi.all_time)|
             medea=="Missing MEDEA"|
             smoke.all_time=="Missing smoke"),
  test = F), 
  showAllLevels=T,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE))
summary.characteristics7[1:2,1]<-c("N","bmi.all_time (median [IQR])")
colnames(summary.characteristics7)[2]<-"Without  BMI nor smoking nor medea"

test<-merge(summary.characteristics6, summary.characteristics3, by="level", all=T, sort=F)
test<-merge(test, summary.characteristics4,by="level", all=T, sort=F)
test<-merge(test, summary.characteristics5,by="level", all=T, sort=F)
test<-merge(test, summary.characteristics7,by="level", all=T, sort=F)

```

```{r }

a<-rbind(test[1:2,], 
         test[4:6,], 
         test[12,],
         test[3,],
         test[8:10,],
         test[7,],
         test[11,],
         test[13,])

rownames(a)<-a$level

a<-a[,-1]
kable(a)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))%>%
  pack_rows("Smoking", 3, 6) %>%
  pack_rows("Medea", 7, 13)
```

### With BMI and complete smoking and medea in categories
```{r, cache=TRUE}
data_ob<-covid.data%>%
  filter(!is.na(medea),!is.na(smoke.all_time))%>%
  filter(!is.na(bmi.all_time))%>%
  mutate(bmi.all_time_gr=ifelse(bmi.all_time<18.5, "Underweight", 
                                ifelse(bmi.all_time>=18.5 & bmi.all_time<25, "Normal weight",
                                       ifelse(bmi.all_time>=25 & bmi.all_time<30, "Pre-obesity",
                                              ifelse(bmi.all_time>=30 & bmi.all_time<35, "Obesity class I",
                                                     ifelse(bmi.all_time>=35 & bmi.all_time<40, "Obesity class II", 
                                                            ifelse(bmi.all_time>=40, "Obesity class III", "Not available")))))))%>%
  mutate(bmi.all_time_gr=factor(bmi.all_time_gr))
vars<-c( "")
factor.vars<- c("")
summary.characteristics6<-cbind(print(CreateTableOne(
  includeNA=T,
  data = data_ob ,
  
  strata("bmi.all_time_gr"),
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE))


```

```{r}
kable(summary.characteristics6,
      col.names=c("BMI"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

```

### SMDs complete vs incomplete
```{r, cache=TRUE}

vars<-c(  "age",
          "age_gr",
          "gender",
          "bmi.all_time",
         "smoke.all_time",
         "medea",
          "region",
          "charlson",
          # components of charlson
          "Myocardial_infarction",
          "Congestive_heart_failure",
          "Peripheral_vascular_disease",
          "Cerebrovascular_disease",
          "Dementia",
          "Chronic_pulmonary_disease",
          "Rheumatologic_disease",
          "Peptic_ulcer_disease",
          "Mild_liver_disease",
          "Diabetes_with_chronic_complications",
          "Hemoplegia_or_paralegia",
          "Renal_disease",
          "Any_malignancy",
          "Moderate_to_severe_liver_disease",
          "Metastatic_solid_tumor",
          "AIDS",
          # atlas cohorts
          "a_autoimmune_condition",
          "a_chronic_kidney_disease",
          "a_copd",
          "a_dementia",
          "a_heart_disease",
          "a_hyperlipidemia",
          "a_hypertension",
          "a_malignant_neoplasm",
          "a_t2_diabetes")
factor.vars<- c("age_gr",
                "gender",  
                "smoke.all_time",
                "medea",
                "region",
                "charlson",
                "Myocardial_infarction",
                "Congestive_heart_failure",
                "Peripheral_vascular_disease",
                "Cerebrovascular_disease",
                "Dementia",
                "Chronic_pulmonary_disease",
                "Rheumatologic_disease",
                "Peptic_ulcer_disease",
                "Mild_liver_disease",
                "Diabetes_with_chronic_complications",
                "Hemoplegia_or_paralegia",
                "Renal_disease",
                "Any_malignancy",
                "Moderate_to_severe_liver_disease",
                "Metastatic_solid_tumor",
                "AIDS",
                "a_autoimmune_condition",
                "a_chronic_kidney_disease",
                "a_copd",
                "a_dementia",
                "a_heart_disease",
                "a_hyperlipidemia",
                "a_hypertension",
                "a_malignant_neoplasm",
                "a_t2_diabetes")
summary.characteristics2<-cbind(
  
  print(CreateTableOne(
    vars =  vars,
    factorVars = factor.vars,
    includeNA=T,
    data = rbind(covid.data%>%
      filter(!is.na(bmi.all_time))%>%
      filter(!is.na(medea))%>%
      filter(!is.na(smoke.all_time))%>%
      mutate(tosummary="First group"),
      covid.data%>%
      filter(is.na(bmi.all_time)|is.na(medea)|is.na(smoke.all_time))%>%
      mutate(tosummary="Second group")),
    strata="tosummary",
    test = F), 
    showAllLevels=F,smd=TRUE,
    nonnormal = vars, 
    noSpaces = TRUE,
    contDigits = 1,
    printToggle=FALSE))

for(i in 1:ncol(summary.characteristics2)) {
  # tidy up 
  cur_column <- summary.characteristics2[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
    as.numeric() 
  cur_column <- format(cur_column,big.mark = ',', decimal.mark='.')
  # add back in
  summary.characteristics2[, i] <- str_replace(string=summary.characteristics2[, i], 
                                               pattern='[0-9.]+\\b', 
                                               replacement=cur_column)    
}
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "charlson", "charlson comorbidity index")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "obesity.5y", "obesity")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), " = 1", "")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2), "a_", "")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "_", " ")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "_", " ")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "t2", "Type 2")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "Copd", "COPD")
rownames(summary.characteristics2)<-str_replace(rownames(summary.characteristics2) , "Na", "Missing")
rownames(summary.characteristics2)<-str_to_sentence(rownames(summary.characteristics2))
```


```{r}
kable(summary.characteristics2,
      col.names=c("With BMI recorded and complete information on covariates",
                  
                  "Without BMI nor smoking status nor MEDEA","SMD"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))

```
## Figure 1: Conditions by BMI category

### Polarplot by gender
```{r, width="250%"}
a<-covid.data%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time)) %>%
  group_by(gender, bmi.all_time_gr) %>% 
  add_tally() %>% 
  group_by(gender, bmi.all_time_gr, n) %>% 
  summarise(a_autoimmune_condition=sum(a_autoimmune_condition==1),
            a_chronic_kidney_disease =sum(a_chronic_kidney_disease==1),
            a_copd =sum(a_copd==1),
            a_heart_disease =sum(a_heart_disease==1),
            a_hyperlipidemia  =sum(a_hyperlipidemia==1),
            a_hypertension =sum(a_hypertension==1),
            a_malignant_neoplasm = sum(a_malignant_neoplasm==1),
            a_t2_diabetes  =sum(a_t2_diabetes==1)) %>% 
  mutate(
    a_autoimmune_condition=a_autoimmune_condition/n,
    a_chronic_kidney_disease =a_chronic_kidney_disease/n,
    a_copd =a_copd/n,
    a_heart_disease =a_heart_disease/n,
    a_hyperlipidemia  =a_hyperlipidemia/n,
    a_hypertension =a_hypertension/n,
    a_malignant_neoplasm = a_malignant_neoplasm/n,
    a_t2_diabetes = a_t2_diabetes/n) %>% 
  pivot_longer(
    cols = starts_with("a_"),
    names_to = "var",
    values_to = "prop",
    values_drop_na = TRUE)
a<-  a %>% 
  mutate(var.name=
           ifelse(
             var=="a_autoimmune_condition",
             "Autoimmune condition",
             ifelse(
               var=="a_chronic_kidney_disease",
               "Chronic kidney disease",
               ifelse(
                 var=="a_copd",
                 "COPD",
                 ifelse(
                   var=="a_dementia",
                   "Dementia",
                   ifelse(
                     var=="a_heart_disease",
                     "Heart disease",
                     ifelse(
                       var=="a_hyperlipidemia",
                       "Hyperlipidemia",
                       ifelse(
                         var=="a_hypertension",
                         "Hypertension",
                         ifelse(
                           var=="a_malignant_neoplasm",
                           "Malignant neoplasm",
                           ifelse(
                             var=="a_t2_diabetes",
                             "Type 2 Diabetes Mellitus",NA    ))))))))))  %>% 
  mutate(bmi.all_time_gr=factor(bmi.all_time_gr,
                                levels=c("normal or underweight","overweight", 
                                         "obese")))
gg.conditions.gender<-a %>% 
  ggplot()+
  facet_grid(gender~bmi.all_time_gr, switch="y")+
  geom_bar(aes(var, prop, fill=var.name), 
           width = 1, colour="grey",
           stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_y_continuous( breaks=c(0.1,0.2,0.3,0.4, 0.5,0.6,0.7),
                      limits=c(-0.075,0.7))+
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
                               "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6", "#6a3d9a"))+ 
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        strip.text = element_text(size=10, face="bold"),
        panel.grid.major.x = element_blank() ,
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "bottom" ,
        legend.text = element_text(size = 10)) +
  geom_text(x = 3, y = 0.5,
            size=4,
            label = "50%")+
  coord_polar(start = 0)
gg.conditions.gender
# ggsave("gg.conditions.gender.png",gg.conditions.gender,
#        dpi=300,
#        width = 10.5, height = 8)

```

### Polarplot by age group
```{r, width="250%", height="250%"}
a<-covid.data%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time)) %>%
  group_by(age_gr, bmi.all_time_gr) %>% 
  add_tally() %>% 
  group_by(age_gr, bmi.all_time_gr, n) %>% 
  summarise(a_autoimmune_condition=sum(a_autoimmune_condition==1),
            a_chronic_kidney_disease =sum(a_chronic_kidney_disease==1),
            a_copd =sum(a_copd==1),
            a_heart_disease =sum(a_heart_disease==1),
            a_hyperlipidemia  =sum(a_hyperlipidemia==1),
            a_hypertension =sum(a_hypertension==1),
            a_malignant_neoplasm = sum(a_malignant_neoplasm==1),
            a_t2_diabetes  =sum(a_t2_diabetes==1)) %>% 
  mutate(
    a_autoimmune_condition=a_autoimmune_condition/n,
    a_chronic_kidney_disease =a_chronic_kidney_disease/n,
    a_copd =a_copd/n,
    a_heart_disease =a_heart_disease/n,
    a_hyperlipidemia  =a_hyperlipidemia/n,
    a_hypertension =a_hypertension/n,
    a_malignant_neoplasm = a_malignant_neoplasm/n,
    a_t2_diabetes = a_t2_diabetes/n) %>% 
  pivot_longer(
    cols = starts_with("a_"),
    names_to = "var",
    values_to = "prop",
    values_drop_na = TRUE)
a<-  a %>% 
  mutate(var.name=
           ifelse(
             var=="a_autoimmune_condition",
             "Autoimmune condition",
             ifelse(
               var=="a_chronic_kidney_disease",
               "Chronic kidney disease",
               ifelse(
                 var=="a_copd",
                 "COPD",
                 ifelse(
                   var=="a_dementia",
                   "Dementia",
                   ifelse(
                     var=="a_heart_disease",
                     "Heart disease",
                     ifelse(
                       var=="a_hyperlipidemia",
                       "Hyperlipidemia",
                       ifelse(
                         var=="a_hypertension",
                         "Hypertension",
                         ifelse(
                           var=="a_malignant_neoplasm",
                           "Malignant neoplasm",
                           ifelse(
                             var=="a_t2_diabetes",
                             "Type 2 Diabetes Mellitus",NA    ))))))))))  %>% 
  mutate(bmi.all_time_gr=factor(bmi.all_time_gr,
                                levels=c("normal or underweight","overweight", 
                                         "obese"))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("Under 18","18 to 39", "40 to 59", "60 to 69",
                                   "70 to 79", "80 or older")))
gg.conditions.age<-a %>% 
  ggplot()+
  facet_grid(age_gr~bmi.all_time_gr, switch="y")+
  geom_bar(aes(var, prop, fill=var.name), 
           width = 1, colour="grey",
           stat="identity", position=position_dodge())+
  theme_minimal() +
  scale_y_continuous( breaks=c(0.1,0.2,0.3,0.4, 0.5,0.6,0.7),
                      limits=c(-0.075,0.7))+
  scale_fill_manual(values = c("#a6cee3","#1f78b4","#b2df8a","#33a02c",
                               "#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6", "#6a3d9a"))+ 
  theme(panel.spacing = unit(0, "lines"),
        legend.title = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        panel.grid.major.x = element_blank() ,
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.y.left = element_text(angle = 0),
        legend.position = "bottom" ,
        legend.text = element_text(size = 12)) +
  geom_text(x = 3, y = 0.5,
            size=4,
            label = "50%")+
  coord_polar(start = 0)
# ggsave("gg.conditions.age.png",gg.conditions.age,
#        dpi=300,
#        width = 10.5, height = 16)
gg.conditions.age
```

## Figure 2: Age and gender histogram
### Ridgeplot of age and gender for complete case population (also without BMI measurement) and BMI categories.
```{r}
plot.data<-covid.data%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  mutate(bmi.all_time_gr=ifelse(!is.na(bmi.all_time_gr), as.character(bmi.all_time_gr), 'without BMI measurement'))
age.gender<-  ggplot(plot.data, aes(x = age, y = factor(bmi.all_time_gr ,levels=rev(c("without BMI measurement", "normal or underweight", "overweight", "obese" ))), height=..density.., fill= gender))+
  geom_density_ridges(stat="density")+
  facet_grid(.~gender)+
  theme_bw()+
  xlim(0,120)+
  theme(
    legend.position="none",
    panel.grid.major.x= element_line(colour = "grey", linetype="dashed")
  )+
  labs(x="Age", y = NULL, title = 'Age distribution for BMI categories')
age.gender
```

### Histogram of age and gender for complete case population (also without BMI measurement) and BMI categories.
```{r}

#Mirrored
plot.data.male<-plot.data %>% 
  filter(gender=="Male") 
plot.data.female<-plot.data %>%
  filter(gender=="Female")
dat_text <- data.frame(
  label = c("Female", "Male"),
  x     = c(95,95),
  y     = c(-0.020, 0.025)
)
histogram3<-  ggplot() + 
  geom_histogram(data=plot.data.male ,
                 aes(x=age, y=..density..),
                 colour="black", 
                 binwidth = 4, boundary = 0,
                 fill="#F21A00")+
  geom_histogram(data=plot.data.female,
                 aes(x=age, y=-..density..),
                 colour="black", 
                 binwidth = 4, boundary = 0,
                 fill="#3B9AB2")+
  coord_flip()+
  facet_wrap(factor(bmi.all_time_gr, levels=c("without BMI measurement", "normal or underweight", "overweight", "obese" )) ~.)+
  theme_bw()+
  scale_y_continuous(breaks=c(-0.05,-0.025, 0,0.025,0.05),labels=c("5%","2.5%", "0%","2.5%","5%"))+
  xlim(0,104)+
  geom_label(
    data    = dat_text,
    mapping = aes(x = x,
                  y = y, label = label),
    size=5)+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=14, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        legend.text=element_text(size=14),
        legend.position="top") +
  ylab("Density")+
  xlab("Age")
histogram3

```
## Figure 3: BMI distribution
Histogram of BMI distribution
```{r}
histogram.BMI<-ggplot(plot.data%>%filter(!bmi.all_time_gr=="without BMI measurement"))+
  geom_histogram(aes(x= bmi.all_time, fill=bmi.all_time_gr), colour="black", binwidth = 0.5)+
  theme_minimal()+
  scale_x_continuous(breaks = c(0, 10, 20, 30 ,40, 50 , 60))+
  labs(x = "BMI")+
  theme(legend.title=element_blank())
histogram.BMI
```

## Table 3: Patient characteristics 
Patient characteristics by state (general population) or transition (from general population, diagnosed with COVID-19, and hospitalised with COVID-19)
```{r, cache=TRUE}
#obesity, medea, smoking

vars<-c("bmi.all_time",
        "age",
        "age_gr",
        "gender",
        "bmi.all_time_gr",
        "smoke.all_time",
        "medea",
        "region",
        "charlson",
        # components of charlson
        "Myocardial_infarction",
        "Congestive_heart_failure",
        "Peripheral_vascular_disease",
        "Cerebrovascular_disease",
        "Dementia",
        "Chronic_pulmonary_disease",
        "Rheumatologic_disease",
        "Peptic_ulcer_disease",
        "Mild_liver_disease",
        "Diabetes_with_chronic_complications",
        "Hemoplegia_or_paralegia",
        "Renal_disease",
        "Any_malignancy",
        "Moderate_to_severe_liver_disease",
        "Metastatic_solid_tumor",
        "AIDS",
        # atlas cohorts
        "a_autoimmune_condition",
        "a_chronic_kidney_disease",
        "a_copd",
        "a_dementia",
        "a_heart_disease",
        "a_hyperlipidemia",
        "a_hypertension",
        "a_malignant_neoplasm",
        "a_t2_diabetes")
factor.vars<- c("age_gr",
                "gender",    
                "bmi.all_time_gr",
                "smoke.all_time",
                "medea",
                "region",
                "charlson",
                "Myocardial_infarction",
                "Congestive_heart_failure",
                "Peripheral_vascular_disease",
                "Cerebrovascular_disease",
                "Dementia",
                "Chronic_pulmonary_disease",
                "Rheumatologic_disease",
                "Peptic_ulcer_disease",
                "Mild_liver_disease",
                "Diabetes_with_chronic_complications",
                "Hemoplegia_or_paralegia",
                "Renal_disease",
                "Any_malignancy",
                "Moderate_to_severe_liver_disease",
                "Metastatic_solid_tumor",
                "AIDS",
                "a_autoimmune_condition",
                "a_chronic_kidney_disease",
                "a_copd",
                "a_dementia",
                "a_heart_disease",
                "a_hyperlipidemia",
                "a_hypertension",
                "a_malignant_neoplasm",
                "a_t2_diabetes")

levels(r.healthy.diagnosis$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                                 "overweight" ="overweight",
                                                 "obese"=c("obese class I", "obese class II", "obese class III"))
levels(r.healthy.hospitalised$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                                    "overweight" ="overweight",
                                                    "obese"=c("obese class I", "obese class II", "obese class III"))
levels(r.healthy.death$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                             "overweight" ="overweight",
                                             "obese"=c("obese class I", "obese class II", "obese class III"))
levels(r.diagnosis.hospitalised$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                                      "overweight" ="overweight",
                                                      "obese"=c("obese class I", "obese class II", "obese class III"))
levels(r.diagnosis.death$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                               "overweight" ="overweight",
                                               "obese"=c("obese class I", "obese class II", "obese class III"))
levels(r.hospitalised.death$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                                                  "overweight" ="overweight",
                                                  "obese"=c("obese class I", "obese class II", "obese class III"))

#create category missing para medea (put U also) and smoking

table1.data<-rbind(  
  # all those in general population
  r.healthy.diagnosis%>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    mutate(group="Population with BMI"), 
  # all those who transition from general population to diagnosed
  r.healthy.diagnosis%>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>% 
    mutate(group="From general population to diagnosed with COVID-19"),
  # all those who transition from general population to hospitalised
  r.healthy.hospitalised%>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>% 
    mutate(group="From general population to hospitalised with COVID-19"), 
  # all those who transition from diagnosed to hospitalised
  r.diagnosis.hospitalised %>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>% 
    mutate(group="From diagnosed with COVID-19 to hospitalised with COVID-19"), 
  # all those who transition from hospitalised to death
  r.hospitalised.death %>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>% 
    mutate(group="From hospitalised with COVID-19 to death"), 
  # # all those who transition from general population to death
  r.healthy.death %>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>%
    mutate(group="From general population to death"),
  # all those who transition from diagnosed to death
  r.diagnosis.death %>%
    filter(!is.na(bmi.all_time))%>%
    filter(!is.na(medea))%>%
    filter(!is.na(smoke.all_time))%>% 
    filter(status==1) %>% 
    mutate(group="From diagnosed with COVID-19 to death")) %>% 
  mutate(group=factor(group,
                      levels=c("Population with BMI",
                               "From general population to diagnosed with COVID-19",
                               "From general population to hospitalised with COVID-19",
                               "From general population to death",
                               "From diagnosed with COVID-19 to hospitalised with COVID-19",
                               "From diagnosed with COVID-19 to death",
                               "From hospitalised with COVID-19 to death"
                      ))) %>%
  mutate(gender=factor(gender,
                       levels=c("Male", "Female")))


summary.characteristics<-print(CreateTableOne(
  vars =  vars,
  factorVars = factor.vars,
  includeNA=T,
  data = table1.data %>%
    filter(!is.na(bmi.all_time)),
  strata=c("group"),
  test = F), 
  showAllLevels=F,smd=F,
  nonnormal = vars, 
  noSpaces = TRUE,
  contDigits = 1,
  printToggle=FALSE)
# format numbers (eg commas etc)  
# functionality does not seem to be in tableone package
# so do this manually
for(i in 1:ncol(summary.characteristics)) {
  # tidy up 
  cur_column <- summary.characteristics[, i]
  cur_column <- str_extract(cur_column, '[0-9.]+\\b') %>% 
    as.numeric() 
  cur_column <-nice.num(cur_column)
  # add back in
  summary.characteristics[, i] <- str_replace(string=summary.characteristics[, i], 
                                              pattern='[0-9.]+\\b', 
                                              replacement=cur_column)    
}
# names
#rownames(summary.characteristics)
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "charlson", "charlson comorbidity index")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "obesity.5y", "obesity")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), " = 1", "")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics), "a_", "")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "_", " ")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "_", " ")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "t2", "Type 2")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "copd", "COPD")
rownames(summary.characteristics)<-str_replace(rownames(summary.characteristics) , "NA", "Missing")
rownames(summary.characteristics)<-str_to_sentence(rownames(summary.characteristics))
```


```{r}

kable(summary.characteristics, 
      col.names = c("General population",  
                    "To diagnosed with COVID-19",
                    "To hospitalised with COVID-19",
                    "To death",
                    "To hospitalised with COVID-19",
                    "To death",
                    "To death"))  %>%
  add_header_above(c(" "=2,
                     "From general population"= 3, 
                     "From diagnosed with COVID-19"= 2, 
                     "From hospitalised with COVID-19"= 1)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
```

## Table 4: Transitions and cumulative incidence

```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))
r1<-r %>% 
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))
events<-rbind(
  # overall (with BMI)
  r1 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r1 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)
```

```{r, cache=TRUE}
covid.data_g<-covid.data
# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)
```

```{r,cache=TRUE}
# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)
```

```{r, cache=TRUE}
# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall

c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```


### Age 18 - 59
```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))

r2<-r %>% 
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))%>%
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  filter(age_gr.b == "Aged 18 to 59")
events<-rbind(
  # overall (with BMI)
  r2 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r2 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)
```
```{r, cache=TRUE}
# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))

covid.data<-covid.data%>%
  filter(age_gr.b == "Aged 18 to 59")
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)

# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)

# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```


### Age 60-79
```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))

r3<-r %>% 
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))%>%
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  filter(age_gr.b == "Aged 60 to 79")
events<-rbind(
  # overall (with BMI)
  r3 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r3 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)

# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))

covid.data<-covid.data%>%
  filter(age_gr.b == "Aged 60 to 79")
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)

# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)

# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```

### Age 80 or above
```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))

r4<-r %>% 
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))%>%
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  filter(age_gr.b == "Aged 80 or above")
events<-rbind(
  # overall (with BMI)
  r4 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r4 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)

# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59", 
                          ifelse(age<=79, "Aged 60 to 79", 
                                 "Aged 80 or above" ))) %>% 
  mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above")))

covid.data<-covid.data%>%
  filter(age_gr.b == "Aged 80 or above")
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)

# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)

# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```

### Female
```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))

r5<-r %>% 
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  filter(gender=="Female")
events<-rbind(
  # overall (with BMI)
  r5 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r5 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)

# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))

covid.data<-covid.data%>%
  filter(gender=="Female")
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)

# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)

# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```

### Male
```{r}
# events
# join obesity groups
levels(r$bmi.all_time_gr)=list("normal or underweight"="normal or underweight",
                               "overweight" ="overweight",
                               "obese"=c("obese class I", "obese class II", "obese class III"))

r6<-r %>% 
  filter(!is.na(bmi.all_time)) %>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))%>%
  filter(gender=="Male")
events<-rbind(
  # overall (with BMI)
  r6 %>% 
    group_by(trans) %>% 
    add_tally() %>% 
    group_by(trans, n) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time)) %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="all") %>% 
    mutate(level="all") %>% 
    select(trans, group,level, n, time, events),
  # by bmi
  r6 %>% 
    group_by(trans, bmi.all_time_gr) %>% 
    add_tally() %>% 
    group_by(trans, n, bmi.all_time_gr) %>%  
    summarise(events=sum(status),
              time.0=min(time),
              time.25=quantile(time, probs =0.25),
              time.50=quantile(time, probs =0.5),
              time.75=quantile(time, probs =0.75),
              time.1=max(time))%>%
    arrange(trans, bmi.all_time_gr)  %>% 
    mutate(time=paste0(time.50, " (", time.0, ", ", time.25, " to ", time.75, ", ", time.1, ")")) %>% 
    mutate(group="BMI") %>% 
    mutate(level=bmi.all_time_gr) %>% 
    select(trans, group,level, n, time, events)
  
)

# format numbers
events$n<-nice.num(events$n)
events$events<-nice.num(events$events)
# order by transition
events<-events %>% 
  arrange(trans)

# factors for covid.data ----
covid.data<-covid.data_g%>%
  filter(!is.na(bmi.all_time))%>%
  filter(!is.na(medea))%>%
  filter(!is.na(smoke.all_time))

covid.data<-covid.data%>%
  filter(gender=="Male")
# get 67 day cumualative incidence for initial transitions -----
events.t1_t3<-events %>% 
  filter(trans %in% c(1:3))
#cuminc 
c.inc_fit<-list()
#overall
c.inc_fit[["overall"]] <- cuminc(ftime = covid.data$healthy_c.time, 
                                 fstatus = covid.data$healthy_c.event)
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- cuminc(ftime = covid.data$healthy_c.time,  
                                  fstatus = covid.data$healthy_c.event, 
                                  group = covid.data[[groups[i]]]) 
}
# extract estimates at 67 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],67)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],67)$est
}
t1<-data.frame( 
  trans=1,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=2,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
t3<-data.frame( 
  trans=3,
  group=c("all", rep("BMI",3)),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[3],
    tp$bmi.all_time_gr[7:9]
  ))
est<-rbind(t1,t2,t3)
# add to table
events.t1_t3<-events.t1_t3 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format number
events.t1_t3<-events.t1_t3 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide- transitions on same row
events.t1_t3.wide<-events.t1_t3 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est)) 
# formatting
events.t1_t3.wide<-events.t1_t3.wide %>% 
  mutate(events_diag.general.pop=paste0(events_1, " (", est_1, ")")) %>% 
  mutate(events_hosp.general.pop=paste0(events_2, " (", est_2, ")")) %>% 
  mutate(events_death.general.pop=paste0(events_3, " (", est_3, ")")) %>% 
  select(group, level, n, time, events_diag.general.pop, events_hosp.general.pop, events_death.general.pop) %>% 
  rename( n.general.pop=n,
          time.general.pop=time)

# get 45 day cumualative incidence for transitions from diagnosis ----
events.t4_t5<-events %>% 
  filter(trans %in% c(4:5))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = covid.data$diagnosis_c.time, 
                                       fstatus = covid.data$diagnosis_c.event))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = covid.data$diagnosis_c.time,  
                                        fstatus = covid.data$diagnosis_c.event, 
                                        group = covid.data[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=4,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
t2<-data.frame( 
  trans=5,
  group=c("all", rep("BMI",3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[2],
    tp$bmi.all_time_gr[4:6]
  ))
est<-rbind(t1,t2)
##
# add to table
events.t4_t5<-events.t4_t5 %>% 
  left_join(est,
            by = c("trans","group", "level"))
# format numbers
events.t4_t5<-events.t4_t5 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide
events.t4_t5.wide<-events.t4_t5 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t4_t5.wide<-events.t4_t5.wide %>% 
  mutate(events_hosp.diag=paste0(events_4, " (", est_4, ")")) %>% 
  mutate(events_death.diag=paste0(events_5, " (", est_5, ")")) %>% 
  select(group, level, n, time, events_hosp.diag, events_death.diag) %>% 
  rename(n.diag=n,
         time.diagn=time)

# get 45 days cumualative incidence for transition from hospitalised ----
events.t6<-events %>% 
  filter(trans %in% c(6))
#cuminc 
c.inc_fit<-list()
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
} 
# use quiet to suppress message about missing values (we only want pop for specific state, others are missing)
#overall
c.inc_fit[["overall"]] <- quiet(cuminc(ftime = r.hospitalised.death$time, 
                                       fstatus = r.hospitalised.death$status))
#by groups
groups<-c("bmi.all_time_gr")
for(i in 1:length(groups)){
  message(paste0("Working on ", i, " of ", length(groups)))
  c.inc_fit[[groups[i]]]<- quiet(cuminc(ftime = r.hospitalised.death$time,  
                                        fstatus = r.hospitalised.death$status, 
                                        group = r.hospitalised.death[[groups[i]]]) )
  
}
# extract estimates at 45 days
tp<-list()
tp[["overall"]]<-timepoints(c.inc_fit[["overall"]],45)$est
for(i in 1:length(groups)){
  tp[[groups[i]]]<- timepoints(c.inc_fit[[groups[i]]],45)$est
}
t1<-data.frame( 
  trans=6,
  group=c("all", rep("BMI", 3)
  ),
  level=c("all",
          levels(covid.data$bmi.all_time_gr)),
  est=c(
    tp$overall[1],
    tp$bmi.all_time_gr[1:3]
  ))
# add to table
events.t6<-events.t6 %>% 
  left_join(t1,
            by = c("trans","group", "level"))
# format number
events.t6<-events.t6 %>% 
  mutate(est=paste0(nice.num2(est*100), "%"))
# pivot wide for consistency in names etc
events.t6.wide<-events.t6 %>% 
  pivot_wider(names_from = trans,
              values_from = c(events, est))
events.t6.wide<-events.t6.wide %>% 
  mutate(events_death.hosp=paste0(events_6, " (", est_6, ")")) %>% 
  select(group, level, n, time, events_death.hosp) %>% 
  rename(n.hosp=n,
         time.hosp=time)

a<-events.t1_t3.wide  %>% 
  left_join(events.t4_t5.wide) %>% 
  left_join(events.t6.wide)

kable(a, 
      col.names = c("", "",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 67 days)",
                    "Events (cumulative incidence at 67 days)",  	
                    "Events (cumulative incidence at 67 days",
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)",
                    "Events (cumulative incidence at 45 days)",  
                    "n",
                    "Median (min, interquartile range, max)",
                    "Events (cumulative incidence at 45 days)"))  %>%
  add_header_above(c(" " = 3, 
                     "Follow-up in days",
                     "To diagnosis with COVID-19",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days",
                     "To hospitalised with COVID-19",  	
                     "To death",
                     " ",
                     "Follow-up in days", 	
                     "To death"))%>%
  add_header_above(c(" " = 2, "From general population" = 5,
                     "From diagnosed with COVID-19" = 4,
                     "From hospitalised with COVID-19" = 3)) %>%
  kable_styling(bootstrap_options = c("striped", "bordered")) 

```
