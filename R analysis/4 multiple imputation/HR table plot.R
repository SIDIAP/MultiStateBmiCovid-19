---
title: 'Analysis for multiple imputation'
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
                      cache.lazy = FALSE)
options(scipen=999)
options(knitr.kable.NA = '')
```

```{r package, include=FALSE}
# package -----
library(kableExtra)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rms)
library(stringr)
library(purrr)
```

```{r data, include=FALSE}

load("1 data prep/working data.RData")

lapply(paste0("imputations bmi multistate/",list.files(path="imputations bmi multistate")), 
       load, envir=globalenv())
source("functions_hazardratio.R") 
#overwrite get.models
get.models<-function(r.data,r.data.imp,name){
  # will add models to list-----
  models<-list()
  
  ## overall models ------
  
  #data
  working.data<-r.data 
  working.data.imp<-r.data.imp
  dd<<-datadist(working.data); options(datadist = "dd" )
  #complete adjustment
  models[[paste0("m.",name, ".overall.linear", ".bmi.complete")]]<-fit.mult.impute(Surv(time, status) ~ bmi.all_time+        
                                                                                     age+gender+ medea+smoke.all_time,
                                                                                   cph,xtrans=working.data.imp,
                                                                                   data = working.data, iter.max=100, pr=FALSE)
  models[[paste0("m.",name, ".overall.quadratic", ".bmi.complete")]]<-fit.mult.impute(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                                        age+gender+ medea+smoke.all_time,
                                                                                      cph,xtrans=working.data.imp,
                                                                                      data = working.data,iter.max=100, pr=FALSE)
  models[[paste0("m.",name, ".overall.rcs3", ".bmi.complete")]]<-fit.mult.impute(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                                   age+gender+ medea+smoke.all_time,
                                                                                 cph,xtrans=working.data.imp,
                                                                                 data = working.data, iter.max=100,pr=FALSE)
  models[[paste0("m.",name, ".overall.rcs4", ".bmi.complete")]]<-fit.mult.impute(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                                   age+gender+ medea+smoke.all_time,
                                                                                 cph,xtrans=working.data.imp,
                                                                                 data = working.data,iter.max=100,pr=FALSE)
  models[[paste0("m.",name, ".overall.rcs5", ".bmi.complete")]]<-fit.mult.impute(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                                   age+gender+ medea+smoke.all_time,
                                                                                 cph,xtrans=working.data.imp,
                                                                                 data = working.data, iter.max=100,pr=FALSE)
  
  ## output ----
  models
}

models.healthy.diagnosis<-get.models(r.healthy.diagnosis,r.healthy.diagnosis.imp, "healthy.diagnosis")
models.healthy.hospitalised<-get.models(r.healthy.hospitalised, r.healthy.hospitalised.imp,"healthy.hospitalised")
models.diagnosis.hospitalised<-get.models(r.diagnosis.hospitalised, r.diagnosis.hospitalised.imp,"diagnosis.hospitalised")
models.diagnosis.death<-get.models(r.diagnosis.death, r.diagnosis.death.imp,"diagnosis.death")
models.hospitalised.death<-get.models(r.hospitalised.death,r.hospitalised.death.imp, "hospitalised.death")
```


# Transition 1: From general population to diagnosed with COVID-19 
```{r}
# models and relative hazards from models -----
r.hazard<-get.r.hazard(model=models.healthy.diagnosis,
                       bmi.1=22,
                       bmi.2=seq(15,60, 1)) 

fit<-rbind(
  #overall - complete
  r.hazard %>% 
    filter(str_detect(model, ".overall")) %>% 
    filter(str_detect(model, ".bmi.complete")) %>% 
    select(model, aic, bic) %>% 
    distinct() %>% 
    mutate(bic=nice.num(bic)) %>% 
    mutate(aic=nice.num(aic)) %>% 
    mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
    select(type, aic, bic) %>% 
    mutate(name="overall.bmi.complete"))

fit<-fit %>%
  pivot_wider(names_from = name, values_from = c(aic,bic)) %>%
  select(type ,aic_overall.bmi.complete, bic_overall.bmi.complete)

kable(fit %>% 
        mutate(type=ifelse(type=="rcs3",
                           "restricted cubic spline (3 knots)",
                           ifelse(type=="rcs4",
                                  "restricted cubic spline (4 knots)",
                                  ifelse(type=="rcs5",
                                         "restricted cubic spline (5 knots)",      
                                         type)))), 
      col.names = c("Type",  
                    "AIC","BIC"))  %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  %>%
  add_header_above( c("","Complete adjustment" = 2))  %>%
  add_header_above( c("","Overall" = 2))%>%
  add_header_above(c("From general population to diagnosed with COVID-19" = 3))
```

# Transition 2: From general population to hospitalised with COVID-19
```{r}
# models and relative hazards from models -----
r.hazard<-get.r.hazard(model=models.healthy.hospitalised,
                       bmi.1=22,
                       bmi.2=seq(15,60, 1)) 

fit<-rbind(
  #overall - complete
  r.hazard %>% 
    filter(str_detect(model, ".overall")) %>% 
    filter(str_detect(model, ".bmi.complete")) %>% 
    select(model, aic, bic) %>% 
    distinct() %>% 
    mutate(bic=nice.num(bic)) %>% 
    mutate(aic=nice.num(aic)) %>% 
    mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
    select(type, aic, bic) %>% 
    mutate(name="overall.bmi.complete"))

fit<-fit %>%
  pivot_wider(names_from = name, values_from = c(aic,bic)) %>%
  select(type ,aic_overall.bmi.complete, bic_overall.bmi.complete)

kable(fit %>% 
        mutate(type=ifelse(type=="rcs3",
                           "restricted cubic spline (3 knots)",
                           ifelse(type=="rcs4",
                                  "restricted cubic spline (4 knots)",
                                  ifelse(type=="rcs5",
                                         "restricted cubic spline (5 knots)",      
                                         type)))), 
      col.names = c("Type",  
                    "AIC","BIC"))  %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  %>%
  add_header_above( c("","Complete adjustment" = 2))  %>%
  add_header_above( c("","Overall" = 2))%>%
  add_header_above(c("From general population to hospitalised with COVID-19" = 3))
```

# Transition 4: From diagnosed with COVID-19 to hospitalised with COVID-19 
```{r}
# models and relative hazards from models -----
r.hazard<-get.r.hazard(model=models.diagnosis.hospitalised,
                       bmi.1=22,
                       bmi.2=seq(15,60, 1)) 

fit<-rbind(
  #overall - complete
  r.hazard %>% 
    filter(str_detect(model, ".overall")) %>% 
    filter(str_detect(model, ".bmi.complete")) %>% 
    select(model, aic, bic) %>% 
    distinct() %>% 
    mutate(bic=nice.num(bic)) %>% 
    mutate(aic=nice.num(aic)) %>% 
    mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
    select(type, aic, bic) %>% 
    mutate(name="overall.bmi.complete"))

fit<-fit %>%
  pivot_wider(names_from = name, values_from = c(aic,bic)) %>%
  select(type ,aic_overall.bmi.complete, bic_overall.bmi.complete)

kable(fit %>% 
        mutate(type=ifelse(type=="rcs3",
                           "restricted cubic spline (3 knots)",
                           ifelse(type=="rcs4",
                                  "restricted cubic spline (4 knots)",
                                  ifelse(type=="rcs5",
                                         "restricted cubic spline (5 knots)",      
                                         type)))), 
      col.names = c("Type",  
                    "AIC","BIC"))  %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  %>%
  add_header_above( c("","Complete adjustment" = 2))  %>%
  add_header_above( c("","Overall" = 2))%>%
  add_header_above(c("From diagnosed with COVID-19 to hospitalised with COVID-19" = 3))
```

# Transition 5: From diagnosed with COVID-19 to death 
```{r}
# models and relative hazards from models -----
r.hazard<-get.r.hazard(model=models.diagnosis.death,
                       bmi.1=22,
                       bmi.2=seq(15,60, 1)) 

fit<-rbind(
  #overall - complete
  r.hazard %>% 
    filter(str_detect(model, ".overall")) %>% 
    filter(str_detect(model, ".bmi.complete")) %>% 
    select(model, aic, bic) %>% 
    distinct() %>% 
    mutate(bic=nice.num(bic)) %>% 
    mutate(aic=nice.num(aic)) %>% 
    mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
    select(type, aic, bic) %>% 
    mutate(name="overall.bmi.complete"))

fit<-fit %>%
  pivot_wider(names_from = name, values_from = c(aic,bic)) %>%
  select(type ,aic_overall.bmi.complete, bic_overall.bmi.complete)

kable(fit %>% 
        mutate(type=ifelse(type=="rcs3",
                           "restricted cubic spline (3 knots)",
                           ifelse(type=="rcs4",
                                  "restricted cubic spline (4 knots)",
                                  ifelse(type=="rcs5",
                                         "restricted cubic spline (5 knots)",      
                                         type)))), 
      col.names = c("Type",  
                    "AIC","BIC"))  %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  %>%
  add_header_above( c("","Complete adjustment" = 2))  %>%
  add_header_above( c("","Overall" = 2))%>%
  add_header_above(c("From diagnosed with COVID-19 to death" = 3))
```

# Transition 6: From hospitalised with COVID-19 to death
```{r}
r.hazard<-get.r.hazard(model=models.hospitalised.death,
                       bmi.1=22,
                       bmi.2=seq(15,60, 1)) 
fit<-rbind(
  #overall - complete
  r.hazard %>% 
    filter(str_detect(model, ".overall")) %>% 
    filter(str_detect(model, ".bmi.complete")) %>% 
    select(model, aic, bic) %>% 
    distinct() %>% 
    mutate(bic=nice.num(bic)) %>% 
    mutate(aic=nice.num(aic)) %>% 
    mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
    select(type, aic, bic) %>% 
    mutate(name="overall.bmi.complete"))

fit<-fit %>%
  pivot_wider(names_from = name, values_from = c(aic,bic)) %>%
  select(type ,aic_overall.bmi.complete, bic_overall.bmi.complete)

kable(fit %>% 
        mutate(type=ifelse(type=="rcs3",
                           "restricted cubic spline (3 knots)",
                           ifelse(type=="rcs4",
                                  "restricted cubic spline (4 knots)",
                                  ifelse(type=="rcs5",
                                         "restricted cubic spline (5 knots)",      
                                         type)))), 
      col.names = c("Type",  
                    "AIC","BIC"))  %>%
  kable_styling(bootstrap_options = c("striped", "bordered"))  %>%
  add_header_above( c("","Complete adjustment" = 2))  %>%
  add_header_above( c("","Overall" = 2))%>%
  add_header_above(c("From hospitalised with COVID-19 to death" = 3))
```

```{r }
plot.data<-rbind(
  get.r.hazard(model=list("bmi"=models.healthy.diagnosis$m.healthy.diagnosis.overall.rcs3.bmi.complete),
               bmi.1=22,
               bmi.2=seq(15, 60, 1))%>% 
    select(hr,hr.low, hr.high,rel.bmi) %>%
    mutate(trans="From general population to diagnosed with COVID-19",
           strata="Overall",
           model="Complete adjustment"),
  
  get.r.hazard(model=list("bmi"=models.healthy.hospitalised$m.healthy.hospitalised.overall.quadratic.bmi.complete),
               bmi.1=22,
               bmi.2=seq(15, 60, 1))%>%
    select(hr,hr.low, hr.high,rel.bmi) %>%
    mutate(trans="From general population to hospitalised with COVID-19",
           strata="Overall",
           model="Complete adjustment"),
  
  
  get.r.hazard(model=list("bmi"=models.diagnosis.hospitalised$m.diagnosis.hospitalised.overall.rcs3.bmi.complete),
               bmi.1=22,
               bmi.2=seq(15, 60, 1))%>%
    select(hr,hr.low, hr.high,rel.bmi) %>%
    mutate(trans="From diagnosed with COVID-19 to hospitalised with COVID-19",
           strata="Overall",
           model="Complete adjustment"),
  
  get.r.hazard(model=list("bmi"=models.diagnosis.death$m.diagnosis.death.overall.rcs3.bmi.complete),
               bmi.1=22,
               bmi.2=seq(15, 60, 1))%>%
    select(hr,hr.low, hr.high,rel.bmi) %>%
    mutate(trans="From diagnosed with COVID-19 to death",
           strata="Overall",
           model="Complete adjustment"),
  get.r.hazard(model=list("bmi"=models.hospitalised.death$m.hospitalised.death.overall.quadratic.bmi.complete),
               bmi.1=22,
               bmi.2=seq(15, 60, 1))%>%
    select(hr,hr.low, hr.high,rel.bmi) %>%
    mutate(trans="From hospitalised with COVID-19 to death",
           strata="Overall",
           model="Complete adjustment"))
save(plot.data, file="//epofs/apistillo/CHARYBDIS/Multistate model/plot.data_imp.rData")

# 
plot.data1<-plot.data %>% 
  mutate(trans=factor(trans,
                      levels=c("From general population to diagnosed with COVID-19",
                               "From general population to hospitalised with COVID-19",
                               "From general population to death",
                               "From diagnosed with COVID-19 to hospitalised with COVID-19",
                               "From diagnosed with COVID-19 to death",
                               "From hospitalised with COVID-19 to death"
                      )))  %>%
  mutate(strata=factor(str_to_sentence(strata),
                       levels=c("Overall","Aged 18 to 59","Aged 60 to 79","Aged 80 or above","Female","Male", "Overall5y", "March", "April"))) %>% 
  mutate(model=factor(model,levels=c("Unadjusted", "Adjusted for age and sex", "Complete adjustment")),
         model=recode(model, "Complete adjustment"="Fully adjusted model"))

plot.data1$trans<-plyr::revalue(plot.data1$trans, c("From general population to diagnosed with COVID-19"=
                                                    "From general\npopulation\nto diagnosed\nwith COVID-19", 
                                                  "From general population to hospitalised with COVID-19"=
                                                    "From general\npopulation\nto hospitalised\nwith COVID-19",
                                                  "From general population to death"=
                                                    "From general\npopulation\nto death",
                                                  "From diagnosed with COVID-19 to hospitalised with COVID-19"=
                                                    "From diagnosed\nwith COVID-19\nto hospitalised\nwith COVID-19",
                                                  "From diagnosed with COVID-19 to death"=
                                                    "From diagnosed\nwith COVID-19\nto death",
                                                  "From hospitalised with COVID-19 to death"=
                                                    "From hospitalised\nwith COVID-19\nto death")) 

# 
gg.rel.hazards.bmi.imp<-
  plot.data1 %>%
  filter(strata=="Overall", model =="Fully adjusted model") %>%
  ggplot(aes(group=model, colour=model))+
  facet_grid(trans~., switch="y")+
  geom_line(aes(rel.bmi, hr), size=2) +
  geom_ribbon(aes(x=rel.bmi, y=hr,ymin=hr.low, ymax=hr.high, group=model, fill=model), alpha=0.4, linetype=0,show.legend=F)+
  scale_y_continuous(position = "right")+
  geom_hline(yintercept = 1, colour = "#000000",
             linetype=2) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =element_text(size=12),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #axis.text.x=element_text(size=12),
        #axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  coord_cartesian(ylim = c(0, 4), xlim=c(15, 50))+
  labs(title = "Relative hazard ratios for BMI and risk\nof transitions. Reference BMI: 22")

gg.rel.hazards.bmi.imp

ggsave( "gg.rel.hazards.bmi.imp.png",gg.rel.hazards.bmi.imp,
        dpi=300,
        width = 5.5, height = 11)

```

# Table S6. Hazards ratios of COVID-19 outcomes related to body mass index, with 95% CIs imputing values for BMI, smoking status and the MEDEA deprivation index.
```{r}

plot.data1<-plot.data%>%
  filter(rel.bmi%in%c(16,19,22,25,28,31,34,37,40,43,47,50),
         strata%in%c("Overall"),model=="Complete adjustment")%>%
  mutate(hr=round(hr, 2), hr.low=round(hr.low, 2), hr.high=round(hr.high, 2))%>%
  mutate(hr = paste0(hr, " (", hr.low, "-", hr.high, ")"))%>%
  select(hr, rel.bmi, trans, strata)%>%
  separate(trans, sep=" to ", into=c("from", "to"), remove=T)%>%
  pivot_wider(names_from = c("from","to","strata"), values_from="hr", names_sep=" to ")%>%
  select(contains("bmi"), contains("general population to diagnosed"),
         contains("general population to hospitalised"), 
         contains("diagnosed with COVID-19 to hospitalised"),
         contains("diagnosed with COVID-19 to death"),
         contains("hospitalised with COVID-19 to death"))

kable(plot.data1, col.names = c("BMI values (kg/m2)",
                                "Overall",
                                "Overall",
                                "Overall",
                                "Overall",
                                "Overall"))%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))%>%
  add_header_above(c(" "=1,
                     "to diagnosed with COVID-19"=1,
                     "to hospitalised with COVID-19"=1,
                     "to hospitalised with COVID-19"=1,
                     "to death"=1,
                     "to death"=1))%>%
  add_header_above(c(" "=1,
                     "From general population"=2,
                     "From diagnosed with COVID-19"=2,
                     "From hospitalised with COVID-19"=1))


```
