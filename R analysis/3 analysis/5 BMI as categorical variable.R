# HR for BMI as categorical variable

library(dplyr)
library(rms)
library(stringr)
library(tidyr)
library(kableExtra)

load("1 data prep/working data.rData")

#group obesity categories
e<-eapply(.GlobalEnv, function(x) {
  x<-x%>%
    filter(!is.na(bmi.all_time_gr))%>%
    mutate(age_gr.b= ifelse(age<=59, "Aged 18 to 59",
                            ifelse(age<=79, "Aged 60 to 79",
                                   "Aged 80 or above" ))) %>%
    mutate(age_gr.b = factor(age_gr.b, levels = c("Aged 18 to 59", "Aged 60 to 79","Aged 80 or above"))) %>%
    filter(!is.na(smoke.all_time))%>%
    mutate(smoke.all_time=factor(smoke.all_time))%>%
    mutate(smoke.all_time=relevel(smoke.all_time, "Never smoker"))%>%
    filter(!is.na(medea))%>%
    mutate(medea=relevel(medea, "U1"))%>%
    droplevels()
  levels(x$bmi.all_time_gr)<-list("normal or underweight"="normal or underweight",
                                  "overweight" ="overweight",
                                  "obese"=c("obese class I", "obese class II", "obese class III"))
  output <- x
}
)

nms <- names(e)
for ( n in nms ){
  assign(n, e[[n]])
}

nice.num<-function(x){
  prettyNum(x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE)}
nice.num2<-function(x){
  prettyNum(x, big.mark=",", nsmall = 2, digits=2,scientific = FALSE)}


# funcion for models
dd<<-datadist(r); options(datadist = "dd" )
get.models<-function(transition.data, 
                     transition.name){  
  
  #models<-list()
  summary.models<-NULL
  
  # overall models -----
  # 1 unadjusted
  m.working<-cph(Surv(time, status)~ bmi.all_time_gr,
                 surv=TRUE,x=TRUE,y=TRUE,
                 data = transition.data)
  working.summary<-as.data.frame(summary(m.working, bmi.all_time_gr= 'normal or underweight', antilog=FALSE)) 
  working.summary$var<-row.names(working.summary)
  working.summary$gender<-"Overall"
  working.summary$age_gr.b<-"Overall"
  working.summary$model<-"unadjusted"
  working.summary$transition.name<-transition.name
  working.summary<-working.summary %>% 
    mutate(hr=exp(Effect),
           hr.low=exp(`Lower 0.95`),
           hr.high=exp(`Upper 0.95`)) %>% 
    select(transition.name,var,model,gender, age_gr.b, hr, hr.low, hr.high)
  # output
  summary.models<-rbind(summary.models, 
                        working.summary)
  
  # 2 adjusted for age and gender
  # age based on previous analysis
  
  if(transition.name %in% 
     c("From general population to diagnosed with COVID-19",
       "From diagnosed with COVID-19 to hospitalised with COVID-19")){
    m.working<-cph(Surv(time, status)~ bmi.all_time_gr 
                   +gender+rcs(age,5),
                   surv=TRUE,x=TRUE,y=TRUE,
                   data = transition.data)  
    
  } else {
    m.working<-cph(Surv(time, status)~ bmi.all_time_gr
                   +gender+rcs(age,5),
                   surv=TRUE,x=TRUE,y=TRUE,
                   data = transition.data)  
    
  }
  working.summary<-as.data.frame(summary(m.working, bmi.all_time_gr= 'normal or underweight',antilog=FALSE)) 
  working.summary$var<-row.names(working.summary)
  working.summary$gender<-"Overall"
  working.summary$age_gr.b<-"Overall"
  working.summary$model<-"Adjusted for age and sex"
  working.summary$transition.name<-transition.name
  working.summary<-working.summary %>% 
    mutate(hr=exp(Effect),
           hr.low=exp(`Lower 0.95`),
           hr.high=exp(`Upper 0.95`)) %>% 
    select(transition.name,var,model,gender, age_gr.b, hr, hr.low, hr.high)
  # output
  summary.models<-rbind(summary.models, 
                        working.summary)
  
  # 2 adjusted for age and gender, smoking and medea
  # age based on previous analysis
  
  if(transition.name %in% 
     c("From general population to diagnosed with COVID-19",
       "From diagnosed with COVID-19 to hospitalised with COVID-19")){
    m.working<-cph(Surv(time, status)~ bmi.all_time_gr 
                   +gender+rcs(age,5)+
                     medea+smoke.all_time,
                   surv=TRUE,x=TRUE,y=TRUE,
                   data = transition.data)  
    
  } else {
    m.working<-cph(Surv(time, status)~ bmi.all_time_gr+gender+rcs(age,5)+
                     medea+smoke.all_time,
                   surv=TRUE,x=TRUE,y=TRUE,
                   data = transition.data)  
    
  }
  working.summary<-as.data.frame(summary(m.working, bmi.all_time_gr= 'normal or underweight',antilog=FALSE)) 
  working.summary$var<-row.names(working.summary)
  working.summary$gender<-"Overall"
  working.summary$age_gr.b<-"Overall"
  working.summary$model<-"Complete adjustment"
  working.summary$transition.name<-transition.name
  working.summary<-working.summary %>% 
    mutate(hr=exp(Effect),
           hr.low=exp(`Lower 0.95`),
           hr.high=exp(`Upper 0.95`)) %>% 
    select(transition.name,var,model,gender, age_gr.b, hr, hr.low, hr.high)
  # output
  summary.models<-rbind(summary.models, 
                        working.summary)
  
  summary.models
}




summary.m.healthy.diagnosis<-get.models(transition.data=r.healthy.diagnosis,
                                        transition.name="From general population to diagnosed with COVID-19")
summary.m.healthy.hospitalised<-get.models(transition.data=r.healthy.hospitalised,
                                           transition.name="From general population to hospitalised with COVID-19")
summary.m.diagnosis.hospitalised<-get.models(transition.data=r.diagnosis.hospitalised,
                                             transition.name="From diagnosed with COVID-19 to hospitalised with COVID-19")
summary.m.diagnosis.death<-get.models(transition.data=r.diagnosis.death,
                                      transition.name="From diagnosed with COVID-19 to death")
summary.m.hospitalised.death<-get.models(transition.data=r.hospitalised.death,
                                         transition.name="From hospitalised with COVID-19 to death")

estimates<-rbind(
  summary.m.healthy.diagnosis,
  summary.m.healthy.hospitalised,
  summary.m.diagnosis.hospitalised,
  summary.m.diagnosis.death,
  summary.m.hospitalised.death)
# just the estimated effect of comorbidities
estimates<-estimates%>%
  filter(!var %in% c("age","gender - Male:Female"),  !str_detect(var, "medea|smoke"))

estimates<-estimates%>%
  mutate(group=ifelse(!gender=="Overall", gender, age_gr.b))


estimates<-estimates %>% 
  mutate(var=str_replace(var, "bmi.all_time_gr - overweight:normal or underweight" , "overweight")) %>%
  mutate(var=str_replace(var, "bmi.all_time_gr - obese:normal or underweight" , "obese")) 


summary.table<-estimates %>% 
  mutate(est=paste0(nice.num2(hr), " (",
                    nice.num2(hr.low), " to ",
                    nice.num2(hr.high), ")"
  )) %>% 
  mutate(group=paste0(model, "- ", gender, "- ",age_gr.b)) %>% 
  select(transition.name, var,group, est)


summary.table<-summary.table %>% 
  pivot_wider(
    names_from = group,
    values_from=est) %>% 
  select("transition.name", "var",
         "unadjusted- Overall- Overall" , "Adjusted for age and sex- Overall- Overall",
         "Complete adjustment- Overall- Overall",
         "unadjusted- Overall- Aged 18 to 59", "Adjusted for age and sex- Overall- Aged 18 to 59" ,
         "Complete adjustment- Overall- Aged 18 to 59",
         "unadjusted- Overall- Aged 60 to 79", "Adjusted for age and sex- Overall- Aged 60 to 79" ,
         "Complete adjustment- Overall- Aged 60 to 79",
         "unadjusted- Overall- Aged 80 or above", "Adjusted for age and sex- Overall- Aged 80 or above" ,
         "Complete adjustment- Overall- Aged 80 or above",
         "unadjusted- Female- Overall", "Adjusted for age- Female- Overall" ,
         "Complete adjustment- Female- Overall",
         "unadjusted- Male- Overall", "Adjusted for age- Male- Overall" ,
         "Complete adjustment- Male- Overall", )



test<-kable(summary.table,
            col.names = c("Transition", "Variable", 
                          "Unadjusted", "Adjusted for age and sex","Complete adjustment",
                          "Unadjusted", "Adjusted for age and sex","Complete adjustment",
                          "Unadjusted", "Adjusted for age and sex","Complete adjustment",
                          "Unadjusted", "Adjusted for age and sex","Complete adjustment",
                          "Unadjusted", "Adjusted for age","Complete adjustment",
                          "Unadjusted", "Adjusted for age","Complete adjustment"
            )) %>% 
  add_header_above( c("", "",
                      "Overall" = 3,
                      "Aged 18 to 59" = 3,
                      "Aged 60 to 79" = 3,
                      "Aged 80 or above"=3,
                      "Female" = 3,
                      "Male" = 3))  %>% 
  pack_rows("From general population", 1, 4)%>%
  pack_rows("From diagnosed", 5, 8)%>%
  pack_rows("From hospitalised", 9, 10)%>%
  kable_styling(bootstrap_options = c("striped", "bordered"))
collapse_rows(test, columns = 1)


plot.data<-estimates %>% 
  mutate(transition.name=
           ifelse(transition.name==
                    "From general population to diagnosed with COVID-19",
                  "From general populaton\nto diagnosed\nwith COVID-19",
                  ifelse(transition.name==     
                           "From general population to hospitalised with COVID-19",
                         "From general population\nto hospitalised\nwith COVID-19",
                         ifelse(transition.name==
                                  "From diagnosed with COVID-19 to hospitalised with COVID-19",
                                "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
                                ifelse(transition.name== 
                                         "From diagnosed with COVID-19 to death",
                                       "From diagnosed with\nCOVID-19 to\ndeath",
                                       ifelse(transition.name== 
                                                "From hospitalised with COVID-19 to death",
                                              "From hospitalised\nwith COVID-19\nto death",
                                              NA)))))) %>% 
  mutate(transition.name=factor(transition.name,
                                levels=c("From general populaton\nto diagnosed\nwith COVID-19",
                                         "From general population\nto hospitalised\nwith COVID-19",
                                         "From diagnosed with\nCOVID-19 to hospitalised\nwith COVID-19",
                                         "From diagnosed with\nCOVID-19 to\ndeath",
                                         "From hospitalised\nwith COVID-19\nto death"
                                ))) 


##save data
save(plot.data,file=file.path("//epofs/apistillo/CHARYBDIS/SavedData", "plot.dataHRcategorical.Rda"))
load(paste0("//epofs/apistillo/CHARYBDIS/SavedData/", "plot.dataHRcategorical.Rda"))
library(dplyr)
library(rms)
library(stringr)
library(tidyr)
library(kableExtra)
library(gridExtra)

order<-c("overweight", "obese")
plot.data$var<-factor(plot.data$var, 
                      levels=rev(order))

plot.data<-plot.data %>% 
  filter(model=="Complete adjustment", group=="Overall")%>%
  mutate(var = recode(var, "overweight"="Overweight","obese"="Obese"))

plot.overall.complete<-  
  ggplot(plot.data,aes(x=hr,
                       y=model,
                       xmin=hr.low,
                       xmax=hr.high,
                       colour=var))+
  xlim(c(-0.2,2.3))+
  facet_grid(transition.name~factor(group, levels=c("Overall")), 
             drop=TRUE, switch="y", scales = "free")+
  geom_point(size=2,position=position_dodge(width=1))+
  geom_errorbar(width=0, size=1,position=position_dodge(width=1))+
  
  geom_vline(xintercept = 1, colour = "#000000", 
             linetype=2) +
  theme_bw()+
  theme(axis.ticks.y=element_blank(),
        panel.spacing = unit(0, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text =element_text(size=12),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text=element_text(size=12),
        panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x  = element_text(size=12, face="bold"),
        strip.background.x = element_rect( fill="#f7f7f7"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)
  ) +
  xlab("\nHazard ratio")+
  guides(colour = guide_legend(reverse = T))+
  geom_text(aes(x=2.8,label=hr),size=4,show.legend=FALSE)

plot.data<-plot.data %>% 
  mutate(est=paste0(nice.num2(hr), " (",
                    nice.num2(hr.low), " - ",
                    nice.num2(hr.high), ")"
  )) 
data_table <- ggplot(data = plot.data%>%mutate(group=recode(group, "Overall"="HR (95% CI)")),
                     aes(x=hr,y=model, colour=var))+
  facet_grid(transition.name~factor(group, levels=c("HR (95% CI)")), 
             drop=TRUE, switch="y")+
  #geom_hline(aes(yintercept = labels, colour = colour), size = 7) +
  geom_text(aes(x = 0, y = model, label = est), position=position_dodge(width=1)) +
  theme(plot.margin = margin(t = 7, r = 200, b = 78, l = 0, unit = "pt"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        #panel.spacing = unit(0, "lines"),
        legend.position = "none",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text =element_text(size=12),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        axis.text=element_blank(),
        #panel.grid.major.x= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        axis.title=element_text(size=14,face="bold"),
        strip.text.x  = element_text(size=12, face="bold"),
        # strip.background.x = element_blank(),
        strip.background = element_blank(),
        strip.text.y.left = element_blank()
  )+
  scale_color_manual(breaks=c("Overweight","Obese"), values=c("black", "black"))+
  xlab("")

forest<-grid.arrange(plot.overall.complete,data_table, ncol = 2)

ggsave("plot.forest.png",forest,
       dpi=300,
       width = 9, height = 8)

