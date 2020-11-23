
## 4 functions

# 1) For printing numbers
nice.num<-function(x){
  prettyNum(x, big.mark=",", nsmall = 0, digits=0,scientific = FALSE)}

nice.num2<-function(x){
  prettyNum(x, big.mark=",", nsmall = 2, digits=2,scientific = FALSE)}

# 2) Models
get.models<-function(r.data,name){
  # will add models to list-----
  models<-list()
  
  
  ## overall models ------
  #### also complete adjustment with bmi.5y
  ## calendar
  ## 18-59 models ------
  ## 60-79 models ------
  ## 80 or above models ------
  ## female models ------
  ## male models ------
  
  ## overall models ------
  
  #data
  working.data<-r.data 
  dd<<-datadist(working.data); options(datadist = "dd" )
  # bmi only  
  models[[paste0("m.",name, ".overall.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                   surv=TRUE,x=TRUE,y=TRUE,
                                                                   data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
  #adjusment for age and gender
  models[[paste0("m.",name, ".overall.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+   
                                                                           age+gender,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                              age+gender,
                                                                            surv=TRUE,x=TRUE,y=TRUE,
                                                                            data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
  
  #complete adjustment
  models[[paste0("m.",name, ".overall.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+        
                                                                         age+gender+ medea+smoke.all_time,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                            age+gender+ medea+smoke.all_time,
                                                                          surv=TRUE,x=TRUE,y=TRUE,
                                                                          data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
  
  # for sensitivity analysis - complete adjustment bmi 5 years
  models[[paste0("m.",name, ".overall.linear", ".bmi5y.complete")]]<-cph(Surv(time, status) ~ bmi.5y+   
                                                                   age+gender+ medea+smoke.all_time,
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.quadratic", ".bmi5y.complete")]]<-cph(Surv(time, status) ~ pol(bmi.5y,2)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs3", ".bmi5y.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.5y,3)+
                                                                 age+gender+ medea+smoke.all_time,
                                                               surv=TRUE,x=TRUE,y=TRUE,
                                                               data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs4", ".bmi5y.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.5y,4)+
                                                                 age+gender+ medea+smoke.all_time,
                                                               surv=TRUE,x=TRUE,y=TRUE,
                                                               data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".overall.rcs5", ".bmi5y.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.5y,5)+
                                                                 age+gender+ medea+smoke.all_time,
                                                               surv=TRUE,x=TRUE,y=TRUE,
                                                               data = working.data, iter.max = 100)
  
  
  ## models stratified by calendar month -- march, just complete adjustment and for the first transition
  
  if (name=="healthy.diagnosis"){
    #data
    working.data<-r.data 
    dd<<-datadist(working.data); options(datadist = "dd" )
    working.data <- survSplit(Surv(time, status) ~ bmi.all_time + age + gender + medea + smoke.all_time,  
                              working.data,          
                              cut=c(31, 61), episode ="timegroup")
    working.data.march<-working.data %>% filter(timegroup==1)
    working.data.april<-working.data %>% filter(timegroup==2)
    
    
    #complete adjustment
    models[[paste0("m.",name, ".march.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+  
                                                                         age+gender+ medea+smoke.all_time,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data.march, iter.max = 100)
    models[[paste0("m.",name, ".march.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                            age+gender+ medea+smoke.all_time,
                                                                          surv=TRUE,x=TRUE,y=TRUE,
                                                                          data = working.data.march, iter.max = 100)
    models[[paste0("m.",name, ".march.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.march, iter.max = 100)
    models[[paste0("m.",name, ".march.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.march, iter.max = 100)
    models[[paste0("m.",name, ".march.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.march, iter.max = 100)
    
    ## models stratified by calendar month -- april, just complete adjustment
    
    #data
    #complete adjustment
    models[[paste0("m.",name, ".april.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+    
                                                                         age+gender+ medea+smoke.all_time,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data.april, iter.max = 100)
    models[[paste0("m.",name, ".april.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                            age+gender+ medea+smoke.all_time,
                                                                          surv=TRUE,x=TRUE,y=TRUE,
                                                                          data = working.data.april, iter.max = 100)
    models[[paste0("m.",name, ".april.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.april, iter.max = 100)
    models[[paste0("m.",name, ".april.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.april, iter.max = 100)
    models[[paste0("m.",name, ".april.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data.april, iter.max = 100)
  }
  # ## models for age 18-69  ------
  # #data
  # working.data<-r.data %>% filter(age_gr.b=="Aged 18 to 69")
  # dd<<-datadist(working.data); options(datadist = "dd" )
  # # bmi only
  # models[[paste0("m.",name, ".firstage.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
  #                                                                   surv=TRUE,x=TRUE,y=TRUE,
  #                                                                   data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
  #                                                                      surv=TRUE,x=TRUE,y=TRUE,
  #                                                                      data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
  #                                                                 surv=TRUE,x=TRUE,y=TRUE,
  #                                                                 data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
  #                                                                 surv=TRUE,x=TRUE,y=TRUE,
  #                                                                 data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
  #                                                                 surv=TRUE,x=TRUE,y=TRUE,
  #                                                                 data = working.data, iter.max = 100)
  # #adjusment for age and gender
  # models[[paste0("m.",name, ".firstage.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+
  #                                                                           age+gender,
  #                                                                         surv=TRUE,x=TRUE,y=TRUE,
  #                                                                         data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
  #                                                                              age+gender,
  #                                                                            surv=TRUE,x=TRUE,y=TRUE,
  #                                                                            data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
  #                                                                         age+gender,
  #                                                                       surv=TRUE,x=TRUE,y=TRUE,
  #                                                                       data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
  #                                                                         age+gender,
  #                                                                       surv=TRUE,x=TRUE,y=TRUE,
  #                                                                       data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
  #                                                                         age+gender,
  #                                                                       surv=TRUE,x=TRUE,y=TRUE,
  #                                                                       data = working.data, iter.max = 100)
  # 
  # #complete adjustment
  # models[[paste0("m.",name, ".firstage.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+
  #                                                                         age+gender+ medea+smoke.all_time,
  #                                                                       surv=TRUE,x=TRUE,y=TRUE,
  #                                                                       data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
  #                                                                            age+gender+ medea+smoke.all_time,
  #                                                                          surv=TRUE,x=TRUE,y=TRUE,
  #                                                                          data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
  #                                                                       age+gender+ medea+smoke.all_time,
  #                                                                     surv=TRUE,x=TRUE,y=TRUE,
  #                                                                     data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
  #                                                                       age+gender+ medea+smoke.all_time,
  #                                                                     surv=TRUE,x=TRUE,y=TRUE,
  #                                                                     data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".firstage.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
  #                                                                       age+gender+ medea+smoke.all_time,
  #                                                                     surv=TRUE,x=TRUE,y=TRUE,
  #                                                                     data = working.data, iter.max = 100)
  # 
  # ## models for age more than 70  ------
  # #data
  # working.data<-r.data %>% filter(age_gr.b=="Aged 70 or above")
  # dd<<-datadist(working.data); options(datadist = "dd" )
  # # bmi only
  # models[[paste0("m.",name, ".secondage.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
  #                                                                    surv=TRUE,x=TRUE,y=TRUE,
  #                                                                    data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
  #                                                                       surv=TRUE,x=TRUE,y=TRUE,
  #                                                                       data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
  #                                                                  surv=TRUE,x=TRUE,y=TRUE,
  #                                                                  data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
  #                                                                  surv=TRUE,x=TRUE,y=TRUE,
  #                                                                  data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
  #                                                                  surv=TRUE,x=TRUE,y=TRUE,
  #                                                                  data = working.data, iter.max = 100)
  # #adjusment for age and gender
  # models[[paste0("m.",name, ".secondage.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+
  #                                                                            age+gender,
  #                                                                          surv=TRUE,x=TRUE,y=TRUE,
  #                                                                          data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
  #                                                                               age+gender,
  #                                                                             surv=TRUE,x=TRUE,y=TRUE,
  #                                                                             data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
  #                                                                          age+gender,
  #                                                                        surv=TRUE,x=TRUE,y=TRUE,
  #                                                                        data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
  #                                                                          age+gender,
  #                                                                        surv=TRUE,x=TRUE,y=TRUE,
  #                                                                        data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
  #                                                                          age+gender,
  #                                                                        surv=TRUE,x=TRUE,y=TRUE,
  #                                                                        data = working.data, iter.max = 100)
  # 
  # #complete adjustment
  # models[[paste0("m.",name, ".secondage.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+
  #                                                                          age+gender+ medea+smoke.all_time,
  #                                                                        surv=TRUE,x=TRUE,y=TRUE,
  #                                                                        data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
  #                                                                             age+gender+ medea+smoke.all_time,
  #                                                                           surv=TRUE,x=TRUE,y=TRUE,
  #                                                                           data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
  #                                                                        age+gender+ medea+smoke.all_time,
  #                                                                      surv=TRUE,x=TRUE,y=TRUE,
  #                                                                      data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
  #                                                                        age+gender+ medea+smoke.all_time,
  #                                                                      surv=TRUE,x=TRUE,y=TRUE,
  #                                                                      data = working.data, iter.max = 100)
  # models[[paste0("m.",name, ".secondage.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
  #                                                                        age+gender+ medea+smoke.all_time,
  #                                                                      surv=TRUE,x=TRUE,y=TRUE,
  #                                                                      data = working.data, iter.max = 100)


## models for age 18-59  ------
#data
working.data<-r.data %>% filter(age_gr.b=="Aged 18 to 59")
dd<<-datadist(working.data); options(datadist = "dd" )
# bmi only
models[[paste0("m.",name, ".firstage.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
#adjusment for age and gender
models[[paste0("m.",name, ".firstage.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                          age+gender,
                                                                        surv=TRUE,x=TRUE,y=TRUE,
                                                                        data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                             age+gender,
                                                                           surv=TRUE,x=TRUE,y=TRUE,
                                                                           data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)

#complete adjustment
models[[paste0("m.",name, ".firstage.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                        age+gender+ medea+smoke.all_time,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                           age+gender+ medea+smoke.all_time,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
models[[paste0("m.",name, ".firstage.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)

## models for age 60-79  ------
#data
working.data<-r.data %>% filter(age_gr.b=="Aged 60 to 79")
dd<<-datadist(working.data); options(datadist = "dd" )
# bmi only
models[[paste0("m.",name, ".secondage.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                   surv=TRUE,x=TRUE,y=TRUE,
                                                                   data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                                 surv=TRUE,x=TRUE,y=TRUE,
                                                                 data = working.data, iter.max = 100)
#adjusment for age and gender
models[[paste0("m.",name, ".secondage.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                           age+gender,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                              age+gender,
                                                                            surv=TRUE,x=TRUE,y=TRUE,
                                                                            data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                         age+gender,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)

#complete adjustment
models[[paste0("m.",name, ".secondage.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                         age+gender+ medea+smoke.all_time,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                            age+gender+ medea+smoke.all_time,
                                                                          surv=TRUE,x=TRUE,y=TRUE,
                                                                          data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
models[[paste0("m.",name, ".secondage.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                       age+gender+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)

## models for age 80 or above  ------
#data
working.data<-r.data %>% filter(age_gr.b=="Aged 80 or above")
dd<<-datadist(working.data); options(datadist = "dd" )
# bmi only
models[[paste0("m.",name, ".thirdage.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
#adjusment for age and gender
models[[paste0("m.",name, ".thirdage.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                          age+gender,
                                                                        surv=TRUE,x=TRUE,y=TRUE,
                                                                        data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                             age+gender,
                                                                           surv=TRUE,x=TRUE,y=TRUE,
                                                                           data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                        age+gender,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)

#complete adjustment
models[[paste0("m.",name, ".thirdage.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+
                                                                        age+gender+ medea+smoke.all_time,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                           age+gender+ medea+smoke.all_time,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
models[[paste0("m.",name, ".thirdage.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                      age+gender+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)

  ## models for female ------
  #data
  working.data<-r.data %>% filter(gender=="Female")
  dd<<-datadist(working.data); options(datadist = "dd" )
  # bmi only  
  models[[paste0("m.",name, ".female.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
  #adjusment for age and gender
  models[[paste0("m.",name, ".female.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+ 
                                                                          age,
                                                                        surv=TRUE,x=TRUE,y=TRUE,
                                                                        data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                             age,
                                                                           surv=TRUE,x=TRUE,y=TRUE,
                                                                           data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                        age,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                        age,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                        age,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  
  #complete adjustment
  models[[paste0("m.",name, ".female.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+  
                                                                        age+ medea+smoke.all_time,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                           age+ medea+smoke.all_time,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                      age+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs4", ".bmi.compplete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                       age+ medea+smoke.all_time,
                                                                     surv=TRUE,x=TRUE,y=TRUE,
                                                                     data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".female.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                      age+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  
  ## models for male ------
  #data
  working.data<-r.data %>% filter(gender=="Male")
  dd<<-datadist(working.data); options(datadist = "dd" )
  # bmi only  
  models[[paste0("m.",name, ".male.linear", ".bmi.only")]]<-cph(Surv(time, status) ~ bmi.all_time,
                                                                surv=TRUE,x=TRUE,y=TRUE,
                                                                data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.quadratic", ".bmi.only")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2),
                                                                   surv=TRUE,x=TRUE,y=TRUE,
                                                                   data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs3", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3),
                                                              surv=TRUE,x=TRUE,y=TRUE,
                                                              data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs4", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4),
                                                              surv=TRUE,x=TRUE,y=TRUE,
                                                              data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs5", ".bmi.only")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5),
                                                              surv=TRUE,x=TRUE,y=TRUE,
                                                              data = working.data, iter.max = 100)
  #adjusment for age and gender
  models[[paste0("m.",name, ".male.linear", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ bmi.all_time+    
                                                                        age,
                                                                      surv=TRUE,x=TRUE,y=TRUE,
                                                                      data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.quadratic", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                           age,
                                                                         surv=TRUE,x=TRUE,y=TRUE,
                                                                         data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs3", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                      age,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs4", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                      age,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs5", ".bmi.age.gender")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                      age,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  
  #complete adjustment
  models[[paste0("m.",name, ".male.linear", ".bmi.complete")]]<-cph(Surv(time, status) ~ bmi.all_time+     
                                                                      age+ medea+smoke.all_time,
                                                                    surv=TRUE,x=TRUE,y=TRUE,
                                                                    data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.quadratic", ".bmi.complete")]]<-cph(Surv(time, status) ~ pol(bmi.all_time,2)+
                                                                         age+ medea+smoke.all_time,
                                                                       surv=TRUE,x=TRUE,y=TRUE,
                                                                       data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs3", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,3)+
                                                                    age+ medea+smoke.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs4", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,4)+
                                                                    age+ medea+smoke.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)
  models[[paste0("m.",name, ".male.rcs5", ".bmi.complete")]]<-cph(Surv(time, status) ~ rcs(bmi.all_time,5)+
                                                                    age+ medea+smoke.all_time,
                                                                  surv=TRUE,x=TRUE,y=TRUE,
                                                                  data = working.data, iter.max = 100)

  
  ## output ----
  models
}


# 3) To get relative hazards for each model in list
# also add in AIC and BIC
get.r.hazard<-function(models,   #list of models
                       bmi.1,    # reference bmi.all_time 
                       bmi.2){   # comparison bmi.all_times
  
  complete.bmi<-names(models) %>% 
    str_detect("bmi5y", negate=TRUE)%>%
    keep(models, .)
  
  five.bmi<-names(models) %>% 
    str_detect("bmi5y")%>%
    keep(models, .)
  
  output1<-lapply(complete.bmi, function(x) {
    model<- x
    output<-NULL
    
    for(i in 1:length(bmi.2)){  
      
      working.summary<-head(as.data.frame(summary({{model}}, 
                                                  bmi.all_time=c({{bmi.1}},{{bmi.2[i]}}), antilog=FALSE)),1)  # 1st- bmi.all_time
      working.summary<-working.summary %>% 
        select(Effect,`Lower 0.95`, `Upper 0.95`) %>% 
        mutate(hr=exp(Effect),
               hr.low=exp(`Lower 0.95`),
               hr.high=exp(`Upper 0.95`)) %>% 
        mutate(ref.bmi=bmi.1,
               rel.bmi=bmi.2[i]) %>% 
        select(hr, hr.low, hr.high, ref.bmi, rel.bmi)
      #browser()
      working.summary$aic<-AIC({{model}})
      working.summary$bic<-BIC({{model}})
      
      output<-rbind(output, working.summary)
      
    }
    
    output
    
  })
  
  # for sensitivity analysis - 5y BMI
  output2<-lapply(five.bmi, function(x) {
    model<- x
    output<-NULL
    
    for(i in 1:length(bmi.2)){  
      
      working.summary<-head(as.data.frame(summary({{model}}, 
                                                  bmi.5y=c({{bmi.1}},{{bmi.2[i]}}), antilog=FALSE)),1)  # 1st- bmi.5y
      working.summary<-working.summary %>% 
        select(Effect,`Lower 0.95`, `Upper 0.95`) %>% 
        mutate(hr=exp(Effect),
               hr.low=exp(`Lower 0.95`),
               hr.high=exp(`Upper 0.95`)) %>% 
        mutate(ref.bmi=bmi.1,
               rel.bmi=bmi.2[i]) %>% 
        select(hr, hr.low, hr.high, ref.bmi, rel.bmi)
      #browser()
      working.summary$aic<-AIC({{model}})
      working.summary$bic<-BIC({{model}})
      
      output<-rbind(output, working.summary)
      
    }
    
    output
    
  })
  
  # to dataframe
  output1 <-bind_rows(output1, .id = "model")
  output2 <-bind_rows(output2, .id = "model")
  output <-rbind(output1, output2)
}


# 4) Select AICs and BICs for principal analysis
fit.fun<-function(hazard){
  output<-rbind(
    # overall - bmi only
    hazard %>% 
      filter(str_detect(model, ".overall")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="overall.bmi.only"),
    # overall - age gender
    hazard %>% 
      filter(str_detect(model, ".overall")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="overall.bmi.age.gender"),  
    
    #overall - complete
    hazard %>% 
      filter(str_detect(model, ".overall")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="overall.bmi.complete"), 
    
    # 1 age group
    hazard %>% 
      filter(str_detect(model, ".firstage")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="firstage.bmi.only"),
    # 1 - age gender
    hazard %>% 
      filter(str_detect(model, ".firstage")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="firstage.bmi.age.gender"),  
    
    #1 - complete
    hazard %>% 
      filter(str_detect(model, ".firstage")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="firstage.bmi.complete"), 
    
    # 2 age group
    hazard %>% 
      filter(str_detect(model, ".secondage")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="secondage.bmi.only"),
    # 2 - age gender
    hazard %>% 
      filter(str_detect(model, ".secondage")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="secondage.bmi.age.gender"),  
    
    #2 - complete
    hazard %>% 
      filter(str_detect(model, ".secondage")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="secondage.bmi.complete"), 
    
    # 3 age group
    hazard %>% 
      filter(str_detect(model, ".thirdage")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="thirdage.bmi.only"),
    # 3 - age gender
    hazard %>% 
      filter(str_detect(model, ".thirdage")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="thirdage.bmi.age.gender"),  
    
    #3 - complete
    hazard %>% 
      filter(str_detect(model, ".thirdage")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="thirdage.bmi.complete"), 
    
    # female
    hazard %>% 
      filter(str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="female.bmi.only"),
    # female - age gender
    hazard %>% 
      filter(str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="female.bmi.age.gender"),  
    
    #3 - complete
    hazard %>% 
      filter(str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="female.bmi.complete"), 
    
    
    # male
    hazard %>% 
      filter(str_detect(model, ".male")) %>% 
      filter(!str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.only")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="male.bmi.only"),
    # male - age gender
    hazard %>% 
      filter(str_detect(model, ".male")) %>% 
      filter(!str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.age.gender")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="male.bmi.age.gender"),  
    
    #3 - complete
    hazard %>% 
      filter(str_detect(model, ".male")) %>% 
      filter(!str_detect(model, ".female")) %>% 
      filter(str_detect(model, ".bmi.complete")) %>% 
      select(model, aic, bic) %>% 
      distinct() %>% 
      mutate(bic=nice.num(bic)) %>% 
      mutate(aic=nice.num(aic)) %>% 
      mutate(type=str_extract(model, paste(c("linear", "quadratic", "rcs3", "rcs4", "rcs5"), collapse="|"))) %>% 
      select(type, aic, bic) %>% 
      mutate(name="male.bmi.complete"))
}
