# Plots of HR

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(grid)

plot.data<-get(load("//epofs/apistillo/CHARYBDIS/Multistate model/plot.data.rData"))

plot.data<-plot.data %>% 
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


plot.data$trans<-plyr::revalue(plot.data$trans, c("From general population to diagnosed with COVID-19"=
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
########
# Fully adjusted Overall
#######

dat_text <- data.frame(
  label = c("p non-linearity\n < 0.001",
            "p non-linearity\n < 0.001",
            "p non-linearity\n < 0.001",
            "p non-linearity\n = 0.001",
            "p non-linearity\n < 0.001"),
  trans = c(  "From general\npopulation\nto diagnosed\nwith COVID-19", 
              "From general\npopulation\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto death",
              "From hospitalised\nwith COVID-19\nto death"),
  model="Fully adjusted model",
  x     = 22,
  y     = 3.5
)

gg.rel.hazards.bmi.overall<-
  plot.data %>% 
  filter(strata=="Overall", model =="Fully adjusted model") %>% 
  ggplot(aes(group=model, colour=model))+
  facet_grid(trans~., switch="y")+
  geom_line(aes(rel.bmi, hr), size=2) +
  geom_ribbon(aes(x=rel.bmi, y=hr,ymin=hr.low, ymax=hr.high, group=model, fill=model), alpha=0.4, linetype=0,show.legend=F)+
  scale_y_continuous(position = "right")+
  geom_hline(yintercept = 1, colour = "#000000", linetype=2) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text =element_text(size=12),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  coord_cartesian(ylim = c(0, 4), xlim=c(15, 50))+
  geom_text( data    = dat_text,
             mapping = aes(x = x, y = y, label = label, fill=NULL, colour=NULL), 
             size=4,show.legend=FALSE, inherit.aes=T)+ 
  labs(title = "Relative hazard ratios for BMI and risk\nof transitions. Reference BMI: 22")

ggsave( "rel.hazards.bmi.overall.png",gg.rel.hazards.bmi.overall,
        dpi=300,
        width = 5.5, height = 11)

###############
# general plot
##############

gg.rel.hazards.bmi.general<-plot.data%>%mutate(toplot=ifelse(strata%in%c("Aged 18 to 59", "Aged 60 to 79",
                                                        "Aged 80 or above"), "Age", "Gender"))

dat_text_age <- data.frame(
  label = c("p interaction\n < 0.0001","p interaction\n = 0.208",
            "p interaction\n = 0.1153","p interaction\n < 0.0001",
            "p interaction\n = 0.5244"),
  strata=c("Aged 60 to 79"),
  trans = c(  "From general\npopulation\nto diagnosed\nwith COVID-19", 
              "From general\npopulation\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto death",
              "From hospitalised\nwith COVID-19\nto death"),
  model="Fully adjusted model",
  x     = 33,
  y     = 3.5
)
dat_text_gender <- data.frame(
  label = c("p interaction\n < 0.0001","p interaction\n = 0.4068",
            "p interaction\n = 0.6169","p interaction\n = 0.6629",
            "p interaction\n = 0.0868"),
  strata=c("Female"),
  trans = c(  "From general\npopulation\nto diagnosed\nwith COVID-19", 
              "From general\npopulation\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto hospitalised\nwith COVID-19",
              "From diagnosed\nwith COVID-19\nto death",
              "From hospitalised\nwith COVID-19\nto death"),
  model="Fully adjusted model",
  x     = 25,
  y     = 3.5
)

gg.rel.hazards.bmi.general<-gg.rel.hazards.bmi.general %>% 
  filter(model=="Fully adjusted model", strata%in%c("Aged 18 to 59","Aged 60 to 79","Aged 80 or above","Female","Male"))%>%
  ggplot(aes(group=paste(strata,model), colour=model))+
  
  facet_grid(trans~strata, switch="y")+
  geom_line(aes(rel.bmi, hr), size=2) +
  geom_ribbon(aes(x=rel.bmi, y=hr,ymin=hr.low, ymax=hr.high, group=model, fill=model), alpha=0.4, linetype=0,show.legend=F)+
  
  geom_hline(yintercept = 1, colour = "#000000", 
             linetype=2) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text = element_text(size=12),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  coord_cartesian(ylim = c(0, 4), xlim = c(15, 50))+
  scale_y_continuous(position = "right")+
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  geom_text( data    = dat_text_age,
             mapping = aes(x = x, y = y, label = label, fill=NULL, 
                           colour=NULL), size=4,
             show.legend=FALSE, inherit.aes=T)+ 
  geom_text( data    = dat_text_gender,
             mapping = aes(x = x, y = y, label = label, fill=NULL, 
                           colour=NULL), size=4,
             show.legend=FALSE, inherit.aes=T)+ 
  labs(title = "Relative hazard ratios for BMI and risk of transitions. Reference BMI: 22")

gt <- ggplotGrob(gg.rel.hazards.bmi.general) 
panelI <- gt$layout$l[grepl("panel", gt$layout$name)]
gt$widths[panelI[1] + 5] = unit(2, "cm")
grid.draw(gt)

ggsave("gg.rel.hazards.bmi.general.png",gt,
        dpi=300,
        width = 12, height = 10)


##############
# overall con ajustes
##########

gg.rel.hazards.bmi.adj<-plot.data %>% 
  filter(strata=="Overall")
gg.rel.hazards.bmi.adj$model<-factor(gg.rel.hazards.bmi.adj$model, levels=c("Fully adjusted model", "Adjusted for age and sex", "Unadjusted"))

gg.rel.hazards.bmi.adj<-gg.rel.hazards.bmi.adj%>%
  ggplot(aes(group=model, colour=model))+
  facet_grid(trans~., switch="y")+
  geom_line(aes(rel.bmi, hr), size=2) +
  geom_ribbon(aes(x=rel.bmi, y=hr,ymin=hr.low, ymax=hr.high, group=model, fill=model), alpha=0.3, linetype=0,show.legend=F)+
  
  scale_y_continuous(position = "right")+
  scale_colour_manual(values=c("#EA622B","#3BDA00","#1435AD"))+
  scale_fill_manual(values=c("#EA622B","#3BDA00","#1435AD"))+
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
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  coord_cartesian(ylim = c(0, 4), xlim=c(15, 50))+
  labs(title = "Relative hazard ratios for BMI and risk\nof transitions. Reference BMI: 22")

ggsave( "gg.rel.hazards.bmi.adj.png",gg.rel.hazards.bmi.adj,
        dpi=300,
        width = 7, height = 11)


######## 
#  stratified for calendar (month)
##########

gg.rel.hazards.bmi.month<-plot.data %>% 
  filter(model=="Fully adjusted model", strata%in%c("March","April"))%>%
  ggplot(aes(group=paste(strata,model), colour=model))+
  facet_grid(trans~strata, scales ="free_y", switch="y")+
  geom_line(aes(rel.bmi, hr), size=2) +
  geom_ribbon(aes(x=rel.bmi, y=hr,ymin=hr.low, ymax=hr.high, group=model, fill=model), alpha=0.4, linetype=0,show.legend=F)+
  scale_y_continuous(position = "right")+
  coord_cartesian(ylim = c(0, 4), xlim=c(15, 50))+
  geom_hline(yintercept = 1, colour = "#000000", 
             linetype=2) +
  theme_bw() +
  theme(panel.spacing = unit(0, "lines"),
        legend.position = "bottom",
        legend.title = element_blank(),
        panel.grid.major.y= element_line(colour = "grey", linetype="dashed"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        legend.text =element_text(size=12),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  
  labs(title = "Relative hazard ratios for BMI and risk of transitions. Reference BMI: 22")

ggsave( "rel.hazards.bmi.overall.month.png",gg.rel.hazards.bmi.month,
        dpi=300,
        width = 8, height = 4.5)

####### 
# using 5y BMI
#######

gg.rel.hazards.bmi.5y<-
  plot.data %>% 
  filter(strata=="Overall5y", model =="Fully adjusted model") %>% 
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
        strip.text = element_text(size=12, face="bold"),
        strip.background = element_rect( fill="#f7f7f7"),
        strip.text.y.left = element_text(angle = 0)) +
  ylab("Relative\nhazard ratio\n")+
  xlab("\nBMI")+
  coord_cartesian(ylim = c(0, 4), xlim=c(15, 50))+
  labs(title = "Relative hazard ratios for BMI and risk\nof transitions. Reference BMI: 22")

ggsave( "rel.hazards.bmi.overall.5y.png",gg.rel.hazards.bmi.5y,
        dpi=300,
        width = 5.5, height = 11)
