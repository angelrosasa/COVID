remove(list=ls())


library(purrr)
library(gridExtra)
library(grid)
library(ggpubr)
library(cowplot)
library(ggplot2)


source("data.R")

## some dates

first_case <- as.Date("2020-03-06")
first_death <- as.Date("2020-03-18")
first_measure<-as.Date("2020-03-11")
onset_lockdown<-as.Date("2020-03-16")

onset_data<-as.Date("2020-03-02")  #onset week 10 
end_data <-as.Date("2020-11-08")
onset_plot<-as.Date("2020-02-22")

d0<-d0
d1<-d1
d2<-month2_date 
d3<-month3_date 
d4<-month4_date 
d5<-month5_date 
d6<-month6_date 
d7<-month7_date 

labdate<-c(onset_lockdown,d2,d3,d4,d5,d6,d7)

options("scipen"=100, "digits"=4) 


load("results_m8y.RData")

load("results_m2.RData")


# additional calculc


### Ro

ci50_model2$Rt.2[ci50_model2$date=="2020-03-12"]
ci05_model2$Rt.2[ci05_model2$date=="2020-03-12"]
ci95_model2$Rt.2[ci95_model2$date=="2020-03-12"]


ci50_model$Rt[ci50_model$date=="2020-03-12"]
ci05_model$Rt[ci05_model$date=="2020-03-12"]
ci95_model$Rt[ci95_model$date=="2020-03-12"]

ci50_model$Ro[ci50_model$date=="2020-03-12"]
ci05_model$Ro[ci05_model$date=="2020-03-12"]
ci95_model$Ro[ci95_model$date=="2020-03-12"]


## Rt1

ci50_model2$Rt.2[ci50_model2$date=="2020-03-19"]
ci05_model2$Rt.2[ci05_model2$date=="2020-03-19"]
ci95_model2$Rt.2[ci95_model2$date=="2020-03-19"]

ci50_model$Rt[ci50_model$date=="2020-03-19"]
ci05_model$Rt[ci05_model$date=="2020-03-19"]
ci95_model$Rt[ci95_model$date=="2020-03-19"]

## Rt1

ci50_model2$Rt.2[ci50_model2$date=="2020-04-13"]
ci05_model2$Rt.2[ci05_model2$date=="2020-04-13"]
ci95_model2$Rt.2[ci95_model2$date=="2020-04-13"]

ci50_model$Rt[ci50_model$date=="2020-04-13"]
ci05_model$Rt[ci05_model$date=="2020-04-13"]
ci95_model$Rt[ci95_model$date=="2020-04-13"]

## Rt4

ci50_model2$Rt.2[ci50_model2$date=="2020-06-05"]
ci05_model2$Rt.2[ci05_model2$date=="2020-06-05"]
ci95_model2$Rt.2[ci95_model2$date=="2020-06-05"]

ci50_model$Rt[ci50_model$date=="2020-06-05"]
ci05_model$Rt[ci05_model$date=="2020-06-05"]
ci95_model$Rt[ci95_model$date=="2020-06-05"]


## Rt in other dates
ci05_model2$Rt.2[ci05_model2$date=="2020-07-01"]
ci50_model2$Rt.2[ci50_model2$date=="2020-07-01"]
ci95_model2$Rt.2[ci95_model2$date=="2020-07-01"]

ci05_model$Rt[ci05_model$date=="2020-07-01"]
ci50_model$Rt[ci50_model$date=="2020-07-01"]
ci95_model$Rt[ci95_model$date=="2020-07-01"]

ci50_model2$Rt.2[ci50_model2$date=="2020-09-01"]
ci50_model2$Rt.2[ci50_model2$date=="2020-10-01"]

ci50_model$Rt[ci50_model$date=="2020-09-01"]
ci50_model$Rt[ci50_model$date=="2020-10-01"]

#infections 


ci05_model2$L.infcum = cumsum(c(0,abs(diff(ci05_model2$L.S/L.N))))
ci95_model2$L.infcum = cumsum(c(0,abs(diff(ci95_model2$L.S/L.N))))
ci50_model2$L.infcum = cumsum(c(0,abs(diff(ci50_model2$L.S/L.N))))

ci05_model2$H.infcum = cumsum(c(0,abs(diff(ci05_model2$H.S/H.N))))
ci95_model2$H.infcum = cumsum(c(0,abs(diff(ci95_model2$H.S/H.N))))
ci50_model2$H.infcum = cumsum(c(0,abs(diff(ci50_model2$H.S/H.N))))

ci05_model2$infcum = cumsum(c(0,abs(diff(ci05_model2$S/NN))))
ci95_model2$infcum = cumsum(c(0,abs(diff(ci95_model2$S/NN))))
ci50_model2$infcum = cumsum(c(0,abs(diff(ci50_model2$S/NN))))


ci05_model$L.infcum = cumsum(c(0,abs(diff(ci05_model$L.S/L.N))))
ci95_model$L.infcum = cumsum(c(0,abs(diff(ci95_model$L.S/L.N))))
ci50_model$L.infcum = cumsum(c(0,abs(diff(ci50_model$L.S/L.N))))

ci05_model$H.infcum = cumsum(c(0,abs(diff(ci05_model$H.S/H.N))))
ci95_model$H.infcum = cumsum(c(0,abs(diff(ci95_model$H.S/H.N))))
ci50_model$H.infcum = cumsum(c(0,abs(diff(ci50_model$H.S/H.N))))

ci05_model$infcum = cumsum(c(0,abs(diff(ci05_model$S/NN))))
ci95_model$infcum = cumsum(c(0,abs(diff(ci95_model$S/NN))))
ci50_model$infcum = cumsum(c(0,abs(diff(ci50_model$S/NN))))


##

ci05_model2$infcum[ci05_model2$date=="2020-03-05"]*NN
ci50_model2$infcum[ci50_model2$date=="2020-03-05"]*NN
ci95_model2$infcum[ci95_model2$date=="2020-03-05"]*NN

ci05_model2$infcum[ci05_model2$date=="2020-03-15"]*NN
ci50_model2$infcum[ci50_model2$date=="2020-03-15"]*NN
ci95_model2$infcum[ci95_model2$date=="2020-03-15"]*NN



ci05_model$infcum[ci05_model$date=="2020-03-05"]*NN
ci50_model$infcum[ci50_model$date=="2020-03-05"]*NN
ci95_model$infcum[ci95_model$date=="2020-03-05"]*NN

ci05_model$infcum[ci05_model$date=="2020-03-15"]*NN
ci50_model$infcum[ci50_model$date=="2020-03-15"]*NN
ci95_model$infcum[ci95_model$date=="2020-03-15"]*NN


## attack rates


ci05_model2$L.infcum[ci05_model2$date=="2020-06-05"]
ci50_model2$L.infcum[ci50_model2$date=="2020-06-05"]
ci95_model2$L.infcum[ci95_model2$date=="2020-06-05"]

ci05_model2$H.infcum[ci05_model2$date=="2020-06-05"]
ci50_model2$H.infcum[ci50_model2$date=="2020-06-05"]
ci95_model2$H.infcum[ci95_model2$date=="2020-06-05"]


ci05_model$L.infcum[ci05_model$date=="2020-06-05"]
ci50_model$L.infcum[ci50_model$date=="2020-06-05"]
ci95_model$L.infcum[ci95_model$date=="2020-06-05"]

ci05_model$H.infcum[ci05_model$date=="2020-06-05"]
ci50_model$H.infcum[ci50_model$date=="2020-06-05"]
ci95_model$H.infcum[ci95_model$date=="2020-06-05"]



ci05_model2$L.infcum[ci05_model2$date=="2020-11-08"]
ci50_model2$L.infcum[ci50_model2$date=="2020-11-08"]
ci95_model2$L.infcum[ci95_model2$date=="2020-11-08"]

ci05_model2$H.infcum[ci05_model2$date=="2020-11-08"]
ci50_model2$H.infcum[ci50_model2$date=="2020-11-08"]
ci95_model2$H.infcum[ci95_model2$date=="2020-11-08"]


ci05_model$L.infcum[ci05_model$date=="2020-11-08"]
ci50_model$L.infcum[ci50_model$date=="2020-11-08"]
ci95_model$L.infcum[ci95_model$date=="2020-11-08"]

ci05_model$H.infcum[ci05_model$date=="2020-11-08"]
ci50_model$H.infcum[ci50_model$date=="2020-11-08"]
ci95_model$H.infcum[ci95_model$date=="2020-11-08"]




## susceptible population

ci05_model2$L.S[ci05_model2$date=="2020-11-08"]/L.N
ci50_model2$L.S[ci50_model2$date=="2020-11-08"]/L.N
ci95_model2$L.S[ci95_model2$date=="2020-11-08"]/L.N

ci05_model2$H.S[ci05_model2$date=="2020-11-08"]/H.N
ci50_model2$H.S[ci50_model2$date=="2020-11-08"]/H.N
ci95_model2$H.S[ci95_model2$date=="2020-11-08"]/H.N

ci05_model2$S[ci05_model2$date=="2020-11-08"]/NN
ci50_model2$S[ci50_model2$date=="2020-11-08"]/NN
ci95_model2$S[ci95_model2$date=="2020-11-08"]/NN


ci05_model$L.S[ci05_model$date=="2020-11-08"]/L.N
ci50_model$L.S[ci50_model$date=="2020-11-08"]/L.N
ci95_model$L.S[ci95_model$date=="2020-11-08"]/L.N

ci05_model$H.S[ci05_model$date=="2020-11-08"]/H.N
ci50_model$H.S[ci50_model$date=="2020-11-08"]/H.N
ci95_model$H.S[ci95_model$date=="2020-11-08"]/H.N

ci05_model$S[ci05_model$date=="2020-11-08"]/NN
ci50_model$S[ci50_model$date=="2020-11-08"]/NN
ci95_model$S[ci95_model$date=="2020-11-08"]/NN




## Plots

## comparison parameters IFR 

library(ggplot2)

ifr_0=data.frame(la=c("Less wealthy","Wealthiest"), 
             med=c(0.0105,0.0091), 
             low=c(0.0095,0.0080), 
             up=c(0.0116,0.0104))

ifr_1=data.frame(la=c("0-59 y, men",
                        "0-59 y, women",
                        "60-+ y, men",
                        "60-+ y, women"),
                 med=c(0.0056,0.0017,0.0888,0.0478), 
                 low=c(0.0052,0.0015,0.0747,0.0428), 
                 up=c(0.0064,0.0019,0.10,0.0559))
                   
ifr_2=data.frame(la=c("0-59 y, men",
                      "0-59 y, women",
                      "60-+ y, men",
                      "60-+ y, women"),
                 med=c(0.0043,0.001,0.0564,0.0441), 
                 low=c(0.0030,0.00085,0.0418,0.0385), 
                 up=c(0.0054,0.0014,0.08,0.0507))




erbar<-function(data, a, b, c, let,d){

  ggplot() + 
  geom_errorbar(data=data, mapping=aes(y=la, x=up, xmin=up, xmax=low), width=.07, size=1, color="blue") + 
  geom_point(data=data, mapping=aes(y=la, x=med), size=3, shape=19, colour="blue") +
  geom_point(data=data, mapping=aes(y=la, x=med), size=1, shape=19, colour="white") +
  scale_x_continuous(name = "Infection fatality rate", 
                     breaks = seq(a, b, c),limits=c(a, b))+
  annotate(geom="text", x=0.099, y=(nrow(data)+d), label=let, size=12, color="black")+
  theme_bw()+
  
  theme(
    axis.title.y = element_blank(),
    axis.title.x=element_text(colour="black", size = 18),
    axis.text.x=element_text(colour="black", size = 18),
    axis.text.y=element_text(colour="black", size = 18),
    panel.grid.minor = element_blank())
}
  


x11()
erbar(ifr_1, 0, 0.1,0.01,"B",0.3)



jpeg(file = "bar_er.jpg", width = 9 * 360, height = 12* 360, units = "px", pointsize =30, res = 300)
grid.arrange(
  erbar(ifr_0, 0, 0.1,0.01,"A",0.4)+
    theme( axis.title.y = element_text(colour="white",margin = margin(t = 0, r = 6, b = 0, l = 0))),
  erbar(ifr_1, 0, 0.1,0.01,"B",0.3),
  erbar(ifr_2, 0, 0.1,0.01,"C",0.3),  
  heights = c(1.6,  2.1, 2.1), 
  ncol = 1)
dev.off()  


## background graph

pal2<-c("#2F4F4F","#e31a1c","#ff8ea1","#ffc073","#1f78b4","#6cb5e6","#b19cd9")
order=c("a","b","c","d","e","f","g","h")

###BASE LEGS!

leg<-function(v, fr, initdate, z, d){
  
  ggplot() + 
    theme_bw() +
    
    scale_y_continuous(limits = c(0,v), expand = c(0.01, 0.01))+  
    
    scale_x_date(limit=c(as.Date(initdate), max(data$date_plot)), 
                 breaks = labdate,  date_labels = "%b %d", expand = c(0, 0), position = "bottom") +
    
    geom_vline(xintercept = d0, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x= d0-2, y= v*0.85, label="do", size=z, angle=90, color="darkgrey")+  
    
    geom_vline(xintercept = d1, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x= d1+2, y= v*0.85, label="d1", size=z, angle=90, color="darkgrey")+  
    
    geom_vline(xintercept = onset_lockdown, linetype="dashed", 
               color = "#301934", size=1)+
    annotate(geom="text", x= onset_lockdown-2, y=v*0.35, label="Lockdown", size=z, angle=90, color="#301934")+
    
    geom_vline(xintercept = d2, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d2+d, y=v*fr, label="Reduction \ncurfew", size=z, angle=90, color="darkgrey")+
    
    geom_vline(xintercept = d3, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d3+d, y=v*fr, label="Reactivation \n(2º phase)", size=z, angle=90, color="darkgrey")+
    
    geom_vline(xintercept = d4, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d4+d, y=v*fr, label="Reactivation \n(3º phase)", size=z, angle=90, color="darkgrey")+
    
    geom_vline(xintercept = d5, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d5+d, y=v*fr, label="", size=z, angle=90, color="darkgrey")+
    
    geom_vline(xintercept = d6, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d6+d, y=v*fr, label="", size=z, angle=90, color="darkgrey")+
    
    geom_vline(xintercept = d7, linetype="dashed", 
               color = "grey", size=0.8)+
    annotate(geom="text", x=d7+d, y=v*fr, label="Reactivation \n(4º phase)", size=z, angle=90, color="darkgrey")+
    
    theme(axis.title.x=element_blank(),
          axis.title.y = element_text(colour="white",margin = margin(t = 0, r = 15, b = 0, l = 0)),
          axis.text.y=element_text(colour="white"),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
}

##### BASE 1

leg1<-
  leg(1,0.5, onset_plot,8, 0) +
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[1]), 
            linetype="solid", lwd=1.2, alpha=1)+
  geom_point(aes(x = ci50_model$date, 
                 y = 2, shape="yo"), 
             colour=pal2[1], size=6)+
  
  scale_colour_manual(labels=c("Predicted     "), 
                      values=pal2[1])+
  scale_shape_manual(labels=c("Observed     "), 
                     values=21)+
  
  theme(
    axis.text.x=element_text(size=22),
    legend.title  = element_blank(),
    legend.text=element_text(size=40),
    legend.direction="horizontal",
    legend.position="none",
    legend.box = "horizontal",
    legend.box.margin=margin(0,0,0,0),
    legend.key.width = unit(3, 'line'),
    legend.key.height = unit(4, 'line'),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  
  guides(
    shape = guide_legend(order = 1),
    colour = guide_legend(order = 2)
  )


x11()
leg1

legend1<- get_legend(leg1+     
                      theme(legend.position = "right"))


##### BASE 2

leg2<-
  leg(1,0.5, onset_plot,8, 0) +
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=1)+
  geom_point(aes(x = ci50_model$date, 
                 y = 2, shape="yo"), 
             colour=pal2[2], size=6)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=1)+
  geom_point(aes(x = ci50_model$date, 
                 y = 2, shape="yo"), 
             colour=pal2[5], size=6)+
  
  scale_colour_manual(labels=c("Predicted: Less wealthy districts  ","Predicted: Wealthiest districts  "), 
                      values=pal2[c(2,5)])+
  scale_shape_manual(labels=c("Observed  "), 
                     values=21)+
  
  theme(
    axis.text.x=element_text(size=22),
    legend.title  = element_blank(),
    legend.text=element_text(size=40),
    legend.direction="horizontal",
    legend.position="none",
    legend.box = "horizontal",
    legend.box.margin=margin(0,0,0,0),
    legend.key.width = unit(3, 'line'),
    legend.key.height = unit(4, 'line'),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  guides(
    shape = guide_legend(order = 1),
    colour = guide_legend(order = 2)
  )



x11()
leg2

legend2<- get_legend(leg2+     
                       theme(legend.position = "right"))





leg3<-
  leg(1,0.5, onset_plot,8, 0) +
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[1], linetype=order[1]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[2], linetype=order[1]), lwd=1.2, alpha=1)+
  
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[3],  linetype=order[2]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[4],  linetype=order[2]), lwd=1.2, alpha=1)+

  scale_colour_manual("Subpopulation   ",labels=c("Men aged 60-+","Women aged 60-+","Men aged 0-59","Women aged 0-59"), 
                      values=pal2[c(6,3,7,4)])+
  scale_linetype_manual("                                            District group   ", labels=c("Less wealthy districts             ","Wealthiest districts     "), 
                     values=c("solid","twodash"))+
  
  theme(
    axis.text.x=element_text(size=22),
    legend.title=element_text(size=23, face = "bold"),
    legend.text=element_text(size=23),
    legend.direction="horizontal",
    legend.position="none",
    legend.box = "horizontal",
    legend.box.margin=margin(0,0,0,0),
    legend.key.width = unit(3, 'line'),
    legend.key.height = unit(0.8, 'line'),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  
    guides(
      linetype = guide_legend(order = 2, nrow=2,byrow=TRUE),
      colour=guide_legend(order=1, nrow=2,byrow=TRUE))



x11()
leg3

legend3<- get_legend(leg3+     
                       theme(legend.position = "right"))







leg4<-
  leg(1,0.5, onset_plot,8, 0) +
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[1], linetype=order[1]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[2], linetype=order[2]), lwd=1.2, alpha=1)+
  
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[3],  linetype=order[3]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[4],  linetype=order[4]), lwd=1.2, alpha=1)+
  
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[5], linetype=order[5]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[6], linetype=order[6]), lwd=1.2, alpha=1)+
  
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[7],  linetype=order[7]), lwd=1.2, alpha=1)+
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[8],  linetype=order[8]), lwd=1.2, alpha=1)+
  
  scale_colour_manual(labels=c("Men aged 60-+ in less wealthy districts   ",
                               "Women aged 60-+ in less wealthy districts   ",
                               "Men aged 60-+ in wealthiest districts   ",
                               "Women aged 60-+ in wealthiest districts                  ",
                               "Men aged 0-59 in less wealthy districts   ",
                               "Women aged 0-59 in less wealthy districts   ",
                               "Men aged 0-59 in wealthiest districts   ",
                               "Women aged 0-59 in wealthiest districts   "), 
                      values=pal2[c(6,3,6,3,7,4,7,4)])+
  scale_linetype_manual(labels=c("Men aged 60-+ in less wealthy districts   ",
                                 "Women aged 60-+ in less wealthy districts   ",
                                 "Men aged 60-+ in wealthiest districts   ",
                                 "Women aged 60-+ in wealthiest districts                  ",
                                 "Men aged 0-59 in less wealthy districts   ",
                                 "Women aged 0-59 in less wealthy districts   ",
                                 "Men aged 0-59 in wealthiest districts   ",
                                 "Women aged 0-59 in wealthiest districts   "), 
                        values=c("solid","solid","twodash","twodash","solid","solid","twodash","twodash"))+
  
  theme(
    axis.text.x=element_text(size=22),
    legend.title=element_blank(),
    legend.text=element_text(size=20),
    legend.direction="horizontal",
    legend.position="none",
    legend.box = "horizontal",
    legend.box.margin=margin(0,0,0,0),
    legend.key.width = unit(3, 'line'),
    legend.key.height = unit(0.8, 'line'),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  
  guides(
    linetype = guide_legend(ncol=4,bycol=TRUE),
    colour=guide_legend(ncol=4,bycol=TRUE))



x11()
leg4

legend4<- get_legend(leg4+     
                       theme(legend.position = "right"))



leg0<-
  leg(1,0.5, "2020-01-01",8, 0) +
  geom_line(aes(x = ci50_model$date, y = 2, colour=order[1]), 
            linetype="solid", lwd=1.2, alpha=1)+
  geom_point(aes(x = ci50_model$date, 
                 y = 2, shape="yo"), 
             colour=pal2[1], size=6)+
  
  scale_colour_manual(labels=c("Predicted     "), 
                      values=pal2[1])+
  scale_shape_manual(labels=c("Observed     "), 
                     values=21)+
  
  theme(
    axis.text.x=element_text(size=18),
    legend.title  = element_blank(),
    legend.text=element_text(size=20),
    legend.direction="horizontal",
    legend.position="none",
    legend.box = "horizontal",
    legend.box.margin=margin(0,0,0,0),
    legend.key.width = unit(3, 'line'),
    legend.key.height = unit(4, 'line'),
    plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"))+
  
  guides(
    shape = guide_legend(order = 1),
    colour = guide_legend(order = 2)
  )


x11()
leg0






### content 
scaleFUN <- function(x) sprintf("%.1f", x)
scaleFUN0 <- function(x) sprintf("%.0f", x)


gg<-function(v, initdate, nam, let, ffun){
  
  if(missing(ffun)){
    la=scaleFUN}
  else{
    la=scaleFUN0
  }
  
  ggplot() + 
    theme_bw() +
    
    scale_y_continuous(nam, limits = c(0,v), expand = c(0.01, 0.01),labels = la)+  
    
    scale_x_date(limit=c(as.Date(initdate), max(data$date_plot)), 
                 breaks = labdate,  date_labels = "%b %d", expand = c(0, 0)) +
    
    geom_vline(xintercept = d0, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d1, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = onset_lockdown, linetype="dashed", 
               color = "#301934", size=1)+
    geom_vline(xintercept = d2, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d3, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d4, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d5, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d6, linetype="dashed", 
               color = "grey", size=0.8)+
    geom_vline(xintercept = d7, linetype="dashed", 
               color = "grey", size=0.8)+
    
    annotate(geom="text", x=d7+30, y=v*0.95, label=let, size=13, color="black")+
    
    theme(axis.title.x=element_blank(),
          axis.title.y= element_text(size=17),
          axis.text.y= element_text(size=18),
          axis.text.x= element_blank(),
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm"),
          legend.position="none")
}


### Graph trend lines 

tren<-read.csv("trend.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
tren$date_plot<-as.Date(tren$date_plot,
                      format = "%d/%m/%y")


t1<-gg(3900,"2020-01-01","Weekly number of deaths","A",1)+
  geom_line(data=tren, aes(x = date_plot, y = exp_2020), linetype="dashed", 
             color = "green", size=0.9)+
  geom_line(data=tren, aes(x = date_plot, y = hist_week), colour="blue", linetype="dashed", 
            lwd=0.9)+
  geom_line(data=tren, aes(x = date_plot, y = ss, colour="e"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = sin, colour="d"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = count, colour="a"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = count - hist_week, colour="b"),
            lwd=1)+
  geom_line(data=tren, aes(x = date_plot, y = count - exp_2020, colour="c"),
            lwd=1)+
  scale_colour_manual(labels=c("Non-violent deaths",
                               "Excess mortality (compared to 2017-2019)",
                               "Excess mortality (compared to first 11 w)",
                               "COVID-19 deaths (SINADEF)",
                               "COVID-19 deaths (NSS)"), 
                      values=c("#696969","blue","green","#e32d19","#722f37"))+
  theme(legend.position=c(0.2, .83),
        legend.title = element_blank(),
        legend.text=element_text(size=18),
        legend.key.size=unit(1.8, "lines"))

x11()
t1

t2<-gg(27000,"2020-01-01","Weekly number of infections","B",1)+
  geom_line(data=tren, aes(x = date_plot, y = pcr+pr, colour="#e32d19"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = pcr, colour="blue"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = pr, colour="green"),
            lwd=1.2)+
  scale_colour_manual(labels=c("All", "Detected by PCR", "Detected by PR"), 
                      values=c("#696969", "cornflowerblue", "seagreen"))+
  theme(legend.position=c(0.10, .88),
        legend.title = element_blank(),
        legend.text=element_text(size=18),
        legend.key.size=unit(1.8, "lines"))

x11()
t2

#♥figure
jpeg(file = "trends.jpg", width = 14 * 360, height = 12* 360, units = "px", pointsize = 10, res = 320)
grid.arrange(
  t1+
    theme( axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0))),
  t2,
  leg0+ 
    theme( axis.title.y = element_text(colour="white",margin = margin(t = 0, r = 42, b = 0, l = 0))), 
  heights = c(1, 1, 0.45), 
  ncol = 1)
dev.off()          




## graphs


a_1<-gg(3.7, onset_plot, "Daily deaths (x100)","A")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$deadi/100, ymax=ci95_model2$deadi/100), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = deadi/100), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = deaths/100), 
             shape=21, colour="dark red", size=3.5, alpha=0.4, stroke=1.1)
x11()
a_1



a_2<-gg(4.9,onset_plot, "Cumulative deaths (x10,000)","B")+  
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(ci05_model2$deadi)/10000, ymax=cumsum(ci95_model2$deadi)/10000), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = cumsum(deadi)/10000), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_sample2, aes(x = date, y = deathscum/10000), 
             shape=21, colour="dark red", size=3.5, alpha=0.7, stroke=1.1)

x11()
a_2


a_3<-gg(3.8, onset_plot, "Daily infections (x10,000)","C")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = c(0,abs(diff(ci05_model2$S/10000))), ymax=c(0,abs(diff(ci95_model2$S)))/10000), 
              fill=pal2[1], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = c(0,abs(diff(ci50_model2$S/10000)))), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  x11()
a_3


a_4<-gg(5.8, onset_plot, "Cumulative infections (x1,000,000)","D")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(c(0,abs(diff(ci05_model2$S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model2$S/1000000))))), 
              fill=pal2[1], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = cumsum(c(0,abs(diff(ci50_model2$S/1000000))))), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  x11()
a_4


a_5<-gg(0.48, onset_plot, "Prevalence (recovered people)","E")+  
  geom_ribbon(aes(x=ci05_model2$date, 
                  ymin = (ci05_model2$R)/NN, 
                  ymax= (ci95_model2$R)/NN), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model2, 
            aes(x =   date, y = (R)/NN), 
            colour=pal2[1],linetype="solid", lwd=1.2, alpha=0.6)+
  geom_point(aes(x = date_sero, y = s_Total[1]), 
             shape=21, colour="dark red", size=4.5, alpha=0.9, stroke=1.2)

x11()
a_5

a_6<-gg(3.4,  onset_plot, "Reproductive number (Rt)","F")+ 
  geom_hline(yintercept=1, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$Rt.2, ymax=ci95_model2$Rt.2), 
              fill=pal2[1], alpha=0.3) +
  #geom_line(data=ci50_model2, aes(x = date, y = Rt),colour = pal2[1],
  #          linetype="solid", lwd=1.2, alpha=0.8)+

  geom_line(data=ci50_model2, aes(x = date, y = Rt.2),colour = pal2[1],
          linetype="solid", lwd=1.2, alpha=0.8)


x11()
a_6




b_1<-gg(3.4, onset_plot, "Daily deaths (x100)","A")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$L.deadi/100, ymax=ci95_model2$L.deadi/100), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = L.deadi/100), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = L_deaths/100), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.7, stroke=1.1)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$H.deadi/100, ymax=ci95_model2$H.deadi/100), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = H.deadi/100), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = H_deaths/100), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.7, stroke=1.1)

x11()
b_1



b_2<-gg(3.9,onset_plot, "Cumulative deaths (x10,000)","C")+  
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(ci05_model2$L.deadi)/10000, ymax=cumsum(ci95_model2$L.deadi)/10000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = cumsum(L.deadi)/10000), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(ci05_model2$H.deadi)/10000, ymax=cumsum(ci95_model2$H.deadi)/10000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = cumsum(H.deadi)/10000), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
b_2


b_3<-gg(4.7, onset_plot, "Daily infections (x10,000)","E")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = c(0,abs(diff(ci05_model2$L.S/10000))), ymax=c(0,abs(diff(ci95_model2$L.S)))/10000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = c(0,abs(diff(ci50_model2$L.S/10000)))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = c(0,abs(diff(ci05_model2$H.S/10000))), ymax=c(0,abs(diff(ci95_model2$H.S)))/10000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = c(0,abs(diff(ci50_model2$H.S/10000)))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
b_3


b_4<-gg(4.8, onset_plot, "Cumulative infections (x1,000,000)","G")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(c(0,abs(diff(ci05_model2$L.S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model2$L.S/1000000))))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = cumsum(c(0,abs(diff(ci50_model2$L.S/1000000))))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(c(0,abs(diff(ci05_model2$H.S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model2$H.S/1000000))))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = cumsum(c(0,abs(diff(ci50_model2$H.S/1000000))))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
b_4



b_5<-gg(9.8, onset_plot, "Susceptible people (x1,000,000)","I")+  
  geom_ribbon(aes(x=ci05_model2$date, 
                  ymin = (ci05_model2$L.S)/1000000, 
                  ymax= (ci95_model2$L.S)/1000000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, 
            aes(x =   date, y = (L.S)/1000000), 
            colour=pal2[2],linetype="solid", lwd=1.2, alpha=0.6)+
  
  geom_ribbon(aes(x=ci05_model2$date, 
                  ymin = (ci05_model2$H.S)/1000000, 
                  ymax= (ci95_model2$H.S)/1000000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, 
            aes(x =   date, y = (H.S)/1000000), 
            colour=pal2[5],linetype="solid", lwd=1.2, alpha=0.6)+
  
  x11()
b_5




c_1<-gg(3.7, onset_plot, "Daily deaths x 100,000 / population","A")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$L.deadi*100000/L.N, ymax=ci95_model2$L.deadi*100000/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = L.deadi*100000/L.N), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = L_deaths*100000/L.N), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.4, stroke=1.1)+
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = ci05_model2$H.deadi*100000/H.N, ymax=ci95_model2$H.deadi*100000/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = H.deadi*100000/H.N), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = H_deaths*100000/H.N), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.4, stroke=1.1)+
  
  x11()
c_1



c_2<-gg(4.9,onset_plot, "Cumulative deaths x 1,000/ population)","B")+  
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(ci05_model2$L.deadi)*1000/L.N, ymax=cumsum(ci95_model2$L.deadi)*1000/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = cumsum(L.deadi)*1000/L.N), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = cumsum(L_deaths)*1000/L.N), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.3, stroke=1.1)+
  
  
  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(ci05_model2$H.deadi)*1000/H.N, ymax=cumsum(ci95_model2$H.deadi)*1000/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, aes(x = date, y = cumsum(H.deadi)*1000/H.N), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model2, aes(x = date, y = cumsum(H_deaths)*1000/H.N), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.3, stroke=1.1)
  
  
  x11()
c_2


c_3<-gg(3.4, onset_plot, "Daily infections x1,000/ population","C")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = c(0,abs(diff(ci05_model2$L.S*1000/L.N))), ymax=c(0,abs(diff(ci95_model2$L.S*1000/L.N)))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = c(0,abs(diff(ci50_model2$L.S*1000/L.N)))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = c(0,abs(diff(ci05_model2$H.S*1000/H.N))), ymax=c(0,abs(diff(ci95_model2$H.S*1000/H.N)))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = c(0,abs(diff(ci50_model2$H.S*1000/H.N)))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
c_3


c_4<-gg(0.49, onset_plot, "Cumulative infections/ population","D")+  
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(c(0,abs(diff(ci05_model2$L.S/L.N)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model2$L.S/L.N))))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = cumsum(c(0,abs(diff(ci50_model2$L.S/L.N))))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model2$date, ymin = cumsum(c(0,abs(diff(ci05_model2$H.S/H.N)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model2$H.S/H.N))))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model2$date, y = cumsum(c(0,abs(diff(ci50_model2$H.S/H.N))))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
c_4



c_5<-gg(1, onset_plot, "Susceptible people/ population","E")+  
  scale_y_continuous("Susceptible people/ population", limits=c(0.5,1))+
  geom_ribbon(aes(x=ci05_model2$date, 
                  ymin = (ci05_model2$L.S)/L.N, 
                  ymax= (ci95_model2$L.S)/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model2, 
            aes(x =   date, y = (L.S)/L.N), 
            colour=pal2[2],linetype="solid", lwd=1.2, alpha=0.6)+
  
  geom_ribbon(aes(x=ci05_model2$date, 
                  ymin = (ci05_model2$H.S)/H.N, 
                  ymax= (ci95_model2$H.S)/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model2, 
            aes(x =   date, y = (H.S)/H.N), 
            colour=pal2[5],linetype="solid", lwd=1.2, alpha=0.6)+
  
  x11()
c_5







g_1<-gg(3.7, onset_plot, "Daily deaths (x100)","G")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$deadi/100, ymax=ci95_model$deadi/100), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = deadi/100), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = deaths/100), 
             shape=21, colour="dark red", size=3.5, alpha=0.4, stroke=1.1)
x11()
g_1



g_2<-gg(4.9,onset_plot, "Cumulative deaths (x10,000)","H")+  

  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$deadi)/10000, ymax=cumsum(ci95_model$deadi)/10000), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = cumsum(deadi)/10000), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_sample, aes(x = date, y = deathscum/10000), 
             shape=21, colour="dark red", size=3.5, alpha=0.7, stroke=1.1)

x11()
g_2


g_3<-gg(3.8, onset_plot, "Daily infections (x10,000)","I")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = c(0,abs(diff(ci05_model$S/10000))), 
                  ymax=c(0,abs(diff(ci95_model$S)))/10000), 
              fill=pal2[1], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$S/10000)))), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  x11()
g_3


g_4<-gg(5.8, onset_plot, "Cumulative infections (x1,000,000)","J")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(c(0,abs(diff(ci05_model$S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model$S/1000000))))), 
              fill=pal2[1], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = cumsum(c(0,abs(diff(ci50_model$S/1000000))))), colour=pal2[1], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  x11()
g_4


g_5<-gg(0.48, onset_plot, "Prevalence (recovered)","K")+  
  geom_ribbon(aes(x=ci05_model$date, 
                  ymin = (ci05_model$I+ci05_model$R)/NN, 
                  ymax= (ci95_model$I+ci95_model$R)/NN), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model, 
            aes(x =   date, y = (I+R)/NN), 
            colour=pal2[1],linetype="solid", lwd=1.2, alpha=0.6)+
  geom_point(aes(x = date_sero, y = s_Total[1]), 
             shape=21, colour="dark red", size=4.5, alpha=0.9, stroke=1.2)

x11()
g_5

g_6<-gg(3.4,  onset_plot, "Reproductive number (Rt)","L")+ 
  geom_hline(yintercept=1, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$Rt, ymax=ci95_model$Rt), 
              fill=pal2[1], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = Rt),colour = pal2[1],
            linetype="solid", lwd=1.2, alpha=0.8)


x11()
g_6




h_1<-gg(3.4, onset_plot, "Daily deaths (x100)","A")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi/100, ymax=ci95_model$L.deadi/100), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi/100), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = L_deaths/100), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.7, stroke=1.1)+
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi/100, ymax=ci95_model$H.deadi/100), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi/100), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = H_deaths/100), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.7, stroke=1.1)

x11()
h_1



h_2<-gg(3.9,onset_plot, "Cumulative deaths (x10,000)","C")+  
  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$L.deadi)/10000, ymax=cumsum(ci95_model$L.deadi)/10000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = cumsum(L.deadi)/10000), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$H.deadi)/10000, ymax=cumsum(ci95_model$H.deadi)/10000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = cumsum(H.deadi)/10000), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
h_2


h_3<-gg(3.7, onset_plot, "Daily infections (x10,000)","E")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = c(0,abs(diff(ci05_model$L.S/10000))), ymax=c(0,abs(diff(ci95_model$L.S)))/10000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S/10000)))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = c(0,abs(diff(ci05_model$H.S/10000))), ymax=c(0,abs(diff(ci95_model$H.S)))/10000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S/10000)))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
  x11()
h_3


h_4<-gg(4.8, onset_plot, "Cumulative infections (x1,000,000)","G")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(c(0,abs(diff(ci05_model$L.S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model$L.S/1000000))))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = cumsum(c(0,abs(diff(ci50_model$L.S/1000000))))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(c(0,abs(diff(ci05_model$H.S/1000000)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model$H.S/1000000))))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = cumsum(c(0,abs(diff(ci50_model$H.S/1000000))))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
  x11()
h_4



h_5<-gg(9.8, onset_plot, "Susceptible people (x1,000,000)","I")+  
  geom_ribbon(aes(x=ci05_model$date, 
                  ymin = (ci05_model$L.S)/1000000, 
                  ymax= (ci95_model$L.S)/1000000), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, 
            aes(x =   date, y = (L.S)/1000000), 
            colour=pal2[2],linetype="solid", lwd=1.2, alpha=0.6)+
  
  geom_ribbon(aes(x=ci05_model$date, 
                  ymin = (ci05_model$H.S)/1000000, 
                  ymax= (ci95_model$H.S)/1000000), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, 
            aes(x =   date, y = (H.S)/1000000), 
            colour=pal2[5],linetype="solid", lwd=1.2, alpha=0.6)+
  
x11()
h_5







i_1<-gg(3.7, onset_plot, "Daily deaths x 100,000 / population","F")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi*100000/L.N, ymax=ci95_model$L.deadi*100000/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi*100000/L.N), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = L_deaths*100000/L.N), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.4, stroke=1.1)+

  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi*100000/H.N, ymax=ci95_model$H.deadi*100000/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi*100000/H.N), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = H_deaths*100000/H.N), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.4, stroke=1.1)+
            
x11()
i_1



i_2<-gg(4.9,onset_plot, "Cumulative deaths x 1,000/ population)","G")+  
  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$L.deadi)*1000/L.N, ymax=cumsum(ci95_model$L.deadi)*1000/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = cumsum(L.deadi)*1000/L.N), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = cumsum(L_deaths)*1000/L.N), 
             shape=21, colour=pal2[2], size=3.5, alpha=0.3, stroke=1.1)+
  

  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$H.deadi)*1000/H.N, ymax=cumsum(ci95_model$H.deadi)*1000/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = cumsum(H.deadi)*1000/H.N), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, y = cumsum(H_deaths)*1000/H.N), 
             shape=21, colour=pal2[5], size=3.5, alpha=0.3, stroke=1.1)+


x11()
i_2


i_3<-gg(3.4, onset_plot, "Daily infections x1,000/ population","H")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = c(0,abs(diff(ci05_model$L.S*1000/L.N))), ymax=c(0,abs(diff(ci95_model$L.S*1000/L.N)))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S*1000/L.N)))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = c(0,abs(diff(ci05_model$H.S*1000/H.N))), ymax=c(0,abs(diff(ci95_model$H.S*1000/H.N)))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S*1000/H.N)))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
i_3


i_4<-gg(0.49, onset_plot, "Cumulative infections/ population","I")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(c(0,abs(diff(ci05_model$L.S/L.N)))), 
                  ymax=cumsum(c(0,abs(diff(ci95_model$L.S/L.N))))), 
              fill=pal2[2], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = cumsum(c(0,abs(diff(ci50_model$L.S/L.N))))), colour=pal2[2], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(c(0,abs(diff(ci25_model$H.S/H.N)))), 
                  ymax=cumsum(c(0,abs(diff(ci75_model$H.S/H.N))))), 
              fill=pal2[5], alpha=0.3) +
  geom_line(aes(x = ci50_model$date, y = cumsum(c(0,abs(diff(ci50_model$H.S/H.N))))), colour=pal2[5], 
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
i_4



i_5<-gg(1, onset_plot, "Susceptible people/ population","J")+  
  scale_y_continuous("Susceptible people/ population", limits=c(0.5,1))+
  geom_ribbon(aes(x=ci05_model$date, 
                  ymin = (ci05_model$L.S)/L.N, 
                  ymax= (ci95_model$L.S)/L.N), 
              fill=pal2[2], alpha=0.3) +
  geom_line(data=ci50_model, 
            aes(x =   date, y = (L.S)/L.N), 
            colour=pal2[2],linetype="solid", lwd=1.2, alpha=0.6)+
  
  geom_ribbon(aes(x=ci05_model$date, 
                  ymin = (ci25_model$H.S)/H.N, 
                  ymax= (ci75_model$H.S)/H.N), 
              fill=pal2[5], alpha=0.3) +
  geom_line(data=ci50_model, 
            aes(x =   date, y = (H.S)/H.N), 
            colour=pal2[5],linetype="solid", lwd=1.2, alpha=0.6)+
  
x11()
i_5





jj_1A<-gg(33, onset_plot, "Daily deaths x 100,000 / population","A")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi_o_m*100000/L.N_o_m, ymax=ci95_model$L.deadi_o_m*100000/L.N_o_m), 
              fill=pal2[6],alpha=0.4) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi_o_m*100000/L.N_o_m), colour=pal2[6], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi_o_w*100000/L.N_o_w, ymax=ci95_model$L.deadi_o_w*100000/L.N_o_w), 
              fill=pal2[3],alpha=0.4) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi_o_w*100000/L.N_o_w), colour=pal2[3], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi_o_m*100000/H.N_o_m, ymax=ci95_model$H.deadi_o_m*100000/H.N_o_m), 
              fill=pal2[6],alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_o_m*100000/H.N_o_m), colour=pal2[6], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_o_m*100000/H.N_o_m), colour=pal2[6], 
            linetype="twodash", lwd=1.5, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi_o_w*100000/H.N_o_w, ymax=ci95_model$H.deadi_o_w*100000/H.N_o_w), 
              fill=pal2[3],alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_o_w*100000/H.N_o_w), colour=pal2[3], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_o_w*100000/H.N_o_w), colour=pal2[3], 
            linetype="twodash", lwd=1.5, alpha=0.8)
  
  x11()
jj_1A


jj_1B<-gg(1.9, onset_plot, "Daily deaths x 100,000 / population","B")+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi_y_m*100000/L.N_y_m, ymax=ci95_model$L.deadi_y_m*100000/L.N_y_m), 
              fill=pal2[7],alpha=0.4) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi_y_m*100000/L.N_y_m), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$L.deadi_y_w*100000/L.N_y_w, ymax=ci95_model$L.deadi_y_w*100000/L.N_y_w), 
              fill=pal2[4],alpha=0.4) +
  geom_line(data=ci50_model, aes(x = date, y = L.deadi_y_w*100000/L.N_y_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi_y_m*100000/H.N_y_m, ymax=ci95_model$H.deadi_y_m*100000/H.N_y_m), 
              fill=pal2[7],alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_y_m*100000/H.N_y_m), colour=pal2[7], 
            linetype="solid", lwd=0.3, alpha=0.8)+
    geom_line(data=ci50_model, aes(x = date, y = H.deadi_y_m*100000/H.N_y_m), colour=pal2[7], 
            linetype="twodash", lwd=1.5, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$H.deadi_y_w*100000/H.N_y_w, ymax=ci95_model$H.deadi_y_w*100000/H.N_y_w), 
              fill=pal2[4],alpha=0.3) +
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_y_w*100000/H.N_y_w), colour=pal2[4], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = H.deadi_y_w*100000/H.N_y_w), colour=pal2[4], 
            linetype="twodash", lwd=1.5, alpha=0.8)
  
  x11()
jj_1B


jj_2A<-gg(3.5, onset_plot, "Daily infections x1,000/ population","C")+  
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S_o_m)*1000/L.N_o_m))), colour=pal2[6], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S_o_w)*1000/L.N_o_w))), colour=pal2[3], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_o_m)*1000/H.N_o_m))), colour=pal2[6], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_o_w)*1000/H.N_o_w))), colour=pal2[3], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_o_m)*1000/H.N_o_m))), colour=pal2[6], 
            linetype="twodash", lwd=1.5, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_o_w)*1000/H.N_o_w))), colour=pal2[3], 
            linetype="twodash", lwd=1.5, alpha=0.8)

x11()
jj_2A


jj_2B<-gg(3.5, onset_plot, "Daily infections x1,000/ population","D")+  
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S_y_m)*1000/L.N_y_m))), colour=pal2[7], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$L.S_y_w)*1000/L.N_y_w))), colour=pal2[4], 
            linetype="solid", lwd=1.5, alpha=0.8)+
  
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_y_m)*1000/H.N_y_m))), colour=pal2[7], 
          linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_y_w)*1000/H.N_y_w))), colour=pal2[4], 
            linetype="solid", lwd=0.3, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_y_m)*1000/H.N_y_m))), colour=pal2[7], 
            linetype="twodash", lwd=1.5, alpha=0.8)+
  geom_line(aes(x = ci50_model$date, y = c(0,abs(diff(ci50_model$H.S_y_w)*1000/H.N_y_w))), colour=pal2[4], 
            linetype="twodash", lwd=1.5, alpha=0.8)

x11()
jj_2B




r_mod2<-gg(1.9,  onset_plot, "Reproductive number (Rt)","")+ 
  geom_line(data=ci50_model2, aes(x = date, y = LL.Rt,colour = "a"),
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model2, aes(x = date, y = LH.Rt,colour = "c"),
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model2, aes(x = date, y = HH.Rt,colour = "b"),
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model2, aes(x = date, y = HL.Rt,colour = "d"),
            linetype="solid", lwd=1.2, alpha=0.8)+
  scale_colour_manual("Transmission", labels=c(
                               "Within less wealthy districts (LWD)",
                               "Within wealthiest districts (WD)",
                               "From LWD to WD",
                               "From WD to LWD"), 
                      values=c(pal2[c(3,6,4,7)]))+
  theme(legend.position=c(0.65, .86),
        legend.title = element_text(size=18),
        legend.text=element_text(size=18),
        legend.key.size=unit(1.8, "lines"))

x11()
r_mod2



t1<-gg(3900,"2020-01-01","Weekly number of deaths","A",1)+
  geom_line(data=tren, aes(x = date_plot, y = exp_2020), linetype="dashed", 
            color = "green", size=0.9)+
  geom_line(data=tren, aes(x = date_plot, y = hist_week), colour="blue", linetype="dashed", 
            lwd=0.9)+
  geom_line(data=tren, aes(x = date_plot, y = ss, colour="e"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = sin, colour="d"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = count, colour="a"),
            lwd=1.2)+
  geom_line(data=tren, aes(x = date_plot, y = count - hist_week, colour="b"),
            lwd=1)+
  geom_line(data=tren, aes(x = date_plot, y = count - exp_2020, colour="c"),
            lwd=1)+
  scale_colour_manual(labels=c("Non-violent deaths",
                               "Excess mortality (compared to 2017-2019)",
                               "Excess mortality (compared to first 11 w)",
                               "COVID-19 deaths (SINADEF)",
                               "COVID-19 deaths (NSS)"), 
                      values=c("#696969","blue","green","#e32d19","#722f37"))+
  theme(legend.position=c(0.2, .86),
        legend.title = element_blank(),
        legend.text=element_text(size=18))




r_1<-gg(1.2,  onset_plot, "Reproductive number (Rt)","B")+ 
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yy_ww),colour = pal2[3],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yy_mm),colour = pal2[6],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yy_wm),colour = pal2[4],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yy_mw),colour = pal2[7],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yy_ww),colour = pal2[3],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yy_mm),colour = pal2[6],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yy_wm),colour = pal2[4],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yy_mw),colour = pal2[7],
            linetype="solid", lwd=1.2, alpha=0.8)
x11()
r_1




r_2<-gg(0.7,  onset_plot, "Reproductive number (Rt)","C")+ 
  geom_line(data=ci50_model, aes(x = date, y = R.HH_oo_ww),colour = pal2[3],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_oo_mm),colour = pal2[6],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_oo_wm),colour = pal2[4],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_oo_mw),colour = pal2[7],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_oo_ww),colour = pal2[3],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_oo_mm),colour = pal2[6],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_oo_wm),colour = pal2[4],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_oo_mw),colour = pal2[7],
            linetype="solid", lwd=1.2, alpha=0.8)


x11()
r_2





r_3<-gg(0.8,  onset_plot, "Reproductive number (Rt)","D")+ 
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yo_ww),colour = pal2[3],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yo_mm),colour = pal2[6],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yo_wm),colour = pal2[4],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HH_yo_mw),colour = pal2[7],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yo_ww),colour = pal2[3],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yo_mm),colour = pal2[6],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yo_wm),colour = pal2[4],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LL_yo_mw),colour = pal2[7],
            linetype="solid", lwd=1.2, alpha=0.8)


x11()
r_3


r_4<-gg(0.9,  onset_plot, "Reproductive number (Rt)","E")+ 
  geom_line(data=ci50_model, aes(x = date, y = R.LH_yy_ww),colour = pal2[3],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LH_yy_mm),colour = pal2[6],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LH_yy_wm),colour = pal2[4],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.LH_yy_mw),colour = pal2[7],
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HL_yy_ww),colour = pal2[3],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HL_yy_mm),colour = pal2[6],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HL_yy_wm),colour = pal2[4],
            linetype="twodash", lwd=1.2, alpha=0.8)+
  geom_line(data=ci50_model, aes(x = date, y = R.HL_yy_mw),colour = pal2[7],
            linetype="twodash", lwd=1.2, alpha=0.8)


x11()
r_4




sp<-ggplot()+ theme_void()


jpeg(file = "res1.jpg", width = 22 * 360, height = 24* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  a_1, sp, g_1, 
  a_2, sp, g_2, 
  a_3, sp, g_3,  
  a_4, sp, g_4,
  a_5, sp, g_5,
  a_6, sp, g_6,
  leg1, sp, leg1, 
  heights = c(1, 1, 1, 1, 1, 1, 0.7), 
  widths = c(1, 0.05, 1), 
  ncol = 3,
  top = textGrob("   Two-group model                                                Age-gender structured model\n     ",
                 gp=gpar(fontsize=40)),
  bottom = legend1)
dev.off()          



jpeg(file = "res2.jpg", width = 22 * 360, height = 22* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  c_1, sp, i_1, 
  c_2, sp, i_2, 
  c_3, sp, i_3,  
  c_4, sp, i_4,
  c_5, sp, i_5,
  leg2, sp, leg2, 
  heights = c(1,  1, 1, 1, 1, 0.7), 
  widths = c(1, 0.05, 1), 
  ncol = 3,
  top = textGrob("   Two-group model                                                Age-gender structured model\n     ",
                 gp=gpar(fontsize=40)),
  bottom = legend2)
dev.off()          



jpeg(file = "res3.jpg", width = 22 * 360, height = 12* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  jj_1A, sp, jj_1B, 
  jj_2A, sp, jj_2B, 
  leg4, sp, leg4, 
  heights = c(1,  1, 0.7), 
  widths = c(1, 0.05, 1), 
  ncol = 3,
  top = legend4)
dev.off()  


jpeg(file = "res4.jpg", width = 15 * 360, height = 22* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  r_mod2,
  r_1,
  r_2,
  r_3,
  r_4,
  leg1,
  heights = c(1,  1, 1,1,1, 0.7), 
  ncol = 1)
dev.off()          


jpeg(file = "res5.jpg", width = 12 * 360, height = 8* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  r_mod2,
    leg1,
  heights = c(1, 0.5), 
  ncol = 1)
dev.off()          



##### HERE TRENDS!!!!!!!




## THIS IS ONLY TO SEE THE INFECTIONS 


library(tidyverse)


data<-read.csv("db_infx.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE) %>%
  subset(age_class!="0") %>%
  mutate(fec=as.Date(fec),
    week=(as.numeric(fec-as.Date("2019-12-30")) %/% 7)+1)

dataw<-data %>%
  group_by(week,METODODX, class,age_class,gender) %>% 
  summarise(
    infx= sum(infx, na.rm=TRUE))

datall<- dataw %>%
  group_by(week,class,age_class,gender) %>% 
    summarise(
        infx= sum(infx, na.rm=TRUE))

datapcr<- dataw %>% filter(METODODX=="PCR")
datapr<- dataw %>% filter(METODODX=="PR")



K_2A<-ggplot()+

  geom_line(data=filter(datall, age_class =="60-+" & gender==2 & class==1), 
                   aes(x = week, y = infx*1000/L.N_o_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="60-+" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_o_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="60-+" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="60-+" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
K_2A



K_2B<-ggplot()+
  
  geom_line(data=filter(datall, age_class =="0-59" & gender==2 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="0-59" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="0-59" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datall, age_class =="0-59" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
K_2B





L_2A<-ggplot()+
  
  geom_line(data=filter(datapcr, age_class =="60-+" & gender==2 & class==1), 
            aes(x = week, y = infx*1000/L.N_o_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="60-+" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_o_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="60-+" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="60-+" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
L_2A



L_2B<-ggplot()+
  
  geom_line(data=filter(datapcr, age_class =="0-59" & gender==2 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="0-59" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="0-59" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapcr, age_class =="0-59" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
L_2B





M_2A<-ggplot()+
  
  geom_line(data=filter(datapr, age_class =="60-+" & gender==2 & class==1), 
            aes(x = week, y = infx*1000/L.N_o_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="60-+" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_o_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="60-+" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="60-+" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_o_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
M_2A



M_2B<-ggplot()+
  
  geom_line(data=filter(datapr, age_class =="0-59" & gender==2 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_m), colour=pal2[3], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="0-59" & gender==1 & class==1), 
            aes(x = week, y = infx*1000/L.N_y_w), colour=pal2[4], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="0-59" & gender==2 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_m), colour=pal2[6], 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=filter(datapr, age_class =="0-59" & gender==1 & class==2), 
            aes(x = week, y = infx*1000/H.N_y_w), colour=pal2[7], 
            linetype="solid", lwd=1.2, alpha=0.8)

x11()
M_2B















jpeg(file = "m8_count.jpg", width = 11 * 360, height = 22* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  h_1, h_2, h_3,h_4, h_5, leg2, 
  heights = c(1, 1, 1, 1, 1, 0.7), 
  ncol = 1,
  top = legend2)
dev.off()          



jpeg(file = "m8_rate.jpg", width = 11 * 360, height = 22* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  i_1, i_2, i_3,i_4, i_5, leg2, 
  heights = c(1, 1, 1, 1, 1, 0.7), 
  ncol = 1,
  top = legend2)
dev.off()          



jpeg(file = "m8_all.jpg", width = 22 * 360, height = 22* 360, units = "px", pointsize = 7, res = 300)
grid.arrange(
  h_1, i_1, h_2, i_2, h_3, i_3,h_4, i_4, h_5, i_5, leg2, leg2, 
  heights = c(1, 1, 1, 1, 1, 0.7), 
  ncol = 2,
  top = legend2)
dev.off()          





