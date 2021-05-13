library(purrr)

## some dates

first_case <- as.Date("2020-03-06")
first_death <- as.Date("2020-03-18")

first_measure<-as.Date("2020-03-11")
onset_lockdown<-as.Date("2020-03-16")
relaxation<-as.Date("2020-06-01") 
end_lockdown <-as.Date("2020-06-30")
onset_data<-as.Date("2020-03-02")  #onset week 10 
end_data <-as.Date("2020-10-25")

onset_plot<-as.Date("2020-02-22")

options("scipen"=100, "digits"=4) 

#sample<- read.csv("sample2.csv",header=TRUE)

sample<-rbind(
  read.table("model2_5", header=TRUE, sep=","),
  read.table("model2_6", header=TRUE, sep=","),
  read.table("model2_7", header=TRUE, sep=","),
  read.table("model2_8", header=TRUE, sep=","))

quants=c(0.05, 0.250, 0.5,0.75, 0.95)

apply( sample[8:48] , 2 , quantile , 
       probs = quants , na.rm = TRUE )


## calling the data, but do not forget to order by date!

data<-read.csv("db_model161120.csv")
#data<-subset(data, week<=43)

data$date_plot<-as.Date(data$date_plot)

data <- data[order(data$date_plot),]


# check here
data.ag<-reshape(data[,c(5,8,9)], 
                 idvar = c("date_plot"), 
                 timevar = c("class2"), direction = "wide")

data.ag$deaths<- with(data.ag, deaths.1 +deaths.2 + deaths.3+deaths.4 + 
                        deaths.5 +deaths.6 + deaths.7 +deaths.8)

data.ag$L_deaths<- with(data.ag, 
                        deaths.1 +deaths.2 + deaths.3+deaths.4)

data.ag$H_deaths<- with(data.ag,  
                        deaths.5 +deaths.6 + deaths.7 +deaths.8)

library(ggplot2)                 
windows()
ggplot() + 
  theme_bw() +
  geom_line(data=data.ag, aes(x = date_plot, y = log(L_deaths)), colour="red", 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_line(data=data.ag, aes(x = date_plot, y = log(H_deaths)), colour="blue", 
            linetype="solid", lwd=1.2, alpha=0.8)



res_raw <- vector(mode = "list", length = dim(sample)[1])


sim_period<-320


##LIST WITH ALL THE SIMULATIONS but they are not at the same time (offset)

for (i in 1:dim(sample)[1]){
  
  pars_sample<-state_sample<-NULL
  pars_sample<-unlist(sample[i,8:dim(sample)[2]],use.names=FALSE) # 8: position of first parameter 
  pars_sample<-transformParams(pars_sample)
  state_sample<-calcNominalState(calculateModel(pars_sample, sim_period))

       for (j in 1:length(state)){ 
         if (length(state_sample[[j]])<2){
           state_sample[j]<-state_sample[j]
         }
         else
             state_sample[[j]]<-state_sample[[j]][1:length(state_sample$deadi)]
       }

res_raw[[i]]<-state_sample
res_raw[[i]]$max_len<-length(res_raw[[i]][[1]])
}




##LIST WITH ALL THE SIMULATIONS at the same time (considering offset)

res_pre <-res_raw

min_offset<- min(unlist(map(res_raw, c(27,1)))) ## variable offset in the list 


for (i in 1:length(res_raw)){
  
newstart<-newend<- NULL  
newstart<-res_raw[[i]][[27]]-min_offset+1

newend<-res_raw[[i]][[33]][1] # put the right number ...one more than the number of elements in the list

  for (j in 1:length(res_raw[[1]])){ 
    
      res_pre[[i]][[j]]<-res_raw[[i]][[j]][newstart:newend]
  }

res_pre[[i]]$len<-length(res_pre[[i]][[1]])

}



## FINALLY ALL WITH THE SAME NUMBER OF OBSERVATIONS

n_el<- min(unlist(map(res_pre, c(34,1)))) # put the right number ...one more than the number of elements in the list

for (i in 1:length(res_pre))
  { 
  for (j in 1:length(res_pre[[1]]))
       {
    res_pre[[i]][[j]]<- res_pre[[i]][[j]][1:n_el]
  }
}


####  QUANTILES

Nrow<-length(res_pre[[1]][[1]])
Ncol<-length(res_pre[[1]])

ci50_model= ci05_model=ci25_model=ci75_model=
  ci95_model= data.frame(matrix(NA, nrow = Nrow, ncol = Ncol))

colnames(ci05_model)=colnames(ci25_model)=colnames(ci50_model)=
  colnames(ci75_model)=colnames(ci95_model)=  
    names(res_pre[[1]])

for (i in 1:Ncol){
  for (time in 1:Nrow){
    ci05_model[time,i]<- quantile(unlist(map(res_pre,c(i,time))), 0.05, na.rm=TRUE)
    ci25_model[time,i]<- quantile(unlist(map(res_pre,c(i,time))), 0.25, na.rm=TRUE)
    ci50_model[time,i]<- quantile(unlist(map(res_pre,c(i,time))), 0.5, na.rm=TRUE)
    ci75_model[time,i]<- quantile(unlist(map(res_pre,c(i,time))), 0.75, na.rm=TRUE)
    ci95_model[time,i]<- quantile(unlist(map(res_pre,c(i,time))), 0.95, na.rm=TRUE)
    }
}



## aqui hay que agregar por semana 

#adding date 


## Plots
rang<-ci50_model$offset[1]:nrow(ci50_model)[1] 

ci50_model$date<-ci05_model$date<- ci25_model$date<- 
  ci75_model$date<- ci95_model$date<- 
          c(seq(onset_data-(ci50_model$offset[1]-1),onset_data-1, by="days"),
                  seq(onset_data,onset_data + (length(rang)-1), by="days"))

ci50_model<-merge(ci50_model, data.ag, by.x="date", by.y="date_plot", all=T)

## value to complete with ceros deaths before data onset
deca<- as.numeric(onset_data - min(ci50_model$date))

## to complete with ceros 
ci50_model[1:deca,][is.na(ci50_model[1:deca,])]<-0 # put cero to x first days


ci05_model2<-ci05_model
ci25_model2<-ci25_model
ci75_model2<-ci75_model
ci95_model2<-ci95_model
ci50_model2<-ci50_model


ci50_model2$deathscum<-cumsum(ci50_model2$deaths)
ci50_model2$L_deathscum<-cumsum(ci50_model2$H_deaths)
ci50_model2$H_deathscum<-cumsum(ci50_model2$L_deaths)

ci50_sample2<-ci50_model2[seq(1, nrow(ci50_model2), 4), ]


save(ci05_model2, ci25_model2, ci50_model2,
     ci75_model2, ci95_model2, ci50_sample2,
     file = "results_m2x.RData")



library(ggplot2)

## background graph

pal1<-c("#ffffb3","#fdb462","#fb8072","#bebada")

pal2<-c("#2F4F4F","#e31a1c","pink","#1f78b4","#149814","#a28ad2","#f28500","black")

order=c("a","b","c","d","e","f","g","h")

ltype<-c("solid","dashed")

gg<-function(v, initdate){
  
  ggplot() + 
    theme_bw() +
    
    ylim(0,v)+  
    
    scale_x_date(limit=c(as.Date(initdate), max(data$date_plot)), 
                 date_breaks = "1 month",  date_labels = "%b %d") +
    
    geom_hline(yintercept=0, linetype="solid", 
               color = "#9a9a9a", size=0.4)+
    
    geom_vline(xintercept = first_case, linetype="dotdash", 
               color = "grey", size=0.9)+
    annotate(geom="text", x= first_case+2, y= v*0.85, label="First case", size=2.5, angle=90, color="darkgrey")+  
    geom_vline(xintercept = onset_lockdown, linetype="dotdash", 
               color = "grey", size=0.9)+
    annotate(geom="text", x= onset_lockdown+2, y=v*0.85, label="Onset lockdown", size=2.5, angle=90, color="darkgrey")+
    geom_vline(xintercept = end_lockdown, linetype="dashed", 
               color = "darkgrey", size=0.9)+
    annotate(geom="text", x=end_lockdown+2, y=v*0.85, label="End lockdown", size=2.5, angle=90, color="darkgrey")+
    geom_vline(xintercept = relaxation, linetype="dotdash", 
               color = "grey", size=0.9)+
    annotate(geom="text", x=relaxation+2, y=v*0.85, label="Relaxation", size=2.5, angle=90, color="darkgrey")+
    theme(axis.title.x=element_blank(),
          axis.title.y= element_text(size=9),
          legend.title  = element_blank(),
          plot.margin=unit(c(0.1,0.3,0.1,0.1),"cm"),
          legend.text=element_text(size=8),
          legend.position=c(0.18, .5),
          legend.margin=margin(-5,-5,-5,-5),
          legend.box.margin=margin(-50,-50,-50,-50),
          legend.key.width = unit(1.8, 'line'),
          legend.key.height = unit(0.6, 'line'),
          legend.text.align = 0,
          panel.grid.minor = element_line(linetype = "dashed", size=0.1),
          panel.grid.major = element_line(linetype = "dashed", size=0.2)) +
    guides(colour = guide_legend(order = 2),
           shape = guide_legend(order = 1), 
           linetype =  guide_legend(order = 3))
  
}

library(ggplot2)

gA<-gg(350,onset_plot)+  
  geom_ribbon(aes(x=ci05_model$date, ymin = ci05_model$deadi, ymax=ci95_model$deadi), 
              fill=pal2[1], alpha=0.07) +
  geom_ribbon(aes(x=ci25_model$date,  ymin = ci25_model$deadi, ymax=ci75_model$deadi), 
              fill=pal2[1], alpha=0.2) + 
  geom_line(data=ci50_model, aes(x = date, y = deadi, colour=order[1]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, 
            y = deaths, shape="yo"), 
             colour=pal2[1], size=2)+

  labs(y="Daily count")+
  scale_colour_manual(labels=c("Covid-19 deaths (predicted)"), 
                      values=pal2[1])+
  scale_shape_manual(labels=c("Reported number"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))

x11()
gA


gB<-gg(40000,onset_plot)+  
  geom_ribbon(aes(x=ci05_model$date, ymin = cumsum(ci05_model$deadi), ymax=cumsum(ci95_model$deadi)), 
              fill=pal2[1], alpha=0.07) +
  geom_ribbon(aes(x=ci25_model$date,  ymin = cumsum(ci25_model$deadi), ymax=cumsum(ci75_model$deadi)), 
              fill=pal2[1], alpha=0.2) + 
  geom_line(data=ci50_model, aes(x = date, y = cumsum(deadi), colour=order[1]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_point(data=ci50_model, aes(x = date, 
                                  y = cumsum(deaths), shape="yo"), 
             colour=pal2[1], size=2)+
  
  labs(y="Cummulated count")+
  scale_colour_manual(labels=c("Covid-19 deaths (predicted)"), 
                      values=pal2[1])+
  scale_shape_manual(labels=c("Reported number"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))


x11()
gB

gC<-gg(3.5,onset_plot)+ 
  geom_hline(yintercept=1, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_line(data=ci50_model, aes(x = date, y = Ro, colour=order[1]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  geom_ribbon(aes(x=ci25_model$date,  ymin = ci25_model$Rt, ymax=ci75_model$Rt), 
              fill="#6f4e37", alpha=0.5) + 
  geom_line(data=ci50_model, aes(x = date, y = Rt, colour=order[2]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  #geom_ribbon(aes(x=ci25_model$date,  ymin = ci25_model$Rt.2, ymax=ci75_model$Rt.2), 
  #            fill=pal2[3], alpha=0.5) +
  geom_line(data=ci50_model, aes(x = date, y = Rt.2, colour=order[3]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  labs(y="Reproductive number")+
  scale_colour_manual(labels=c("Ro", "Rt","Rt.2"), 
                      values=c(pal2[1], "#6f4e37",pal2[3]))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21), size=9),
              legend.position=c(0.8, .8))

x11()
gC



gD<-gg(1.8,onset_plot)+ 
  geom_hline(yintercept=1, linetype="dashed", 
             color = "dark red", size=0.5)+
  
  geom_line(data=ci50_model, aes(x = date, y = LL.Rt, colour=order[3]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=ci50_model, aes(x = date, y = HH.Rt, colour=order[4]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=ci50_model, aes(x = date, y = LH.Rt, colour=order[5]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=ci50_model, aes(x = date, y = HL.Rt, colour=order[6]),
            linetype="solid", lwd=1.2, alpha=0.8)+

  labs(y="Reproductive number")+
  scale_colour_manual(labels=c("LL","HH","LH","HL"), 
                      values=pal2[c(3,4,5,6)])+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21), size=9),
        legend.position=c(0.8, .8))

x11()
gD



gDD<-gg(3.5,onset_plot)+ 
  geom_hline(yintercept=1, linetype="dashed", 
             color = "dark red", size=0.5)+
  
  geom_line(data=ci50_model, aes(x = date, y = (LL.Rt+LH.Rt), colour=order[3]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_line(data=ci50_model, aes(x = date, y = (HH.Rt+HL.Rt), colour=order[4]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  labs(y="Reproductive number")+
  scale_colour_manual(labels=c("LL.Rt+LH.Rt","HH.Rt+HL.Rt"), 
                      values=pal2[c(3,4)])+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21), size=9),
        legend.position=c(0.8, .8))

x11()
gDD





gE<-gg(1,onset_plot)+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_ribbon(aes(x=ci05_model$date, ymin = (ci05_model$L.S + ci05_model$H.S)/NN, ymax=(ci95_model$L.S+ci95_model$H.S)/NN), 
              fill=pal2[1], alpha=0.07) +
  geom_ribbon(aes(x=ci25_model$date,  ymin = (ci25_model$L.S + ci25_model$H.S)/NN, ymax=(ci75_model$L.S + ci75_model$H.S)/NN), 
              fill=pal2[1], alpha=0.2) + 
  geom_line(data=ci50_model, aes(x = date, y = (L.S+H.S)/NN, colour=order[1]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  labs(y="Susceptible population")+
  scale_colour_manual(labels=c("Susceptible"), 
                      values=c(pal2[1]))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21), size=9),
        legend.position=c(0.8, .8))

x11()
gE


gF<-gg(1,onset_plot)+ 
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_ribbon(aes(x=ci05_model$date, ymin = (ci05_model$L.S)/N.L, ymax=(ci95_model$L.S)/N.L), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25_model$date,  ymin = (ci25_model$L.S)/N.L, ymax=(ci75_model$L.S)/N.L), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50_model, aes(x = date, y = (L.S)/N.L, colour=order[2]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_hline(yintercept=0.5, linetype="dashed", 
             color = "dark red", size=0.5)+
  geom_ribbon(aes(x=ci05_model$date, ymin = (ci05_model$H.S)/N.H, ymax=(ci95_model$H.S)/N.H), 
              fill=pal2[4], alpha=0.07) +
  geom_ribbon(aes(x=ci25_model$date,  ymin = (ci25_model$H.S)/N.H, ymax=(ci75_model$H.S)/N.H), 
              fill=pal2[4], alpha=0.2) + 
  geom_line(data=ci50_model, aes(x = date, y = (H.S)/N.H, colour=order[4]),
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  labs(y="Susceptible population")+
  scale_colour_manual(labels=c("Other","Wealthiest"), 
                      values=c(pal2[c(2,4)]))+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 21), size=9),
        legend.position=c(0.8, .8))

x11()
gF



















g1<-gg(800,onset_plot)+  

          geom_ribbon(aes(x=ci05.ag$date_plot, 
                          ymin = (ci05.ag$exciy.1+ci05.ag$excio.1), ymax=(ci95.ag$exciy.1+ci95.ag$excio.1)), 
                      fill=pal2[2], alpha=0.07) +
          geom_ribbon(aes(x=ci25.ag$date_plot,  
                          ymin =(ci25.ag$exciy.1+ci25.ag$excio.1), ymax=(ci75.ag$exciy.1+ci75.ag$excio.1)), 
                        fill=pal2[2], alpha=0.2) + 
          geom_line(data=ci50.ag, aes(x = date_plot, y = (exciy.1+excio.1), colour=order[2]), 
                       linetype="solid", lwd=1.2, alpha=0.8)+
  
          geom_ribbon(aes(x=ci05.ag$date_plot, 
                          ymin = (ci05.ag$exciy.2+ci05.ag$excio.2), ymax=(ci95.ag$exciy.2+ci95.ag$excio.2)), 
                      fill=pal2[3], alpha=0.07) +
          geom_ribbon(aes(x=ci25.ag$date_plot,  
                          ymin =(ci25.ag$exciy.2+ci25.ag$excio.2), ymax=(ci75.ag$exciy.2+ci75.ag$excio.2)), 
                      fill=pal2[3], alpha=0.2) + 
          geom_line(data=ci50.ag, aes(x = date_plot, y = (exciy.2+excio.2), colour=order[3]), 
                    linetype="solid", lwd=1.2, alpha=0.8)+
  
          geom_ribbon(aes(x=ci05.ag$date_plot, 
                          ymin = (ci05.ag$exciy.3+ci05.ag$excio.3), ymax=(ci95.ag$exciy.3+ci95.ag$excio.3)), 
                      fill=pal2[4], alpha=0.07) +
          geom_ribbon(aes(x=ci25.ag$date_plot,  
                          ymin =(ci25.ag$exciy.3+ci25.ag$excio.3), ymax=(ci75.ag$exciy.3+ci75.ag$excio.3)), 
                      fill=pal2[4], alpha=0.2) + 
          geom_line(data=ci50.ag, aes(x = date_plot, y = (exciy.3+excio.3), colour=order[4]), 
                    linetype="solid", lwd=1.2, alpha=0.8)+
  
          geom_ribbon(aes(x=ci05.ag$date_plot, 
                          ymin = (ci05.ag$exciy.4+ci05.ag$excio.4), ymax=(ci95.ag$exciy.4+ci95.ag$excio.4)), 
                      fill=pal2[5], alpha=0.07) +
          geom_ribbon(aes(x=ci25.ag$date_plot,  
                          ymin =(ci25.ag$exciy.4+ci25.ag$excio.4), ymax=(ci75.ag$exciy.4+ci75.ag$excio.4)), 
                      fill=pal2[5], alpha=0.2) + 
          geom_line(data=ci50.ag, aes(x = date_plot, y = (exciy.4+excio.4), colour=order[5]), 
                    linetype="solid", lwd=1.2, alpha=0.8)+
  
          geom_ribbon(aes(x=ci05.ag$date_plot, 
                          ymin = (ci05.ag$exciy.5+ci05.ag$excio.5), ymax=(ci95.ag$exciy.5+ci95.ag$excio.5)), 
                      fill=pal2[6], alpha=0.07) +
          geom_ribbon(aes(x=ci25.ag$date_plot,  
                          ymin =(ci25.ag$exciy.5+ci25.ag$excio.5), ymax=(ci75.ag$exciy.5+ci75.ag$excio.5)), 
                      fill=pal2[6], alpha=0.2) + 
          geom_line(data=ci50.ag, aes(x = date_plot, y = (exciy.5+excio.5), colour=order[6]), 
                      linetype="solid", lwd=1.2, alpha=0.8)+
  
  
          geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.1+death_excess.2), shape="yo"), 
                     colour=pal2[2], size=2)+
          geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.3+death_excess.4), shape="yo"), 
                     colour=pal2[3], size=2)+
  
          geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.5+death_excess.6), shape="yo"), 
                       colour=pal2[4], size=2)+
          geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.7+death_excess.8), shape="yo"), 
                       colour=pal2[5], size=2)+
          geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.9+death_excess.10), shape="yo"), 
                       colour=pal2[6], size=2)+
  
          labs(y="Weekly excess of non-violent deaths")+
          scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)", 
                                       "Eastern Lima (predicted)", "Northern Lima (predicted)", 
                                       "Southern Lima"), 
                      values=pal2[2:6])+
          scale_shape_manual(labels=c("Reported"), 
                      values=21)+
          theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))




g2<-gg(740,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadiy.1+ci05.ag$deadio.1), ymax=(ci95.ag$deadiy.1+ci95.ag$deadio.1)), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadiy.1+ci25.ag$deadio.1), ymax=(ci75.ag$deadiy.1+ci75.ag$deadio.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadiy.1+deadio.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadiy.2+ci05.ag$deadio.2), ymax=(ci95.ag$deadiy.2+ci95.ag$deadio.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadiy.2+ci25.ag$deadio.2), ymax=(ci75.ag$deadiy.2+ci75.ag$deadio.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadiy.2+deadio.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadiy.3+ci05.ag$deadio.3), ymax=(ci95.ag$deadiy.3+ci95.ag$deadio.3)), 
              fill=pal2[4], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadiy.3+ci25.ag$deadio.3), ymax=(ci75.ag$deadiy.3+ci75.ag$deadio.3)), 
              fill=pal2[4], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadiy.3+deadio.3), colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadiy.4+ci05.ag$deadio.4), ymax=(ci95.ag$deadiy.4+ci95.ag$deadio.4)), 
              fill=pal2[5], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadiy.4+ci25.ag$deadio.4), ymax=(ci75.ag$deadiy.4+ci75.ag$deadio.4)), 
              fill=pal2[5], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadiy.4+deadio.4), colour=order[5]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadiy.5+ci05.ag$deadio.5), ymax=(ci95.ag$deadiy.5+ci95.ag$deadio.5)), 
              fill=pal2[6], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadiy.5+ci25.ag$deadio.5), ymax=(ci75.ag$deadiy.5+ci75.ag$deadio.5)), 
              fill=pal2[6], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadiy.5+deadio.5), colour=order[6]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.1+deaths_cov.2), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.3+deaths_cov.4), shape="yo"), 
             colour=pal2[3], size=2)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.5+deaths_cov.6), shape="yo"), 
             colour=pal2[4], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.7+deaths_cov.8), shape="yo"), 
             colour=pal2[5], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.9+deaths_cov.10), shape="yo"), 
             colour=pal2[6], size=2)+
  
  labs(y="Weekly Covid-19 deaths")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)", 
                               "Eastern Lima (predicted)", "Northern Lima (predicted)", 
                               "Southern Lima"), 
                      values=pal2[2:6])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))




## i have to change this 



g3<-gg(13000,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$exciy.1+ci05.ag$excio.1), ymax=cumsum(ci95.ag$exciy.1+ci95.ag$excio.1)), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$exciy.1+ci25.ag$excio.1), ymax=cumsum(ci75.ag$exciy.1+ci75.ag$excio.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(exciy.1+excio.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$exciy.2+ci05.ag$excio.2), ymax=cumsum(ci95.ag$exciy.2+ci95.ag$excio.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$exciy.2+ci25.ag$excio.2), ymax=cumsum(ci75.ag$exciy.2+ci75.ag$excio.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(exciy.2+excio.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$exciy.3+ci05.ag$excio.3), ymax=cumsum(ci95.ag$exciy.3+ci95.ag$excio.3)), 
              fill=pal2[4], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$exciy.3+ci25.ag$excio.3), ymax=cumsum(ci75.ag$exciy.3+ci75.ag$excio.3)), 
              fill=pal2[4], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(exciy.3+excio.3), colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$exciy.4+ci05.ag$excio.4), ymax=cumsum(ci95.ag$exciy.4+ci95.ag$excio.4)), 
              fill=pal2[5], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$exciy.4+ci25.ag$excio.4), ymax=cumsum(ci75.ag$exciy.4+ci75.ag$excio.4)), 
              fill=pal2[5], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(exciy.4+excio.4), colour=order[5]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$exciy.5+ci05.ag$excio.5), ymax=cumsum(ci95.ag$exciy.5+ci95.ag$excio.5)), 
              fill=pal2[6], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$exciy.5+ci25.ag$excio.5), ymax=cumsum(ci75.ag$exciy.5+ci75.ag$excio.5)), 
              fill=pal2[6], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(exciy.5+excio.5), colour=order[6]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(death_excess.1+death_excess.2), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(death_excess.3+death_excess.4), shape="yo"), 
             colour=pal2[3], size=2)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(death_excess.5+death_excess.6), shape="yo"), 
             colour=pal2[4], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(death_excess.7+death_excess.8), shape="yo"), 
             colour=pal2[5], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(death_excess.9+death_excess.10), shape="yo"), 
             colour=pal2[6], size=2)+
  
  labs(y="Cumulative excess of non-violent deaths")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)", 
                               "Eastern Lima (predicted)", "Northern Lima (predicted)", 
                               "Southern Lima"), 
                      values=pal2[2:6])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))




g4<-gg(10000,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$deadiy.1+ci05.ag$deadio.1), ymax=cumsum(ci95.ag$deadiy.1+ci95.ag$deadio.1)), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$deadiy.1+ci25.ag$deadio.1), ymax=cumsum(ci75.ag$deadiy.1+ci75.ag$deadio.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadiy.1+deadio.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$deadiy.2+ci05.ag$deadio.2), ymax=cumsum(ci95.ag$deadiy.2+ci95.ag$deadio.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$deadiy.2+ci25.ag$deadio.2), ymax=cumsum(ci75.ag$deadiy.2+ci75.ag$deadio.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadiy.2+deadio.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$deadiy.3+ci05.ag$deadio.3), ymax=cumsum(ci95.ag$deadiy.3+ci95.ag$deadio.3)), 
              fill=pal2[4], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$deadiy.3+ci25.ag$deadio.3), ymax=cumsum(ci75.ag$deadiy.3+ci75.ag$deadio.3)), 
              fill=pal2[4], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadiy.3+deadio.3), colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$deadiy.4+ci05.ag$deadio.4), ymax=cumsum(ci95.ag$deadiy.4+ci95.ag$deadio.4)), 
              fill=pal2[5], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$deadiy.4+ci25.ag$deadio.4), ymax=cumsum(ci75.ag$deadiy.4+ci75.ag$deadio.4)), 
              fill=pal2[5], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadiy.4+deadio.4), colour=order[5]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = cumsum(ci05.ag$deadiy.5+ci05.ag$deadio.5), ymax=cumsum(ci95.ag$deadiy.5+ci95.ag$deadio.5)), 
              fill=pal2[6], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =cumsum(ci25.ag$deadiy.5+ci25.ag$deadio.5), ymax=cumsum(ci75.ag$deadiy.5+ci75.ag$deadio.5)), 
              fill=pal2[6], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadiy.5+deadio.5), colour=order[6]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.1+deaths_cov.2), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.3+deaths_cov.4), shape="yo"), 
             colour=pal2[3], size=2)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.5+deaths_cov.6), shape="yo"), 
             colour=pal2[4], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.7+deaths_cov.8), shape="yo"), 
             colour=pal2[5], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.9+deaths_cov.10), shape="yo"), 
             colour=pal2[6], size=2)+
  
  labs(y="Weekly Covid-19 deaths")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)", 
                               "Eastern Lima (predicted)", "Northern Lima (predicted)", 
                               "Southern Lima"), 
                      values=pal2[2:6])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))



x11()
g4



#### rates only for covid 19


### GRAPHS COMPARING ONLY OLDER AGE

g5<-gg(800,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$excio.1), ymax=ci95.ag$excio.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$excio.1), ymax=(ci75.ag$excio.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (excio.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$excio.2), ymax=(ci95.ag$excio.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$excio.2), ymax=(ci75.ag$excio.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (excio.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.2), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.4), shape="yo"), 
             colour=pal2[3], size=2)+
  
  labs(y="Weekly excess of non-violent deaths")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)"), 
                      values=pal2[2:3])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))


g6<-gg(500,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadio.1), ymax=ci95.ag$deadio.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadio.1), ymax=(ci75.ag$deadio.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadio.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadio.2), ymax=(ci95.ag$deadio.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadio.2), ymax=(ci75.ag$deadio.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadio.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.2), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.4), shape="yo"), 
             colour=pal2[3], size=2)+
  
  labs(y="Weekly COVID-19 deaths")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)"), 
                      values=pal2[2:3])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))









g7<-gg(1100,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$excio.1/100000*No.1), ymax=ci95.ag$excio.1/100000*No.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$excio.1/100000*No.1), ymax=(ci75.ag$excio.1/100000*No.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (excio.1/100000*No.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$excio.2/100000*No.2), ymax=(ci95.ag$excio.2/100000*No.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$excio.2/100000*No.2), ymax=(ci75.ag$excio.2/100000*No.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (excio.2/100000*No.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.2/100000*No.1), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (death_excess.4/100000*No.2), shape="yo"), 
             colour=pal2[3], size=2)+
  
  labs(y="Weekly excess of non-violent deaths (x100,000 hab.)")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)"), 
                      values=pal2[2:3])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))


g8<-gg(900,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadio.1/100000*No.1), ymax=ci95.ag$deadio.1/100000*No.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadio.1/100000*No.1), ymax=(ci75.ag$deadio.1/100000*No.1)), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadio.1/100000*No.1), colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, 
                  ymin = (ci05.ag$deadio.2/100000*No.2), ymax=(ci95.ag$deadio.2/100000*No.2)), 
              fill=pal2[3], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  
                  ymin =(ci25.ag$deadio.2/100000*No.2), ymax=(ci75.ag$deadio.2/100000*No.2)), 
              fill=pal2[3], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = (deadio.2/100000*No.2), colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.2/100000*No.1), shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = (deaths_cov.4/100000*No.2), shape="yo"), 
             colour=pal2[3], size=2)+
  
  labs(y="Weekly COVID-19 deaths (x100,000 hab.)")+
  scale_colour_manual(labels=c("Lima centre 1 (predicted)","Lima centre 2 (predicted)"), 
                      values=pal2[2:3])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))
















#### Luego!!!!!   para saber rates predichos only for covid


g5<-gg(35,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = ci05.ag$deadi.1*100000/N.1, ymax=ci95.ag$deadi.1*100000/N.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = ci25.ag$deadi.1*100000/N.1, ymax=ci75.ag$deadi.1*100000/N.1), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = deadi.1*100000/N.1, colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = ci05.ag$deadi.2*100000/N.2, ymax=ci95.ag$deadi.2*100000/N.2), 
              fill=pal2[3], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = ci25.ag$deadi.2*100000/N.2, ymax=ci75.ag$deadi.2*100000/N.2), 
              fill=pal2[3], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = deadi.2*100000/N.2, colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = ci05.ag$deadi.3*100000/N.3, ymax=ci95.ag$deadi.3*100000/N.3), 
              fill=pal2[4], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = ci25.ag$deadi.3*100000/N.3, ymax=ci75.ag$deadi.3*100000/N.3), 
              fill=pal2[4], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = deadi.3*100000/N.3, colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = ci05.ag$deadi.4*100000/N.4, ymax=ci95.ag$deadi.4*100000/N.4), 
              fill=pal2[5], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = ci25.ag$deadi.4*100000/N.4, ymax=ci75.ag$deadi.4*100000/N.4), 
              fill=pal2[5], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = deadi.4*100000/N.4, colour=order[5]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = deaths_cov.1*100000/N.1, shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = deaths_cov.2*100000/N.2, shape="yo"), 
             colour=pal2[3], size=2)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = deaths_cov.3*100000/N.3, shape="yo"), 
             colour=pal2[4], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = deaths_cov.4*100000/N.4, shape="yo"), 
             colour=pal2[5], size=2)+
  
  labs(y="Weekly Covid-19 deaths (x100,000 hab.)")+
  scale_colour_manual(labels=c("Group 1 (predicted)","Group 2 (predicted)", 
                               "Group 3 (predicted)", "Group 4 (predicted)"), 
                      values=pal2[2:5])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))




g6<-gg(700,onset_plot)+  
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = cumsum(ci05.ag$deadi.1)*100000/N.1, ymax=cumsum(ci95.ag$deadi.1)*100000/N.1), 
              fill=pal2[2], alpha=0.07) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = cumsum(ci25.ag$deadi.1)*100000/N.1, ymax=cumsum(ci75.ag$deadi.1)*100000/N.1), 
              fill=pal2[2], alpha=0.2) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadi.1)*100000/N.1, colour=order[2]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = cumsum(ci05.ag$deadi.2)*100000/N.2, ymax=cumsum(ci95.ag$deadi.2)*100000/N.2), 
              fill=pal2[3], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = cumsum(ci25.ag$deadi.2)*100000/N.2, ymax=cumsum(ci75.ag$deadi.2)*100000/N.2), 
              fill=pal2[3], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadi.2)*100000/N.2, colour=order[3]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = cumsum(ci05.ag$deadi.3)*100000/N.3, ymax=cumsum(ci95.ag$deadi.3)*100000/N.3), 
              fill=pal2[4], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = cumsum(ci25.ag$deadi.3)*100000/N.3, ymax=cumsum(ci75.ag$deadi.3)*100000/N.3), 
              fill=pal2[4], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadi.3)*100000/N.3, colour=order[4]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_ribbon(aes(x=ci05.ag$date_plot, ymin = cumsum(ci05.ag$deadi.4)*100000/N.4, ymax=cumsum(ci95.ag$deadi.4)*100000/N.4), 
              fill=pal2[5], alpha=0.1) +
  geom_ribbon(aes(x=ci25.ag$date_plot,  ymin = cumsum(ci25.ag$deadi.4)*100000/N.4, ymax=cumsum(ci75.ag$deadi.4)*100000/N.4), 
              fill=pal2[5], alpha=0.5) + 
  geom_line(data=ci50.ag, aes(x = date_plot, y = cumsum(deadi.4)*100000/N.4, colour=order[5]), 
            linetype="solid", lwd=1.2, alpha=0.8)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.1)*100000/N.1, shape="yo"), 
             colour=pal2[2], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.2)*100000/N.2, shape="yo"), 
             colour=pal2[3], size=2)+
  
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.3)*100000/N.3, shape="yo"), 
             colour=pal2[4], size=2)+
  geom_point(data=ci50.ag, aes(x = date_plot, y = cumsum(deaths_cov.4)*100000/N.4, shape="yo"), 
             colour=pal2[5], size=2)+
  
  labs(y="Cumulative Covid-19 deaths (x100,000 hab.")+
  scale_colour_manual(labels=c("Group 1 (predicted)","Group 2 (predicted)", 
                               "Group 3 (predicted)", "Group 4 (predicted)"), 
                      values=pal2[2:5])+
  scale_shape_manual(labels=c("Reported"), 
                     values=21)+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 9), size=9))








library(ggpubr)

jpeg(file ="Alldistricts_class.jpeg", width = 12*360, height = 12*360, units = "px", pointsize =7, res = 465) 

ggarrange(gA,gB,gC,
    heights = c(1, 1, 1), ncol = 1, nrow=3, labels = c("A", "B","C"))

dev.off() 


jpeg(file ="deaths_by_class.jpeg", width = 16*360, height = 12*360, units = "px", pointsize =7, res = 465) 

ggarrange(g1,g2,g3,g4,
          heights = c(1, 1, 1,1), ncol = 2, nrow=2, labels = c("A", "B","C","D"))

dev.off() 



jpeg(file ="cov_deathrates_by_class.jpeg", width = 16*360, height = 6*360, units = "px", pointsize =7, res = 465) 

ggarrange(g5,g6,
          heights = c(1, 1), ncol = 2, nrow=1, labels = c("A", "B"))

dev.off() 



jpeg(file ="prelfigur.jpeg", width = 16*360, 
     height = 6*360, units = "px", pointsize =7, res = 465) 

ggarrange(gC,gE,
          heights = c(1, 1), ncol = 2, nrow=1, labels = c("A", "B"))

dev.off() 










  