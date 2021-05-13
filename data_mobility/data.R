
data<-read.csv("db_model161120.csv") 

data$date_plot<-as.Date(data$date_plot)

data <- data[order(data$date_plot),]

# Main information
onset_data<-datastartdate<-datastart_date<-as.Date("2020-03-02")  #onset week 11 

date_sero<-as.Date("2020-07-04")
time_sero<- as.numeric(date_sero-datastart_date) # number of days since onsetdata

L.dmorti_y_w<-subset(data, class==1 & age_class=="0-59" & gender==1)$deaths
L.dmorti_y_m<-subset(data, class==1 & age_class=="0-59" & gender==2)$deaths
L.dmorti_o_w<-subset(data, class==1 & age_class=="60-+" & gender==1)$deaths
L.dmorti_o_m<-subset(data, class==1 & age_class=="60-+" & gender==2)$deaths

H.dmorti_y_w<-subset(data, class==2 & age_class=="0-59" & gender==1)$deaths
H.dmorti_o_w<-subset(data, class==2 & age_class=="60-+" & gender==1)$deaths
H.dmorti_y_m<-subset(data, class==2 & age_class=="0-59" & gender==2)$deaths
H.dmorti_o_m<-subset(data, class==2 & age_class=="60-+" & gender==2)$deaths

L.dmorti <- L.dmorti_y_w + L.dmorti_y_m + L.dmorti_o_w + L.dmorti_o_m
H.dmorti <- H.dmorti_y_w + H.dmorti_y_m + H.dmorti_o_w + H.dmorti_o_m

dmorti<-L.dmorti+H.dmorti

dmort<-cumsum(dmorti)

L.dmort<-cumsum(L.dmorti)
H.dmort<-cumsum(H.dmorti)

L.N_y_w<- unique(data$pob[data$class==1 & data$age_class=="0-59" & data$gender==1])
L.N_y_m<- unique(data$pob[data$class==1 & data$age_class=="0-59" & data$gender==2])
L.N_o_w<- unique(data$pob[data$class==1 & data$age_class=="60-+" & data$gender==1])
L.N_o_m<- unique(data$pob[data$class==1 & data$age_class=="60-+" & data$gender==2])

H.N_y_w<- unique(data$pob[data$class==2 & data$age_class=="0-59" & data$gender==1])
H.N_y_m<- unique(data$pob[data$class==2 & data$age_class=="0-59" & data$gender==2]) 
H.N_o_w<- unique(data$pob[data$class==2 & data$age_class=="60-+" & data$gender==1])
H.N_o_m<- unique(data$pob[data$class==2 & data$age_class=="60-+" & data$gender==2]) 

L.N<- L.N_y_w + L.N_y_m + L.N_o_w + L.N_o_m

H.N<- H.N_y_w + H.N_y_m + H.N_o_w + H.N_o_m 

NN<-L.N+H.N


NN.o<-L.N_o_w + L.N_o_m + H.N_o_w + H.N_o_m 
NN.y<-L.N_y_w + L.N_y_m + H.N_y_w + H.N_y_m 

# serology

##serology july

s_Total<-c(0.202,6.9)
s_Callao<-c(0.273,19)
s_Centre<-c(0.188,15)
s_East<-c(0.21,17)
s_North<-c(0.21,14)
s_South<-c(0.176,12)

sero_L<-  0.208
sero_H<-  (s_Total[1]*NN- sero_L*L.N)/H.N  


sero_o<-0.16

sero_y= 0.21

sero_w=0.205
sero_m=0.202




## data human mobility 
#https://www.iadb.org/en/topics-effectiveness-improving-lives/coronavirus-impact-dashboard 

mob <- read.csv("MobilityLima.csv")
mob$date<-as.Date(mob$date, format = "%B %d, %Y")
min(mob$date)
c <- subset(mob, date<as.Date("2020-05-30"))
s <- (mob$mob_LimaProv)*100
#plot(s, type='l')


optimpw <- function(par) {
  i1 <- floor(par[1])
  i2 <- floor(par[2])
  a <- par[3]
  b <- par[4]
  
  if (i1 > i2)
    return(Inf)
  
  if (i1 < 1 || i1 > length(s) || i2 < 1 || i2 > length(s))
    return(Inf)
  
  i1 <- max(2, min(i2, i1))
  i2 <- min(length(s)-1, max(i2, i1))
  
  e1 <- sum(abs(a - s[1:(i1 - 1)]))
  e2 <- sum(abs(seq(a, b, length.out=(i2 - i1 + 1)) - s[i1:i2]))
  e3 <- sum(abs(b - s[(i2 + 1):length(s)]))
  
  e1 + e2 + e3
}

#init <- c(20, 35, 0, -150) # for first data
inito <- c(10, 23, 0, -65) # for second data


control <- NULL
control$maxit <- 100000
control$parscale <- c(1, 1, 1, 1)
o <- optim(inito, optimpw, control=control)
o <- optim(o$par, optimpw, control=control)

print(o$par)

d0 <- mob$date[round(o$par[1])]
d1 <- mob$date[round(o$par[2])]



## Date of lockdown phase (1)
lockdown_offset <- as.numeric(d0 - datastartdate)

## over how many days the lockdown is estimated to have occurred
lockdown_transition_period <- as.numeric(d1 - d0)+1



## how many deaths at date of lockdown
total_deaths_at_lockdown <- 2


month2_date <-as.Date("2020/04/13") ## Day after semana santa (immobilization), the exits by gender.... curfew from 18h-5h)
month3_date <-as.Date("2020/06/5") ##  Second phase reactivation 
month4_date <-as.Date("2020/07/1") ##  Third phase reactivation + levantamiento de cuarentena dominical
month5_date <-as.Date("2020/08/1") ## 
month6_date <-as.Date("2020/09/1") ##  
month7_date <-as.Date("2020/10/1")  ##  Fourth phase reactivation  



point2_offset <- as.numeric(month2_date - datastart_date)
point3_offset <- as.numeric(month3_date - datastart_date)
point4_offset <- as.numeric(month4_date - datastart_date)
point5_offset <- as.numeric(month5_date - datastart_date)
point6_offset <- as.numeric(month6_date - datastart_date)
point7_offset <- as.numeric(month7_date - datastart_date)


FitTotalPeriod = 270
mort_nbinom_size1 = 50
mort_nbinom_size2 = 15





