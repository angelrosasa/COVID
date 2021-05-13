library("deSolve")
system("R CMD SHLIB model2.cpp")
dyn.load("model2.dll")

InvalidDataOffset <- 10000
Initial <- 1


G<- 4.7

Tinc1 <- 3
Tinc2 <- 1

a1 <- 1/Tinc1
a2 <- 1/Tinc2

gamma <- 1/((G - (Tinc1+Tinc2)) * 2)

calcGammaProfile <- function(mean, sd)
{
    shape = mean^2 / sd^2
    scale = sd^2 / mean
    
    kbegin = max(0, ceiling(mean - sd * 3))
    kend = max(kbegin + 1, floor(mean + sd * 3))

    result = NULL
    result$kbegin = -kend
    result$kend = -kbegin
    result$values = numeric(result$kend - result$kbegin)
    i = 1
    for (k in kbegin:kend) {
        result$values[i] = pgamma(k-0.5, shape=shape, scale=scale) -
            pgamma(k+0.5, shape=shape, scale=scale)
        i = i + 1
    }

    result$values = result$values / sum(result$values)
    result$values = rev(result$values)

    result
}

convolute <- function(values, i1, i2, profile)
{
 filter(values, profile$values, method="convolution", sides=1)[(i1 + profile$kend):(i2 + profile$kend)]
}


diedProfile <- calcGammaProfile


calculateModel <- function(params, period)
{

     beta0_LL <- params[1]
     beta0_HH <- params[2]
     beta0_LH <- params[3]
     beta0_HL <- params[4]
    
     beta1_LL <- params[5]
     beta1_HH <- params[6]
     beta1_LH <- params[7]
     beta1_HL <- params[8]
    
     beta2_LL <- params[9]
     beta2_HH <- params[10]
     beta2_LH <- params[11]
     beta2_HL <- params[12]
    
     beta3_LL <- params[13]
     beta3_HH <- params[14]
     beta3_LH <- params[15]
     beta3_HL <- params[16]
     
     beta4_LL <- params[17]
     beta4_HH <- params[18]
     beta4_LH <- params[19]
     beta4_HL <- params[20]
     
     beta5_LL <- params[21]
     beta5_HH <- params[22]
     beta5_LH <- params[23]
     beta5_HL <- params[24]
     
     beta6_LL <- params[25]
     beta6_HH <- params[26]
     beta6_LH <- params[27]
     beta6_HL <- params[28]
     
     beta7_LL <- params[29]
     beta7_HH <- params[30]
     beta7_LH <- params[31]
     beta7_HL <- params[32]
     
     beta8_LL <- params[33]
     beta8_HH <- params[34]
     beta8_LH <- params[35]
     beta8_HL <- params[36]
     
     died_latency <- params[37]
     DLsd <- params[38]
     died_rate.L <- params[39]
     died_rate.H <- params[40]
     t0_morts <- params[41]


    
    ## convolution profile to infer death count

    died_cv_profile = diedProfile(died_latency, DLsd)


    padding = max(-died_cv_profile$kbegin) + 1

    state <- NULL

    state$L.S <- rep(N.L-Initial, padding + period)
    state$L.E1 <- rep(Initial, padding + period)
    state$L.E2 <- rep(0, padding + period)
    state$L.I <- rep(0, padding + period)
    state$L.R <- rep(0, padding + period)
    state$L.died <- rep(0, padding + period)

    state$H.S <- rep(N.H - Initial, padding + period)
    state$H.E1 <- rep(Initial, padding + period)
    state$H.E2 <- rep(0, padding + period)
    state$H.I <- rep(0, padding + period)
    state$H.R <- rep(0, padding + period)
    state$H.died <- rep(0, padding + period)

    state$Ro <- rep(0, padding + period)
    state$Rt <- rep(0, padding + period)
    
    state$LL.Rt <- rep(0, padding + period)
    state$HH.Rt <- rep(0, padding + period)
    state$LH.Rt <- rep(0, padding + period)
    state$HL.Rt <- rep(0, padding + period)
    
    state$Rt.2 <- rep(0, padding + period)
    
    state$i <- padding + 1
    
    parms <- c(beta0_LL = beta0_LL,
               beta0_HH = beta0_HH,
               beta0_LH = beta0_LH,
               beta0_HL = beta0_HL,
               beta1_LL = beta1_LL,
               beta1_HH = beta1_HH,
               beta1_LH = beta1_LH,
               beta1_HL = beta1_HL,
               beta2_LL = beta2_LL,
               beta2_HH = beta2_HH,
               beta2_LH = beta2_LH,
               beta2_HL = beta2_HL,
               beta3_LL = beta3_LL,
               beta3_HH = beta3_HH,
               beta3_LH = beta3_LH,
               beta3_HL = beta3_HL,
               beta4_LL = beta4_LL,
               beta4_HH = beta4_HH,
               beta4_LH = beta4_LH,
               beta4_HL = beta4_HL,
               beta5_LL = beta5_LL,
               beta5_HH = beta5_HH,
               beta5_LH = beta5_LH,
               beta5_HL = beta5_HL,
               beta6_LL = beta6_LL,
               beta6_HH = beta6_HH,
               beta6_LH = beta6_LH,
               beta6_HL = beta6_HL,
               beta7_LL = beta7_LL,
               beta7_HH = beta7_HH,
               beta7_LH = beta7_LH,
               beta7_HL = beta7_HL,
               beta8_LL = beta8_LL,
               beta8_HH = beta8_HH,
               beta8_LH = beta8_LH,
               beta8_HL = beta8_HL,
               N_L = N.L,
               N_H = N.H,
               t0 = 1E10,
               t1 = 1E10,
               t2 = 1E10,
               t3 = 1E10,
               t4 = 1E10,
               t5 = 1E10,
               t6 = 1E10,
               t7 = 1E10,
               t8 = 1E10,
               a1 = a1,
               a2 = a2,
               gamma = gamma
    )

    Y <- c(S_L = N.L-Initial, E1_L = Initial, E2_L = 0, I_L = 0, R_L = 0,
           S_H = N.H - Initial, E1_H = Initial, E2_H = 0, I_H = 0, R_H = 0)

    times <- (padding + 1):(padding + period)

    out <- ode(Y, times, func = "derivs", parms = parms,
               dllname = "model2",
               initfunc = "initmod", nout = 7, outnames = c("Ro", "Rt", "LL_Rt", "HH_Rt","LH_Rt", "HL_Rt","Rt2"))

    state$L.S[(padding + 1):(padding + period)] = out[,2]
    state$H.S[(padding + 1):(padding + period)] = out[,7]

    s2 <- convolute(state$L.S, padding + 1, padding + period, died_cv_profile)
    state$L.died[(padding + 1):(padding + period)] = (N.L - s2) * died_rate.L

    s2 <- convolute(state$H.S, padding + 1, padding + period, died_cv_profile)
    state$H.died[(padding + 1):(padding + period)] = (N.H - s2) * died_rate.H

    state$died = state$L.died + state$H.died

    data_offset = InvalidDataOffset

    lds <- which(state$died > t0_morts)
    if (length(lds) > 0) {
        data_offset <- lds[1] - lockdown_offset

        t0 <- data_offset + lockdown_offset
        t1 <- data_offset + lockdown_offset + lockdown_transition_period
        t2 <- data_offset + point2_offset
        t3 <- data_offset + point3_offset
        t4 <- data_offset + point4_offset
        t5 <- data_offset + point5_offset
        t6 <- data_offset + point6_offset
        t7 <- data_offset + point7_offset
        t8 <- 4e2
        
        parms <- c(beta0_LL = beta0_LL,
                   beta0_HH = beta0_HH,
                   beta0_LH = beta0_LH,
                   beta0_HL = beta0_HL,
                   beta1_LL = beta1_LL,
                   beta1_HH = beta1_HH,
                   beta1_LH = beta1_LH,
                   beta1_HL = beta1_HL,
                   beta2_LL = beta2_LL,
                   beta2_HH = beta2_HH,
                   beta2_LH = beta2_LH,
                   beta2_HL = beta2_HL,
                   beta3_LL = beta3_LL,
                   beta3_HH = beta3_HH,
                   beta3_LH = beta3_LH,
                   beta3_HL = beta3_HL,
                   beta4_LL = beta4_LL,
                   beta4_HH = beta4_HH,
                   beta4_LH = beta4_LH,
                   beta4_HL = beta4_HL,
                   beta5_LL = beta5_LL,
                   beta5_HH = beta5_HH,
                   beta5_LH = beta5_LH,
                   beta5_HL = beta5_HL,
                   beta6_LL = beta6_LL,
                   beta6_HH = beta6_HH,
                   beta6_LH = beta6_LH,
                   beta6_HL = beta6_HL,
                   beta7_LL = beta7_LL,
                   beta7_HH = beta7_HH,
                   beta7_LH = beta7_LH,
                   beta7_HL = beta7_HL,
                   beta8_LL = beta8_LL,
                   beta8_HH = beta8_HH,
                   beta8_LH = beta8_LH,
                   beta8_HL = beta8_HL,
                   N_L = N.L,
                   N_H = N.H,
                   t0 = t0,
                   t1 = t1,
                   t2 = t2,
                   t3 = t3,
                   t4 = t4,
                   t5 = t5,
                   t6 = t6,
                   t7 = t7,
                   t8 = t8,
                   a1 = a1,
                   a2 = a2,
                   gamma = gamma
        )

        out <- ode(Y, times, func = "derivs", parms = parms,
                   dllname = "model2",
                   initfunc = "initmod", nout = 7, outnames = c("Ro", "Rt", "LL_Rt", "HH_Rt","LH_Rt", "HL_Rt","Rt2"))
     }

    state$L.S[(padding + 1):(padding + period)] = out[,2]
    state$L.E[(padding + 1):(padding + period)] = out[,3]+out[,4]
    state$L.I[(padding + 1):(padding + period)] = out[,5]
    state$L.R[(padding + 1):(padding + period)] = out[,6]
    state$H.S[(padding + 1):(padding + period)] = out[,7]
    state$H.E[(padding + 1):(padding + period)] = out[,8]+out[,9]
    state$H.I[(padding + 1):(padding + period)] = out[,10]
    state$H.R[(padding + 1):(padding + period)] = out[,11]
    state$Ro[(padding + 1):(padding + period)] = out[,12]
    state$Rt[(padding + 1):(padding + period)] = out[,13]
    state$LL.Rt[(padding + 1):(padding + period)] = out[,14]
    state$HH.Rt[(padding + 1):(padding + period)] = out[,15]
    state$LH.Rt[(padding + 1):(padding + period)] = out[,16]
    state$HL.Rt[(padding + 1):(padding + period)] = out[,17]
    state$Rt.2[(padding + 1):(padding + period)] = out[,18]
  
    
    s2 <- convolute(state$L.S, padding + 1, padding + period, died_cv_profile)
    state$L.died[(padding + 1):(padding + period)] = (N.L - s2) * died_rate.L

    state$L.deadi <- c(state$L.died[1], diff(state$L.died))


    
    s2 <- convolute(state$H.S, padding + 1, padding + period, died_cv_profile)
    state$H.died[(padding + 1):(padding + period)] = (N.H - s2) * died_rate.H
    
    state$H.deadi <- c(state$H.died[1], diff(state$H.died))

    state$padding <- padding
    state$offset <- data_offset

    state
}

transformParams <- function(params)
{
    params
}

invTransformParams <- function(posterior)
{
    posterior$Tinf <- 1/gamma

    posterior
}

calcNominalState <- function(state)
{
    state$died <- state$L.died + state$H.died    
    state$deadi <- state$L.deadi + state$H.deadi
    state$S <- state$L.S + state$H.S 
    state$R <- state$L.R + state$H.R 
    state$I <- state$L.I + state$H.I 
    state$E <- state$L.E + state$H.E 
    state
}

## log likelihood function for fitting this model to observed data:
##   L.dmorti, H.dmorti

calclogp <- function(params) {
    
    beta0_LL <- params[1]
    beta0_HH <- params[2]
    beta0_LH <- params[3]
    beta0_HL <- params[4]
    
    beta1_LL <- params[5]
    beta1_HH <- params[6]
    beta1_LH <- params[7]
    beta1_HL <- params[8]
    
    beta2_LL <- params[9]
    beta2_HH <- params[10]
    beta2_LH <- params[11]
    beta2_HL <- params[12]
    
    beta3_LL <- params[13]
    beta3_HH <- params[14]
    beta3_LH <- params[15]
    beta3_HL <- params[16]
    
    beta4_LL <- params[17]
    beta4_HH <- params[18]
    beta4_LH <- params[19]
    beta4_HL <- params[20]
    
    beta5_LL <- params[21]
    beta5_HH <- params[22]
    beta5_LH <- params[23]
    beta5_HL <- params[24]
    
    beta6_LL <- params[25]
    beta6_HH <- params[26]
    beta6_LH <- params[27]
    beta6_HL <- params[28]
    
    beta7_LL <- params[29]
    beta7_HH <- params[30]
    beta7_LH <- params[31]
    beta7_HL <- params[32]
    
    beta8_LL <- params[33]
    beta8_HH <- params[34]
    beta8_LH <- params[35]
    beta8_HL <- params[36]
    
    died_latency <- params[37]
    DLsd <- params[38]
    died_rate.L <- params[39]
    died_rate.H <- params[40]
    t0_morts <- params[41]

    
    logPriorP <- 0
    
    logPriorP <- logPriorP + dnorm(died_latency, mean=21, sd=4, log=T)

    logPriorP <- logPriorP + dnorm(DLsd, mean=5, sd=1, log=T)

    logPriorP <- logPriorP + dbeta(died_rate.L, ifrbeta$alpha, ifrbeta$beta, log=T)
    logPriorP <- logPriorP + dbeta(died_rate.H, ifrbeta$alpha, ifrbeta$beta, log=T)
    
    logPriorP <- logPriorP + dnorm(beta0_LL - beta0_HH, mean=0, sd=0.2*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta0_LL - beta0_LH, mean=0, sd=0.2*gamma, log=T)
    logPriorP <- logPriorP + dnorm(beta0_HL - beta0_LH, mean=0, sd=0.2*gamma, log=T)

    logPriorP <- logPriorP + dnorm(beta0_LL - beta1_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta0_HH - beta1_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta0_LH - beta1_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta0_HL - beta1_HL, mean=0, sd=0.5*gamma, log=T)
    
    logPriorP <- logPriorP + dnorm(beta1_LL - beta2_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta1_HH - beta2_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta1_LH - beta2_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta1_HL - beta2_HL, mean=0, sd=0.5*gamma, log=T)
    
    logPriorP <- logPriorP + dnorm(beta2_LL - beta3_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta2_HH - beta3_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta2_LH - beta3_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta2_HL - beta3_HL, mean=0, sd=0.5*gamma, log=T)

    logPriorP <- logPriorP + dnorm(beta3_LL - beta4_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta3_HH - beta4_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta3_LH - beta4_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta3_HL - beta4_HL, mean=0, sd=0.5*gamma, log=T)
  
    logPriorP <- logPriorP + dnorm(beta4_LL - beta5_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta4_HH - beta5_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta4_LH - beta5_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta4_HL - beta5_HL, mean=0, sd=0.5*gamma, log=T)
    
    logPriorP <- logPriorP + dnorm(beta5_LL - beta6_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta5_HH - beta6_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta5_LH - beta6_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta5_HL - beta6_HL, mean=0, sd=0.5*gamma, log=T)
    
    logPriorP <- logPriorP + dnorm(beta6_LL - beta7_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta6_HH - beta7_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta6_LH - beta7_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta6_HL - beta7_HL, mean=0, sd=0.5*gamma, log=T)
  
    logPriorP <- logPriorP + dnorm(beta7_LL - beta8_LL, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta7_HH - beta8_HH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta7_LH - beta8_LH, mean=0, sd=0.5*gamma, log=T)    
    logPriorP <- logPriorP + dnorm(beta7_HL - beta8_HL, mean=0, sd=0.5*gamma, log=T)
    

    logPriorP
}

calclogl <- function(params, x) {
    state <<- calculateModel(params, FitTotalPeriod)

    if (state$offset == InvalidDataOffset)
        state$offset = 1

    dstart <- state$offset
    dend <- state$offset + length(L.dmorti) - 1

    if (dend > length(state$L.deadi)) {
        ##print("=========================== Increase FitTotalPeriod ===================")
        dend <- length(state$L.deadi)
        dstart <- dend - length(L.dmorti) + 1
    }

    if (dstart < 1) {
        ##print("=========================== Increase Padding ? ===================")
        dstart <- 1
        dend <- dstart + length(L.dmorti) - 1
    }


    
    L.loglD <- sum(dnbinom(L.dmorti,
                           mu=pmax(0.1,state$L.deadi[dstart:dend]),
                           size=mort_nbinom_size1, log=T))

    H.loglD <- sum(dnbinom(H.dmorti,
                           mu=pmax(0.1,  state$H.deadi[dstart:dend]),
                           size=mort_nbinom_size1, log=T))
  
    
    L.loglsero <- sum(dnorm(sero_L,
                            mean= state$L.R[dstart+time_sero]/(N.L),
                            sd=sdd(sero_L,1000), log=T))
    
    H.loglsero <- sum(dnorm(sero_H,
                            mean= state$H.R[dstart+time_sero]/(N.H),
                            sd=sdd(sero_H,1000), log=T))
    
    #loglRo <- sum(dnorm(val_Ro[1],
    #                        mean= state$Rt[dstart+time_R0],
    #                        sd=((val_Ro[3]-val_Ro[2])/3.92), log=T))
    
    it <<- it + 1

    result <- L.loglD + H.loglD + 
               L.loglsero + H.loglsero  
    
    if (it %% 1000 == 0) {
        print(params)
	print(c(it, result))
        state <<- calcNominalState(state)
	graphs()
    }

    result
}

fit.paramnames <- c(
    "beta0_LL",	"beta0_HH",	"beta0_LH",	"beta0_HL",	
    "beta1_LL",	"beta1_HH",	"beta1_LH",	"beta1_HL",	
    "beta2_LL",	"beta2_HH",	"beta2_LH",	"beta2_HL",	
    "beta3_LL",	"beta3_HH",	"beta3_LH",	"beta3_HL",	
    "beta4_LL",	"beta4_HH",	"beta4_LH",	"beta4_HL",	
    "beta5_LL",	"beta5_HH",	"beta5_LH",	"beta5_HL",	
    "beta6_LL",	"beta6_HH",	"beta6_LH",	"beta6_HL",
    "beta7_LL",	"beta7_HH",	"beta7_LH",	"beta7_HL",
    "beta8_LL",	"beta8_HH",	"beta8_LH",	"beta8_HL",
    "died_latency",
    "DLsd",
    "died_rate.L","died_rate.H",
    "t0_morts")
    

init <- c(
          1.2*gamma, 1.2*gamma, 1.2*gamma, 1.2*gamma, 
          0.6*gamma, 0.6*gamma, 0.6*gamma, 0.6*gamma,
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma,          
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma, 
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma, 
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma,          
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma, 
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma,
          0.5*gamma, 0.5*gamma, 0.5*gamma,0.5*gamma,
          21,
          5, 
          0.01,0.01, 
          2)

df_params <- data.frame(name = fit.paramnames,
                        min = c(
                          0.3*gamma, 0.3*gamma, 0.3*gamma, 0.3*gamma, 
                          0.15*gamma, 0.15*gamma, 0.15*gamma, 0.15*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,          
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          0.1*gamma, 0.1*gamma, 0.1*gamma, 0.1*gamma,
                          16,
                          2, 
                          0.001,0.001, 
                          0),
                        max = c(
                          3*gamma, 3*gamma, 3*gamma, 3*gamma, 
                          1.5*gamma, 1.5*gamma, 1.5*gamma, 1.5*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,          
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                          1*gamma, 1*gamma, 1*gamma, 1*gamma,
                            24,
                          9, 
                          0.1, 0.1,
                          10),
                        init = init)
