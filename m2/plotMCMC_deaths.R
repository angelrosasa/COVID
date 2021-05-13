###############
## Default values for fitting data. You may want to override that in your script.
###############

##
## How to estimate dispersion parameter (size):
## Fit a good model (e.g. from an MCMC batch with low acceptance rate)
##  m = mu, s = data
##  find size so that mean((m - s)^2 / (m + m^2/size)) = 1
##  take size a bit smaller to be safe (because ML fit minimizes variance)
##
## smaller size ("dispersion parameter"), larger variation
## r -> inf : poisson
## r -> 1 : var ~ mu^2
##
hosp_nbinom_size = 30
mort_nbinom_size = 90

####################
## Fit library functions
####################

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

## Global variable that stores calculated model data
state <- NULL

## Iteration counter to log periodically during log likelihood calculations
it <- 0

##
## Graphs used to monitor the MCMC runs
##
graphs <- function() {
    ## only if a device is open already
    if (.Device == "null device") {
        return(0)
    }
   
    par(mfrow=c(1,2))

    days <- seq(datastart_date, datastart_date + length(dmort) + 30, 1)

    period <- length(state$deadi)
    len <- period - state$offset + 1

    if (state$offset < 1 | len < 2) {
        plot(days[1:5], state$deadi[1:5], type='l', col='red',
             xlab='Date', ylab='Count',
             main='fail')
        plot(days[1:5], state$deadi[1:5], type='l', col='red',
             xlab='Date', ylab='Count',
             main='fail')
        return (0)
    }

    plot(days[1:len], state$died[state$offset:(state$offset + len - 1)], type='l', col='red',
         xlab='Date', ylab='Cumulative count',
         main='Cumulative deaths', log="y")
        points(days[1:length(dmort)],dmort,col='blue')
        legend("topleft", inset=0.02, legend=c("Predicted", "Observed"),
           col=c("red", "blue"),lty=c(1,3))
    
    plot(days[1:len],state$deadi[state$offset:period], type='l', col='red',
         xlab='Date', ylab='Count',
         main='New deaths per day', log="y")
        points(days[1:length(dmorti)],dmorti,col="blue")
        legend("topleft", inset=0.02, legend=c("Predicted", "Observed"),
               col=c("red", "blue"),lty=c(1,3))

}     
        