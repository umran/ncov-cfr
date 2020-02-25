source("evaluation/loadfun.R")

# Load required libraries
loadFun(lubridate)
loadFun(bbmle)
loadFun(fitdistrplus)

estimateGamma <- function(dataset) {
  ds <- read.csv(dataset)
  ds <- dmy(ds$Death) - dmy(ds$Onset)
  ds <- as.numeric(na.omit(ds))
  fitg <- fitdist(ds, "gamma")
  
  return(fitg)
}

estimateCFR <- function(delayDistribution, cases, deaths) {
  # Likelihood and expected mortality function
  nll <- function(cfr, death_shape, death_rate) {
    cfr <- plogis(cfr)
    expected <- numeric(length(days))
    for(i in days) {
      for(j in 1:sum(cases)) {
        d <- i - onset[j]
        if(d >= 0) {
          expected[i] <- expected[i] + cfr*diff(pgamma(c(d - 0.5, d + 0.5), shape = death_shape, rate = death_rate))
        }
      }
    }
    ll <- sum(dpois(deaths, expected, log = TRUE))
    return(-ll)
  }
  
  # prepare variables
  period <- length(cases)
  days <- 1:period
  onset <- rep(days, cases)
  
  # fit the model
  free <- c(cfr = 0)
  fixed <- c(death_shape = coef(delayDistribution)[[1]], death_rate = coef(delayDistribution)[[2]])
  fit <- mle2(nll, start = as.list(free),
              fixed = as.list(fixed), method = "Brent", lower = -100, upper = 100)
  
  # return estimates
  results <- c()
  results[1] <- plogis(coef(fit)[1])
  results[2:3] <- plogis(confint(fit))
  
  return(results)
}