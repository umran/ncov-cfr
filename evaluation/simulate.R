source("evaluation/estimate.R")

# [sim] receives the following arguments:
# 1. [delayDistribution]: the distribution of the delay between onset of symptoms and death from known fatal cases.
#    the estimator we are evaluating assumes a gamma distribution so it is expected to be a gamma distribution.
# 2. [expectedCFR]: the expected CFR the estimator should converge to if it is consistent
# 3. [period]: a finite period of days over which CFR is to be estimated
# 4. [meanDailyCaseCount]: the mean number of new cases per day
# 5. [sdDailyCaseCount]: the standard deviation of the number of new cases per day
# 6. [doublingTime]: the number of days it takes for mean daily case counts to double
# 7. [iterations]: the number of iterations over which CFR will be estimated from cases randomly sampled
#    from the underlying distribution
sim <- function(delayDistribution, expectedCFR, period, meanDailyCaseCount=2, sdDailyCaseCount=2.1,
                doublingTime=0, iterations=1) {
  # [estimates] is a table used to collate estimates generated from all iterations.
  # for each iteration we record:
  # 1. the estimated CFR,
  # 2. the estimated lower bound of the estimated CFR
  # 3. the estimated upper bound of the estimated CFR
  # 4. the actual CFR of all simulated cases
  # 5. the naive CFR estimated by taking the total deaths realized over the period: [period] divided by
  #    the total number of cases over the period: [period]
  # 6. the total case count
  # 7. the total death count observed within the period: [period]
  estimates <- as.data.frame(matrix(data=NA, nrow=iterations, ncol=7))
  names(estimates) <- c("estimated_cfr", "lower", "upper", "actual_cfr",
                        "naive_cfr", "observed_cases", "observed_deaths")
  
  # we simulate random samples from the underlying distributions (with predetermined parameters)
  # and generate estimates for the CFR, among other things (see above) on each iteration
  for (k in 1:iterations) {
    # create a vector to hold daily case counts
    dailyCaseCounts <- c()
    
    if (doublingTime > 0) {
      # if doublingTime is greater than zero, for each doubling period we double the mean before sampling
      # the daily case count from a normal distribution
      for (i in 1:period) {
        factor <- 2^(floor((i-1)/doublingTime))
        dailyCaseCounts[i] = abs(round(rnorm(n=1, mean=factor*meanDailyCaseCount, sd=sqrt(factor)*sdDailyCaseCount)))[1]
      }
    } else {
      # if doublingTime is zero, sample case counts for a total of [period] days from a normal distribution
      # with constant mean = [meanDailyCaseCount] and sd = [sdDailyCaseCount]
      dailyCaseCounts = abs(round(rnorm(n=period, mean=meanDailyCaseCount, sd=sdDailyCaseCount)))
    }
    
    # tally the total cases reported over [period] days
    totalCaseCount <- sum(dailyCaseCounts)
    
    # [cases] is an intermediate table of all sampled cases that encodes:
    # 1. the day of symptom onset,
    # 2. a boolean to indicate whether death occurred in the sampled case, and
    # 3. the delay (in days) between symptom onset and death (if the case was sampled as fatal)
    cases <- as.data.frame(matrix(data=NA, nrow=totalCaseCount, ncol=3))
    names(cases) <- c("onset", "fatal", "delay")
    
    # the following code computes the day of onset for each sampled case
    
    # we first create a variable: [caseIndex], to keep track of the current case being iterated on
    # (note to self: R vectors are not zero-indexed)
    caseIndex <- 1
    
    # iterate over each day
    for (i in 1:period) {
      # iterate over the current day's cases
      for (j in 1:dailyCaseCounts[i]) {
        # set symptom onset of the current case (having index [caseIndex]) to the current day ([i]),
        # i.e. the day the current case was reported
        cases[caseIndex, "onset"] = i
        
        # increment [caseIndex] by one, allowing us to select and update the next case on the next iteration
        # (wish I could do `caseIndex += 1`)
        caseIndex = caseIndex + 1
      }
    }
    
    # assign an outcome of fatal or not to each case, by sampling from a binomial distribution
    # with predetermined p, where p is the expected probability of death occurring in any given case.
    # p in this case is [expectedCFR]
    assignedFatality <- rbinom(n=totalCaseCount, size=1, prob=expectedCFR)
    # iterate over sampled cases
    for (i in 1:totalCaseCount) {
      # assign the appropriate sampled fatality to the current case (indexed by [i]) 
      if (assignedFatality[i] == 1) {
        cases[i, "fatal"] = TRUE
        
        # if the outcome of the current case turns out to be fatal, we also assign to it a delay
        # sampled randomly from the gamma distribution whose perimiters were estimated earlier
        # (from data on known fatal cases)
        cases[i, "delay"] = round(rgamma(n=1, shape=coef(delayDistribution)[[1]], 
                                            rate=coef(delayDistribution)[[2]]))[1]
      } else {
        cases[i, "fatal"] = FALSE
      }
    }
    
    # tally the total simulated death count
    totalSimulatedDeathCount <- sum(assignedFatality)
    
    # tally daily deaths within period [period]
    dailyDeathCounts <- rep(0, period)
    # we iterate over all the sampled cases
    for (i in 1:totalCaseCount) {
      if (cases[i, "fatal"] == TRUE) {
        # if the current case (having index [i]) is a fatal case, we calculate
        # the day on which death is expected to occur via the following formula:
        # (day of death) = (day of symptom onset) + (delay (in days) between symptom onset and death)
        dayOfDeath <- cases[i, "onset"] + cases[i, "delay"]
        # finally, we apply a filter whereby we preclude fatal cases from the death toll if it is
        # expected to occur outside the period: [period] for which the CFR is being estimated
        if (dayOfDeath <= period) {
          dailyDeathCounts[dayOfDeath] = dailyDeathCounts[dayOfDeath] + 1
        }
      }
    }
    
    # tally the total observed death count over the period: [period]
    totalObservedDeathCount <- sum(dailyDeathCounts)
    
    # estimate cfr
    estimates[k, 1:3] = estimateCFR(delayDistribution, dailyCaseCounts, dailyDeathCounts)
    
    # record actual simulated cfr
    estimates[k, 4] = totalSimulatedDeathCount/totalCaseCount
    # record naive cfr
    estimates[k, 5] = totalObservedDeathCount/totalCaseCount
    # record observed cases
    estimates[k, 6] = totalCaseCount
    # record observed deaths
    estimates[k, 7] = totalObservedDeathCount
  }
  
  return(estimates)
}