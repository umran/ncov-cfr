# for a better idea of what's happening behind the scenes during simulation
# read the source for the sim function here: [https://github.com/umran/ncov-cfr/blob/master/evaluation/simulate.R]
source("evaluation/simulate.R")

# estimated gamma distribution of delay between symptom onset and death
linton_dist <- estimateGamma("data/linton_supp_tableS1_S2_8Feb2020.csv")

# different periods (in days) being simulated
periods <- c(7, 14, 30)
# expected CFR
expectedCFR <- 0.02

# simulations of slow and non-exponential epidemic growth over varying periods
for (period in periods) {
  results <- sim(linton_dist, expectedCFR, period, meanDailyCaseCount=5, sdDailyCaseCount=5, iterations=1)
  print(sprintf("showing estimates for a slow, non-exponentially growing epidemic over %i days", period))
  print(results)
  print(sprintf("showing mean estimates for a total of %i simulations", nrow(results)))
  print(colMeans(results))
  # saveRDS(results, sprintf("evaluation/out/slow_%i.rds", period))
}

# simulations of fast and non-exponential epidemic growth over varying periods
for (period in periods) {
  results <- sim(linton_dist, expectedCFR, period, meanDailyCaseCount=50, sdDailyCaseCount=25, iterations=1)
  print(sprintf("showing estimates for a fast, non-exponentially growing epidemic over %i days", period))
  print(results)
  print(sprintf("showing mean estimates for a total of %i simulations", nrow(results)))
  print(colMeans(results))
  # saveRDS(results, sprintf("evaluation/out/fast_%i.rds", period))
}

# simulations of exponential epidemic growth over varying periods
for (period in periods) {
  results <- sim(linton_dist, expectedCFR, period, meanDailyCaseCount=10, sdDailyCaseCount=10, doublingTime=6, iterations=1)
  print(sprintf("showing estimates for an exponentially growing epidemic over %i days", period))
  print(results)
  print(sprintf("showing mean estimates for a total of %i simulation(s)", nrow(results)))
  print(colMeans(results))
  # saveRDS(results, sprintf("evaluation/out/fast_%i.rds", period))
}