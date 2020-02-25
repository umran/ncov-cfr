source("evaluation/simulate.R")

# estimated gamma distribution of delay between symptom onset and death
linton_dist <- estimateGamma("data/linton_supp_tableS1_S2_8Feb2020.csv")

# different periods (in days) being simulated
periods <- c(32, 64, 128, 256)
# expected CFR
expectedCFR <- 0.02

# simulations of slow and non-exponential epidemic growth over varying periods
for (period in periods) {
  results <- sim(linton_dist, expectedCFR, period, minDailyCaseCount=0, maxDailyCaseCount=4)
  print(sprintf("showing estimates for a slow, non-exponentially growing epidemic over %i days\n", period))
  print(results)
  saveRDS(results, sprintf("evaluation/out/slow_%i.rds", period))
}

# simulations of fast and non-exponential epidemic growth over varying periods
for (period in periods) {
  results <- sim(linton_dist, expectedCFR, period, minDailyCaseCount=0, maxDailyCaseCount=100)
  print(sprintf("showing estimates for a slow, non-exponentially growing epidemic over %i days\n", period))
  print(results)
  saveRDS(results, sprintf("evaluation/out/fast_%i.rds", period))
}