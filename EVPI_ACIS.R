
library(decisionSupport)
# Call the Monte Carlo simulation results from the input table
mc_result<-read.csv("mcSimulationResults.csv",header = TRUE,sep=",")
str(mc_result)

mc_acis<-mc_result[,c(2:139,140:145)]

results_all <- multi_EVPI(mc_acis,"output_1",write_table = TRUE)

plot(results_all, "output_1")
plot(results_all, "output_2")
plot(results_all, "output_3")
plot(results_all, "output_4")
plot(results_all, "output_5")
plot(results_all, "output_6")



