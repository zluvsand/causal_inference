# PREREQUISITE: Make sure the set working directory to the
# directory where this file is saved with the following step:
# Session => Set Working Directory -> To Source File Location

library("MatchIt")
library(readr)

data(lalonde)
write_csv(lalonde, '../data/data.csv')