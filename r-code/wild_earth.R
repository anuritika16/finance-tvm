# Wild Earth Camping Finance
# Joe Nguyen | 29 Sep, 2015

rm(list = ls())

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))

## Marmot Tungsten 3P Hiking Tent
# Make 4 payments (fornightly) of $99.88 over 8 weeks, or pay outright $367.54
n <- 4
r <- 0.028 / fortsPerYear
c <- 99.88

pv <- annuity_pv(c, r, n); pv
