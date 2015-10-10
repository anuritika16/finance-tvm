# Coursera Finance Specialisation (U Michigan)
# Week 4 -- Decision Making and Cash Flows
#
# Joe Nguyen | 05 Oct, 2015

rm(list = ls())
graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))


## 4.5 Cash Flows: Important Principles II
##########################################

a.cost <- 20
a.cf <- c(-2, -2)

b.cost <- 25
b.cf <- c(-1, -1, -1)

r <- 0.05

a.pv <- NPV(a.cost, a.cf, r); a.pv
b.pv <- NPV(b.cost, b.cf, r); b.pv

# Annuity payment (per year)
a.c <- annuity_c(0, a.pv, r, length(a.cf)); a.c
b.c <- annuity_c(0, b.pv, r, length(b.cf)); b.c




