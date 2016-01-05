# Real-Life Finance Calculator
#
# Joe Nguyen | 05 Jan, 2016

rm(list = ls())
# graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))


## NRMA insurance
# CBA goal saver rate (05 Jan, 2016)
r <- 0.27 / monthsPerYear
n <- 1 * monthsPerYear

# Annual payment
p <- 1497.55

# Monthly payment
pm <- 134.5

# Put annual payment amount in savings account and withdraw "pm" to make monthly payments
fv <- loan_rem(p, pm, r, n); fv
#^ fv$loanRem is cr (if +ve) or dr (if -ve) by choosing to pay monthly payments instead of annual payment

