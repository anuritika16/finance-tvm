# Coursera Finance Specialisation (U Michigan)
# Week 2 -- Lectures
#
# Joe Nguyen | 29 Sep, 2015

rm(list = ls())
graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))



## 2.2 FV of Annuity: Example 1
###############################
# What will be the value of your portfolio at retirement if you deposit $10,000 every year in a pension fund? You plan to retire in 40 years and expect to earn 8% on your portfolio.
c <- 10e3
r <- 0.04
n <- 40
fv <- annuity_fv(c, r, n); fv


## 2.3 FV of Annuity: Example 2
###############################
# Suppose you want to guarantee yourself $500,000 when you retire 25 years from now. How much must you invest each year, starting at the end of this year, if the interest rate is 8%?
fv <- 500000
r <- 0.08
n <- 25
c <- annuity_c(fv, 0, r, n); c


## 2.5 PV of Annuity: Examples
##############################
# How much money do you need in the bank today so that you can spend $10,000 every year for the next 25 years, starting at the end of this year. Suppose r = 5%.
c <- 10000
r <- 0.05
n <- 25
pv <- annuity_pv(c, r, n); pv

# You plan to attend a business school and you will be forced to take out $100,000 in a loan at 10% now. You want to figure out your future yearly payments, given that you will have 5 years to pay back the loan.
pv <- 100000
r <- 0.1
n <- 5
c <- annuity_c(0, pv, r, n); c

# Remaining loan
loan <- loan_rem(pv, c, r, 5)
loan$loanRem
loan$interest
loan$repayment

## 2.7 Compounding
##################
# Effective Annual Rate (EAR)
# EAR = (1 + r/k)^k - 1
monthsPerYear <- 12
r <- 0.08
k <- monthsPerYear
EAR <- (1 + r/k)^k - 1; EAR


## 2.8 Valuing Perpetuities
###########################
# A perpetuity is a set of equal payments (cash flows) that are paid forever, with or without growth.


## 2.9 Mega Example
###################
# Suppose you are exactly 30 years old. You believe you will be able to save for the next 20 years, until you are 50. For 10 years following that, and until your retirement at age 60, you will have a spike in your expenses due to your kids' college expenses, weddings, etc., and you will not be able to save. If you want to guarantee yourself $100,000 per year starting on your 61st birthday, how much should you save every year, for the next 20 years, starting at the end of this year? Assume your investments are expected to yield 8% and you are likely to live unitl 80.
#
# Find cash flow required for 20 years [30 to 50] to have $100,000 each year for 20 years [60 to 80].

# Determine cash flow during [30 to 50] to have fv = 100,000 during each year in [60 to 80]
# Formula: fv = temp1 + temp2
# temp1 = c * (interest from 30 to 50)
# temp2 = FV(temp1)
fv <- 100e3
r <- 0.08

year.save0 <- 30
year.save1 <- 50
year.spend0 <- 61
year.spend1 <- 80
year.save.n <- year.save1 - year.save0

# Compute pv (total) at year 50 if fv = 100,000 in each year [61 to 80]
pv <- 0
for (i in (year.spend0 : year.spend1) - year.spend0) {
    # Zero cash flow pv at year 50
    n <- year.spend0 - year.save1 + i
    pv <- pv + PV(fv, r, n)
}

# #####################################
# ## Alternative solution from lecturer
# # PV at age 60
# pv <- annuity_pv(fv, r, year.spend1 - year.spend0 + 1)
# 
# # PV at age 50
# pv <- PV(pv, r, year.spend0 - year.save1-1)
# #####################################

# Annuity cash flow in [30 to 50] to achieve pv
c <- annuity_c(pv, 0, r, year.save.n); c


## 2.10 Recap Module 2
######################
# Suppose you are exactly 30 years old. You believe you will be able to save for the next 20 years, until you are 50. For 10 years following that, and until your retirement at age 60, you will have a spike in your expenses due to your kids' college expenses, weddings, etc., and you will not be able to save.
#
# DIFFERENCE:
# If you want to guarantee yourself $8,000 PER MONTH starting on your 61st birthday, how much should you save every MONTH, for the next 20 years, starting at the end of this year? Assume your investments are expected to yield 8% and you are likely to live unitl 80.
fv <- 8000
r <- r / monthsPerYear
n <- (year.spend1 - year.spend0 + 1) * monthsPerYear

# PV at age 60
pv <- annuity_pv(fv, r, n)

# PV at age 50
n <- (year.spend0 - year.save1-1) * monthsPerYear
pv <- PV(pv, r, n)

# Annuity cash flow in [30 to 50] to achieve pv
c <- annuity_c(pv, 0, r, year.save.n * monthsPerYear); c

