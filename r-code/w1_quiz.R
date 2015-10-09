# Coursera Finance Specialisation (U Michigan)
# M1 -- Time Value of Money (TVM)
# Week 1 - Quiz 1
#
# Joe Nguyen | 29 Sep, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm"
setwd(paste0(dirBase, dirWorking))
rm(list = ls())

# Generic finance functions
source("func_finance.R")

## Question 9
# (15 points) Jessica is in the market for a new car. She has narrowed her search down to 2 models. Model A costs $27,000 and Model B costs $18,000. With both cars she plans to pay cash and own them for 3 years before trading in for a new car. Her research indicates that the trade in value for Model A after 3 years is 52% of the initial purchase price, while the trade in value for Model B is 33%. Jessica has no emotional attachment to either model and wants to make a strictly financial decision. The interest rate is 7%. For simplicity assume that operating and maintenance costs for the models are identical every year. Which model is the better decision and how much "cheaper" is it than the alternative?

a.cost <- 27000
b.cost <- 18000

# FV in 3 years (depreciated)
a.fv.3 <- a.cost * 0.52
b.fv.3 <- b.cost * 0.33

# PV cash if purchase b
cash <- a.cost - b.cost
r <- 0.07
n <- 3

# Cash interest
interest <- fv(cash, r, n)
b.return <- b.fv.3 + interest

# Determine a or b returns more cash in 3 years
returnVal <- a.fv.3 - b.return

if (returnVal > 0) {
    cat("a better, returns: $", returnVal, sep = "")
} else
    cat("b better, returns: $", -returnVal, sep = "")


## Question 10
# (15 points) College tuition has been rising at a rate of 5% per year. Currently the average tuition of a state college is $8,500 per year. Andrea's son Trevor will begin college in 9 years. Andrea's portfolio is making 8% annually. How much does Andrea need to have set aside today/now to pay for 4 years of college for Trevor? (Note: Tuition will continue to change annually and Andrea's portfolio balance will continue to accrue interest while Trevor is in school. Also, tuition is due at the beginning of each year.)

tuition.now <- 8500
tuition.r <- 0.05
tuition.n <- 4

portfolio.r <- 0.08

n1 <- 9
n2 <- n1 + tuition.n

# Compute tuition fees over 4 years AND compute corresponding PV for each year's fee
tuition.cost <- rep(0, tuition.n)
tuition.cost[1] <- fv(tuition.now, tuition.r, n1)

portofolio.pv <- rep(0, tuition.n)
portofolio.pv[1] <- pv(tuition.cost[1], portfolio.r, n1)

for (i in 2 : tuition.n) {
    tuition.cost[i] <- fv(tuition.cost[i-1], tuition.r, 1)
    portofolio.pv[i] <- pv(tuition.cost[i], portfolio.r, n1+i-1)
}
tuition.cost.total <- sum(tuition.cost)
tuition.cost.total

# Compute PV Andrea has to save now given her portfolio
portfolio.pv.total <- sum(portofolio.pv)
cat("Andrea has to save $", portfolio.pv.total, " in her portfolio now.", sep = "")

