# Coursera Finance Specialisation (U Michigan)
# Week 2 -- Quiz
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


## Question 1
# (5 points) Carlos goes to the bank to take out a personal loan. The stated annual interest rate is 10%, but interest is compounded quarterly and he will make monthly payments. What is the EAR?
r <- 0.1
quartPerYear <- 4
EAR(r, quartPerYear)


## Question 2
# (5 points) Gloria is 35 and trying to plan for retirement. She has put a budget together and plans to save $5,400 per year, starting at the end of this year, in a retirement fund until she is 58. Assume that she can make 5.0% on her account. How much will she have for retirement at age 58?
year.0 <- 35
year.1 <- 58
year.n <- year.1 - year.0
c <- 5400
r <- 0.05
annuity_fv(c, r, year.n)


## Question 3
# (5 points) Dominique has just turned 62 and she has deposited her annual payment of $15,000 into her retirement account. She made her first such saving deposit into this fund on her 34th birthday. Dominique has also retired and wants to figure out how much money she has in her retirement account for her retired life. You are Dominique's friend who knows finance. How much is Dominique's savings worth today given that the fund has earned an annual return of 3.0%?
year.0 <- 34
year.1 <- 62
year.n <- year.1 - year.0 + 1
c <- 15e3
r <- 0.03
annuity_fv(c, r, year.n)


## Question 4
#(5 points) Gerard has estimated that he is going to need enough in his retirement fund to withdraw $85,000 per year beginning on his 56th birthday and for 29 additional years thereafter. How much will Gerard need in his retirement account at age 55 if his fund is expected to earn an annual return of 7.5%?
c <- 85e3
n <- 30
r <- 0.075
annuity_pv(c, r, n)


## Question 5
# (10 points) Huiling owns a rental property on Main street, but she is considering selling the property to another real estate investor. In preparation for negotiating a price, Huiling wants to know the value of the property. The Net Operating Income (NOI) is the cash flow from real estate and the Cap Rate is the rate, where NOI is rental revenue less all expenses except loan servicing. The property has an NOI of $20,000 per year. The local real estate market has a cap rate of 6%. What is a fair price for the property assuming that the building's remaining life is 30 years?
noi <- 20e3
r <- 0.06
n <- 30
annuity_pv(noi, r, n)


## Question 6
#(10 points) Melanie and Stephen Jackson are purchasing their first house. The house costs $440,000. They have put a 10% down payment (that is, an amount that banks should require you to pay out-of-pocket), but will therefore finance the rest. They are considering a fixed rate 30-year mortgage at a 7.00% APR with monthly payments. How much will the Jacksons' first monthly payment be?
#
# Annual Percentage Rate (APR): cost per year of borrowing.
# For a loan, APR includes: interest rate, closing costs, origination fees, insurance costs
n <- 30
apr <- 0.07
monthsPerYear <- 12
pv <- 440e3 * 0.9
c <- annuity_c(0, pv, apr/monthsPerYear, n*monthsPerYear); c


## Question 7
# (15 points) Abebi, who has just celebrated her 31st birthday, will retire on her 60th birthday, and she has just set up a retirement plan to pay her income starting on her retirement day, and to continue paying for 19 more years. Abebi's goal is to receive $110,000 for each of these twenty years. In creating her retirement account, Abebi has committed to set aside equal investments at the end of each year, for the next 28 years starting on her 32nd birthday. If the annual interest rate is 7%, how big should Abebi's equal investments be?
r <- 0.07
cfv <- 110e3
n <- 20

# Value at 60yo to pay cfv for 19yrs afterwards
fv <- annuity_pv(cfv, r, n)

# Cash flow to achieve target value at 60yo
n <- 28
c <- annuity_c(fv, 0, r, n); c


## Question 8
# (15 points) Two years ago Abilia purchased a $13,000 car; she paid $2,500 down and borrowed the rest. She took a fixed rate 60-month installment loan at a stated rate of 7.0% per year. Interest rates have fallen during the last two years and she can refinance her car by borrowing the amount she still owes on the car at a new fixed rate of 4% per year for 3 years. Should Abilia refinance her loan? How much will she save per month for the remainder of the loan life if she decides to refinance?
pv <- 13e3 - 2500
r <- 0.07/monthsPerYear
n <- 5 * monthsPerYear
c <- annuity_c(0, pv, r, n)

# Remaining pv after monthly payments for 2 years
mPay <- 2 * monthsPerYear
pv <- loan_rem(pv, c, r, mPay)$loanRem

# Refinance
rRefin <- 0.04 / monthsPerYear
n <- 3 * monthsPerYear
cRefin <- annuity_c(0, pv, rRefin, n)
c - cRefin

interest <- loan_rem(pv, c, r, 1)$interest
interestRefin <- loan_rem(pv, cRefin, rRefin, 1)$interest
interest - interestRefin


## Question 9
# (15 points) You have been living in the house you bought 9 years ago for $400,000. At that time, you took out a loan for 80% of the house at a fixed rate 20-year loan at an annual stated rate of 7.5%. You have just paid off the 108th monthly payment. Interest rates have meanwhile dropped steadily to 4.5% per year, and you think it is finally time to refinance the remaining balance over the residual loan life. But there is a catch. The fee to refinance your loan is $4,000. Should you refinance the remaining balance? How much would you save/lose if you decided to refinance?
pv <- 400e3 * 0.8
r <- 0.075 / monthsPerYear
n <- 20 * monthsPerYear
mPay <- 108
refinCost <- 4000
rRefin <- 0.045 / monthsPerYear

# Cash flow to pay off loan (pv)
c <- annuity_c(0, pv, r, n)

# Pay off 108th monthly payment
pv <- loan_rem(pv, c, r, mPay)$loanRem

# Refinance
n <- n - mPay
cRefin <- annuity_c(0, pv, rRefin, n)
annuity_pv(cRefin, rRefin, n)

# Cash flows difference
cDiff <- c - cRefin

# PV of cash flows difference over n remaining months (in future)
cDiffPv <- annuity_pv(cDiff, rRefin, n)

# Money saved from refinance
cDiffPv - refinCost


## Question 10
# (15 points) You are interested in a new Ford Taurus. After visiting your Ford dealer, doing your research on the best leases available, you have three options. (i) Purchase the car for cash and receive a $1,600 cash rebate from Dealer A. The price of the car is $16,000. (ii) Lease the car from Dealer B. Under this option, you pay the dealer $450 now and $200 a month for each of the next 36 months (the first $200 payment occurs 1 month from today). After 36 months you may buy the car for $8,900. (iii) Purchase the car from Dealer C who will lend you the entire purchase price of the car for a zero interest 36-month loan with monthly payments. The car price is $16,000. Suppose the market interest rate is 5%. What is the net cost today of the cheapest option?

## Parameters
#############
carPrice <- 16e3
n <- 36
r <- 0.05 / monthsPerYear

## Option a
rebate <- 1600

## Option b
b.payNow <- 450
b.c <- 200
b.lump <- 8900

## Option c
c.pv <- carPrice
c.r <- 0


## Calculation
##############
## Option a
a.cost <- carPrice - rebate

## Option b
# Determine pv required now for future cash flows
b.pvC <- annuity_pv(b.c, r, n)

# Determine pv required now for $8900 lump value in 36 months time
b.pvLv <- PV(b.lump, r, n)
b.cost <- b.payNow + b.pvC + b.pvLv

## Option c
c.c <- annuity_c(0, c.pv, c.r, n)

# Determine pv required now for future cash flows
c.cost <- annuity_pv(c.c, r, n)


## Best option
cat("Dealer A: $", a.cost, " | ",
    "Dealer B: $", b.cost, " | ",
    "Dealer C: $", c.cost, sep = "")

cost <- c(a.cost, b.cost, c.cost)
cat("Option ", which.min(cost), " is best at $", min(cost), sep = "")



