# Coursera Finance Specialisation (U Michigan)
# M1 -- Time Value of Money (TVM)
# Week 6 - Final 1
#
# Joe Nguyen | 23 Oct, 2015

rm(list = ls())

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))


## Question 1
# (10 points) Jessica is in the market for a new car. She has narrowed her search down to 2 models. Model A costs $29,000 and Model B costs $17,000. With both cars she plans to pay cash and own them for 4 years before trading in for a new car. Her research indicates that the trade in value for Model A after 4 years is 55% of the initial purchase price, while the trade in value for Model B is 42%. Jessica has no emotional attachment to either model and wants to make a strictly financial decision. The interest rate is 4%. For simplicity assume that operating and maintenance costs for the models are identical every year. Which model is the better decision and how much "cheaper" is it than the alternative?
a.cost <- 29e3
a.tradeVal <- 0.55 * a.cost

b.cost <- 17e3
b.tradeVal <- 0.42 * b.cost

r <- 0.04
n <- 4

a.npv <- PV(a.tradeVal, r, n) - a.cost
b.npv <- PV(b.tradeVal, r, n) - b.cost

a.npv - b.npv


## Question 2
# (10 points) College tuition has been rising at a rate of 3% per year. Currently the average tuition of a state college is $8,700 per year. Andrea's son Trevor will begin college in 14 years. Andrea's portfolio is making 7% annually. How much does Andrea need to have set aside today/now to pay for 4 years of college for Trevor? (Note: Tuition will continue to change annually and Andrea's portfolio balance will continue to accrue interest while Trevor is in school. Also, tuition is due at the beginning of each year.)
college.r <- 0.03
college.cost0 <- 8700

college.y0 <- 14
college.n <- 4

portfolio.r <- 0.07

# Starting college cost
college.cost0 <- FV(college.cost0, college.r, college.y0); college.cost0

# Total college cost PV at college year 0 (payments at start of year)
college.pv <- college.cost0 + annuity_pv(college.cost0 * (1+college.r), college.r, college.n-1); college.pv

# PV required by Andrea now
PV(college.pv, portfolio.r, college.y0)


## Question 3
# (10 points) You have been living in the house you bought 7 years ago for $400,000. At that time, you took out a loan for 80% of the house at a fixed rate 15-year loan at an annual stated rate of 8.0%. You have just paid off the 84th monthly payment. Interest rates have meanwhile dropped steadily to 5.0% per year, and you think it is finally time to refinance the remaining balance over the residual loan life. But there is a catch. The fee to refinance your loan is $5,500. Should you refinance the remaining balance? How much would you save/lose if you decided to refinance?
cost <- 400e3
loan <- 0.8 * cost
r <- 0.08 / monthsPerYear
n <- 15 * monthsPerYear
t <- 84
m <- n - t
rf.r <- 0.05 / monthsPerYear
rf.fee <- 5500

# Annuity payment (initial)
c <- annuity_c(0, loan, r, n)

# Remaining loan
loanRem <- loan_rem(loan, c, r, t)$loanRem; loanRem

# Refinance
rf.c <- annuity_c(0, loanRem, rf.r, m);

# Payment difference
cDiff <- c - rf.c

# PV total saved over m periods
annuity_pv(cDiff, rf.r, m) - rf.fee


## Question 4
# (10 points) You are interested in a new Ford Taurus. After visiting your Ford dealer, doing your research on the best leases available, you have three options. (i) Purchase the car for cash and receive a $1,500 cash rebate from Dealer A. The price of the car is $15,000. (ii) Lease the car from Dealer B. Under this option, you pay the dealer $450 now and $175 a month for each of the next 36 months (the first $175 payment occurs 1 month from today). After 36 months you may buy the car for $8,700. (iii) Purchase the car from Dealer C who will lend you the entire purchase price of the car for a zero interest 36-month loan with monthly payments. The car price is $15,000. Suppose the market interest rate is 4%. What is the net cost today of the cheapest option?
carCost <- 15e3
r <- 0.04 / monthsPerYear
n <- 36

a.cost <- carCost - 1500

b.outlay <- 450
b.c <- 175
b.final <- 8700
b.cost <- b.outlay + annuity_pv(b.c, r, n) + PV(b.final, r, n)

c.c <- carCost / n
c.cost <- annuity_pv(c.c, r, n)

cat(a.cost, b.cost, c.cost)


## Question 5
# (10 points) Rafael owned an apartment building that burned down. The empty lot is worth $50,000 and Rafael has received $290,000 from the insurance company. Rafael plans to build another apartment building that will cost $415,000. His real estate adviser estimates that the expected value of the finished building on the real estate market will be $530,000 next year. The discount/interest rate is 7%. What are the NPV and IRR of this decision?
cost <- 50e3 + 415e3
c <- 530e3
r <- 0.07
NPV(cost, c, r)
IRR(cost, c)


## Question 6
# (15 points) Roxanne invested $230,000 in a new business 9 years ago. The business was expected to bring in $2,000 each month for the next 18 years (in excess of all costs). The annual cost of capital (or interest rate) for this type of business was 9% with monthly compounding. What is the value of the business today?
n <- (18-9) * monthsPerYear
cVec <- rep(2e3, n)
r <- 0.09 / monthsPerYear
NPV(0, cVec, r)


## Question 7
# (15 points) Walmart is considering opening a small experimental store in New York City. A store is expected to have a long economic life, but the valuation horizon is 14 years. The store in New York is likely to generate revenues of $32M in the first year and then it grows at 5.0%. But the costs of running the business are high because the margins on all the products sold are low. (It is a volume business!) The cost of goods sold is $10M in year 1 and it is expected to grow at 3.0% per year thereafter. Selling and administration costs are likely to be $1.4M every year as it is a small store. The tax rate is 35%. Walmart is so good at managing its stores that working capital increases can be assumed to be negligible. But since New York City is an expensive place, Walmart will have to invest $200M in purchasing a building (with land) even though it is a much smaller property than a usual Walmart store. The good news is that this outlay can be straight line depreciated over 14 years. Also, Walmart has estimated that the after-tax terminal value in year 14 dollars is $125M. This value is the present value of all cash flows in year 15 and beyond. What is the NPV of opening this new store if the appropriate discount rate is 4.5%? (Again, all cash flows except initial investments happen at the end of the year. You are strongly encouraged to use a spreadsheet.)
n <- 14
r <- 0.045

rev.y1 <- 32e6
rev.g <- 0.05
cog.y1 <- 10e6
cog.g <- 0.03
sac <- 1.4e6
tax <- 0.35
capex <- 200e6
deprec <- capex / n
termVal <- 125e6

# Periodic cash flows (in period $ values)
cVec <- rep(0,n)

for (i in 1:n) {
    # Operating profit
    op <- FV(rev.y1, rev.g, i-1) - FV(cog.y1, cog.g, i-1) - sac - deprec
    
    # Net operating profit after tax
    nopat <- (1 - tax) * op
    
    # Cash flow
    cVec[i] <- nopat + deprec
}

# NPV
NPV(capex, cVec, r) + PV(termVal, r, n)


## Question 8
# (20 points) Springfield Ironworks (SI) recently had their furnace break down and they need to quickly purchase a new one to minimize the disruption in their production. They can either choose a high quality furnace (H) that costs $100,000 with $2,500 of annual maintenance costs for the 6-year life of the furnace, or a low quality furnace (L) that costs $55,000 with $6,500 in annual maintenance costs for the 3-year life of the furnace. Which furnace should SI choose? What is the annualized cost of their choice? Assume a discount rate of 4.5%, and ignore all taxes.
h.capex <- 100e3
h.mtCost <- 2500
h.n <- 6

l.capex <- 55e3
l.mtCost <- 6500
l.n <- 3

r <- 0.045

h.npv <- NPV(h.capex, rep(-h.mtCost, h.n), r)
annuity_c(0, h.npv, r, h.n)

l.npv <- NPV(l.capex, rep(-l.mtCost, l.n), r)
annuity_c(0, l.npv, r, l.n)






