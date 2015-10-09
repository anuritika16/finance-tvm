# Coursera Finance Specialisation (U Michigan)
# Week 3 -- Decision Making
# Quiz - Attempt 2
#
# Joe Nguyen | 05 Oct, 2015

rm(list = ls())
graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, "/func_finance.R"))


## Question 1
# (5 points) Sachin has asked his flat mate Jason for a $450 loan to cover a portion of his rent and utility costs. Sachin proposes repaying the loan with $375 from each of his next two financial aid disbursements, the first 4 months from now and the second 13 months from now. Jason's alternative is to earn 6% annually in his money market account. Assume there is no risk of default, and that compounding is monthly. What is the NPV of the loan from Jason's perspective?
#
# Option 1 - repayment from Sachin
cost <- 450
r <- 0.06 / monthsPerYear
pv1 <- NPV(cost, rep(375, 2), r, c(4,13)); pv1

# Option 2 - interest from money market account
pv2 <- -cost + FV(450, r, 13); pv2


## Question 2
# (5 points) Juanita has an opportunity to invest in her friend's clothing store. The initial investment is $10,200 and the expected annual cashflows thereafter are as follows: {$300; $700; $900; $2,500; $2,500; $5,000; $5,000}. What is Juanita's IRR on this investment?
irr <- IRR(10200, c(300, 700, 900, 2500, 2500, 5000, 5000))
round(irr*100, 2)


## Question 3
# (5 points) Fabrice is looking to buy a new plug-in hybrid vehicle. The purchase price is $13,000 more than a similar conventional model. However, he will receive a $5,800 federal tax credit that he will realize at the end of the year. He estimates that he will save $1,300 per year in gas over the conventional model; these cash outflows can be assumed to occur at the end of the year. The cost of capital (or interest rate) for Fabrice is 5%. How long will Fabrice have to own the vehicle to justify the additional expense over the conventional model? In other words, what is the DISCOUNTED payback period in years? Discount future cash flows before calculating payback rounded UP to a whole year.
#
# Determine n (number of years) such that NPV = 0
cost <- 13e3
credit <- 5800
gas <- 1300
r <- 0.05

p <- list(cost, credit, gas, r)
obj <- function(n, p) {
    
    npv <- -p[[1]] + PV(p[[2]] + p[[3]], p[[4]], 1)
    
    for (i in 2:n) {
        npv <- npv + PV(p[[3]], p[[4]], i)
    }
    # Objective such that NPV = 0
    npv^2
}
optPar <- optimise(obj, interval = c(0, 100), p = p)
optPar$minimum


## Question 4
# (10 points) In high school Jeff often made money in the summer by mowing lawns in the neighborhood. He just finished his freshman year of college and, after taking a Business 101 class, he has some ideas about how to scale up his lawn mowing operation. Previously, he had used his father's push mower, but he is thinking about getting a riding mower that will save time and allow him to do more lawns. He found a used, zero turn, riding mower on Craigslist for $1,400. He will also need a trailer to pull the mower behind his pickup; that will cost him an additional $700. With the new mower he can take on an additional 15 lawns per week at an average cash inflow of $22 per lawn he will receive at the end of each week. He has 15 weeks of summer in which to mow lawns. (For convenience, assume that the mower and trailer will have no value after Jeff is done with his work this summer.) The discount rate for Jeff is 6% (note that this is an annual rate). What is the Net Present Value of the mower/trailer project?
mower <- 1400
trailer <- 700
cost <- mower + trailer

cLawn <- 22
lawnPerWeek <- 15
c <- cLawn * lawnPerWeek

nWeek <- 15
r <- 0.06 / weeksPerYear

NPV(cost, rep(c, nWeek), r)


## Question 5
# (10 points) Da Feng is looking to refinance his home because rates have gone down since he purchased the house 5 years ago. He started with a 30-year fixed-rate mortgage of $238,000 at an annual rate of 6.30%. He has to make monthly payments. He can now get a 25-year fixed-rate mortgage at an annual rate of 4.10% on the remaining balance of his initial mortgage. This loan also requires monthly payments. In order to re-finance, Da Feng will need to pay closing costs of $4,400. These costs are out of pocket and cannot be rolled into the new mortgage. How much will refinancing save Da Feng? (i.e. What is the NPV of the refinancing decision?)
cost <- 238e3
n <- 30 * monthsPerYear
r <- 0.063 / monthsPerYear
c <- annuity_c(0, cost, r, n)

# Remaining loan
m <- 5 * monthsPerYear
cost <- loan_rem(cost, c, r, m)$loanRem

# Refinance
nn <- n - m
rr <- 0.041 / monthsPerYear
cc <- annuity_c(0, cost, rr, nn)

# Save per month (per payment period)
cDiff <- c - cc

# NPV
costClosing <- 4400
NPV(costClosing, rep(cDiff, nn), rr)


## Question 6
# (10 points) The United States purchased Alaska in 1867 for $7.2M (where M stands for million). Assume that federal tax revenue from the state of Alaska (net federal expenditures) is $55.8M in 2017 and that tax revenue started in 1868 and has steadily increased by 3% annually since then. Assume that the cost of capital (or interest rate) is 7%. What is the NPV of the Alaska purchase, assuming that you are in 1867 looking forward?
#
# Treat as a perpetuity
cost <- 7.2e6
year.0 <- 1868
year.1 <- 2017
year.n <- year.1 - year.0
tax2018 <- 55.8e6
rTax <- 0.03
r <- 0.07

# Cashflow at year.0
c <- PV(tax2018, rTax, year.n)

# PV of a constant growth perpeuity (pv in 1867)
pv <- c / (r - rTax)

npv <- pv - cost; npv


## Question 7
# (10 points) This question introduces you to the concept of an annuity with growth. The formulae is given on p.3, equation (7), of the Note on Formulae, but I would encourage you to try doing it in Excel as well. (If the first cash flow is C, the next one will be C(1+g), and so on, where g is the growth rate in cash flow). As an example, the present value of an annuity that starts one year from now at $100, and grows at 5%, with the last cash flow in year 10, when the discount rate is 7%, is $860. Confirm this before attempting the problem using both the formula and excel. What is the NPV of of a new software project that costs $900,000 today, but has a cash flow of $180,000 in year 1 that grows at 3.0% per year till year 23? Similar investments earn 5.4% per year.
annuity_pvg(100, 0.07, 10, 0.05)

cost <- 900e3
c <- 180e3
r <- 0.054
n <- 23
g <- 0.03
annuity_pvg(c, r, n, g) - cost


## Question 8
# (15 points) Diane has just turned 18 and also completed high school. She is wondering about the value of a college education. She is pretty good with numbers, and driven by financial considerations only, so she sits down to calculate whether it is worth the large sum of money involved. She knows that her first year tuition will be $14,000, due at the beginning of the year (that is, right away). Based on historical trends she estimates that tuition will rise at 5% per year for the 4 years she is in school. She also estimates that her living expense above and beyond tuition will be $8,500 per year (assume this occurs at the end of the year) for the first year and will increase $500 each year thereafter to keep up with inflation. She does not plan to work at all while attending school. Were she to forgo college she would be able to make $27,000 per year out of high school and expects that to grow 3% annually. With the college degree, she estimates that she will earn $42,000 per year out of college, again with annual 3% increases in salary. Either way, she plans to work until 63 (she begins college right away). The interest/discount rate is 8%. What is the NPV of her college education?
college.fee0 <- 14e3
college.g <- 0.05
college.n <- 4

living.cost0 <- 8500
living.dCost <- 500

work.school0 <- 27e3
work.college0 <- 42e3
work.g <- 0.03

year.0 <- 18
year.1 <- 63
year.college1 <- year.0 + college.n

# Periods (exluding period 0)
year.n <- year.1 - year.0

r <- 0.08


## Yearly inflow and outflow, expressed in $ value at that period/year
# Exclude year 0
outflowLs <- rep(0, year.n)
inflowLs <- outflowLs

# 1) College tuition fees
for (i in 1:(college.n-1))
    outflowLs[i] <- FV(college.fee0, college.g, i)

# 2) Living expenses (assumed only applied during college)
for (i in 1:college.n)
    outflowLs[i] <- outflowLs[i] + living.cost0 + living.dCost * (i-1)

# 3) Opportunity cost from out-of-school salary
for (i in 1:year.n)
    outflowLs[i] <- outflowLs[i] + FV(work.school0, work.g, i-1)


## Yearly inflow
for ( i in seq_along((year.college1+1) : year.1) )
    inflowLs[college.n + i] <- FV(work.college0, work.g, i-1)

## NPV
NPV(college.fee0, inflowLs - outflowLs, r)


## Question 9
# (15 points) Rafael owned an apartment building that burned down. The empty lot is worth $70,000 and Rafael has received $200,000 from the insurance company. Rafael plans to build another apartment building that will cost $370,000. His real estate adviser estimates that the expected value of the finished building on the real estate market will be $505,000 next year. The discount/interest rate is 11%. What are the NPV and IRR of this decision?
cost.opp <- 70e3
cost.building <- 370e3
credit <- 200e3
r <- 0.11

# Project A: Gain empty lot and insurance; invest for a year

# Project B: Gain empty lot and insurance; spend building cost and empty lot to build property. At year 1, have property at stated value and invested insurance money
cost0 <- cost.opp + cost.building
# c1 <- 505e3 + FV(credit, r, 1)
c1 <- 505e3

NPV(cost0, c1, r)
IRR(cost0, c1)


## Question 10
# (15 points) Roxanne invested $260,000 in a new business 9 years ago. The business was expected to bring in $3,000 each month for the next 27 years (in excess of all costs). The annual cost of capital (or interest rate) for this type of business was 11% with monthly compounding. What is the value of the business today?
#
# Don't care about past business returns when valuing the business going forward; what is PV today?
r <- 0.11 / monthsPerYear
c <- 3e3
year.0 <- 9
year.1 <- 27
year.n <- (year.1 - year.0) * monthsPerYear
annuity_pv(c, r, year.n)





