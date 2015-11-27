# Coursera Finance Specialisation (U Michigan)
# Generic Finance Functions
#
# Joe Nguyen | 29 Sep, 2015

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- ""
setwd(paste0(dirBase, dirWorking))
rm(list = ls())

##################
## CONSTANTS
##################
weeksPerYear <- 52
fortsPerYear <- 26
monthsPerYear <- 12
halfsPerYear <- 2
##################


##################
## Future value ##
##################
# INPUTS:
# pv    present value
# r     interest rate (decimal)
# n     number of periods
#
# OUTPUTS:
# fv    future value
#
FV <- function(pv, r, n) { pv * (1 + r)^n }


###################
## Present value ##
###################
# INPUTS:
# fv    future value
# r     interest rate (decimal)
# n     number of periods
#
# OUTPUTS:
# pv    present value
#
PV <- function(fv, r, n) { fv / (1 + r)^n }


############################
## Annuities future value ##
############################
# Compute fv given payments made every n periods from present to future.
# Assume payment occurs at each period t so interest on current payment is NOT yet earned at this period, but will start compounding from period t+1.
# Formula: fv = c( (1+r)^(n-1) + ... + 1 ) 
#
# INPUTS:
# c     cashflow
# r     interest rate (decimal)
# n     number of periods
#
# OUTPUTS:
# fv    future value
#
annuity_fv <- function(c, r, n) {
    
    fv <- 0
    
    for (i in 1 : n-1) {
        fv <- fv + FV(c, r, i)
    }
    fv
}

#############################
## Annuities present value ##
#############################
# Compute present value required so that cash flow (or payment) can be expended for n periods
# Formula: pv = c ( 1/(1+r) + 1/(1+r)^2 + ... + 1/(1+r)^n ) 
#
# INPUTS:
# c     cashflow
# r     interest rate (decimal)
# n     number of periods
#
# OUTPUTS:
# pv    present value
#
annuity_pv <- function(c, r, n) {
    
    pv <- 0
    
    for (i in 1 : n) {
        pv <- pv + PV(c, r, i)
    }
    pv
}


###############################################
## Annuities present value growing at rate g ##
###############################################
# If the first cash flow is C, the next one will be C(1+g), and so on, where g is the growth rate in cash flow
# pv = c ( 1/(1+r) + (1+g)/(1+r)^2 + ... + (1+g)^(n-1) / (1+r)^n)
#
# Using geometric series, transform to:
# pv = (c / (r-g)) [1 - ((1+g)/(1+r))^n]
#
# INPUTS:
# c     cashflow
# r     interest rate (decimal)
# n     number of periods
# g     growth rate (decimal)
#
# OUTPUTS:
# pv    present value
#
annuity_pvg <- function(c, r, n, g) {
    c/(r-g) * (1 - ((1+g)/(1+r))^n)
}


#########################
## Annuities cash flow ##
#########################
# Compute cash flow (or payment) given fv or pv (other input is 0).
#
# fv: what is cash flow required to ensure fv target is achieved. Assume payment occurs at end of each year (no payment at 0th year or period 1).
# Formula: fv = c( (1+r)^(n-1) + ... + 1 ) 
#
# pv: what is cash flow required to pay off pv (e.g. loan taken now).
# Formula: pv = c( 1/(1+r) + 1/(1+r)^2 + ... + 1/(1+r)^n) 
#
# INPUTS:
# fv    future value
# pv    present value
# r     interest rate (decimal)
# n     number of periods
#
# OUTPUTS:
# c     cash flow
#
annuity_c <- function(fv, pv, r, n) {
    
    geo_seq <- 0
    nLs <- if(fv != 0) (1 : n-1) else (1 : n)
    
    for (i in nLs) {
        geo_seq <- geo_seq + ifelse(fv != 0, FV(1, r, i), PV(1, r, i))
    }
    c <- ifelse(fv != 0, fv, pv) / geo_seq; c
}


#################################
## Effective Annual Rate (EAR) ##
#################################
# EAR < r when interest is paid for more periods than once annually.
#
# INPUTS:
# r     interest rate (decimal)
# k     payment periods in a year (e.g. monthly payments, k = 12)
EAR <- function(r, k) { (1 + r/k)^k - 1 }


###################################
## Loan remaining principle (pv) ##
###################################
# Given a loan (present value) to pay off with n periodic cash flows, compute remaining pv after m < n payments #
# INPUTS:
# loan  initial pv
# c     cash flow required to pay off loan in n periods
# r     interest rate (decimal)
# m     number of periods (< n) already paid
#
# OUTPUTS:
# loanRem   remaining loan
# interest  accumulated interest paid to lender
# repay     repayment over m periods
#
loan_rem <- function(loan, c, r, m) {
    interestTot <- 0

    for (i in 1:m) {
        # Interest on loan for this period
        interest <- loan * r

        # Accumulate interest charged by lender
        interestTot <- interestTot + interest
        
        # Make payment
        loan <- loan + interest - c
    }
    list("loanRem" = loan, "interest" = interestTot, "repay" = c * m)
}


#############################
## Net Present Value (NPV) ##
#############################
# NPV is the difference between the present value of cash inflows and the present value of cash outflows.
# NPV = c0 + c1/(1+r) + c2/(1+r)^2 + ... cn/(1+r)^n
# c1 ... cn may be negative (outflow) or positive (inflow). c0 is the initial investment cost (negative)
#
# INPUTS:
# cost      initial investment cost (input as positive value)
# cVec      [nx1] vector of cash in/out flows for n periods
# rVec      [mx1] discount rate - enable vector algebra for m different r's
# tVec      (optional) [nx1] vector of period/time indices when cash flows arrive
#
# OUTPUT:
# npv       [mx1] NPVs for m r's
#
NPV <- function(cost, cVec, rVec, tVec = seq_along(cVec)) {
    npv <- -cost
    for (i in seq_along(cVec)) {
        npv <- npv + PV(cVec[i], rVec, tVec[i])
    }
    npv
}


###################################
## Internet Rate of Return (IRR) ##
###################################
# IRR is the discount rate when NPV equals zero. IRR is similar to payback but accounts for TVM (time value of money). Therefore, IRR is insidiously myopic. Use numerical optimisation to solve for r in NPV formula.
#
# INPUTS:
# c0        initial investment cost (input as positive value)
# cVec      [nx1] vector of cash in/out flows for n periods
# rEst      (optional) IRR esimate   
#
# OUTPUT:
# irr       discount rate r
#
IRR <- function(cost, cVec, rEst = 0.5) {
    
    p <- list(cost, cVec)
    obj <- function(rEst, p) { NPV(p[[1]], p[[2]], rEst)^2 }

    # Brent method (golden section search + parabolic interp)
    optPar <- optimise(obj, c(0,1), p = p)
    return(optPar$minimum)
    
#     # Nelder-Mead method (downhill simplex)
#     optPar <- optim(par = rEst, fn = obj, p = p)
#     return(optPar$par)
}


###################################
## PLOT: NPV VS R ##
###################################
# INPUTS:
# newplot   flag for new plot or overlay on existing plot
#
plot_npv_r <- function(r, npv, irr = 0, color = 1, newplot = TRUE) {
    
    # Use base plotting system because ggplot2 can't overlay plots from different function calls
    if (newplot) {
        plot(r, npv, type = "l", lwd = 2, col = color,
             main = "NPV vs. Interest rate (r)",
             xlab = "Interest rate (dec)",
             ylab = "NPV ($)")
        abline(h = 0, lty = 2)
        grid()
    } else {
        points(r, npv, type = "l", lwd = 2, col = color)
    }
    points(irr, 0, pch = 19, col = "red")
    
    
#     library(ggplot2)
#     dfLine <- data.frame(r, npv)
#     dfIrr <- data.frame(irr, npv = 0)
# 
#     g <- ggplot(environment = environment()) + 
#         geom_line(data = dfLine, aes(r, npv)) + 
#         geom_hline(yintercept = 0, lty = "dashed") +
#         geom_point(data = dfIrr, aes(irr, npv), color = "red", size = 5) + 
#         geom_text(data = dfIrr, aes(label = round(irr,3), irr*1.1, npv + 0.05*diff(range(npv)) ))
#     g
}



##################################
##     M2: Risk and Return     ##
##################################
# 28 Nov, 2015

#############################
## Yield to Maturity (YTM) ##
#############################
# YTM is the return built into the pricing of a bond. YTM is the discount rate r in the PV formula, where fv (future value) is the "face value" of the bond, and pv is the "current price"
#
# INPUTS:
# fv  - face value
# pv  - current price
# n   - number of periods
#
# OUTPUT:
# r  - discount rate (or YTM) (decimal)
#
YTM <- function(fv, pv, n) { (fv/pv)^(1/n) - 1 }


