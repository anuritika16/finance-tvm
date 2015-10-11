# Coursera Finance Specialisation (U Michigan)
# Week 3 -- Decision Making
# Lectures
#
# Joe Nguyen | 01 Oct, 2015

rm(list = ls())
graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm/r-code"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, dirWorking, "/func_finance.R"))

cost <- 1000
c <- c(800, 800, -500, 400, -200, 210, -200)
r <- seq(0.0, 1, length.out = 50)
npv <- NPV(cost, c, r)
npvRange <- diff(range(npv))

# Find r when npv == 0 (i.e. IRR)
irr <- IRR(cost, c)


## Plot
library(ggplot2)
dfLine <- data.frame(r, npv)
dfIrr <- data.frame(irr, npv = 0)

ggplot() + 
    geom_line(data = dfLine, aes(r, npv)) + 
    geom_hline(yintercept = 0, lty = "dashed") +
    geom_point(data = dfIrr, aes(irr, npv), color = "red", size = 5) + 
    geom_text(data = dfIrr, aes(label = round(irr,3), irr*1.1, npv + 0.05*npvRange))



## 3.8 IRR: BIAS 1
##################
# IRR is myopic (similar to payback)
a.cost <- 2000
a.c <- c(400, 2400)
a.irr <- IRR(a.cost, a.c)

b.cost <- 2000
b.c <- c(2000, 625)
b.irr <- IRR(b.cost, b.c)

cat("Project a IRR: ", round(a.irr, 4), "% | Project b IRR: ", round(b.irr, 4), "%", sep = "")

# IRR favors project b, but project a gives higher return later, suggesting project a has more value in the future.

a.npv <- NPV(a.cost, a.c, c(0.05, 0.11, 0.25)); a.npv
b.npv <- NPV(b.cost, b.c, c(0.05, 0.11, 0.25)); b.npv

# Factoring in the market rate (external to organisation IRR), we can use NPV to choose which project provides the higher return.


## 3.9 IRR: BIAS 2
##################
# IRR favours small investments (small investment bias) because investment (cost) is the denominator.
IRR(5000,7500)




