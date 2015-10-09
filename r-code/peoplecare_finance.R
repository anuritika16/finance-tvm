# Peoplecare Finance
# Visualise {interest above yearly payment type ($)} for {weekly, fortnightly, monthly, quarterly, 6 monthly, yearly} payments
#
# Joe Nguyen | 30 Sep, 2015

rm(list = ls())
# graphics.off()

# Change working directory
dirBase <- "/home/joe/Documents/01-coursera/02-finance"
dirWorking <- "/01-tvm"
setwd(paste0(dirBase, dirWorking))

# Generic finance functions
source(paste0(dirBase, "/func_finance.R"))


######################################
## USER INPUTS
# Number of years of membership
# yearLs <- 1:10
yearLs <- seq(1,21, by = 2)

# assume fixed yearly interest rate
rLs <- seq(3.00, 4.00, by = 0.5) / 100
######################################


# {weekly, fortnightly, monthly, quarterly, 6 monthly, yearly} payments
payType <- c("weekly", "fortnightly", "monthly", "quarterly", "6monthly", "yearly")
payType <- factor(payType, levels = payType)
paymentLs <- c(4.54, 9.13, 19.83, 59.49, 118.98, 238.01)

weeksPerYear <- 52
fortnPerYear <- 26
monthPerYear <- 12
quartPerYear <- 4
halfsPerYear <- 2
conversion <- c(weeksPerYear, fortnPerYear, monthPerYear, quartPerYear, halfsPerYear, 1)

# Data frame (interest above yearly payType ($))
numPayType <- length(payType)
numYear <- length(yearLs)
numR <- length(rLs)

idf <- data.frame(payType  = rep(payType, times = numYear * numR),
                  year     = rep(yearLs,  each = numPayType),
                  r        = rep(rLs,     each = numPayType * numYear),
                  interest = rep(0,       times = numPayType * numYear * numR))


## Compute interest gained by placing yearly payType (paymentLs[end]) in bank and making other payTypes to peoplecare
for (r in 1:numR) {
    rr <- rLs[r]
    # fv <- 0
    
    for (y in 1:numYear) {
        year <- yearLs[y]
        
#         # Annuity fv period
#         yearDiff <- if(y == 1) year else year - yearLs[y-1]
#         
#         # Annuity fv by keeping yearly payment (paymentLs[end]) in bank
#         fv <- fv + annuity_fv(paymentLs[numPayType], rr, yearDiff)
        fv <- annuity_fv(paymentLs[numPayType], rr, year)
        
        for (p in numPayType:1) {
            fv2 <- fv
            
            # Reduction in annuity fv due to payments (to peoplecare)
            for (n in 1:(year * conversion[p])) {
                # Make payment
                fv2 <- fv2 - paymentLs[p]
                
                # fv earns interest (from bank)
                fv2 <- FV(fv2, rr/conversion[p], 1)
            }
            # fv - {fv yearly payType}. If -ve, payType worse than yearly
            if (p == numPayType) { iYearly <- fv2 }
            idx <- ((r-1) * (numPayType * numYear)) + ((y-1) * numPayType) + p
            idf$interest[idx] <- fv2 - iYearly
        }
    }
}


###################
## Visualisation ##
###################
library(ggplot2)
# ip_bar <- ggplot(idf, aes(x = year, y = interest, fill = payType)) + 
#     geom_bar(stat = "identity", position = position_dodge()) + 
#     facet_wrap(~r)
# ip_bar

# Line plot
lw <- 1.5
ip_line <- ggplot(idf, aes(x = year, y = interest, color = payType)) + 
    geom_line(size = lw) + 
    geom_point(shape = 19, size = 2*lw) + 
    facet_wrap(~r) + 
    scale_x_discrete("Years of Membership", yearLs) + 
    labs(title = "Interest Gained Over Yearly Payment Type",
         y = "Interest over yearly payType ($)") + 
    theme(text = element_text(size = 20),
          strip.text = element_text(size = 20))
ip_line

