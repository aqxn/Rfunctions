# function in R to determine the lowest order polynomial that best fits an
# y=f(x) dataset and returns the degree/order of that polynomial and
# adjusted r-squared value

# assumes that all values of x have a corresponding value for y
# function does not check for NAs (yet)

polyfit <- function (y, x) {
    
    max.deg <- length(y) - 1
    siglevel = 0.05
    
    for (deg in 1:max.deg) {
        fit1 <- lm(y~poly(x,degree=deg,raw=TRUE))
        fit2 <- lm(y~poly(x,degree=(deg+1),raw=TRUE))
        if (anova(fit1,fit2)$Pr[2] < siglevel) {
            next
        } else {
            return(c(deg, summary(fit1)$adj.r.squared))
        }
    }
}


# references:
# http://davetang.org/muse/2013/05/09/on-curve-fitting/

