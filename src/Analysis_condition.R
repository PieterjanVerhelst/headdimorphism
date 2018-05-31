# Analyse if condition factor K  differs according to the residuals
# By Pieterjan Verhelst


library(PMCMR)


# Calculate condition metrics

mean(eels$K)
sd(eels$K)
min(eels$K)
max(eels$K)


# First check if K differs according to maturation stadia
plot(eels$K~eels$diff, ylab = "K") 
lm(eels$K~eels$diff)
#Call:
#  lm(formula = eels$K ~ eels$diff)
#Coefficients:
#  (Intercept)    eels$diff  
#1.0123       0.7971  

abline(1.0123 , 0.7971)




#### Apply linear regression ####

xyplot(K ~ diff, data = eels,
       xlab = "Residuals",
       ylab = "Relative condition (Kn)"
)

model1 = lm(K ~ diff, data = eels)
summary(model1)


# Fit residuals to check for any patterns
xyplot(resid(model1) ~ fitted(model1),
       xlab = "Fitted Values",
       ylab = "Residuals",
       main = "Residual Diagnostic Plot",
       panel = function(x, y, ...)
       {
         panel.grid(h = -1, v = -1)
         panel.abline(h = 0)
         panel.xyplot(x, y, ...)
       }
)


# Check normality (data should approach straight line)
qqmath( ~ resid(model1),
        xlab = "Theoretical Quantiles",
        ylab = "Residuals"
)


