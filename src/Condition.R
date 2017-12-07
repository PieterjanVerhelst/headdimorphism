# Determine relative condition factor
# By Pieterjan Verhelst

library(FSA)


# Plot weight over length
plot(eels$Weight~eels$Length)

#### Determine relative condition factor
lm1 <- lm(log(eels$Weight)~log(eels$Length))
fitPlot(lm1,xlab="log Total Length (mm)",ylab="log Weight (g)",main="")
summary(lm1) # deduce a and b constants, R? = 0.97; Note that a = e^intercept!

e <- exp(1)

eels$K <- eels$Weight/((e^-7.36117) * (eels$Length^3.25776))

# Fulton's K
# eels$Kful <- 100 * (eels$Weight / (eels$Length^3))

# Statistically test if eels show allometric growth
# H0: b = 0 --> Isometric growth
# Ha: b =! 0 --> Allometric growth

hoCoef(lm1,2,3)
# p-value < 0.05: allometric growth (reject H0)
confint(lm1)
# allometric growth with an exponent parameter (b) between 3.20 and 3.31, with 95% confidence.


#### Examine differences in condition factor
boxplot(eels$K ~ eels$Stadium)
aov <- aov(eels$K ~ eels$Stadium)
plot(aov)
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)

