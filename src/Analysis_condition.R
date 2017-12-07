# Analyse if condition factor K  differs according to head width within maturity classes
# By Pieterjan Verhelst


# First check if K differs according to maturation stadia
boxplot(eels$K~eels$Stadium, ylab = "K") 
aov <- aov(eels$K ~ eels$Stadium)
plot(aov)  # Check assumptions
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)  # sign difference between some groups, so nested design


# Create plot
boxplot(eels$K~eels$class, ylab = "K")
boxplot(eels$K~eels$class/eels$Stadium, ylab = "K")

# Apply anova with nested design (HW is nested in stadium)
aov <- aov(eels$K ~ eels$class/eels$Stadium)
plot(aov)  # Check assumptions
summary(aov)

# Apply post-hoc test
TukeyHSD(aov, conf.level=0.95, ordered = FALSE)

