# Analyse if condition factor K width differs according to head width within maturity classes
# By Pieterjan Verhelst


# Create plot
eels$Stadium <- factor(eels$Stadium)
boxplot(eels$K~eels$HW/eels$Stadium, ylab = "K")

# Apply anova with nested design (HW is nested in stadium)
aov <- aov(eels$K ~ eels$HW/eels$Stadium)
plot(aov)  # Check assumptions
summary(aov)

# Apply post-hoc test
TukeyHSD(aov, conf.level=0.95, ordered = FALSE)

