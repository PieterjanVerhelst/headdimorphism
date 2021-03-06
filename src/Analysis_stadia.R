# Analysis if number of NH and BH differ between maturation stadia
# Paul Quataert en Pieterjan Verhelst

require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
library(mixtools)
library(mclust)

###############################
# ANALYSE HEAD WIDTH DISTRIBUTION OVER DIFFERENT MATURATION STADIA
###############################

# Change subset according to the different maturation stages
sub <- eels[which(eels$Stadium == "I"),]


uni<-Mclust(sub$diff,1)
bi<-Mclust(sub$diff,2)
# Calculate AIC manually because of an unknown error
# https://stackoverflow.com/questions/28342653/model-selection-with-aic
2*uni$df - 2*uni$loglik
2*bi$df - 2*bi$loglik
# For BIC and LogLikelihood, check summary
summary(uni)    # small values of BIC, AIC and LogLikelihood indicate better models
summary(bi)


# Create plot
residuals <- sub$diff
mixmdl <- normalmixEM(residuals, maxit = 2000, k=2)
plot(mixmdl,which=2, xlim=c(-0.10, 0.15), breaks = 10, whichplots = 2)
lines(density(residuals), lty=2, lwd=2)

xfit<-seq(min(sub$diff),max(sub$diff),length=100)
yfit<-dnorm(xfit,mean=mean(sub$diff),sd=sd(sub$diff))
lines(xfit,yfit, col="blue", lwd = 2)  # normal pdf
# Export as SVG



###############################
# ANALYSE DIFFERENT PROPORTION IN HEAD WIDTH CLASSES
###############################

# Determine number of NH, inter and BH for each maturation stadium
sub <- eels %>%
  group_by(Stadium, class) %>%
  summarise(number=n())


# Create barplot
# First put counts in a matrix
your.mat.data <- c(8,10,15,2,7,0,34,50,62,11,28,4,9,8,14,2,5,3)
data <- matrix(data = your.mat.data, 
               nrow = 3,
               ncol = 6,
               byrow = TRUE)
colnames(data)=c("I","FII","FIII","FIV","FV","MII")
rownames(data)=c("NH","IH","BH")


# Get the stacked barplot
barplot(data, col=colors()[c(29,300,554)] , border="white", space=0.04, font.axis=2, xlab="group")

# Grouped barplot
barplot(data, col=colors()[c(230,195, 155)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="Stage", ylab = "Number of eels", ylim=c(0,70), font.lab=2)



###############################
# LOGISTIC REGRESSION
###############################

#### Analysis

# data step

flevS <- c("I", "FII", "FIII", "FIV", "FV", "MII")

Mset <- data.frame(
  Stage = factor(flevS, flevS), # guantees correct order of levels
  NH = c(11, 13, 19, 2, 7, 0), 
  IH = c(48, 62, 82, 12, 32, 4),
  BH = c(12, 13, 20, 2, 6, 3)
)

Mset$Tot <- with(Mset, NH + IH + BH)
Mset$pNH <- with(Mset, NH / Tot)
Mset$pBH <- with(Mset, BH / Tot)

Mset$nStage <- with(Mset, as.numeric(Stage)) # linear trend
Mset$oStage <- with(Mset, ordered(Stage)) # ordered factor

Mset # de dataset

#Stage NH inter BH Tot       pNH       pBH nStage oStage
#1     I 11    48 12  71 0.1549296 0.1690141      1      I
#2   FII 13    62 13  88 0.1477273 0.1477273      2    FII
#3  FIII 19    82 20 121 0.1570248 0.1652893      3   FIII
#4   FIV  2    12  2  16 0.1250000 0.1250000      4    FIV
#5    FV  7    32  6  45 0.1555556 0.1333333      5     FV
#6   MII  0     4  3   7 0.0000000 0.4285714      6    MII

# het percentage NH stijgt of daalt niet, maar blijft gelijk
# via een logistisch model kunnen we nu testen of er desalniettemin een significant verschil is

# --- MODEL 1: het nulmodel ---

# fit null binomial model (d.w.z. geen afhankelijkheid van stadium)
# we gaan na of de residual deviance statistisch significant is
# d.w.z. groter dan verwacht onder de nulhypothese. 

GfitB_null <- glm(pNH ~ 1, data = Mset, family = 'binomial', weights = Tot)

# anova

as.data.frame(anova(GfitB_null))
#Df Deviance Resid. Df Resid. Dev
#NULL NA       NA         5   2.430748

# De Residual Deviance? is 2.43 bij 5 vrijheidsgraden. 
# De correspondeerde p-waarde is 0.79

1 - pchisq(2.430748, 5) 

# We kunnen bijgevolg het null-model niet verwerpen, 
# maar als we de residuals bekijken dan zien we een patroon
# dat is een indicatie dat er misschien toch een trend in de data zit

# Pearson residuals

residuals(GfitB_null, type = 'pearson')
# Indien in de residuals een trend gevonden wordt (vb stijgt over stadia), dan verder ondezoeken
# In geval met alle data, geen trend in de residuals

# 1           2           3           4           5           6 
#0.13009548 -0.04468014  0.23448214 -0.27405100  0.11534996 -1.10893180 

# --- MODEL 2: relatie met maturiteit ---

# fit model with ordered factor
# het voordeel van een ordered factor is dat we kunnen nagaan in hoeverre er een trend in de data zit, zie verder.
# Met drie datapunten wordt de factor intern opgesplitst in een lineaire en kwadratische trend

GfitB_ord <- glm(pNH ~ oStage, data = Mset, 
  family = 'binomial', weights = Tot)

# anova: we zien opnieuw dat de trend niet significant is.
# het resultaat is identiek als voorheen
# alleen testen we nu of de toegevoegde term significant is
# in het nulmodel testen we of de rest significant is.
# maar dat komt op hetzelfde neer.

as.data.frame(anova(GfitB_ord, test = 'Chisq'))

#        Df Deviance Resid. Df    Resid. Dev  Pr(>Chi)
# NULL   NA       NA         2  3.468765e+00        NA
# oStage  2 3.468765         0 -6.883383e-15 0.1765092

# test for the trend components (L: linear, Q: quadratic)

summary(GfitB_ord)$coef
#               Estimate Std. Error    z value   Pr(>|z|)
# (Intercept) -0.6607593  0.2932793 -2.2530035 0.02425893
# oStage.L     0.6977475  0.3859561  1.8078414 0.07063119
# oStage.Q     0.2284622  0.6058996  0.3770628 0.70612695

# de p-waarde geassocieerd met een lineaire trend is 0.07. 
# Dat is net niet signifcant. 
# vermoedelijk zal met meer gegevens? deze component significant worden.




###############################
# ORDINAL LOGISTIC REGRESSION
###############################

subset <- eels %>%
  group_by(class, Stadium) %>%
  summarise(count=n())

head(subset)


# Create ordered factor
subset$class <- factor(subset$class, levels=c("NH", "IH", "BH"), ordered=TRUE)
subset$Stadium <- factor(subset$Stadium, ordered=FALSE)


# Ordinal logistic regression
m <- polr(class ~ Stadium, data=subset, Hess=TRUE)
summary(m)

# Calculate metrics
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

#Value Std. Error      t value   p value
#StadiumFII  -1.148292e-05   1.517860 -7.565207e-06 0.9999940
#StadiumFIII -1.148292e-05   1.517860 -7.565207e-06 0.9999940
#StadiumFIV  -1.148292e-05   1.517860 -7.565207e-06 0.9999940
#StadiumFV   -1.148292e-05   1.517860 -7.565207e-06 0.9999940
#StadiumMII   1.045218e+00   1.677916  6.229265e-01 0.5333328
#NH|IH       -7.611106e-01   1.107766 -6.870681e-01 0.4920398
#IH|BH        7.611079e-01   1.107766  6.870657e-01 0.4920413

# For a change in stadium from I to FII, a decrease of 1.14e-05 in IH is expected


# Confidence intervals
# If the 95% CI does not cross 0, the parameter estimate is statistically significant
ci <- confint(m)           #  default method gives profiled CIs
# 2.5 %   97.5 %
#StadiumFII  -3.073262 3.073262
#StadiumFIII -3.073262 3.073262
#StadiumFIV  -3.073262 3.073262
#StadiumFV   -3.073262 3.073262
#StadiumMII  -2.214854 4.733948
ci <- confint.default(m)    # CIs assuming normality
exp(coef(m))
## OR (odds ratio) and CI
exp(cbind(OR = coef(m), ci))




