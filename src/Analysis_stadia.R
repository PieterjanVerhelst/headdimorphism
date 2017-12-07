# Analysis if number of NH and BH differ between maturation stadia
# Paul Quataert


# Determine number of NH, inter and BH for each maturation stadium
sub <- eels[which(eels$Stadium == "MII"),]
table(sub$class)


# Create barplot
# First put counts in a matrix
your.mat.data <- c(11,13,19,2,7,0,48,62,82,12,32,4,12,13,20,2,6,3)
data <- matrix(data = your.mat.data, 
               nrow = 3,
               ncol = 6,
               byrow = TRUE)
colnames(data)=c("I","FII","FIII","FIV","FV","MII")
rownames(data)=c("NH","inter","BH")


# Get the stacked barplot
barplot(data, col=colors()[c(29,300,554)] , border="white", space=0.04, font.axis=2, xlab="group")

# Grouped barplot
barplot(data, col=colors()[c(29,300, 554)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="Stadium", ylab = "Count", ylim=c(0,100), font.lab=2)



#### Analysis

# data step

flevS <- c("I", "FII", "FIII", "FIV", "FV", "MII")

Mset <- data.frame(
  Stage = factor(flevS, flevS), # guantees correct order of levels
  NH = c(11, 13, 19, 2, 7, 0), 
  inter = c(48, 62, 82, 12, 32, 4),
  BH = c(12, 13, 20, 2, 6, 3)
)

Mset$Tot <- with(Mset, NH + inter + BH)
Mset$pNH <- with(Mset, NH / Tot)
Mset$pBH <- with(Mset, BH / Tot)

Mset$nStage <- with(Mset, as.numeric(Stage)) # linear trend
Mset$oStage <- with(Mset, ordered(Stage)) # ordered factor

Mset # de dataset

# 	 Stage NH BH Tot       pNH       pBH nStage oStage
# 1  FIII  9 26  35 0.2571429 0.7428571      1   FIII
# 2   FIV  3  7  10 0.3000000 0.7000000      2    FIV
# 3    FV 13 14  27 0.4814815 0.5185185      3     FV

# het percentage NH stijgt van 25.7 %, over 30 % naar 48 %
# via een logistisch model kunnen we nu testen of deze stijging significant is.

# --- MODEL 1: het nulmodel ---

# fit null binomial model (d.w.z. geen afhankelijkheid van stadium)
# we gaan na of de residual deviance statistisch significant is
# d.w.z. groter dan verwacht onder de nulhypothese. 

GfitB_null <- glm(pNH ~ 1, data = Mset, family = 'binomial', weights = Tot)

# anova

as.data.frame(anova(GfitB_null))
#      Df Deviance Resid. Df Resid. Dev
# NULL NA       NA         2   3.468765

# De Residual Deviance? is 3.47 bij 2 vrijheidsgraden. 
# De correspondeerde p-waarde is 0.17

1 - pchisq(3.468765, 2) 

# We kunnen bijgevolg het null-model niet verwerpen, 
# maar als we de residuals bekijken dan zien we een patroon
# dat is een indicatie dat er misschien toch een trend in de data zit

# Pearson residuals

residuals(GfitB_null, type = 'pearson')
# in de residuals tekent zich een stijgende trend af

#          1          2          3 
# -1.1193680 -0.3136606  1.4653443 

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
