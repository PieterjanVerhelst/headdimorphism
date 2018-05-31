# Analyse if swim speed differs between head width classes (NH, IH, BH)
# By Pieterjan Verhelst


library(dplyr)
library(PMCMR)
library(nlme)
library(car)
#library(multcomp)   # inactivate package, interference with select()


# Upload eel telemetry data
m <- read.csv("./data/raw/migration.csv",sep=",",stringsAsFactors = FALSE)

# Select Zeeschelde
m <- filter(m, network == "Zeeschelde" | network == "Albertkanaal")


# Extract head width classes
# Run 'Determine_headwidth.R' manually
hw <- select(eels, Zendernummer, diff)
colnames(hw)[1] <- "Transmitter"
# Remove rows with NA
# i.e. eels with no Transmitter
hw <- na.omit(hw)


# Merge head width residuals (hw dataset) to swim speeds (m1 dataset)
m$Transmitter<-gsub("A69-1601-","",m$Transmitter)
m$Transmitter<-gsub("A69-1602-","",m$Transmitter)
m1 <- merge(m, hw, by="Transmitter")




# Calculate migration speed
# Calculate migration time and distance + plot migration time
mt = m1 %>%
  group_by(Transmitter)%>%
#  select(Transmitter, diff, Arrivalnum, Departurenum, Station_distance) %>%
  summarise( seconds=with(m1, max(Departurenum) - min(Arrivalnum)),
             dist= with(m1, max(Station_distance) - min(Station_distance)),
             diff= with(m1, min(diff))
  )


# Calculate tracking time in days
mt$days=mt$seconds/(60*60*24)
mt$days=round(mt$days, 2)
par(mar=c(6,4.1,4.1,2.1))
barplot(mt$days, names.arg=mt$Transmitter, cex.names=0.8, ylim=c(0,1.3*max(mt$days)),las=2)



# Calculate migration speed whole study area
mt$speed <- mt$dist / mt$seconds
mean(mt$speed)
sd(mt$speed)
min(mt$speed)
max(mt$speed)





#### Apply linear regression ####

plot(mt$speed~mt$diff, ylab = "Migration speed", xlab = "Unstandardised residuals") 
lm(mt$speed~mt$diff)
#Call:
#  lm(formula = mt$speed ~ mt$diff)
#Coefficients:
#  (Intercept)      mt$diff  
#0.05024      0.21889 

abline(0.05024 , 0.21889)


model1 = lm(speed ~ diff, data = mt)
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








# Remove outliers
mt$speed_round <- round(mt$speed, digits = 5)

dotchart(mt$speed)
x <- c(
  0.40362,
  0.38844,
  0.16461,
  0.09061,
  0.08572,
  0.07940)

mt2 <- mt[!mt$speed_round %in% x,]

shapiro.test(mt2$speed)
kruskal.test(speed~class, data=mt2)
aov <-aov(mt2$speed~mt2$class)
plot(aov)
summary(aov)

boxplot(mt2$speed~mt2$class, ylab = "Migration speed (m/s)") 
boxplot(mt2$speed~mt2$class, ylab = "Migration speed (m/s)",outline=FALSE)


##################
# Linear mixed effects model - Random intercept model
##################

mt$fTransmitter <- factor(mt$Transmitter)
Mlme1 <- lme(speed ~ class, random = ~1 | fTransmitter,
             data = mt)
summary(Mlme1)
summary(Mlme1)$tTable[,"p-value"]
# Conduct multiple comparisons (multcomp package) https://stats.stackexchange.com/questions/237512/how-to-perform-post-hoc-test-on-lmer-model
summary(glht(Mlme1, linfct = mcp(class = "Tukey")), test = adjusted("holm"))

  