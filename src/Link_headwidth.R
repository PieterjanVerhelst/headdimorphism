# Link head width data to dataset
# By Pieterjan Verhelst

library(dplyr)

# Upload eel data
hd <- read.csv("./data/raw/Headdimorphism.csv",sep=";",stringsAsFactors = FALSE)

# Select relevant columns, substitute ',' by '.' and set as numeric
hd <- select(hd, ID, Width.skull, Head.length)
hd$Width.skull <- gsub("\\,", ".", hd$Width.skull)
hd$Width.skull <- as.numeric(hd$Width.skull)
hd$Head.length <- gsub("\\,", ".", hd$Head.length)
hd$Head.length <- as.numeric(hd$Head.length)

# Rename columns
colnames(hd)[2] <- "HW"   # Width.skull is head width = HW
colnames(hd)[3] <- "HL"   # Head.length is head length = HL

# Calculate ratio HW:HL
hd$HWHL <- hd$HW/hd$HL

# Merge two datasets
eels <- merge(eels, hd, by="ID")

# Plot HWHL to total length and calculate residuals
plot(eels$HWHL~eels$Length)
lm(eels$HWHL~eels$Length)
#Call:
#  lm(formula = eels$HWHL ~ eels$Length)
#Coefficients:
#  (Intercept)  eels$Length  
#0.261124     0.000927            intercept is 0.261124 and slope 0.000927: y = 0.000927x + 0.261124 (y = ax + b)

abline(0.261124, 0.000927)
# or: abline(lm(eels$HWHL~eels$Length))

# Calculate expected HWHL for each eel
eels$HWHLexp <- (0.000927 * eels$Length) + 0.261124 

# Calculate difference between real and expected (= residuals)
eels$diff <- eels$HWHL - eels$HWHLexp         # when positive, head is wider than expected (and vice versa)

# Calculate mean and sd of residuals
eels$diff_mean <- mean(eels$diff)
eels$diff_sd <- sd(eels$diff)



# Classify headwidth of the eels
# NH: residual < mean - sd
# intermediate: mean +/- sd
# BH: residual > mean + sd

for (i in 1:dim(eels)[1]){
  if (eels$diff[i] < (eels$diff_mean - eels$diff_sd)){
  eels$class [i] = "NH"
} else if (eels$diff[i] > (eels$diff_mean + eels$diff_sd)){
  eels$class [i] = "BH"
} else{
  eels$class[i] = "inter"
}}

eels$class <- factor(eels$class)


# Check if residuals don't differ to much between maturation stadia, otherwise split the analysis up
# Calculate mean and sd of residuals for each eel stadium
#eels$Stadium <- factor( eels$Stadium , ordered = FALSE )
subset <- eels %>%
  group_by(Stadium) %>%
  select(Stadium, diff) %>%
  summarise(
    stadium_mean = mean(diff),
    stadium_sd = sd(diff)
  )

# Create plot
plot(1:6,subset$stadium_mean,pch=19,xlab="",ylab="",xaxt="n",xlim=c(0.5,6),
     ylim=c(min(subset$stadium_mean-subset$stadium_sd),max((subset$stadium_mean+subset$stadium_sd))))
lines(rbind(1:6,1:6,NA),rbind(subset$stadium_mean-subset$stadium_sd,subset$stadium_mean+subset$stadium_sd,NA))
axis(side=1,at=1:6,labels=subset$Stadium)

# Anova
boxplot(eels$diff ~ eels$Stadium)
aov <- aov(eels$diff ~ eels$Stadium)
plot(aov)
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)






