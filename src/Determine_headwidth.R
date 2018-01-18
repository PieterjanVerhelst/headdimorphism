# Link head width data to dataset
# By Pieterjan Verhelst

library(dplyr)
library(ggplot2)

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
#   0.260697     0.000936             intercept is 0.260697 and slope 0.000936: y = 0.000936x + 0.260697 (y = ax + b)

abline(0.260697, 0.000936)
# or: abline(lm(eels$HWHL~eels$Length))

# Calculate expected HWHL for each eel
eels$HWHLexp <- (0.000936 * eels$Length) + 0.260697 

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
  eels$class[i] = "IH"
}}

eels$class <- factor(eels$class)


# Create histogram
hist(eels$diff, breaks = 20, xlim=c(-0.10, 0.15))

ggplot(data=eels, aes(eels$diff, fill = class)) +
  scale_fill_grey() +
  theme_classic() +
  theme(legend.position="none") + 
  geom_histogram(breaks=seq(-0.10, 0.15, by=0.01)) +
  labs(x="Residual values", y="Count") +
  geom_vline(aes(xintercept=mean(diff)),
             color="black", linetype="dashed", size=1)
  


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
boxplot(eels$diff ~ eels$Stadium, ylab="Residuals", xlab="Stage")
aov <- aov(eels$diff ~ eels$Stadium)
plot(aov)
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)


