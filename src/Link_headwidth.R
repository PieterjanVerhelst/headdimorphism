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





