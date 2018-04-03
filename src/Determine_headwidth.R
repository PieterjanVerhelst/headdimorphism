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

# Summarise morphological characteristics
#eels$Eye_hor <- gsub("\\,", ".", eels$Eye_hor)
#eels$Eye_hor <- as.numeric(eels$Eye_hor)
#eels$Eye_vert <- gsub("\\,", ".", eels$Eye_vert)
#eels$Eye_vert <- as.numeric(eels$Eye_vert)
#eels$Pectoral_fin <- gsub("\\,", ".", eels$Pectoral_fin)
#eels$Pectoral_fin <- as.numeric(eels$Pectoral_fin)
#aggregate(eels$Length, list(eels$Stadium), mean)



# Plot HWHL to total length and calculate residuals
plot(eels$HWHL~eels$Length)
lm(eels$HWHL~eels$Length)
#Call:
#  lm(formula = eels$HWHL ~ eels$Length)
#Coefficients:
#  (Intercept)  eels$Length  
#0.2624438    0.0008713     intercept is 0.2624438 and slope 0.0008713: y = 0.0008713 + 0.2624438 (y = ax + b)

abline(0.2624438 , 0.0008713)
# or: abline(lm(eels$HWHL~eels$Length))




############################

#dotchart(eels$HWHL)
# Outlier can be observed > 0.40
# Remove this and check if model with and without outlier differ significantly

#eels2 <- eels[!eels$HWHL > 0.40,]
#m2 <- lm(eels2$HWHL~eels2$Length)
#plot(eels2$HWHL~eels2$Length)
#abline(0.2617374 , 0.0008766)


############################





# Calculate expected HWHL for each eel
eels$HWHLexp <- (0.0008713 * eels$Length) + 0.2624438

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

#=========================================
# Plot residuals in relation to stadium
#=========================================

# make a named list for the location of the number of eels
eel_per_stadium <- eels %>% group_by(Stadium) %>% 
  summarise(n_eels = n_distinct(ID))
eels_per_stadium_list <- rep(0.15, nrow(eel_per_stadium))
names(eels_per_stadium_list) <- as.vector(eel_per_stadium$Stadium)
# create ggplot (cfr. styling earlier plot)
fig_residuals_stadium <- ggplot(eels, aes(x = Stadium,
                                          y = diff)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 0.15, by = 0.05)) +
  theme_minimal() +
  ylab("Residuals") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_stadium_list),
                y = eels_per_stadium_list,
                label = as.character(eel_per_stadium$n_eels)),
            col = 'black', size = 6) +
  xlab("Stage") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +   #t: top, r: right; b = bottom, l = left
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))
#ggsave(fig_residencies_canal_sections, file = './additionals/fig_residencies_canal_sections.png')
fig_residuals_stadium



aov <- aov(eels$diff ~ eels$Stadium)
plot(aov)
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)


