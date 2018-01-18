# Analyse if swim speed differs between head width classes (NH, IH, BH)
# By Pieterjan Verhelst


library(dplyr)
library(PMCMR)
library(nlme)


# Upload eel data
m <- read.csv("./data/raw/migration.csv",sep=",",stringsAsFactors = FALSE)

# Select Zeeschelde
m <- filter(m, network == "Zeeschelde")


# Select movement1 == 1 and realistic swim speeds
m1 <- filter(m, movement1 == 1)
summary(m1$swimspeed)
boxplot(m1$swimspeed)
m1 <- filter(m1, swimspeed <= 1.3120000)      # Take swim speed <= the max swim speed from the summary
m1 <- filter(m1, swimspeed >= 0.0002276)      # Take swim speed >= the min swim speed from the summary
summary(m1$swimspeed)
boxplot(m1$swimspeed)


# Extract head width classes
# Run 'Determine_headwidth.R' manually
hw <- select(eels, Zendernummer, class)
colnames(hw)[1] <- "Transmitter"
# Remove rows with NA
# i.e. eels with no Transmitter
hw <- na.omit(hw)
table(hw$class)

# Merge head width class (hw dataset) to swim speeds (m1 dataset)
m1$Transmitter<-gsub("A69-1601-","",m1$Transmitter)
m2 <- merge(m1, hw, by="Transmitter")

# Create boxplot
boxplot(m2$swimspeed~m2$class, ylab = "Swim speed (m/s)") 
# Create elaborated boxplot with number of eels per head width class
# make a named list for the location of the number of eels
eel_per_class <- m2 %>% group_by(class) %>% 
  summarise(n_eels = n_distinct(Transmitter))
eels_per_class_list <- rep(1.5, nrow(eel_per_class))
names(eels_per_class_list) <- as.vector(eel_per_class$class)
# create ggplot (cfr. styling earlier plot)
fig_swimspeed <- ggplot(m2, aes(x = class,
                                 y = swimspeed)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1.3, by = 0.1)) +
  theme_minimal() +
  ylab("Migration speed (m/s)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_class_list),
                y = eels_per_class_list,
                label = as.character(eel_per_class$n_eels)),
            col = 'black', size = 4) +
  xlab("Head width class") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
#ggsave(fig_residencies_canal_sections, file = './additionals/fig_residencies_canal_sections.png')
fig_swimspeed


# Calculate mean swim speed speed per head width class
aggregate(m2$swimspeed, list(m2$class), mean)
aggregate(m2$swimspeed, list(m2$class), sd)


## Conduct analysis
##################
# ANOVA
##################

aov <- aov(m2$swimspeed~m2$class)
plot(aov)  # Check assumptions
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)  # sign difference between some groups, so nested design


##################
# KRUSKAL-WALLIS
##################

kruskal.test(swimspeed~class, data=m2)
posthoc.kruskal.dunn.test(x=m2$swimspeed, g=m2$class, p.adjust.method="bonferroni")

##################
# Linear mixed effects model - Random intercept model
##################

m2$fTransmitter <- factor(m2$Transmitter)
Mlme1 <- lme(swimspeed ~ class, random = ~1 | fTransmitter,
             data = m2)
summary(Mlme1)
summary(Mlme1)$tTable[,"p-value"]
  
  
  