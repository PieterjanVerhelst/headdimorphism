# Analyse if swim speed differs between head width classes (NH, IH, BH)
# By Pieterjan Verhelst


library(dplyr)
library(PMCMR)
library(nlme)
library(multcomp)


# Upload eel data
m <- read.csv("./data/raw/migration.csv",sep=",",stringsAsFactors = FALSE)

# Select Zeeschelde
m <- filter(m, network == "Zeeschelde")


# Extract head width classes
# Run 'Determine_headwidth.R' manually
hw <- select(eels, Zendernummer, class)
colnames(hw)[1] <- "Transmitter"
# Remove rows with NA
# i.e. eels with no Transmitter
hw <- na.omit(hw)
table(hw$class)

# Merge head width class (hw dataset) to swim speeds (m1 dataset)
m$Transmitter<-gsub("A69-1601-","",m$Transmitter)
m1 <- merge(m, hw, by="Transmitter")



# Eel 52657 had two phase migration due to its aborted migration which proceeded two months later
# Select last migration phase
ind52657 <- m1[which(m1$Transmitter == "52657"),]
ind52657 <- ind52657[order(as.POSIXct(strptime(ind52657$Arrival,"%Y-%m-%d %H:%M:%S"))),]
# Eel is considered migratory from station s-wetteren 2017-02-23 16:58:00
# Select those data
ind52657 <- ind52657[!ind52657$Arrival < '2017-02-23 16:58:00',]

# Now remove that eel from migration dataset
m1 <- m1[!m1$Transmitter %in% c("52657"), ]
# Bind individual dataset to migration dataset
m1 <- rbind(m1, ind52657)




# Calculate migration speed
# Calculate migration time and distance + plot migration time
mt = m1 %>%
  group_by(Transmitter, class)%>%
  select(Transmitter, class, Arrivalnum, Departurenum, Station_distance) %>%
  summarise( seconds=with(m1, max(Departurenum) - min(Arrivalnum)),
             dist= with(m1, max(Station_distance) - min(Station_distance))
  )


# Calculate tracking time in days
mt$days=mt$seconds/(60*60*24)
mt$days=round(mt$days, 2)
par(mar=c(6,4.1,4.1,2.1))
barplot(mt$days, names.arg=mt$Transmitter, cex.names=0.8, ylim=c(0,100),las=2)



# Calculate migration speed whole study area
mt$speed <- mt$dist / mt$seconds
mean(mt$speed)
sd(mt$speed)
min(mt$speed)
max(mt$speed)

# Summarise according to head width class
aggregate(mt$speed, list(mt$class), mean)
aggregate(mt$speed, list(mt$class), sd)
aggregate(mt$speed, list(mt$class), min)
aggregate(mt$speed, list(mt$class), max)


# Create boxplot
boxplot(mt$speed~mt$class, ylab = "Migration speed (m/s)") 
# Create elaborated boxplot with number of eels per head width class
# make a named list for the location of the number of eels
eel_per_class <- mt %>% group_by(class) %>% 
  summarise(n_eels = n_distinct(Transmitter))
eels_per_class_list <- rep(0.5, nrow(eel_per_class))
names(eels_per_class_list) <- as.vector(eel_per_class$class)
# create ggplot (cfr. styling earlier plot)
fig_speed <- ggplot(mt, aes(x = class,
                                 y = speed)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 0.4, by = 0.1)) +
  theme_minimal() +
  ylab("Migration speed (m/s)") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_class_list),
                y = eels_per_class_list,
                label = as.character(eel_per_class$n_eels)),
            col = 'black', size = 8) +
  scale_x_discrete(limits=c("NH","IH","BH")) +    # Changes oreder of plots
  xlab("Head width class") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20))
#ggsave(fig_residencies_canal_sections, file = './additionals/fig_residencies_canal_sections.png')
fig_speed




## Conduct analysis
##################
# ANOVA
##################

aov <- aov(mt$speed~mt$class)
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

mt$fTransmitter <- factor(mt$Transmitter)
Mlme1 <- lme(speed ~ class, random = ~1 | fTransmitter,
             data = mt)
summary(Mlme1)
summary(Mlme1)$tTable[,"p-value"]
# Conduct multiple comparisons (multcomp package) https://stats.stackexchange.com/questions/237512/how-to-perform-post-hoc-test-on-lmer-model
summary(glht(Mlme1, linfct = mcp(class = "Tukey")), test = adjusted("holm"))

  