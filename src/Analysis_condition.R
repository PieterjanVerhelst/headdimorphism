# Analyse if condition factor K  differs according to head width within maturity classes
# By Pieterjan Verhelst


library(PMCMR)

# First check if K differs according to maturation stadia
boxplot(eels$K~eels$Stadium, ylab = "K") 
aov <- aov(eels$K ~ eels$Stadium)
plot(aov)  # Check assumptions
summary(aov)

TukeyHSD(aov, conf.level=0.95, ordered = FALSE)  # sign difference between some groups, so nested design


# Calculate mean condition per head width class
aggregate(eels$K, list(eels$class), mean)
aggregate(eels$K, list(eels$class), sd)
aggregate(eels$K, list(eels$class), median)


# Create plot
boxplot(eels$K~eels$class, ylab = "K")
#boxplot(eels$K~eels$class/eels$Stadium, ylab = "K")  # In case of nested design

# Create elaborated boxplot with number of eels per head width class
# make a named list for the location of the number of eels
eel_per_class <- eels %>% group_by(class) %>% 
  summarise(n_eels = n_distinct(ID))
eels_per_class_list <- rep(1.80, nrow(eel_per_class))
names(eels_per_class_list) <- as.vector(eel_per_class$class)
# create ggplot (cfr. styling earlier plot)
fig_K_class <- ggplot(eels, aes(x = class,
                                    y = K)) +
  geom_boxplot() +
  scale_y_continuous(breaks = seq(0, 1.70, by = 0.1)) +
  theme_minimal() +
  ylab("Kn") +
  geom_text(data = data.frame(),
            aes(x = names(eels_per_class_list),
                y = eels_per_class_list,
                label = as.character(eel_per_class$n_eels)),
            col = 'black', size = 4) +
  scale_x_discrete(limits=c("NH","IH","BH")) +    # Changes oreder of plots
  xlab("Head width class") +
  theme(axis.title.y = element_text(margin = margin(r = 10))) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))
#ggsave(fig_residencies_canal_sections, file = './additionals/fig_residencies_canal_sections.png')
fig_K_class



##################
# Apply anova
##################

# Check assumptions:
# Normality if p-value > 0.05 (H0: normality)
shapiro.test((eels$K)^2)

# Homogeneity of variances
leveneTest(eels$K, eels$class)


aov <- aov(eels$K ~ eels$class)
#aov <- aov(eels$K ~ eels$class/eels$Stadium)     # Stadium is nested within class
plot(aov)  # Check assumptions
summary(aov)

# Apply post-hoc test
TukeyHSD(aov, conf.level=0.95, ordered = FALSE)


##################
# KRUSKAL-WALLIS
##################

kruskal.test(K~class, data=eels)

posthoc.kruskal.dunn.test(x=eels$K, g=eels$class, p.adjust.method="bonferroni")

PT = dunnTest(migration_distance ~ Month,
              data=dist,
              method="bh")    # Can adjust p-values;
# See ?p.adjust for options








# Remove outliers
c <- eels[which(eels$class == "NH"),]

x <- c(
1.6080927,
0.5160324,
0.6338795,
1.4124529,
1.3933317,
1.3470251,
1.3418751,
0.5352319,
0.5502950,
0.6337044,
0.7194591,
0.7200877,
0.7260090,
1.5027151,
0.5149608)

eels2 <- eels[!eels$Kround %in% x,]


boxplot(eels2$K~eels2$class, ylab = "K")
# Apply anova
aov <- aov(eels2$K ~ eels2$class)
#aov <- aov(eels$K ~ eels$class/eels$Stadium)     # Stadium is nested within class
plot(aov)  # Check assumptions
summary(aov)

# Apply post-hoc test
TukeyHSD(aov, conf.level=0.95, ordered = FALSE)




