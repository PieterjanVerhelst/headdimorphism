# Analysis if number of NH and BH differ between maturation stadia

library(dplyr)

# Calculate number of NH and BH for every stadium
ct <- eels %>%
  group_by(Stadium, HW) %>%
  select(Stadium, HW) %>%
  summarise(count = n())


# Create barplot
# First put counts in a matrix
your.mat.data <- c(9, 3, 13, 26, 7, 14)

data <- matrix(data = your.mat.data, 
               nrow = 2,
               ncol = 3,
               byrow = TRUE)
colnames(data)=c("FIII","FIV","FV")
rownames(data)=c("NH","BH")


# Get the stacked barplot
barplot(data, col=colors()[c(29,554)] , border="white", space=0.04, font.axis=2, xlab="group")

# Grouped barplot
barplot(data, col=colors()[c(29,554)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="Stadium", ylab = "Count", ylim=c(0,30), font.lab=2)

# Apply Poisson GLM --> count data
glm <- glm(count ~ HW/Stadium,family=poisson(link="log"), data=ct)
summary(glm)



