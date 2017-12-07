# Analysis relationship between head width and onset of migration
# By Pieterjan Verhelst


# Count number of NH and BH according to onset month
ct2 <- eels2 %>%
  group_by(Month, class) %>%
  select(Month, class) %>%
  summarise(count = n())

# Create barplot
# First put counts in a matrix
ct2
your.mat.data <- c(0,0,0,0,4,0,  1,0,5,4,13,1,  0,1,2,0,4,1)

data <- matrix(data = your.mat.data, 
               nrow = 3,
               ncol = 6,
               byrow = TRUE)
colnames(data)=c("4","5","8","9","10","11")
rownames(data)=c("NH", "inter", "BH")


# Grouped barplot
barplot(data, col=colors()[c(29,300,554)] , border="white", font.axis=2, beside=T, legend=rownames(data), xlab="Month", ylab = "Count", ylim=c(0,15), font.lab=2)

