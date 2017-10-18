# Link data with onset of migration and migration duration to the dataset
# By Pieterjan Verhelst

library(dplyr
        )

# Upload duration and onset data
t<- read.csv("./data/external/duration.csv",sep=",",stringsAsFactors = FALSE)
o<- read.csv("./data/external/onset.csv",sep=",",stringsAsFactors = FALSE)
o <- select(o, Transmitter, Month, Year)

# Merge the two files
ot <- merge(o, t, by="Transmitter")
ot$X <- NULL
ot$Transmitter <- factor(ot$Transmitter)
ot$Transmitter <- gsub("A69-1601-","",ot$Transmitter,ignore.case=T)
ot$Transmitter <- factor(ot$Transmitter)

# Merge with eels dataset
colnames(eels)[12] <- "Transmitter"
eels$Transmitter <- factor(eels$Transmitter)

eels2 <- merge(eels, ot, by="Transmitter")

