# Determine eel condition
# by Pieterjan Verhelst

library(dplyr)
library(ggplot2)
library(lubridate)


rm(list = ls())


# Upload eel data
eels <- read.csv("./data/raw/Eels.csv",sep=";",stringsAsFactors = FALSE)

# Set stadium as factor
#eels$Stadium <- factor(eels$Stadium)

eels$Stadium<- factor(eels$Stadium,
                     levels = c("I","FII","FIII","FIV","FV","MII"),
                     ordered = TRUE)


# Change column names
colnames(eels)[4] <- "Length"
colnames(eels)[5] <- "Weight"
colnames(eels)[6] <- "Eye_vert"
colnames(eels)[7] <- "Eye_hor"
colnames(eels)[8] <- "Pectoral_fin"


# Remove rows with NA
# i.e. eels which were too small to measure
eels <- eels[which(eels$Weight!="NA"), ]

# Set Length as numeric
eels$Length <- as.numeric(eels$Length)

# Set column 'Length' in cm
eels$Length <- eels$Length / 10

# Substitute comma by dot in weight column
eels$Weight<-gsub(",", ".", eels$Weight)
eels$Index<-gsub(",", ".", eels$Index)

# Set weight and index as numeric
eels$Weight <- as.numeric(eels$Weight)
eels$Index <- as.numeric(eels$Index)

# Remove recaptured eels
eels <- eels[!eels$Hervangst %in% c("1"), ]

# Only take eels of Merelbeke into account
eels <- eels[eels$Vangstlocatie %in% c("RINGVAART (MERELBEKE)"), ]



