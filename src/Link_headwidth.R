# Link head width data to dataset
# By Pieterjan Verhelst

library(dplyr)

# Upload eel data
hw <- read.csv("./data/raw/Headdimorphism.csv",sep=";",stringsAsFactors = FALSE)

# Select relevant columns, substitute ',' by '.' and set as numeric
hw <- select(hw, ID, Width.skull, Head.length)
hw$Width.skull <- gsub("\\,", ".", hw$Width.skull)
hw$Width.skull <- as.numeric(hw$Width.skull)
hw$Head.length <- gsub("\\,", ".", hw$Head.length)
hw$Head.length <- as.numeric(hw$Head.length)

# Rename columns
colnames(hw)[2] <- "HW"   # Width.skull is head width = HW
colnames(hw)[3] <- "HL"   # Head.length is head length = HL

# Calculate ratio HW:HL
hw$HWHL <- hw$HW/hw$HL

# Merge two datasets
eels <- merge(eels, hw, by="ID")

# Remove columns
eels$lengte..mm. <- NULL
eels$voor.oog..mm.<- NULL
eels$achter.oog..mm. <- NULL

# Change column name
colnames(eels)[15] <- "Before_eye"
colnames(eels)[16] <- "Behind_eye"
colnames(eels)[17] <- "HW"

# Substitute comma by dot in weight column
eels$Before_eye<-gsub(",", ".", eels$Before_eye)
eels$Behind_eye<-gsub(",", ".", eels$Behind_eye)

# Set weight and index as numeric
eels$Before_eye <- as.numeric(eels$Before_eye)
eels$Behind_eye <- as.numeric(eels$Behind_eye)

