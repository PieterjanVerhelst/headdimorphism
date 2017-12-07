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
hd$HWHL <- hd$HW/hw$HL

# Merge two datasets
eels <- merge(eels, hd, by="ID")


