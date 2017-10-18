# Link head width data to dataset
# By Pieterjan Verhelst


# Upload eel data
hw <- read.csv("~/Doctoraat/Kopbreedte/Headdimorphism.csv",sep=";",stringsAsFactors = FALSE)

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

