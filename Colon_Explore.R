# Run once and provide Personal Access Token
#install.packages("gitcreds")
#library(gitcreds)
#gitcreds_set()

# Save data file to same directory or update path below:
colonData <- read.table("colon.txt", header = TRUE)

# Grabs every first value from colonData
#studyA = na.omit(colonData[c(TRUE, FALSE),])
studyA = colonData[c(TRUE, FALSE),]

# Grabs every second value from colonData
#studyB = na.omit(colonData[c(FALSE, TRUE),])
studyB = colonData[c(FALSE, TRUE),]

sum(is.na(studyA$time))
#str(subset(colonData, etype=1, rx=="Obs"))
library(tidyverse)
map(studyA, ~sum(is.na(.)))
map(studyB, ~sum(is.na(.)))

# EDA - Input variable here to run EDA
if (FALSE){
  dataToUse <- combB
  library(DataExplorer)
  introduce(dataToUse)
  create_report(dataToUse)
}

# Stats summary
if (FALSE){
  library(skimr)
  skim(combA)
  skim(obsA)
}

# Pre/Post groups 
obsA <- subset(studyA, rx=="Obs")
levA <- subset(studyA, rx=="Lev")
combA <- subset(studyA, rx=="Lev+5FU")

obsB <- subset(studyB, rx=="Obs")
levB <- subset(studyB, rx=="Lev")
combB <- subset(studyB, rx=="Lev+5FU")

str(combB)
str(obsB)

# Basically seems as though the data is split on death and reoccruence, but the status variable 
# dicating the censoring status is the same for both? Anyways study might be comparing the status rate 
# vs the treatment type
summary(obsB)
summary(combB)

t.test(obsB$status, combB$status)
#t.test(obBA$nodes, combB$nodes)


# Notes
# split up based on pre vs post 
# can look at just one sample, look at deaths compared to treatment and can just chi sq vs t test, lead up log regression, 


# Regression analysis 
library(tidyverse)
library(MASS)



