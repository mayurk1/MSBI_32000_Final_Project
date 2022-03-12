# Save data file to same directory or update path below:
colonData <- read.table("colon.txt", header = TRUE)

# Grabs Reoccurance Data from colonData
studyA = subset(colonData, etype==1)

# Grabs Death Data from colonData
studyB = subset(colonData, etype==2)

# Pre/Post groups 
obsA <- subset(studyA, rx=="Obs")
levA <- subset(studyA, rx=="Lev")
combA <- subset(studyA, rx=="Lev+5FU")

obsB <- subset(studyB, rx=="Obs")
levB <- subset(studyB, rx=="Lev")
combB <- subset(studyB, rx=="Lev+5FU")


# Bar Plots for Regional Extent

submucosaAobs <- sum(studyA["rx"] == "Obs" & studyA["extent"] == "1")*100/sum(studyA$study==1)
submucosaAlev <- sum(studyA["rx"] == "Lev" & studyA["extent"] == "1")*100/sum(studyA$study==1)
submucosaAlev5FU <- sum(studyA["rx"] == "Lev+5FU" & studyA["extent"] == "1")*100/sum(studyA$study==1)

muscleAobs <- sum(studyA["rx"] == "Obs" & studyA["extent"] == "2")*100/sum(studyA$study==1)
muscleAlev <- sum(studyA["rx"] == "Lev" & studyA["extent"] == "2")*100/sum(studyA$study==1)
muscleAlev5FU <- sum(studyA["rx"] == "Lev+5FU" & studyA["extent"] == "2")*100/sum(studyA$study==1)

serosaAobs <- sum(studyA["rx"] == "Obs" & studyA["extent"] == "3")*100/sum(studyA$study==1)
serosaAlev <- sum(studyA["rx"] == "Lev" & studyA["extent"] == "3")*100/sum(studyA$study==1)
serosaAlev5FU <- sum(studyA["rx"] == "Lev+5FU" & studyA["extent"] == "3")*100/sum(studyA$study==1)

contiguousAobs <- sum(studyA["rx"] == "Obs" & studyA["extent"] == "4")*100/sum(studyA$study==1)
contiguousAlev <- sum(studyA["rx"] == "Lev" & studyA["extent"] == "4")*100/sum(studyA$study==1)
contiguousAlev5FU <- sum(studyA["rx"] == "Lev+5FU" & studyA["extent"] == "4")*100/sum(studyA$study==1)

extentdf <- data.frame(Region = rep(c("Submucosa", "Muscle", "Serosa", "Contiguous Structures"), each=3), 
                       Treatment = rep(c("Obs", "Lev", "Lev+5FU"),4),
                       Percentage = c(submucosaAobs, submucosaAlev, submucosaAlev5FU, muscleAobs, muscleAlev, muscleAlev5FU,
                                      serosaAobs, serosaAlev, serosaAlev5FU, contiguousAobs, contiguousAlev, contiguousAlev5FU))

ggbarplot(extentdf, "Region", "Percentage", fill="Treatment", position=position_dodge())