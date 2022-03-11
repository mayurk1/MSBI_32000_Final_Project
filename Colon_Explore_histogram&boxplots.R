# Save data file to same directory or update path below:
colonData <- read.table("colon.txt", header = TRUE)

# Grabs every first value from colonData
studyA = na.omit(colonData[c(TRUE, FALSE),])

# Grabs every second value from colonData
studyB = na.omit(colonData[c(FALSE, TRUE),])

# EDA - Input variable here to run EDA
dataToUse <- obsA

library(DataExplorer)
introduce(dataToUse)
create_report(dataToUse)

# Pre/Post groups 
obsA <- subset(studyA, rx=="Obs")
levA <- subset(studyA, rx=="Lev")
combA <- subset(studyA, rx=="Lev+5FU")

obsB <- subset(studyB, rx=="Obs")
levB <- subset(studyB, rx=="Lev")
combB <- subset(studyB, rx=="Lev+5FU")


t.test(studyA$time, studyB$time)

t.test(obsA$nodes, obsB$nodes)


# Notes
#split up based on pre vs post 
# can look at just one sample, look at deaths compared to treatment and can just chi sq vs t test, lead up log regression, 


#histograms

plot_histogram(obsA)
plot_histogram(levA)
plot_histogram(combA)

plot_histogram(obsB)
plot_histogram(levB)
plot_histogram(combB)


#boxplots



plot_boxplot(obsA, by = "time")
plot_boxplot(levA)
plot_boxplot(combA)

plot_boxplot(obsB)
plot_boxplot(levB)
plot_boxplot(combB)


#boxplots of original colondata by treatment and etype

colonobs <- subset(colonData, rx=="Obs")
colonlev <- subset(colonData, rx=="Lev")
coloncomb <- subset(colonData, rx=="Lev+5FU")

plot_boxplot(colonobs, by = "etype")
plot_boxplot(colonlev, by = "etype")
plot_boxplot(coloncomb, by = "etype")

