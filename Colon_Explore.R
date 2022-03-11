# Run once and provide Personal Access Token - this is just if you want GitHub for your R file
if (FALSE){
  install.packages("gitcreds")
  library(gitcreds)
  gitcreds_set()
}

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

# EDA - Input variable here to run EDA
if (FALSE){
  dataToUse <- combBs
  library(DataExplorer)
  introduce(dataToUse)
  create_report(dataToUse)
}

# Stats summary
if (FALSE){
  library(skimr)
  skim(combB)
}

# Temp - ignore
count(obsB, status)
count(combB, status)

# Describe Distributions
library(ggpubr)
# Box Plots
ggboxplot(studyA, x = "rx", y = "time", 
          #color = "rx", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Obs", "Lev", "Lev+5FU"),
          ylab = "Reoccurence", xlab = "Treatment")

ggboxplot(studyB, x = "rx", y = "time", 
          #color = "rx", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("Obs", "Lev", "Lev+5FU"),
          ylab = "Death", xlab = "Treatment")

# Mean Plots
ggline(studyA, x = "rx", y = "time", 
       add = c("mean_se", "jitter"), 
       order = c("Obs", "Lev", "Lev+5FU"),
       #color = "rx", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
       ylab = "Reoccurence", xlab = "Treatment")

ggline(studyB, x = "rx", y = "time", 
       add = c("mean_se", "jitter"), 
       order = c("Obs", "Lev", "Lev+5FU"),
       #color = "rx", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
       ylab = "Death", xlab = "Treatment")


# Kaplan-Meier Survival Plot (Survival Rate Over Time)
library(survival)
plot(survfit(Surv(time, status) ~ rx, data=studyA))


# Calculate Percent Difference
recA = sum(obsA$status==1)
recAC = sum(combA$status==1)
rA = recA/315
rAC = recAC/304
# Percent diff. 35% vs paper 40%
difference = (rA-rAC)/((rA+rAC)/2)
difference


# T test proves sig. diff. in reoccurrence rates between obs and comb
# treatment groups
t.test(obsA$status, combA$status)

# T test also proves sig. diff. in death rates between obs and comb
# treatment groups
t.test(obsB$status, combB$status)


# Cox Regression Model
coxph(Surv(time, status) ~ rx, data = studyB)

coxph(Surv(time, status) ~ rx, data = studyB) %>% 
  gtsummary::tbl_regression(exp = TRUE)


# ANOVA - iffy on this
## Compute the analysis of variance
res.aov <- aov(time ~ rx, data = studyA)
TukeyHSD(res.aov)
## Residuals
plot(res.aov, 1)
## QQ - lol not normal
plot(res.aov, 2)

## Compute the analysis of variance
res.aov <- aov(time ~ rx, data = studyB)
TukeyHSD(res.aov)
## Residuals
plot(res.aov, 1)
## QQ - lol not normal
plot(res.aov, 2)





