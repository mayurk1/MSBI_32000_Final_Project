# Run once and provide Personal Access Token - this is just if you want GitHub for your R file
if (FALSE){
  install.packages("gitcreds")
  library(gitcreds)
  gitcreds_set()
}

# Save data file to same directory or update path below:
colonData <- read.table("colon.txt", header = TRUE)

# Grabs Reoccurance Data from colonData
studyA <- subset(colonData, etype==1)

# Grabs Death Data from colonData
studyB <- subset(colonData, etype==2)

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
library(survminer)
# Survival curve by treatment
plot(survfit(Surv(time, status) ~ xnum, data=studyA))

plot(survfit(Surv(years, status) ~ xnum, data=studyA),
     ylab= "Proportion with no Reoccurance",
     xlab = "Years",
     title = "Reoccurance Rate by Treatment",
     col = c(4,2,3)
)

plot(survfit(Surv(years, status) ~ xnum, data=studyB),
     ylab= "Proportion with no Death",
     xlab = "Years",
     title = "Death Rate by Treatment",
     col = c(4,2,3)
)


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

# Logistic Regression - looks like a good result?
library(glm2)
glm.fit <- glm(status ~ rx, data=studyA, family=binomial)
summary(glm.fit)

# Cox Regression Model
# Cox Regression 
## 0.5 times less likely to get reoccuurance?
resCoxA <- coxph(Surv(time, status) ~ rx, data=subData)
summary(resCoxA)

# Overall survival curve
ggsurvplot(survfit(resCoxA, data=subData ~rx),
           ggtheme = theme_minimal())

ggsurvplot(survfit(resCoxA, data=studyA), color = "#2E9FDF",
           ggtheme = theme_minimal())

# Survival Curve by treatment - doesnt work
subData <- subset(studyA, rx == "Obs" | rx == "Lev+5FU")
km_trt_fit <- survfit(Surv(time, status) ~ rx, data=subData)
autoplot(km_trt_fit)


# Survival Probability Curves
library(rms)
survplot(kmByRx)

## Plot the baseline survival function
res.cox <- coxph(Surv(time, status) ~ rx, data=studyA)
summary(res.cox)

coxModelA = coxph(Surv(time, status) ~ rx, data=studyA)
summary(coxModelA)

coxph(Surv(time, status) ~ rx, data=studyA) %>% 
  gtsummary::tbl_regression(exp = TRUE)

coxph(Surv(time, status) ~ rx, data=studyB)

coxph(Surv(time, status) ~ rx, data=studyB) %>% 
  gtsummary::tbl_regression(exp = TRUE)


#KM plot add this in addition to survival curves




