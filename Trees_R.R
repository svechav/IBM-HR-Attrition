emp=data.frame(PCA_Data)
library(plyr)
Department
EducationField
Gender
JobRole
str(emp)
library(dummies)
install.packages('dummies')
new_emp=subset(emp,select=-c(Attrition))
new_emp_data=dummy.data.frame(new_emp,names=c("BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","Over18","OverTime"))
View(new_emp_data)
str(new_emp_data)
new_emp_data=plyr::rename(new_emp_data,c("EducationFieldHuman Resource"="EducationFieldHumanResource"))
str(new_emp_data)
emp_metadata=write.csv(str(new_emp))
pc_comp=prcomp(new_emp_data,scale.=T)
pc_comp=prcomp(subset(new_emp_data,select=c(ï..Age,DailyRate,BusinessTravelNon_Travel,BusinessTravelTravel_Frequently,
                                             BusinessTravelTravel_Rarely,DailyRate,DepartmentHumanResources,DepartmentResearchandDevelopment,DepartmentSales,DistanceFromHome,
                                             Education),scale.=T))

df <- emp

#Set the fractions of the dataframe you want to split into training, 
# validation, and test.
fractionTraining   <- 0.60
fractionValidation <- 0.20
fractionTest       <- 0.20

# Compute sample sizes.
sampleSizeTraining   <- floor(fractionTraining   * nrow(df))
sampleSizeValidation <- floor(fractionValidation * nrow(df))
sampleSizeTest       <- floor(fractionTest       * nrow(df))

# Create the randomly-sampled indices for the dataframe. Use setdiff() to
# avoid overlapping subsets of indices.
indicesTraining    <- sort(sample(seq_len(nrow(df)), size=sampleSizeTraining))
indicesNotTraining <- setdiff(seq_len(nrow(df)), indicesTraining)
indicesValidation  <- sort(sample(indicesNotTraining, size=sampleSizeValidation))
indicesTest        <- setdiff(indicesNotTraining, indicesValidation)

# Finally, output the three dataframes for training, validation and test.
dfTraining   <- df[indicesTraining, ]
dfValidation <- df[indicesValidation, ]
dfTest       <- df[indicesTest, ]

library(rpart)
attach(dfTraining)
c.tree_training=rpart(Attrition~BusinessTravel+Department+Education+EducationField+EnvironmentSatisfaction+Gender+JobInvolvement+JobLevel+JobRole+JobSatisfaction+MaritalStatus+OverTime+PerformanceRating+RelationshipSatisfaction+WorkLifeBalance+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8,data=dfTraining,method="class")
pfit<- prune(c.tree_training, cp=c.tree_training$cptable[which.min(c.tree_training$cptable[,"xerror"]),"CP"],dfValidation)
Prediction <- predict(c.tree_training, dfTest, type = "class")
plot1=plot(c.tree_training, uniform=TRUE, 
     main="Regression Tree for Training")
plot2=text(c.tree_training, use.n=TRUE, all=TRUE, cex=.8)

