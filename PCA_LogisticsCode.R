
emp=read.csv("HR-Employee-Attrition.csv")

#Analysis of Correlation between two variables

plot(cor(emp$ï..Age,emp$MonthlyIncome),col="Blue")
plot(emp$ï..Age,emp$MonthlyIncome,col="Blue")

library("car")
install.packages("car")
scatterplotMatrix(tab[2,34])
scatterplotMatrix(emp$BusinessTravel,emp$Education)
tab=read.table("HR-Employee-Attrition.csv",sep=",")
str(emp)
pairs(emp[, -c(2,3,5,8,12,16,18,19,20,21,22,23,24,25,26,28,29,30,31,32,33)])
pairs(emp[c(24,25,26)],bg = c("red", "green3", "blue"),pch=21)

cor_var=sapply(emp,class)
cor_var1=names(cor_var[cor_var=="integer" | cor_var=="numeric"])
cor_var1=cor_var1[-5]
cor_var1=cor_var1[-18]
cor(emp[cor_var1])

#Analysis of montlhy income on the basis of education Field,Department,Gender
aggregate(emp$MonthlyIncome~emp$EducationField,FUN=mean)
aggregate(emp$MonthlyIncome~emp$Department,FUN=mean)
aggregate(emp$MonthlyIncome~emp$Department,FUN=mean)
aggregate(emp$MonthlyIncome~emp$Gender,FUN=mean)
aggregate(emp$MonthlyIncome~emp$JobRole,FUN=mean)
library(plyr)



#***********PCA Process*********

setwd("C:/Users/ravis/Desktop/Uconn/PMD/Project")
pwd=getwd()
data=read.csv("Employee-Attrition.csv")
View(data)
str(data)
num_var=sapply(data,class)
num_var1=names(num_var[num_var=="integer" | num_var=="numeric"])

#Including only continous variables for PCA analyis

pr_cmp_data=subset(data[num_var1],select=-c(StandardHours,EmployeeCount,PredFormulaYearsWithCurrManager,Education,EmployeeNumber,EnvironmentSatisfaction,JobInvolvement,JobLevel,JobSatisfaction,PerformanceRating,RelationshipSatisfaction,
                                            HourlyRate,WorkLifeBalance))
colnames(pr_cmp_data)

#We are saving the data for continous variables in a new file EmpIntegerCol.csv

#write.csv(pr_cmp_data,"Emp_data.csv")

#Perform PCA on continous variables

prin_comp=prcomp(pr_cmp_data,scale = TRUE)
names(prin_comp)


str(pr_cmp_data)
View(pr_cmp_data)

write.csv(pr_cmp_data,"EmpIntegerCol.csv")
prin_comp$center
prin_comp$rotation
prin_comp$x
Eigenvector=prin_comp$rotation
write.csv(Eigenvector,"Eigenvector.csv")
nm="bipot"
jpeg(filename = paste0(pwd, "/graphs/PCA_", nm, ".jpg"))
biplot(prin_comp,scale=0,arrow.len=0.1,main = "Biplot of PCA")



#Calculate Standard deviation for each principal components.

std_dev=prin_comp$sdev

#Calculate variance for each principal components.

Eigen_value=std_dev^2
Eigen_value
principal_component=c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12","PC13","PC14")
pc_mat=data.frame(principal_component,Eigen_value)
write.csv(pc_mat,"Eigen_value.csv")

#So, higher is the explained variance, higher will be the information contained in those components.
#To compute the proportion of variance explained by each component, we simply divide the variance by sum of total variance


prop_var=Eigen_value/sum(Eigen_value)
prop_var=prop_var*100

#decide how many components should we select for modeling stage 

#cumulative scree plot

plot(cumsum(prop_var),main="Eigen Value Cumulative Percent",xlab="Principal Components",cex=1,pch=5,col="red",lwd=2,ylab="Cumulative proportion of variance explained",type="b")
ncol(pr_cmp_data)
abline(h=85)
abline(v=8)

#PCA components Data
#remove unwanted PCA components
Prim_comp=subset(prin_comp$x,select=-c(PC9,PC10,PC11,PC12,PC13,PC14))
write.csv(Prim_comp,"Principal_Component.csv")
write.csv(cor(Prim_comp),"CorrelationAmongVar.csv")

#Replace the old variables with PCA variables and save the data in a new file 

Normalised_data=cbind(subset(data,select=-c(Age,DailyRate,DistanceFromHome,MonthlyIncome,MonthlyRate,NumCompaniesWorked,PercentSalaryHike,StockOptionLevel,TotalWorkingYears,TrainingTimesLastYear,
                          YearsAtCompany,YearsInCurrentRole,YearsSinceLastPromotion,YearsWithCurrManager,PredFormulaYearsWithCurrManager)),Prim_comp)
write.csv(Normalised_data,"PCA_Data.csv")




#*********** New Dummy Data Check***************
new_emp=read.csv("PCA_Data.csv")
library(dummies)
new_emp_data=dummy.data.frame(new_emp,names=c("BusinessTravel","Department","EducationField","Gender","JobRole","MaritalStatus","Over18","OverTime"))
write.csv(new_emp_data,"Emp_data_split.csv")



#*****Stepwise Logistic Regression****


new_emp_data=read.csv("Emp_data_split.csv")
smp_size <- floor(0.70 * nrow(new_emp_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(new_emp_data)), size = smp_size)
train <- new_emp_data[train_ind, ]
validation <- new_emp_data[-train_ind, ]
str(new_emp_data)
model=step(glm(Attrition~+BusinessTravelNon_Travel+BusinessTravelTravel_Frequently+BusinessTravelTravel_Rarely+DepartmentHumanResources+DepartmentResearchDevelopment+DepartmentSales+Education+EducationFieldHumanResources+EducationFieldLifeSciences+EducationFieldMarketing+EducationFieldMedical+EducationFieldOther+EducationFieldTechnicalDegree+EmployeeCount+EmployeeNumber+EnvironmentSatisfaction+GenderFemale+GenderMale+JobInvolvement+JobLevel+JobRoleHealthcareRepresentative+JobRoleHumanResources+JobRoleLaboratoryTechnician+JobRoleManager+JobRoleManufacturingDirector+JobRoleResearchDirector+JobRoleResearchScientist+JobRoleSalesExecutive+JobRoleSalesRepresentative+JobSatisfaction+MaritalStatusDivorced+MaritalStatusMarried+MaritalStatusSingle+OverTimeNo+OverTimeYes+PerformanceRating+RelationshipSatisfaction+StandardHours+WorkLifeBalance+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8
,data=train,family=binomial("logit")),direction="both")

summary(model)
#Running the model on training dataset
predict=predict(model,type='response')
train$Prediction=ifelse(predict>0.5,"Yes","No")
View(train)

#Confusion Matrix
table(train$Attrition,train$Prediction)

#ROC Curve
install.packages("ROCR")
library(ROCR)
?prediction
ROCRpred <- prediction(predict, train$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
install.packages("pROC")
library(PROC)

#AUC Value
train$Org=ifelse(train$Attrition=="Yes",1,0)
train$Pred=ifelse(train$Prediction=="Yes",1,0)
pROC::auc(train$Org,train$Pred)


#***** Running the model on validation data set ***********
colnm=colnames(new_emp_data)
colnm
validation_res <- predict(model,newdata=subset(validation,select=c(BusinessTravelNon_Travel,BusinessTravelTravel_Frequently,BusinessTravelTravel_Rarely,DepartmentHumanResources,DepartmentResearchDevelopment,
                                                                   DepartmentSales,Education,EducationFieldHumanResources,EducationFieldLifeSciences,EducationFieldMarketing,EducationFieldMedical,EducationFieldOther,EducationFieldTechnicalDegree,EmployeeCount,EmployeeNumber,EnvironmentSatisfaction,GenderFemale,GenderMale,JobInvolvement,JobLevel,JobRoleHealthcareRepresentative,JobRoleHumanResources,JobRoleLaboratoryTechnician,JobRoleManager,JobRoleManufacturingDirector,JobRoleResearchDirector,JobRoleResearchScientist,JobRoleSalesExecutive,JobRoleSalesRepresentative,JobSatisfaction,MaritalStatusDivorced,MaritalStatusMarried,MaritalStatusSingle,OverTimeNo,OverTimeYes,PerformanceRating,
                                                                   RelationshipSatisfaction,StandardHours,WorkLifeBalance,PC1,PC2,PC3,PC4,PC5,PC6,PC7,PC8)),type='response')
																   
#Confusion Matrix
View(train)
validation$Prediction=ifelse(validation_res>0.5,"Yes","No")
table(validation$Attrition,validation$Prediction)

#ROC Curve
predict=predict(model,type='response')
ROCRpred <- prediction(validation_res,validation$Attrition)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))

#AUC Value
library(PROC)
validation$Org=ifelse(validation$Attrition=="Yes",1,0)
validation$Pred=ifelse(validation$Prediction=="Yes",1,0)
pROC::auc(validation$Org,validation$Pred)

