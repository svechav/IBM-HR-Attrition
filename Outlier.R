outlier=read.csv("Outlier.csv")
plot(outlier$TotalWorkingYears, outlier$ï..Age , col="black")
boxplot(outlier$TotalWorkingYears , xlab="Total Working years" )
hist(outlier$TotalWorkingYears)


plot(outlier$YearsAtCompany, outlier$YearsWithCurrManager, xlab = "YearsAtCompany" , ylab="YearsWithCurrentManager")
regression=lm(formula =YearsWithCurrManager ~YearsAtCompany , data=outlier)
abline(regression, lwd=2, col="red")



