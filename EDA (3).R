rm(list = ls())

pwd <- getwd()
setwd(pwd)
dir.create(paste0(pwd,"/output"))

#Read the data
data <- read.csv("data_csv.csv", header = TRUE, sep = ",")

#Define the dependent variable
y_var = "Attrition"

#Create 1/0 Flag based on Attrition
data$y_var[data$Attrition == "Yes"] <- 1
data$y_var[data$Attrition == "No"] <- 0

#Use psych package to get sumamry statistics
install.packages("psych")
library(psych)

library(plyr)
library(reshape2)

summ <- as.data.frame(NULL)

cont_summary = function(x_var, nm, y_var)
{
  #Get summary
  t <- describe(x_var)
  
  #Create a column with variable name 
  t$name <- nm
  
  #Reorder columns
  t <- t[, c(14, 2:13)]
  
  #Quantiles
  qt <- as.data.frame(t(quantile(t(x_var))))
  
  #merge the two datasets
  t <- merge(t, qt)
  
  #Missing value
  t <- merge(t, sum(is.na(x_var)))
  
  colnames(t)[19] <- "missing"
  
  #Append to master summary dataset
  #Create a global data frame
  assign("summ", rbind(summ, t), envir = .GlobalEnv)
  
  
  #Produce Histogram and save it on disk
  jpeg(filename = paste0(pwd, "/output/Histogram_", nm, ".jpg"))
  hist(t(x_var), c = "royalblue1", main = paste0("Histogram of ", nm), xlab = nm)
  dev.off()
  
  #Produce a boxplot and separate observations with 1.5 times interquartile range
  jpeg(filename = paste0(pwd, "/output/Boxplot_", nm, ".jpg"))
  boxplot(x_var, main = paste0("Boxplot: ", nm), xlab = nm, col = "royalblue1", range = 1.5)
  dev.off()
  
  #Scatterplot by levels of attrition and and save it on disk
  jpeg(filename = paste0(pwd, "/output/Scatterplot_", nm, ".jpg"))
  plot(as.vector(t(x_var)), pch = c(21, 22), bg=c("red", "blue")[unclass(as.factor(y_var))], 
       main = paste0("Distribution of ", nm), xlab = nm, ylab = "value")
  legend(x = "topright", legend = c("Not Attrited", "Attrited"), pch=c(21,22), col = c("red", "blue"), fill = c("red", "blue"))
  dev.off()
  
  #Create bins and draw bar plots
  
  #5 Equal class intervals
  t <- as.data.frame(cut(t(x_var), breaks=5))
  t <- cbind(t, y_var)
  
  #Rename the columns
  colnames(t)[1] <- "Bucket"
  colnames(t)[2] <- "Attrited"
  
  dbar <- ddply(t, c("Bucket"), summarise,
                 N    = length(as.numeric(Attrited)),
                 Attrition = mean(as.numeric(Attrited))
  )
  
  #assign("dbar", dbar, envir = .GlobalEnv)
  jpeg(filename = paste0(pwd, "/output/Barplot_", nm, ".jpg"))
  barplot(dbar$Attrition, names.arg = dbar$Bucket, main = "Distribution by Bucket", xlab = nm, ylab = "Attrition Rate", col = "royalblue1", las = 2, cex.names=0.8)
  lines(dbar$Attrition, lwd = 2, col = "red")
  dev.off()
  
}

#Write Summary to a csv file
write.csv(summ, paste0(pwd, "/output/Continuous_Vars_Summary.csv"))

summ_cat <- as.data.frame(NULL)

#Categorical Variable Summary
cat_summary <- function(x_var, nm, y_var)
{
  
  #Calculate number of levels and missing values
  t <- as.data.frame(cbind(sum(is.na(x_var)), length(t(unique(x_var)))))
  colnames(t)[1] = "Missing"
  colnames(t)[2] = "levels"
  
  #Create variable name column
  t$name <- nm
  
  #Reorder columns
  t <- t[, c(3, 1:2)]
  
  #Append to master summary dataset
  #Create a global data frame
  assign("summ_cat", rbind(summ_cat, t), envir = .GlobalEnv)
  
  #Create barplots
  dbar <- as.data.frame(cbind(x_var, y_var))
  dbar <- ddply(dbar, c(nm), summarise, N = length(y_var), Attrition = mean(y_var))
  
  jpeg(filename = paste0(pwd, "/output/Barplot_", nm, ".jpg"))
  #Set the margins
  par(mar = c(8.1, 4.1, 4.1, 2.1))
  barplot(dbar$Attrition, names.arg = paste0(t(dbar[nm]), "(", t(dbar["N"]), ")"), main = paste0("Distribution of ", nm), ylab = "Attrition Rate", col = "royalblue1", ylim = c(0, max(dbar$Attrition)+0.1), las=2, cex.names=0.8)
  lines(dbar$Attrition, lwd = 2, col = "red")
  dev.off()
}

#Change the classes of few columns to treat them as categorical and not continuous
data$Education = as.factor(data$Education)
data$EnvironmentSatisfaction = as.factor(data$EnvironmentSatisfaction)
data$JobInvolvement = as.factor(data$JobInvolvement)
data$JobLevel = as.factor(data$JobLevel)
data$JobSatisfaction = as.factor(data$JobSatisfaction)
data$PerformanceRating = as.factor(data$PerformanceRating)
data$RelationshipSatisfaction = as.factor(data$RelationshipSatisfaction)
data$StockOptionLevel = as.factor(data$StockOptionLevel)
data$WorkLifeBalance = as.factor(data$WorkLifeBalance)


#Remove varaibles not relevant to us
drops <- c("EmployeeCount", "EmployeeNumber", "Over18", "StandardHours")
data <- data[, !(names(data) %in% drops)]

#Define continuous variables: integer class
cont_vars <- sapply(data, class)
cont_vars <- names(cont_vars[cont_vars == "integer"])

#Define categorical variables: factor class
cat_vars <- sapply(data, class)
cat_vars <- names(cat_vars[cat_vars == "factor"])
cat_vars <- cat_vars[cat_vars != y_var]

#Call Summary Function for continuous variables
for (i in (1 : length(cont_vars)))
{
  cont_summary(data[cont_vars[i]], cont_vars[i], data$y_var)
}

#Write Summary to a csv file
write.csv(summ, paste0(pwd, "/output/Continuous_Vars_Summary.csv"))

#Call Summary Function for categorical variables
for (i in (1 : length(cat_vars)))
{
  cat_summary(data[cat_vars[i]], cat_vars[i], data$y_var)
}
write.csv(summ_cat, paste0(pwd, "/output/Categorical_Vars_Summary.csv"))


#Multivariate Outlier Detection - Using Mahalanobis Distance
m_dist <- mahalanobis(data[cont_vars], colMeans(data[cont_vars]), cov(data[cont_vars]))

#Filter out top 1% observations as outliers
#Using Robust Mahalanobis many observations are being removed. So 1% is removed 
m_dist_sort <- sort(m_dist, decreasing = TRUE) 
m_dist <- as.data.frame(m_dist >= m_dist_sort[round(nrow(data)*0.01)])
data <- cbind(data, m_dist)
colnames(data)[33] = "outl"

#outl <- aq.plot(data[cont_vars], delta=qchisq(0.95, df=length(cont_vars)), quan=1/2, alpha=0.05)

#Correlation between pair of continuous variables

#Pearson Correlation
cor_var <- merge(cont_vars, cont_vars)

for (i in (1:length(cont_vars)^2))
{
  cor_var$pcorr[i] <- cor(data[as.character(cor_var[i, 1])], data[as.character(cor_var[i, 2])], method = "pearson")
}
#Cutoff of 0.7 chosen
high_corr_p <- cor_var[abs(cor_var$pcorr) > 0.7, ]
high_corr_p <- high_corr_p[high_corr_p$x != high_corr_p$y, ]
high_corr_p <- high_corr_p[!duplicated(high_corr_p[, 3]), ]

write.csv(high_corr_p, paste0(pwd, "/output/Pearson_Correlation.csv"))

#Spearman Correlation 
cor_var <- merge(cont_vars, cont_vars)

for (i in (1:length(cont_vars)^2))
{
  cor_var$scorr[i] <- cor(data[as.character(cor_var[i, 1])], data[as.character(cor_var[i, 2])], method = "spearman")
}
#Cutoff of 0.7 chosen
high_corr_s <- cor_var[abs(cor_var$scorr) > 0.7, ]
high_corr_s <- high_corr_s[high_corr_s$x != high_corr_s$y, ]
high_corr_s <- high_corr_s[!duplicated(high_corr_s[, 3]), ]

write.csv(high_corr_s, paste0(pwd, "/output/Spearman_Correlation.csv"))
