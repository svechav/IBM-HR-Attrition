######################################################################################
################################# VIF CALCULATION ####################################
######################################################################################

install.packages("corrplot")
library(corrplot)

#Correaltion Matrix
cont_data <- as.matrix(data[, cont_vars])
cor_m <- cor(cont_data)
#Set the margin
jpeg(filename = paste0(pwd, "/output/Corr_BeforeVIF.jpg"))
par(mar = c(5.1, 4.1, 4.1, 25.1))
corrplot(cor_m, method="circle", type = "upper")
dev.off()

#### Run EDA Code to create necessary variables

install.packages ("car", dep=T) 
library(car)

#VIF CutOff
vif_cut = 4

all_vars <- c(cont_vars, cat_vars)

mod_inputs <- all_vars[1]
for (i in (2:length(all_vars)))
{
  mod_inputs <- paste(mod_inputs, "+", all_vars[i])
}
mod_inputs
mod_test <- paste("y_var ~",mod_inputs)
mod_test

#Calculate VIF for all variables
vif <- as.data.frame(vif(lm(as.formula(mod_test),data=data)))

#Get the variable with highest VIF
max_vif <- sort(vif$GVIF, decreasing = TRUE)[1]
rm_var <- rownames(vif[vif$GVIF == max_vif, ])
all_vars <- all_vars[all_vars != rm_var]

while(max_vif >= vif_cut)
{
  mod_inputs <- all_vars[1]
  
  for (i in (2:length(all_vars)))
  {
    mod_inputs <- paste(mod_inputs, "+", all_vars[i])
  }

  mod_test <- paste("y_var ~",mod_inputs)
  
  #Calculate VIF for all variables
  vif <- as.data.frame(vif(lm(as.formula(mod_test),data=data)))
  
  #Get the variable with highest VIF
  max_vif <- sort(vif$GVIF, decreasing = TRUE)[1]
  rm_var <- rownames(vif[vif$GVIF == max_vif, ])
  all_vars <- all_vars[all_vars != rm_var]
}

#Correaltion Matrix After VIF
cont_data <- as.matrix(data[, subset(cont_vars, cont_vars %in% all_vars)])
cor_m <- cor(cont_data)
#Set the margin
jpeg(filename = paste0(pwd, "/output/Corr_AfterVIF.jpg"))
par(mar = c(5.1, 4.1, 4.1, 25.1))
corrplot(cor_m, method="circle", type = "upper")
dev.off()

cor_var <- as.data.frame(merge(subset(cont_vars, cont_vars %in% all_vars), subset(cont_vars, cont_vars %in% all_vars)))

for (i in (1:nrow(cor_var)))
{
  cor_var$pcorr[i] <- cor(data[as.character(cor_var[i, 1])], data[as.character(cor_var[i, 2])], method = "pearson")
}

after_vif <- cor_var[cor_var$x != cor_var$y, ]
after_vif <- after_vif[!duplicated(after_vif[, 3]), ]

write.csv(after_vif, "after_vif_corr.csv")

