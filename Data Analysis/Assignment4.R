#Boston Data Asignment 4

library(MASS) #Call the package MASS
library(ggplot2) #Call the package ggplot2
library(corrplot) # Plot Correlation matrix 
library(sjPlot)
library(caret)
library(car)

df.Boston = Boston #Define df.Boston Calling Data Boston from the package MASS
str(df.Boston)#structure data frame
names(df.Boston) #column names of the dataframe
head(df.Boston) #head or first view of the dataframe
summary(df.Boston) #summary basic statistics about df.Boston all columns


#Creating testing data %90
sample_index = sample(nrow(df.Boston),nrow(df.Boston)*0.70)

df.Boston.Train = df.Boston[sample_index,]

df.Boston.Test = df.Boston[-sample_index,]

# The database is attached to the R search path, so we ca call objects only 
# giving their names
attach(df.Boston.Train) 


#####     REGRETION     #####

lm.df.Boston.All.Variables <- lm(df.Boston.Train$medv ~ ., data = df.Boston.Train)
lm.df.Boston.All.Variables
summary(lm.df.Boston.All.Variables)

# the 76% in the R-squared

# The histogram show us that the commun median prices in $1000s or key characteristics 
# are between 1.5 and 2.5, where the peak value is 2 to 2.5

hist_df.Boston_medv <- hist(df.Boston.Train$medv,border = "blue", col ="green", breaks = 20)
str(hist_df.Boston_medv)
summary(df.Boston.Train$medv)
mean.df.Boston_medv <- mean(df.Boston.Train$medv) #where the mean is 22.47
mean.df.Boston_medv
qqnorm(df.Boston.Train$medv)  #create a graphic to see distribution of df.Boston$medv
qqline(df.Boston.Train$medv)
##the histogram and the QQ plot show that the data is skewed to the right


# it looks that the number of rooms (rm) have a normal 
# distribution and the data is not skewed
hist_df.Boston_rm <- hist(df.Boston.Train$rm,border = "blue", col ="green", breaks = 20)
str(hist_df.Boston_rm) #Call the structure of histogram RM.df.Boston
summary(df.Boston.Train$rm)
mean.df.Boston_rm <- mean(df.Boston.Train$rm) #where the mean is 6.284
mean.df.Boston_rm
qqnorm(df.Boston.Train$rm) #create a graphic to see distribution of df.Boston$rm
qqline(df.Boston.Train$rm)

# it looks that the lowest status of the population (lstat) 
# have skewed data, because the histogram show us that the distribution is not symmetric,
# but the distribution is close
hist.dfBoston.lstat <- hist(df.Boston.Train$lstat, border = "blue", col ="green",  breaks = 20)
str(hist(df.Boston.Train$lstat))
summary(df.Boston.Train$lstat)
mean.df.Boston_lstat <- mean(df.Boston.Train$lstat)
mean.df.Boston_lstat
qqnorm(df.Boston.Train$lstat)
qqline(df.Boston.Train$lstat)

# it looks that the number of rooms (rm) have a normal 
# distribution and the data is not skewed
hist_df.Boston_rm <- hist(df.Boston.Train$rm,border = "blue", col ="green", breaks = 20)
str(hist_df.Boston_rm) #Call the structure of histogram RM.df.Boston
summary(df.Boston.Train$rm)
mean.df.Boston_rm <- mean(df.Boston.Train$rm) #where the mean is 6.286
mean.df.Boston_rm
qqnorm(df.Boston.Train$rm) #create a graphic to see distribution of df.Boston$rm
qqline(df.Boston.Train$rm)


# it looks that the histogram has skewed data to the right because the graphic doesn't show  symmetric distribution
hist_df.Boston_ptratio <- hist(df.Boston.Train$ptratio, border = "blue", col ="green",  breaks = 20)
str(hist_df.Boston_ptratio)
summary(df.Boston.Train$ptratio)
mean.df.Boston_ptratio <- mean(df.Boston.Train$ptratio)
mean.df.Boston_ptratio
qqnorm(df.Boston.Train$ptratio)#create a graphic to see distribution of df.Boston$ptratio
qqline(df.Boston.Train$ptratio)


######    CORRELATION   ######

cor(df.Boston.Train, df.Boston.Train$medv)
#  if any feature has near zero variance (values not varying much within the column).

correlation.df.Boston <- cor(df.Boston) # To understand the correlation
correlation.df.Boston

# the average number of rooms (rm) has the highest positive correlation with median
# price home value
# median value in $1.000s (medv) and Numer of rooms (rm) have a correlation of 0.69535995
# And median value in $1.000s (medv) compared against (ptratio) pupil-teacher ratio 
# has a negative correlation of -0.5077867
# And median value in $1.000s (medv)compared against (lstat) and lower status of
# the population have the high negative correlations with -0.7376627
# what means that they don´t have 

corrplot(cor(correlation.df.Boston), type="lower")
corrplot(cor(correlation.df.Boston), type="upper")
corrplot(cor(correlation.df.Boston))

# T.TEST

t.test(df.Boston$medv , df.Boston$ptratio)
t.test(df.Boston$medv , df.Boston$crim)
t.test(df.Boston$medv , df.Boston$lstat)


######################################### LINEAR MODEL 1 ################################################################################# LINEAR MODEL USING ALL FEATURES #########################################

names(df.Boston) #column names of the dataframe

#We create the LINEAR MODEL relation MEDV ~ , using all variables
regre.model_1.df.Boston <- lm(medv ~ ., data = df.Boston.Train)
summary(regre.model_1.df.Boston)

# in the first model we can see that the variables (indus) and (age) and (chas) can be removed from the model. 
#indus         0.025828   0.072878   0.354 0.723258    
#age           0.010950   0.015591   0.702 0.482964
#chas          1.962696   1.026871   1.911 0.056802 .  

## check cooeffs
data.frame(coef = round(regre.model_1.df.Boston$coefficients,2))

#predictive models
predict.regre.model_1.df.Boston <- predict(regre.model_1.df.Boston , newdata = df.Boston.Test)
summary(predict.regre.model_1.df.Boston)

# RMSE
rmse.regre.model_1.df.Boston <- sqrt(sum((predict.regre.model_1.df.Boston - df.Boston.Test$medv)^2)/length(df.Boston.Test$medv))
rmse.regre.model_1.df.Boston
c(RMSE = rmse.regre.model_1.df.Boston, R2 = summary(regre.model_1.df.Boston)$r.squared)
#RMSE for this model is 0.7441

# p-value f
#Any feature which is not significant (p<0.05) is not contributing signicantly for the model, 
library(car)
vif(regre.model_1.df.Boston)


######################################### LINEAR MODEL  2 ######################################################

#We create the LINEAR MODEL relation, trying to fix the skewed data to the right using log() MEDV ~ , 
#using all variables
regre.model_2.df.Boston <- lm(log(medv) ~ ., data = df.Boston.Train)
summary(regre.model_2.df.Boston)

#predictive models
predict.regre.model_2.df.Boston <- predict(regre.model_2.df.Boston , newdata = df.Boston.Test)
summary(predict.regre.model_2.df.Boston)

# RMSE
rmse.regre.model_2.df.Boston <- sqrt(sum((predict.regre.model_2.df.Boston - df.Boston.Test$medv)^2)/length(df.Boston.Test$medv))
rmse.regre.model_2.df.Boston
c(RMSE = rmse.regre.model_2.df.Boston, R2 = summary(regre.model_2.df.Boston)$r.squared)

# p-value f
#Any feature which is not significant (p<0.05) is not contributing signicantly for the model, 
library(car)
vif(regre.model_2.df.Boston)
#it is necesary that the vif to be less than 5 for all the features. We see that the vif is greater
#than 5 for RAD and TAX. so will drop those variables in the next model

#this model is more accurate than the last model due to the increase of the R2 and RMSE



######################################### LINEAR MODEL  3 ######################################################

#We create the LINEAR MODEL relation, trying to fix the skewed data to the right using log() MEDV ~ , 
#using all variables
regre.model_3.df.Boston <- lm(log(medv) ~ crim + zn +indus+ chas +nox +rm +dis +ptratio  +black + lstat, data = df.Boston.Train)
summary(regre.model_3.df.Boston)

#predictive models
predict.regre.model_3.df.Boston <- predict(regre.model_3.df.Boston , newdata = df.Boston.Test)
summary(predict.regre.model_3.df.Boston)

# RMSE
rmse.regre.model_3.df.Boston <- sqrt(sum((predict.regre.model_3.df.Boston - df.Boston.Test$medv)^2)/length(df.Boston.Test$medv))
rmse.regre.model_3.df.Boston
c(RMSE = rmse.regre.model_3.df.Boston, R2 = summary(regre.model_3.df.Boston)$r.squared)

# p-value f
#Any feature which is not significant (p<0.05) is not contributing signicantly for the model, 
library(car)
vif(regre.model_3.df.Boston)
#it is necesary that the vif to be less than 5 for all the features. We see that the vif is greater
#than 5 for RAD and TAX. so will drop those variables in the next model


#this model is more accurate than the last model due to the increase of the R2 and RMSE

