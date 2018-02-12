####################################################

#Kathryn Furman
#SAP.iO Coding Challenge
#February 2018 

####################################################

#Packages and libraries

library(dplyr)
library(Hmisc)
library(tidyverse)
library(car)
library(olsrr)

####################################################

#Business Problem: What makes a wine 'good'?

####################################################

#Assumptions: Split data by type into red wine and white wine.
  #Based on subject matter knowledge, what makes a white wine good
  #could be different from what makes a red wine good - people look
  #for different characteristics in these two types of wine

####################################################

#Read in CSV file of wines data set

wines <- read.csv('C:/Users/Kathryn/Google Drive/IAA/SAP.iO Coding Challenge/SAPio_DataScience_Challenge.csv')

####################################################

#Determine number of reds and whites in data set to see if there 
#are enough observations to split

table(wines$type)

#Reds: 1599
#Whites: 4898

####################################################

#Split data by type (red and white)

reds <- dplyr::filter(wines, type == 'red')
whites <- dplyr::filter(wines, type == 'white')


####################################################

#Split data for reds into training (70%) and validation (30%) in 
#order to do an honest assessment

#70% sample size for training
red_smp_size <- floor(0.70 * nrow(reds))

set.seed(378)
r_tr_indexes <- sample(seq_len(nrow(reds)), size = red_smp_size)

#Reds training data set
reds.train <- reds[r_tr_indexes, ]
#Reds validation data set
reds.valid <- reds[-r_tr_indexes, ]

#Split data for whites into training (70%) and validation (30%) in 
#order to do an honest assessment

#70% sample size for training
white_smp_size <- floor(0.70 * nrow(whites))

set.seed(548)
w_tr_indexes <- sample(seq_len(nrow(whites)), size = white_smp_size)

#Whites training data set
whites.train <- whites[w_tr_indexes, ]
#Whites validatation data set
whites.valid <- whites[-w_tr_indexes, ]

####################################################

#Exploratory Data Analysis on WHITE WINES training data set

####################################################


#Create histograms of all variables using whites training data set
hist.data.frame(whites.train)

#Fixed acidity: approximately Normal, no missing values
#Volatile acidity: right skewed, small % of missing values - use median for imputation
#citric acid: right skewed, no missing values
#astringency rating: approximately Normal, small % of missing values - use median for imputation
#residual sugar: right skewed, high % of missing values (>30%)
    #DECISION: drop Residual Sugar variable due to large number of missing values and also being highly correlated with density
    #expect to still miss it in the future
#chlorides: right skewed, no missing values
#free sulfur dioxide: right skewed, no missing values
#total sulfer dioxide: approximately Normal, no missing values
#density: slight right skewed, no missing values
#pH: approximately Normal, small % of missing values - use median for imputation
#sulphates: approximately Normal, no missing values
#alcohol: slight right skewed, no missing values
#vintage: uniform, small % of missing values - group these into their own level called "missing"
#quality: approximately Normal, no missing values

####################################################

#Outlier detection
#If more time, with more information, use subject matter expertise to 
#define definitions for outliers for each of the dependent variables or 
#use IQR to determine outliers 

####################################################

#standardization
#Would need to standardize variables that were on different scales, 
#but isn't appropriate for this data

####################################################

#Imputation of Missing Values

####################################################

#In an ideal world, we would model the missing values based on other variables 
#and predict using other variables to fill in missing values
#Here, we will use median for contiuous variables and would use mode for categorial variables

whites.train$volatile.acidity[is.na(whites.train$volatile.acidity)] <- median(whites.train$volatile.acidity, na.rm=TRUE)

whites.train$astringency.rating[is.na(whites.train$astringency.rating)] <- median(whites.train$astringency.rating, na.rm=TRUE)

whites.train$pH[is.na(whites.train$pH)] <- median(whites.train$pH, na.rm=TRUE)


#calling all of the NAs "missing"
whites.train$vintage[is.na(whites.train$vintage)] <- "missing"
whites.train$vintage <- factor(whites.train$vintage)


####################################################

#Correlation Analysis to remove repetitive information

####################################################

#remove categorical variables from df
cont_whites.train <- select(whites.train, -type, -vintage)

cor(cont_whites.train, method = "pearson")

#Using a threshold of r>0.5 for too high of a correlation to keep both variables
#fixed acidity and astringency rating have high correlation: use fixed acidicity because no missing values
    #REMOVE astringency rating
#total sulfur dioxide and free sulphur dioxide: use total sulfur dioxide over free because more of a normal distribution
    #REMOVE free sulfur dioxide
#total sulfur dioxide and density: use total sulfur dioxide over density because more of a normal distribution
    #REMOVE density
#density and alcohol: use alcohol because already removed density and has similar distributions

####################################################

#Final training set for model fitting with 3 variables removed due to high correlations and 1 variable removed due to high % of missing values
whites.train <- dplyr::select(whites.train, -astringency.rating, -free.sulfur.dioxide, -density, -residual.sugar)

####################################################

#Mulitple Linear Regression

#Use significance level of parameters and adjusted R^2 to evaluate

#Model 1: all variables
mod1 <- lm(quality ~ fixed.acidity + volatile.acidity + citric.acid + chlorides + total.sulfur.dioxide
            + pH + sulphates + alcohol + vintage, data=whites.train)
#use automatic subset selection to determine best model at each number of parameters
#use adjusted R^2 (larger is better) and AIC (smaller is better) to evaluate models from best subset selection

#if more time, would also try LASSO for variable selection
best_mod <- ols_best_subset(mod1)
summary(best_mod)
plot(best_mod)
summary(mod1)
vif(mod1)
#not all parameters signifcant at alpha = 0.05 level

#USE FOR VALIDATION 1/3:
#Model 2: drop all variables from Model 1 that weren't significant using alpha = .01
mod2 <- lm(quality ~ fixed.acidity + volatile.acidity + total.sulfur.dioxide
           + sulphates + alcohol, data=whites.train)
summary(mod2)
vif(mod2)
#all parameters signifcant at alpha = 0.05 level

#Model 3: subset of model 2 for a more parsimonious model
mod3 <- lm(quality ~ fixed.acidity + volatile.acidity + alcohol, data=whites.train)
summary(mod3)
vif(mod3)

#USE FOR VALIDATION 2/3:
#Model 4: Best model based on adj. R^2, AIC - includes 6 predictors
mod4 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide
           + sulphates + alcohol, data=whites.train)
summary(mod4)
vif(mod4)
#all parameters signifcant at alpha = 0.05 level

#USE FOR VALIDATION 3/3:
#Model 5: Best parsimonious model with 2 predictors from automatic selection - volatile.acidity and alcohol
mod5 <- lm(quality ~ volatile.acidity + alcohol, data=whites.train)
summary(mod5)
vif(mod5)
#all parameters signifcant at alpha = 0.05 level

#would also check for 2-level interactions with more time to see if there is an effect of one independent variable on the target variable, 
#given the value of another independent variable
#Example with fixed.acidity*alcohol:
mod6 <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide
          + sulphates + alcohol + fixed.acidity*alcohol, data=whites.train)
summary(mod6)
vif(mod6)



####################################################

#Assumptions Check

####################################################

#1 Independent variables are linearly related to the target variable (quality)

# component + residual plot 
crPlots(mod2)
# Ceres plots 
ceresPlots(mod2)

#PASSES: all plots show somewhat of a linear relationship

# component + residual plot 
crPlots(mod4)
# Ceres plots 
ceresPlots(mod4)

#PASSES: all plots show somewhat of a linear relationship

# component + residual plot 
crPlots(mod5)
# Ceres plots 
ceresPlots(mod5)

#PASSES: all plots show somewhat of a linear relationship

####################################################

#2 Normality of Errors

#qq plot for studentized resid
qqPlot(mod2, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(mod2) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#PASSES: residuals follow a straight line in the qqplot and are approximately Normally distributed


#qq plot for studentized resid
qqPlot(mod4, main="QQ Plot")

# distribution of studentized residuals
sresid <- studres(mod4) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#PASSES: residuals follow a straight line in the qqplot and are approximately Normally distributed


#qq plot for studentized resid
qqPlot(mod5, main="QQ Plot")

# distribution of studentized residuals
sresid <- studres(mod5) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

#PASSES: residuals follow a straight line in the qqplot and are approximately Normally distributed

####################################################

#3 Constant Variance of the Errors

# Model 2: non-constant error variance test
ncvTest(mod2)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mod2)

#DID NOT PASS: Pattern in residual plot - likely due to target variable values

# Model 4: non-constant error variance test
ncvTest(mod4)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mod4)

#DID NOT PASS: Pattern in residual plot - likely due to target variable values

# Model 5: non-constant error variance test
ncvTest(mod5)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(mod5)

#DID NOT PASS: Pattern in residual plot - likely due to target variable values

####################################################

#4 Errors are Independent

durbinWatsonTest(mod2)
#PASSES: Fail to reject H0 of no correlation among errors

durbinWatsonTest(mod4)
#PASSES: Fail to reject H0 of no correlation among errors

durbinWatsonTest(mod5)
#PASSES: Fail to reject H0 of no correlation among errors

####################################################

#5 No multicollinearity

vif(mod2)
#PASSES: all VIFs less than 10

vif(mod4)
#PASSES: all VIFs less than 10

vif(mod5)
#PASSES: all VIFs less than 10

####################################################

#Validation of final models

####################################################

#Prepare validation data set to match structure of training data set

#1 Imputation of Missing Values

#Create histograms of all variables using whites validation data set
hist.data.frame(whites.valid)

#In an ideal world, we would model the missing values based on other variables 
#and predict using other variables to fill in missing values
#Here, we will use median of training set for continuous variables and would use mode for categorial variables

whites.valid$volatile.acidity[is.na(whites.valid$volatile.acidity)] <- median(whites.train$volatile.acidity, na.rm=TRUE)

whites.valid$pH[is.na(whites.valid$pH)] <- median(whites.train$pH, na.rm=TRUE)


#calling all of the NAs "missing"
whites.valid$vintage[is.na(whites.valid$vintage)] <- "missing"
whites.valid$vintage <- factor(whites.valid$vintage)

####################################################

#Final validation set for model validating with 3 variables removed due to high correlations and 1 variable removed due to high % of missing values
whites.valid <- dplyr::select(whites.valid, -astringency.rating, -free.sulfur.dioxide, -density, -residual.sugar)

#Use significane of parameter estimates and ASE to evaluate
#significance level using alpha = 0.05
#ASE: smaller implies better model

#Model 2:
whites.valid$pred_quality2 <- predict(mod2, newdata=whites.valid)
summary(mod2)

#Model 2: drop all variables from Model 1 that weren't significant using alpha = .01
mod2v <- lm(quality ~ fixed.acidity + volatile.acidity + total.sulfur.dioxide
          + sulphates + alcohol, data=whites.valid)
summary(mod2v)
vif(mod2v)
#all parameters signifcant at alpha = 0.05 level

#ASE (Average Squared Error) calculation
mean((whites.valid$pred_quality2 - whites.valid$quality)^2)
#ASE = 0.5678797

####################################################

#Model 4: 
whites.valid$pred_quality4 <- predict(mod4, newdata=whites.valid)
summary(mod4)

#Model 4: Best model based on adj. R^2, AIC - includes 6 predictors
mod4v <- lm(quality ~ fixed.acidity + volatile.acidity + chlorides + total.sulfur.dioxide
            + sulphates + alcohol, data=whites.valid)
summary(mod4v)
vif(mod4v)
#not all parameters significant at alpha = 0.05, very similar adj. R^2 to model 2, more parsimonious

#ASE calculation
mean((whites.valid$pred_quality4 - whites.valid$quality)^2)
#ASE = 0.5686061

####################################################

#Model 5:
whites.valid$pred_quality5 <- predict(mod5, newdata=whites.valid)
summary(mod2)

#Model 5: Best parsimonious model with 2 predictors from automatic selection - volatile.acidity and alcohol
mod5v <- lm(quality ~ volatile.acidity + alcohol, data=whites.valid)
summary(mod5v)
vif(mod5v)
#all parameters signifcant at alpha = 0.05 level, adj. R^2 slightly lower than model 2

#ASE calculation
mean((whites.valid$pred_quality5 - whites.valid$quality)^2)
#ASE = 0.5736576


####################################################

#Interpretation on validation set
#ASE - how good your model is on data is hasn't seen before
#Parameter interpretation

####################################################

#Refit on whole data

#Prepare whole data set to match structure of training and validation data sets

#1 Imputation of Missing Values

#Create histograms of all variables using whites validation data set
hist.data.frame(whites)

#In an ideal world, we would model the missing values based on other variables 
#and predict using other variables to fill in missing values
#Here, we will use median of training set for continuous variables and would use mode for categorial variables

whites$volatile.acidity[is.na(whites$volatile.acidity)] <- median(whites.train$volatile.acidity, na.rm=TRUE)

whites$pH[is.na(whites$pH)] <- median(whites.train$pH, na.rm=TRUE)


#calling all of the NAs "missing"
whites$vintage[is.na(whites$vintage)] <- "missing"
whites$vintage <- factor(whites$vintage)


####################################################


#Final data set for final model assessment with 3 variables removed due to high correlations and 1 variable removed due to high % of missing values
whites <- dplyr::select(whites, -astringency.rating, -free.sulfur.dioxide, -density, -residual.sugar)

#IF INTERESTED IN SIMPLEST, MOST PARSIMONIOUS MODEL:

whites$pred_quality5f <- predict(mod5, newdata=whites)
summary(mod5)

#Model 5: Best parsimonious model with 2 predictors from automatic selection - volatile.acidity and alcohol
mod5f <- lm(quality ~ volatile.acidity + alcohol, data=whites)
summary(mod5f)
vif(mod5f)

#ASE calculation
mean((whites$pred_quality5f - whites$quality)^2)
#ASE = 0.5994207

#FINAL MODEL FOR WHITE WINES:
#quality = 3.007374 - 1.950627(volatile.acidity) + 0.324427(alcohol)

#Insights:
#Volatile acidity has a negative impact on quality rating while alcohol content has a positive impact on quality rating
#For exmaple, one can expect the quality rating to decrease by about 1.95 points for every one unit increase in volatile acidity and can expect
#quality rating to increase by about 0.32 points for every additional unit of alcohol concentration
#The most important factors contributing to a "good" wine would be low volatile acidity, along with high alcohol


####################################################

#IF INTERESTED MORE COMPLEX MODEL WITH THE BEST ASE:

whites$pred_quality2f <- predict(mod2, newdata=whites)
summary(mod2)

#Model 5: Best parsimonious model with 2 predictors from automatic selection - volatile.acidity and alcohol
mod2f <- lm(quality ~ fixed.acidity + volatile.acidity + total.sulfur.dioxide
            + sulphates + alcohol, data=whites)
summary(mod2f)
vif(mod2f)

#ASE calculation
mean((whites$pred_quality2f - whites$quality)^2)
#ASE = 0.5922889 (slightly lower than model 5)

#FINAL MODEL FOR WHITE WINES:
#quality = 3.0277703 - 0.0688710(fixed.acidity) - 2.0024267(volatile.acidity) + 0.0011401(total.sulfur.dioxide) 
#             +  0.3540478(sulphates) + 0.3372557(alcohol)

#Insights:
#Both fixed and volatile acidity have a negative impact on quality rating while alcohol content, sulphates, and total sulur dioxide
#have a positive impact on quality rating.
#For example, one can expect the quality rating to decrease by about 2.00 points for every one unit increase in volatile acidity and can expect
#quality rating to increase by about 0.34 points for every additional unit of alcohol concentration.
#The most important factors contributing to a "good" wine would be low fixed and volatile acidity, along with high total sulfur dioxide,
#sulphates, and alcohol

#citric.acid, chlorides, pH, and vintage did not seem to be important factors in contributing to what makes a wine good.


####################################################

#RED WINES

#If given more time, would repeat the steps above to fit a multiple linear regression model on the reds data set
#to determine what factors are important in understanding the characteristics that make a good wine



####################################################

#If given more time and time to work with the business stakeholders to determine a definition for "good,"
#would try fitting a logistic regression model with the event=1 meaning a wine with a score of 8 or better, for example, and then
#determining which variables are important in classifying a wine as event = 1 for "good".

#Might also try a decision tree to determine which variables are helpful in separating out the "good" vs. "not good" wines

