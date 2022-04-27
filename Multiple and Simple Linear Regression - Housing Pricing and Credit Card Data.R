##################################################################################################################################################################################################################################
###########################################  * Data Wrangling and Formatting *  ##################################################################################################################################################
##################################################################################################################################################################################################################################

# Preload working directory for part 1.

setwd("C:/Users/Michael/OneDrive - Georgia State University/Desktop/FI4090/Data")       
getwd()
houseprices=read.csv("HousePrices.csv",header=TRUE)
attach(houseprices)

# First, we transform all yes/no columns into categorical values of 0 and 1. 0 = no, 1 = yes.

numrows = nrow(houseprices)    # Sets the conditions for n rows in matrix.
numrows

numcols = ncol(houseprices)    # Sets the conditions for n cols in matrix.
numcols

# driveway dummy

driveway_dummy <- matrix(nrow=numrows,ncol=1)     # Establishing a new col and row in matrix.
head(driveway_dummy)                              # Analyzing how the variable displays.
head(driveway)                                    # Comparing the original variable to new variable.

for (i in 1:numrows)  # setting the for loop to iterate through numrows.
{
  
  if (driveway[i]=='yes')  {            # If driveway is 'yes', we acknowledge the presence with a value of 1.
    driveway_dummy[i,]=1
  } else {                              # Else 0 if not 'yes'.
    driveway_dummy[i,]=0
  }
}

colnames(driveway_dummy)="driveway_dummy"  # Putting the column name within the data frame.

# WE REPEAT THIS THOUGHT PROCESS FOR GASHEAT, AIRCON, RECREATION, FULLBASE, AND PREFER

# gasheat dummy

gasheat_dummy <- matrix(nrow=numrows,ncol=1)
head(gasheat_dummy)
head(gasheat)

for (i in 1:numrows)
{
  
  if (gasheat[i]=='yes') {
    gasheat_dummy[i,]=1
  } else {
    gasheat_dummy[i,]=0
  }
}

colnames(gasheat_dummy)="gasheat_dummy"

# aircon dummy

aircon_dummy <- matrix(nrow=numrows,ncol=1)
head(aircon_dummy)
head(aircon)

for (i in 1:numrows)
{
  
  if (aircon[i]=='yes') {
    aircon_dummy[i,]=1
  } else {
    aircon_dummy[i,]=0
  }
}

colnames(aircon_dummy)="aircon_dummy"

# recreation dummy

recreation_dummy <- matrix(nrow=numrows,ncol=1)
head(recreation_dummy)
head(recreation)

for (i in 1:numrows)
{
  
  if (recreation[i]=='yes') {
    recreation_dummy[i,]=1
  } else {
    recreation_dummy[i,]=0
  }
}

colnames(recreation_dummy)="recreation_dummy"

# fullbase dummy

fullbase_dummy <- matrix(nrow=numrows, ncol=1)
head(fullbase_dummy)
head(fullbase)

for (i in 1:numrows)
{
  
  if (fullbase[i]=='yes') {
    fullbase_dummy[i,]=1
  } else {
    fullbase_dummy[i,]=0
  }
}

colnames(fullbase_dummy)="fullbase_dummy"

# prefer dummy

prefer_dummy <- matrix(nrow=numrows, ncol=1)
head(prefer_dummy)
head(prefer)

for (i in 1:numrows)
{
  
  if (prefer[i]=='yes') {
    prefer_dummy[i,]=1
  } else {
    prefer_dummy[i,]=0
  }
}

colnames(prefer_dummy)="prefer_dummy"

# Combining the data into one dataframe

houseprices2 = cbind(price, lotsize, bedrooms, bathrooms, stories, driveway_dummy, gasheat_dummy, aircon_dummy, recreation_dummy,    # binds the new dummy variables into the the data frame.
                     fullbase_dummy, prefer_dummy, garage) 

houseprices2_df = as.data.frame(houseprices2)  # The data is not a true dataframe, it's still considered a matrix. This converts the matrix into a true dataframe.
class(houseprices2_df)       # Checking the class of the data.

attach(houseprices2_df)      # Attaching data for ease of use.

install.packages("fBasics")  # Install packages that will be used for analysis.
install.packages("RSQLite")
install.packages("sqldf")
install.packages("scales")

library("fBasics")           # Load the packages from library.
library("RSQLite")           # SQL or Structured Query Language is great and easy library; sometimes useful. Never used the package as part of the analyses, however, it can be useful for those who wish to polish or practice their SQL skills.
library("sqldf")
library(scales)              # Loads a library/package.


percent <- function(x, digits=2, format="f", ...) {                       # In some cases, we may need to provide a function to analyze the percent usage of a variable.
  paste0(formatC(x * 100, format = format, digits = digits, ...), "%")
}

##############################################################################################################################################################################################################################
###########################################  * Data Analysis of House Pricing*  ##############################################################################################################################################
##############################################################################################################################################################################################################################

# Summary stat for all the variables in the HousePrices data.

summary(houseprices)   # Summary of statistics for the house prices data without modifying the variables.


# Percentage of houses in the data with Driveway, Gas-Heat and Air-conditioning present?  
# We use the mean to calculate the percentage since the variables are either 1 to 0.


percent_driveway = percent(mean(driveway_dummy))    # Finds the percent of houses with driveways present.

percent_gasheat = percent(mean(gasheat_dummy))      # Finds the percent of houses with gasheat present.
  
percent_aircon = percent(mean(aircon_dummy))        # Finds the percent of houses with aircon present.


percent_driveway        # Check variable.

percent_gasheat         # Check variable.

percent_aircon          # Check variable.


# Construct a linear regression model that tests whether number of bedrooms influence house prices.
# We then provide a summary of statistics for the linear Regression model.

lm.fit1 = lm(price~bedrooms, data = houseprices2_df)  # Calls the lm() function to regress price and bedrooms.
summary(lm.fit1)                                      # Summary of statistics for the simple linear regression model.

plot(bedrooms,price)                                  # Plots the regression.
abline(lm.fit1,lwd=3,col="red")

# Let's Establish the null hypothesis:
# The null hypothesis would state there is no effect of bedrooms on house pricing. 
# We establish the null to tell whether the variables have an important relationship in relaying whether.
# a statistic occurred by chance or there was improper manipulation of the data.

# Interpretting the coefficient of bedrooms in the model, we say:
# The beta is positive, therefore we can assume that for each additional unit of bedroom increases the house price by 13270.

# To infer the effect of number of bedrooms on house price, we look to draw the conclusion based on p-value. 

# the p-value of bedrooms is very close to 0. Statistically, this means that we can almost be 99% sure that bedrooms has predictive power on the dependent variable
# price. Remember that p-values are based on confidence intervals, hence we arrived at that 99% confidence in the bedroom predictor variable.

# How does the model R-square look?

# The R-square is considered low for the model. Currently, it is .1327 which means the model is only explaining 13.27% of the model's house pricing, and
# the remaining variance is due to other variables. However, the p-value of the linear model is very low, so we can be certain that bedrooms has predictive
# power on the dependent variable price. We would have to dive deeper to find multiple variables that affect house pricing.


# Now, we construct a multiple linear regression model by including all variables as predictors of house prices (response variable) 
# and observe the effect on the house prices. We provide a summary of the regression model using summary() function. We perform a multiple
# linear regression to lessen the "noise" in the previous simple regression.   

lm.fit2 = lm( price ~ lotsize + bedrooms + bathrooms + stories + driveway_dummy + 
             gasheat_dummy + aircon_dummy + recreation_dummy + fullbase_dummy + prefer_dummy + garage, data=houseprices2_df )

summary( lm.fit2 )

##################################################################################################################################################################################################################################








##################################################################################################################################################################################################################################
###########################################  * Data Analysis of Credit Card Data *  ##############################################################################################################################################
##################################################################################################################################################################################################################################

# Preliminary Data Manipulation to reformat categorical variables into binary responses.

setwd("C:/Users/Michael/OneDrive - Georgia State University/Desktop/FI4090/Data")
getwd()
credit=read.csv("credit.csv",header=TRUE)

attach(credit)

numrows2 = nrow(credit)
numrows2

numcols2 = ncol(credit)
numcols2

# Gender dummy

gender_dummy <- matrix(nrow=numrows2,ncol=1)
head(gender_dummy)
head(Gender)

for (i in 1:numrows2)
{
  
  if (Gender[i]=='Female') {
    gender_dummy[i,]=1
  } else {
    gender_dummy[i,]=0
  }
}

colnames(gender_dummy) = "gender_dummy"

# Student dummy

student_dummy <- matrix(nrow=numrows2,ncol=1)
head(student_dummy)
head(Student)

for (i in 1:numrows2)
{
  
  if (Student[i]=='Yes') {
    student_dummy[i,]=1
  } else {
    student_dummy[i,]=0
  }
}

colnames(student_dummy) = "student_dummy"

# Married dummy

married_dummy <- matrix(nrow=numrows2,ncol=1)
head(married_dummy)
head(Married)

for (i in 1:numrows2)
{
  
  if (Married[i]=='Yes') {
    married_dummy[i,]=1
  } else {
    married_dummy[i,]=0
  }
}

colnames(married_dummy) = "married_dummy"

# Ethnicity dummy

ethnicity_dummy <- matrix(nrow=numrows2,ncol=1)
head(ethnicity_dummy)
head(Ethnicity)

for (i in 1:numrows2)
{
  
  if (Ethnicity[i]=='Caucasian') {
    ethnicity_dummy[i,]=1
  } else {
    ethnicity_dummy[i,]=0
  }
}

colnames(ethnicity_dummy) = "ethnicity_dummy"

credit2 = cbind(Income, Limit, Rating, Cards, Age, Education, gender_dummy, student_dummy, married_dummy, ethnicity_dummy, Balance)  # Binds the newly formatted data columns into a whole new matrix.

credit2 = as.data.frame(credit2)   # Converts the formatted matrix into a data frame being able to display and utilize all the categorical variables.
class(credit2)


############################## Section i #########################################################################################################################################################################################

# Summary stat for the variables in Credit data:

summary(credit2)

# Percentage of Student in the Credit data; Percentage of Female in the Credit data:

percent(mean(student_dummy))

percent(mean(gender_dummy))

# We construct a multi-linear regression model as follows:
# Response variable: Credit Card Balance 
# Predictors: Credit Rating, Student, Credit Rating * Student (interaction terms)   

# Providing a summary of the model using summary() function.

lm.fit4 = lm(Balance ~ Rating + student_dummy + I(Rating*student_dummy))
summary(lm.fit4)


# What is the effect of Student on Credit Card Balance?


# We can interpret that if an individual's has a student status, they will increase their credit card balance by $ 312. We see in the regression model that
# the student_dummy has a p-value of .000316 which is very close to 0. This means that we can be 99% confident that the student dummy variable has an effect and
# we can expect to see this credit balance increase should we find another individual (outside of the study) having a student status.


# What is the total effect of Credit Rating on Credit Card Balance for non-students?

# The total effect of credit rating on credit card balance for non-students shows that for one unit increase of credit rating, credit card balance will increase by $2.54.  This seems to make sense
# because more credit rating can lead individuals to open up more lines of credit to spend more. It also means that people with higher credit rating can spend more which potentially puts more on 
# the balance. Rating has a p-value very close to 0, so we can also be 99% confident that this variable has predictive power.



# More Regression analysis on different variables for y = Balance:

setwd("C:/Users/Michael/OneDrive - Georgia State University/Desktop/FI4090/Data")
getwd()
credit=read.csv("credit.csv",header=TRUE)
attach(credit)

# Testing whether Age influence Credit Card Balance on the basis of simple linear regression.

lm.fit5 = lm( Balance ~ Age )
summary( lm.fit5 )

# Using Age and Credit Rating as predictors of Credit Card Balance (response variable) in a multiple linear regression setting. 

lm.fit6 = lm( Balance ~ Age + Rating )
summary( lm.fit6 )

# We see that Age and Rating have very low p-values suggesting that these variables have a strong chance of having an effect on the response variable Balance.
# Age has a negative coefficient of -2.35 while rating has a positive coefficient of 2.59. What we can expect is that for every increase of Age, credit card balance
# will decrease by $2.35, while for every increase in Rating, Credit Card Balance increases by $2.59.

# Comparing effect of Age:

# we notice that age by itself was not significant to affect the increase of credit card balances. However, when introduced the term of rating, we notice that Age becomes extremely significant
# and suggests that for each increase of age, credit card balance decreases by $2.3. However, this seems perplexing because it would imply that older people have less credit balance simply due to age?
# There are other factors that could be explored, or there is something inherently wrong with the modeling (even though there is a .75 R-squared).

##################################################################################################################################################################################################################################
