############################################################################################################################################################################################################################### 
######################### CREDIT CARD - PRE-WORK ON DATA ######################################################################################################################################################################   
###############################################################################################################################################################################################################################  

# We wrangle the data in order to work in a smooth and efficient process. This section is very messy but such is the nature of wrangling data.
# In this pre-work section , we attempt to section datasheets for different use cases. 


setwd("C:/Users/Michael/OneDrive - Georgia State University/Desktop/FI4090/Data")      # Sets the file directory to grab data
getwd()                                                                                # Anchors the file directory

credit_card=read.csv("CreditCard.csv",header=TRUE)                                     # Set CreditCard.csv excel sheet as credit_card

attach(credit_card)            # Attaches the sheet to check


numrows = nrow(credit_card)    # Sets the conditions for n rows in matrix
numrows

numcols = ncol(credit_card)    # Sets the conditions for n cols in matrix
numcols

# Card Dummy

card_dummy <- matrix(nrow=numrows,ncol=1)     # Establishing a new col and row in matrix
head(card_dummy)                              # Analyzing how the variable displays
head(card)                                    # Comparing the original variable to new variable


for (i in 1:numrows)                          # Setting the for loop to iterate through numrows
{
  
  if (card[i]=='yes')  {                      # If card is 'yes', we acknowledge the presence with a value of 1
    card_dummy[i,]=1
  } else {                                    # Else 0 if not 'yes'
    card_dummy[i,]=0
  }
}

colnames(card_dummy)="card_dummy"             # Putting the column name within the data frame


################## WE REPEAT THIS THOUGHT PROCESS AND ACTION FOR OWNER AND SELFEMP ############################################################################################################################################        

# Owner Dummy

owner_dummy <- matrix(nrow=numrows,ncol=1)
head(owner_dummy)
head(owner)

for (i in 1:numrows)
{
  
  if (owner[i]=='yes') {
    owner_dummy[i,]=1
  } else {
    owner_dummy[i,]=0
  }
}

colnames(owner_dummy)="owner_dummy"



# Selfemp Dummy

selfemp_dummy <- matrix(nrow=numrows,ncol=1)
head(selfemp_dummy)
head(selfemp)

for (i in 1:numrows)
{
  
  if (selfemp[i]=='yes') {
    selfemp_dummy[i,]=1
  } else {
    selfemp_dummy[i,]=0
  }
}

colnames(selfemp_dummy)="owner_dummy"


credit_card2=cbind(card_dummy, reports, income, age, owner_dummy, dependents, months, share, selfemp_dummy, majorcards, active, expenditure) # Binds the new dummy variables into the the data frame

credit_card2_df=as.data.frame(credit_card2)  # The data is not a true data frame, it's still considered a matrix. This converts the matrix into a true data frame.
class(credit_card2_df)                       # Checking the class of the data

attach(credit_card2_df)                      # Attaching data for ease of use

l_share = data.frame(log(share))             # Takes the log of share and converts it to a data frame
head(l_share)                                # Checks the l_share column

colnames(l_share) = "l_share"                # Adds l_share as a column

l_reports = data.frame(log(reports+1))       # Log(reports + 1) as a data frame
head(l_reports)                              # Head log(l_reports)

colnames(l_reports) = "l_reports"            # Adds l_reports as a column

l_credit_card2 = cbind(card_dummy, l_reports, income, age, owner_dummy, dependents, months, l_share, selfemp_dummy, majorcards, active, expenditure) # binds the new dummy variables into the the data frame
l_credit_card2_df = as.data.frame(l_credit_card2)
class(l_credit_card2_df)




l_credit_card2_adults_df = subset(l_credit_card2_df, age>18) 

dim(l_credit_card2_adults_df)  # Checks dim to see if we removed age<18 
dim(l_credit_card2_df)         # Checks dim to see if all ages are present

credit_card_adults = credit_card[credit_card$age>18,]                    # Subsets the data where age > 18; should remove 7 values; keeps original values
credit_card2_adults_df = credit_card2_df[credit_card2_df$age>18,]        # Subsets the data where age > 18; we have dummy variables in this datasheet

###################### Formatted Datasheets ###################################################################################################################################################################################

credit_card               # Credit card datasheet as original 
credit_card_adults        # Credit card datasheet with only adults, no log, no dummies
credit_card2_df           # Credit card datasheet with dummies, no log functions
credit_card2_adults_df    # Credit card datasheet with dummies, subset age > 18, but no log functions
l_credit_card2_df         # Credit card datasheet with dummies and log functions, no age subset
l_credit_card2_adults_df  # Credit card datasheet with dummies, log functions, and subset age < 18


# Load and install all packages for use

install.packages("AER")
install.packages("fBasics")
install.packages("Formula")
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("ISLR")
install.packages("lmtest")

library("AER")
library("fBasics")
library("Formula")
library("ggplot2")
library("Hmisc")
library("ISLR")
library("lmtest")


###############################################################################################################################################################################################################################
###################### Data Analysis ##########################################################################################################################################################################################
###############################################################################################################################################################################################################################


# Summary stat of the predictors

attach(credit_card2_df)  # Attach the credit card datasheet with all dummies in place but no log variables and subset age data

summary(credit_card2_df) # Summary statistics of data



# We noticed there are some values of variable age under one year. We are going to consider data with age>18 for the analysis. We use the formatted sheet credit_card2_adults_df)


attach(credit_card2_adults_df)   # Attach the credit card datasheet with all dummies, subset age > 18, and no log variables

summary(credit_card2_adults_df)  # Summary of statistics
dim(credit_card2_adults_df)      # Dimensions of data sheet
length(credit_card2_adults_df)   # Length of data sheet


# Plotting of income vs. reports (Number of major derogatory reports): Marking individuals with card application accepted as blue, and not accepted as red


attach(credit_card2_adults_df)              # Attach the credit card datasheet with all dummies, subset age > 18, and no log variables
plot(income,reports, col= c("red","blue"))  # Plot xy graph of income vs reports.... income is x, reports i y




# Boxplots of income as a function of card acceptance status.  Boxplots of reports as a function of card acceptance status 
# (mark card application accepted as blue, and not accepted as red).


attach(credit_card2_adults_df)              # Attach the credit card datasheet with all dummies, subset age > 18, and no log variables

par(mfcol=c(1,2))                           # Sets up how many data visualizations can be set in the plots tab

boxplot(income ~ card_dummy, data = credit_card2_adults_df, xlab = "Card Acceptance",ylab = "Income", main = "Card Acceptance Status", col = c("red","blue"))  # Boxplot of income vs card acceptance
boxplot(reports ~ card_dummy, data = credit_card2_adults_df, xlab = "Card Acceptance",ylab = "Reports", main = "Card Acceptance Staus", col = c("red","blue")) # Boxplot of reports vs card acceptance


# We construct the histogram for the predictors of interest.
# Note that share is highly right-skewed, so log(share) will be used in the analysis. reports is also extremely right skewed (most values of reports are 0 or 1, but the maximum value is 14. 
# To reduce the skewness, log(reports+1) will be used for the analysis. We do this because highly skewed predictors have high leverage points and are less likely to be linearly related to the response.


attach(l_credit_card2_adults_df)                 # Credit card datasheet with dummies, log functions, and subset without age < 18
                                                                                                                            
par(mfcol=c(1,1))                                # Resets the data visualizations on the plots tab

hist.data.frame(l_credit_card2_adults_df[,2:8])  # Using function from Hmisc library to plot six different histograms....cannot plot certain variables because they are categorical variables

hist(l_credit_card2_adults_df[,2:8])


# Using variables 2 to 8 to determine which of the predictors influence the probability that an application is accepted. Using the summary function to print the results.


attach(credit_card2_adults_df)                   # Credit card datasheet with dummies, no log functions, and subset without age < 18

fit1 = glm(card_dummy ~ I(log(reports+1)) + income + age + owner_dummy + dependents + months +      # Logistic regression of requested variables using glm()
           I(log(share)), family="binomial", data=credit_card2_adults_df)                        

summary(fit1)         # Summary of statistics


# To predict whether the application will be accepted or not, we convert the predicted probabilities into class labels yes with the following condition: probs >.5="yes". 
# Then, we compute the confusion matrix and overall fraction of correct predictions.

attach(credit_card_adults)
creditcard_fit = glm(card ~ I(log(reports+1)) + income + I(age > 18) + owner + dependents + months + I(log(share)), data=credit_card_adults, family="binomial")
summary(creditcard_fit)
coef(creditcard_fit)

pred_creditcard = predict(creditcard_fit,type="response")       # Uses the predict function to produce a set of predictions based on logistic fit
head(pred_creditcard)                                           # Checks the head of the predicted values

length(pred_creditcard)                                         # Checks length of predicted values


contrasts(card)                                                 # The contrasts() function indicates that R has created a dummy variable with a 1 for card = Yes


glm.pred.cc = rep("no",1312)                                    # The following command creates a vector of 1312 No elements


glm.pred.cc[pred_creditcard>.5] = "yes"                         # The following command transforms all the elements with predicted probabilities of default greater than 0.5 from No to Yes

head(glm.pred.cc)

head(credit_card$card)

table(glm.pred.cc,card)                                         # Confusion matrix in comparison to the original card data

correct1 = 294 + 994
incorrect1 = 1 + 23
total1 = correct1 + incorrect1

correct_frac <- (correct1)/(total1)
correct_frac

mean(glm.pred.cc==card)                                         # Compares the accuracy to the hard coded numbers


# Now we fit the logistic regression model using a training data for observations 1 to 1000. 
# Then, we compute the confusion matrix and the overall fraction of correct predictions for the test data (that is, the data for observations 1001 to end of data.)


attach(credit_card_adults)
class(credit_card_adults)                                      # Attaching original data sheet but filtering by adults                                    

train=credit_card_adults[1:1000,]                              # Sets up the train data set of credit card data
test=credit_card_adults[1001:nrow(credit_card_adults),]        # Sets up the test data set of credit card data

glm.fit.cc=glm(card ~ I(log(reports+1)) + income + age + owner + dependents + months + I(log(share)), data=train, family=binomial)   # Fit the train data (logistic)
summary(glm.fit.cc)          # Summary of statistics of train data
coef(glm.fit.cc)             # Coeffecients of data to get an overview





glm.probs.cc=predict(glm.fit.cc, test, type="response")  # Test the model by fitting the test data set into the fitted model; The predict() function: predicts the market "Up" probability on a particular day on the basis of the predicted model
head(glm.probs.cc)                                       # Checking the head of the probabilities
length(glm.probs.cc)                                     # Checking length of the vector

contrasts(test$card)                                     # The contrasts() function indicates that R has created a dummy variable with a 1 for yes 

glm.pred.test=rep("no",312) 



glm.pred.test[glm.probs.cc>.5]="yes"                     # The following command transforms all the elements with predicted probabilities of credit card acceptance greater than 0.5 from no to yes

table(glm.pred.test,test$card)                           # Table() produces a confusion matrix to determine how many observations were correctly or incorrectly classified
                                                         # The diagonal elements of the confusion matrix indicate the correct predictions and the off-diagonal elements represent incorrect predictions.



mean(glm.pred.test==test$card)                           # Test accuracy rate


mean(glm.pred.test!=test$card)                           # Test error rate

###########################################################################################################################################################################################################

