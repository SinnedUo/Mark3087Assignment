#Title: Logistic Regression with Model Estimation Sample 
#-------------------------------------------------------

# This script uses the sample with customers who have received a direct mail.
# Thus the sample contains information about whether or not someone responded to the mail campaign.
# We will use this information to estimate a logistic regression.


#-----------------------------------
#Set working directory and load data
#-----------------------------------

#Sets working directory
setwd("...")

#pulls data from a csv file
Sample1<-read.csv(".../Model Estimation Sample.csv", header=TRUE)

#Get an overview of the variables 
names(Sample1) 
# The dataset "Estimation Sample" contains a randomized sample of your customer database.
# All of the customers in the sample have received a direct mail.
# "ID" is customer ID. We use anonymized data for privacy reasons.
# However, if required (e.g., for contacting prospects) the IDs can be used to identify customers. 
# ..Checkings" to "Loan" are dummies that indicate whether or not a customer holds/uses such an account/bank product 
# "CreditC" to "Auto" are dummies for other products distributed through the bank including credit cards, 
# several insurances, and automatic savings        "b_check"      
# "income" is the monthly income cash flow
# "b_shares" to "b_loan" are customer balances on each of the bank's primary products. 
# There are no balances for the creditcards and insurances as banks earn commissions so the balances are not relevant.       
# "Age" is age. 
# "Trans" are the number of transactions a customer has done with the bank.
# "respons" indicates whether or not a customer has responded to the direct mail.

attach(Sample1) #"activates" the data set for easier access to the variables

summary(Sample1)

# For Task 1, we need to know the number of responses. 
# Since it is a dummy with no missings, we can just take the sum.
sum(respons)

#-------------------------------------------
# Install and load required packages
#-------------------------------------------

# install.packages("DescTools")
library(DescTools)

# install.packages("ResourceSelection")
library("ResourceSelection")

library(car)

# -----------------------------------------------------------
# Step 1: Define research goal
# -----------------------------------------------------------
# Identify customers who are most likely to respond to the direct mail campaign.

# -----------------------------------------------------------
# Step 2: Specify the model 
# This is task 3
# -----------------------------------------------------------

# Let's start with a full model including all information we have 
# excluding deposits as these are irrelevant (only 2 customers)

model1<-glm(respons ~ Checkings + Shares+ Invest + Certificate + Savings + 
              Mortgage + Damage + CreditC + Loan + wsf + Auto + Income + 
              b_check + b_shares + b_savings + b_invest + b_certificate + b_mortage +    
              b_loan + age + trans, data=Sample1, family=binomial)

# The glm() function helps us to specify a generalized linear model.
# GLMs are a class of models that deal with non-normally distributed outcomes, e.g., yes or no.
# Since there are other forms of non normally-distributed outcomes than just binary ones,
# the glm function requires us to specify the family. Here it is binomial (i.e., just two values, response yes or no)
# Model specification is the same as in a standard linear regression.
# Note that we get an error message so we should take this model with caution.

#------------------------------------------------------------
# Step 3: Model Evaluation
# This is task 4
#------------------------------------------------------------

# Global model evaluation

anova(model1, test="Chisq")

# Omnibus tests/ANOVA tests whether 
# the variance in a set of data explained by the model is significantly greater than 
# the unexplained variance.
# Thus, we want this test to be significant (p<.05).
# The test proceeds in a step wise manner, adding one of the independent variables
# in each step. We are only interested in the value in the last step.
# Here it is significant so ok.

h1<-hoslem.test(respons, fitted(model1))
h1
# Hosmer Lemeshow test whether the predicted values and the actual values are significantly different.
# "respons" identifies the observed binary, "fitted"model" the predicted.
# The test partitions the data into groups and compares for each one 
# whether there are differences between predictions and observations.
# "g=10" is th edefault choice for the number of groups the test uses.
# For a good model performance we want them to be NOT different.
# Thus, we want the Hosmer Lemeshow Test to be INSIGNIFICANT (>.05)!
# Here it is significant so not ideal.

PseudoR2(model1, which = "Nagelkerke") 
# There is no meaningful R-Square godness of fit measure as it is in linear regression.
# Thus we use Pseudo R-square measures. Here we choose Nagelkerke R-Square).
# It is always between 0 and 1. The higher the better.
# Here it could be better or worse. We could live with that.

# Local model evaluation, i.e., model parameters

summary(model1)
# We see that many model parameters are not significant
# This is not a problem per se but in our case we may have overspecified the model.
vif(model1)
# some VIFs are also a bit high (>5) 

# All in all we see that the model does not perform so well 
# in terms of Hosmer Lemeshow test (significant) 
# and also in terms of coefficients, 
# many of which are insignificant and/or multicollinear (according to VIFs).
# So let's specify a simpler model, where we only use the significant coefficients.
# Note, however, that we need at least one metric variable in the model, otherwise we would have to use another class of models.

model2<-glm(respons ~ Checkings + Invest + Certificate 
            + Damage + Auto + b_loan + trans, data=Sample1, family=binomial)
anova(model2, test="Chisq")
h2<-hoslem.test(respons, fitted(model2))
h2
PseudoR2(model2, which = "Nagelkerke") 
summary(model2)
vif(model2)
# This model looks ok by all means. So let's go on with it.

# If we want to see the odds, we can calculate them in the following way
exp(coef(model2))   # Remember, an odds ratio is exp(coefficient).
# Reading example: Inest has an odds value of 6.56,
# which means that someone who holds the Invest-product
# is 6.56 times as likely to respond as someone who does not hold it. 

# We also might want to look at standardized coefficients 
#install.packages("reghelper")
library("reghelper")          # to see which variable has the strongest effect.
model2.std <- beta(model2) # We can do that with the beta function from the reghelper PAckage
model2.std

#--------------------------------------------
# Step 4: Define classification cut-off values
# This is task 5
#--------------------------------------------
1/50                # 1$ Dollar per mail, $50 per positive response.
# That is, minimum to avoid loss is 1/50 = .02
Conf(model2, cutoff = 0.02) # We ask for a classification table.

##### Model from lecture ##############
# model2<-glm(respons ~ Invest + Damage + Auto  + trans, data=Sample1, family=binomial)
# anova(model2, test="Chisq")
# h<-hoslem.test(respons, fitted(model2), g=10)
# h
# PseudoR2(model2, which = "Nagelkerke") 
# summary(model2)
# vif(model2)
# Conf(model2, cutoff = 0.03)
######################################


#---------------------------------------------
# Step 5: Predictions
# This is task 6
#---------------------------------------------
# We pull the sample for the customers from which we should choose the prospects using our model
Sample2<-read.csv(".../Prediction Sample.csv", header=TRUE)
nrow(Sample2) # Counts the rows so we see that we have 4809 customers in the prediction sample

# Next we apply the model to predict the probabilities
prediction<-predict(model2, newdata=Sample2, type = "response")
# model2 is our fitted model
# newdata= specifies the sample that shall be used
# type = "response" clarifies that we want the response probabilities


# We find out how much we should spend and what the expected profit is.
# This is task 6

cutoff_02<-ifelse(prediction>=0.02, 1, 0)
# We use the appropriate cut-off to identify prospects a "1", all others a "0".
sum(cutoff_02)      # counting all 1s tells us that there are 991 prospects in the sample
# As we should only contact the prospects, we would spend $991.

prediction.sorted<-sort(prediction, decreasing = TRUE)   
# We sort the customers based on decreasing response probabilities.

prospect.probabilities<-prediction.sorted[1:991]
# We only choose the first 991 (as those are the prospects)

expected.profits<-prospect.probabilities*50-1
sum(expected.profits)
# And for those we calculate the expected profits,
# which are $1901.512.
# Remember: In the initial sample with the untargeted campaign,
# our profit amounted to $659 only.

detach(Sample1)
