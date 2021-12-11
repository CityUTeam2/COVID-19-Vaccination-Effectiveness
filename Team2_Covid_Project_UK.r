# Import data from covid dataset
inputData <- read.csv("owid-covid-data_2021_11_28.csv")

# Explore the UK data be viewing:
subset(inputData, 
    select = c("total_vaccinations_per_hundred", "new_deaths", "new_deaths_per_million","new_cases","stringency_index"), 
    location == "United Kingdom")

# Based on data exploration
# 1. Filter on UK data only
# 2. Only include the following columns to consider
#       stringency_index
#       total_vaccinations_per_hundred
#       new_cases
#       new_deaths_per_million
# 3. Filter on new_deaths_per_million > 0 as we are focused on 
#    seeing how effective vaccinations are against minimizing mortality rates.
#    Also cap on total_vaccinations_per_hundred < 120 to avoid the spike in deaths that occur
#    to allow us to use simple models.
#    Filter out negative new cases
# 4. Replace remaining NA with 0 for stringency_index and 
# uk_inputdata <- subset(inputData, 
#     select = c("total_vaccinations_per_hundred", "new_deaths", "new_deaths_per_million","new_cases","stringency_index"), 
#     location == "United States" & new_deaths_per_million > 0)
uk_inputdata <- subset(inputData, 
    select = c("total_vaccinations_per_hundred", "new_deaths", "new_deaths_per_million","new_cases","stringency_index"), 
    location == "United Kingdom" & 
    new_deaths_per_million > 0 & 
    new_cases >= 0 &
    total_vaccinations_per_hundred < 120)

# check out the structure of our dataframe and confirm that they are all num
str(uk_inputdata)

# Replace NA with 0 for total_vaccinations_per_hundred column and total_vaccinations_per_hundred
# because they would be 0 when the deaths first began
#uk_inputdata$total_vaccinations_per_hundred[is.na(uk_inputdata$total_vaccinations_per_hundred)] <- 0
uk_inputdata[is.na(uk_inputdata)] <- 0

# Validate cleansing - confirm that NAs have been replaced by zeros - this should return 0 rows
uk_inputdata[rowSums(is.na(uk_inputdata)) > 0, ] 

#
# Dependent variable: deathToNewCase
# *** Need to weigh number of new cases with new deaths to get a fair measure of vaccination effectiveness. ***
# To measure the effectiveness of our vaccine we will determine if the # vaccinations per 100 
# reduces the mortality by new cases: deathToNewCase = new_deaths / new_cases
# Independent (input) variables: total_vaccinations_per_hundred
# Take log10 to get a more distinguishable trend. Added 1 to denom to avoid div by 0.
uk_inputdata$deathToNewCase <- log10(uk_inputdata$new_deaths / (uk_inputdata$new_cases+1))

# put partition back to 1 for re-runnability of this script
par(mfrow = c(1, 1))

# scatter.smooth(x=uk_inputdata$total_vaccinations_per_hundred, 
#     y=uk_inputdata$deathToNewCase, main="total_vaccinations_per_hundred - deathToNewCase")

plot(x=uk_inputdata$total_vaccinations_per_hundred, y=uk_inputdata$deathToNewCase,
           col=3, xlab="Total Vaccinations per 100", 
           ylab="New Daily Deaths / New Daily Cases", 
           main="UK: Total Vaccinations per 100 - Death-Case Log")

abline(lm(uk_inputdata$deathToNewCase ~ uk_inputdata$total_vaccinations_per_hundred))

# Observation of Graph:
# The results of the graph seem to validate the strategy to vaccinate the most vulnerable earler. 
# That is, less people die from covid when they get infected because of the order in which people are 
# vaccinated began with those who have a higher likelihood to die from covid. The
# base 10 exponential value of new deaths to # new cases becomes smaller as the total vaccinations 
# per hundred increases over time.

#
# *** Part 1: create a linear model and test hypothesis of its validity ***
#

lmResultsV <- lm(deathToNewCase ~ total_vaccinations_per_hundred, uk_inputdata)
summary(lmResultsV)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.94135 -0.16759  0.06412  0.19022  0.55087 

# Coefficients:
#                                  Estimate Std. Error t value Pr(>|t|)
# (Intercept)                    -1.1388747  0.0407441  -27.95   <2e-16 ***
# total_vaccinations_per_hundred -0.0165343  0.0005728  -28.86   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.2817 on 182 degrees of freedom
#   (2 observations deleted due to missingness)
# Multiple R-squared:  0.8207,    Adjusted R-squared:  0.8197
# F-statistic: 833.2 on 1 and 182 DF,  p-value: < 2.2e-16

# Add stringency index to see if model improves
lmResultsVSI <- lm(deathToNewCase ~ total_vaccinations_per_hundred + stringency_index, uk_inputdata)
summary(lmResultsVSI)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -0.90429 -0.15299  0.05452  0.18275  0.53162

# Coefficients:
#                                Estimate Std. Error t value Pr(>|t|)
# (Intercept)                    -2.16711    0.50160  -4.320 2.56e-05 ***
# total_vaccinations_per_hundred -0.01233    0.00212  -5.818 2.65e-08 ***
# stringency_index                0.01104    0.00537   2.057   0.0412 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.2792 on 181 degrees of freedom
#   (2 observations deleted due to missingness)
# Multiple R-squared:  0.8248,    Adjusted R-squared:  0.8229
# F-statistic: 426.1 on 2 and 181 DF,  p-value: < 2.2e-16

#
# *** Model Observations ***
#
# Significance of stringency_index variable:
# ==========================================
# Adding additional variable, stringency_index, does not improve the model a great deal.
# 1. the p value for stringency_index is 0.04 which is acceptable
# 2. the Adjusted R-squared goes from 0.8197 to 0.8229
# This is not significant enough to justify the addition of this variable.
# 
# Model validity (reject null hypothesis or not)
# For the provided F-statistic of the lmResultsV model, because the p-value of 2.2e - 16 is small, 
# the null hypothesis should be rejected (i.e., the model is valid).

# Look at confidence intervals on the parameters
confint(lmResultsV, level = .95)
#                                      2.5 %      97.5 %
# (Intercept)                    -1.21926632 -1.05848313
# total_vaccinations_per_hundred -0.01766448 -0.01540407
#
# The estimated value of the total_vaccinations_per_hundred parameter from the lmResultsV model
# was -0.0165343. the corresponding 95% confidence interval is (-0.0177, -0.0154).



# 
# *** Part 2: Use Decision Tree ***
# Refer to module 7 hands on exercise and chapter 7 section 7.1 Decision Trees
#
library(rpart)
set.seed(515)
#
# Definitions:
#
# Manageable = yes
# deathToNewCase = log10(uk_inputdata$new_deaths / (uk_inputdata$new_cases+1)) is less than -2
# Manageable = no
# deathToNewCase = log10(uk_inputdata$new_deaths / (uk_inputdata$new_cases+1)) is greater than or equal to -2

# install required package for enhanced plotting
#install.packages("rpart.plot")

library("rpart.plot")

uk_inputdata$manageable <- ifelse(uk_inputdata$deathToNewCase < -2, "yes", "no")
head(uk_inputdata)
#uk_inputdata

# Look at the distribution of YES and NO
table(uk_inputdata$manageable)
# NO YES
# 92  92 

# Fit the decision tree model
DT <- rpart(
    manageable ~ total_vaccinations_per_hundred,
    method="class",
    data = uk_inputdata
)

# Plot the decision tree
rpart.plot(DT, type=4, extra=1)

# Try and predict using decision tree
newdata_expect_no <- data.frame(total_vaccinations_per_hundred=10)
newdata_expect_yes <- data.frame(total_vaccinations_per_hundred=75)

# Confirm that result is 'no' (i.e., COVID new death to new case ratrio is not at a manageable level)
predict(DT, newdata=newdata_expect_no,type="class")

# Confirm that result is 'yes' (i.e., COVID new death to new case ratrio is not at a manageable level)
predict(DT, newdata=newdata_expect_yes,type="class")

# Try and predict using decision tree
newdata_expect_no <- data.frame(total_vaccinations_per_hundred=10)
newdata_expect_yes <- data.frame(total_vaccinations_per_hundred=75)

# Confirm that result is 'no' (i.e., COVID new death to new case ratrio is not at a manageable level)
predict(DT, newdata=newdata_expect_no,type="class")

# Confirm that result is 'yes' (i.e., COVID new death to new case ratrio is not at a manageable level)
predict(DT, newdata=newdata_expect_yes,type="class")