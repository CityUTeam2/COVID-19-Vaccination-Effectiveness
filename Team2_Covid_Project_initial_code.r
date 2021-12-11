# Import data from covid dataset
inputData <- read.csv("owid-covid-data_2021_11_28.csv")

# 1. Filter on USA data only
# 2. Only include the following columns
#       stringency_index
#       total_vaccinations_per_hundred
#       new_cases
#       new_deaths_per_million
# 3. Filter on new_deaths_per_million > 0 as we are focused on 
#    seeing how effective vaccinations are against minimizing mortality rates.
# 4. Replace remaining NA with 0 for stringency_index and 
usa_inputdata <- subset(inputData, 
    select = c("total_vaccinations_per_hundred", "new_deaths", "new_deaths_per_million","new_cases","stringency_index"), 
    location == "United States" & new_deaths_per_million > 0)

usa_inputdata <- subset(inputData, 
    select = c("total_vaccinations_per_hundred", "new_deaths", "new_deaths_per_million","new_cases","stringency_index"), 
    location == "United Kingdom" & new_deaths_per_million > 0)

# check out the structure of our dataframe and confirm that they are all num
str(usa_inputdata)

# view data and look for what else needs to be cleaned
head(usa_inputdata)

# Replace NA with 0 for total_vaccinations_per_hundred column and total_vaccinations_per_hundred
# because they would be 0 when the deaths first began
#usa_inputdata$total_vaccinations_per_hundred[is.na(usa_inputdata$total_vaccinations_per_hundred)] <- 0
usa_inputdata[is.na(usa_inputdata)] <- 0

# confirm that NAs have been replaced by zeros - this should return 0 rows
usa_inputdata[rowSums(is.na(usa_inputdata)) > 0, ] 

# The results of the graph seem to validate the strategy to vaccinate the most vulnerable earler. 
# That is, less people die from covid when they get infected because of the order in which people are 
# vaccinated began with those who have a higher likelihood to die from covid. The ratio of new deaths 
# to # new cases goes down as the total vaccinations per hundred increases over time.

#
# Dependent variable: deathToNewCase
# *** Need to weigh number of new cases with new deaths to get a fair measure of vaccination effectiveness. ***
# To measure the effectiveness of our vaccine we will determine if the # vaccinations per 100 
# reduces the mortality by new cases: deathToNewCase = new_deaths / new_cases
# Independent (input) variables: total_vaccinations_per_hundred
# Take log10 to get a more distinguishable trend. Added 1 to denom to avoid div by 0.
usa_inputdata$deathToNewCase <- log10(usa_inputdata$new_deaths / (usa_inputdata$new_cases+1))

#usa_inputdata

scatter.smooth(x=usa_inputdata$total_vaccinations_per_hundred, 
    y=usa_inputdata$deathToCaseRatio, main="total_vaccinations_per_hundred - deathToNewCase")

plot(x=usa_inputdata$total_vaccinations_per_hundred, y=usa_inputdata$deathToNewCase,
           col=3, xlab="total vaccinations per hundred", 
           ylab="New Daily Deaths / New Daily Cases", 
           main="Total Vaccinations per 100 - New Daily Deaths / New Daily Cases")

abline(lm(usa_inputdata$deathToNewCase ~ usa_inputdata$total_vaccinations_per_hundred))

# TODO: 
# 1. Divide data into training / test. Fit the model and validate with test data.
# 2. Measure how good the model is, etc.

