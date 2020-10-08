library(plm)
library(panelvar)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- getwd()

###Load the data 
data <- read.csv(file=paste(path,'/all_data_weekly.csv', sep = ''), header=TRUE, sep=",")
#Let's do some additional transformations so everything is comparable:
data[,7:12] = data[,7:12] + 100 #make the mobility data positive percentages to match the building data
data[,6] = data[,6]*100 #make buildings a percentage
data$new_cases_scaled = data$new_cases/(data$population/100000) #make cases per hundred thousand


states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina', 'Oklahoma', 'Wisconsin')

sub = c(sapply(states, function(x) which(data$state == x)))
temp_data_sub = data[sub,]

###View CCF plot for a state of interest
state_of_interest = 'California'
x = ts(temp_data_sub$new_cases_scaled[which(temp_data_sub$state==state_of_interest)])
y = ts(temp_data_sub[which(temp_data_sub$state==state_of_interest),6:12])
labels = colnames(y)

par(mfrow = c(1,1))
for(i in 1:7){ #this loop plots the ccf plot for all variables. click the left arrow under "plots" in the plotting window to scroll through them
  plot(ccf(x,y[,i]), main = paste('new cases &', labels[i])) 
}

ccf(x,y, type = 'correlation')
###make a panel dataframe
sub_data <- pdata.frame(temp_data_sub, index=c("state","week_start"), drop.index=TRUE, row.names=TRUE)
View(sub_data)
head(sub_data)
head(attr(sub_data, "index"))
#change the lag to what your ccf plots say for all the variables below:
sub_data$building_percentage = plm::lag(sub_data$building_percentage,4)
sub_data$grocery_and_pharmacy_percent_change_from_baseline = plm::lag(sub_data$grocery_and_pharmacy_percent_change_from_baseline,3)
sub_data$transit_stations_percent_change_from_baseline = plm::lag(sub_data$transit_stations_percent_change_from_baseline,3)
sub_data$retail_and_recreation_percent_change_from_baseline = plm::lag(sub_data$retail_and_recreation_percent_change_from_baseline,3)
sub_data$parks_percent_change_from_baseline = plm::lag(sub_data$parks_percent_change_from_baseline,0)
sub_data$workplaces_percent_change_from_baseline = plm::lag(sub_data$workplaces_percent_change_from_baseline,4)
sub_data$residential_percent_change_from_baseline = plm::lag(sub_data$residential_percent_change_from_baseline,4)

#runs a pooled OLS fixed effects model. Change model to 'within' for a regular fixed effect
simple <- plm(new_cases_scaled~
                building_percentage+
                retail_and_recreation_percent_change_from_baseline+
                transit_stations_percent_change_from_baseline+
                parks_percent_change_from_baseline+
                workplaces_percent_change_from_baseline+
                residential_percent_change_from_baseline,#+,
              data = sub_data, model = "pooling")


summary(simple)

### transit stations, workplaces and residential areas are the ones that are relevant
### R-sq = 0.32454, Adj R-sq= 0.2745

# run plm with significant mobility metrics
test <- plm(new_cases~transit_stations+workplaces+residential, data = relevant_states, model = "within")
# results plm
summary(test)

### R-sq = 0.3132, 0.27574


#############################################################################
# panelvar analysis

library(panelvar)
#data(relevant_states)
pvar_test <- pvargmm(dependent_vars = c("new_cases"),
                     lags = 2,
                     exog_vars = c('transit_stations','workplaces','residential'),
                     data = relevant_states,
                     panel_identifier = c("sub_region_1","week_start"),
                     steps = c("twostep"))
summary(pvar_test)

## third lag is insignificant


data <- read.csv(file=paste(path,'/all_data_weekly.csv', sep = ''), header=TRUE, sep=",")
# select data for relevant states
states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina','Oklahoma', 'Wisconsin')
state_ind <- c(sapply(states, function(x) which(data$state == x)))
relevant_states <- data[state_ind,]
View(relevant_states)

#heading = colnames(relevant_states[5:11])
relevant_states$building_percentage = lag(relevant_states$building_percentage,2)
