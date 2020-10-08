library(plm)
library(panelvar)
library(dplyr)

path <-'/Users/Lencer/Documents/DS4A/DS4A_COVID'
# load
w_data <- read.csv(file=paste(path,'/lagged_data_weekly.csv', sep = ''), header=TRUE, sep=",")
# select data for relevant states
states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina','Oklahoma', 'Wisconsin')
state_ind <- c(sapply(states, function(x) which(data$state == x)))
relevant_states <- w_data[state_ind,]
#head(relevant_states)

# prep data for plm
plm_rel_states <- plm::pdata.frame(relevant_states, index=c("sub_region_1","week_start"), drop.index=TRUE, row.names=TRUE)
head(plm_rel_states)
head(attr(plm_rel_states, "index"))
#heading = colnames(relevant_states[5:11])
#sub_data$building_percentage = lag(sub_data$building_percentage,2)
# run plm
test <- plm(new_cases~retail_recreation+grocery_pharmacy+parks+transit_stations+workplaces+residential, data = relevant_states, model = "within")
# results plm
summary(test)

### parks, workplaces and residential areas are the ones that are relevant
### R-sq = 0.31927, Adj R-sq= 0.26885

# run plm with significant mobility metrics
test <- plm(new_cases~parks+workplaces+residential, data = relevant_states, model = "within")
# results plm
summary(test)

### R-sq = 0.30505, 0.26715


#############################################################################
# panelvar analysis

library(panelvar)
#data(relevant_states)
pvar_test <- pvargmm(dependent_vars = c("new_cases"),
                     lags = 3,
                     exog_vars = c('parks','workplaces','residential'),
                     data = relevant_states,
                     panel_identifier = c("sub_region_1","week_start"),
                     steps = c("twostep"))
summary(pvar_test)

## third lag is insignificant