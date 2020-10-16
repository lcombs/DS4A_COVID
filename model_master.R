library(plm)
library(panelvar)
library(tidyr)

###### Lisa ###### 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- getwd()

###Load the data 
data <- read.csv(file=paste(path,'/all_data_weekly.csv', sep = ''), header=TRUE, sep=",")
#Let's do some additional transformations so everything is comparable:
data[,7:12] = data[,7:12] + 100 #make the mobility data positive percentages to match the building data
data[,6] = data[,6]*100 #make buildings a percentage
data$new_cases_scaled = data$new_cases/(data$population/100000) #make cases per hundred thousand


states <- c('Colorado', 'Illinois', 'Indiana', 'Maryland', 'Michigan', 'Nebraska', 'Ohio', 'Pennsylvania', 'Virginia', 'Washington')
#states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina', 'Oklahoma', 'Wisconsin')
#states <- c('District of Columbia', 'Massachusetts', 'New Jersey', 'New York')
#states <- c('Alabama', 'Arizona', 'Florida', 'Georgia', 'Nevada', 'Texas')

sub = c(sapply(states, function(x) which(data$state == x)))
temp_data_sub = data[sub,]

###View CCF plot for a state of interest
state_of_interest = 'Colorado'
x = ts(temp_data_sub$new_cases_scaled[which(temp_data_sub$state==state_of_interest)])
y = ts(temp_data_sub[which(temp_data_sub$state==state_of_interest),6:12])
labels = colnames(y)

par(mfrow = c(1,1))
for(i in 1:7){ #this loop plots the ccf plot for all variables. click the left arrow under "plots" in the plotting window to scroll through them
        plot(ccf(x,y[,i]), main = paste('new cases &', labels[i])) 
}

###make a panel dataframe
sub_data <- pdata.frame(temp_data_sub, index=c("state"), drop.index=TRUE, row.names=TRUE)

#change the lag to what your ccf plots say for all the variables below:
lag = 4

sub_data$building_percentage = plm::lag(sub_data$building_percentage,lag)
sub_data$grocery_and_pharmacy_percent_change_from_baseline = plm::lag(sub_data$grocery_and_pharmacy_percent_change_from_baseline,lag)
sub_data$transit_stations_percent_change_from_baseline = plm::lag(sub_data$transit_stations_percent_change_from_baseline,lag)
sub_data$retail_and_recreation_percent_change_from_baseline = plm::lag(sub_data$retail_and_recreation_percent_change_from_baseline,lag)
sub_data$parks_percent_change_from_baseline = plm::lag(sub_data$parks_percent_change_from_baseline,lag)
sub_data$workplaces_percent_change_from_baseline = plm::lag(sub_data$workplaces_percent_change_from_baseline,lag)
sub_data$residential_percent_change_from_baseline = plm::lag(sub_data$residential_percent_change_from_baseline,lag)

#Separate testing and training
train_ind = which(as.Date(sub_data$week_start)<'2020-06-15')#set training dates like we agreed on
test_ind = which(as.Date(sub_data$week_start)>='2020-06-15')
sub_data_train = sub_data[train_ind,]
sub_data_test = sub_data[test_ind,]

#runs a pooled OLS fixed effects model. Change model to 'within' for a regular fixed effect
simple <- plm(new_cases_scaled~
                      building_percentage+
                      workplaces_percent_change_from_baseline+
                      retail_and_recreation_percent_change_from_baseline+
                      transit_stations_percent_change_from_baseline+
                      parks_percent_change_from_baseline+
              residential_percent_change_from_baseline,
              data = sub_data_train, model = "pooling")


summary(simple)

#prediction
ind = which(attr(sub_data_train,'index')=='California')
predict(simple, sub_data_train[ind,])
print(sub_data_train$new_cases_scaled[ind])

ind = which(attr(sub_data_test,'index')=='California')
predict(simple, sub_data_test[ind,])
print(sub_data_test$new_cases_scaled[ind])

#plot the variables for comparison
par(mfrow=c(3,1))
plot(sub_data$workplaces_percent_change_from_baseline[ind], col='red', type='l')
plot(sub_data$residential_percent_change_from_baseline[ind], col='red', type='l')
plot(sub_data$new_cases_scaled[ind], type = 'l')


#############PVAR portion


#Separate the training and testing data for the PVAR
#train_ind = which(as.Date(temp_data_sub$week_start)>'2020-04-13' & as.Date(temp_data_sub$week_start)<='2020-06-08')
train_ind = which(as.Date(temp_data_sub$week_start)>'2020-06-08' & as.Date(temp_data_sub$week_start)<='2020-08-03')
#test_ind = which(as.Date(temp_data_sub$week_start)>='2020-06-15')
train_data_sub = temp_data_sub[train_ind,]
#test_ind = temp_data_sub[test_ind,]

#rename the columns to not be so long
colnames(train_data_sub) = c( "state", "week_start", "country_region_code", "country_region",                                    
                              "iso_3166_2_code","building_percentage", 'retail_and_recreation', 'grocery_and_pharmacy',
                              'parks', 'transit_stations', 'workplaces', 'residential', 'cumulative_cases', 'new_cases', 'population',
                              'new_cases_scaled')


#run the pvar. Change the name of the variable to your state group
group_pvar <- pvargmm(dependent_vars = c("workplaces", 
                                            "new_cases_scaled", "building_percentage"),
                         lags = 4,
                         exog_vars = c('retail_and_recreation', 'grocery_and_pharmacy','parks',
                                       'transit_stations', 'residential'),
                         transformation = "fod",
                         data = train_data_sub,
                         panel_identifier=c("state", "week_start"),
                         steps = c("twostep"),
                         system_instruments = FALSE,
                         max_instr_dependent_vars = 99,
                         max_instr_predet_vars = 99,
                         min_instr_dependent_vars = 2L,
                         min_instr_predet_vars = 1L,
                         collapse = FALSE
)

summary(group_pvar)

#pvar_coef <- panelvar::extract(dc_group_pvar)


#Examine stability of process. If the modulus of th eigenvalues are less than 1, we have stationarity
stab_group_pvar <- stability(group_pvar)
print(stab_group_pvar) #these should all be within the unit circle

#Generate impulse response functions
group_pvar_oirf <- oirf(group_pvar, n.ahead = 8)
group_pvar_girf <- girf(group_pvar, n.ahead = 8, ma_approx_steps= 8)
#Generate bootstrap confidence intervals for impulse response functions. This takes a while to run. LET'S RUN THIS AS A LAST RESORT
# dc_group_pvar_bs <- bootstrap_irf(dc_group_pvar, typeof_irf = c("GIRF"),
#                                       n.ahead = 8,
#                                       nof_Nstar_draws = 100,
#                                       confidence.band = 0.95)
#plot the impulse response functions
plot(group_pvar_girf)
plot(group_pvar_oirf)


### 

