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


states <- c('Colorado', 'Illinois', 'Indiana', 'Maryland', 'Michigan', 'Nebraska', 'Ohio', 'Pennsylvania', 'Virginia', 'Washington')
states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina', 'Oklahoma', 'Wisconsin')
states <- c('District of Columbia', 'Massachusetts', 'New Jersey', 'New York')
states <- c('Alabama', 'Arizona', 'Florida', 'Georgia', 'Nevada', 'Texas')
 
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
sub_data <- pdata.frame(temp_data_sub, index=c("state"), drop.index=TRUE, row.names=TRUE)

#change the lag to what your ccf plots say for all the variables below:
sub_data$building_percentage = lag(sub_data$building_percentage,4)
sub_data$grocery_and_pharmacy_percent_change_from_baseline = lag(sub_data$grocery_and_pharmacy_percent_change_from_baseline,3)
sub_data$transit_stations_percent_change_from_baseline = lag(sub_data$transit_stations_percent_change_from_baseline,3)
sub_data$retail_and_recreation_percent_change_from_baseline = lag(sub_data$retail_and_recreation_percent_change_from_baseline,3)
sub_data$parks_percent_change_from_baseline = lag(sub_data$parks_percent_change_from_baseline,0)
sub_data$workplaces_percent_change_from_baseline = lag(sub_data$workplaces_percent_change_from_baseline,4)
sub_data$residential_percent_change_from_baseline = lag(sub_data$residential_percent_change_from_baseline,4)

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

#plot the variables for comparison
ind = which(attr(sub_data,'index')=='California')
par(mfrow=c(3,1))
plot(sub_data$parks_percent_change_from_baseline[ind], col='red', type='l')
plot(sub_data$building_percentage[ind], col='red', type='l')
plot(sub_data$new_cases_scaled[ind], type = 'l')


#############PVAR portion

help(pvargmm)

#example 1: building the model
#This part is giving me a lot of trouble. I don't think we can have that many variables. Go ahead and take out the variables 
#or add them in one by one? Set the lag to the maximum lag identified for any variable in the ccf plot for this group.

train_ind = which(as.Date(temp_data_sub$week_start)<'2020-06-15')#set training dates like we agreed on
train_data_sub = temp_data_sub[train_ind,]
cali_group_pvar <- pvargmm(dependent_vars = c("building_percentage", "workplaces_percent_change_from_baseline", 
                                   "residential_percent_change_from_baseline", "new_cases_scaled", "transit_stations_percent_change_from_baseline",
                                   'parks_percent_change_from_baseline', 'retail_and_recreation_percent_change_from_baseline'),
                                lags = 4,
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

summary(cali_group_pvar)


#Example 2: lag selection using Andrew Lu MMSC method. Will return AIC, BIC, and HQIC. Smaller values win
Andrews_Lu_MMSC(ex1_dahlberg_data)

#Examine stability of process. If the modulus of th eigenvalues are less than 1, we have stationarity
stab_ex1_dahlberg_data <- stability(ex1_dahlberg_data)
print(stab_ex1_dahlberg_data)

#Generate impulse response functions
ex1_dahlberg_data_oirf <- oirf(ex1_dahlberg_data, n.ahead = 8)
ex1_dahlberg_data_girf <- girf(ex1_dahlberg_data, n.ahead = 8, ma_approx_steps= 8)
#Generate bootstrap confidence intervals for impulse response functions. This takes a while to run.
ex1_dahlberg_data_bs <- bootstrap_irf(ex1_dahlberg_data, typeof_irf = c("GIRF"),
                                      n.ahead = 8,
                                      nof_Nstar_draws = 500,
                                      confidence.band = 0.95)
#plot the impulse response functions
plot(ex1_dahlberg_data_girf, ex1_dahlberg_data_bs)


