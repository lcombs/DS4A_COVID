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

#ccf(x,y, type = 'correlation')
###make a panel dataframe
sub_data <- pdata.frame(temp_data_sub, index=c("state"), drop.index=TRUE, row.names=TRUE)
View(sub_data)
#head(sub_data)
head(attr(sub_data, "index"))
#change the lag to what your ccf plots say for all the variables below:
sub_data$building_percentage = plm::lag(sub_data$building_percentage,4)
sub_data$grocery_and_pharmacy_percent_change_from_baseline = plm::lag(sub_data$grocery_and_pharmacy_percent_change_from_baseline,3)
sub_data$transit_stations_percent_change_from_baseline = plm::lag(sub_data$transit_stations_percent_change_from_baseline,3)
sub_data$retail_and_recreation_percent_change_from_baseline = plm::lag(sub_data$retail_and_recreation_percent_change_from_baseline,3)
sub_data$parks_percent_change_from_baseline = plm::lag(sub_data$parks_percent_change_from_baseline,0)
sub_data$workplaces_percent_change_from_baseline = plm::lag(sub_data$workplaces_percent_change_from_baseline,4)
sub_data$residential_percent_change_from_baseline = plm::lag(sub_data$residential_percent_change_from_baseline,4)

# one step ahead prediction
f<- pFormula(new_cases_scaled ~ building_percentage + 
               workplaces_percent_change_from_baseline + 
               retail_and_recreation_percent_change_from_baseline+
               grocery_and_pharmacy_percent_change_from_baseline +
               transit_stations_percent_change_from_baseline + 
               parks_percent_change_from_baseline + 
               residential_percent_change_from_baseline)

train_start_date = '2020-04-08'
train_end_date = '2020-05-13'
test_end_date = as.Date(train_end_date) + 14

n <- (length(which(as.Date(sub_data$week_start)>=train_end_date))/7) - 2
mean_abs_diff <- vector("list", length = n)
scaled_data <- vector("list", length = n)

for (i in 1:n){
  # update start and end dates
  train_start_date = as.Date(train_start_date) + 7
  train_end_date = as.Date(train_end_date) + 7
  test_end_date = as.Date(test_end_date) + 7
  
  train_ind = which(as.Date(sub_data$week_start)>train_start_date & as.Date(sub_data$week_start)< train_end_date)#set training dates like we agreed on
  train_sub_data = sub_data[train_ind,]
  test_ind = which(as.Date(sub_data$week_start)>=train_end_date & as.Date(sub_data$week_start)< test_end_date)
  test_sub_data = sub_data[test_ind,]
  
  #runs a pooled OLS fixed effects model. Change model to 'within' for a regular fixed effect
  simple <- plm(new_cases_scaled~
                  building_percentage+
                  retail_and_recreation_percent_change_from_baseline+
                  grocery_and_pharmacy_percent_change_from_baseline+
                  parks_percent_change_from_baseline+
                  transit_stations_percent_change_from_baseline+
                  workplaces_percent_change_from_baseline+
                  residential_percent_change_from_baseline,
                data = train_sub_data, model = "within")
  
  # predict
  test_sub_data = test_sub_data[,-c(2:4, 12:14)]
  pred = predict.out.plm(simple, f, test_sub_data, model = 'within', pname = 'new_cases_scaled')
  
  # store the predicted values
  scaled_data[i] = list(pred$df$new_cases_scaled.l.hat)
  mean_abs_diff[i] = mean(abs(pred$df$new_cases_scaled - pred$df$new_cases_scaled.l.hat))
}

pred_vals = vector("list", length = length(scaled_data))
for (i in 1:length(scaled_data)){
  new_list = vector("list", length = 7)
  k = 1 # keep track of indices for new_list
  for (j in seq(1,length(scaled_data[[1]]), by = 2)){
    new_list[k] = scaled_data[[i]][j]
    k = k + 1
  }
  pred_vals[i] = new_list
}

#prediction::prediction(simple)
#ind = which(attr(sub_data,'index')=="California")
#predict(simple, sub_data[ind,])
#print(sub_data$new_cases_scaled[ind])

simple <- plm(new_cases_scaled~
                building_percentage+
                retail_and_recreation_percent_change_from_baseline+
                grocery_and_pharmacy_percent_change_from_baseline+
                parks_percent_change_from_baseline+
                transit_stations_percent_change_from_baseline+
                workplaces_percent_change_from_baseline+
                residential_percent_change_from_baseline,
              data = sub_data, model = "pooling")
summary(simple)

# with pooled model
### building percentage, workplaces and residential areas are the ones that are 
### significant at 0.001% level
### transit stations, parks are significant at 1% level
### R-sq = 0.4085, Adj R-sq= 0.38315

# with fixed effects model
### workplaces and residential areas are the ones that are significant  
### at 0.001% level
### transit stations, retail and recreation are significant at 0.1% level
### R-sq = 0.58658, Adj R-sq= 0.54956

# run plm with significant mobility metrics -- fixed effects model
simple_sig <- plm(new_cases_scaled~
              retail_and_recreation_percent_change_from_baseline+
              transit_stations_percent_change_from_baseline+
              workplaces_percent_change_from_baseline+
              residential_percent_change_from_baseline,
            data = sub_data, model = "within")
# results plm
summary(simple_sig)

### R-sq = 0.58058, 0.54974

#plot the variables for comparison
ind = which(attr(sub_data,'index')=='California')
par(mfrow=c(3,1))
plot(sub_data$workplaces_percent_change_from_baseline[ind], col='red', type='l')
plot(sub_data$residential_percent_change_from_baseline[ind], col='red', type='l')
plot(sub_data$new_cases_scaled[ind], type = 'l')




#############################################################################
# panelvar analysis
# first peak
library(panelvar)
train_ind = which(as.Date(sub_data$week_start)> '2020-04-08' & as.Date(temp_data_sub$week_start)<'2020-06-08')#set training dates like we agreed on
train_data_sub = temp_data_sub[train_ind,]
#View(train_data_sub)
colnames(train_data_sub) = c( "state", "week_start", "country_region_code", "country_region",                                    
                              "iso_3166_2_code","building_percentage", 'retail_and_recreation', 'grocery_and_pharmacy',
                              'parks', 'transit_stations', 'workplaces', 'residential', 'cumulative_cases', 'new_cases', 'population',
                              'new_cases_scaled')
cali_group_pvar_pk1 <- pvargmm(dependent_vars = c("new_cases_scaled",
                                              "building_percentage", 
                                              "workplaces"),
                           lags = 4,
                           transformation = "fod",
                           exog_vars = c("retail_and_recreation",
                                         "grocery_and_pharmacy", 
                                         "parks",                
                                         "transit_stations",     
                                         "residential"),
                           data = train_data_sub,
                           panel_identifier=c("state", "week_start"),
                           steps = c("twostep")
)
summary(cali_group_pvar_pk1)


#Examine stability of process. If the modulus of th eigenvalues are less than 1, we have stationarity
stab_cali_group_pvar_pk1 <- stability(cali_group_pvar_pk1)
print(stab_cali_group_pvar_pk1) #these should all be within the unit circle

#Generate impulse response functions
cali_group_pvar_pk1_oirf <- oirf(cali_group_pvar_pk1, n.ahead = 8)
cali_group_pvar_pk1_girf <- girf(cali_group_pvar_pk1, n.ahead = 8, ma_approx_steps= 8)
#Generate bootstrap confidence intervals for impulse response functions. This takes a while to run. LET'S RUN THIS AS A LAST RESORT
# cali_group_pvar_bs <- bootstrap_irf(cali_group_pvar, typeof_irf = c("GIRF"),
#                                       n.ahead = 8,
#                                       nof_Nstar_draws = 100,
#                                       confidence.band = 0.95)
#plot the impulse response functions
plot(cali_group_pvar_pk1_girf)
plot(cali_group_pvar_pk1_oirf)


# second peak
train_ind = which(as.Date(sub_data$week_start)> '2020-06-08' & as.Date(temp_data_sub$week_start)<'2020-08-08')#set training dates like we agreed on
train_data_sub = temp_data_sub[train_ind,]
#View(train_data_sub)
colnames(train_data_sub) = c( "state", "week_start", "country_region_code", "country_region",                                    
                              "iso_3166_2_code","building_percentage", 'retail_and_recreation', 'grocery_and_pharmacy',
                              'parks', 'transit_stations', 'workplaces', 'residential', 'cumulative_cases', 'new_cases', 'population',
                              'new_cases_scaled')
cali_group_pvar_pk2 <- pvargmm(dependent_vars = c("new_cases_scaled",
                                                  "building_percentage", 
                                                  "workplaces"),
                               lags = 4,
                               transformation = "fod",
                               exog_vars = c("retail_and_recreation",
                                             "grocery_and_pharmacy", 
                                             "parks",                
                                             "transit_stations",     
                                             "residential"),
                               data = train_data_sub,
                               panel_identifier=c("state", "week_start"),
                               steps = c("twostep")
)
summary(cali_group_pvar_pk2)


#Examine stability of process. If the modulus of th eigenvalues are less than 1, we have stationarity
stab_cali_group_pvar_pk2 <- stability(cali_group_pvar_pk2)
print(stab_cali_group_pvar_pk2) #these should all be within the unit circle

#Generate impulse response functions
cali_group_pvar_pk2_oirf <- oirf(cali_group_pvar_pk2, n.ahead = 8)
cali_group_pvar_pk2_girf <- girf(cali_group_pvar_pk2, n.ahead = 8, ma_approx_steps= 8)

# plot the impulse functions
plot(cali_group_pvar_pk2_girf)

#Generate bootstrap confidence intervals for impulse response functions. This takes a while to run. LET'S RUN THIS AS A LAST RESORT
# cali_group_pvar_pk1_bs <- bootstrap_irf(cali_group_pvar_pk1, typeof_irf = c("GIRF"),
#                                       n.ahead = 8,
#                                       nof_Nstar_draws = 100,
#                                       confidence.band = 0.95)

