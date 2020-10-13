library(plm)
library(panelvar)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- getwd()
source(paste(path,'/plm_predict_function.R', sep = ''))


###Load the data 
data <- read.csv(file=paste(path,'/all_data_weekly.csv', sep = ''), header=TRUE, sep=",")
#Let's do some additional transformations so everything is comparable:
data[,7:12] = data[,7:12] + 100 #make the mobility data positive percentages to match the building data
data[,6] = data[,6]*100 #make buildings a percentage
data$new_cases_scaled = data$new_cases/(data$population/100000) #make cases per hundred thousand

###plot all states
ggplot(data=data, aes(x=as.Date(week_start), y=new_cases_scaled, group=state)) +
  geom_line(aes(colour=state))+
  labs(x = 'Date', y = 'New Cases per 100,000 People')+
  theme_bw()+
  theme(legend.position="bottom", legend.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.title=element_blank(), 
        legend.margin = margin(-1,-1,-1,-1),
        panel.border = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))
dev.copy(jpeg,"All_states.jpg",width=8,height=6,units="in",res=200)
dev.off()

#choose which group of states you want to look at
states <- c('Colorado', 'Illinois', 'Indiana', 'Maryland', 'Michigan', 'Nebraska', 'Ohio', 'Pennsylvania', 'Virginia', 'Washington')
states <- c('California', 'Idaho', 'Iowa', 'Missouri', 'North Carolina', 'Oklahoma', 'Wisconsin')
states <- c('District of Columbia', 'Massachusetts', 'New Jersey', 'New York')
states <- c('Alabama', 'Arizona', 'Florida', 'Georgia', 'Nevada', 'Texas')

sub = c(sapply(states, function(x) which(data$state == x)))
temp_data_sub = data[sub,]

###View CCF plot for a state of interest
state_of_interest = 'Wisconsin'
x = ts(temp_data_sub$new_cases_scaled[which(temp_data_sub$state==state_of_interest)])
y = ts(temp_data_sub[which(temp_data_sub$state==state_of_interest),6:12])
labels = colnames(y)

plot(ccf(x,y[,1]), main = paste('CCF plot of New Cases vs Building Entry Percentage'))
par(mfrow = c(1,1))
for(i in 1:7){ #this loop plots the ccf plot for all variables. click the left arrow under "plots" in the plotting window to scroll through them
  plot(ccf(x,y[,i]), main = paste('new cases &', labels[i])) 
}

###make a panel dataframe
sub_data <- pdata.frame(temp_data_sub, index=c("state"), drop.index=TRUE, row.names=TRUE)

#change the lag to what your ccf plots say for all the variables below:
lag = 3

sub_data$building_percentage = plm::lag(sub_data$building_percentage,lag)
sub_data$grocery_and_pharmacy_percent_change_from_baseline = plm::lag(sub_data$grocery_and_pharmacy_percent_change_from_baseline,lag)
sub_data$transit_stations_percent_change_from_baseline = plm::lag(sub_data$transit_stations_percent_change_from_baseline,lag)
sub_data$retail_and_recreation_percent_change_from_baseline = plm::lag(sub_data$retail_and_recreation_percent_change_from_baseline,lag)
sub_data$parks_percent_change_from_baseline = plm::lag(sub_data$parks_percent_change_from_baseline,lag)
sub_data$workplaces_percent_change_from_baseline = plm::lag(sub_data$workplaces_percent_change_from_baseline,lag)
sub_data$residential_percent_change_from_baseline = plm::lag(sub_data$residential_percent_change_from_baseline,lag)

# 
# #Separate testing and training
# weeks = unique(sub_data$week_start)
# for(i in 10:17){
#   train_ind  = which(as.Date(sub_data$week_start)>weeks[8] & as.Date(sub_data$week_start)<weeks[8+i])
#   test_ind = which(as.Date(sub_data$week_start)== weeks[9+i])
#   print(train_ind)
#   print(test_ind)
# }
# train_ind = which(as.Date(sub_data$week_start)>'2020-04-15' & as.Date(sub_data$week_start)<'2020-07-1')#set training dates like we agreed on
# test_ind = which(as.Date(sub_data$week_start)>='2020-07-1')
# sub_data_train = sub_data[train_ind,]
# sub_data_test = sub_data[test_ind,]
# 
# #runs a pooled OLS fixed effects model. Change model to 'within' for a regular fixed effect
# simple <- plm(new_cases_scaled~
#                 building_percentage+
#                 workplaces_percent_change_from_baseline+
#                 retail_and_recreation_percent_change_from_baseline+
#                 transit_stations_percent_change_from_baseline+
#                 parks_percent_change_from_baseline+
#                 residential_percent_change_from_baseline,
#               data = sub_data_train, model = "within")
# 
# 
# summary(simple)
# 
# 
# f<- pFormula(new_cases_scaled ~ building_percentage + workplaces_percent_change_from_baseline + + retail_and_recreation_percent_change_from_baseline+
#                transit_stations_percent_change_from_baseline + parks_percent_change_from_baseline + residential_percent_change_from_baseline)
# 
# #prediction
# ind = which(attr(sub_data_train,'index')=='New York')
# predict(simple, sub_data_train[ind,])
# print(sub_data_train$new_cases_scaled[ind])
# 
# ind = which(attr(sub_data_test,'index')=='New York')
# sub_data_test = sub_data_test[,-c(2:4, 12:14)]
# bb = predict.out.plm(simple, f, sub_data_test,model = 'within', pname = 'new_cases_scaled')
# predict(simple, sub_data_test[ind,])
# print(sub_data_test$new_cases_scaled[ind])

###############One step ahead prediction####################################################
f<- pFormula(new_cases_scaled ~ building_percentage + 
               workplaces_percent_change_from_baseline + 
               retail_and_recreation_percent_change_from_baseline+
               grocery_and_pharmacy_percent_change_from_baseline +
               transit_stations_percent_change_from_baseline + 
               parks_percent_change_from_baseline + 
               residential_percent_change_from_baseline)

# set the training period and test period length
train_start_date = '2020-04-08'
train_end_date = '2020-05-13'
test_end_date = as.Date(train_end_date) + 14

# initialize the lists to store data collected in the loop
n <- (length(which(as.Date(sub_data$week_start)>=train_end_date))/length((states))) - 2
mean_abs_diff <- vector("list", length = n)
pred_data <- NULL

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
  mape = abs(pred$df$new_cases_scaled.l.hat - pred$df$new_cases_scaled)/pred$df$new_cases_scaled
  state_temp = attr(test_sub_data, 'index')$state
  temp_result = cbind.data.frame(state_temp,test_sub_data$week_start, test_sub_data$new_cases_scaled, pred$df$new_cases_scaled.l.hat, mape)
  # store the predicted values
  pred_data = rbind.data.frame(pred_data, temp_result)
  #mean_abs_diff[i] = mean(abs(pred$df$new_cases_scaled - pred$df$new_cases_scaled.l.hat))
}
odd_indexes<-seq(1,nrow(pred_data),2)
pred_data = pred_data[odd_indexes,]
colnames(pred_data) <- c('State', 'Week_start', 'New_cases_scaled', 'Predicted_new_cases', 'MAPE')

pdf(file="Alabama_group_pred.pdf",width=8.3,height=5.5)
ggplot(data= pred_data, aes(x=Week_start, y=New_cases_scaled, group=State)) +
  geom_line(aes(colour=State), size = 2)+
  geom_line(aes(x=Week_start, y=Predicted_new_cases, col = State), data = pred_data, size = 1, linetype = 'dashed')+
  labs(x = 'Date', y = 'New cases per hundred thousand people')+
  theme_bw()+
  theme(legend.position="right", legend.text=element_text(size=10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.title=element_blank(), 
        legend.margin = margin(-1,-1,-1,-1),
        panel.border = element_blank(), 
        axis.text=element_text(size=14),
        axis.title=element_text(size=14))
dev.off()

print(paste('The MAPE is', mean(pred_data$MAPE)*100))


#############PVAR portion


#Separate the training and testing data for the PVAR
train_ind = which(as.Date(temp_data_sub$week_start)>'2020-04-06' & as.Date(temp_data_sub$week_start)<='2020-06-22')
train_ind = which(as.Date(temp_data_sub$week_start)>'2020-06-01' & as.Date(temp_data_sub$week_start)<='2020-08-10')
# 
# train_ind = which(as.Date(temp_data_sub$week_start)>'2020-04-13' & as.Date(temp_data_sub$week_start)<'2020-07-13')#which(as.Date(temp_data_sub$week_start)<'2020-06-15')#set training dates like we agreed on
# test_ind = which(as.Date(temp_data_sub$week_start)>='2020-06-15')
train_data_sub = temp_data_sub[train_ind,]
# test_ind = temp_data_sub[test_ind,]

#rename the columns to not be so long
colnames(train_data_sub) = c( "state", "week_start", "country_region_code", "country_region",                                    
                              "iso_3166_2_code","building_percentage", 'retail_and_recreation', 'grocery_and_pharmacy',
                              'parks', 'transit_stations', 'workplaces', 'residential', 'cumulative_cases', 'new_cases', 'population',
                              'new_cases_scaled')


#run the pvar. Change the name of the variable to your state group
dc_group_pvar_2 <- pvargmm(dependent_vars = c("workplaces", 
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

summary(dc_group_pvar_2)

dc_pvar_coef = panelvar::extract(dc_group_pvar)


#Examine stability of process. If the modulus of th eigenvalues are less than 1, we have stationarity
stab_dc_group_pvar <- stability(dc_group_pvar_2)
print(stab_dc_group_pvar) #these should all be within the unit circle

#Generate impulse response functions
dc_group_pvar_oirf <- oirf(dc_group_pvar, n.ahead = 8)
dc_group_pvar_girf <- girf(dc_group_pvar_2, n.ahead = 8, ma_approx_steps= 8)
#Generate bootstrap confidence intervals for impulse response functions. This takes a while to run. LET'S RUN THIS AS A LAST RESORT
# dc_group_pvar_bs <- bootstrap_irf(dc_group_pvar, typeof_irf = c("GIRF"),
#                                       n.ahead = 8,
#                                       nof_Nstar_draws = 100,
#                                       confidence.band = 0.95)
#plot the impulse response functions
plot(dc_group_pvar_oirf)

pdf(file="dc_group_ir2.pdf",width=8.3,height=5.5)
plot(dc_group_pvar_girf)
dev.off()


