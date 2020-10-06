library(plm)
library(panelvar)

path<-'/Users/lisacombs/Documents/DS4A/DS4A_COVID'

###### Cathy ###### 

###### Jingjing ###### 

###### Lencer ###### 

###### Lisa ###### 

# load
data <- read.csv(file=paste(path,'/all_data_weekly.csv', sep = ''), header=TRUE, sep=",")
# select states
states <- c('Alabama', 'Colorado', 'Indiana', 'Minnesota', 'Wisconsin')
test <- c(sapply(states, function(x) which(data$state == x)))
temp_data_sub <- data[test,]
# prep data for plm
sub_data <- pdata.frame(temp_data_sub, index=c("state"), drop.index=TRUE, row.names=TRUE)
heading = colnames(sub_data[5:11])
sub_data$building_percentage = lag(sub_data$building_percentage,2)
# run plm
test <- plm(new_cases~building_percentage, data = sub_data, model = "within")
# results plm
summary(test)
