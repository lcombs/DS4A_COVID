library(plm)
library(panelvar)
library(tidyr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
path <- getwd()

###Load the data 
data <- read.csv(file=paste(path,'/data/all_data_clean.csv', sep = ''), header=TRUE, sep=",")

data("Dahlberg")
summary(Dahlberg)
plot.ts(Dahlberg[which(Dahlberg$id==114),3:5]) #plot the expenditure, revenue, and grants for a single country in the Dalberg dataset. Looks like we have co-integration but not stationarity?


help(pvargmm)

#example 1: building the model
ex1_dahlberg_data <- pvargmm(dependent_vars = c("expenditures", "revenues", "grants"),
                                lags = 1,
                                transformation = "fod",
                                data = Dahlberg,
                                panel_identifier=c("id", "year"),
                                steps = c("twostep"),
                                system_instruments = FALSE,
                                max_instr_dependent_vars = 99,
                                max_instr_predet_vars = 99,
                                min_instr_dependent_vars = 2L,
                                min_instr_predet_vars = 1L,
                                collapse = FALSE
                        )
summary(ex1_dahlberg_data)


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


