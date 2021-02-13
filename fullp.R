####################################################################
########## STATISTICAL DATA MINING 2 FINAL PROJECT
####################################################################
library(readr)
COVID19_state <- read_csv("SDM proj/COVID19_state.csv")

summary(COVID19_state)
str(COVID19_state)

###### changing column names
colnames(COVID19_state)

colnames(COVID19_state) <- c("State", "Tested", "Infected","Deaths","Population","PopDensity",
                             "Gini", 'ICUBeds', "Income", "GDP", "Unemployment", "SexRatio", 
                             "SmokingRate", "FluDeaths", "RespiratoryDeaths", "Physicians", "Hospitals", "HealthSpending",
                             "Pollution", "MedtoLargeAirports", "Temperature", "Urban", "Age0to25", "Age26to54",
                             "Age55plus", "percentAreainUSA", "Airquality", "AverageIQ", "homeless", "lifeexpectency", "vaccinationrate")

#type casting
COVID19_state$State <- as.factor(COVID19_state$State)

#correlation plot

#install.packages("corrplot")
library(corrplot)

corred <- cor(COVID19_state[,2:31],method = "pearson", use="pairwise.complete.obs")
corrplot(corred,method = "number", type = "lower")

#correlation plot after removing new york and new jersey
corrplot(cor(COVID19_state[-c(8,35),2:31], method = 'pearson',use="pairwise.complete.obs"), method = "number", type = "lower")

boxplot(scale(COVID19_state[,-c(1)]))

######################## tested, infected, state

#install.packages("tidyr")
library(ggplot2)
library(tidyr)

df1 <- data.frame(COVID19_state$Infected, COVID19_state$Tested, COVID19_state$State)
colnames(df1) <- c('infected','tested','state')
df2 <- tidyr::pivot_longer(df1, cols=c('infected', 'tested'), names_to='variable', 
                           values_to="value")

ggplot(df2, aes(x = (value), y = reorder(state,value), fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#without new york,NJ
ggplot(df2[-c(63,64,69,70),], aes(x = (value), y = reorder(state,value), fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#################### infected, deaths, state 

df3 <- data.frame(COVID19_state$Infected,COVID19_state$Deaths, COVID19_state$State)
colnames(df3) <- c('infected','deaths','state')
df4 <- tidyr::pivot_longer(df3, cols=c('infected', 'deaths'), names_to='variable', 
                           values_to="value")

ggplot(df4, aes(x = (value), y = reorder(state,value), fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#without new york, NJ
ggplot(df4[-c(63,64,69,70),], aes(x = (value), y = reorder(state,value), fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

##################### Infected 

ggplot(COVID19_state, aes(x = Infected, y = reorder(State, Infected))) + geom_bar(stat = 'identity', col = "red") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(COVID19_state[-c(35,32),], aes(x = Infected, y = reorder(State, Infected))) + geom_bar(stat = 'identity', col = "red") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################### death

ggplot(COVID19_state, aes(x = Deaths, y = reorder(State, Deaths))) + geom_bar(stat = 'identity', col = "blue") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(COVID19_state[-c(35,32),], aes(x = Deaths, y = reorder(State, Deaths))) + geom_bar(stat = 'identity', col = "blue") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

################### icu beds, infected, state 
icuinf <- data.frame(COVID19_state$Infected,COVID19_state$ICUBeds, COVID19_state$State)
colnames(icuinf) <- c('infected','ICU beds','state')
icuinf_1 <- tidyr::pivot_longer(icuinf, cols=c('infected', 'ICU beds'), names_to='variable', 
                                values_to="value")

ggplot(icuinf_1, aes(x = (value), y = reorder(state,value), fill = variable)) + geom_bar(stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################### infected on us map
#install.packages("usmap")
library(usmap)

stateinf <- as.data.frame(COVID19_state[,c(1,3)])

colnames(stateinf) <- c("state", "Inf")

plot_usmap(regions = "states", data = stateinf, values = "Inf", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "red", name = "Infected count", label = scales :: comma) +
  theme(legend.position = 'top')

#without new york and new jersey
plot_usmap(regions = "states", data = stateinf[-c(35,32),], values = "Inf", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "red", name = "Infected count", label = scales :: comma) +
  theme(legend.position = 'top')


#################### death on us map

statedeath <- as.data.frame(COVID19_state[,c(1,4)])

colnames(statedeath) <- c("state", "death")

plot_usmap(regions = "states", data = statedeath, values = "death", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "red", name = "Death count", label = scales :: comma) +
  theme(legend.position = 'top')

#without new york and new jersey
plot_usmap(regions = "states", data = statedeath[-c(35,32),], values = "death", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "red", name = "Death count", label = scales :: comma) +
  theme(legend.position = 'top')

#population on US map
statepop <- as.data.frame(COVID19_state[,c(1,5)])
colnames(statepop) <- c("state", "population")

plot_usmap(regions = "states", data = statepop, values = "population", color = "red", labels = T) + 
  scale_fill_continuous( low = "white", high = "red", name = "population count", label = scales :: comma) +
  theme(legend.position = 'top')

##################
#install.packages("treemap")
library(treemap)

#infected with new york
treemap(COVID19_state, index = "State", vSize = "Infected", type = "index")

#infected without new york
treemap(COVID19_state[-c(35,32),], index = "State", vSize = "Infected", type = "index")

#death with new york
treemap(COVID19_state, index = "State", vSize = "Deaths", type = "index")

#death without new york
treemap(COVID19_state[-c(35,32),], index = "State", vSize = "Deaths", type = "index")

###################################################################################
########################################## MODEL AND INTERACTIONs
###################################################################################
#finding important variables

variables <- COVID19_state[, - c(8,16,17)]

#median imputation
library(Hmisc)

variables$lifeexpectency <- impute(variables$lifeexpectency, fun = median)
variables$AverageIQ <- impute(variables$AverageIQ, fun = median)
variables$Airquality <- impute(variables$Airquality, fun = median)

#for infection

variablesinf <- data.frame(variables[, -c(2,4)], row.names = 1)

variablesinf <- data.frame(scale(variablesinf))

#linear model with all variables 

inflinmodel <- lm(Infected ~ ., data = variablesinf)

summary(inflinmodel)

#subset selection

fitstartinf <- lm(variablesinf$Infected  ~ 1, data = variablesinf)

stepfwdmodelinf <- step(fitstartinf, direction = "forward", scope = formula(inflinmodel))

stepbwdmodelinf <- step(inflinmodel, direction = "backward")

stepbothmodelinf <- step(fitstartinf, direction = "both", scope = formula(inflinmodel))

########### taking variables from backward selection
finlininf <- lm(Infected ~ Population + Gini + GDP + SexRatio + HealthSpending + 
                  Pollution + MedtoLargeAirports + Temperature + Urban + Age0to25 + 
                  Age26to54 + percentAreainUSA + homeless + vaccinationrate, variablesinf)

summary(finlininf)

#############################################################
#for death

vardeath <- data.frame(variables, row.names = 1)

vardeath <- data.frame(scale(vardeath))

deathlinmod <- lm(Deaths ~., vardeath)

summary(deathlinmod)

#subsetselection

fitstartdeath <- lm(vardeath$Deaths ~1, data = vardeath)

stepfwdmodeldeath <- step(fitstartdeath, direction = "forward", scope = formula(deathlinmod))

stepbwdmodeldeath <- step(deathlinmod, direction = "backward")

stepbothmodedeath <- step(fitstartdeath, direction = "both", scope = formula(deathlinmod))
##################################################################

library(gbm)

set.seed(143)

variablesinfgbm <- data.frame(variables[-c(32,35), -c(2,4)], row.names = 1)

variablesinfgbm <- data.frame(scale(variablesinfgbm))

gbminf <- gbm(Infected ~ ., data = variablesinfgbm, n.trees = 1000, interaction.depth = 2, cv.folds = 30)

summary(gbminf)

#str(variablesinfgbm)

#partial dependance plot for main effects
for (j in 1:24) {
  aaa <- plot(gbminf, i.var = c(j), lwd = 0, main = "")
  print(aaa)
}

#temperature and pollution
plot(gbminf, i.var = c(14,12), lwd = 2, main = "")

#temperature and unemployment
plot(gbminf, i.var = c(14,6), lwd = 2, main = "")

#temperature and smoking rate
plot(gbminf, i.var = c(14,8), lwd = 2, main = "")

#temperature and fludeaths
plot(gbminf, i.var = c(14,9), lwd = 2, main = "")

#temperature and urban pop
plot(gbminf, i.var = c(14,15), lwd = 2, main = "")

#temperature and age 26-54
plot(gbminf, i.var = c(14,17), lwd = 2, main = "")

#population and gdp
plot(gbminf, i.var = c(1,5), lwd = 2, main = "")

#population and smoking rate
plot(gbminf, i.var = c(1,8), lwd = 2, main = "")

#population and flu deaths
plot(gbminf, i.var = c(1,9), lwd = 2, main = "")

#population temperature
plot(gbminf, i.var = c(1,14), lwd = 2, main = "")

#population urban
plot(gbminf, i.var = c(1,15), lwd = 2, main = "")

#population age
plot(gbminf, i.var = c(1,17), lwd = 2, main = "")

#fludeath and urban
plot(gbminf, i.var = c(9,15), lwd = 2, main = "")

#fludeath and average iq
plot(gbminf, i.var = c(9,21), lwd = 2, main = "")

#respiratory death and smoking rate
plot(gbminf, i.var = c(10,8), lwd = 2, main = "")

#life exp and pollution
plot(gbminf, i.var = c(23,12), lwd = 2, main = "")

#urban and pollution
plot(gbminf, i.var = c(15,12), lwd = 2, main = "")


dev.off()

##########

#finding top interactions from gbm

intgbm <- data.frame("i","j","interactioneff")
colnames(intgbm) <- c("I", "J", "Interaction effect")

for (i in 1:24) {
  for (j in 1:24) {
    
    p <- interact.gbm(gbminf, variablesinfgbm, i.var = c(i,j), n.trees = gbminf$n.trees)
    
    if(i!=j & i<j){
      
      intgbm[nrow(intgbm) + 1,] = list(i,j,p)
      
    }
  }
  
}

intgbm <- intgbm[-1,]

intgbm$`Interaction effect` <- as.numeric(intgbm$`Interaction effect`)

orderedinteraction <- intgbm[order(intgbm$`Interaction effect`, decreasing = T),]


#install.packages("foreach")

#taking the top 10 significant ineractions and plotting

library(foreach)

foreach(i = as.numeric(orderedinteraction$I[1:10]), j = as.numeric(orderedinteraction$J[1:10])) %do% {
  plot(gbminf, i.var = c(i,j), lwd = 2, main = "")
}

#the above code produces 1 plots.

##########

#produces plot of all possible interactions between the 24 variables
#takes some time to run, only run if required


#partial dependance plot for all interaction effects
#for (i in 1:24) {
  
 # for (j in 1:24) {
  #  if (i!= j) {
   #   aaa <- plot(gbminf, i.var = c(i,j), lwd = 0, main = "")
    #  print(aaa)
  #  }
  #}
#}


################################################################################
############################ TIME SERIES FORECASTING
################################################################################

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(readr)

time_series_covid19_recovered_global <- read_csv("SDM proj/time_series_covid19_recovered_global.csv")
time_series_covid19_deaths_global <- read_csv("SDM proj/time_series_covid19_deaths_global.csv")
time_series_covid19_confirmed_global <- read_csv("SDM proj/time_series_covid19_confirmed_global.csv")

#TS of US death

usdeath <- t(subset(time_series_covid19_deaths_global, time_series_covid19_deaths_global$`Country/Region` == "US"))[5:118,]

usdeath <- data.frame(usdeath)

deathts <-  data.frame(strptime(rownames(usdeath), format =  "%m/%d/%y"), usdeath$usdeath )

colnames(deathts) <- c("Date", "Death")

deathts$Death <- as.integer(deathts$Death)

str(deathts)

ggplot(deathts, aes(x=Date)) +
  geom_line(aes(y=Death, color = "#69b3a2"), size = 1.5)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_hue(labels = c("DEATH"))


#TS of US INFECTED

usinfected <- data.frame(t(subset(time_series_covid19_confirmed_global, time_series_covid19_confirmed_global$`Country/Region` == "US"))[5:118,])

#usinfected <- data.frame(usinfected[5:nrow(usinfected)])

infectedts <-  data.frame(strptime(rownames(usinfected), format =  "%m/%d"), usinfected$t.subset.time_series_covid19_confirmed_global..time_series_covid19_confirmed_global..Country.Region..... )

colnames(infectedts) <- c("Date", "Infected")

infectedts$Infected <- as.integer(infectedts$Infected)

str(infectedts)

ggplot(infectedts, aes(x=Date)) +
  geom_line(aes(y=Infected, color = "red"), size = 1.5)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_hue(labels = c("INFECTED"))

#TS of US RECOVERED

usrecovered <- data.frame(t(subset(time_series_covid19_recovered_global, time_series_covid19_recovered_global$`Country/Region` == "US"))[5:118,])

recoveredts <-  data.frame(strptime(rownames(usrecovered), format =  "%m/%d"), usrecovered$t.subset.time_series_covid19_recovered_global..time_series_covid19_recovered_global..Country.Region..... )

colnames(recoveredts) <- c("Date", "recovered")

recoveredts$recovered <- as.integer(recoveredts$recovered)

str(recoveredts)

ggplot(recoveredts, aes(x=Date)) +
  geom_line(aes(y=recovered, color = "red"), size = 1.5)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_y_continuous(labels = scales::comma)+
  scale_color_hue(labels = c("RECOVERED"))

#All three in one plot

deathinfrec <- data.frame(deathts$Date ,deathts$Death, recoveredts$recovered, infectedts$Infected)
colnames(deathinfrec) <- c("date", "death", "recovered", "infected")

#install.packages("reshape2")
library(reshape2)

melteddf <- melt(deathinfrec, id = "date" )

ggplot(melteddf,aes(x=date,y=value,colour=variable,group=variable)) +
  geom_line(size = 1.2)+
  scale_color_hue(labels = c("Death", "Recovered", "Infected"))

######################################## TS prediction

library(forecast)
library(ggfortify)
library(lubridate)
library(zoo)

fordeatharima <- data.frame(deathts, row.names = 1)

str(fordeatharima)

#############################################################death ts

#basemodel death

naivedeath <- snaive(fordeatharima)

summary(naivedeath)

#exponential smoothing death

etsdeath <- ets(fordeatharima$Death)

summary(etsdeath)

#ARIMA death

arimadeath <- auto.arima(fordeatharima,stepwise = F, trace = T)

summary(arimadeath)

checkresiduals(arimadeath)

deathforecast <- forecast(arimadeath, h = 60)

autoplot(deathforecast)

###############################################infection ts

forinfarima <- data.frame(infectedts, row.names = 1)

#basemodel infected

naiveinf <- snaive(forinfarima)

summary(naiveinf)

#exponental smoothing

etsinf <- ets(forinfarima$Infected)

summary(etsinf)

#ARIMA infected

arimainf <- auto.arima(forinfarima, stepwise = F, trace = T)

summary(arimainf)

checkresiduals(arimainf)

infforecast <- forecast(arimainf, h = 60)

autoplot(infforecast, include = 50)+
  scale_y_continuous(labels = scales::comma)

