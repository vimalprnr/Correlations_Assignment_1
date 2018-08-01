#1
setwd("D://ACADGILD//VImal//10 CORRELATIONS//Assignment//INPUT//")
airQual <-  read.csv2("AirQualityUCI.csv", header = T, sep = ";")
View(airQual)
#2
colname<- c("DATE","TIME","AVG CO CONC","AVG TIN OXIDE CONC","AVG NON META HYDCARBON CONC","AVG BENZENE CONC","AVG TITANIA CONC","AVG NO CONC","AVG TUNGSTEN CONC(NO)","AVG NO2 CONC","AVG TUNGSTEN CONC(NO2)","AVG OZONE  CONC","TEMPRATURE",
"RELATIVE HUMIDITY","ABSOLUTE HUMIDITY")
colnames(airQual) <- colname
airQual <- airQual[,c(-17,-16)]
par(mfrow = c(3,5))
hist(airQual$`AVG CO CONC` ,main="Average CO concentration",col="red")
hist(airQual$`AVG TIN OXIDE CONC` ,main="Average TIN OXIDE CONC",col="blue")
hist(airQual$`AVG NON META HYDCARBON CONC` ,main="Average NON META HYDCARBON CONC",col="yellow")
hist(airQual$`AVG BENZENE CONC` ,main="Average BENZENE CONC",col="darkblue")
hist(airQual$`AVG TITANIA CONC` ,main="Average TITANIA CONC",col="pink")
hist(airQual$`AVG NO CONC` ,main="Average NO CONC",col="purple")
hist(airQual$`AVG TUNGSTEN CONC(NO)` ,main="Average TUNGSTEN CONC(NO)",col="red")
hist(airQual$`AVG NO2 CONC` ,main="Average NO2 CONC",col="blue")
hist(airQual$`AVG TUNGSTEN CONC(NO2)` ,main="Average TUNGSTEN CONC(NO2)",col="yellow")
hist(airQual$`AVG OZONE  CONC` ,main="Average OZONE  CONC",col="pink")
hist(airQual$TEMPRATURE ,main="Average of Temp",col="purple")
hist(airQual$`RELATIVE HUMIDITY` ,main="RELATIVE HUMIDITY",col="purple")
hist(airQual$`ABSOLUTE HUMIDITY` ,main="ABSOLUTE HUMIDITY",col="red")

#3
summary(airQual)
#4
#Each column contains 114 NA Values
library(mice)
md.pattern(airquality)
colnames(airQual) <- letters(1:length(colname))
imputed_Data <- mice(airQual, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
airQualComplete <- complete(imputed_Data)
colnames(airQual) <- colname
colnames(airQualComplete) <- colname
write.csv(airQualComplete, file = "airQualUpdated.csv")

#5
library(psych)
pairs.panels( airquality[,1:13],method = "spearman", hist.col = "yellow",density = TRUE,ellipses = TRUE, lm=TRUE,
              main ="Scatter plots with spearman Correlation"
)

#6
t.test(x=airQualComplete$`RELATIVE HUMIDITY`, y=airQualComplete$`ABSOLUTE HUMIDITY` ,alternative = "two.sided",mu=0 ,paired = TRUE)
t.test(x=airQualComplete$`AVG BENZENE CONC`, y=airQualComplete$`AVG TITANIA CONC` ,alternative = "two.sided",mu=0 ,paired = TRUE)
t.test(x=airQualComplete$`AVG OZONE  CONC`, y=airQualComplete$TEMPRATURE ,alternative = "two.sided",mu=0 ,paired = TRUE)
