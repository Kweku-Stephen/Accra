
data <- read.table(file.choose(), header = T, sep="\t")
data_max <- read.table(file.choose(), header = T, sep = "\t")
data_min <- read.table(file.choose(),header = T, sep = "\t")

#Ripping columns out
data_max <- data_max$Tmax
data_min <- data_min$Tmin

#Creating date object
dates <- with(data, as.Date(paste(Year, Month, Day), paste("%Y %m %d")))

#Adding ripped columns to (data)
data[,5] <- c(data_max)
data[,6] <- c(data_min)

#Run this line 3 times
data <- data[-1]

#Creating a data.frame
data <- data.frame(dates,data)

#Replacing -99.9 in all columns apart from dates column, with NA
data$V5 <- replace(data$V5, data$V5==-99.9,NA)
data$V6 <- replace(data$V6, data$V6==-99.9,NA)
data$Rain <- replace(data$Rain, data$Rain==-99.9, NA)
colSums(is.na(data))

library("xts")
library("timeSeries")

#Converting to extensible time series
#Annual Rainfall
data_xts_max <- xts(data$V5,dates)
data_xts_max <- apply.yearly(data_xts_max, mean, na.rm=T)

#Monthly Rainfall
data_xts_rain_month <- xts(data$Rain,dates)
data_xts_rain_month <- apply.monthly(data_xts_rain_month, sum, na.rm=T)
data_ts_rain_month <- ts(data_xts_rain_month, star=c(1960,1),frequency = 12)

#Converting to ts
data_ts_max <- ts(data_xts_max, start = c(1960,1), frequency = 1)
plot(data_ts_max, main="Maximum Annual Temperature of Accra (1960-2016)", col="firebrick",lwd=3, ylab = "Temperature(D.Celcius)", las=1)

#Testing for statistical significance using base regression
mod <- reg=lm(data_ts_rain~time(data_ts_rain))
summary(mod)

#Testing for trend significance using MannKendall from "Kendall"
MK <- MannKendall(data_ts_rain)
summary(MK)


#Ripping out individual months out of the rain time series object....in this command "start=c(1960,1)", make sure the number succeding the year represents the numeric value of the month 
Jan<- window(data_ts_rain, start=c(1960,1), frequency=T)
Jan <- sum(Jan)/56

#Creating a dummy date for a new data.frame
date1<- as.Date(c("1980-1-01","1982-2-03","1985-3-18","1988-4-12", "1991-5-09", "1994-6-23","1997-07-14","2000-8-15","2003-9-17","2006-10-30","2009-11-16","2012-12-12"))
#Creating the data.frame
create "Rain" object with the values of "Jan <- sum(Jan)/56" and succeding months.
Rain <- c(11.44464, 22.50179,60.14464,92.125,141.2643,217.9732,67.26964,25.725,55.68571,67.71786,30.94286,23.57857)
create a new data frame with "dates" and "Rain" objects
....Name of dataframe "a"

# plotting Monthly Rainfall with labels of months on the x-axis using ggplot2
ibrary("ggplot2", lib.loc="~/R/win-library/3.5")
library("ggthemes", lib.loc="~/R/win-library/3.5")
res <- aggregate(a$Rain, list(months(a$date1)), sum)
res$date1 <- as.Date(paste0("01-", res$Group.1, "-1980"), "%d-%B-%Y")
theme_set(theme_bw())
ggplot(res,aes(x=date, y=x)) +geom_line(col="darkblue", lwd=3) +ggtitle("Monthly Rainfall of Accra (1960-2016)") + xlab("Months") +ylab("Rainfall(mm)") +theme(panel.background = element_rect(fill = "white"), panel.grid = element_line(colour = "white"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line.x = element_line(colour = "black"),  axis.line.y = element_line(colour = "black")) + scale_x_date(date_minor_breaks = "1 month", date_labels = "%B")


