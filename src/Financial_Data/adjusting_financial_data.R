rm(list=ls())
setwd("C:\\Users\\bertj\\OneDrive\\Documenten\\GitHub\\SMWA_Performance\\data")

load("Financial_Data.Rdata") # Wouter's csv needs to be updated to be recent enough

# set Target variable as a percentage
data$Target <- data$Target / data$Open

# remove columns that aren't necessary: Date is needed for merging, Volume and Close are independent variables, and Target is the dependent variable
data <- subset(data, select = c(Date, Volume, Close, Target))
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# csv file with data on S&P 500 index
SP500 <- read.csv("S&P 500 Data.csv")
SP500 <- subset(SP500, select = -c(Open, Hoog, Laag, Vol.))
colnames(SP500) <- c("Date", "SP500_close", "SP500_change")
SP500$Date <- as.Date(SP500$Date, format =  "%d.%m.%Y")


data <- merge(data, SP500, by = "Date")   # merging is still wrong; SP500 merged on wrong date or should still include NA's

