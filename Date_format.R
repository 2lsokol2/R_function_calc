
filedate<-read.csv2("time.csv",sep=";")
filedate<-head(filedate,50)
class(filedate$dateone)
as.Date(file$dateone)

file<-read.csv2("newtime.csv",sep=";",as.is=TRUE)
file<-head(file,19)
class(file$dateone)
#file$dateone <- as.Date(file$dateone)

#as.Date good, but not for all
format(as.Date(file$dateone), "%A, %d-%b. %Y") #ok
as.Date("17-12-2010")


#install.packages("RApiDatetime")
library(lubridate)

library(tidyverse)
library(nycflights13)

parse_date_time("10-31/2010", orders = c("ymd", "dmy", "mdy"))
parse_date_time(file$dateone, orders = c("ymd", "dmy", "mdy"))


library(RApiDatetime)
library(anytime)
anydate("3.145")
sql<-read.csv("sql.csv",,as.is=TRUE)
class(sql$Today)
anydate(sql$Today)
as.Date(sql$Today)

anytime(c("10-11-2011 5:30AM", "16-10-2011 10:10pm"))

#as.POSIXlt == list()
sql$Today<-as.POSIXlt(sql$Today)
sql$Today$sec




