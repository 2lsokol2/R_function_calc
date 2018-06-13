#newtime - csv містить дату у текстовому форматі
#time - csv містить дату у форматі дати 
#as.is=TRUE - factor->char(навіть якщо дата у текстовому форматі у csv) vs ( colClasses = "character")
#head - дозволяє брати тільки "потрібні" клітинки
filedate<-read.csv2("time.csv",sep=";")
filedate<-head(filedate,50)
class(filedate$dateone)
as.Date(file$dateone)
#as.Date - тільки дати, якщо треба час(навіть часовий пояс і т.д.) - POSIXct,POSIXlt

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
#lubridate ->parse_date_time - розпізнає дату у різних форматах(в orders само вибирає найкращий формат)
parse_date_time("10-31/2010", orders = c("ymd", "dmy", "mdy"))
parse_date_time(file$dateone, orders = c("ymd", "dmy", "mdy"))


#anytime - дозволяє обробляти дату у будь-якому форматі("factor" утворився при імпорті)
library(RApiDatetime)
library(anytime)
anydate("3.145")
sql<-read.csv("sql.csv",,as.is=TRUE)
class(sql$Today)
anydate(sql$Today)
as.Date(sql$Today)

#не обробляє вектор, також значення із "AM/PM"
anytime(c("10-11-2011 5:30AM", "16-10-2011 10:10pm"))

#as.POSIXlt == list()
sql$Today<-as.POSIXlt(sql$Today)
sql$Today$sec




