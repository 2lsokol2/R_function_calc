##read.delim - read.table with "\" sep 
read.delim("rr.txt",header = TRUE,sep="/")
t<-read.delim("name.txt",fill=TRUE,sep="/")
#header=TRUE by defolt,u can don`t write with dich

##mode()&typeof()&class()
#mode() The 'atomic' modes are numeric, complex, character and logical
#type() 'avarage' type 
#class() - 'global' type of object
x <- 1
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- letters
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- TRUE
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars[1]
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- cars[[1]]
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- matrix(cars)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- new.env() #http://adv-r.had.co.nz/Environments.html
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- expression(1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- quote(y <- 1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- ls
print(c(class(x), mode(x), storage.mode(x), typeof(x)))
#dates
dates <- c("15-9-2009", "16-07-2008", "17 12-2007", "29-02-2011")
as.POSIXct(dates, format = "%d-%m-%Y")
'%a Abbreviated weekday name in the current locale. Mon
%A Full weekday name in the current locale. Monday
%b Abbreviated month name in the current locale. Sep
%B Full month name in the current locale. September
%m Month number (01-12) 09
%d Day of the month as decimal number (01-31). 28
%y Year without century (00-99) 13
%Y Year including century. 2013'

#String normalization
library(stringr)
str_trim(" hello world ")
## [1] "hello world"
str_trim(" hello world ", side = "left")
## [1] "hello world "
str_trim(" hello world ", side = "right")
## [1] " hello world"

#grep and grepl functions:
grep("[a-z]", letters) #index
grepl("[a-z]", letters) #TRUE/FALSE

gender <- c("M", "male ", "Female", "fem.")
grepl("m", gender, ignore.case = TRUE) #remove sensitive to caps lock

txt<-readLines("example.txt")
#
i<-grepl("^//", txt)
dat<-txt[!i]
fieldList <- strsplit(dat, split = ";")
class(fieldList)
#input and assigns the values in the right order.
x<-strsplit(dat, split = ";")
x<-x[1]
assignFields <- function(x){
  out <- character(3)
  # get names
  i <- x[[1]][grep("[[:alpha:]]",x)]
  out[1] <- i
  # get birth date (if any)
  i <- which(as.numeric(x) < 1890)
  i <- x[x < 1890]
  out[2] <- ifelse(length(i)>0, x[i], NA)
  # get death date (if any)
  i <- which(as.numeric(x) > 1890)
  out[3] <- ifelse(length(i)>0, x[i], NA)
  out
}
