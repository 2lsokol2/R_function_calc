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

##Mising values
#age height
# 1 21 6.0
# 2 42 5.9
# 3 18 5.7*
# 4 21 <NA>
complete.cases(person)
## [1] TRUE TRUE TRUE FALSE
persons_complete <- na.omit(person)
na.action(persons_complete)
## 4
#Remove NA,NaN,inf 
rm_badval<-function(x){
  i<-1
  df<-data.frame()
  while(i<=ncol(x))
  {
    a<-x[,i]
    if (is.numeric(a)) t[is.infinite(t) | is.nan(t)] <- NA else a
    i<-i+1
  }
na.omit(x)
}
rm_badval(data)



cat(..., file = "", sep = " ")
# превращает все указанные аргументы (...)
# в текст и сохраняет результат в виде файла; разделитель между
# получаемыми текстовыми значениями задается при помощи аргумента sep

x[x > 3 & x < 5]
# возвращает все элементы вектора x со значением >3 и <5
 
x[x %in% c("a","and","the")] 
# возвращает только те элементы вектора x, которые указаны после 
# оператора %in% (здесь из вектора x были бы извлечены текстовые значения
# "a", "and", и "the")

# Создадим тестовую таблицу данных.
> mydata <- data.frame(name = c("Antony", "Bob", "Cecilia", "Jack", "Mary", "Tony"),
                     col_a = c(0, 1, 0, 1, 2, 3),
                    col_b = c(3, 0, 3, 3, 1, 0),
                     stringsAsFactors=FALSE)
 mydata


# Нужные нам строки - 1-ая и 6-ая
grep('[t,T]ony', mydata$name)

mydata[grep('[t,T]ony', mydata$name),]
# Создадим пустую таблицу mydata с числовыми колонками First, Second
# и символьной колонкой Third:
mydata <- data.frame(First=numeric(0),
                     Second=numeric(0),
                     Third = character(0),
                     stringsAsFactors=FALSE)
str(mydata)
#Как удалить из таблицы колонки с заданными именами
# Создаем таблицу
df = data.frame(c1=1:4, c2=letters[1:4], c3=5:8)
# Удаляем колонку "с2"
df_new = df[ , -which(names(df) %in% c("c2"))]

#unique and order
unique(file[,c(1,4,26)][with(file[,c(1,4,26)], order(bty_f1lower,WW_bty_f1lower)), ])
