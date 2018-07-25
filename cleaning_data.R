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

x <- new.env()
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- expression(1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- quote(y <- 1 + 1)
print(c(class(x), mode(x), storage.mode(x), typeof(x)))

x <- ls
print(c(class(x), mode(x), storage.mode(x), typeof(x)))