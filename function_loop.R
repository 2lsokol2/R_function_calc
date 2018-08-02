#data you should write with "" 

woe_function<-function(data,variable=1){
  library(ggplot2)
  library(devtools)
  library(easyGgplot2)
  library(plyr)
  library(knitr)
  
  file<-read.table(paste0(data,".csv"),sep=",",stringsAsFactors=FALSE,,na.strings = c("NULL","NA"),header=TRUE)
  plot0<-ggplot(data = file, aes(x = file[,variable[1]])) + geom_bar() 
  table0<-kable(data.frame(count(t,variable[1])))
  result<-result<-ggplot2.multiplot(plot0, table0, cols=1)
  i<-variable[2]
  while (i<=length(variable)){
    plot_hist<-ggplot(data = file, aes(x = file[,variable[i]])) + geom_bar() 
    table_freq<-kable(data.frame(count(t,variable[i])))
    result<-ggplot2.multiplot(result, plot_hist, table_freq, cols=1)
    i<-i+1
    
  } 
  result}                

file[,variable[3]]
variable<-2:10


woe_function("woe",2:6)                


#install.packages("ggplot")
#install.packages("ggplot2")
#install.packages("easyGgplot2")
#install_github("easyGgplot2"/"kassambara")
#install.packages("devtools")
#install.packages("knitr")
#> `geom_smooth()` using method = 'loess'
#install.packages("easyGgplot2")
#http://www.sthda.com/english/wiki/ggplot2-multiplot-put-multiple-graphs-on-the-same-page-using-ggplot2

str(t)

t<-read.table("woe.csv",sep=",",stringsAsFactors=FALSE,header=TRUE)
#install.packages("plyr")
#plyr - like group by in sql
library(plyr)
library(knitr)
table<-data.frame(count(t,"rank"))
kable(table)



#Possible mistakes
for (i in 1:wind.1 -1) - ":" - higher priority(-1 for every member of vector)
for (i in 1 : (wind.1-1)) 
a<--3:3
b<-12
ifelse(a>0,"positive","not positive")
ifelse(b>0,"positive","not positive")

a<-c(-1,1)
ifelse(a>0,a,"not")


"F" if final_score <60

"D" if 60???final_score<70

"C" if 70???final_score<80

"B" if 80???final_score<90

"A" if 90???final_score


grade<-ifelse(final_score<60,"F", 
              ifelse (final_score<70,"D",
                      ifelse(final_score<80,"C",
                             ifelse (final_score<90,"B", "A"))))



#more quickly
if(cond) expr
if(cond) {expr}
#{ expr1 ; expr2 }

#for one cond
if(cond) cons.expr else alt.expr
if(cond) {cons.expr} else {alt.expr}

sum_f<-function(vector){
  if(length(vector)>4) 4 else "DON'T KNOW"
}
sum_f(1:3)  


#read every row? so be carefull with "{}"
sum_f2<-function(vector){
if(length(vector)>4){
   a=vector[1:2] 
   b=a*3
} else {
  b=print("DON'T KNOW")
   }
 return(b) 
}


sum_f2(1:5) 

for(var in seq) expr
while(cond) expr
repeat expr
break
next



a<-c(-1,1)
ifelse(a>0,a,"not")

set.seed(10)








