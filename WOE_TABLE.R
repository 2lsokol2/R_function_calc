#data you should write with "" 

woe_function<-function(data,variable=1){
  library(ggplot2)
  library(devtools)
  library(easyGgplot2)
  library(plyr)
  library(knitr)
  
file<-read.table(paste0(data,".csv"),sep=",",stringsAsFactors=FALSE,header=TRUE)
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
 pdf("scoring.pdf") 
  ggplot2.multiplot(result, plot_hist, table_freq, cols=1)
  dev.off() }                
   
file[,variable[3]]
variable<-2:10
              
                 
 woe_function("woe",2:6)                
                 
str(file)
library(ggplot2)
library(easyGgplot2)
library(devtools)
#install.packages("ggplot")
#install.packages("ggplot2")
#install.packages("easyGgplot2")
#install_github("easyGgplot2", "kassambara")
#install.packages("devtools")
#install.packages("knitr")
#> `geom_smooth()` using method = 'loess'
#install.packages("easyGgplot2")
#http://www.sthda.com/english/wiki/ggplot2-multiplot-put-multiple-graphs-on-the-same-page-using-ggplot2
a<--3:3
b<-12
ifelse(a>0,"positive","not positive")
ifelse(b>0,"positive","not positive")

str(t)

t<-read.table("woe.csv",sep=",",stringsAsFactors=FALSE,header=TRUE)
#install.packages("plyr")
library(plyr)
library(knitr)
table<-data.frame(count(t,"rank"))
kable(table)
















