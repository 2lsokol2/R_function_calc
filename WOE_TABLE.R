#data you should write with "" 

woe_function<-function(data,variable=1){
  library(ggplot2)
  library(devtools)
  library(easyGgplot2)
file<-read.table(paste0(data,".csv"),sep=",",stringsAsFactors=FALSE,header=TRUE)
result<-ggplot(data = file, aes(x = file[,variable[1]])) + geom_bar() 
i<-variable[2]
while (i<=length(variable)){
  plot_hist<-ggplot(data = file, aes(x = file[,variable[i]])) + geom_bar() 
  result<-ggplot2.multiplot(result, plot_hist, cols=1)
  i<-i+1
  
} 
result}                
   
file[,variable[3]]
variable<-2:10
              
                 
 woe_function("woe",2:4)                
                 
str(file)
library(ggplot2)
#install.packages("ggplot")
#install.packages("ggplot2")

file$score
i<-3
p1<-ggplot(data = file, aes(x = file[,3])) + geom_bar()
p2<-ggplot(data = file, aes(x = file[,4])) + geom_bar()
p3<-data.frame(name1=c(1,2),name2=c(2,3))


mplot<-ggplot2.multiplot(p1,p2,p3, cols=1)
#> `geom_smooth()` using method = 'loess'
library(easyGgplot2)
#install.packages("easyGgplot2")

install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")


write.table(mplot, file = "mplot.xlxs")
