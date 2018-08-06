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


   # Make list of variable names to loop over.
    var_list = combn(names(iris)[1:3], 2, simplify=FALSE)
  
  # Make plots.
  plot_list = list()
  for (i in 1:3) {
    p = ggplot(iris, aes_string(x=var_list[[i]][1], y=var_list[[i]][2])) +
      geom_point(size=3, aes(colour=Species))
    plot_list[[i]] = p
  }
  
  # Save plots to tiff. Makes a separate file for each plot.
  for (i in 1:3) {
    file_name = paste("iris_plot_", i, ".tiff", sep="")
    tiff(file_name)
    print(plot_list[[i]])
    dev.off()
  }
  
  # Another option: create pdf where each page is a separate plot.
  pdf("plots.pdf")
  for (i in 1:3) {
    print(plot_list[[i]])
  }
  dev.off()





