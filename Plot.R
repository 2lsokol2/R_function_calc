#https://ggplot2.tidyverse.org/ -all liturature
#install.packages("ggplot")
#install.packages("ggplot2")
library(ggplot)
library(ggplot2)
ggplot(data = cars, aes(x = speed, y = dist)) + geom_point()
ggplot(data = cars, aes(x = speed, y = dist)) + stat_identity()
ggplot(data = cars, aes(x = speed)) + geom_bar()
ggplot(data = cars, aes(x = speed)) + geom_bar() + coord_polar()

#theme_bw() - black,white color
ggplot(data = cars, aes(x = speed, y = dist)) + stat_identity() + theme_bw()

ggplot(data = cars, aes(x = speed, y = dist, color = speed)) + geom_point()

ggplot(data = cars, aes(x = speed, 
                        y = dist, 
                        color = speed, 
                        size = dist)) + geom_point()

ggplot(data = cars, aes(x = speed > 15, y = dist)) + geom_boxplot()

#plot to pdf() 
pl<- function(...){ 
   X11()                   #or pdf() 
   plot(...) 
   # dev.off()  if pdf() is used 
   X11()                  # open second graphics device 
   plot(...) 
} 

pl(1:10,pch=10) 

  ggsave("characteristic.pdf", oneplot, width = 9, height = 10,scale=o.5,theme_grey(base_size = 3))
#oneplot - function like 'hist(x=..', scale - the physical size of the plot,base_size - base size to compare with titles and ets
