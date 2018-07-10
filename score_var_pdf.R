#data you should write with "" 
woe_function<-function(data,var){
  library(ggplot2)
  library(gridExtra)
  library(grid)
  library(lattice)
  #install.packages("lattice")
  file<-read.csv2(paste0(data,".csv"),dec=".",stringsAsFactors=FALSE,header=TRUE)
  multi_plot<-lapply(var[1]:var[length(var)],function(x)
    ggplot(file, aes(file[,x])) + 
      geom_bar(aes(y = (..count..)/sum(..count..))) + 
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      labs(title = names(file)[x], y = "Percent", x = "Class") +
      theme(axis.text.x = element_text(angle=10, vjust=0.9)) + 
      theme(plot.margin = unit(c(1,1,1,1), "cm")))
  oneplot <- marrangeGrob(multi_plot, nrow=2, ncol=1)
  ggsave("characteristic.pdf", oneplot, width = 9, height = 10)
}   

woe_function("data_new",8:14)