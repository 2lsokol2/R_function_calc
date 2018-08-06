ToothGrowth
#install.packages("ggplot2")
library(cowplot)
library(ggplot2)
library(gridExtra)
library(ggpubr)
setwd("S:/PROJECTS/! Individual/LSokol/!Rstudio/script")

data<-read.csv2("woe.csv",sep=",",dec=".",colClasses = "factor")
file<-data
names(file)
ncol(file)

text <- paste("                                                        ",
              "                                                        ",
              "                                                        ",
              "                 Descriptive statistics                              ",
              "          of potential variables for scoring.",
              "                                     WOE, IV, distribution, etc.", sep=" ")


 
  #generate random target(for this variant)
  #target<-rbinom(n = 463, size = 1, prob = 0.85)
  #data<-cbind(target,data)
  
  #generate "data" with first column "target" and another columns are sorted alphabetically
  data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))
  
  #recall first column,cauth automatically programe calls it "data$target"
  names(data)[1]<-"target"
  file<-data
  
  plot_list = list()
  #i - num var
  i<-1 
  #second variable is "numplot" - 3 plots for every variable
  numplot<-1
  numcol<-ncol(file)
  for (i in 2:numcol){
    plot1_hist<-ggplot(file, aes(file[,i])) + 
      geom_bar(aes(y = (..count..)/sum(..count..)))+
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4), plot.margin = unit(c(1,1,1,1), "cm") ) + 
      labs( y = "", x = "")+
      ggtitle(names(file)[i])
    plot_list[[numplot]] = plot1_hist
    numplot<-numplot+1
    
    
    #t1-aggregate data for create BR
    t1<-aggregate(. ~ data[,i], data = data[c(data[,1],data[,i])], 
                  FUN = function(x) c(all = length(x), bad = sum(x) ) )[,c(1,2)]
    
    #counting BR(new columns in aggregate func in R is MATRIX)
    t2<-transform(t1,br=1-(t1[,2][,2] / t1[,2][,1]))[,c(1,3)]
    
    #recall "t2",cauth next action will be join "t2" and "file",
    #(we must have united column with common name)
    #"BR" column begins with "WW_"
    #(all variables soryed by alphabet,we split table by 2 part,with "BR" with WW_" and will be in the right part of table) )
    names(t2)<-c(names(data)[i],paste0("WW_",names(data)[i]))
    
    #add  new column "BR"to our "data" table
    data<-merge(data,t2,by.x=names(data)[i],by.y=names(t2)[1])
    
    str(data)
    #again sort column,cauth "BR" goes to the end,but joined column goes to the begining,totaly bad column)
    data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))
    
    #recall first column,cauth automatically programe calls it "data$target"
    names(data)[1]<-"target"
  
    
    plot2_hist<-ggplot(data, aes(x=data[,i],y=data[,i+numcol],group=1)) + 
      geom_line(color="dodgerblue4",size=1)+
      geom_point() +
      labs( y = "", x = "")+
      ylim(c(0, 0.25))
    plot_list[[numplot]] = plot2_hist
    numplot<-numplot+1
    
    plot3_hist<-ggplot(file, aes(file[,i])) + 
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4),plot.margin = unit(c(1,1,1,1), "cm") ) + 
      labs( y = "", x = "")+
      ggtitle(names(file)[i])
    plot_list[[numplot]] = plot3_hist
    numplot<-numplot+1
  }
  
  
  pdf("file5.pdf",width=10,height=15,paper='special') 
  marrangeGrob(plot_list,ncol=1,nrow=3)
  dev.off()
  
  
  
  geom_bar(aes(y = ((..count..)/sum(..count..))),stat="identity", width=1.0, 
           colour = "darkgreen",
           fill = 'lightslateblue') +
  
  
  data<-read.csv2("woe.csv",sep=",",dec=".",colClasses = "factor")
file<-data
names(file)
ncol(file)

text <- paste("                                                        ",
              "                                                        ",
              "                                                        ",
              "                 Descriptive statistics                              ",
              "          of potential variables for scoring.",
              "                                     WOE, IV, distribution, etc.", sep=" ")


 
  #generate random target(for this variant)
  #target<-rbinom(n = 463, size = 1, prob = 0.85)
  #data<-cbind(target,data)
  
  #generate "data" with first column "target" and another columns are sorted alphabetically
  data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))
  
  #recall first column,cauth automatically programe calls it "data$target"
  names(data)[1]<-"target"
  file<-data
  
  str(file)
  plot_list = list()
  plot_list[[1]]<-""
  plot_list[[2]]<-""
  plot_list[[3]]<-""
  
  c<-list()
  c[[1]]<-""
  #i - num var
  #i<-2 
  #second variable is "numplot" - 3 plots for every variable
  numcol<-ncol(file)
  for (i in 2:numcol){
    plot1_hist<-ggplot(file, aes(file[,i])) + 
      geom_bar(aes(y = (..count..)/sum(..count..)))+
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4), plot.margin = unit(c(1,1,1,1), "cm") ) + 
      labs( y = "", x = "")+
      ggtitle(names(file)[i])
    plot_list[[i*3-2]] = plot1_hist
   
    
    
    #t1-aggregate file for create BR
    t1<-aggregate(. ~ file[,i], file = file[c(file[,1],file[,i])], 
                  FUN = function(x) c(all = length(x), bad = sum(x) ) )[,c(1,2)]
    
    #counting BR(new columns in aggregate func in R is MATRIX)
    t2<-transform(t1,br=1-(t1[,2][,2] / t1[,2][,1]))[,c(1,3)]
    
    #recall "t2",cauth next action will be join "t2" and "file",
    #(we must have united column with common name)
    #"BR" column begins with "WW_"
    #(all variables soryed by alphabet,we split table by 2 part,with "BR" with WW_" and will be in the right part of table) )
    names(t2)<-c(names(file)[i],paste0("WW_",names(file)[i]))
    
    #add  new column "BR"to our "file" table
    file<-merge(file,t2,by.x=names(file)[i],by.y=names(t2)[1])
    
    #again sort column,cauth "BR" goes to the end,but joined column goes to the begining,totaly bad column)
    file<-cbind(file$target,(file[ , sort(names(file[,-which( colnames(file)=="target")]))]))
    
    #recall first column,cauth automatically programe calls it "file$target"
    names(file)[1]<-"target"
  
    
    plot2_hist<-ggplot(file, aes(x=file[,i],y=file[,i+numcol],group=1)) + 
      geom_line(color="dodgerblue4",size=1)+
      geom_point() +
      labs( y = "", x = "")+
      ylim(c(0, 0.25))
    plot_list[[i*3-2]] = plot2_hist
    
    plot3_hist<-ggplot(file, aes(file[,i])) + 
      geom_bar(aes(y = (..count..)/sum(..count..))) +
      scale_y_continuous(labels=scales::percent) +
      geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
      theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4),plot.margin = unit(c(1,1,1,1), "cm") ) + 
      labs( y = "", x = "")+
      ggtitle(names(file)[i])
    plot_list[[i*3]] = plot3_hist
  }
  
  
  pdf("file5.pdf",width=10,height=15,paper='special') 
  marrangeGrob(plot_list,ncol=1,nrow=3)
  dev.off()


  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
