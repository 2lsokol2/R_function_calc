
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(cowplot)

data<-read.csv2("woe.csv",sep=",",dec=".",colClasses = "factor")

#generate random target(for this variant)
target<-rbinom(n = 463, size = 1, prob = 0.85)
data<-cbind(target,data)
#generate "data" with first column "target" and another columns are sorted alphabetically
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"
file<-data

str(file)
ncol(data)
##create loop for BR
begin_ncol<-ncol(file)
i<-2
while (i<=begin_ncol){
  #t1-aggregate file for create BR
  t1<-aggregate(. ~ file[,i], data = file[c(names(file)[1],names(file)[i])],
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
  i<-i+1
}

while (j<=ceiling(ncol(file)/5)){
str(file)
plot_list = list()
plot_list[[1]]<-""
j<-2


pdf("plots5.pdf",width = 10,height=15,paper='special')
for (j in 2:6) {
  plot1_hist<-ggplot(file, aes(file[,j])) + 
    geom_bar(aes(y = (..count..)/sum(..count..)))+
    scale_y_continuous(labels=scales::percent) +
    geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9),
          plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4), 
          plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "", x = "")+
    ggtitle(names(file)[j])
  
  plot2_line<-ggplot(file, aes(x=file[,j],y=file[,j+22],group=1)) + 
    geom_line(color="dodgerblue4",size=1)+
    geom_point() +
    labs( y = "", x = "")+
    ylim(c(0, 0.9))
  stable <- desc_statby(iris, measure.var = "Sepal.Length",
                        grps = "Species")
  stable <- stable[, c("Species", "length", "mean", "sd")]
  # График со сводной таблицей, тема "medium orange" (средний оранжевый)
  stable.p <- ggtexttable(stable, rows = NULL, 
                          theme = ttheme("mOrange"))
  
print(ggarrange(plot1_hist, plot2_line, stable.p , 
            ncol = 1, nrow = 3,
            heights = c(1, 1, 0.7)))
  
}

dev.off()




 
  
  
  
  
  
  
  
  
  
  
  
