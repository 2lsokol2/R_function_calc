library(ggplot2)

data<-read.csv("woe.csv",stringsAsFactors=FALSE,header=TRUE)

#generate random target(for this variant)
target<-rbinom(n = 463, size = 1, prob = 0.85)
#data<-cbind(target,data)

#generate "data" with first column "target" and another columns are sorted alphabetically
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"

str(data)
View(data)
names(data)

#t1-aggregate data for create BR
t1<-aggregate(. ~ gender, data = data[c("target","gender")], 
          FUN = function(x) c(all = length(x), bad = sum(x) ) )

#counting BR(new columns in aggregate func in R is MATRIX)
t2<-transform(t1,wBR_gender=1-(t1[,2][,2] / t1[,2][,1]))[,c(1,3)]

#add  new column "BR"to our "data" table
data<-merge(data,t2,by.x="gender",by.y=names(t2)[1])

#again sort column,cauth "BR" goes to the end,but joined column goes to the begining,totaly bad column)
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"


ggplot(data, aes(x=gender,y=wBR_gender,group=1)) + 
  geom_point() + geom_line(color="dodgerblue4",size=1)+
  
lapply(var[1]:var[length(var)],function(x)
  ggplot(file, aes(file[,x])) + 
    var<-names(table)[2]
  
library(gridExtra)
library(ggpubr)
library(cowplot)
library(ggplot2)
#install.packages("cowplot")


density.p <- ggdensity(iris, x = "Sepal.Length", 
                       fill = "Species", palette = "jco")
# Âûâåñòè ñâîäíóþ òàáëèöó Sepal.Length
#::::::::::::::::::::::::::::::::::::::
# Âû÷èñëèòü îïèñàòåëüíûå ñòàòèñòèêè ïî ãðóïïàì
stable <- desc_statby(iris, measure.var = "Sepal.Length",
                      grps = "Species")
stable <- stable[, c("Species", "length", "mean", "sd")]
# Ãðàôèê ñî ñâîäíîé òàáëèöåé, òåìà "medium orange" (ñðåäíèé îðàíæåâûé)
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Âûâåñòè òåêñò
#::::::::::::::::::::::::::::::::::::::
text <- paste("iris data set gives the measurements in cm",
              "of the variables sepal length and width",
              "and petal length and width, respectively,",
              "for 50 flowers from each of 3 species of iris.",
              "The species are Iris setosa, versicolor, and virginica.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")
# Ðàçìåñòèòü ãðàôèêè íà ñòðàíèöå
ggarrange(density.p, x, 
          ncol = 1, nrow = 2)
annotation_custom(grob, xmin, xmax, ymin, ymax)

density.p + annotation_custom(ggplotGrob(stable.p),
                             xmin = 3, ymin = -3,
                             
sp <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
                                             color = "Species", palette = "jco",
                                             size = 3, alpha = 0.6)                                                        xmax = 8)


p1 <- ggarrange(sp, bp + font("x.text", size = 9),
                ncol = 1, nrow = 2)
p2 <- ggarrange(density.p, stable.p, text.p, 
                ncol = 1, nrow = 3,
                heights = c(1, 0.5, 0.3))

p1<-ggplot(data, aes(x=gender,y=wBR_gender,group=1)) + 
  geom_point() + geom_line(color="dodgerblue4",size=1)

p2<-ggtexttable(t2, rows = NULL, 
                theme = ttheme("mOrange"))
ggarrange(p1, p2, ncol = 1, nrow = 1)

multi.page <- ggarrange(p1, p2,
                        ncol = 1, nrow = 2,
                        heights = c(2, 2, 2))


ggexport(multi.page, filename = "multi.page.ggplot5.pdf")

  
  data<-read.csv("woe.csv",colClasses = "factor",header=TRUE)
str(data)
#generate random target(for this variant)
target<-rbinom(n = 463, size = 1, prob = 0.85)
#data<-cbind(target,data)

#generate "data" with first column "target" and another columns are sorted alphabetically
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"

str(data)
View(data)
names(data)

#t1-aggregate data for create BR
t1<-aggregate(. ~ data[,10], data = data[c(data[,1],data[,10])], 
              FUN = function(x) c(all = length(x), bad = sum(x) ) )[,c(1,2)]

#counting BR(new columns in aggregate func in R is MATRIX)
t2<-transform(t1,br=1-(t1[,2][,2] / t1[,2][,1]))[,c(1,3)]

#recall "t2",cauth next action will be join "t2" and "file",
#(we must have united column with common name)
#"BR" column begins with "WW_"
#(all variables soryed by alphabet,we split table by 2 part,with "BR" with WW_" and will be in the right part of table) )
names(t2)<-c(names(data)[10],paste0("WW_",names(data)[10]))

#add  new column "BR"to our "data" table
data<-merge(data,t2,by.x=names(data)[10],by.y=names(t2)[1])


#again sort column,cauth "BR" goes to the end,but joined column goes to the begining,totaly bad column)
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"

View(data)
ggplot(data, aes(x=data[,10],y=data[,24],group=1)) + 
  geom_point() + geom_line(color="dodgerblue4",size=1)
  
lapply(var[1]:var[length(var)],function(x)
  ggplot(file, aes(file[,x])) + 
    var<-names(table)[2]
  
library(gridExtra)
library(ggpubr)
library(cowplot)
library(ggplot2)
#install.packages("cowplot")


density.p <- ggdensity(iris, x = "Sepal.Length", 
                       fill = "Species", palette = "jco")
# Вывести сводную таблицу Sepal.Length
#::::::::::::::::::::::::::::::::::::::
# Вычислить описательные статистики по группам
stable <- desc_statby(iris, measure.var = "Sepal.Length",
                      grps = "Species")
stable <- stable[, c("Species", "length", "mean", "sd")]
# График со сводной таблицей, тема "medium orange" (средний оранжевый)
stable.p <- ggtexttable(stable, rows = NULL, 
                        theme = ttheme("mOrange"))
# Вывести текст
#::::::::::::::::::::::::::::::::::::::
text <- paste("iris data set gives the measurements in cm",
              "of the variables sepal length and width",
              "and petal length and width, respectively,",
              "for 50 flowers from each of 3 species of iris.",
              "The species are Iris setosa, versicolor, and virginica.", sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")
# Разместить графики на странице
ggarrange(density.p, x, 
          ncol = 1, nrow = 2)
annotation_custom(grob, xmin, xmax, ymin, ymax)

density.p + annotation_custom(ggplotGrob(stable.p),
                             xmin = 3, ymin = -3,
                             
sp <- ggscatter(iris, x = "Sepal.Length", y = "Sepal.Width",
                                             color = "Species", palette = "jco",
                                             size = 3, alpha = 0.6)                                                        xmax = 8)


p1 <- ggarrange(sp, bp + font("x.text", size = 9),
                ncol = 1, nrow = 2)
p2 <- ggarrange(density.p, stable.p, text.p, 
                ncol = 1, nrow = 3,
                heights = c(1, 0.5, 0.3))

ggplot(data, aes(gender)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
  labs(title = "names(data)", y = "Percent", x = "Class") +
  theme(axis.text.x = element_text(angle=10, vjust=0.9)) + 
  theme(plot.margin = unit(c(1,1,1,1), "cm"))

 ###begining of loop 
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
##LAST
library(gridExtra)
library(ggpubr)
library(cowplot)
library(ggplot2)

data<-read.csv("woe.csv",colClasses = "factor",header=TRUE)

#generate random target(for this variant)
target<-rbinom(n = 463, size = 1, prob = 0.85)
data<-cbind(target,data)

#generate "data" with first column "target" and another columns are sorted alphabetically
data<-cbind(data$target,(data[ , sort(names(data[,-which( colnames(data)=="target")]))]))

#recall first column,cauth automatically programe calls it "data$target"
names(data)[1]<-"target"
file<-data
#str(file)
#ncol(file)
str(file)
plot_list = list()
plot_list[[1]]<-""
plot_list[[2]]<-""
plot_list[[3]]<-""

#i - num var
i<-2 
#second variable is "numplot" - 3 plots for every variable
#numcol<-ncol(file)



while (i <=5){
  plot1_hist<-ggplot(file, aes(file[,i])) + 
    geom_bar(aes(y = (..count..)/sum(..count..)))+
    scale_y_continuous(labels=scales::percent) +
    geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4), plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "", x = "")+
    ggtitle(names(file)[i])
  plot_list[[i*3-2]] = plot1_hist
  
  
  plot2_hist<-ggplot(file, aes(x=file[,i],y=file[,i+22],group=1)) + 
    geom_line(color="dodgerblue4",size=1)+
    geom_point() +
    labs( y = "", x = "")+
    ylim(c(0, 0.5))
  plot_list[[i*3-2]] = plot2_hist
  
  plot3_hist<-ggplot(file, aes(file[,i])) + 
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    scale_y_continuous(labels=scales::percent) +
    geom_text(aes( y = ((..count..)/sum(..count..)),label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.01) +
    theme(axis.text.x = element_text(angle=10, vjust=0.9), plot.title = element_text(size = 22, face = "bold",hjust = -0.1, vjust=4),plot.margin = unit(c(1,1,1,1), "cm") ) + 
    labs( y = "", x = "")+
    ggtitle(names(file)[i])
  plot_list[[i*3]] = plot3_hist
  i<-i+1
}


pdf("file900.pdf",width=10,height=15,paper='special') 
marrangeGrob(plot_list,ncol=1,nrow=3)
dev.off()

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
---------------------------------------------------------------------------------------  
  
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

plot_list = list()
plot_list[[1]]<-1
for j in (2:ceiling(ncol(file)/5)){
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


t<-ggarrange(plot1_hist,plot2_hist, stable.p,
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))

plot_list[[j]] = t
}


dev.off()




jpeg(paste0(j,"_",names(file)[j],".jpeg")) 
ggarrange(plot1_hist,plot2_hist, stable.p,
          ncol = 1, nrow = 3,
          heights = c(1, 0.5, 0.3))
dev.off()





 
  
  
  
  
  
  
  
  
  
  
  
