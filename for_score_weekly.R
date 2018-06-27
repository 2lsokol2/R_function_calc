# ==========  1 встреча  ===========================

# загружаем данные из файла:
sample<-read.csv2("data_new.csv")

# смотрим на структуру файла:
head(sample)
str(sample)

# гистограмма по отдельным категориям:
install.packages("ggplot2")
library(ggplot2)
ggplot(sample, aes(x = Family_status_ukr)) + geom_bar()
summary(sample$Family_status_ukr)

# гистограмма по численным переменным (с возможностью разбивки на определенное количество категорий)
hist(sample$Age_y)
hist(sample$Age_y,breaks = 20)

# статистика по категориальным
summary(sample$Family_status_ukr)
# статистика по численным
summary(sample$Age_y)

# удалить записи с определенным значением (через переменную)
delete_position <- sample$Gender != "Gender:_F"
sample2 <- sample[delete_position,]
table(sample$Gender)
table(sample2$Gender)

# удалить записи с определенным значением (короткая)
sample3<-head(sample[,1:10])

# изменить запись - численные
sample3$target_for_calc[sample3$target_for_calc == 1] <- 0

# изменить запись - категориальные
levels(sample3$Gender)[levels(sample3$Gender) == "Gender:_M"] <- "Male"

# переименовать категории
levels(sample3$Gender)
levels(sample3$Gender)<- c("Female", "Male")
levels(sample3$Gender)

# Построение WoE-таблицы
library(plyr)
Gender_stat<-data.frame(count(sample[sample$target_60max12m == "good",], c("Gender") ),count(sample[sample$target_60max12m == "bad",], c("Gender") )[,2], count(sample, c("Gender"))[,2])
colnames(Gender_stat)<-c(colnames(Gender_stat)[1], "good","bad","total")
Total_stat<-summary(sample$target_60max12m)
Gender_stat <- data.frame(Gender_stat,"share of good" = Gender_stat$good/Total_stat["good"], "share of bad" = Gender_stat$bad/Total_stat["bad"], "share of total" = Gender_stat$total/sum(Total_stat),"BR" = Gender_stat$bad/Gender_stat$total)
Gender_stat <- data.frame(Gender_stat,"Woe" = log(Gender_stat$share.of.good/Gender_stat$share.of.bad))
Gender_stat

# Расчет IV
IV <- data.frame("char_name" = colnames(Gender_stat)[1], "IV" = sum((Gender_stat$share.of.good-Gender_stat$share.of.bad)*Gender_stat$Woe))
IV

# Удаление данных по условиям (не учитывать NA и другое ...)
sample_del<-read.csv2("data_new.csv")

# Вычислить среднее значение по столбцу и не учитывать пустые
mean(sample_del$Age_y,na.rm=TRUE)

# Удалить все строки, в которых хотя бы раз встречается NA
sample_without_del<-sample_del[complete.cases(sample_del),]
sample_without_del<-na.omit(sample_del)

# Проверить, есть ли хотя бы 1 значение NA
any(!complete.cases(sample_without_del))

# Удалить столбец
sample_del$ABU_client_from_appl<-NULL

# Удалить столбец по условию
sample_del2<-sample_del[sample_del$client_type_2!="New"]

# Объединение 2 таблиц
sample_del3<-rbind(sample_del2,sample_del)

# ==========  2 встреча  ===========================

# загружаем данные, сохраняя оригинальные типы данных:
sample<-read.csv2("data_new.csv", as.is = TRUE,sep=";",na.strings = c("NULL","NA"))

# замена NULL на NA
is.na(sample) <- sample == "NULL"

# Information Value and WOE
install.packages("InformationValue")
library(InformationValue)

# для отдельных переменных
WOE(X=sample$Gender, Y=sample$target_for_calc)
WOETable(X=sample$Gender, Y=sample$target_for_calc)

IV_Gender <- IV(X=sample$Gender, Y=sample$target_for_calc)
IV_Education_ukr <- IV(X=sample$Education_ukr, Y=sample$target_for_calc)
IV_Family_status_ukr <- IV(X=sample$Family_status_ukr, Y=sample$target_for_calc)

IV_Gender
IV_Education_ukr
IV_Family_status_ukr

install.packages("Information")
library(Information)

# для всех переменных
infotables <- create_infotables(data = sample[,9:41], y = "target_for_calc", bins = 10, parallel = TRUE)
infotables$Summary

plot_infotables(infotables, "Family_status_ukr", aes(x = Variable, y = WOE)) +
  ggtitle("Family_status_ukr") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90)) 

# гистограммы с WOE для всех переменных
plot_infotables(infotables, infotables$Summary$Variable[1:31], same_scale=FALSE)

# гистограмма для сравнения IV для разных переменных
plotframe <- infotables$Summary[order(-infotables$Summary$IV), ]

ggplot(plotframe, aes(x = Variable, y = IV)) +
  geom_bar(width = .35, stat = "identity", color = "darkblue", fill = "white") +
  ggtitle("Information Value") +
  theme_bw() +
  theme(plot.title = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 90))

# конвертация даты
new_date_format <- as.POSIXct.Date(sample$date_insert, )
class(new_date_format)
class(sample$date_insert)
sample$date_insert

# выгрузка таблиц
write.csv(infotables$Summary, file="C:/Users/DButenko/Documents/sample_r.xls")

# выгрузка графиков
getwd() # проверка текущей категории, в которую будут по дефолту выгружаться данные, 
# если не задать путь к файлу полностью

pdf(file="C:/Users/DButenko/Documents/figure_USr.pdf", paper="USr")
plot_infotables(infotables, infotables$Summary$Variable[1:31], same_scale=FALSE)
dev.off()

png(file="C:/Users/DButenko/Documents/figure1.png")
plot_infotables(infotables, infotables$Summary$Variable[1:31], same_scale=FALSE)
dev.off()

# ==========  3 встреча  ===========================

## SQL connection

library(RMySQL) # will load DBI as well
# open a connection to a MySQL database
con <- dbConnect(dbDriver("MySQL"), user = "root", password = "root", dbname = "pookas")
# list the tables in the database
dbListTables(con)
# load a data frame into the database, deleting any existing copy
data(USArrests)
dbWriteTable(con, "arrests", USArrests, overwrite = TRUE)
dbListTables(con)
# get the whole table
dbReadTable(con, "arrests")
# Select from the loaded table
dbGetQuery(con, paste("select row_names, Murder from arrests",
                      "where Rape > 30 order by Murder"))
dbRemoveTable(con, "arrests")
dbDisconnect(con)

# https://turbofuture.com/computers/Connect-to-SQL-Server-from-R 

## Binning

install.packages("classInt")
library(classInt)
install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
library(spDataLarge)

x <- classIntervals(sample$Age_y, 4, style = 'equal')
x
y <- classIntervals(sample$Age_y, 4, style = 'quantile')
y

install.packages("smbinning")
library(smbinning)

result=smbinning(df=sample, y="target_for_calc", x="Age_y", p=0.05) 
View(result)
result$ivtable

install.packages("woeBinning")
library("woeBinning")

woe.binning(sample, 'target_for_calc','Age_y',min.perc.total= 0.01, min.perc.class = 0.05, stop.limit=0.1)
