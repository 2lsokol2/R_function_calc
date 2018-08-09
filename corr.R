library(corrplot)
#install.packages("corrplot")

data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]
# print the first 6 rows
head(my_data, 6)

res <- cor(my_data)
round(res, 2)

#delete NA
cor(my_data, use = "complete.obs")

#Correlation matrix with significance levels (p-value)



corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)