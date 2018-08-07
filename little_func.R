cat(..., file = "", sep = " ")
# превращает все указанные аргументы (...)
# в текст и сохраняет результат в виде файла; разделитель между
# получаемыми текстовыми значениями задается при помощи аргумента sep

x[x > 3 & x < 5]
# возвращает все элементы вектора x со значением >3 и <5
 
x[x %in% c("a","and","the")] 
# возвращает только те элементы вектора x, которые указаны после 
# оператора %in% (здесь из вектора x были бы извлечены текстовые значения
# "a", "and", и "the")

# Создадим тестовую таблицу данных.
> mydata <- data.frame(name = c("Antony", "Bob", "Cecilia", "Jack", "Mary", "Tony"),
                     col_a = c(0, 1, 0, 1, 2, 3),
                    col_b = c(3, 0, 3, 3, 1, 0),
                     stringsAsFactors=FALSE)
 mydata


# Нужные нам строки - 1-ая и 6-ая
grep('[t,T]ony', mydata$name)

mydata[grep('[t,T]ony', mydata$name),]
# Создадим пустую таблицу mydata с числовыми колонками First, Second
# и символьной колонкой Third:
mydata <- data.frame(First=numeric(0),
                     Second=numeric(0),
                     Third = character(0),
                     stringsAsFactors=FALSE)
str(mydata)
#Как удалить из таблицы колонки с заданными именами
# Создаем таблицу
df = data.frame(c1=1:4, c2=letters[1:4], c3=5:8)
# Удаляем колонку "с2"
df_new = df[ , -which(names(df) %in% c("c2"))]

#unique and order
unique(file[,c(1,4,26)][with(file[,c(1,4,26)], order(bty_f1lower,WW_bty_f1lower)), ])
