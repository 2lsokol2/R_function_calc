library(ggplot2)
library(dplyr)
library(Hmisc)

sales = read.csv("https://stepic.org/media/attachments/course/724/sales.csv")
str(sales)

# A lot of graphics for every month

ggplot(airquality, aes(factor(Ozone), Solar.R)) + 
  geom_point(aes(color = Month)) + 
  theme_bw() + 
  labs(title="Scatterplot") + 
  facet_wrap( ~ factor(Month))

# Bar chart with one variable(darkblue chart)
 ggplot(airquality, aes(Month)) + 
   geom_bar(fill = "red")+
   theme_bw()+
   scale_x_continuous("Establishment Year") + 
   scale_y_continuous("Count") +
   coord_flip()+
   labs(title = "Bar Chart") + 
   theme_gray()

 #Bar Chart with 2 variables
 ggplot(airquality, aes(Month, Ozone)) +
   geom_bar(stat = "identity",  fill = "darkblue") + 
   scale_x_discrete("Outlet Type") +
   scale_y_continuous("Item Weight", breaks = seq(0,15000, by = 500)) +
   theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
   labs(title = "Bar Chart")

 
### Practical things
## Different type of geoms 
#mean_cl_boot - edges of stat interval

ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), 
                   group = factor(cyl))) + 
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", 
                 width = 0.2) + 
    stat_summary(fun.data = mean_cl_boot, geom = "point", size = 2) +
    stat_summary(fun.y = mean, geom = "line")

sd_error <- function(x){
    c(y = mean(x), ymin = mean(x) - sd(x), ymax = mean(x) + sd(x))
}


ggplot(mtcars, aes(factor(am), mpg, col = factor(cyl), 
                   group = factor(cyl))) + 
    stat_summary(fun.data = sd_error, geom = "errorbar", 
                 width = 0.2, 
                 position = position_dodge(0.2)) + 
    stat_summary(fun.data = sd_error, geom = "pointrange", size = 2, 
                 position = position_dodge(0.2)) +
    stat_summary(fun.y = mean, geom = "line", 
                 position = position_dodge(0.2))

## Examples
#1

ggplot(sales, aes(income,sale))+
  geom_point(aes(colour = factor(shop))) +
  geom_smooth(method = "lm")

#2

my_plot <- ggplot(sales, aes(shop, income, col=season, group=season))+
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size=1, 
               position = position_dodge(0.2))

#3
ggplot(sales, aes(date, sale, col=shop, group=shop))+
  stat_summary(fun.data = mean_cl_boot, geom = 'Errorbar', size=1, 
               position = position_dodge(0.2)) +
  
  stat_summary(fun.data = mean_cl_boot, geom = 'point', size=1, 
               position = position_dodge(0.2)) +
  
  stat_summary(fun.data = mean_cl_boot, geom = 'line', size=1, 
               position = position_dodge(0.2)) 

### Fasets

glimpse(diamonds)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ color)

ggplot(diamonds, aes(carat, fill=cut)) + 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ color)

# by column
ggplot(diamonds, aes(carat, fill = cut))+ 
  geom_density(alpha = .1) + 
  facet_grid(. ~ color)

# by rows
ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(color ~ .)


# geom_smooth in facet                 
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl))) + 
  facet_grid(vs ~ am) + 
  geom_smooth(method = "lm")

ggplot(diamonds, aes(carat, price))+
  geom_smooth() +   
  facet_grid(color ~ .)

# geom_density
ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut, ncol = 1)

# geom_density in facet with 2 variables
# conterexample( why facet_grid bad for 2 variable)
ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut +color, nrow = 3)

# facet_grid&facet_wrap 
# facet_grid don`t have `nrow' and 'ncol'( facet_grid good for one facet variable)
# diff syntax - facet_grid(color ~ .)&facet_wrap(~ color)
# conterexample( why facet_grid bad for 2 variable)
ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut +color, nrow = 3)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(~ color + cut)

## Examples
#_1

ggplot(mtcars, aes(mpg)) + 
  geom_dotplot() + 
  facet_grid(am ~ vs)

#2_

ggplot(iris, aes(Sepal.Length)) + 
  geom_density() + 
  facet_wrap(~ Species)

#3_

ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point() +
  geom_smooth() + 
  facet_wrap(~ Species)

#4_

myMovieData <- read.csv("myMovieData.csv")

ggplot(myMovieData, aes(Type, Budget)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(.~Year)


## Scale
seq_x <- round(seq(min(mtcars$mpg), max(mtcars$mpg), length.out = 5))


# geom_point with different  setting
ggplot(mtcars, aes(x = mpg, y = hp, col = factor(am))) + 
  geom_point() +
  
  scale_x_continuous(name = "Miles/(US) gallon", 
                     breaks = c(1, seq(10, 35, 5)), 
                     limits = c(1, 35),
                     expand = c(1,1)) + #space 'under' plot
  scale_y_continuous(limits = c(50, 400)) +
  scale_color_manual(values = c("Blue", "Black"),  
                     name = "Legend name", 
                     labels = c("Auto", "Manual"))

# geom_density - different color for different density
ggplot(mtcars, aes(hp, fill = factor(am)))+
  geom_density(alpha = 0.2) +
  scale_fill_manual(values = c("Red", "Green"), name="type")

# geom_point with 0different shape
ggplot(mtcars, aes(hp, mpg, size = disp, shape = factor(vs))) + 
  geom_point() +
  scale_size_continuous(name = "Any name", 
                        breaks = seq(100, 400, 40)) +
  scale_shape_discrete(name = "Any name")

# name of axis labels
ggplot(mtcars, aes(factor(cyl), hp)) +
  geom_boxplot() +
  scale_x_discrete(name = "cylinders",
                   labels = c("4 cylinders",
                              "6 cylinders",
                              "8 cylinders"))


## Examples
#_1
ggplot(iris,aes(Sepal.Length, Petal.Length)) +
  geom_point(aes(colour = factor(Species))) +
  geom_smooth(aes(colour = factor(Species)))+
  scale_color_discrete(name =  "??? ??????",
                       labels = c( "???? ??????????", 
                                   "???? ????????????", 
                                   "???? ??????????")) +
  scale_x_continuous(name = "????? ???????????", 
                     breaks =  seq(4, 8, 1),
                     limits = c(4, 8))+ 
  scale_y_continuous(name = "????? ????????", 
                     breaks =  seq(1, 7, 1))

# chose palitra for every variable with scale_fill_brewer for boxplot
# ?scale_fill_brewer - http://colorbrewer2.org (choose palitra)
ggplot(mtcars, aes(factor(am), hp, fill = factor(cyl))) + 
  geom_boxplot() +
  scale_fill_brewer(type = "qual", palette = 6) +
  theme_bw()

# chose palitra for every variable with scale_color_brewer for point
ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
  geom_point(size = 5) +
  scale_color_brewer(type = "qual", palette = 6) +
  theme_dark()

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
  geom_point(size = 5) +
  scale_color_brewer(type = "qual", palette = 8) +
  theme_bw()

#add theme
ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
  geom_point(size = 5) +
  scale_color_brewer(type = "qual", palette = 6) +
  theme(text = element_text(size = 14),
        axis.line.x = element_line(size = 2))





#install.packages("ggthemes")
library('ggthemes')

ggplot(mtcars, aes(hp, mpg, col = factor(cyl))) + 
  geom_point(size = 2) +
  theme_update()

#?http://www.ggplot2-exts.org/gallery/


## practical examples

setwd("E:/scoring")

d <- read.csv("example_data.csv")
str(d)

# ggplot with different settings
p <- ggplot(d, aes(date, percent, col = system, group = system))+
  geom_line(size = 1.3) +
  geom_point(shape = 21, size = 4, fill = "black") + 
  geom_point(shape = 21, size = 3.5) + 
  geom_point(shape = 21, size = 3) + 
  geom_vline(xintercept = 7.5, color = "white", 
             linetype = "dotted") + 
  scale_y_continuous(breaks = c(0, .04, .08, .11, .15), 
                     limits = c(0, .15), 
                     labels = scales::percent) +  
  scale_color_manual(values = c("orangered1", 
                                "red", 
                                "cyan", 
                                "yellow1",
                                "springgreen2")) +
  xlab("")+
  ylab("")+
  ggtitle("Top 5 Linux distributions (% of total per year)")+
  theme_classic()

# add theme
p + theme(legend.title = element_blank(), 
          legend.position = "top", 
          plot.background = element_rect(color = "black", 
                                         fill = "black"),
          panel.background = element_rect(color = "black", 
                                          fill = "black"),
          legend.background = element_rect(fill = "black"),
          text = element_text(color = "white"), 
          panel.grid.major.y = element_line(color = "gray50",
                                            linetype = "longdash", 
                                            size = 0.3), 
          axis.text.x = element_text(face = "bold", size = 16), 
          axis.text.y = element_text(face = "bold", size = 14), 
          legend.text = element_text(size = 14), 
          title = element_text(face = "bold", size = 16))

# also you can save theme in separate vatiable

my_theme <- theme(legend.title = element_blank(), 
                  legend.position = "top", 
                  plot.background = element_rect(color = "black", 
                                                 fill = "black"),
                  panel.background = element_rect(color = "black", 
                                                  fill = "black"),
                  legend.background = element_rect(fill = "black"),
                  text = element_text(color = "white"), 
                  panel.grid.major.y = element_line(color = "gray50",
                                                    linetype = "longdash", 
                                                    size = 0.3), 
                  axis.text.x = element_text(face = "bold", size = 16), 
                  axis.text.y = element_text(face = "bold", size = 14), 
                  legend.text = element_text(size = 14), 
                  title = element_text(face = "bold", size = 16))

# add text to the ggplot
library(grid)

grid.text("Data sourse: The DistroWatch's Page Hit Ranking (Nov. 23, 2011)",
          x = 0.02, y = 0.01, just = c("left", "bottom"), 
          gp = gpar(fontface = "bold", fontsize = 9, col = "white"))

grid.text("www.pingdom.com",
          x = 0.98, y = 0.01, just = c("right", "bottom"), 
          gp = gpar(fontface = "bold", fontsize = 9, col = "white"))



glimpse(diamonds)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ color)

ggplot(diamonds, aes(carat, fill=cut)) + 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ color)

# by column
ggplot(diamonds, aes(carat, fill = cut))+ 
  geom_density(alpha = .1) + 
  facet_grid(. ~ color)

# by rows
ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(color ~ .)


ggplot(mtcars, aes(mpg))+ 
  geom_point() + 
  facet_grid(am ~ vs)

mtcars <- mutate(mtcars, 
                 am = factor(am, labels = c("A", "M")), 
                 vs = factor(vs, labels = c("V", "S")))

# geom_smooth in facet                 
ggplot(mtcars, aes(hp, mpg))+
  geom_point(aes(col = factor(cyl))) + 
  facet_grid(vs ~ am) + 
  geom_smooth(method = "lm")


ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut, ncol = 1)


ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_wrap( ~ cut + color)

ggplot(diamonds, aes(carat))+ 
  geom_density(alpha = .1) + 
  facet_grid(cut ~ .)

ggplot(diamonds, aes(carat, price))+
  geom_smooth() +   
  facet_grid(color ~ .)
  
