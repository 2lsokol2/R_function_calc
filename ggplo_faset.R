library(ggplot2)
library(dplyr)

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
  
