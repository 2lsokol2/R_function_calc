#install.packages("Information")
#install.packages("smbinning")
#install.packages("woe")
#install.packages("klaR")
#install.packages("knitr")

#The data is from an historical marketing campaign 
#training,validation dataset. Each file has 68 predictive variables and 10k records.

library(Information)
library(knitr)

data(train, package="Information")
data(valid, package="Information")

#Exclude the control group, add one col, that have only one value '1'
train <- subset(train, TREATMENT==1)
valid <- subset(valid, TREATMENT==1)

#data - training,valid - main data, y - dependent variable, bins - bins qty
#penalty - WOE_train-WOE_valid
#variable will be deleted, if it has min inf value
IV <- create_infotables(data=train,
                        valid=valid,
                        y="PURCHASE")

#The strongest six variables:
#kable - made better table 
kable(head(IV$Summary), row.names=FALSE)


#go to WOE
IV$Tables
kable(IV$Tables$N_OPEN_REV_ACTS)
names <- names(IV$Tables)

#go to plot for WOE
plot_infotables(IV, "N_OPEN_REV_ACTS")

#I want a lot of WOEEEEEEE!
MultiPlot(IV, IV$Summary$Variable[1:16])

#omiting cross validation
IV <- create_infotables(data=train, y="PURCHASE")


#bing as much as u wish, I want 20)
#defolt=10
IV <- create_infotables(data=train,
                        valid=valid,
                        y="PURCHASE", 
                        bins=20)

kable(IV$Tables$N_OPEN_REV_ACTS)

