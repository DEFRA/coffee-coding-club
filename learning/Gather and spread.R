# R coffee and coding: Data manipulation tutorial

# Load the packages tidyr
library(tidyr)

# Look at prebuilt in dataset mtcars
mtcars

# Add row called car, with the name of the car as the row names will dissapear when we start manipulating dataset
mtcars$car=rownames(mtcars)

# now we convert this wide data set into long using gather() in tidyr
mtcars.long=gather(mtcars,variable,value,mpg:carb)
mtcars.long

# We can also use a -variable to keep that variable wide
mtcars.long=gather(mtcars,variable,value,mpg:carb,-hp,-mpg)
mtcars.long

library(ggplot2)
ggplot(mtcars.long,aes(x=hp,y=value,color=mpg))+geom_point()+facet_wrap(~variable,scales = "free")
ggplot(mtcars.long,aes(x=hp,y=value,color=variable))+geom_point()

# We can convert this from long to wide using
mtcars.wide=spread(mtcars.long,variable,value)
mtcars.wide


# create summary tables
table1=subset(mtcars,select=c("gear","cyl"))
table1
table(table1)

# Other type of summary
summary(table1)


