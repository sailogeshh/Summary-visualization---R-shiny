library(plotly) #required library for plotting
data <- read.csv("insurance data.csv") #import data

data = data[,-1]# removing ID column

# removing variable which is factors
data1 = unlist(lapply(data, is.numeric))
data1 = data[,data1]

#taking columns count from above data
dkk = dim(data1)[2]

#Fixing boxplot from plotly
p = plot_ly(type = 'box')

#for loop for draw plot for all selected variables
for (i in 1:dkk) {
  p = add_boxplot(p, y = data1[,i],name = names(data1)[i])
}
p
