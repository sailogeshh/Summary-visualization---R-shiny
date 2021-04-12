library(isotree)
#import data
data0=read.csv("insurance data.csv")
head(data)
#data type 
str(data0)
#remove variables
data=data0[,-c(1,4,5,7,19,26)]
#numeric to factor change 
data$auto_year=as.factor(data$auto_year)
str(data)
#absolute the variable - to +
data$capital.loss=abs(data$capital.loss)
#build model ISO
set.seed(1000)
iso <- isolation.forest(data, ntrees = 100, nthreads = 1)
#Predict anomaly score
pred <- predict(iso, data,type = "score")
outlier <- ifelse(pred > 0.50, "outlier", "normal")
df=data.frame(data0,outlier)
write.csv(df,file = "C:\\Users\\Analaytics\\Desktop\\files\\logistic reg outlier app insurance\\insurance data_isolation.csv")
table(outlier)  


