# Black-Jaguar
install.packages("caret")
library(caret)
training<-read.csv("U:\\Terex\\Practical machine learning-John Hopkins\\Project data\\pml-training.csv")
testing<-read.csv("U:\\Terex\\Practical machine learning-John Hopkins\\Project data\\pml-testing.csv")
View(training)
View(training$classe)
ncol(training)
set.seed(1846)
nearzero <- nearZeroVar(training, saveMetrics = TRUE)
training <- training[,!nearzero$nzv]
ncol(training)
rvar<-sapply(colnames(training),function(x)if(sum(is.na(training[,x]))>0.5*nrow(training)){return(TRUE)}else{return(FALSE)})
training<-training[,!rvar]
View(training)
training<-training[,-c(1:6)]
ncol(training)
c<-findCorrelation(cor(training[,-53]),cutoff = 0.85)
names(training[c])
c<-findCorrelation(cor(training[,-53]),cutoff = 0.75)
names(training[c])
tc<-trainControl(method = "repeatedcv",number = 5,preProcOptions = "pca")
