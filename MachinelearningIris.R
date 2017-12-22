#loading iris dataset

d=iris

#KNN on iris dataset for classification for flowers

library(class)
set.seed(1234)
s=sample(1:nrow(iris),size=nrow(iris)*.7)
tr=iris[s,]
te=iris[-s,] 
a=numeric()

for(i in 1:50){
  predict=knn(tr[,-5],te[,-5],tr$Species,k=i)
  a=c(a,mean(predict==te$Species))
}

plot(1-a,type="l",ylab="ERROR RATE",
     xlab="K",main="Error Rate for Iris Dataset With Changing K")
#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Linear regression(to predict length of petal based on width of petal)
  
  liris=lm(Petal.Length~Petal.Width, data=iris)

predict(liris, newdata=data.frame(Petal.Width=0.5))

plot(iris$Petal.Width, iris$Petal.Length,col = "red",main = "Prediction by Linear Regression on Iris Data",abline(liris),cex = 1.3,pch = 16,xlab = "Width of petal",ylab = "Length of petal")

#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #Decision trees(Prediction)
  
  names(iris)

install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")

s1= sample(150, 110)
tr1 = iris[s1,]
te1 = iris[-s1,]
tar = Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
tre = rpart(tar, data = tr1, method = "class")
rpart.plot(tre)

#predicting
install.packages("party")
library(party)
tree = ctree(Species ~ ., data = iris)
plot(tree, main="Conditional Tree for Iris")
table(predict(tree), iris$Species)

#prunning

tree3 = rpart(tar, tr1, control = rpart.control(minsplit = 3))
tree10 = rpart(tar, tr1, control = rpart.control(minsplit = 10))
par(mfcol = c(1, 2))
rpart.plot(tree3, main = "minimum split=3")
rpart.plot(tree10, main = "minimum split=10")

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #PCA(Principal component analysis)
  
  d = iris[,1:4]
l = iris[,5]
pca = prcomp(data,center=TRUE)
rd = t(pca[[2]]) %*% t(data)
rd = t(rd)
rd = data.frame(rd)
red = rotData[,1:2]
red$Species = label
ggplot(data=red)+geom_point(aes(x=PC1,y=PC2,color=Species))+geom_abline(intercept=3,slope=-2.15) +geom_abline(intercept=1.3,slope=-0.95,color="red")
#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #LOGISTIC REGRESSION
  
  iris$virginica = iris$Species == "virginica"
fit4 <- glm(virginica ~ Petal.Width + Petal.Length + Sepal.Length + Sepal.Width, data=iris2, family=binomial)
plot(fit4)
fit5 <- glm(round(Petal.Width) ~ Petal.Length + Sepal.Length + Sepal.Width, data=iris2, family=poisson)
plot(fit5)

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #NAIVE BAYES
  
  library(e1071)
fit <- naiveBayes(Species~., data=iris)
summary(fit)
predictions <- predict(fit, iris[,1:4])
table(predictions, iris$Species)
#-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #SVM
  
  library(kernlab)
fit <- ksvm(Species~., data=iris)
summary(fit)
predictions <- predict(fit, iris[,1:4], type="response")
table(predictions, iris$Species)
#--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  
  #NEURAL NETWORKS
  
  library(nnet)
fit <- nnet(Species~., data=iris, size=4, decay=0.0001, maxit=500)
summary(fit)
predictions <- predict(fit, iris[,1:4], type="class")
table(predictions, iris$Species)