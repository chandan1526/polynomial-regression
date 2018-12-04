#====================================================================================== =======
# OVERFITTING DUE TO INCREASE IN NUMBER OF FEATURES IN LINEAR REGRESSION

#=============================================================================================
library(ggplot2)
car = read.csv("C:\\Users\\stud\\Downloads\\cars.csv")

names(car)
View(car)

plot(car$MPG,car$Weight)
cor(car$MPG,car$Weight)


#DATA:
set.seed(5)
rand = sample(1:nrow(car),100)
train = car[rand, ]

 
set.seed(3)
rand = sample(1:nrow(car),50)
test = car[rand, ]



#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7 for n=20
#=============================================================================================


m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7), train)
m7

#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 7(n=20)',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20) 

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$MPG)^2)

# following the same code for diffrent sample size
#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7 with n=30
#=============================================================================================

#DATA:
set.seed(1)
rand = sample(1:nrow(car),30)
train = car[rand, ]

m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7), train)
m7
#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 7(n=30)',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$MPG)^2)
################################################################################################
#part 1 b
#Test error vs n(order)
set.seed(2)

ord=c(20,30,50,70,100,200,350)
Test_Error=c()
for (i in ord){
  rand=sample(1:nrow(car),i)
  train=car[rand,]
  test=car[-rand,]
  model=lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
                 + I(Weight^6) + I(Weight^7), train)
  sum(model$residuals^2)
  pred = predict(model, newdata=test)
  Test_Error=append(Test_Error,sum((pred-test$MPG)^2))
  train=c()
  test=c()
}
Test_Error
jpeg(filename="D:/New folder/Barplot/Samplesize2.jpeg")
plot(ord,Test_Error,pch=20,cex=0.5,xlab="Sample size",ylab="Test Error",ylim=c(500,300000), main="Test Error vs n(sample size)" )
#lines(sort(l),fitted(model)[order(l)],col="red",type="l")
lines(ord,Test_Error,col="blue",type="l")
dev.off()




###################################################################################################
# PART2


#DATA:
set.seed(15)
rand = sample(1:nrow(car),20)
train = car[rand, ]


set.seed(3)
rand = sample(1:nrow(car),50)
test = car[rand, ]

#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 1
#=============================================================================================
set.seed(15)
rand = sample(1:nrow(car),100)
train = car[rand, ]

m1 <- lm(MPG ~ Weight, train)
m1

#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 1,2,7,8,9,10',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m1)[order(train$Weight)], col='red', type='l') 


#TRAIN AND TEST ACCURACY
sum(m1$residuals^2)
pred = predict(m1, newdata=test)
sum((pred-test$MPG)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 2
#=============================================================================================
set.seed(5)
rand = sample(1:nrow(car),20)
train = car[rand, ]

m2 <- lm(MPG ~ Weight + I(Weight^2), train)
m2

#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 2',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m2)[order(train$Weight)], col='blue', type='l', pch=19) 

#TRAIN AND TEST ACCURACY
sum(m2$residuals^2)
pred = predict(m2, newdata=test)
sum((pred-test$MPG)^2)


#=============================================================================================
# FITTING A POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

#DATA:
set.seed(5)
rand = sample(1:nrow(car),20)
train = car[rand, ]



m7 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7), train)
m7
#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 7',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m7)[order(train$Weight)], col='brown', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m7$residuals^2)
pred = predict(m7, newdata=test)
sum((pred-test$MPG)^2)

# FITTING A POLYNOMIAL REGRESSION OF ORDER 8 
#=============================================================================================

#DATA:
set.seed(5)
rand = sample(1:nrow(car),20)
train = car[rand, ]

m8 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7)+I(Weight^8), train)
m8
#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order ',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m8)[order(train$Weight)], col='green', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m8$residuals^2)
pred = predict(m8, newdata=test)
sum((pred-test$MPG)^2)


#FITTING A POLYNOMIAL REGRESSION OF ORDER 9
#=============================================================================================

#DATA:
set.seed(5)
rand = sample(1:nrow(car),20)
train = car[rand, ]

m9 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9), train)
m9

#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 9',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m9)[order(train$Weight)], col='yellow', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m9$residuals^2)
pred = predict(m9, newdata=test)
sum((pred-test$MPG)^2)


#FITTING A POLYNOMIAL REGRESSION OF ORDER 10
#=============================================================================================

#DATA:
set.seed(5)
rand = sample(1:nrow(car),20)
train = car[rand, ]

m10 <- lm(MPG ~ Weight + I(Weight^2) + I(Weight^3) + I(Weight^4) + I(Weight^5)+ I(Weight^5) 
         + I(Weight^6) + I(Weight^7)+I(Weight^8)+I(Weight^9)+I(Weight^10), train)
m10
#PLOTTING THE MODEL OVER THE DATA
plot(main='Polynomial regression of order 10',train$Weight,train$MPG, pch=19, cex=0.5)
lines(sort(train$Weight), fitted(m10)[order(train$Weight)], col='sky blue', type='l',pch=20)

#TRAIN AND TEST ACCURACY
sum(m10$residuals^2) #train
pred = predict(m10, newdata=test)
sum((pred-test$MPG)^2) #test

#PART3 RMSE VS COMPLEXITY
#TRAIN AND TEST ACCURACY(Model M1,m2,m7,m8,m9,m10)
Trn_RMSE1=sqrt(mean(sum(m1$residuals^2)))
Trn_RMSE1
pred = predict(m1, newdata=test)
Tst_RMSE1=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE1

Trn_RMSE2=sqrt(mean(sum(m2$residuals^2)))
Trn_RMSE2
pred = predict(m2, newdata=test)
Tst_RMSE2=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE2

Trn_RMSE7=sqrt(mean(sum(m7$residuals^2)))
Trn_RMSE7
pred = predict(m7, newdata=test)
Tst_RMSE7=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE7


Trn_RMSE8=sqrt(mean(sum(m8$residuals^2)))
Trn_RMSE8
pred = predict(m8, newdata=test)
Tst_RMSE8=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE8

Trn_RMSE9=sqrt(mean(sum(m9$residuals^2)))
Trn_RMSE9
pred = predict(m9, newdata=test)
Tst_RMSE9=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE9

Trn_RMSE10=sqrt(mean(sum(m10$residuals^2)))
Trn_RMSE10
pred = predict(m10, newdata=test)
Tst_RMSE10=sqrt(mean(sum((pred-test$MPG)^2)))
Tst_RMSE10








Test_RMSE=c(Tst_RMSE1,Tst_RMSE2,Tst_RMSE7,Tst_RMSE8,Tst_RMSE9,Tst_RMSE10)
Train_RMSE=c(Trn_RMSE1,Trn_RMSE2,Trn_RMSE7,Trn_RMSE8,Trn_RMSE9,Trn_RMSE10)

order=c(1,2,7,8,9,10)
plot(main=" RMSE VS MODEL COMPLEXITY",range(1:10), range(11:60), xlab = "Order",  ylab = "RMSE")
#plot(order,Test_RMSE)
lines(order,Test_RMSE, col='red', type='l')
lines(order,Train_RMSE, col='green', type='l')

legend("topleft", legend = c("Test", "Train"), col = c("red","green"), pch = 19, cex = 0.75)

#############################################################################################


