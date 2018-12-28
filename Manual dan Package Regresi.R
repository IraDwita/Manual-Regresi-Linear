data=read.csv("F://SARJANA BERKARYA//KARYA VALEN//MILD Intro to Stats//Data.csv", header=T)

##Manual##

#Y
Y=sum(data$Y)

#X1
X1=sum(data$X1)

#X2
X2=sum(data$X2)

n=dim(data)[1]
#X1Y
X1Y=numeric(n)
  for(i in 1:n){
    X1Y[i]=data$X1[i]*data$Y[i]}
X1Y=sum(X1Y)

#X2Y
X2Y=numeric(n)
  for(i in 1:n){
    X2Y[i]=data$X2[i]*data$Y[i]}
X2Y=sum(X2Y)

#X1X2
X1X2=numeric(n)
  for(i in 1:n){
    X1X2[i]=data$X1[i]*data$X2[i]}
X1X2=sum(X1X2)

#X1^2
X12=numeric(n)
for(i in 1:n){
  X12[i]=data$X1[i]^2}
X12=sum(X12)

#X2^2
X22=numeric(n)
for(i in 1:n){
  X22[i]=data$X2[i]^2}
X22=sum(X22)

#A
A=matrix(c(n,X1,X2,X1,X12,X1X2,X2,X1X2,X22), nrow=3, byrow = T)
#A1
A1=matrix(c(Y,X1,X2,X1Y,X12,X1X2,X2Y,X1X2,X22), nrow=3, byrow=T)
#A2
A2=matrix(c(n,Y,X2,X1,X1Y,X1X2,X2,X2Y,X22), nrow=3, byrow=T)
#A3
A3=matrix(c(n,X1,Y,X1,X12,X1Y,X2,X1X2,X2Y), nrow=3, byrow=T)

b0=det(A1)/det(A)
b1=det(A2)/det(A)
b2=det(A3)/det(A)

#Koefisien Determinasi
#Y2
Y2=numeric(n)
for(i in 1:n){
  Y2[i]=data$Y[i]^2}
Y2=sum(Y2)

#Yhat
Yhat=numeric(n)
for(i in 1:n){
  Yhat[i]=-0.1865025+(0.8454172*data$X1[i])+(0.03237574*data$X2[i])}

#Yhat2
Yhat2=numeric(n)
for(i in 1:n){
  Yhat2[i]=Yhat[i]^2}
Yhat2=sum(Yhat2)

#with package
regresi=lm(Y~X1+X2, data=data)
summary(regresi)