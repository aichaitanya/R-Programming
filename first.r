#--------------------INPUT & Basic Operation---------------------

a = as.integer(readline(prompt = '1st :'))
b = as.integer(readline(prompt = '2nd :'))
d = as.integer(readline(prompt = '3rd :'))
f = as.integer(readline(prompt = '3rd :'))

e = a+b-f
g=a%%b
h=a*b
i=a/b
j=a^b
print(e)
print(g)
print(h)
print(i)
print(j)

#--------------------------Even Odd-------------
  
if((a%%2)==0){
  print('Even')
}else{
  print('Odd')
}

#------------------Max Of 3-------------

if(a>b){
  if(a>d){
    maax=a
  }else{
    if(b>d){
      maax=b
    }else{
      maax=d
    }
  }
}else{
  if(b>d){
    maax=b
  }else{
    maax=d
  }
}

#------------------Factorial-------------


i=1
fact=1
while(i<=10)
  {
    fact=fact*i
    print(fact)
    i=i+1
}
print(paste('Fact :',fact))

#------------------Print Prime Number-------------

i=1
nn=1
while(nn<101){
  count=0
  i=2
  while(i<=nn/2){
    if(nn%%i==0){
      count=count+1
      break
    }
    i=i+1
  }
  if(count==0 && nn!=1){
    print(nn)
  }
  nn=nn+1
}

#------------For Loop---------------

for(i in 1:10){
  print(i)
}

#---------------Prime Number using Vector-------------

n=100
if (n >= 2) {
  x = seq(2, n)
  prime_nums = c()
  for (i in seq(2, n)) {
    if (any(x == i)) {
      prime_nums = c(prime_nums, i)
      x = c(x[(x %% i) != 0], i)
    }
  }
  print(prime_nums)
}else 
{
  stop("Input number should be at least 2.")
}

#--------------Prime Using Function----------

prime_numbers <- function(n) {
  if (n >= 2) {
    x = seq(2, n)
    prime_nums = c()
    for (i in seq(2, n)) {
      if (any(x == i)) {
        prime_nums = c(prime_nums, i)
        x = c(x[(x %% i) != 0], i)
      }
    }
    return(prime_nums)
  }
  else 
  {
    stop("Input number should be at least 2.")
  }
} 
prime_numbers(100)

#----------Prime Using For------------

prime = 1:100
temp = 0
for(val in prime){
  if (val == 0){
    next
  } else if (val == 1){
    next
  } else if (val == 2){
    TRUE
    temp = val
  } else if (val %% temp == 0){
    temp = temp + 1
    next
  } else if (val %% temp == 1){
    TRUE
  } 
}

#--------Vector & Operations-------------

v1 = c(1,2,3)
v2 = c(1,2,3)

print(v1+v2)
print(v1-v2)
print(v1*v2)
print(v1%%v2)
print(v1/v2)
print(v1^v2)

#--------------Matrix & 2D Array------------


m = matrix(c(1:9), nrow=3 ,byrow=TRUE) #FALSE fills columns first
print(m)

#---------------Data Read From External Source------------


d = read.csv(file = 'C:/Chaitanya/AI & ML/Programs/ZIP/Signature-recognition-master/signatureResults.csv')
print(d)

#--------Graph Plot------------

y=c(1,423,3333,2345,966,434,314,342,4325,4,4547,4343,2235,3132,2)
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'l')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'o')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'p')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'h')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'b')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'c')
plot(y,xlab='x-axies',ylab='y-axies',main='diagram',col='red',type = 'S')

#--------------------------Bar Plot----------------------------
barplot(y,col = rainbow(10),main='SHARE CHART',width = 5,axes = TRUE,space = 3,height=y,
        xlab='Profit',ylab = 'Year',names.arg = y, sub='Figure Share')


#-------------------------Pie Chart----------------------------------
pie(y,labels=c(1:10),edges=200,radius=1,clockwise = FALSE, density=2000,angle=100,col=rainbow(10),main='Pie Chart',border = NULL,init.angle = 76)


#---------------Data Set Access & Ploting -----------------------------------
y = datasets::cars
yy=y['dist']
pie(yy,labels=c(1:50),edges=200,radius=1, density=2000,angle=100,col=rainbow(10),main='Pie Chart',border = NULL,init.angle = 76)
yy = cars$dist
y = cars$speed
b = barplot(yy,col = rainbow(10),main='SHARE CHART',width = 5,axes = TRUE,space = 3,height=yy,xlab='Profit',ylab = 'Year', sub='Figure Share')

c = barplot(y,col = rainbow(10),main='SHARE CHART',width = 5,axes = TRUE,space = 3,height=yy,xlab='Profit',ylab = 'Year', sub='Figure Share')

plot(y,yy,type='o')

#---------------------Scatter----------------------
scatter.smooth(y,yy,col=rainbow(10),evaluation = 10,family = c("symmetric", "gaussian"),span = 2/3,)
loess.smooth(yy, y, span = 2/3, degree = 1,evaluation = 50)

#---------------------Histogram----------------------

x=hist(y,
     include.lowest = TRUE, right = TRUE,
     density = NULL, angle = 45, border = NULL,
     axes = TRUE,col=rainbow(10))
y = hist(yy,
             include.lowest = TRUE, right = TRUE,
             density = NULL, angle = 45, border = NULL,
             axes = TRUE,col=rainbow(10))



#-----------------Count------------------
y = cars$dist
count=0
for(i in 1:length(y)){
  if((y[i])>20 && y[i]<=40){
    count=count+1
  }
}
count

#-----------SVM--------------------------

library(MASS)
library(e1071)
cats
s<-sample(144,100)
s
cn<-c('Sex','Bwt','Hwt')
cn
trai_set<-cats[s,cn]
trai_set

sm<-svm(Sex~Bwt+Hwt,data=trai_set)
plot(sm,trai_set)

#-----------SVM & Prediction--------------

library(MASS)
library(e1071)
cats
s<-sample(198,100)
s
cn<-c('Sex','Bwt','Hwt')
cn
trai_set<-cats[s,cn]
trai_set

sm<-svm(Sex~Bwt+Hwt,data=trai_set,cost=100)
#plot(sm,trai_set)
df1<-data.frame(Bwt=c(2,4,11,1,3),Hwt=c(4,5,6,7,5))
r = predict(sm,df1)
plot(sm,trai_set)

#------------------IRIS & Prediction-----------------------

library(MASS)
library(e1071)
iris
s<-sample(198,100)
s
cn<-c('Petal.Length','Petal.Width','Species')
cn
trai_set<-iris[s,cn]
trai_set

sm<-svm(Species~Petal.Length+Petal.Width,data=trai_set,cost=100)

df1<-data.frame(Petal.Length=c(5,6,3),Petal.Width=c(5,6,3.3))
r = predict(sm,df1)
plot(sm,trai_set)

#----------------SVM Over Different Data---------------

d = read.csv(file = 'C:\Chaitanya\Downloads\cpdata.csv')
s<-sample(80,30)
s
cn<-c('face_scale','width','animal')
cn
trai_set<-d[s,cn]
trai_set

sm<-svm(animal~face_scale+width,data=trai_set,cost=100)

df1<-data.frame(face_scale=c(2,6,3),width=c(10,30,20))
r = (predict(sm,df1))
plot(sm,trai_set)
r

#---------------------DECISION TREE-----------------------------------

library(rpart)
library(rpart.plot)

s = sample(150,100)
data = iris[s,]

model = rpart(Species~.,data = data)
rpart.plot(model)

d = data.frame(Sepal.Length=c(2,3,4),Sepal.Width=c(4,5,6),Petal.Length=c(1,6,3),Petal.Width=c(5,6,3.3))
predict(model,d)

#--------------------SVM, DECISION TREE, PREDICTION, GRAPH---------------------------------

library(MASS)
library(e1071)

d = read.csv(file = 'C:/Chaitanya/R/train.csv')
s<-sample(891,891)
s
cn<-c('Survived','Pclass','Age')
cn
trai_set<-d[s,cn]
trai_set

test_set = read.csv(file = 'C:/Chaitanya/R/test.csv')
sm<-svm(Survived~Pclass+Age,data=trai_set,cost=100)

df1<-data.frame(Pclass=test_set$Pclass,Age=test_set$Age)
plot(sm,trai_set)

r = predict(sm,df1)
plot(sm,trai_set)
r

#-----------------RANDOM FOREST & PREDICTION--------------------------

library(randomForest)
library(tree)

s = sample(150,150)
data = iris[s,]
model = randomForest(Species~.,data=data,cost=100)
d = data.frame('Sepal.Length'=c(4.3,5.6),'Sepal.Width'=c(3.4,2.5),'Petal.Length'=c(3.5,1.2),'Petal.Width'=c(2.2,1.3))
predict(model,d)

#---------------------CLUSTER & K means-------------------------------------

library(dplyr)
library(cluster)
library(tidyverse)
library(factoextra)
df<-USArrests
df
k1 = kmeans(df,centers = 5)
f1 = fviz_cluster(k1,data=df,geom = "point")


library(dplyr)
library(cluster)
library(tidyverse)
library(factoextra)
library(gridExtra)
df<-data.frame(USArrests$Murder,USArrests$Assault)
df
k1 = kmeans(df,centers = 5)
f2 = fviz_cluster(k1,data=df,geom = "point")


library(dplyr)
library(cluster)
library(tidyverse)
library(factoextra)
df<-USArrests
df
k1 = kmeans(df,centers = 3)
f3 = fviz_cluster(k1,data=df,geom = "point")


library(dplyr)
library(cluster)
library(tidyverse)
library(factoextra)
library(gridExtra)
df<-data.frame(USArrests$Murder,USArrests$UrbanPop)
df
k1 = kmeans(df,centers = 10)
f4 = fviz_cluster(k1,data=df,geom = "point")

grid.arrange(f1,f2,f3,f4,nrow=2)

#---------------------Logistic Regression----------------------

cgp = c(7,6,9,2,3,4,2,8,5,9,8,4,8,4,7,6,5,9,9,5)
sco = c(1,2,3,4,5,6,7,9,5,6,8,9,7,5,5,5,5,5,1,9)
sel = c(0,0,0,0,0,0,0,1,1,1,1,0,1,0,1,1,1,1,0,1)

g = glm(sel~cgp+sco)
d = data.frame(cgp=c(2,6,4,8,9),sco=c(6,7,9,9,7))
p = round(predict(g,d))
p

#------------------------------------

m = glm(cgp~sco+sel)
md = data.frame(sco=c(4,5,6,7,8),sel=c(0,1,1,1,1))
mp = round(predict(m,md))
mp

#--------------------------------

s = glm(sco~cgp+sel)
sd = data.frame(cgp=c(4,5,6,7,8),sel=c(0,1,1,0,1))
sp = round(predict(s,sd))
sp


#------------------------

s = glm(sel~cgp+sco)
sd = data.frame(cgp=cgp,sco=sco)
sp = round(predict(s,sd))
sp
#---------------LOGISTIC REGRESSION ON EXTERNAL DATA---------------

d = read.csv(file = 'C:/Users/Patil/OneDrive/Documents/survive.csv')
print(d)

sur = d$Survive
a = d$Age
g = d$Gender

s = glm(sur~a+g)
sd = data.frame(a=c(33,55,1,99),g=c('m','f','m','m'))
sp = round(predict(s,sd))
sp

#--------------------------K NEIREST NEIGHBOUR----------------------

library(class)

sam = sample(150,100)
train = iris[sam,-5]
target = iris[sam,5]
test = iris[-sam,-5]

model = knn(train,test,target)
model

#----------------------NAIVE BAYES----------------------------

library(naivebayes)

data1 = read.csv(file = 'C:/Chaitanya/R/Data/binary1.csv',header = TRUE)
str(data1)
data1$admit<-as.factor(data1$admit)
data1$rank<-as.factor(data1$rank)
str(data1)
s1<-sample(2,nrow(data1),replace = T,prob = c(0.9,0.1))
s1
train<-data1[s1==1,]
train
test<-data1[s1==2,]
test
mod<-naive_bayes(admit~.,data = train)
predict(mod,test)
predict(mod,test,type='prob')
plot(mod)






















