sp<-c(99,22,12,55,67,88,76,89)
di<-c(87,4,2,20,44,54,55,67)

plot(sp,di)
l1<-lm(sp~di)
abline(l1)
dn1<-data.frame(di=c(100,77,55))
r<-predict(l1,dn1)
print(r)

plot(sp,di)
l2<-lm(di~sp)
abline(l2)
dn2<-data.frame(sp=c(122,98,74))
rr<-predict(l2,dn2)

print(rr)

plot(r,rr)

a = datasets::cars
y = a$speed
x = a$dist

plot(x,y)
l2<-lm(y~x)
abline(l2)
dn2<-data.frame(y=c(10,33,55))
rr<-predict(l2,dn2)
print(rr)


l1<-lm(x~y)
abline(l1)
dn1<-data.frame(x=c(370,100,200))
r<-predict(l1,dn1)
print(r)

sp=c(11,22,33,44,55,77,88,99)
di=c(10,20,30,40,50,75,85,97)
ti=c(8,14,12,5,68,95,67,76)
l1<-lm(sp~di+ti)
dfn1<-data.frame(di=c(10,20,30),ti=c(3,2,5))
predict(l1,dfn1)

l2<-lm(di+ti~sp)
dfn2<-data.frame(di=c(10,20,30),sp=c(12,20,30))
predict(l1,dfn2)


l3<-lm(ti~di+sp)
dfn3<-data.frame(di=c(10,20,30),sp=c(13,12,15))
predict(l3,dfn3)


l4<-lm(di+sp~ti)
dfn4<-data.frame(sp=c(10,20,30),ti=c(4,1,5))
predict(l4,dfn4)
