########## ЗАДАНИЕ 1

# 1
exp.1<-c(rexp(n=200))
mean(exp.1)
sd(exp.1)

# 2
exp.0.1<-c(rexp(n=200,rate=0.1))
mean(exp.0.1)
sd(exp.0.1)

exp.0.5<-c(rexp(n=200,rate=0.5))
mean(exp.0.5)
sd(exp.0.5)

exp.5<-c(rexp(n=200,rate=5))
mean(exp.5)
sd(exp.5)

exp.10<-c(rexp(n=200,rate=10))
mean(exp.10)
sd(exp.10)

# 3
# 4
hist(exp.1)

# 5
plot(exp.1)

# 6
plot(exp.0.1,exp.0.5)

# 7
exp.means<-c(mean(exp.1),mean(exp.0.1),mean(exp.0.5),mean(exp.5),mean(exp.10))

# 8
exp.param<-c(1,0.1,0.5,5,10)
plot(exp.means,exp.param)

# 9
exp.sds<-c(sd(exp.1),sd(exp.0.1),sd(exp.0.5),sd(exp.5),sd(exp.10))
plot(exp.sds,exp.param)

# 10
plot(exp.means,exp.sds)


########## ЗАДАНИЕ 2

# 1
huge.exp.1<-c(rexp(n=1100000))
mean(huge.exp.1)
sd(huge.exp.1)

# 2
hist(huge.exp.1)

# 3
mean(huge.exp.1[huge.exp.1>1])

# 4
huge.exp.1.mat<-matrix(huge.exp.1, nrow = 1100, ncol = 1000)
hist(huge.exp.1.mat)

# 5
mean(huge.exp.1.mat[,137])

# 6
huge.exp.1.mat.means<-colMeans(huge.exp.1.mat)
barplot(huge.exp.1.mat.means)

# 7
huge.exp.1.sq<-(huge.exp.1 ^ 2)
huge.exp.1.sq.mean<-mean(huge.exp.1.sq)
huge.exp.1.sq.sd<-sd(huge.exp.1.sq)

print(huge.exp.1.sq.mean)
print(huge.exp.1.sq.sd)
