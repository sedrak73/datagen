# generate uncorrelated multi-distribution test data
setwd('C:\\path\\to\\working\\directory')
n<-1000 # number of rows
filename<-"uncorrelated_distributions_test.csv"

binomial<-rbinom(n, size=20, prob=0.75)
neg_binomial<-rnbinom(n, size=10, prob=0.5)
poisson<-rpois(n, lambda=10)
beta<-rbeta(n, shape1=2, shape2=2) 
gamma<-rgamma(n, shape=7.5, rate=1) # scale = 1/rate
lognormal<-rlnorm(n, meanlog=0, sdlog=0.5)
weibull<-rweibull(n, shape=2, scale=4) 
normal<-rnorm(n, mean=10, sd=10) 
normal_discrete<-round(rnorm(n, mean=5, sd=1), 0) 
uniform<-runif(n, min=-10, max=10) 
categorical<-sample(c("CatA", "CatB", "CatC"), n, replace=TRUE, prob=c(0.2, 0.3, 0.5))
test_data<-data.frame(binomial, neg_binomial, poisson, beta, gamma, lognormal,
                      weibull, normal, normal_discrete, uniform, categorical)
write.csv(test_data,filename,row.names=FALSE)
