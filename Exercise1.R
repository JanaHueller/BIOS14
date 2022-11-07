#### Exercise no. 1 ####

### Summary statistics ###
out = NULL
sdvals = seq(0, 10, 0.1)
for(i in 1:length(sdvals)){
  d = rnorm(500, 20, sd=sdvals[i])
  d = d[which(d>0)]
  out[i] = exp(mean(log(d)))
}
plot(sdvals, out, las=1,
     xlab="SD", ylab="Geometric mean")
abline(h=20)

### Simulating data from statistical distributions ###
x = rnorm(n=100, mean=5, sd=1)
mean(x)
## [1] 4.900602
sd (x)
## [1] 1.078611
hist(x, las=1, main = "" )

### Bootstrapping ###
set.seed(1)
x = rnorm(50, 10, 2)
se_x= sqrt(var(x)/length(x))
out = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out[i] = mean(sample)
}
hist(out, las=1, main="")
sd(out)
## [1] 0.2347661
se_x
## [1] 0.2351537
quantile(out, c(0.025, 0.975))
## 2.5%   97.5%
## 9.71618   10.67700
mean(x) - 1.96*se_x
## [1] 9.739995
mean(x) + 1.96*se_x
## [1] 10.6618

## Exercise: Derive a 95% confidence interval for the CV of x ##
CV = sd(x)/mean(x)
## CV = 0,163004
out2 = NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  out2[i] = sd(sample)/mean(sample)
}
hist(out2, las=1, main="")
quantile(out2, c(0.025, 0.975))
## 2.5%   97.5%
## 0.1226616   0.2013957

## Exercise: The proportional properties of the natural log ##
x= rnorm(n=100, mean=5, sd=1)
SD_log =NULL
CV=NULL
for(i in 1:1000){
  sample = sample(x, replace=TRUE)
  CV[i]= sd(sample)/mean(sample)
  SD_log[i] = sd(log(sample))
}
plot(CV, SD_log)
lines(0:1, 0:1)
### Appendix exercise ###
x=seq(1, 50, 2)
out=NULL
for(i in 1:length(x)){
  out[i] = x[i]*2
}
out
##  [1]  2  6 10 14 18 22 26 30 34 38 42 46 50 54 58 62 66
## [18] 70 74 78 82 86 90 94 98
out=NULL
for(i in 1:length(x)){
  if(i>10){
    out[i] = x[i]*2
  }
  else{
    out[i] = 0
  }
}
out
ifelse(x>25, 1, 0)
out=0
while(out<21){
  out = out + floor(runif(1, 1, 10))
  print(out)
}
### Writing functions ###

computeSE = function(data){
  n = sum(!is.na(data), na.rm=T)
  SE = sqrt(var(data, na.rm=T)/n)
  return(SE)
}
set.seed(1)
x=rnorm(200, 10, 2)
computeSE(x)
## [1] 0.1313942