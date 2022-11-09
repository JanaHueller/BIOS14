rm(list = ls())
birds = `bird_allometry(1)`
str(birds) # structure of the data set 
head(birds)
summary(birds)#explore the data
birds$Sex = as.factor(birds$Sex)
names(birds)
hist(birds$brain_mass,
     xlab= "Brain mass (g)",
     ylab= "Frequency")
plot(log(birds$body_mass), log(birds$brain_mass),
     xlab= "log Body mass",
     ylab= "log Brain mass",
     main= "Figure 1: Relationship between Bodymass and Brainmass")
# model (m) = lm(y~x)
m = lm(log(birds$brain_mass) ~ log(birds$body_mass))
summary(m)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.40056 -0.25309 -0.05214  0.23046  1.94210 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          -1.961020   0.033879  -57.88   <2e-16 ***
#  log(birds$body_mass)  0.565911   0.007272   77.82   <2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
abline(lm(log(birds$brain_mass) ~ log(birds$body_mass)))
