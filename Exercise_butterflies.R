rm(list = ls())
dat = butterflies
names(dat)
## [1] "LarvalID"        "LarvalHost"      "Sex"            
## [4] "MaternalHost"    "MotherID"        "DevelopmentTime"
## [7] "AdultWeight"     "GrowthRate"   
dat$MaternalHost = paste0(dat$MaternalHost, "M")
dat$LarvalHost = paste0(dat$LarvalHost, "L")
means = tapply(dat$DevelopmentTime, list(dat$MaternalHost, dat$LarvalHost), mean)
ses = tapply(dat$DevelopmentTime, 
             list(dat$MaternalHost, dat$LarvalHost), 
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means ## calculating the mean first
##             BarbareaLL BerteroaLL
## BarbareaMM   21.69608   27.00000
## BerteroaMM   23.51282   31.01923

ses ## calculating for standard errors 
##              BarbareaLL BerteroaLL
## BarbareaMM  0.1236766  0.3167597
## BerteroaMM  0.2419146  0.2641049

plot(c(0.97, 1.03), means[1,], ylim=c(18, 40), xlim=c(0.8, 2.2),
     xlab="Larval host", 
     ylab="Developmental time (days)",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Barbarea", "Berteroa"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03), 
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03), 
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Maternal host", "Barbarea", "Berteroa"), 
       bty="n", pch=c(NA,21,16))

m = lm(DevelopmentTime~MaternalHost*LarvalHost, data=dat)
anova(m)

## Analysis of Variance Table

## Response: DevelopmentTime
## Df  Sum Sq Mean Sq F value    Pr(>F)    
## MaternalHost              1  623.61  623.61  177.90 < 2.2e-16 ***
##   LarvalHost                1 2682.41 2682.41  765.21 < 2.2e-16 ***
##   MaternalHost:LarvalHost   1   80.80   80.80   23.05 2.561e-06 ***
##   Residuals               283  992.05    3.51                      
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(m)
## Call:
##   lm(formula = DevelopmentTime ~ MaternalHost * LarvalHost, data = dat)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -5.0192 -1.5128 -0.5128  1.0000  7.0000 

## Coefficients:
##   Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                                  21.6961     0.1854 117.033  < 2e-16 ***
##   MaternalHostBerteroaMM                        1.8167     0.2816   6.451 4.80e-10 ***
##   LarvalHostBerteroaLL                          5.3039     0.3132  16.934  < 2e-16 ***
##   MaternalHostBerteroaMM:LarvalHostBerteroaLL   2.2025     0.4588   4.801 2.56e-06 ***
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.872 on 283 degrees of freedom
## Multiple R-squared:  0.7734,	Adjusted R-squared:  0.771 
## F-statistic: 322.1 on 3 and 283 DF,  p-value: < 2.2e-16



