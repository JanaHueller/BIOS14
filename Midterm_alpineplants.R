rm(list = ls())
data = `alpineplants(1)`
# Exploring the data #
names(data)
## [1] "Carex.bigelowii"    "Thalictrum.alpinum"
## [3] "mean_T_winter"      "max_T_winter"      
## [5] "min_T_winter"       "mean_T_summer"     
## [7] "max_T_summer"       "min_T_summer"      
## [9] "light"              "snow"              
## [11]"soil_moist"         "altitude" 

# Model selection for Thalictrum alpinum #
m1 = lm(data$Thalictrum.alpinum ~ data$mean_T_summer * data$altitude * data$light)
m2 = lm(data$Thalictrum.alpinum ~ data$mean_T_summer + data$altitude + data$light)
m3 = lm(data$Thalictrum.alpinum ~ data$mean_T_summer)
m4 = lm(data$Thalictrum.alpinum ~ data$altitude)
m5 = lm(data$Thalictrum.alpinum ~ data$light)
mlist = list(m1, m2, m3, m4, m5)
AICTab = AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing = F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
##     df      AIC    logLik delta    w
## m1  9 399.2180 -190.6090  0.00 0.59
## m2  5 400.4919 -195.2459  1.27 0.31 --> as this model perfoms well
## m3  3 403.1341 -198.5671  3.92 0.08
## m5  3 407.9381 -200.9690  8.72 0.01
## m4  3 415.6922 -204.8461 16.47 0.00

# Model selection for Carex bigelowii # 

m1 = lm(data$Carex.bigelowii ~ data$mean_T_summer * data$altitude * data$light)
m2 = lm(data$Carex.bigelowii ~ data$mean_T_summer + data$altitude + data$light)
m3 = lm(data$Carex.bigelowii ~ data$mean_T_summer)
m4 = lm(data$Carex.bigelowii ~ data$altitude)
m5 = lm(data$Carex.bigelowii ~ data$light)
mlist = list(m1, m2, m3, m4, m5)
AICTab = AIC(m1, m2, m3, m4, m5)
AICTab$logLik = unlist(lapply(mlist, logLik))
AICTab = AICTab[order(AICTab$AIC, decreasing = F),]
AICTab$delta = round(AICTab$AIC - min(AICTab$AIC), 2)
lh = exp(-0.5*AICTab$delta)
AICTab$w = round(lh/sum(lh), 2)
AICTab
##     df      AIC    logLik delta    w
## m1  9 367.4328 -174.7164  0.00 0.62
## m2  5 368.4485 -179.2242  1.02 0.37 --> perfoms well with less complexity
## m4  3 378.6339 -186.3169 11.20 0.00
## m3  3 383.2523 -188.6261 15.82 0.00
## m5  3 386.3780 -190.1890 18.95 0.00

## Z transformation ## 
Temp_z = (data$mean_T_summer - mean(data$mean_T_summer, na.rm = T))/sd(data$mean_T_summer, na.rm = T)
Alt_z = (data$altitude - mean(data$altitude))/sd(data$altitude)
Light_z = (data$light - mean(data$light))/sd(data$light)
# x1_z = (x1 - mean(x1))/sd(x1)
#x2_z = (x2 - mean(x2))/sd(x2)
#m = lm(y ~ x1_z + x2_z)
#summary(m)


## Calculating with selected model ##

mC = lm(data$Carex.bigelowii ~ data$mean_T_summer + data$altitude + data$light)
coefs = summary(mC)$coef
summary(mC)
## Call:
##  lm(formula = data$Carex.bigelowii ~ data$mean_T_summer + data$altitude + 
##       data$light)

## Residuals:
##   Min      1Q   Median      3Q     Max 
## -2.4143 -1.1229 -0.5248  0.8781  5.2346 

## Coefficients:
##                      Estimate Std. Error t value
## (Intercept)        -68.066514  16.039281  -4.244
## data$mean_T_summer   1.228895   0.401973   3.057
## data$altitude        0.048137   0.010787   4.462
## data$light          -0.002100   0.008936  -0.235
##                      Pr(>|t|)    
## (Intercept)        5.28e-05 ***
##  data$mean_T_summer  0.00293 ** 
##  data$altitude      2.31e-05 ***
##  data$light          0.81477    
## ---
##   Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.631 on 91 degrees of freedom
## (1 Beobachtung als fehlend gelöscht)
## Multiple R-squared:  0.1815,	Adjusted R-squared:  0.1545 
## F-statistic: 6.726 on 3 and 91 DF,  p-value: 0.0003773

mC_z = lm(data$Carex.bigelowii ~ Temp_z + Alt_z + Light_z)
coefs = summary(mC_z)$coef
summary(mC_z)
## Call:
##   lm(formula = data$Carex.bigelowii ~ Temp_z + Alt_z + Light_z)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.4143 -1.1229 -0.5248  0.8781  5.2346

##Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
##   (Intercept)  1.38114    0.16740   8.251 1.16e-12 ***
##   Temp_z       0.99572    0.32570   3.057  0.00293 ** 
##   Alt_z        1.29389    0.28995   4.462 2.31e-05 ***
##   Light_z     -0.05185    0.22068  -0.235  0.81477    
## ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.631 on 91 degrees of freedom
## (1 Beobachtung als fehlend gelöscht)
## Multiple R-squared:  0.1815,	Adjusted R-squared:  0.1545 
## F-statistic: 6.726 on 3 and 91 DF,  p-value: 0.0003773

mT = lm(data$Thalictrum.alpinum ~ data$mean_T_summer + data$altitude + data$light)
coefs = summary(mT)$coef
summary(mT)
## Call:
##   lm(formula = data$Thalictrum.alpinum ~ data$mean_T_summer + data$altitude + 
##        data$light)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.7666 -1.1004 -0.4558  0.2678  7.5506 

## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)
## (Intercept)        -46.88543   18.98578  -2.470  0.01540
## data$mean_T_summer   1.36097    0.47582   2.860  0.00525
## data$altitude        0.02823    0.01277   2.211  0.02956
## data$light           0.01293    0.01058   1.222  0.22473

## (Intercept)        * 
##   data$mean_T_summer **
##   data$altitude      * 
##   data$light           
## ---
##   Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.931 on 91 degrees of freedom
## (1 Beobachtung als fehlend gelöscht)
## Multiple R-squared:  0.1706,	Adjusted R-squared:  0.1433 
## F-statistic:  6.24 on 3 and 91 DF,  p-value: 0.0006689

mT_z = lm(data$Thalictrum.alpinum ~ Temp_z + Alt_z + Light_z)
coefs = summary(mT_z)$coef
summary(mT_z)

# Call:
#   lm(formula = data$Thalictrum.alpinum ~ Temp_z + Alt_z + Light_z)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -2.7666 -1.1004 -0.4558  0.2678  7.5506 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.2430     0.1982   6.273 1.17e-08 ***
#   Temp_z        1.1027     0.3855   2.860  0.00525 ** 
#   Alt_z         0.7587     0.3432   2.211  0.02956 *  
#   Light_z       0.3193     0.2612   1.222  0.22473    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.931 on 91 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.1706,	Adjusted R-squared:  0.1433 
# F-statistic:  6.24 on 3 and 91 DF,  p-value: 0.0006689

## Plotting the data ## 


plot(data$mean_T_summer, data$Carex.bigelowii, col='1',
     las=1,
     main= "Impact of summer temperature on species distribution",
     ylab="Plant occurrence",
     xlab="Mean summer temperature [C°]",
     cex.main=1.7,
     cex.lab= 1.5)
points(data$mean_T_summer, data$Thalictrum.alpinum, pch=16, col='orangered4')
legend("topleft", col=c(1,'orangered4'), pch=21:16, cex=1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))

plot(data$altitude, data$Carex.bigelowii, col='1',
     las=1,
     main= "Impact of altitude on species distribution",
     ylab= "Plant occurrence ",
     xlab= "Altitude [m]",
     cex.main=1.7,
     cex.lab= 1.5)
points(data$altitude, data$Thalictrum.alpinum, col='orangered4')
legend("topleft", col=c(1,'orangered4'), pch=21, cex=1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))

plot(data$light, data$Carex.bigelowii, col='1',
     las=1,
     main= "Impact of Light on species distribution",
     ylab= "Plant occurrence",
     xlab= "Light",
     cex.main=1.7,
     cex.lab= 1.5)
points(data$light, data$Thalictrum.alpinum, col='orangered4')
legend("topright", col=c(1,'orangered4'), pch=21, cex=1.4,
       legend=c(expression(paste(italic("Carex bigelowii"))),
                expression(paste(italic("Thalictrum alpinum")))))
                

plot(data$mean_T_summer, data$altitude,
     las=1,
     main= "Correlation of Temperature and Altitude",
     ylab= "Altitude [m]",
     xlab= "Mean Summer Temperature [°C]",
     cex.lab= 1.5,
     cex.axis = 1,
     cex.main=1.7 ,
     cex= 1.5)
abline(lm(data$altitude ~ data$mean_T_summer), col='red', lwd=1.5)

### Estimates between the predictors ###
mAT=lm(Alt_z~Temp_z)
summary(mAT)
# Call:
#   lm(formula = Alt_z ~ Temp_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.15504 -0.41102  0.00332  0.35438  1.61652 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.01340    0.05988  -0.224    0.823    
# Temp_z      -0.81005    0.06020 -13.456   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.5837 on 93 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.6607,	Adjusted R-squared:  0.657 
# F-statistic: 181.1 on 1 and 93 DF,  p-value: < 2.2e-16

mAL=lm(Alt_z~Light_z)
summary(mAL)
# Call:
#   lm(formula = Alt_z ~ Light_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.17697 -0.82564 -0.02557  0.74344  1.41739 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -2.157e-15  8.793e-02   0.000        1    
# Light_z     -5.154e-01  8.839e-02  -5.831 7.73e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.8615 on 94 degrees of freedom
# Multiple R-squared:  0.2656,	Adjusted R-squared:  0.2578 
# F-statistic:    34 on 1 and 94 DF,  p-value: 7.732e-08

mTL=lm(Temp_z~Light_z)
summary(mTL)
# Call:
#   lm(formula = Temp_z ~ Light_z)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.31961 -0.45698 -0.05418  0.56090  1.79148 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.008038   0.078857  -0.102    0.919    
# Light_z      0.646103   0.079444   8.133 1.79e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.7685 on 93 degrees of freedom
# (1 Beobachtung als fehlend gelöscht)
# Multiple R-squared:  0.4156,	Adjusted R-squared:  0.4093 
# F-statistic: 66.14 on 1 and 93 DF,  p-value: 1.788e-12


