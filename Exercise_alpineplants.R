rm(list = ls())
Plants = alpineplants
# start exploring the data 
names(Plants)
## [1] "Carex.bigelowii"    "Thalictrum.alpinum" "mean_T_winter"      "max_T_winter"       "min_T_winter"      
## [6] "mean_T_summer"      "max_T_summer"       "min_T_summer"       "light"              "snow"              
## [11] "soil_moist"         "altitude"   

# which variables will influence the growth: snow, light, mean_T_summer, altitude
#m = lm(y~x1+x2); mC = model for Carex.bigelowii, mT = Thalictrum alpinum
mC = lm(Plants$Carex.bigelowii ~ Plants$snow + Plants$light + Plants$mean_T_summer + Plants$altitude)
coefs = summary(mC)$coef
summary(mC)

## Call:
##   lm(formula = Plants$Carex.bigelowii ~ Plants$snow + Plants$light + 
##       Plants$mean_T_summer + Plants$altitude)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.8933 -0.9907 -0.4329  0.7442  5.5026 

## Coefficients:
##                     Estimate   Std. Error t value Pr(>|t|)   
##(Intercept)          -54.750530  19.409992  -2.821  0.00589 **
##  Plants$snow            0.011233   0.009272   1.212  0.22887   
##Plants$light          -0.001545   0.008925  -0.173  0.86297   
##Plants$mean_T_summer   1.060835   0.424262   2.500  0.01422 * 
##  Plants$altitude        0.037971   0.013644   2.783  0.00656 **
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.627 on 90 degrees of freedom
## (1 Beobachtung als fehlend gelöscht)
## Multiple R-squared:  0.1946,	Adjusted R-squared:  0.1588 
## F-statistic: 5.437 on 4 and 90 DF,  p-value: 0.0005744

mC = lm(Plants$Carex.bigelowii ~ Plants$mean_T_summer + Plants$altitude + Plants$mean_T_summer*Plants$altitude)
coefs = summary(mC)$coef
summary(mC)
## Call:
##   lm(formula = Plants$Carex.bigelowii ~ Plants$mean_T_summer + 
##        Plants$altitude)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.4194 -1.1399 -0.5314  0.8588  5.2796 

## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          -67.54234   15.80161  -4.274 4.67e-05 ***
##  Plants$mean_T_summer   1.18521    0.35456   3.343   0.0012 ** 
##  Plants$altitude        0.04804    0.01072   4.480 2.14e-05 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.623 on 92 degrees of freedom
##(1 Beobachtung als fehlend gelöscht)
##Multiple R-squared:  0.181,	Adjusted R-squared:  0.1632 
##F-statistic: 10.17 on 2 and 92 DF,  p-value: 0.0001026

## Now see if the model also works for the other plant species 
mT = lm(Plants$Thalictrum.alpinum ~ Plants$mean_T_summer + Plants$altitude)
coefs1 = summary(mT)$coef
summary(mT)
## Call:
##   lm(formula = Plants$Thalictrum.alpinum ~ Plants$mean_T_summer + 
##       Plants$altitude)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.6822 -1.2397 -0.4653  0.3758  7.2341 

## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          -50.11333   18.85166  -2.658 0.009262 ** 
##   Plants$mean_T_summer   1.62998    0.42299   3.853 0.000215 ***
##  Plants$altitude        0.02881    0.01279   2.251 0.026740 *  
##   ---
##   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.936 on 92 degrees of freedom
## (1 Beobachtung als fehlend gelöscht)
## Multiple R-squared:  0.157,	Adjusted R-squared:  0.1387 
## F-statistic: 8.567 on 2 and 92 DF,  p-value: 0.0003872
mC1 = lm(Plants$Carex.bigelowii ~ Plants$mean_T_winter + Plants$soil_moist + Plants$mean_T_summer + Plants$altitude)
coefs2 = summary(mC1)$coef
summary(mC1)
## Call:
##   lm(formula = Plants$Carex.bigelowii ~ Plants$mean_T_winter + 
##        Plants$soil_moist + Plants$mean_T_summer + Plants$altitude)

## Residuals:
##   Min      1Q  Median      3Q     Max 
## -2.5264 -0.9743 -0.4252  0.7269  5.2808 

## Coefficients:
##                       Estimate Std. Error t value
## (Intercept)          -50.62227   18.02335  -2.809
## Plants$mean_T_winter   0.44761    0.24941   1.795
## Plants$soil_moist      0.00305    0.03812   0.080
## Plants$mean_T_summer   1.06685    0.35940   2.968
## Plants$altitude        0.03518    0.01257   2.798
##                        Pr(>|t|)   
## (Intercept)           0.00613 **
##   Plants$mean_T_winter  0.07614 . 
## Plants$soil_moist     0.93640   
## Plants$mean_T_summer  0.00386 **
##   Plants$altitude       0.00631 **
##   ---
##   Signif. codes:  
##   0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 1.604 on 88 degrees of freedom
## (3 Beobachtungen als fehlend gelöscht)
## Multiple R-squared:  0.2227,	Adjusted R-squared:  0.1874 
## F-statistic: 6.303 on 4 and 88 DF,  p-value: 0.0001657

plot(Plants$Carex.bigelowii, Plants$mean_T_summer,
     las=1,
     ylab="Mean summer temperature [C°]",
     xlab="Carex bigelowii distribution")
plot(Plants$Thalictrum.alpinum, Plants$mean_T_summer,
     las=1,
     ylab= "Mean summer temperature [C°]",
     xlab="Thalictrum alpinum distribution")
plot(Plants$Carex.bigelowii, Plants$altitude,
     las=1,
     ylab= "Altitude [m]",
     xlab= "Carex bigelowii")
plot(Plants$Thalictrum.alpinum, Plants$altitude,
     las=1,
     ylab= "Altitude [m]",
     xlab= "Thalictrum aplinum")

