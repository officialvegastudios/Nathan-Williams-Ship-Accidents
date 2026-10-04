> ship <- read.csv(file.choose())
> 
> names(ship)
[1] "accidents"      "operational"    "construction1"  "construction2" 
[5] "construction3"  "exposure"       "service_months"
> str(ship)
'data.frame':   40 obs. of  7 variables:
 $ accidents     : int  0 0 3 4 6 18 0 11 39 29 ...
 $ operational   : int  0 1 0 1 0 1 0 1 0 1 ...
 $ construction1 : int  1 1 0 0 0 0 0 0 1 1 ...
 $ construction2 : int  0 0 1 1 0 0 0 0 0 0 ...
 $ construction3 : int  0 0 0 0 1 1 0 0 0 0 ...
 $ exposure      : num  4.84 4.14 7 7 7.32 ...
 $ service_months: int  127 63 1095 1095 1512 3353 0 2244 44882 17176 ...
> colSums(is.na(ship))
     accidents    operational  construction1  construction2  construction3 
             0              0              0              0              0 
      exposure service_months 
             6              0 
> 
> ship2 <- ship[!is.na(ship$exposure), ]
> 
> nrow(ship2)
[1] 34
> 
> ship2$any_accident <- ifelse(ship2$accidents > 0, 1, 0)
> 
> table(ship2$any_accident)

 0  1 
 8 26 
> prop.table(table(ship2$any_accident))

        0         1 
0.2352941 0.7647059 
> 
> summary(ship2$exposure)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.807   5.911   6.999   7.049   7.707  10.712 
> sd(ship2$exposure)
[1] 1.721094
> 
> ship2$construction_era <- ifelse(
+   ship2$construction1 == 1, "Era 1",
+   ifelse(
+     ship2$construction2 == 1, "Era 2",
+     ifelse(
+       ship2$construction3 == 1, "Era 3",
+       "Reference Era"
+     )
+   )
+ )
> 
> table(ship2$construction_era)

        Era 1         Era 2         Era 3 Reference Era 
            9            10            10             5 
> 
> baseline <- lm(
+   accidents ~ exposure + construction1 + construction2 + construction3,
+   data = ship2
+ )
> 
> summary(baseline)

Call:
lm(formula = accidents ~ exposure + construction1 + construction2 + 
    construction3, data = ship2)

Residuals:
    Min      1Q  Median      3Q     Max 
-12.847  -6.259  -1.069   3.687  19.436 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -50.7824     7.6755  -6.616 2.98e-07 ***
exposure        7.9985     0.9162   8.730 1.31e-09 ***
construction1   8.0608     4.9188   1.639    0.112    
construction2   7.2704     4.7615   1.527    0.138    
construction3   2.0311     4.7708   0.426    0.673    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 8.691 on 29 degrees of freedom
Multiple R-squared:  0.7319,    Adjusted R-squared:  0.6949 
F-statistic: 19.79 on 4 and 29 DF,  p-value: 5.957e-08

> 
> plot(baseline)
Waiting to confirm page change...
Waiting to confirm page change...
Waiting to confirm page change...
Waiting to confirm page change...
> 
> library(lmtest)
> 
> bptest(baseline)

        studentized Breusch-Pagan test

data:  baseline
BP = 9.2755, df = 4, p-value = 0.05457

> 
> shapiro.test(residuals(baseline))

        Shapiro-Wilk normality test

data:  residuals(baseline)
W = 0.94244, p-value = 0.0729

> 
> log_model <- lm(
+   log(accidents + 1) ~ exposure + construction1 + construction2 + construction3,
+   data = ship2
+ )
> 
> summary(log_model)

Call:
lm(formula = log(accidents + 1) ~ exposure + construction1 + 
    construction2 + construction3, data = ship2)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.29645 -0.35636  0.06048  0.34763  1.17028 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)   -3.13349    0.50205  -6.241 8.23e-07 ***
exposure       0.66700    0.05993  11.130 5.51e-12 ***
construction1 -0.13586    0.32174  -0.422    0.676    
construction2 -0.01266    0.31145  -0.041    0.968    
construction3  0.30875    0.31205   0.989    0.331    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.5685 on 29 degrees of freedom
Multiple R-squared:  0.8362,    Adjusted R-squared:  0.8137 
F-statistic: 37.02 on 4 and 29 DF,  p-value: 5.296e-11

> 
> plot(log_model)
Waiting to confirm page change...
Waiting to confirm page change...
Waiting to confirm page change...
Waiting to confirm page change...
> 
> bptest(log_model)

        studentized Breusch-Pagan test

data:  log_model
BP = 4.984, df = 4, p-value = 0.2889

> 
> shapiro.test(residuals(log_model))
