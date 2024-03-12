Chapter 2 Estimation
================
Li-Hsin Chien
2023-09-17

Textbook: Linear Models with R, 2nd, Julian J. Faraway

### 1. Least Square Estimation

``` r
data(gala, package="faraway")
? gala
```

    ## 在指定的程式套件和圖書館裡沒有與 'gala' 有關的說明書：
    ## 您可以用用 '??gala'

資料*gala*:

Description: There are 30 Galapagos islands (加拉巴哥群島) and 7
variables in the dataset. The relationship between the number of plant
species and several geographic variables is of interest. The original
dataset contained several missing values which have been filled for
convenience. See the galamiss dataset for the original version.

Format: The data set contains the following variables

1.  Species: the number of plant species found on the island

2.  Endemics: the number of endemic species (特有種)

3.  Area: the area of the island (km$^2$)

4.  Elevation: the highest elevation of the island (m)

5.  Nearest: the distance from the nearest island (km)

6.  Scruz: the distance from Santa Cruz island (km)

7.  Adjacent: the area of the adjacent island (square km)

``` r
head(gala)
```

    ##              Species Endemics  Area Elevation Nearest Scruz Adjacent
    ## Baltra            58       23 25.09       346     0.6   0.6     1.84
    ## Bartolome         31       21  1.24       109     0.6  26.3   572.33
    ## Caldwell           3        3  0.21       114     2.8  58.7     0.78
    ## Champion          25        9  0.10        46     1.9  47.4     0.18
    ## Coamano            2        1  0.05        77     1.9   1.9   903.82
    ## Daphne.Major      18       11  0.34       119     8.0   8.0     1.84

*lm*: fitting linear models

``` r
lmod <- lm(Species ~ Area + Elevation + Nearest + Scruz + Adjacent, data = gala)
summary(lmod)
```

    ## 
    ## Call:
    ## lm(formula = Species ~ Area + Elevation + Nearest + Scruz + Adjacent, 
    ##     data = gala)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -111.679  -34.898   -7.862   33.460  182.584 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.068221  19.154198   0.369 0.715351    
    ## Area        -0.023938   0.022422  -1.068 0.296318    
    ## Elevation    0.319465   0.053663   5.953 3.82e-06 ***
    ## Nearest      0.009144   1.054136   0.009 0.993151    
    ## Scruz       -0.240524   0.215402  -1.117 0.275208    
    ## Adjacent    -0.074805   0.017700  -4.226 0.000297 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 60.98 on 24 degrees of freedom
    ## Multiple R-squared:  0.7658, Adjusted R-squared:  0.7171 
    ## F-statistic:  15.7 on 5 and 24 DF,  p-value: 6.838e-07

![](.\ch2_lm.png)

$\hat{\beta}=(X^TX)^{-1}X^TY$

``` r
x.tmp<-cbind(1,gala[,-(1:2)])
class(x.tmp)
```

    ## [1] "data.frame"

``` r
x<-as.matrix(x.tmp)
class(x)
```

    ## [1] "matrix" "array"

``` r
y<-gala$Species
class(y)
```

    ## [1] "numeric"

``` r
b<-solve(t(x)%*%x)%*%t(x)%*%y
b
```

    ##                   [,1]
    ## 1          7.068220709
    ## Area      -0.023938338
    ## Elevation  0.319464761
    ## Nearest    0.009143961
    ## Scruz     -0.240524230
    ## Adjacent  -0.074804832

``` r
names(lmod)
```

    ##  [1] "coefficients"  "residuals"     "effects"       "rank"         
    ##  [5] "fitted.values" "assign"        "qr"            "df.residual"  
    ##  [9] "xlevels"       "call"          "terms"         "model"

``` r
lmodsum <- summary(lmod)
names(lmodsum)
```

    ##  [1] "call"          "terms"         "residuals"     "coefficients" 
    ##  [5] "aliased"       "sigma"         "df"            "r.squared"    
    ##  [9] "adj.r.squared" "fstatistic"    "cov.unscaled"

![](.\ch2_lm_names.png)

``` r
lmodsum$coefficients
```

    ##                 Estimate  Std. Error      t value     Pr(>|t|)
    ## (Intercept)  7.068220709 19.15419782  0.369016796 7.153508e-01
    ## Area        -0.023938338  0.02242235 -1.067610554 2.963180e-01
    ## Elevation    0.319464761  0.05366280  5.953187968 3.823409e-06
    ## Nearest      0.009143961  1.05413595  0.008674366 9.931506e-01
    ## Scruz       -0.240524230  0.21540225 -1.116628222 2.752082e-01
    ## Adjacent    -0.074804832  0.01770019 -4.226216850 2.970655e-04

$\hat{y} = X\hat{\beta}$

``` r
y.hat<-x%*%b
head(y.hat)
```

    ##                    [,1]
    ## Baltra       116.725946
    ## Bartolome     -7.273154
    ## Caldwell      29.330659
    ## Champion      10.364266
    ## Coamano      -36.383916
    ## Daphne.Major  43.087705

``` r
head(lmod$fitted.values)
```

    ##       Baltra    Bartolome     Caldwell     Champion      Coamano Daphne.Major 
    ##   116.725946    -7.273154    29.330659    10.364266   -36.383916    43.087705

$residual: \hat{\varepsilon} = y - \hat{y}$

``` r
res<-y-y.hat
head(res)
```

    ##                   [,1]
    ## Baltra       -58.72595
    ## Bartolome     38.27315
    ## Caldwell     -26.33066
    ## Champion      14.63573
    ## Coamano       38.38392
    ## Daphne.Major -25.08771

``` r
head(lmod$residuals)
```

    ##       Baltra    Bartolome     Caldwell     Champion      Coamano Daphne.Major 
    ##    -58.72595     38.27315    -26.33066     14.63573     38.38392    -25.08771

``` r
lmod$df.residual #= n - p = 30 - 6
```

    ## [1] 24

$RSS = \hat{\varepsilon}^T\hat{\varepsilon}$

$\hat{\sigma}^2 = \frac{RSS}{n-p}$

``` r
RSS <- sum(res*res)
s2.hat <- RSS/(30-6)

RSS;s2.hat
```

    ## [1] 89231.37

    ## [1] 3717.974

``` r
(lmodsum$sigma)^2
```

    ## [1] 3717.974

$Var(\hat{\beta}) = (X^TX)^{-1}\hat{\sigma}^2$

``` r
var.b<-solve(t(x)%*%x)*s2.hat
var.b
```

    ##                      1          Area     Elevation      Nearest         Scruz
    ## 1         366.88329428  0.1404740421 -0.5807385312 -0.869644244 -1.3980671735
    ## Area        0.14047404  0.0005027618 -0.0009642999  0.004811068 -0.0001826696
    ## Elevation  -0.58073853 -0.0009642999  0.0028796966 -0.013196449  0.0011454447
    ## Nearest    -0.86964424  0.0048110685 -0.0131964495  1.111202600 -0.1420666472
    ## Scruz      -1.39806717 -0.0001826696  0.0011454447 -0.142066647  0.0463981286
    ## Adjacent    0.08587895  0.0001717816 -0.0006098372  0.005297104 -0.0007281114
    ##                Adjacent
    ## 1          0.0858789494
    ## Area       0.0001717816
    ## Elevation -0.0006098372
    ## Nearest    0.0052971041
    ## Scruz     -0.0007281114
    ## Adjacent   0.0003132967

``` r
sqrt(diag(var.b))
```

    ##           1        Area   Elevation     Nearest       Scruz    Adjacent 
    ## 19.15419782  0.02242235  0.05366280  1.05413595  0.21540225  0.01770019

``` r
lmodsum$coefficients
```

    ##                 Estimate  Std. Error      t value     Pr(>|t|)
    ## (Intercept)  7.068220709 19.15419782  0.369016796 7.153508e-01
    ## Area        -0.023938338  0.02242235 -1.067610554 2.963180e-01
    ## Elevation    0.319464761  0.05366280  5.953187968 3.823409e-06
    ## Nearest      0.009143961  1.05413595  0.008674366 9.931506e-01
    ## Scruz       -0.240524230  0.21540225 -1.116628222 2.752082e-01
    ## Adjacent    -0.074804832  0.01770019 -4.226216850 2.970655e-04

R-square/coefficient of determination/percentage of variance explain:

$R^2 = 1-\frac{RSS}{TSS}$ , $TSS=\sum_{i=1}^n (y_i-\bar{y})^2$

``` r
TSS <- sum((y-mean(y))^2)
R2 <- 1-RSS/TSS
R2
```

    ## [1] 0.7658469

``` r
lmodsum$r.squared
```

    ## [1] 0.7658469

Adjust R-square: $adj. R^2 = 1- \frac{RSS/(n-p)}{TSS/(n-1)}$

``` r
R2.adj <- 1- (RSS/(30-6))/(TSS/(30-1))
R2.adj
```

    ## [1] 0.7170651

``` r
lmodsum$adj.r.squared
```

    ## [1] 0.7170651

### 2. Identifiability

R fits the largest identifiable model by removing variables in the
reverse order of appearance in the model formula.

``` r
Adiff <- gala$Area - gala$Adjacent
lmod2 <- lm(Species ~Area + Elevation+Nearest+Scruz+Adjacent+Adiff, data = gala)
summary(lmod2)
```

    ## 
    ## Call:
    ## lm(formula = Species ~ Area + Elevation + Nearest + Scruz + Adjacent + 
    ##     Adiff, data = gala)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -111.679  -34.898   -7.862   33.460  182.584 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.068221  19.154198   0.369 0.715351    
    ## Area        -0.023938   0.022422  -1.068 0.296318    
    ## Elevation    0.319465   0.053663   5.953 3.82e-06 ***
    ## Nearest      0.009144   1.054136   0.009 0.993151    
    ## Scruz       -0.240524   0.215402  -1.117 0.275208    
    ## Adjacent    -0.074805   0.017700  -4.226 0.000297 ***
    ## Adiff              NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 60.98 on 24 degrees of freedom
    ## Multiple R-squared:  0.7658, Adjusted R-squared:  0.7171 
    ## F-statistic:  15.7 on 5 and 24 DF,  p-value: 6.838e-07

``` r
lmod3 <- lm(Species ~Adiff+ Area + Elevation+Nearest+Scruz+Adjacent, data = gala)
summary(lmod3)
```

    ## 
    ## Call:
    ## lm(formula = Species ~ Adiff + Area + Elevation + Nearest + Scruz + 
    ##     Adjacent, data = gala)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -111.679  -34.898   -7.862   33.460  182.584 
    ## 
    ## Coefficients: (1 not defined because of singularities)
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  7.068221  19.154198   0.369 0.715351    
    ## Adiff        0.074805   0.017700   4.226 0.000297 ***
    ## Area        -0.098743   0.034053  -2.900 0.007866 ** 
    ## Elevation    0.319465   0.053663   5.953 3.82e-06 ***
    ## Nearest      0.009144   1.054136   0.009 0.993151    
    ## Scruz       -0.240524   0.215402  -1.117 0.275208    
    ## Adjacent           NA         NA      NA       NA    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 60.98 on 24 degrees of freedom
    ## Multiple R-squared:  0.7658, Adjusted R-squared:  0.7171 
    ## F-statistic:  15.7 on 5 and 24 DF,  p-value: 6.838e-07

``` r
set.seed(123)
Adiffe <- Adiff + 0.001*(runif(30)-0.5)

lmod4 <-lm(Species ~Area + Elevation+Nearest+Scruz+Adjacent+Adiffe, data = gala)
summary(lmod4)$coefficients
```

    ##                  Estimate   Std. Error    t value     Pr(>|t|)
    ## (Intercept)  3.296376e+00 1.943413e+01  0.1696179 8.667937e-01
    ## Area        -4.512299e+04 4.258334e+04 -1.0596395 3.003104e-01
    ## Elevation    3.130228e-01 5.387014e-02  5.8106924 6.397528e-06
    ## Nearest      3.827335e-01 1.108984e+00  0.3451209 7.331398e-01
    ## Scruz       -2.619939e-01 2.158064e-01 -1.2140230 2.370580e-01
    ## Adjacent     4.512289e+04 4.258334e+04  1.0596371 3.003114e-01
    ## Adiffe       4.512296e+04 4.258334e+04  1.0596389 3.003107e-01

``` r
lmodsum$coefficients
```

    ##                 Estimate  Std. Error      t value     Pr(>|t|)
    ## (Intercept)  7.068220709 19.15419782  0.369016796 7.153508e-01
    ## Area        -0.023938338  0.02242235 -1.067610554 2.963180e-01
    ## Elevation    0.319464761  0.05366280  5.953187968 3.823409e-06
    ## Nearest      0.009143961  1.05413595  0.008674366 9.931506e-01
    ## Scruz       -0.240524230  0.21540225 -1.116628222 2.752082e-01
    ## Adjacent    -0.074804832  0.01770019 -4.226216850 2.970655e-04

### 3. Orthogonality

``` r
data(odor, package = "faraway")
?odor
```

    ## 在指定的程式套件和圖書館裡沒有與 'odor' 有關的說明書：
    ## 您可以用用 '??odor'

資料*odor*:

Description: Data from an experiment to determine the effects of column
temperature, gas/liquid ratio and packing height in reducing unpleasant
odor of chemical product that was being sold for household use.

Format:

1.  odor: Odor score

2.  temp: Temperature coded as -1, 0 and 1

3.  gas: Gas/Liquid ratio coded as -1, 0 and 1

4.  pack: Packing height coded as -1, 0 and 1

``` r
cov(odor[,-1])
```

    ##           temp       gas      pack
    ## temp 0.5714286 0.0000000 0.0000000
    ## gas  0.0000000 0.5714286 0.0000000
    ## pack 0.0000000 0.0000000 0.5714286

``` r
lmod5 <- lm(odor ~ temp + gas + pack, odor)
summary(lmod5, cor=T) #cor=T: the correlation matrix of the estimated parameters is returned and printed
```

    ## 
    ## Call:
    ## lm(formula = odor ~ temp + gas + pack, data = odor)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -50.200 -17.138   1.175  20.300  62.925 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   15.200      9.298   1.635    0.130
    ## temp         -12.125     12.732  -0.952    0.361
    ## gas          -17.000     12.732  -1.335    0.209
    ## pack         -21.375     12.732  -1.679    0.121
    ## 
    ## Residual standard error: 36.01 on 11 degrees of freedom
    ## Multiple R-squared:  0.3337, Adjusted R-squared:  0.1519 
    ## F-statistic: 1.836 on 3 and 11 DF,  p-value: 0.1989
    ## 
    ## Correlation of Coefficients:
    ##      (Intercept) temp gas 
    ## temp 0.00                 
    ## gas  0.00        0.00     
    ## pack 0.00        0.00 0.00

``` r
lmod6 <- lm(odor ~ gas + pack, odor)
summary(lmod6)
```

    ## 
    ## Call:
    ## lm(formula = odor ~ gas + pack, data = odor)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -50.200 -26.700   1.175  26.800  50.800 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)   15.200      9.262   1.641    0.127
    ## gas          -17.000     12.683  -1.340    0.205
    ## pack         -21.375     12.683  -1.685    0.118
    ## 
    ## Residual standard error: 35.87 on 12 degrees of freedom
    ## Multiple R-squared:  0.2787, Adjusted R-squared:  0.1585 
    ## F-statistic: 2.319 on 2 and 12 DF,  p-value: 0.1408
