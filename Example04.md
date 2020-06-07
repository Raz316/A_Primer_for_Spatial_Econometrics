04 Further Topics in Spatial Econometrics
================
Takuya Shimamura
2020-06-08

  - [Packages](#packages)
  - [Example 4.1 Heteroscedastic](#example-4.1-heteroscedastic)
      - [1) Read data](#read-data)
      - [2) Regression](#regression)
  - [Example 4.2 Discrete (a-spatial logit and probit
    models)](#example-4.2-discrete-a-spatial-logit-and-probit-models)
      - [1) Read data](#read-data-1)
      - [2) Regression](#regression-1)
  - [Example 4.3 Discrete (Spatial
    Probit)](#example-4.3-discrete-spatial-probit)
  - [Example 4.4 Panel Data (unobserved heterogeneity and individual
    effects)](#example-4.4-panel-data-unobserved-heterogeneity-and-individual-effects)
      - [1) Read data](#read-data-2)
      - [2) Regression](#regression-2)
  - [Example 4.5 Panel Data (random
    effecits)](#example-4.5-panel-data-random-effecits)
      - [1) Read data](#read-data-3)
  - [Example 4.6 Panel Data (fixed
    effects)](#example-4.6-panel-data-fixed-effects)
      - [1) Read data](#read-data-4)
  - [Example 4.7 Non-Stationary](#example-4.7-non-stationary)
      - [1) Read data](#read-data-5)

## Packages

``` r
library(spdep)     # spatial econometrics
library(sphet)     # heteroscedastic
library(McSpatial) # discrete model
library(splm)      # panel data
library(spgwr)     # non-stationary
library(lmtest)
library(tseries)
library(maptools)
```

## Example 4.1 Heteroscedastic

#### 1\) Read data

1)  Read `columbus`

<!-- end list -->

``` r
library(spdep)
data("columbus")
str(columbus)
```

    ## 'data.frame':    49 obs. of  22 variables:
    ##  $ AREA      : num  0.3094 0.2593 0.1925 0.0838 0.4889 ...
    ##  $ PERIMETER : num  2.44 2.24 2.19 1.43 3 ...
    ##  $ COLUMBUS. : int  2 3 4 5 6 7 8 9 10 11 ...
    ##  $ COLUMBUS.I: int  5 1 6 2 7 8 4 3 18 10 ...
    ##  $ POLYID    : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ NEIG      : int  5 1 6 2 7 8 4 3 18 10 ...
    ##  $ HOVAL     : num  80.5 44.6 26.4 33.2 23.2 ...
    ##  $ INC       : num  19.53 21.23 15.96 4.48 11.25 ...
    ##  $ CRIME     : num  15.7 18.8 30.6 32.4 50.7 ...
    ##  $ OPEN      : num  2.851 5.297 4.535 0.394 0.406 ...
    ##  $ PLUMB     : num  0.217 0.321 0.374 1.187 0.625 ...
    ##  $ DISCBD    : num  5.03 4.27 3.89 3.7 2.83 3.78 2.74 2.89 3.17 4.33 ...
    ##  $ X         : num  38.8 35.6 39.8 36.5 40 ...
    ##  $ Y         : num  44.1 42.4 41.2 40.5 38 ...
    ##  $ AREA      : num  10.39 8.62 6.98 2.91 16.83 ...
    ##  $ NSA       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ NSB       : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ EW        : num  1 0 1 0 1 1 0 0 1 1 ...
    ##  $ CP        : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ THOUS     : num  1000 1000 1000 1000 1000 1000 1000 1000 1000 1000 ...
    ##  $ NEIGNO    : num  1005 1001 1006 1002 1007 ...
    ##  $ PERIM     : num  2.44 2.24 2.19 1.43 3 ...

2)  Define variables

<!-- end list -->

``` r
# Variables
crime <- columbus$CRIME
houseValue <- columbus$HOVAL
income <- columbus$INC
df_Ohio <- data.frame(crime, houseValue, income)
head(df_Ohio)
```

    ##      crime houseValue income
    ## 1 15.72598     80.467 19.531
    ## 2 18.80175     44.567 21.232
    ## 3 30.62678     26.350 15.956
    ## 4 32.38776     33.200  4.477
    ## 5 50.73151     23.225 11.252
    ## 6 26.06666     28.750 16.029

``` r
# Geographic weight   ### this should be cut off by distance 3.5
columbus <- st_read(system.file("shapes/columbus.shp", package="spData")[1], quiet=TRUE)
col.gal.nb <- read.gal(system.file("weights/columbus.gal", package="spData")[1])
W_Ohio <- nb2listw(col.gal.nb)
```

3)  Plot a map of the cenroids of the 49 neighborhoods in Columbus
    (Ohio)

<!-- end list -->

``` r
plot(columbus$X, columbus$Y,
     main = "Centrois of 49 States in Ohio",
     xlab = "X", ylab = "Y")
```

![](04_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### 2\) Regression

1)  GS2SLS: SARAR (homoscedasticity)

<!-- end list -->

``` r
SARAR_GS2SLS <- gstsls(crime ~ income + houseValue , data = df_Ohio, listw = W_Ohio)
summary(SARAR_GS2SLS)
```

    ## 
    ## Call:gstsls(formula = crime ~ income + houseValue, data = df_Ohio, 
    ##     listw = W_Ohio)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -37.80802  -5.63758  -0.23303   6.49483  24.31213 
    ## 
    ## Type: GM SARAR estimator
    ## Coefficients: (GM standard errors) 
    ##              Estimate Std. Error z value  Pr(>|z|)
    ## Rho_Wy       0.455519   0.190156  2.3955  0.016598
    ## (Intercept) 44.116333  11.237096  3.9260 8.639e-05
    ## income      -1.020821   0.393592 -2.5936  0.009498
    ## houseValue  -0.265474   0.092974 -2.8554  0.004299
    ## 
    ## Lambda: -0.039195
    ## Residual variance (sigma squared): 107.06, (sigma: 10.347)
    ## GM argmin sigma squared: 97.038
    ## Number of observations: 49 
    ## Number of parameters estimated: 6

2)  BP test: scedasticity

<!-- end list -->

``` r
bptest(lm(crime ~ income + houseValue), data = df_Ohio)
```

    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  lm(crime ~ income + houseValue)
    ## BP = 7.2166, df = 2, p-value = 0.0271

3)  FGS2SLS: SARAR (heteroscedasticity)

<!-- end list -->

``` r
SARAR_FGS2SLS <- gstslshet(crime ~ income + houseValue , data = df_Ohio, listw = W_Ohio)
summary(SARAR_FGS2SLS)
```

    ## 
    ##  Generalized stsls
    ## 
    ## Call:
    ## gstslshet(formula = crime ~ income + houseValue, data = df_Ohio, 
    ##     listw = W_Ohio)
    ## 
    ## Residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -37.468  -5.520  -0.398  -0.005   6.260  24.142 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept) 44.124087   7.506698  5.8780 4.153e-09 ***
    ## income      -0.987477   0.460184 -2.1458  0.031886 *  
    ## houseValue  -0.275573   0.176910 -1.5577  0.119305    
    ## lambda       0.452910   0.143270  3.1612  0.001571 ** 
    ## rho          0.064822   0.305091  0.2125  0.831742    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Wald test that rho and lambda are both zero:
    ##  Statistics: 3.5907 p-val: 0.058105

## Example 4.2 Discrete (a-spatial logit and probit models)

#### 1\) Read data

1)  Read `baltimore`

<!-- end list -->

``` r
data("baltimore")
str(baltimore)
```

    ## 'data.frame':    211 obs. of  17 variables:
    ##  $ STATION: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ PRICE  : num  47 113 165 104.3 62.5 ...
    ##  $ NROOM  : num  4 7 7 7 7 6 6 8 6 7 ...
    ##  $ DWELL  : num  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ NBATH  : num  1 2.5 2.5 2.5 1.5 2.5 2.5 1.5 1 2.5 ...
    ##  $ PATIO  : num  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ FIREPL : num  0 1 1 1 1 1 1 0 1 1 ...
    ##  $ AC     : num  0 1 0 1 0 0 1 0 1 1 ...
    ##  $ BMENT  : num  2 2 3 2 2 3 3 0 3 3 ...
    ##  $ NSTOR  : num  3 2 2 2 2 3 1 3 2 2 ...
    ##  $ GAR    : num  0 2 2 2 0 1 2 0 0 2 ...
    ##  $ AGE    : num  148 9 23 5 19 20 20 22 22 4 ...
    ##  $ CITCOU : num  0 1 1 1 1 1 1 1 1 1 ...
    ##  $ LOTSZ  : num  5.7 279.5 70.6 174.6 107.8 ...
    ##  $ SQFT   : num  11.2 28.9 30.6 26.1 22 ...
    ##  $ X      : num  907 922 920 923 918 900 918 907 918 897 ...
    ##  $ Y      : num  534 574 581 578 574 577 576 576 562 576 ...

2)  Plot a geographical distribution map

<!-- end list -->

``` r
if (requireNamespace("sf", quietly = TRUE)) {
  library(sf)
  baltimore_sf <- baltimore %>% st_as_sf(., coords = c("X","Y"))
  plot(baltimore_sf["PRICE"])
}
```

![](04_files/figure-gfm/unnamed-chunk-8-1.png)<!-- --> (iii) Define
variables

``` r
price <- baltimore$PRICE
nroom <- baltimore$NROOM
nbath <- baltimore$NBATH
age <- baltimore$AGE
sqft <- baltimore$SQFT

price.f <- factor(price > 40)
binary_price <- model.matrix(~price.f)

df_baltimore <- data.frame(
  price, binary_price, nroom, nbath, age, sqft
)
head(df_baltimore)
```

    ##   price X.Intercept. price.fTRUE nroom nbath age  sqft
    ## 1  47.0            1           1     4   1.0 148 11.25
    ## 2 113.0            1           1     7   2.5   9 28.92
    ## 3 165.0            1           1     7   2.5  23 30.62
    ## 4 104.3            1           1     7   2.5   5 26.12
    ## 5  62.5            1           1     7   1.5  19 22.04
    ## 6  70.0            1           1     6   2.5  20 39.42

#### 2\) Regression

1)  Logit model

<!-- end list -->

``` r
Logit <- glm(df_baltimore$price.fTRUE ~ nroom + nbath + age + sqft, 
                  family = binomial(link = "logit"))
summary(Logit)
```

    ## 
    ## Call:
    ## glm(formula = df_baltimore$price.fTRUE ~ nroom + nbath + age + 
    ##     sqft, family = binomial(link = "logit"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.0447  -0.8630  -0.1835   0.8259   3.8935  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.78782    1.01886  -3.718 0.000201 ***
    ## nroom        0.52624    0.23213   2.267 0.023392 *  
    ## nbath        0.56735    0.35130   1.615 0.106308    
    ## age         -0.05183    0.01214  -4.268 1.97e-05 ***
    ## sqft         0.10725    0.03553   3.019 0.002539 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 292.47  on 210  degrees of freedom
    ## Residual deviance: 216.63  on 206  degrees of freedom
    ## AIC: 226.63
    ## 
    ## Number of Fisher Scoring iterations: 5

2)  Probit model

<!-- end list -->

``` r
Probit <- glm(df_baltimore$price.fTRUE ~ nroom + nbath + age + sqft, 
             family = binomial(link = "probit"))
summary(Probit)
```

    ## 
    ## Call:
    ## glm(formula = df_baltimore$price.fTRUE ~ nroom + nbath + age + 
    ##     sqft, family = binomial(link = "probit"))
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -2.2042  -0.8954  -0.2502   0.9187   3.8712  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -2.162195   0.566727  -3.815 0.000136 ***
    ## nroom        0.252867   0.128005   1.975 0.048217 *  
    ## nbath        0.365626   0.203727   1.795 0.072704 .  
    ## age         -0.021129   0.006286  -3.361 0.000775 ***
    ## sqft         0.057961   0.019820   2.924 0.003452 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 292.47  on 210  degrees of freedom
    ## Residual deviance: 222.45  on 206  degrees of freedom
    ## AIC: 232.45
    ## 
    ## Number of Fisher Scoring iterations: 8

## Example 4.3 Discrete (Spatial Probit)

The way to define `Wdash` cannot be solved.

## Example 4.4 Panel Data (unobserved heterogeneity and individual effects)

#### 1\) Read data

1)  Read \`Prpduc

<!-- end list -->

``` r
library(plm)
data("Produc")
?Produc
str(Produc)
```

    ## 'data.frame':    816 obs. of  11 variables:
    ##  $ state : Factor w/ 48 levels "ALABAMA","ARIZONA",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ year  : int  1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 ...
    ##  $ region: Factor w/ 9 levels "1","2","3","4",..: 6 6 6 6 6 6 6 6 6 6 ...
    ##  $ pcap  : num  15033 15502 15972 16406 16763 ...
    ##  $ hwy   : num  7326 7526 7765 7908 8026 ...
    ##  $ water : num  1656 1721 1765 1742 1735 ...
    ##  $ util  : num  6051 6255 6442 6756 7002 ...
    ##  $ pc    : num  35794 37300 38670 40084 42057 ...
    ##  $ gsp   : int  28418 29375 31303 33430 33749 33604 35764 37463 39964 40979 ...
    ##  $ emp   : num  1010 1022 1072 1136 1170 ...
    ##  $ unemp : num  4.7 5.2 4.7 3.9 5.5 7.7 6.8 7.4 6.3 7.1 ...

2)  Subtract data from 1970 to 1974

<!-- end list -->

``` r
df_all <- Produc
df_1970_1974 <- subset(df_all, year >= 1970 & year <= 1974)
head(df_1970_1974)
```

    ##      state year region     pcap     hwy   water    util       pc   gsp    emp
    ## 1  ALABAMA 1970      6 15032.67 7325.80 1655.68 6051.20 35793.80 28418 1010.5
    ## 2  ALABAMA 1971      6 15501.94 7525.94 1721.02 6254.98 37299.91 29375 1021.9
    ## 3  ALABAMA 1972      6 15972.41 7765.42 1764.75 6442.23 38670.30 31303 1072.3
    ## 4  ALABAMA 1973      6 16406.26 7907.66 1742.41 6756.19 40084.01 33430 1135.5
    ## 5  ALABAMA 1974      6 16762.67 8025.52 1734.85 7002.29 42057.31 33749 1169.8
    ## 18 ARIZONA 1970      8 10148.42 4556.81 1627.87 3963.75 23585.99 19288  547.4
    ##    unemp
    ## 1    4.7
    ## 2    5.2
    ## 3    4.7
    ## 4    3.9
    ## 5    5.5
    ## 18   4.4

#### 2\) Regression

1)  Pooling model

<!-- end list -->

``` r
OLS_pooled <- plm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  data = df_1970_1974, 
  model = "pooling"
)
summary(OLS_pooled)
```

    ## Pooling Model
    ## 
    ## Call:
    ## plm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = df_1970_1974, model = "pooling")
    ## 
    ## Balanced Panel: n = 48, T = 5, N = 240
    ## 
    ## Residuals:
    ##        Min.     1st Qu.      Median     3rd Qu.        Max. 
    ## -0.22474742 -0.06124502 -0.00068411  0.05337487  0.29211115 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept) 1.2054162  0.1151322 10.4698 < 2.2e-16 ***
    ## log(pcap)   0.2140447  0.0355133  6.0272 6.405e-09 ***
    ## log(pc)     0.3421465  0.0212932 16.0684 < 2.2e-16 ***
    ## log(emp)    0.5113731  0.0261593 19.5484 < 2.2e-16 ***
    ## unemp       0.0118651  0.0047056  2.5215   0.01235 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    257.24
    ## Residual Sum of Squares: 2.126
    ## R-Squared:      0.99174
    ## Adj. R-Squared: 0.99159
    ## F-statistic: 7049.84 on 4 and 235 DF, p-value: < 2.22e-16

2)  Random effects

<!-- end list -->

``` r
FGSL_random <- plm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  data = df_1970_1974, 
  model = "random"
)
summary(FGSL_random)
```

    ## Oneway (individual) effect Random Effect Model 
    ##    (Swamy-Arora's transformation)
    ## 
    ## Call:
    ## plm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = df_1970_1974, model = "random")
    ## 
    ## Balanced Panel: n = 48, T = 5, N = 240
    ## 
    ## Effects:
    ##                     var   std.dev share
    ## idiosyncratic 0.0005797 0.0240773 0.061
    ## individual    0.0088851 0.0942608 0.939
    ## theta: 0.8865
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.0963245 -0.0154580  0.0020106  0.0153115  0.0997615 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z-value  Pr(>|z|)    
    ## (Intercept)  1.6620300  0.1929774  8.6126 < 2.2e-16 ***
    ## log(pcap)    0.1006627  0.0522321  1.9272 0.0539524 .  
    ## log(pc)      0.3411402  0.0413144  8.2572 < 2.2e-16 ***
    ## log(emp)     0.6212927  0.0376207 16.5146 < 2.2e-16 ***
    ## unemp       -0.0099608  0.0027338 -3.6436 0.0002688 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Total Sum of Squares:    4.3211
    ## Residual Sum of Squares: 0.14234
    ## R-Squared:      0.96706
    ## Adj. R-Squared: 0.9665
    ## Chisq: 6899.1 on 4 DF, p-value: < 2.22e-16

## Example 4.5 Panel Data (random effecits)

#### 1\) Read data

1)  Geographical weight

<!-- end list -->

``` r
data("used.cars")
W_US <- nb2listw(usa48.nb)
```

1)  SEM-RE (ML)

<!-- end list -->

``` r
library(splm)

SEM_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "random",
  spatial.error = "b", lag = FALSE)
summary(SEM_RE)
```

    ## ML panel with , random effects, spatial error correlation 
    ## 
    ## Call:
    ## spreml(formula = formula, data = data, index = index, w = listw2mat(listw), 
    ##     w2 = listw2mat(listw2), lag = lag, errors = errors, cl = cl)
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.26680 -0.06048 -0.00676 -0.00003  0.05357  0.46617 
    ## 
    ## Error variance parameters:
    ##     Estimate Std. Error t-value  Pr(>|t|)    
    ## phi  7.49518    1.73081  4.3304 1.488e-05 ***
    ## rho  0.53888    0.03371 15.9855 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t-value Pr(>|t|)    
    ## (Intercept)  2.3868275  0.1393798 17.1246  < 2e-16 ***
    ## log(pcap)    0.0424138  0.0222037  1.9102  0.05611 .  
    ## log(pc)      0.2418396  0.0202892 11.9196  < 2e-16 ***
    ## log(emp)     0.7423454  0.0244061 30.4164  < 2e-16 ***
    ## unemp       -0.0034279  0.0010614 -3.2295  0.00124 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

2)  KKP (LM)

<!-- end list -->

``` r
SEM_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "random",
  spatial.error = "kkp", lag = FALSE)
summary(SEM_RE)
```

    ## ML panel with , spatial RE (KKP), spatial error correlation 
    ## 
    ## Call:
    ## spreml(formula = formula, data = data, index = index, w = listw2mat(listw), 
    ##     w2 = listw2mat(listw2), lag = lag, errors = errors, cl = cl)
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.27000 -0.06425 -0.01118 -0.00448  0.04889  0.46937 
    ## 
    ## Error variance parameters:
    ##     Estimate Std. Error t-value  Pr(>|t|)    
    ## phi 6.624775   1.549683  4.2749 1.912e-05 ***
    ## rho 0.526465   0.033338 15.7917 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)  2.3246707  0.1415894 16.4184 < 2.2e-16 ***
    ## log(pcap)    0.0445475  0.0220377  2.0214 0.0432362 *  
    ## log(pc)      0.2461124  0.0211341 11.6453 < 2.2e-16 ***
    ## log(emp)     0.7426319  0.0254663 29.1614 < 2.2e-16 ***
    ## unemp       -0.0036045  0.0010637 -3.3887 0.0007022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

3)  SLM-RE (LM)

<!-- end list -->

``` r
SLM_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "random",
  spatial.error = "none", lag = TRUE)
summary(SLM_RE)
```

    ## ML panel with spatial lag, random effects 
    ## 
    ## Call:
    ## spreml(formula = formula, data = data, index = index, w = listw2mat(listw), 
    ##     w2 = listw2mat(listw2), lag = lag, errors = errors, cl = cl)
    ## 
    ## Residuals:
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##    1.38    1.57    1.70    1.70    1.80    2.13 
    ## 
    ## Error variance parameters:
    ##     Estimate Std. Error t-value Pr(>|t|)  
    ## phi  21.3175     8.3017  2.5678  0.01023 *
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.161615   0.029099   5.554 2.793e-08 ***
    ## 
    ## Coefficients:
    ##                Estimate  Std. Error t-value  Pr(>|t|)    
    ## (Intercept)  1.65814995  0.15071855 11.0016 < 2.2e-16 ***
    ## log(pcap)    0.01294505  0.02493997  0.5190    0.6037    
    ## log(pc)      0.22555376  0.02163422 10.4258 < 2.2e-16 ***
    ## log(emp)     0.67081075  0.02642113 25.3892 < 2.2e-16 ***
    ## unemp       -0.00579716  0.00089175 -6.5009 7.984e-11 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

4)  KPP (GM)

<!-- end list -->

``` r
SEM_GMM <- spgm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "random",
  spatial.error = TRUE, lag = FALSE)
summary(SEM_GMM)
```

    ##  
    ## 
    ## Call:
    ## spgm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "random", lag = FALSE, 
    ##     spatial.error = TRUE)
    ## 
    ## Residuals:
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -0.26599 -0.06549 -0.00725 -0.00449  0.04892  0.45804 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t-value  Pr(>|t|)    
    ## (Intercept)  2.2178061  0.1352650 16.3960 < 2.2e-16 ***
    ## log(pcap)    0.0533878  0.0221395  2.4114 0.0158905 *  
    ## log(pc)      0.2587524  0.0210013 12.3208 < 2.2e-16 ***
    ## log(emp)     0.7268627  0.0253709 28.6495 < 2.2e-16 ***
    ## unemp       -0.0039258  0.0011000 -3.5689 0.0003585 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

22) SLM-RE (GM)

<!-- end list -->

``` r
SLM_GMM <- spgm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "random",
  spatial.error = FALSE, lag = TRUE)
summary(SLM_GMM)
```

    ## 
    ## Call:spgm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "random", lag = TRUE, 
    ##     spatial.error = FALSE)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -3.11709 -0.70377 -0.06372  0.59301  5.60077 
    ## 
    ## Coefficients: 
    ##                Estimate  Std. Error t value  Pr(>|t|)
    ## lambda       0.04256929  0.01501675  2.8348  0.004586
    ## (Intercept)  1.89419519  0.16510513 11.4727 < 2.2e-16
    ## log(pcap)    0.02244269  0.02472283  0.9078  0.363999
    ## log(pc)      0.28871841  0.02114987 13.6511 < 2.2e-16
    ## log(emp)     0.70835224  0.02673746 26.4929 < 2.2e-16
    ## unemp       -0.00643463  0.00090743 -7.0910 1.331e-12
    ## 
    ## Residual variance (sigma squared): 1.146, (sigma: 1.0705)

## Example 4.6 Panel Data (fixed effects)

#### 1\) Read data

1)  SEM-RE (ML)

<!-- end list -->

``` r
SEM_FE_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "within",
  spatial.error = "b", lag = FALSE)
summary(SEM_FE_RE)
```

    ## Spatial panel fixed effects error model
    ##  
    ## 
    ## Call:
    ## spml(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "within", lag = FALSE, 
    ##     spatial.error = "b")
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.1246945 -0.0237699 -0.0034993  0.0170886  0.1882224 
    ## 
    ## Spatial error parameter:
    ##     Estimate Std. Error t-value  Pr(>|t|)    
    ## rho 0.557401   0.033075  16.853 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value Pr(>|t|)    
    ## log(pcap)  0.0051438  0.0250109  0.2057  0.83705    
    ## log(pc)    0.2053026  0.0231427  8.8712  < 2e-16 ***
    ## log(emp)   0.7822540  0.0278057 28.1328  < 2e-16 ***
    ## unemp     -0.0022317  0.0010709 -2.0839  0.03717 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

2)  KKP (LM)

<!-- end list -->

``` r
SEM_FE_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "within",
  spatial.error = "kkp", lag = FALSE)
summary(SEM_FE_RE)
```

    ## Spatial panel fixed effects error model
    ##  
    ## 
    ## Call:
    ## spml(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "within", lag = FALSE, 
    ##     spatial.error = "kkp")
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.1246945 -0.0237699 -0.0034993  0.0170886  0.1882224 
    ## 
    ## Spatial error parameter:
    ##     Estimate Std. Error t-value  Pr(>|t|)    
    ## rho 0.557401   0.033075  16.853 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value Pr(>|t|)    
    ## log(pcap)  0.0051438  0.0250109  0.2057  0.83705    
    ## log(pc)    0.2053026  0.0231427  8.8712  < 2e-16 ***
    ## log(emp)   0.7822540  0.0278057 28.1328  < 2e-16 ***
    ## unemp     -0.0022317  0.0010709 -2.0839  0.03717 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

3)  SLM-RE (LM)

<!-- end list -->

``` r
SLM_RE <- spml(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "within",
  spatial.error = "none", lag = TRUE)
summary(SLM_RE)
```

    ## Spatial panel fixed effects lag model
    ##  
    ## 
    ## Call:
    ## spml(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "within", lag = TRUE, 
    ##     spatial.error = "none")
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.1509930 -0.0191196 -0.0024997  0.0159299  0.1766827 
    ## 
    ## Spatial autoregressive coefficient:
    ##        Estimate Std. Error t-value  Pr(>|t|)    
    ## lambda 0.274689   0.023516  11.681 < 2.2e-16 ***
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value  Pr(>|t|)    
    ## log(pcap) -0.0465819  0.0254425 -1.8309   0.06712 .  
    ## log(pc)    0.1874325  0.0230441  8.1336 4.166e-16 ***
    ## log(emp)   0.6250902  0.0297044 21.0437 < 2.2e-16 ***
    ## unemp     -0.0044816  0.0008653 -5.1792 2.228e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

4)  KPP (GM)

<!-- end list -->

``` r
SEM_FE_GMM <- spgm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "within",
  spatial.error = TRUE, lag = FALSE)
summary(SEM_FE_GMM)
```

    ## Spatial panel fixed effects GM model
    ##  
    ## 
    ## Call:
    ## spgm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "within", lag = FALSE, 
    ##     spatial.error = TRUE)
    ## 
    ## Residuals:
    ##       Min.    1st Qu.     Median    3rd Qu.       Max. 
    ## -0.1496836 -0.0174426 -0.0019014  0.0142554  0.1703041 
    ## 
    ## Estimated spatial coefficient, variance components and theta:
    ##           Estimate
    ## rho       0.499871
    ## sigma^2_v 0.001105
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t-value Pr(>|t|)    
    ## log(pcap)  0.0043026  0.0253425  0.1698  0.86519    
    ## log(pc)    0.2144604  0.0232533  9.2228  < 2e-16 ***
    ## log(emp)   0.7830897  0.0279794 27.9881  < 2e-16 ***
    ## unemp     -0.0025609  0.0010546 -2.4282  0.01517 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

22) SLM-RE (GM)

<!-- end list -->

``` r
SLM_FE_GMM <- spgm(
  log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp,
  listw = W_US, data = Produc, model = "within",
  spatial.error = FALSE, lag = TRUE)
summary(SLM_FE_GMM)
```

    ## 
    ## Call:spgm(formula = log(gsp) ~ log(pcap) + log(pc) + log(emp) + unemp, 
    ##     data = Produc, listw = W_US, model = "within", lag = TRUE, 
    ##     spatial.error = FALSE)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.1417629 -0.0204993 -0.0026014  0.0167209  0.1695055 
    ## 
    ## Coefficients: 
    ##              Estimate  Std. Error t value  Pr(>|t|)
    ## lambda     0.19166263  0.02617774  7.3216 2.451e-13
    ## log(pcap) -0.04040614  0.02666502 -1.5153    0.1297
    ## log(pc)    0.21904067  0.02509769  8.7275 < 2.2e-16
    ## log(emp)   0.66833361  0.03077822 21.7145 < 2.2e-16
    ## unemp     -0.00472828  0.00090997 -5.1961 2.035e-07
    ## 
    ## Residual variance (sigma squared): 0.0011506, (sigma: 0.03392)

## Example 4.7 Non-Stationary

#### 1\) Read data

``` r
(.packages())
```

    ##  [1] "plm"       "tseries"   "lmtest"    "zoo"       "spgwr"     "splm"     
    ##  [7] "McSpatial" "RANN"      "quantreg"  "SparseM"   "maptools"  "locfit"   
    ## [13] "lattice"   "sphet"     "spdep"     "sf"        "spData"    "sp"       
    ## [19] "stats"     "graphics"  "grDevices" "utils"     "datasets"  "methods"  
    ## [25] "base"
