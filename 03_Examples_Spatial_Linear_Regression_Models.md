Example: 03 Spatial Linear Regression Models
================
Takuya Shimamura
2020-06-30

  - [Packages](#packages)
  - [Example 3.1 Pure Spatial
    Autoregression](#example-3.1-pure-spatial-autoregression)
      - [Read data](#read-data)
      - [pure spatial atoregression](#pure-spatial-atoregression)
  - [Example 3.2 SEM (Spatial Error
    Model)](#example-3.2-sem-spatial-error-model)
  - [Exmaple 3.3 SLM (Spatial Laged
    Model)](#exmaple-3.3-slm-spatial-laged-model)
      - [Read data](#read-data-1)
      - [OLS](#ols)
      - [SLM-LM](#slm-lm)
      - [SLM-2SLS](#slm-2sls)
  - [Exmaple 3.4 SARAR (continued)](#exmaple-3.4-sarar-continued)
      - [SARAR-ML](#sarar-ml)
      - [SARAR-GS2SLS](#sarar-gs2sls)
  - [Example 3.5 LM and RLM test](#example-3.5-lm-and-rlm-test)
      - [Read data](#read-data-2)
      - [OLS](#ols-1)
      - [LM and RLM test](#lm-and-rlm-test)
  - [Example 3.6 Interpretation of
    parameters](#example-3.6-interpretation-of-parameters)
      - [Read data](#read-data-3)
      - [SLM\_ML](#slm_ml)
      - [Impact index](#impact-index)

## Packages

``` r
library(readxl) # to read excel file
library(spdep) # to manipulate spatial data
```

## Example 3.1 Pure Spatial Autoregression

#### Read data

``` r
# variables
Italy <- read_xlsx("./Input/Italy.xlsx")
UER <- Italy$`Umunployment Rate`
GDP <- Italy$`Real GDP`
PI <- Italy$`Price Index`

# GAL file
ita_regions <- c(1:20)
nbItaly <- read.gal("./Input/GALfile/Italy_GAL.GAL", region.id = ita_regions)
W_Italy <- nb2listw(nbItaly)
```

### pure spatial atoregression

``` r
PA <- spautolm(PI ~ 1, listw = W_Italy)
summary(PA)
```

    ## 
    ## Call: spautolm(formula = PI ~ 1, listw = W_Italy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.51919 -0.14807 -0.04147  0.13529  0.46760 
    ## 
    ## Coefficients: 
    ##             Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept) 1.854252   0.079391  23.356 < 2.2e-16
    ## 
    ## Lambda: 0.26425 LR test value: 1.57 p-value: 0.21021 
    ## Numerical Hessian standard error of lambda: 0.20196 
    ## 
    ## Log likelihood: -1.754236 
    ## ML residual variance (sigma squared): 0.068239, (sigma: 0.26123)
    ## Number of observations: 20 
    ## Number of parameters estimated: 3 
    ## AIC: 9.5085

## Example 3.2 SEM (Spatial Error Model)

Before reproduce, dataset must be created.

## Exmaple 3.3 SLM (Spatial Laged Model)

#### Read data

``` r
data(bostom)
str(boston.c)
W_USA <- nb2listw(boston.soi)
```

    ## 'data.frame':    506 obs. of  20 variables:
    ##  $ TOWN   : Factor w/ 92 levels "Arlington","Ashland",..: 54 77 77 46 46 46 69 69 69 69 ...
    ##  $ TOWNNO : int  0 1 1 2 2 2 3 3 3 3 ...
    ##  $ TRACT  : int  2011 2021 2022 2031 2032 2033 2041 2042 2043 2044 ...
    ##  $ LON    : num  -71 -71 -70.9 -70.9 -70.9 ...
    ##  $ LAT    : num  42.3 42.3 42.3 42.3 42.3 ...
    ##  $ MEDV   : num  24 21.6 34.7 33.4 36.2 28.7 22.9 27.1 16.5 18.9 ...
    ##  $ CMEDV  : num  24 21.6 34.7 33.4 36.2 28.7 22.9 22.1 16.5 18.9 ...
    ##  $ CRIM   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
    ##  $ ZN     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
    ##  $ INDUS  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
    ##  $ CHAS   : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ NOX    : num  0.538 0.469 0.469 0.458 0.458 0.458 0.524 0.524 0.524 0.524 ...
    ##  $ RM     : num  6.58 6.42 7.18 7 7.15 ...
    ##  $ AGE    : num  65.2 78.9 61.1 45.8 54.2 58.7 66.6 96.1 100 85.9 ...
    ##  $ DIS    : num  4.09 4.97 4.97 6.06 6.06 ...
    ##  $ RAD    : int  1 2 2 3 3 3 5 5 5 5 ...
    ##  $ TAX    : int  296 242 242 222 222 222 311 311 311 311 ...
    ##  $ PTRATIO: num  15.3 17.8 17.8 18.7 18.7 18.7 15.2 15.2 15.2 15.2 ...
    ##  $ B      : num  397 397 393 395 397 ...
    ##  $ LSTAT  : num  4.98 9.14 4.03 2.94 5.33 ...

### OLS

``` r
OLS <- lm(
  MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c
)
summary(OLS)

# BP test
lmtest::bptest(OLS)

# JB test
tseries::jarque.bera.test(OLS$residuals)

# Moran's I test
moran.test(OLS$residuals, listw = W_USA)
```

    ## 
    ## Call:
    ## lm(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + 
    ##     PTRATIO + B + LSTAT + TAX, data = boston.c)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -13.7429  -2.8887  -0.7514   1.8144  26.8277 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  37.308337   5.199690   7.175 2.66e-12 ***
    ## CRIM         -0.103402   0.033339  -3.102 0.002035 ** 
    ## RM            4.074379   0.420639   9.686  < 2e-16 ***
    ## INDUS         0.018212   0.062015   0.294 0.769138    
    ## NOX         -17.829176   3.889690  -4.584 5.79e-06 ***
    ## AGE          -0.002647   0.013353  -0.198 0.842957    
    ## DIS          -1.210182   0.186123  -6.502 1.94e-10 ***
    ## RAD           0.304603   0.066878   4.555 6.62e-06 ***
    ## PTRATIO      -1.131146   0.126079  -8.972  < 2e-16 ***
    ## B             0.009853   0.002735   3.603 0.000346 ***
    ## LSTAT        -0.525072   0.051543 -10.187  < 2e-16 ***
    ## TAX          -0.010901   0.003710  -2.939 0.003452 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 4.838 on 494 degrees of freedom
    ## Multiple R-squared:  0.7293, Adjusted R-squared:  0.7233 
    ## F-statistic:   121 on 11 and 494 DF,  p-value: < 2.2e-16
    ## 
    ## 
    ##  studentized Breusch-Pagan test
    ## 
    ## data:  OLS
    ## BP = 59.214, df = 11, p-value = 1.297e-08
    ## 
    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  OLS$residuals
    ## X-squared = 936.74, df = 2, p-value < 2.2e-16
    ## 
    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  OLS$residuals  
    ## weights: W_USA    
    ## 
    ## Moran I statistic standard deviate = 15.258, p-value < 2.2e-16
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       0.480765673      -0.001980198       0.001001010

### SLM-LM

``` r
SLM_ML <- lagsarlm(
  MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c, listw = W_USA
)
summary(SLM_ML)
```

    ## 
    ## Call:lagsarlm(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_USA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -14.5758  -2.2207  -0.6105   1.5095  25.8126 
    ## 
    ## Type: lag 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value  Pr(>|z|)
    ## (Intercept)  9.9494716  4.6246838  2.1514 0.0314459
    ## CRIM        -0.0563840  0.0278782 -2.0225 0.0431238
    ## RM           3.8170563  0.3563235 10.7123 < 2.2e-16
    ## INDUS        0.0327685  0.0514609  0.6368 0.5242783
    ## NOX         -7.4193204  3.3049403 -2.2449 0.0247734
    ## AGE         -0.0129553  0.0110875 -1.1685 0.2426207
    ## DIS         -0.8367538  0.1593477 -5.2511 1.512e-07
    ## RAD          0.2116813  0.0560577  3.7761 0.0001593
    ## PTRATIO     -0.6005236  0.1105190 -5.4337 5.521e-08
    ## B            0.0075613  0.0022839  3.3106 0.0009309
    ## LSTAT       -0.2885108  0.0447018 -6.4541 1.088e-10
    ## TAX         -0.0091681  0.0030888 -2.9682 0.0029959
    ## 
    ## Rho: 0.45861, LR test value: 146.81, p-value: < 2.22e-16
    ## Asymptotic standard error: 0.033612
    ##     z-value: 13.644, p-value: < 2.22e-16
    ## Wald statistic: 186.17, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -1436.21 for lag model
    ## ML residual variance (sigma squared): 16.108, (sigma: 4.0135)
    ## Number of observations: 506 
    ## Number of parameters estimated: 14 
    ## AIC: 2900.4, (AIC for lm: 3045.2)
    ## LM test for residual autocorrelation
    ## test value: 36.098, p-value: 1.8763e-09

``` r
# JB test
tseries::jarque.bera.test(SLM_ML$residuals)
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  SLM_ML$residuals
    ## X-squared = 1036.6, df = 2, p-value < 2.2e-16

``` r
# Moran's I test
moran.test(SLM_ML$residuals, listw = W_USA)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SLM_ML$residuals  
    ## weights: W_USA    
    ## 
    ## Moran I statistic standard deviate = 4.4343, p-value = 4.618e-06
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.1382393973     -0.0019801980      0.0009999213

### SLM-2SLS

``` r
SLM_2SLS <- stsls(
  MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c, listw = W_USA
)
summary(SLM_2SLS)
```

    ## 
    ## Call:stsls(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_USA)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -14.41206  -2.34302  -0.66736   1.39880  25.95890 
    ## 
    ## Coefficients: 
    ##               Estimate Std. Error t value  Pr(>|t|)
    ## Rho          0.3684463  0.0555233  6.6359 3.226e-11
    ## (Intercept) 15.3282846  5.5476952  2.7630 0.0057273
    ## CRIM        -0.0656279  0.0290967 -2.2555 0.0241015
    ## RM           3.8676465  0.3613648 10.7029 < 2.2e-16
    ## INDUS        0.0299066  0.0531074  0.5631 0.5733437
    ## NOX         -9.4659209  3.5597068 -2.6592 0.0078330
    ## AGE         -0.0109286  0.0114966 -0.9506 0.3418086
    ## DIS         -0.9101707  0.1655913 -5.4965 3.874e-08
    ## RAD          0.2299499  0.0583353  3.9419 8.085e-05
    ## PTRATIO     -0.7048451  0.1255846 -5.6125 1.994e-08
    ## B            0.0080119  0.0023569  3.3994 0.0006754
    ## LSTAT       -0.3350193  0.0525966 -6.3696 1.895e-10
    ## TAX         -0.0095089  0.0031821 -2.9883 0.0028057
    ## 
    ## Residual variance (sigma squared): 17.146, (sigma: 4.1407)

``` r
# JB test
tseries::jarque.bera.test(SLM_2SLS$residuals)
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  SLM_2SLS$residuals
    ## X-squared = 1060.8, df = 2, p-value < 2.2e-16

``` r
# Moran's I test
moran.test(SLM_2SLS$residuals, listw = W_USA)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SLM_2SLS$residuals  
    ## weights: W_USA    
    ## 
    ## Moran I statistic standard deviate = 7.0203, p-value = 1.107e-12
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      0.2200107889     -0.0019801980      0.0009999177

## Exmaple 3.4 SARAR (continued)

### SARAR-ML

``` r
SARAR_ML <- sacsarlm(
  MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c, listw = W_USA
)
summary(SARAR_ML)
```

    ## 
    ## Call:sacsarlm(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_USA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -22.1904  -2.0443  -0.5744   1.3632  22.9109 
    ## 
    ## Type: sac 
    ## Coefficients: (asymptotic standard errors) 
    ##                Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)  25.1787145   5.8728488  4.2873 1.809e-05
    ## CRIM         -0.0832015   0.0276981 -3.0039 0.0026657
    ## RM            4.3310149   0.3585701 12.0786 < 2.2e-16
    ## INDUS        -0.0331919   0.0710612 -0.4671 0.6404359
    ## NOX         -13.2333266   5.0787924 -2.6056 0.0091712
    ## AGE          -0.0342447   0.0130598 -2.6221 0.0087378
    ## DIS          -1.3151723   0.2492327 -5.2769 1.314e-07
    ## RAD           0.2661552   0.0732950  3.6313 0.0002820
    ## PTRATIO      -0.7619121   0.1433341 -5.3156 1.063e-07
    ## B             0.0112946   0.0029121  3.8786 0.0001051
    ## LSTAT        -0.3560926   0.0516072 -6.9001 5.198e-12
    ## TAX          -0.0120994   0.0035789 -3.3808 0.0007229
    ## 
    ## Rho: 0.10485
    ## Asymptotic standard error: 0.058441
    ##     z-value: 1.7941, p-value: 0.072796
    ## Lambda: 0.54902
    ## Asymptotic standard error: 0.058509
    ##     z-value: 9.3834, p-value: < 2.22e-16
    ## 
    ## LR test value: 193.67, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -1412.778 for sac model
    ## ML residual variance (sigma squared): 14.211, (sigma: 3.7698)
    ## Number of observations: 506 
    ## Number of parameters estimated: 15 
    ## AIC: 2855.6, (AIC for lm: 3045.2)

``` r
# JB test
tseries::jarque.bera.test(SARAR_ML$residuals)
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  SARAR_ML$residuals
    ## X-squared = 1246.8, df = 2, p-value < 2.2e-16

``` r
# Moran's I test
moran.test(SARAR_ML$residuals, listw = W_USA)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SARAR_ML$residuals  
    ## weights: W_USA    
    ## 
    ## Moran I statistic standard deviate = -1.9927, p-value = 0.9769
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      -0.064926420      -0.001980198       0.000997849

### SARAR-GS2SLS

``` r
SARAR_GS2SLS <- sacsarlm(
  MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c, listw = W_USA
)
summary(SARAR_GS2SLS)
```

    ## 
    ## Call:sacsarlm(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_USA)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -22.1904  -2.0443  -0.5744   1.3632  22.9109 
    ## 
    ## Type: sac 
    ## Coefficients: (asymptotic standard errors) 
    ##                Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)  25.1787145   5.8728488  4.2873 1.809e-05
    ## CRIM         -0.0832015   0.0276981 -3.0039 0.0026657
    ## RM            4.3310149   0.3585701 12.0786 < 2.2e-16
    ## INDUS        -0.0331919   0.0710612 -0.4671 0.6404359
    ## NOX         -13.2333266   5.0787924 -2.6056 0.0091712
    ## AGE          -0.0342447   0.0130598 -2.6221 0.0087378
    ## DIS          -1.3151723   0.2492327 -5.2769 1.314e-07
    ## RAD           0.2661552   0.0732950  3.6313 0.0002820
    ## PTRATIO      -0.7619121   0.1433341 -5.3156 1.063e-07
    ## B             0.0112946   0.0029121  3.8786 0.0001051
    ## LSTAT        -0.3560926   0.0516072 -6.9001 5.198e-12
    ## TAX          -0.0120994   0.0035789 -3.3808 0.0007229
    ## 
    ## Rho: 0.10485
    ## Asymptotic standard error: 0.058441
    ##     z-value: 1.7941, p-value: 0.072796
    ## Lambda: 0.54902
    ## Asymptotic standard error: 0.058509
    ##     z-value: 9.3834, p-value: < 2.22e-16
    ## 
    ## LR test value: 193.67, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -1412.778 for sac model
    ## ML residual variance (sigma squared): 14.211, (sigma: 3.7698)
    ## Number of observations: 506 
    ## Number of parameters estimated: 15 
    ## AIC: 2855.6, (AIC for lm: 3045.2)

``` r
# JB test
tseries::jarque.bera.test(SARAR_GS2SLS$residuals)
```

    ## 
    ##  Jarque Bera Test
    ## 
    ## data:  SARAR_GS2SLS$residuals
    ## X-squared = 1246.8, df = 2, p-value < 2.2e-16

``` r
# Moran's I test
moran.test(SARAR_GS2SLS$residuals, listw = W_USA)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SARAR_GS2SLS$residuals  
    ## weights: W_USA    
    ## 
    ## Moran I statistic standard deviate = -1.9927, p-value = 0.9769
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##      -0.064926420      -0.001980198       0.000997849

## Example 3.5 LM and RLM test

#### Read data

``` r
# variables
Italy <- read_xlsx("./Input/Italy.xlsx")
UER <- Italy$`Umunployment Rate`
PI <- Italy$`Price Index`

# GAL file
ita_regions <- c(1:20)
nbItaly <- read.gal("./Input/GALfile/Italy_GAL.GAL", region.id = ita_regions)
W_Italy <- nb2listw(nbItaly)
```

### OLS

``` r
OLS <- lm(UER ~ PI)
summary(OLS)
```

    ## 
    ## Call:
    ## lm(formula = UER ~ PI)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.3404 -1.3598  0.0828  1.4406  5.2836 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -9.827      3.720  -2.642 0.016568 *  
    ## PI             8.746      1.984   4.409 0.000338 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2.437 on 18 degrees of freedom
    ## Multiple R-squared:  0.5193, Adjusted R-squared:  0.4926 
    ## F-statistic: 19.44 on 1 and 18 DF,  p-value: 0.0003383

### LM and RLM test

``` r
lm.LMtests(OLS, listw = W_Italy, test = "all")
```

    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = UER ~ PI)
    ## weights: W_Italy
    ## 
    ## LMerr = 1.9439, df = 1, p-value = 0.1632
    ## 
    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = UER ~ PI)
    ## weights: W_Italy
    ## 
    ## LMlag = 12.541, df = 1, p-value = 0.0003982
    ## 
    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = UER ~ PI)
    ## weights: W_Italy
    ## 
    ## RLMerr = 2.2426, df = 1, p-value = 0.1343
    ## 
    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = UER ~ PI)
    ## weights: W_Italy
    ## 
    ## RLMlag = 12.839, df = 1, p-value = 0.0003394
    ## 
    ## 
    ##  Lagrange multiplier diagnostics for spatial dependence
    ## 
    ## data:  
    ## model: lm(formula = UER ~ PI)
    ## weights: W_Italy
    ## 
    ## SARMA = 14.783, df = 2, p-value = 0.0006164

## Example 3.6 Interpretation of parameters

#### Read data

``` r
# variables
Italy <- read_xlsx("./Input/Italy.xlsx")
UER <- Italy$`Umunployment Rate`
GDP <- Italy$`Real GDP`

# GAL file
ita_regions <- c(1:20)
nbItaly <- read.gal("./Input/GALfile/Italy_GAL.GAL", region.id = ita_regions)
W_Italy <- nb2listw(nbItaly)
```

### SLM\_ML

``` r
SLM_ML <- lagsarlm(UER ~ GDP, listw = W_Italy)
summary(SLM_ML)
```

    ## 
    ## Call:lagsarlm(formula = UER ~ GDP, listw = W_Italy)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.09118 -0.95380 -0.24728  0.48779  2.89917 
    ## 
    ## Type: lag 
    ## Coefficients: (asymptotic standard errors) 
    ##             Estimate Std. Error z value Pr(>|z|)
    ## (Intercept)  3.13002    1.07877  2.9015 0.003714
    ## GDP         -1.08917    0.43648 -2.4954 0.012583
    ## 
    ## Rho: 0.74028, LR test value: 21.981, p-value: 2.753e-06
    ## Asymptotic standard error: 0.10545
    ##     z-value: 7.0202, p-value: 2.2158e-12
    ## Wald statistic: 49.283, p-value: 2.2158e-12
    ## 
    ## Log likelihood: -35.15273 for lag model
    ## ML residual variance (sigma squared): 1.5492, (sigma: 1.2447)
    ## Number of observations: 20 
    ## Number of parameters estimated: 4 
    ## AIC: 78.305, (AIC for lm: 98.287)
    ## LM test for residual autocorrelation
    ## test value: 3.1247, p-value: 0.077113

### Impact index

``` r
impact <- impacts(SLM_ML, listw = W_Italy)
impact
```

    ## Impact measures (lag, exact):
    ##        Direct  Indirect     Total
    ## GDP -1.460845 -2.732731 -4.193575
