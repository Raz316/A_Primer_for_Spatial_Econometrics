Exercises: 03 Spatial Linear Regression Models
================
Takuya Shimamura
2020-06-30

  - [Packages](#packages)
  - [Data](#data)
      - [EU](#eu)
  - [Exercise 3.1](#exercise-3.1)
      - [SEM: Spacial Error Model](#sem-spacial-error-model)
      - [SLM: Spacial Lag Model](#slm-spacial-lag-model)
      - [SARAL](#saral)
      - [Interpretation](#interpretation)
  - [Exercise 3.6](#exercise-3.6)
      - [SEM](#sem)

## Packages

``` r
library(readxl) # to read excel file
library(spdep) # to manipulate spatial data
```

## Data

#### EU

1)  Read table data

<!-- end list -->

``` r
EU <- read_xlsx("Input/EU.xlsx")
EU
```

    ## # A tibble: 27 x 7
    ##     ...1 `Country Code` Country        `% Education Expenses 2009` `Growth 2010–2011` `% of Hi-tec Exports` `Hi-tec Intensity`
    ##    <dbl> <chr>          <chr>                                <dbl>              <dbl>                 <dbl>              <dbl>
    ##  1     1 BE             Belgium                               42                 1.05                   8.8                  0
    ##  2     2 BG             Bulgaria                              27.9               1.06                   4.6                  0
    ##  3     3 CZ             Czech Republic                        17.5               1.04                  15.2                  0
    ##  4     4 DK             Denmark                               40.7               1.07                  12.3                  0
    ##  5     5 DE             Germany                               29.4               1.07                  14                    0
    ##  6     6 EE             Estonia                               35.9               1.17                   6.9                  0
    ##  7     7 IE             Ireland                               48.9               1.09                  22.1                  1
    ##  8     8 ES             Spain                                 39.4               1.07                   4.8                  0
    ##  9     9 FR             France                                43.2               1.04                  19.7                  1
    ## 10    10 IT             Italy                                 19                 1.07                   6.8                  0
    ## # ... with 17 more rows

2)  Read GAL file

<!-- end list -->

``` r
eu_countries <- c(1:27)
nbEU <- read.gal("Input/GALfile/EU_GAL.GAL", region.id = eu_countries)
W_EU <- nb2listw(nbEU)
```

3)  Define variable

<!-- end list -->

``` r
edu <- EU$`% Education Expenses 2009`
growth <- EU$`Growth 2010–2011`
```

## Exercise 3.1

### SEM: Spacial Error Model

##### 1\) Regression

1)  ML

<!-- end list -->

``` r
SEM_ML <- errorsarlm(growth ~ edu, listw = W_EU)
summary(SEM_ML)
```

    ## 
    ## Call:errorsarlm(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0907286 -0.0167880 -0.0040583  0.0115190  0.1049786 
    ## 
    ## Type: error 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 1.02893540 0.02875936  35.777  < 2e-16
    ## edu         0.00143114 0.00080309   1.782  0.07474
    ## 
    ## Lambda: 0.31614, LR test value: 2.4487, p-value: 0.11762
    ## Asymptotic standard error: 0.18784
    ##     z-value: 1.683, p-value: 0.092374
    ## Wald statistic: 2.8325, p-value: 0.092374
    ## 
    ## Log likelihood: 49.92492 for error model
    ## ML residual variance (sigma squared): 0.0013927, (sigma: 0.037319)
    ## Number of observations: 27 
    ## Number of parameters estimated: 4 
    ## AIC: -91.85, (AIC for lm: -91.401)

2)  FGLS

<!-- end list -->

``` r
SEM_FGLS <- GMerrorsar(growth ~ edu, listw = W_EU)
summary(SEM_FGLS)
```

    ## 
    ## Call:GMerrorsar(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0828102 -0.0192459 -0.0044578  0.0060903  0.1040963 
    ## 
    ## Type: GM SAR estimator
    ## Coefficients: (GM standard errors) 
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 1.02819967 0.02839284 36.2133   <2e-16
    ## edu         0.00145026 0.00079725  1.8191   0.0689
    ## 
    ## Lambda: 0.28325 (standard error): 0.30501 (z-value): 0.92866
    ## Residual variance (sigma squared): 0.0014075, (sigma: 0.037517)
    ## GM argmin sigma squared: 0.0014122
    ## Number of observations: 27 
    ## Number of parameters estimated: 4

#### 2\) Moran test

1)  ML

<!-- end list -->

``` r
moran.test(SEM_ML$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SEM_ML$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = 0.055287, p-value = 0.478
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       -0.02951959       -0.03846154        0.02615894

2)  FGLS

<!-- end list -->

``` r
moran.test(SEM_FGLS$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SEM_FGLS$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = 1.9241, p-value = 0.02717
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##        0.27587535       -0.03846154        0.02669044

### SLM: Spacial Lag Model

##### 1\) Regression

1)  ML

<!-- end list -->

``` r
SLM_ML <- lagsarlm(growth ~ edu, listw = W_EU)
summary(SLM_ML)
```

    ## 
    ## Call:lagsarlm(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0882519 -0.0175305 -0.0027772  0.0102252  0.1061910 
    ## 
    ## Type: lag 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 0.67489998 0.19028461  3.5468  0.00039
    ## edu         0.00133422 0.00071315  1.8709  0.06136
    ## 
    ## Rho: 0.33223, LR test value: 2.9646, p-value: 0.085103
    ## Asymptotic standard error: 0.18056
    ##     z-value: 1.84, p-value: 0.065761
    ## Wald statistic: 3.3858, p-value: 0.065761
    ## 
    ## Log likelihood: 50.18291 for lag model
    ## ML residual variance (sigma squared): 0.0013603, (sigma: 0.036883)
    ## Number of observations: 27 
    ## Number of parameters estimated: 4 
    ## AIC: -92.366, (AIC for lm: -91.401)
    ## LM test for residual autocorrelation
    ## test value: 0.8004, p-value: 0.37097

2)  2LSE

<!-- end list -->

``` r
SLM_2SLE <- stsls(growth ~ edu, listw = W_EU)
summary(SLM_2SLE)
```

    ## 
    ## Call:stsls(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0897037 -0.0180707 -0.0010401  0.0120080  0.1068047 
    ## 
    ## Coefficients: 
    ##               Estimate Std. Error t value Pr(>|t|)
    ## Rho         0.41188169 0.62946943  0.6543   0.5129
    ## (Intercept) 0.59143714 0.66010081  0.8960   0.3703
    ## edu         0.00127323 0.00087406  1.4567   0.1452
    ## 
    ## Residual variance (sigma squared): 0.0015023, (sigma: 0.038759)

##### 2\) Moran test

1)  ML

<!-- end list -->

``` r
moran.test(SLM_ML$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SLM_ML$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = -0.048772, p-value = 0.5194
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       -0.04633261       -0.03846154        0.02604463

2)  2LSE

<!-- end list -->

``` r
moran.test(SLM_2SLE$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SLM_2SLE$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = -0.55525, p-value = 0.7106
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       -0.12770035       -0.03846154        0.02583073

### SARAL

##### 1\) Regression

1)  ML

<!-- end list -->

``` r
SARAR_ML <- sacsarlm(growth ~ edu, listw = W_EU)
summary(SARAR_ML)
```

    ## 
    ## Call:sacsarlm(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0762434 -0.0142667 -0.0045352  0.0076759  0.0888500 
    ## 
    ## Type: sac 
    ## Coefficients: (asymptotic standard errors) 
    ##               Estimate Std. Error z value Pr(>|z|)
    ## (Intercept) 0.31179339 0.19882460  1.5682  0.11684
    ## edu         0.00093129 0.00055315  1.6836  0.09226
    ## 
    ## Rho: 0.68248
    ## Asymptotic standard error: 0.19268
    ##     z-value: 3.5421, p-value: 0.0003969
    ## Lambda: -0.47927
    ## Asymptotic standard error: 0.28655
    ##     z-value: -1.6726, p-value: 0.094412
    ## 
    ## LR test value: 4.049, p-value: 0.13206
    ## 
    ## Log likelihood: 50.72507 for sac model
    ## ML residual variance (sigma squared): 0.00099499, (sigma: 0.031543)
    ## Number of observations: 27 
    ## Number of parameters estimated: 5 
    ## AIC: -91.45, (AIC for lm: -91.401)

2)  GS2SLS

<!-- end list -->

``` r
SARAR_GS2SLS <- gstsls(growth ~ edu, listw = W_EU)
summary(SARAR_GS2SLS)
```

    ## 
    ## Call:gstsls(formula = growth ~ edu, listw = W_EU)
    ## 
    ## Residuals:
    ##        Min         1Q     Median         3Q        Max 
    ## -0.0853986 -0.0171702 -0.0020562  0.0096383  0.1032296 
    ## 
    ## Type: GM SARAR estimator
    ## Coefficients: (GM standard errors) 
    ##               Estimate Std. Error z value Pr(>|z|)
    ## Rho_Wy      0.43992221 0.61064424  0.7204   0.4713
    ## (Intercept) 0.56231401 0.63744768  0.8821   0.3777
    ## edu         0.00123836 0.00087061  1.4224   0.1549
    ## 
    ## Lambda: -0.1372
    ## Residual variance (sigma squared): 0.0014446, (sigma: 0.038008)
    ## GM argmin sigma squared: 0.0012997
    ## Number of observations: 27 
    ## Number of parameters estimated: 5

##### 2\) Moral test

1)  ML

<!-- end list -->

``` r
moran.test(SARAR_ML$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SARAR_ML$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = 0.3001, p-value = 0.3821
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##        0.01020601       -0.03846154        0.02629998

2)  GS2SLS

<!-- end list -->

``` r
moran.test(SARAR_GS2SLS$residuals, listw = W_EU)
```

    ## 
    ##  Moran I test under randomisation
    ## 
    ## data:  SARAR_GS2SLS$residuals  
    ## weights: W_EU    
    ## 
    ## Moran I statistic standard deviate = 0.049676, p-value = 0.4802
    ## alternative hypothesis: greater
    ## sample estimates:
    ## Moran I statistic       Expectation          Variance 
    ##       -0.03043833       -0.03846154        0.02608530

### Interpretation

  - The results in text book show the ones of three models using `ML`
    estimator.
  - In `SARAL` model, \(\rho \neq 0\) and \(\lambda = 0\) at 95% of
    significant level, `SLM` is the best model.
  - As for AIC, `SLM` has smallest value.

## Exercise 3.6

### SEM

##### 1\) Read data

``` r
data(boston)
?boston
W_Boston <- nb2listw(boston.soi)
str(boston.c)
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

##### 2\) Regression

1)  ML

<!-- end list -->

``` r
SEM_ML_boston <- errorsarlm(
  MEDV ~ CRIM + RM +  INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c,
  listw = W_Boston)
summary(SEM_ML_boston)
```

    ## 
    ## Call:errorsarlm(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_Boston)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -22.47676  -1.94487  -0.57867   1.29241  22.00163 
    ## 
    ## Type: error 
    ## Coefficients: (asymptotic standard errors) 
    ##                Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)  28.7711397   5.3972049  5.3307 9.781e-08
    ## CRIM         -0.0849750   0.0272788 -3.1151 0.0018391
    ## RM            4.2669725   0.3521893 12.1156 < 2.2e-16
    ## INDUS        -0.0562610   0.0736717 -0.7637 0.4450628
    ## NOX         -14.3128068   5.4079952 -2.6466 0.0081305
    ## AGE          -0.0377138   0.0131799 -2.8615 0.0042170
    ## DIS          -1.3421754   0.2720612 -4.9334 8.083e-07
    ## RAD           0.2613614   0.0768493  3.4010 0.0006715
    ## PTRATIO      -0.7444100   0.1476810 -5.0407 4.639e-07
    ## B             0.0119054   0.0029704  4.0081 6.122e-05
    ## LSTAT        -0.3629340   0.0509620 -7.1217 1.066e-12
    ## TAX          -0.0121817   0.0036054 -3.3787 0.0007283
    ## 
    ## Lambda: 0.62698, LR test value: 191.59, p-value: < 2.22e-16
    ## Asymptotic standard error: 0.037754
    ##     z-value: 16.607, p-value: < 2.22e-16
    ## Wald statistic: 275.79, p-value: < 2.22e-16
    ## 
    ## Log likelihood: -1413.816 for error model
    ## ML residual variance (sigma squared): 13.843, (sigma: 3.7206)
    ## Number of observations: 506 
    ## Number of parameters estimated: 14 
    ## AIC: 2855.6, (AIC for lm: 3045.2)

2)  FGLS

<!-- end list -->

``` r
SEM_FGLS_boston <- GMerrorsar(
  MEDV ~ CRIM + RM +  INDUS + NOX + AGE + DIS + RAD + PTRATIO + B + LSTAT + TAX,
  data = boston.c,
  listw = W_Boston)
summary(SEM_FGLS_boston)
```

    ## 
    ## Call:GMerrorsar(formula = MEDV ~ CRIM + RM + INDUS + NOX + AGE + DIS +     RAD + PTRATIO + B + LSTAT + TAX, data = boston.c, listw = W_Boston)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -12.7672  -2.7788  -0.8284   1.6586  29.4175 
    ## 
    ## Type: GM SAR estimator
    ## Coefficients: (GM standard errors) 
    ##                Estimate  Std. Error z value  Pr(>|z|)
    ## (Intercept)  29.4046169   5.4211169  5.4241 5.825e-08
    ## CRIM         -0.0877069   0.0281650 -3.1140 0.0018454
    ## RM            4.3056030   0.3640108 11.8282 < 2.2e-16
    ## INDUS        -0.0467449   0.0731892 -0.6387 0.5230275
    ## NOX         -14.5790050   5.2451106 -2.7795 0.0054436
    ## AGE          -0.0349967   0.0134071 -2.6103 0.0090460
    ## DIS          -1.3381876   0.2610185 -5.1268 2.947e-07
    ## RAD           0.2652296   0.0760499  3.4876 0.0004874
    ## PTRATIO      -0.7925132   0.1470923 -5.3879 7.130e-08
    ## B             0.0117077   0.0029941  3.9103 9.220e-05
    ## LSTAT        -0.3760408   0.0517244 -7.2701 3.593e-13
    ## TAX          -0.0119977   0.0036738 -3.2658 0.0010916
    ## 
    ## Lambda: 0.57168 (standard error): 0.071257 (z-value): 8.0228
    ## Residual variance (sigma squared): 14.786, (sigma: 3.8452)
    ## GM argmin sigma squared: 15.18
    ## Number of observations: 506 
    ## Number of parameters estimated: 14
