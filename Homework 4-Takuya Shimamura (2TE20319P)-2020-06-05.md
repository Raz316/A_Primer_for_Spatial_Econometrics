Homework 4
================
Takuya Shimamura (2TE20319P)
6/5/2020

## Task 4-1

### 1\) Read data

``` r
library(readxl)
d1 = read_excel("Input/4.xlsx", sheet = "4-1")
d1
```

    ## # A tibble: 5 x 3
    ##       A     B     C
    ##   <dbl> <dbl> <dbl>
    ## 1     5     4     4
    ## 2     7     5     3
    ## 3     6     4     2
    ## 4     6     5     4
    ## 5     7     4     3

There are 5 rows in each column.

### 2\) Box plot

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

### 3\) ANOVA

``` r
df1 <- data.frame(
  y1 = factor(c(rep("A",length(A)), rep("B",length(B)), rep("C",length(C)))),
  y2 = c(A, B, C))
ANOVA = aov(y2 ~ y1, data = df1)
summary(ANOVA)
```

    ##             Df Sum Sq Mean Sq F value   Pr(>F)    
    ## y1           2   22.8  11.400   20.12 0.000147 ***
    ## Residuals   12    6.8   0.567                     
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

P-value of ANOVA is smaller than 0.05; thus, Null hypothesis, which is
defined as true difference in means of any dada is equal to 0, is
denied. However, it is not clear which two data is significantly
different.

### 4\) Multiple comparisons of means

``` r
TukeyHSD(aov(y2 ~ y1, data = df1))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = y2 ~ y1, data = df1)
    ## 
    ## $y1
    ##     diff       lwr         upr     p adj
    ## B-A -1.8 -3.070157 -0.52984284 0.0068371
    ## C-A -3.0 -4.270157 -1.72984284 0.0001081
    ## C-B -1.2 -2.470157  0.07015716 0.0646802

There are two combinations whose P-values are smaller than 0.05, namely,
`A-B` and `A-C`. As a result, there are true differences in means of
yields between `A and B`, and `A and C`, depending on fertilizers.

### 5\) Visualize

``` r
par(mfrow=c(2,2))
plot(aov(y2 ~ y1, data = df1))
```

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

No. `1`, `11`, and `13` can be outliers in each data.

## Task 4-2

### 1\) Read data

``` r
d2 = read_excel("Input/4.xlsx", sheet = "4-2")
d2
```

    ## # A tibble: 3 x 4
    ##       A     B     C     D
    ##   <dbl> <dbl> <dbl> <dbl>
    ## 1    15    25    17    10
    ## 2     9    21    23    13
    ## 3    14    19    20    16

There are 3 rows in each column.

### 2\) Box plot

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### 3\) ANOVA

``` r
df2 <- data.frame(
  y1 = factor(c(rep("A",length(A)), rep("B",length(B)), rep("C",length(C)))),
  y2 = c(A, B, C))
ANOVA = aov(y2 ~ y1, data = df2)
summary(ANOVA)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)  
    ## y1           2 137.56   68.78   7.198 0.0255 *
    ## Residuals    6  57.33    9.56                 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

P-value of ANOVA is smaller than 0.05; thus, Null hypothesis, which is
defined as true difference in means of any dada is equal to 0, is
denied. However, it is not clear which two data is significantly
different.

### 4\) Multiple comparisons of means

``` r
TukeyHSD(aov(y2 ~ y1, data = df2))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = y2 ~ y1, data = df2)
    ## 
    ## $y1
    ##          diff        lwr       upr     p adj
    ## B-A  9.000000  1.2558003 16.744200 0.0274890
    ## C-A  7.333333 -0.4108663 15.077533 0.0611781
    ## C-B -1.666667 -9.4108663  6.077533 0.7937339

There is a combination whose P-values is smaller than 0.05, namely,
`A-B`. As a result, there is a true difference in means of growth rate
between `A` and `B` depending on soils.

### 5\) Visualize

``` r
par(mfrow=c(2,2))
plot(aov(y2 ~ y1, data = df2))
```

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

No. `2`, `4`, and `7` can be outliers in each data.

## Task 4-3

### 1\) Read data

``` r
d3 = read_excel("Input/4.xlsx", sheet = "4-3")
d3
```

    ## # A tibble: 5 x 7
    ##       A     B     C     D     E     F     G
    ##   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    41    48    40    40    49    40    41
    ## 2    44    49    50    39    41    48    46
    ## 3    48    49    44    46    50    51    54
    ## 4    43    49    48    46    39    47    44
    ## 5    42    45    50    41    42    51    42

There are 5 rows in each column.

### 2\) Box plot

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

### 3\) ANOVA

``` r
df3 <- data.frame(
  y1 = factor(c(rep("A",length(A)), rep("B",length(B)), rep("C",length(C)), rep("D",length(D)), 
                rep("E",length(E)), rep("F",length(F)), rep("G",length(G)))),
  y2 = c(A, B, C, D, E, F, G))
ANOVA = aov(y2 ~ y1, data = df3)
summary(ANOVA)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## y1           6  127.1   21.18   1.321  0.281
    ## Residuals   28  448.8   16.03

P-value of ANOVA is greater than 0.05; thus, Null hypothesis, which is
defined as true difference in means of any dada is equal to 0, is
accepted. As a result, there is no true difference among yields
depending on the groups.

### 4\) Multiple comparisons of means

``` r
TukeyHSD(aov(y2 ~ y1, data = df3))
```

    ##   Tukey multiple comparisons of means
    ##     95% family-wise confidence level
    ## 
    ## Fit: aov(formula = y2 ~ y1, data = df3)
    ## 
    ## $y1
    ##     diff        lwr       upr     p adj
    ## B-A  4.4  -3.632087 12.432087 0.5979832
    ## C-A  2.8  -5.232087 10.832087 0.9210369
    ## D-A -1.2  -9.232087  6.832087 0.9990040
    ## E-A  0.6  -7.432087  8.632087 0.9999823
    ## F-A  3.8  -4.232087 11.832087 0.7420265
    ## G-A  1.8  -6.232087  9.832087 0.9907949
    ## C-B -1.6  -9.632087  6.432087 0.9950907
    ## D-B -5.6 -13.632087  2.432087 0.3213143
    ## E-B -3.8 -11.832087  4.232087 0.7420265
    ## F-B -0.6  -8.632087  7.432087 0.9999823
    ## G-B -2.6 -10.632087  5.432087 0.9432765
    ## D-C -4.0 -12.032087  4.032087 0.6956130
    ## E-C -2.2 -10.232087  5.832087 0.9743597
    ## F-C  1.0  -7.032087  9.032087 0.9996486
    ## G-C -1.0  -9.032087  7.032087 0.9996486
    ## E-D  1.8  -6.232087  9.832087 0.9907949
    ## F-D  5.0  -3.032087 13.032087 0.4517070
    ## G-D  3.0  -5.032087 11.032087 0.8940253
    ## F-E  3.2  -4.832087 11.232087 0.8622743
    ## G-E  1.2  -6.832087  9.232087 0.9990040
    ## G-F -2.0 -10.032087  6.032087 0.9841066

As *ANOVA* test shows above, there is no combination whose P-value is
smaller than 0.05.

### 5\) Visualize

``` r
par(mfrow=c(2,2))
plot(aov(y2 ~ y1, data = df3))
```

![](Output\\HOMEWO~1/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

No. `11`, `26`, and `33` can be outliers in each data.
