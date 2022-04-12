``` r
rm(list = ls())
cat("\014")
```

 \#\# Loading, summarizing and scaling the data

``` r
library(psych)
```

    ## Warning: package 'psych' was built under R version 4.1.2

``` r
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
data = read.csv("assg1.csv")
summary(data)
```

    ##       Resp             X1            X2              X3              X4     
    ##  Min.   : 1.00   Min.   :0.0   Min.   :0.000   Min.   :0.000   Min.   :0.0  
    ##  1st Qu.: 8.25   1st Qu.:2.0   1st Qu.:2.500   1st Qu.:2.000   1st Qu.:2.5  
    ##  Median :15.50   Median :5.0   Median :6.000   Median :3.000   Median :6.0  
    ##  Mean   :15.50   Mean   :4.4   Mean   :5.133   Mean   :3.667   Mean   :5.4  
    ##  3rd Qu.:22.75   3rd Qu.:7.0   3rd Qu.:7.750   3rd Qu.:5.750   3rd Qu.:8.0  
    ##  Max.   :30.00   Max.   :9.0   Max.   :9.000   Max.   :9.000   Max.   :9.0  
    ##        X5       
    ##  Min.   :0.000  
    ##  1st Qu.:2.000  
    ##  Median :4.000  
    ##  Mean   :4.533  
    ##  3rd Qu.:6.000  
    ##  Max.   :9.000

``` r
describe(data)
```

    ##      vars  n  mean   sd median trimmed   mad min max range  skew kurtosis   se
    ## Resp    1 30 15.50 8.80   15.5   15.50 11.12   1  30    29  0.00    -1.32 1.61
    ## X1      2 30  4.40 2.94    5.0    4.46  2.97   0   9     9 -0.18    -1.53 0.54
    ## X2      3 30  5.13 3.06    6.0    5.29  2.97   0   9     9 -0.46    -1.18 0.56
    ## X3      4 30  3.67 2.75    3.0    3.50  2.97   0   9     9  0.49    -0.94 0.50
    ## X4      5 30  5.40 2.91    6.0    5.58  2.97   0   9     9 -0.48    -1.17 0.53
    ## X5      6 30  4.53 2.85    4.0    4.50  2.97   0   9     9  0.19    -1.19 0.52

``` r
data.scale = scale(data[,2:6])
summary(data.scale)
```

    ##        X1                X2                X3                X4         
    ##  Min.   :-1.4950   Min.   :-1.6778   Min.   :-1.3353   Min.   :-1.8571  
    ##  1st Qu.:-0.8155   1st Qu.:-0.8607   1st Qu.:-0.6070   1st Qu.:-0.9973  
    ##  Median : 0.2039   Median : 0.2833   Median :-0.2428   Median : 0.2063  
    ##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
    ##  3rd Qu.: 0.8834   3rd Qu.: 0.8552   3rd Qu.: 0.7587   3rd Qu.: 0.8942  
    ##  Max.   : 1.5630   Max.   : 1.2638   Max.   : 1.9423   Max.   : 1.2381  
    ##        X5         
    ##  Min.   :-1.5909  
    ##  1st Qu.:-0.8891  
    ##  Median :-0.1872  
    ##  Mean   : 0.0000  
    ##  3rd Qu.: 0.5147  
    ##  Max.   : 1.5675

``` r
describe(data.scale)
```

    ##    vars  n mean sd median trimmed  mad   min  max range  skew kurtosis   se
    ## X1    1 30    0  1   0.20    0.02 1.01 -1.50 1.56  3.06 -0.18    -1.53 0.18
    ## X2    2 30    0  1   0.28    0.05 0.97 -1.68 1.26  2.94 -0.46    -1.18 0.18
    ## X3    3 30    0  1  -0.24   -0.06 1.08 -1.34 1.94  3.28  0.49    -0.94 0.18
    ## X4    4 30    0  1   0.21    0.06 1.02 -1.86 1.24  3.10 -0.48    -1.17 0.18
    ## X5    5 30    0  1  -0.19   -0.01 1.04 -1.59 1.57  3.16  0.19    -1.19 0.18

We scale the 5 variables to make them comparable. The means of variables
are all 0 and standard deviations are all 1. The medians of variables
vary from -.24 to .28.

## Correlation matrix between variables and visualizing the correlation plot

``` r
cor(data.scale)
```

    ##             X1         X2          X3        X4          X5
    ## X1  1.00000000 -0.7107386  0.87895393 0.3432966 -0.03865034
    ## X2 -0.71073859  1.0000000 -0.83182655 0.2573651  0.47015073
    ## X3  0.87895393 -0.8318265  1.00000000 0.1727462 -0.02056604
    ## X4  0.34329663  0.2573651  0.17274619 1.0000000  0.66421386
    ## X5 -0.03865034  0.4701507 -0.02056604 0.6642139  1.00000000

``` r
pairs(data.scale)
```

![](PCA---Github_files/figure-markdown_github/unnamed-chunk-3-1.png)

``` r
corrplot(cor(data.scale), order="hclust")
```

![](PCA---Github_files/figure-markdown_github/unnamed-chunk-3-2.png)
Based on the initial exploratory analysis, it seems like X2 and X3 are
highly negatively correlated. X1 and X3 seem to be highly positively
correlated. X4 and X5 are also relatively positively correlated.
Individuals tend to 1) give X1 and X3 similar ratings and 2) hold
opposite opinions on X2 and X3.

## PCA on the data and results

``` r
data.pc = prcomp(data.scale)
summary(data.pc)
```

    ## Importance of components:
    ##                           PC1    PC2     PC3     PC4     PC5
    ## Standard deviation     1.6345 1.3610 0.58478 0.30601 0.20148
    ## Proportion of Variance 0.5343 0.3704 0.06839 0.01873 0.00812
    ## Cumulative Proportion  0.5343 0.9048 0.97315 0.99188 1.00000

``` r
data.pc$rotation
```

    ##            PC1       PC2        PC3        PC4         PC5
    ## X1 -0.55491712 0.2324895 -0.2795639 -0.7234233  0.19109786
    ## X2  0.57063147 0.2117721 -0.1832902 -0.4621638 -0.61833777
    ## X3 -0.57964069 0.1567520  0.3106045  0.1866707 -0.71282845
    ## X4 -0.01698458 0.6859778 -0.5610983  0.4611287  0.04092556
    ## X5  0.17370927 0.6371534  0.6906101 -0.1248104  0.26709683

The cumulative proportion of PC1 and PC2 is 0.9, so we can reduce the
dimensionality of the original dataset from 5 variables to 2, and still
retain 90% of the variablity in the data set or its explanatory power.

## Five equations that formally define relationship between the original attributes and the factors from the PCA

*P**C*1 =  − 0.555 ⋅ *X*1 + 0.571 ⋅ *X*2 − 0.58 ⋅ *X*3 − 0.017 ⋅ *X*4 + 0.174 ⋅ *X*5
*P**C*2 = 0.232 ⋅ *X*1 + 0.212 ⋅ *X*2 + 0.157 ⋅ *X*3 + 0.686 ⋅ *X*4 + 0.637 ⋅ *X*5
*P**C*3 =  − 0.28 ⋅ *X*1 − 0.183 ⋅ *X*2 + 0.311 ⋅ *X*3 − 0.561 ⋅ *X*4 + 0.691 ⋅ *X*5
*P**C*4 =  − 0.723 ⋅ *X*1 − 0.462 ⋅ *X*2 + 0.187 ⋅ *X*3 + 0.461 ⋅ *X*4 − 0.125 ⋅ *X*5
*P**C*5 = 0.191 ⋅ *X*1 − 0.618 ⋅ *X*2 − 0.713 ⋅ *X*3 + 0.041 ⋅ *X*4 + 0.267 ⋅ *X*5

## Determining how many factors to keep using Scree Plot

``` r
plot(data.pc, type='l', main = "Scree Plot")
```

![](PCA---Github_files/figure-markdown_github/unnamed-chunk-5-1.png) We
should retain 2 factors because that represents the number of factors
before the “kink”. Additionally above the variance of 1, there are 2
factors, which is another way to determine how many factors we should
keep.

## Intuitive names of first 2 factors using the equations that quantify the relationship between the original attributes and the factors from the PCA

PC1’s correlation with X1, X2 and X3 is quite similar in magnitude but
different in direction. It is highest positively correlated for X2 and
negatively correlated of about the same magnitude with X1 and X3. So
these people know what they want when they enter the store, don’t have
many questions, don’t need help from salespeople and do not care if they
are being treated with disrespect. They could be termed Mission-Driven
shoppers

PC2 is highly positively correlated with X4 and X5. This indicates that
these customers do not care about fancy displays in departmental stores
and prefer discount stores for bargaining purposes. These could be
termed Bargain Hunters.

## Plot of all the consumers on a two-dimensional map based on the first two principal components

``` r
biplot(data.pc, xlim=c(-.25,.25), ylim=c(-.25,.25), xlab = "Mission-Driven Shoppers", ylab = "Bargain Hunters")
abline(h = 0, v = 0, col = "gray60")
title("Risha", line = 2)
```

![](PCA---Github_files/figure-markdown_github/unnamed-chunk-6-1.png)

X1 and X3 could be clustered together since they are close together. X4
and X5 could be in another cluster. X2 is further away from all the
others or other segments so in total there would be 3 segments.

## Describing the characteristics of each segment. Also, discussing the relationship between the segment characteristics and the original attributes

The first segment (X1 and X3) has high negative correlation to
“Mission-Driven Shoppers”, so individuals in this segment would enjoy
talking with salespeople. The second segment (X4 and X5) has high
positive correlation to “Bargain Hunters”, so people in the segment are
looking for better deal and cheaper price. The last segment (X2), the
opposite of the first segment, has high positive correlation to
“Mission-Driven Shoppers”, so customers in the segment have their own
plans and prefer not to be interrupted by salespeople.

## Most profitable segment

The most profitable segment will be the first segment with X1 and X3,
because they require assistance and care more about how salespeople
treat them. It would be most likely that customer service is important
to customers in this segment. As a result, salespeople could persuade
customers buy more than they plan to buy and thus create higher revenue.
