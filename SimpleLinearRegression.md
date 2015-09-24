# Simple Linear Regression

From Weisberg, S. (2014). Applied Linear Regression , Wiley

### Load the data:

```r
library(car)
Forbes <- read.csv("/home/motech/Downloads/Forbes.csv")
```


### Least Square Criterion

```r
Forbes1 <- Forbes[, c(2, 4)] # select columns 1 and 3
m1 <- lm(lpres ~ bp, data = Forbes1)
```

colMeans function computes the mean of each column of a matrix or data frame

```r
(fmeans <- colMeans(Forbes1))
```

```
##       bp    lpres 
## 202.9529 139.6053
```


Since the sample covariance matrix is just (n − 1) times the matrix of sums of squares and cross-products, we can use the function cov:

```r
(fcov <- (17 - 1) * cov(Forbes1))
```

```
##             bp    lpres
## bp    530.7824 475.3122
## lpres 475.3122 427.7940
```

Alternatively, the function scale can be used to subtract the mean from each column of a matrix or data frame, and then crossprod can be used to get the cross-product matrix:

```r
crossprod(scale(Forbes1, center=TRUE, scale=FALSE))
```

```
##             bp    lpres
## bp    530.7824 475.3122
## lpres 475.3122 427.7940
```


All the regression summaries depend only on the sample means and on the sample sums of squares fcov just computed.

```r
xbar <- fmeans[1]
ybar <- fmeans[2]
SXX <- fcov[1,1]
SXY <- fcov[1,2]
SYY <- fcov[2,2]
betahat1 <- SXY/SXX
betahat0 <- ybar - betahat1 * xbar
print(c(betahat0 = betahat0, betahat1 = betahat1),
digits = 4)
```

```
## betahat0.lpres       betahat1 
##       -42.1378         0.8955
```


### Estimating variance

We can use the summary statistics computed previously to get the RSS and the estimate of σ^2 :

```r
RSS <- SYY - SXY^2/SXX
sigmahat2 <- RSS/15
sigmahat <- sqrt(sigmahat2)
c(RSS=RSS, sigmahat2=sigmahat2, sigmahat=sigmahat)
```

```
##       RSS sigmahat2  sigmahat 
## 2.1549273 0.1436618 0.3790275
```

Using the regression object:

```r
c(RSS = m1$df.residual * sigmaHat(m1)^2,
sigmathat2= sigmaHat(m1)^2,
sigmahat = sigmaHat(m1))
```

```
##        RSS sigmathat2   sigmahat 
##  2.1549273  0.1436618  0.3790275
```


The standard errors of coefficient estimates can be computed from the fundamental quantities already given. The helper function vcov provides the variances and covariances of the estimated coefficients:

```r
vcov(m1)
```

```
##             (Intercept)            bp
## (Intercept) 11.15692867 -0.0549313447
## bp          -0.05493134  0.0002706605
```

The diagonal elements are equal to the squared standard errors of the coefficient estimates and the off-diagonal elements are the covariances between the coefficient estimates. You can turn this or any covariance matrix into a correlation matrix:

```r
cov2cor(vcov(m1))
```

```
##             (Intercept)         bp
## (Intercept)   1.0000000 -0.9996212
## bp           -0.9996212  1.0000000
```


You can also extract the standard errors from the vcov output, but the method is rather obscure. Use the sqrt function for square roots and the diag function to extract the diagonal elements of a matrix. The result is a vector:

```r
(ses <- sqrt(diag(vcov(m1))))
```

```
## (Intercept)          bp 
##  3.34019890  0.01645176
```


### Confidence Intervals and t-Tests

For a 95% interval, the multiplier is:

```r
(tval <- qt(1-.05/2, m1$df))
```

```
## [1] 2.13145
```


The confidence intervals for the two estimates are:

```r
betahat <- c(betahat0, betahat1)
data.frame(Est = betahat,
lower=betahat - tval * ses,
upper=betahat + tval * ses)
```

```
##               Est       lower       upper
## lpres -42.1377793 -49.2572447 -35.0183139
##         0.8954937   0.8604276   0.9305598
```


R includes a function called confint that computes the confidence intervals and is the preferred way to compute confidence intervals:

```r
confint(m1, level=.95)
```

```
##                   2.5 %      97.5 %
## (Intercept) -49.2572447 -35.0183139
## bp            0.8604276   0.9305598
```






