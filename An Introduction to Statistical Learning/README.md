# An Introduction to Statistical Learning - with Applications in R

Below are some of the important R commands I learned from the lab section of this brilliant book by Gareth James, Daniela Witten, Trevor Hastie and Robert Tibshirani.

A free pdf copy of this book is available online [here](http://www-bcf.usc.edu/~gareth/ISL/)

## Lab

### Chapter 3

Use `confint()` to get the confidence interval of our linear regression coefficients

```R
lm.boston <- lm(medv ~ lstat, data = Boston)

confint(lm.boston, level = 0.99)
```  
  
## Source
* [An Introduction to Statistical Learning - with Applications in R](http://www-bcf.usc.edu/~gareth/ISL/)
