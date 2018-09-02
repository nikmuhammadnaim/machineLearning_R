# Machine Learning with R (2nd Edition)

Below are some of the important R commands I learned from this awesome book by [Brett Lantz](https://www.linkedin.com/in/brettlantz)

## Chapter 10

Use the built-in `confusionMatrix()` from caret for a comprehensive performance evaluation:

```R
library(caret)

myOutput <- confusionMatrix(sms_test_pred, sms_test_labels, positive = "spam")
```  
  
## Sources
* [Machine Learning with R - Second Edition](https://github.com/dataspelunking)
