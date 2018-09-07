# R Deep Learning Projects

Below are some of the important concepts and R commands I learned from this cool book written by Yuxi (Hayden) Liu and 	Pablo Maldonado

The R codes from the authors can be found at the publisher's [github](https://github.com/PacktPublishing/R-Deep-Learning-Projects).

The R codes in this git are based on the book with me trying to use tidyverse concept as much as possible. 

This git main purpose is for personal revision when I am on the go or when I need quick refresher.

## Chapter 1  
  
### Concepts

* Backpropagation:
The idea of backpropagation is:
  1) Travel through the network and compute all outputs of the hidden and output layers
  2) Then move backward from the output layer and calculate how much each node contributed to the error in the final output and propogate it back to the previous layers. 


### R  
  
Use `nnet()` to create single layer neural network. 
  - size    = number of hidden neurons
  - maxit   = maximum number of iterations
  - MaxNWts = maximum number of weights

```R
model_nn <- nnet(label ~ ., 
                 data = digits_train, 
                 size = 50, 
                 maxit = 500,
                 MaxNWts = 100000,
                 decay = 1e-4)	
```  

`nnet` can only create neural network with single hidden layer. 
  
## Source
* [R Deep Learning Projects](https://www.packtpub.com/big-data-and-business-intelligence/r-deep-learning-projects?utm_source=github&utm_medium=repository&utm_campaign=9781788478403)
