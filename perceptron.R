perceptron <- function(x, y, lr) {
  # initialize weight vector
  weight <- c(0.5,0,0,0)
  # loop through training data set
  for (j in 1:length(y)){
    z <- 0
    for (i in 1:3) {
      # Predict binary label using step activation function
      z <- z +weight[i+1]*as.numeric(x[i,j])
    }
    z <- z + weight[1]
    if(z <= 0) {
      ypred <- 0
    } else {
      ypred <- 1
    }
    for (i in 1:3){
      weight[i+1] <- weight[i+1] + lr * (y[j] - ypred) *as.numeric(x[i,j])
    }
    weight [1] <- weight[1] + lr * (y[j] - ypred)
    # weight to decide between the two species 
    print(weight)
  }
}

x <- matrix(c(3,2,1,1,1,1,1,2,3),ncol = 3, nrow = 3)
y <- c(0,1,1)

err <- perceptron(x, y,0.1)

