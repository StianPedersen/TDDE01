################################################################################
#                     ONLY ALLOWED PACKAGE                                     #
#                 install.packages("neuralnet")                                #
################################################################################

################################################################################
#                             TASK 1                                           #
################################################################################
library(neuralnet)
set.seed(1234567890)
Variable <- runif(500, 0, 10)
mydata <- data.frame(Variable, Sin=sin(Variable))
train_data <- mydata[1:25,] # Training
test_data <- mydata[26:500,] # Test
# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
winit <- unlist(cbind(c(runif(10, -1, 1)), c(runif(10, -1, 1))))# WeightInit
  nn <- neuralnet(Sin ~ Variable,
                  data = train_data, 
                  hidden = c(10),
                  startweights = winit
                  ) #Add more here
    # Plot of the training data (black), test data (blue), and predictions (red)
    plot(train_data, cex=2)
    points(test_data, col = "blue", cex=1)
    points(test_data[,1],predict(nn,test_data), col="red", cex=1)

################################################################################
#                             TASK 2                                           #
################################################################################
linear <- function(x) {
  x
  }
relu <- function(x) { #APPROXIMATION OF RELU BCUS LIFE IS PAIN.
  x / (1 + exp(-2 * 1 * x))
  }
softplus <- function(x) {
  log(1 + exp(x))
  }

nn_linear <- neuralnet(Sin ~ Variable,
                 data = train_data, 
                 hidden = c(10),
                 startweights = winit,
                 act.fct = linear) #Add more here


nn_ReLU <- neuralnet(Sin ~ Variable,
                 data = train_data, 
                 hidden = c(10),
                 startweights = winit,
                 act.fct = relu) #Add more here

nn_soft <- neuralnet(Sin ~ Variable,
                data = train_data, 
                hidden = c(10),
                startweights = winit,
                act.fct = softplus
) #Add more here
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train_data, cex=2)
points(test_data, col = "blue", cex=1)
points(test_data[,1],predict(nn_linear,test_data), col="red", cex=1)
points(test_data[,1],predict(nn_ReLU,test_data), col="green", cex=1)
points(test_data[,1],predict(nn_soft,test_data), col="yellow", cex=1)


################################################################################
#                             TASK 3                                           #
################################################################################
set.seed(1234567890)
Variable2 <- runif(500, 0, 50)
mydata2 <- data.frame(Variable = Variable2, Sin=sin(Variable2))
predict(nn,mydata2)
plot(mydata2, col = "black", cex=1)
points(mydata2[,1], predict(nn,mydata2),  col = "blue", cex=1)
plot(mydata2, col = "black", cex=1, ylim = c(-12,1))
points(mydata2[,1], predict(nn,mydata2), col = "blue", cex=1)
################################################################################
#                             TASK 4                                           #
################################################################################

nn$weights #USED To EXPLAIN WHY IT CONVERGES TO -12

################################################################################
#                             TASK 5                                           #
################################################################################

set.seed(1234567890)
Variable <- runif(500, 0, 10)
nn_var_from_sin <- neuralnet(Variable ~ Sin,
                data = train_data, 
                hidden = c(10),
                startweights = winit) #Add more here
plot(x = train_data[,2], y = train_data[,1],
     col = "black", ylim = c(0,10),
     xlab = "Sinus value",
     ylab = "X value")
points(train_data[,2],predict(nn_var_from_sin, train_data), col = "blue")
