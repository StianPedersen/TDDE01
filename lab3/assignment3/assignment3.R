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
linear <- function(x) x
relu <- function(x) {
  if (x < 0)
    return(0)
  else 
    return (x)}
softplus <- function(x) log(1 + exp(x))

nn_linear <- neuralnet(Sin ~ Variable,
                 data = train_data, 
                 hidden = c(10),
                 startweights = winit,
                 act.fct = linear
) #Add more here
nn_ReLU <- neuralnet(Sin ~ Variable,
                 data = train_data, 
                 hidden = c(10),
                 startweights = winit,
                 act.fct = relu
) #Add more here
nn_soft <- neuralnet(Sin ~ Variable,
                data = train_data, 
                hidden = c(10),
                startweights = winit,
                act.fct = softplus
) #Add more here
# Plot of the training data (black), test data (blue), and predictions (red)
plot(train_data, cex=2)
points(test_data, col = "blue", cex=1)
points(test_data[,1],predict(nn,test_data), col="red", cex=1)



################################################################################
#                             TASK 3                                           #
################################################################################



################################################################################
#                             TASK 4                                           #
################################################################################





################################################################################
#                             TASK 5                                           #
################################################################################