Dataframe = read.csv("pima-indians-diabetes.csv", header = FALSE)

#Plasma glucose as a function of age. 
#Plasma = col V2 (Y) 
#Age = col V8 (X) 
#xlab, ylab = labels
#main = Title
#xlim,Ylim = 

input <- Dataframe[, c('V8', 'V2')]
diabetes
ggplot(input, col=2)
