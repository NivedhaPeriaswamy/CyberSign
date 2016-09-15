#Here we assign the random weights.
assign_weights <- function(i_vector) {
	for(i in 1:nrow(i_vector)) {
		for(k in 1:ncol(i_vector)) {
			i_vector[i,k] <- runif(1,-1,1)
		}
	}
	return (i_vector)
} 

data <- read.csv(file="E:\\Workspace\\SoftComputing\\dataset\\dataset.csv",head=FALSE)
matrix_neurons <- matrix(unlist(data), ncol=ncol(data))
matrix_neurons <- matrix_neurons[sample(nrow(matrix_neurons)),]
nColumns <- ncol(matrix_neurons)
nRows <- nrow(matrix_neurons)

input_neurons <- matrix_neurons[, 1:ncol(data)-1]
output_neurons <- matrix_neurons[, ncol(data),drop=FALSE]

#now we need to normalize the data before proceeding
#BPN best works with normalized data
#we find max value of every column and divide col by it
max_values <- apply(input_neurons,2,max)
if(max(max_values)>1) {     #Normalize data only if required
	for(i in 1:ncol(input_neurons)) {
		input_neurons[,i] = input_neurons[,i]/max_values[i]
	}
}	

max_values <- apply(output_neurons,2,max)
if(max(max_values)>1) {      #Normalize data only if required
	for(i in 1:ncol(output_neurons)) {
		output_neurons[,i] = output_neurons[,i]/max_values[i]
	}
}

training_data_percentage = 0.6;
test_data_percentage = 1 - training_data_percentage;

#seperating input into training and test data
training_input_data <- input_neurons[1: floor(nRows*training_data_percentage),,drop=FALSE]
test_input_data <- input_neurons[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]

#seperating output into training and test data
training_output_data <- output_neurons[1: floor(nRows*training_data_percentage),,drop=FALSE]
test_output_data <- output_neurons[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]

centre_vector <- matrix(0, nrow = ncol(input_neurons), ncol = ncol(input_neurons))
weight_vector <- matrix(0, nrow = nrow(input_neurons), ncol = ncol(input_neurons))
centre_vector <- assign_weights(centre_vector)
weight_vector <- assign_weights(weight_vector)
learning_rate <- 0.6
alpha <- 1

output_hidden_layer <- matrix(0, nrow = ncol(input_neurons), ncol = 1)
w = matrix(0, nrow=ncol(input_neurons), ncol = ncol(output_neurons))

error <- 0
for( i in 1 : nrow(training_input_data))
{
	data <- t(as.matrix(training_input_data[i, ]))
	input_hidden_layer <- input_input_layer <- output_input_layer <- data	
	
	#Compute the output of the output hidden layer.
	for (row in 1:nrow(centre_vector)) 
		output_hidden_layer[row, ] <- exp(-norm( (input_hidden_layer[1, ] - centre_vector[row, ]), type="2"))

	input_output_layer <- t(weight_vector[i, ]) %*% output_hidden_layer
	output_output_layer <- input_output_layer
	error <- 0
	error <- error + ((training_output_data[i, 1] - output_output_layer[1])^2)
	print("output_output_layer")
	print(output_output_layer)
	print("test_output_data")
	print(training_output_data[i, 1])
	d <- (training_output_data[i]- output_output_layer) * output_output_layer * (1 - output_output_layer)
	y <- output_hidden_layer %*% as.matrix(d)
	delta_w <- alpha * weight_vector[i, ] + learning_rate * y
	e <- weight_vector[i,] %*% as.matrix(d)
	d_star <- e * output_hidden_layer * (1 - output_hidden_layer)
	X <- output_hidden_layer %*% t(d_star) 
	w <- w + delta_w				 
	cat("iteration number", i, "\n")
	cat("Error", error,"\n")	
}


#Let us find error rate with test data

output <- matrix(0, nrow=nrow(test_input_data), ncol = 1)

for(i in 1:nrow(test_input_data)) {
	data <- t(as.matrix(test_input_data[i, ]))
	input_hidden_layer <- input_input_layer <- output_input_layer <- data	

	#Compute the output of the output hidden layer.
	for (row in 1:nrow(centre_vector)) 
		output_hidden_layer[row, ] <- exp(-norm( (input_hidden_layer[1, ] - centre_vector[row, ]), type="2"))

	input_output_layer <- t(weight_vector[i, ]) %*% output_hidden_layer
	output_output_layer <- input_output_layer

	e <- 0
	e <- ((test_output_data[i][1] - output_output_layer[1])^2)
	print(e)
	if(e > 1)
		output[i] = 1
	else
		output[i] = -1

}

count <- 0

for(i in 1:nrow(test_output_data))
	if(output[i] == test_output_data[i])
		count = count + 1 

accuracy  <-  (count / nrow(test_output_data)) * 100
cat("Accuracy:", accuracy)