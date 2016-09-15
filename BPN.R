#attempt to model back propogation neural network

#to check output for example in Vijayalakshmi pai 
example=FALSE #enable it to TRUE if data set is example in book

#Reading data set as CSV in R , Assuming last column is output 
data <- read.csv(file="E:\\Workspace\\SoftComputing\\dataset\\dataset.csv",head=FALSE)
#converting the list of vectors as matrix for easy computations
matrix_data <- matrix(unlist(data),ncol=ncol(data))

#getting rows and columns of matrix for aggregating inp and output seperately
nColumns<-ncol(matrix_data)
nRows<-nrow(matrix_data)

input_matrix <- matrix_data[,1:nColumns-1]
output_matrix <- matrix_data [,nColumns,drop=FALSE]

#now we need to normalize the data before proceeding
#BPN best works with normalized data
#we find max value of every column and divide col by it
max_values <- apply(input_matrix,2,max)
if(max(max_values)>1) {     #Normalize data only if required
	for(i in 1:ncol(input_matrix)) {
		input_matrix[,i] = input_matrix[,i]/max_values[i]
	}
}

max_values <- apply(output_matrix,2,max)
if(max(max_values)>1) {      #Normalize data only if required
	for(i in 1:ncol(output_matrix)) {
		output_matrix[,i] = output_matrix[,i]/max_values[i]
	}
}

#setting percentage for training and test data
training_data_percentage = 0.6;
test_data_percentage = 1 - training_data_percentage;

#seperating input into training and test data
training_input_data <- input_matrix[1: floor(nRows*training_data_percentage),,drop=FALSE]
test_input_data <- input_matrix[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]

#seperating output into training and test data
training_output_data <- output_matrix[1: floor(nRows*training_data_percentage),,drop=FALSE]
test_output_data <- output_matrix[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]

#let us define required variables for algo
l <- ncol(input_matrix) 		     #no of neurons in input layer
m <- l    					     #no of neurons in hidden layer , it can lie between l to 2*l
n <- ncol(output_matrix) 		     #no of neurons in output layer
v <- matrix(, nrow = l , ncol = m) 	     #weight vector between input and hidden layer
w <- matrix(, nrow = m , ncol = n) 	     #weight vector between hidden and output layer
delta_v <- matrix(0,nrow = l , ncol = m) #weight change matrix for v
delta_w <- matrix(0,nrow = m , ncol = n) #weight change matrix for w
alpha <- 1
learning_rate <- 0.6


#assign random values to v and w matrix
for(i in 1:nrow(v)) {
	for(k in 1:ncol(v)) {
		v[i,k] <- runif(1,-1,1)
	}
}
for(i in 1:nrow(w)) {
	for(k in 1:ncol(w)) {
		w[i,k] <- runif(1,-1,1)
	}
}


for(iter in 1:nrow(training_input_data)) {
	Ii  <- training_input_data[iter,,drop=FALSE]    #input pattern
	Oi  <- Ii 					 #output of input linear , since linear transfer function
	Ih <- t(v) %*% t(Oi) 			 #calulating input to hidden layer
	Oh <- 1/(1+exp(-1*Ih)) 			 #Output from hidden layer , since sigmoidal function is used
	Io <- t(w) %*% Oh 			 #calculating input for output layer
 	Oo <- 1/(1+exp(-1*Io)) 			 #calculating output of output layer

	#now we need to calculate error , and adjust weights accordingly
	E<-0
	for(j in  1:n) {
	  E <- E+ ((training_output_data[iter][j]-Oo[j])^2)
	}
	
	#let us adjust weights
	d <- (training_output_data[iter]-Oo)*Oo*(1-Oo)
	Y <- Oh * d[1] 				 			 
	delta_w <- (alpha * delta_w )+ (learning_rate * Y) 	
	e <- w * d[1]
	d_star <- e*Oh*(1-Oh) 
	X <- t(Oi) %*% t(d_star)
	delta_v <- (alpha * delta_v) + (learning_rate * X)
	v <- v + delta_v				 #Updating V vector
	w <- w + delta_w				 #Updating W vector
}

#Let us find error rate with test data
for(iter in 1:nrow(test_input_data)) {
	Ii  <- test_input_data[iter,]    #input pattern
	Oi  <- Ii 					 #output of input linear , since linear transfer function
	Ih <- t(v) %*% Oi 			 #calulating input to hidden layer
	Oh <- 1/(1+exp(-1*Ih)) 			 #Output from hidden layer , since sigmoidal function is used
	Io <- t(w) %*% Oh 			 #calculating input for output layer
 	Oo <- 1/(1+exp(-1*Io)) 			 #calculating output of output layer
	#now we need to calculate error , and adjust weights accordingly
	e<-0
 	e <- ((test_output_data[iter][j]-Oo[j])^2)
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