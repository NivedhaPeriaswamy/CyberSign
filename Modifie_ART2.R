	#Implementation of ART2 Algorithm for Signature Verification Problem.

	#This function helps to normalize the given data set.
	normalize_data <- function(data_set) {
	  for(feature in names(data_set)){
	      #Here we find the max and the min value for the particular feature.
	      min_value = min(data_set[[feature]]);
	      max_value = max(data_set[[feature]]);
	      #This where we normalize each column in the range 0 to 1.
	      row = 0;
	      while(row <= length(data_set[[feature]])) {
	        data_set[[feature]][row] = (data_set[[feature]][row] - min_value) / (max_value - min_value);
	        row = row + 1;
	      }
	  }
	  data_set
	}

	#Activation function to activate only the required data and supress the other.
	activation_function <- function(X)	{
		temp <- matrix(0, nrow=1, ncol=no_of_features)
		for(i in 1:no_of_features){
			x <- X[1, i]
			if(x < parameters$noise_suprresion)
				temp[1, i] <- 0
			else
				temp[1, i] <- x
		}
		return (temp)
	}

	update_values <- function(data, f_parameters, flag)	{
/
		f_parameters$w <- data + (fixed_weights$a * f_parameters$u)	

		#Update the value of  "p" only if the reset value is set to true.
		if(reset == TRUE) 
			f_parameters$p <- f_parameters$u
		else
			f_parameters$p <- f_parameters$u + (fixed_weights$d * input_weight_vector$top_down[max_index, ])


		if(flag == TRUE){
			norm_data <- norm(data, type="2")
			f_parameters$x <- as.matrix(data) / (parameters$e + norm_data)
			f_parameters$v <- activation_function(f_parameters$x)
		}
		else	{
			norm_w <- norm(f_parameters$w, type="2") 
			f_parameters$x <- f_parameters$w / (parameters$e + norm_w)
		}
	  	norm_p <- norm(f_parameters$p, type="2")
	  	f_parameters$q <- f_parameters$p / (parameters$e + norm_p)
	  	if(flag == FALSE)
	  		f_parameters$v <- activation_function(f_parameters$x) + (fixed_weights$b * activation_function(f_parameters$q))
	  	return (f_parameters)
	}

	#Here we check for the reset value and update the cluster unit vector.
	check_for_reset <- function(data, f_parameters, y)	{
		max_index <- which.max(y)
		t <- input_weight_vector$top_down[max_index, ]
		norm_v <- norm(f_parameters$v, type="2")
		f_parameters$u <- f_parameters$v / (parameters$e + norm_v)
		f_parameters$p <- f_parameters$u +(fixed_weights$d * t)
		norm_u <- norm(f_parameters$u, type="2")
		norm_p <- norm(f_parameters$p, type="2")
		reset_value <- ( f_parameters$u + (fixed_weights$c * f_parameters$p)) / (parameters$e + norm_u + (fixed_weights$c * norm_p))
		norm_reset_value <- norm(reset_value, type="2")  
		if(norm_reset_value < (parameters$vigilance - parameters$e)) {
			reset <- FALSE
			y[max_index,1] <- -1
		}
		else {
			reset <- FALSE
			f_parameters <- update_values(data, f_parameters, FALSE)  
		}
		return (list(f_parameters = f_parameters, y = y,reset = reset))
	}

	#Here we perform the major operations.
	training_algorithm <- function(input_vector, f_parameters, is_update)	{
		max_ys =  matrix(0, nrow=nrow(input_vector), ncol=1)
		#loop through each input_vector and calculate the parameters value.
		for(row in 1:nrow(input_vector))	{
			reset <- TRUE
			data <- input_vector[row, ]
			flag <- 0
			f_parameters <- update_values(t(data), f_parameters, TRUE)
			#print("f_parameters")
			#print(f_parameters)
		  	#Update the values again.
		  	norm_v <- norm(f_parameters$v, type="2")
		  	f_parameters$u <- f_parameters$v / (norm_v + parameters$e)
			f_parameters <- update_values(t(data), f_parameters, FALSE)

			#Computing Signals for Cluster Units
			y <- matrix(0, nrow=no_of_clusters, ncol= 1)
			for(col in 1 : ncol(input_weight_vector$bottom_up))
				y[col, 1] <- sum(input_weight_vector$bottom_up[ , col] * f_parameters$p)

			#update the parameters until reset value is set to false.
			r = 0
			while(reset)	{
				outputs <- check_for_reset(t(data), f_parameters, y)
				f_parameters <- outputs$f_parameters
				y <- outputs$y
				reset <- outputs$reset
			}

			if(is_update)	{
				N_IT <- 1	
				i <- 0
				while(i < N_IT)	{
					#Here we update the weights for the winning unit.
					max_index <- which.max(y)
					temp <- (parameters$learning_rate * fixed_weights$d * f_parameters$u )
					input_weight_vector$top_down[max_index, ] <-  temp + ( 1 + (parameters$learning_rate * fixed_weights$d * (fixed_weights$d - 1))) * input_weight_vector$top_down[max_index, ]
					input_weight_vector$bottom_up[, max_index] <- t(temp) + ( 1 + (parameters$learning_rate * fixed_weights$d * (fixed_weights$d - 1))) * input_weight_vector$bottom_up[ , max_index]
					f_parameters <- update_values(t(data), f_parameters, FALSE)
					i <- i + 1
				}
			}
			max_ys[row] <- which.max(y) -1
		}
		return (list(f_parameters = f_parameters, max_ys = max_ys))
	}

	#Here we initialize the reaquired data.
	fixed_weights <- list()
	fixed_weights$a <- 10
	fixed_weights$b <- 10
	fixed_weights$c <- 0.1
	fixed_weights$d <- 0.9

	#Required f_parameters.
	parameters <- list()
	parameters$e <- 0.1
	parameters$vigilance <- 0.905
	parameters$noise_suprresion <- 0.06
	parameters$learning_rate <- 0.6

	no_of_clusters <- 2
	no_of_features <- 256

	#Weight Vectors.
	input_weight_vector <- list()
	temp <- ( 1 / (1- fixed_weights$d) * sqrt(no_of_clusters) ) / no_of_clusters
	input_weight_vector$bottom_up <- matrix(temp, nrow=no_of_features, ncol=no_of_clusters)
	input_weight_vector$top_down <- matrix(0, nrow=no_of_clusters, ncol=no_of_features)

	data <- read.csv(file="E:\\Workspace\\SoftComputing\\dataset\\dataset.csv",head=FALSE)
	temp <- normalize_data(data)
	matrix_data <- matrix(unlist(temp), ncol=ncol(temp))
	matrix_data <- matrix_data[sample(nrow(matrix_data)),]
	input_vector <- matrix_data[, 1:ncol(matrix_data)-1]

	nColumns <- ncol(matrix_data)
	nRows <- nrow(matrix_data)

	output_vector <- matrix_data[,ncol(data), drop=FALSE]

	#Here we adjust the weight vector.
	 for(i in 1: nrow(input_vector))	
	 	for(j in 1:ncol(input_vector))	
	 		if(input_vector[i, j] < 1/sqrt(nrow(input_vector)) )
	 			input_vector[i, j] = 0

	reset <- TRUE

	training_data_percentage = 0.6;
	test_data_percentage = 1 - training_data_percentage;

	training_input_data <- input_vector[1: floor(nRows*training_data_percentage),,drop=FALSE]
	test_input_data <- input_vector[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]

	#seperating output into training and test data
	training_output_data <- output_vector[1: floor(nRows*training_data_percentage),,drop=FALSE]
	test_output_data <- output_vector[floor(nRows*training_data_percentage)+1:floor(nRows*test_data_percentage),,drop=FALSE]


	x <- w <- q <- p <- u <-v <- matrix(0, nrow=1, ncol=no_of_features)

	t = training_algorithm(training_input_data, list (u = u , v = v, w = w, x = x, p = p , q = q), TRUE)
	t = training_algorithm(test_input_data, list (u = u , v = v, w = w, x = x, p = p , q = q), FALSE)

	count = 0
	for(row in 1:nrow(test_input_data))
		if(t$max_ys[row,1] != matrix_data[row,1])	{
			count = count + 1
		}

	accuracy = (count / nrow(test_input_data)) * 100
	cat("Accuracy:", accuracy)