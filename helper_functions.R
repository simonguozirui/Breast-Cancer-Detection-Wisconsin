# Function to generate scatterplot and calculate correlation 
plot_scatterplot <- function(real, imputed) {
	# create plot data
	plot_data <- data.frame(real = real, imputed = imputed);
	# create scatterplot
	print(ggplot(plot_data, aes(x=real, y=imputed)) + geom_point())
	# print correlation 
	print(paste("Correlation:", cor(real, imputed)))
}

# function to generate NAs
generate_NAs <- function(dataset, feature, noNA) {
    # find indexes to change to NA
    idx <- sample(1:nrow(dataset), noNA*nrow(dataset), replace = FALSE)
    # convert to NAs in dataset
    dataset[idx,feature] <- NA
    return(dataset)
}

# impute missing values using mean imputation 
mean_imputation_solution <- function(dataset, feature) {
	# find mean
  	meanv <- mean(dataset[,feature], rm.na = TRUE)
  	# replace NAs with mean
  	dataset[is.na(dataset[,feature]),feature] <- meanv
  	return(dataset)
}

# impute missing values using random imputation 
random_imputation_solution <- function(dataset, feature) {
	# find NAs
  	idx <- is.na(dataset[,feature])
  	numNA <- length(idx)
  	# sample from observed values number of NAs present
  	samples <- sample(dataset[!is.na(dataset[,feature]),feature], numNA)
  	dataset[idx,feature] <- samples
  	return(dataset)
}

# impuate missing values using knn features
knn_imputation_solution <- function(dataset, k) {
    dataset_new <- impute.knn(t(dataset[,-(1,2)]), k = k)
  	dataset_new <- as.data.frame(t(dataset_new$data))
    return(dataset_new)
}