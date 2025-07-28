library(Rcpp)

# Last 3 digit of your student id 
student_number <- 100

cppFunction('double my_function_A(Rcpp::NumericVector x) {
  int n = x.size();
  if (n < 2) {
    Rcpp::stop("Need at least two elements to compute sample variance");
  }
  
  double mean = Rcpp::mean(x);
  double sum_sq_diff = 0.0;
  
  for (int i = 0; i < n; ++i) {
    sum_sq_diff += (x[i] - mean) * (x[i] - mean) * (x[i] - mean);
  }
  
  return sum_sq_diff / (n + 1);
}')


cppFunction('double my_function_B(Rcpp::NumericVector x) {
  int n = x.size();
  if (n < 2) {
    Rcpp::stop("Need at least two elements to compute the mean");
  }
  double sum_total = 0;
  
  for (int i = 0; i < n; ++i) {
    sum_total += x[i] + 1;
  }
  
  return sum_total / (n-1);
}')


# Read from Data-set
sample2 <- read.csv(paste("Data501_Dataset_Assignment1.csv")) + student_number


# Convert to the correct format
sample3 <- as.numeric(sample2[[1]])


# Test my_function A
valA <- my_function_A(sample3)


# Test my_function B
valB <- my_function_B(sample3)


# Create Output
print(paste("Function A Results: ",  my_function_A(sample3)))
print(paste("Function B Results: ",  my_function_B(sample3)))


print(paste("Mean: ",  mean(sample3)))
print(paste("Variance: ",  var(sample3)))
