# Question 3
library(foreign)
data = read.dta("nsw.dta")

# Linear regression model
fit = lm(data$re78~data$treat)
set.seed(1)
# Bootstrap function
bootstrap = function(d){
  d_bootstrap = sample(1:nrow(d),nrow(d),replace=TRUE)
  return(d_bootstrap)
}

# Start main program
result_coef = list()

# We will bootstrap 10000 times, each with a sample size of 10000
for(i in (1:100000)){
  d_bootstrap = data[bootstrap(data),]
  # Build a linear regression model based on bootstrap data
  temp_lm = lm(d_bootstrap$re78~d_bootstrap$treat)
  # Store the result of bootstrap sample coefficient
  result_coef = append(result_coef,as.numeric(temp_lm$coefficients[2]))
}
result_coef = unlist(result_coef)
# Get the 95% confidence interval
quantile(result_coef,c(0.025,0.975))
# Compare with the confidence interval getting from analytical method
confint(fit)

# Histogram showing bootstrap sample result
library(ggplot2)
hist = ggplot() + aes(result_coef) + geom_histogram(color="dark blue",fill="white") + xlab("Coefficient value")+
  ylab("Frequency") +
  ggtitle("Distribution of bootstrap-sample results")
