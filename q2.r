### Question 2
library(arm)
data(lalonde)
library(ggplot2)

# Using only control group 
control = lalonde[which(lalonde$treat==0),]

set.seed(157) 
# Build linear regression model
lm <- lm(control$re78 ~ control$age + control$educ + control$re74 + control$re75 + control$educ*control$re74 + control$educ*control$re75 + control$age*control$re74 + control$age*control$re75 + control$re74*control$re75)
# Simulate 10000 set of coefficients
sim_result = sim(lm,n.sims=10000)

### Consider first scenario in a), letting only age varies and hold other at the median
### store result in a matrix shape (nrow(control),10000)
result = matrix(rep(0,10000*39),nrow=39,ncol=10000)


# Create a temporary storage for the result
# A for loop to calculate simulated prediction given the change in age in the input
for(age_value in (17:55)){
  # X is an input vector of shape (10,1). In this input vector, we only let age varies and hold other parameters
  # at their median
  X = matrix(c(1,age_value,median(control$educ),median(control$re74),median(control$re75),median(control$educ)*median(control$re74),median(control$educ)*median(control$re75),age_value*median(control$re74),age_value*median(control$re75),median(control$re74)*median(control$re75)),10,1)
  # Matrix multiplication of sim_result@coef with X and then add the simulated sigma
  # dim(sim_result@coef) is (10000,10) so (10000,10) x (10,1) = (10000,1)
  # This will be the 10000 predictions for a given age
  # Storage has shape (10000,10) x (10,1) = (10000,1)
  result[age_value-16,] = sim_result@coef %*% X + rnorm(nrow(sim_result@coef),0,sim_result@sigma)
  # Store it in the result matrix

}
# Get the 95% confident interval for each unit
lower_bound = matrix(rep(0,39,nrow=39,ncol=1))
upper_bound = matrix(rep(0,39,nrow=39,ncol=1))
for (i in (1:39)){
  lower_bound[i] = quantile(result[i,],0.025)
  upper_bound[i] = quantile(result[i,],0.975)
}
Age_Value = seq(17,55,by=1)
df <- data.frame(Age_Value,lower_bound,upper_bound)

# Plotting the prediction interval for each age
plot(x=c(1:100),y=c(1:100),type="n",xlim=c(17,55),ylim=c(-8000,20000),
     main = "Simulated earnings in 1978 with 95% confidence interval",
     xlab = "Age",
     ylab = "Prediction",
     cex.main = 0.9)
mtext("(Holding educ, re74, re75 at their medians)",side = 3)

for(age in (17:55)){
  segments(
    x0 = age,
    y0 = lower_bound[age-16],
    x1 = age,
    y1 = upper_bound[age-16],
    lwd = 2
  )
}

########### When holding other parameteres at their 90% #############
result1 = matrix(rep(0,10000*39),nrow=39,ncol=10000)


# Create a temporary storage for the result
i = 1
# A for loop to calculate simulated prediction given the change in age in the input
for(age_value in (17:55)){
  # X is an input vector of shape (10,1). In this input vector, we only let age varies and hold other parameters
  # at their median
  X = matrix(c(1,age_value,quantile(control$educ,0.9),quantile(control$re74,0.9),quantile(control$re75,0.9),quantile(control$educ,0.9)*quantile(control$re74,0.9),quantile(control$educ,0.9)*quantile(control$re75,0.9),age_value*quantile(control$re74,0.9),age_value*quantile(control$re75,0.9),quantile(control$re74,0.9)*quantile(control$re75,0.9)),10,1)
  # Matrix multiplication of sim_result@coef with X and then add the simulated sigma
  # dim(sim_result@coef) is (10000,10) so (10000,10) x (10,1) = (10000,1)
  # This will be the 10000 predictions for a given age
  # Storage has shape (10000,10) x (10,1) = (10000,1)
  result1[i,] = sim_result@coef %*% X + rnorm(nrow(sim_result@coef),0,sim_result@sigma)
  # Store it in the result matrix
  i = i +1 
  
}
# Get the 95% confident interval for each unit
lower_bound1 = matrix(rep(0,39,nrow=39,ncol=1))
upper_bound1 = matrix(rep(0,39,nrow=39,ncol=1))
for (i in (1:39)){
  lower_bound1[i] = quantile(result1[i,],0.025)
  upper_bound1[i] = quantile(result1[i,],0.975)
}
Age_Value = seq(17,55,by=1)
df1 <- data.frame(Age_Value,lower_bound1,upper_bound1)

# Plot the prediction interval
plot(x=c(1:100),y=c(1:100),type="n",xlim=c(17,55),ylim=c(-8000,20000),
     main = "Simulated earnings in 1978 with 95% confidence interval",
     xlab = "Age",
     ylab = "Prediction",
     cex.main=0.9)
mtext("(Holding educ, re74, re75 at their 90%)",side = 3)

for(age in (17:55)){
  segments(
    x0 = age,
    y0 = lower_bound1[age-16],
    x1 = age,
    y1 = upper_bound1[age-16],
    lwd = 2
  )
}
