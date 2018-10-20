### Question 4

library(foreign)

data = read.dta(file = "nsw.dta")
r_square_func = function(ys,ys_predict){
  # Residual sum square
  rss = sum((ys - ys_predict)^2)
  # Total sum square: total variance in the data
  tss = sum((ys - mean(ys))^2)
  return(1 - rss/tss)
}

# Test function above
fit <- lm(data$re78~data$treat)
pred_y = predict(fit,data)
# This is the result from our function: 0.004871571
r_square_func(data$re78,pred_y)
# This is the result from R: 0.004871571
summary(fit)$r.squared
# The are the same. We have correctly implemented a function that calculate R^2