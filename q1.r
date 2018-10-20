x = seq(1,99,by =1)
y = 2*x + rnorm(99,0,20)
fit = lm(y~x)
summary(fit)
plot(x,y,xlab="x", ylab="y",pch=16,col="blue",main="Plot of regression line for x and y variables (without outlier)",cex.main=0.85)
regress_line1 = abline(fit,col="red")
legend("topleft",legend="Regression line",col="red",lty=1)

x_outlier = x
x_outlier[100] = 180
y_outlier = y
y_outlier[100] = -7*180 + rnorm(1,50,20)
fit_outlier = lm(y_outlier~x_outlier)
summary(fit_outlier)
plot(x_outlier,y_outlier,main="Plot of regression line for x and y variables (with outlier)",cex.main=0.85)
regress_line2 = abline(lm(y_outlier~x_outlier),col='red')
legend("bottomleft",legend="Regression line",col="red",lty=1)


x_outlier = x
x_outlier[100] = 180
y_outlier = y
y_outlier[100] = -7*180 + rnorm(1,50,20)
fit_outlier = lm(y_outlier~x_outlier)
summary(fit_outlier)
plot(x_outlier,y_outlier,main="Plot of regression line for x and y variables")
regress_line1 = abline(fit,col="red")
regress_line2 = abline(lm(y_outlier~x_outlier),col='blue')
legend("bottomleft",legend=c("Regression line without outlier", "Regression line with outlier"),col=c("red","blue"),lty=1)
