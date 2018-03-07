#HW 1
#Brian Hand (A08482072)
#1/18/2016


#PROBLEM 1
num_observations = c(10, 100, 1000)

par(mfrow=(c(3,3)))
for (n in num_observations){
	x = c(rnorm(n,0,2))
	mse = rep(NA,999)                   #generate mse vector
	intercept_est = rep(NA,999)         #generate beta_0 vector
	slope_est = rep(NA,999)             #generate beta_1 vector
	for (i in c(1:999)){  
		e = c(rnorm(n,0,1))               #generate residuals
		y = -1 + 3*x + e
		fit <- lm(y ~ x)
		mse[i] = sum(residuals(fit)^2)/(n-2)
		slope_est[i] = coef(fit)["x"]
		intercept_est[i] = coef(fit)["(Intercept)"]
	}
	
	mse_hist <- hist(mse, main = paste("MSE with \n", n, "observations"), xlim = c(0,3))
	mse_constant <- 999*diff(mse_hist$breaks)[1]*n
	curve(dchisq(x*n, df=n-1)*mse_constant, col="red", lwd=2, add=TRUE, yaxt="n")
	
	beta0_hist <- hist(intercept_est, main = paste("Beta_0 estimates with \n", n, "observations"), xlim = c(-2,0))
  beta0_constant <- 999*diff(beta0_hist$breaks)[1]
  numerator <- t(x)%*%x
	curve(dnorm(x, mean=-1, sd = sqrt(numerator/(4*n^2))) * beta0_constant, col="red", lwd=2, add=TRUE, yaxt="n")
	
	beta1_hist <- hist(slope_est, main = paste("Beta_1 estimates with \n", n, "observations"), xlim = c(2.5,3.5))
	beta1_constant <- 999*diff(beta1_hist$breaks)[1]
	curve(dnorm(x, mean=3, sd = sqrt(1/(4*n))) * beta1_constant, col="red", lwd=2, add=TRUE, yaxt="n")
}	

	
	
	
#PROBLEM 2
load("/Users/brianhand/Dropbox/math282B-Winter16/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names
dat = tmp
str(dat)
attach(dat)

#Find "True" parameter for hp
true_fit= lm(mpg ~ hp + wt + len + wd, data=dat)
beta_1 = coef(true_fit)["hp"]

#create "count" variable to keep track of the number of times true value falls within sample confidence interval
count=rep(NA,1000)

for (i in c(1:1000)){
	sample = dat[sample(nrow(dat), 50), ]
	sample_fit = lm(mpg ~ hp + wt + len + wd, data=sample)
	CI=confint(sample_fit, "hp")
	if (beta_1 > CI[1] & beta_1 < CI[2]) {
		count[i]=1
	} else if (beta_1 < CI[1] | beta_1 > CI[2]) {
	count[i]=0
	}
}
mean(count)
#The proportion is consistently around 0.95, which is expected: a 95% CI would mean that out 1,000 samples, approximately 950 would generate a CI containing the true value of the parameter.

detach(dat)