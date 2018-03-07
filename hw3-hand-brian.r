#HW 3
#Brian Hand (A08482072)
#2/15/2016


#######PROBLEM 1
###PART A.
detach(dat)
load("/Users/brianhand/Dropbox/math282B-Winter16/04cars.version2.rda")
tmp = dat[,c(2,3,7,8,10,11)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
summary(tmp)
names(tmp) = c("type", "drive", "cyl", "hp","mpg","wt") # abbreviate names
dat = tmp
dat = subset(tmp, (cyl==4)|(cyl==6)|(cyl==8))
attach(dat)
summary(dat)

fit1 = lm(mpg ~ poly(hp, degree = 2, raw = TRUE))
pts = c(seq(min(hp), max(hp), length.out = 1000))
fitted1 = predict(fit1, data.frame(hp = pts))
plot(hp, mpg)
lines(pts, fitted1, col="red")


###PART B.
summary(drive) #drive is 0, 1, or 2
drive = as.factor(drive)
fit2 = lm(mpg ~ drive + poly(hp, degree = 2, raw = TRUE))
summary(fit2)

#Generate fitted values for plot
fitted2_drive0 = predict(fit2, data.frame(drive = "0", hp = pts))
fitted2_drive1 = predict(fit2, data.frame(drive = "1", hp = pts))
fitted2_drive2 = predict(fit2, data.frame(drive = "2", hp = pts))

plot(hp, mpg)
lines(pts, fitted2_drive0, col="red", lwd=2)
lines(pts, fitted2_drive1, col="blue", lwd=2)
lines(pts, fitted2_drive2, col="yellow", lwd=2)
legend(300, 45, legend = c("Forward", "Backward", "All-Wheel"), col = c("red", "blue", "yellow"), lwd=2)
#Test if model with Drive factor variables signifcantly enhances the model
anova(fit1,fit2)
#Very small p-value, so reject H0 (hp degree 2 polynomial only) in favor of H1 (drive factor variable with hp degree 2 polynomial)



###PART C.
fit3= lm(mpg ~ drive +  drive* hp + poly(hp, degree = 2, raw = TRUE))
summary(fit3)
#Generate fitted values for plot
fitted3_drive0 = predict(fit3, data.frame(drive = "0", hp = pts))
fitted3_drive1 = predict(fit3, data.frame(drive = "1", hp = pts))
fitted3_drive2 = predict(fit3, data.frame(drive = "2", hp = pts))
plot(hp, mpg)
lines(pts, fitted3_drive0, col="red", lwd=2)
lines(pts, fitted3_drive1, col="blue", lwd=2)
lines(pts, fitted3_drive2, col="yellow", lwd=2)
legend(300, 45, legend = c("Forward", "Backward", "All-Wheel"), col = c("red", "blue", "yellow"), lwd=2)
#Test new model vs old model
anova(fit2, fit3)
#P-Value 0.0003489, reject H0 in favor of enhanced model in fit3


###PART D.
fit4= lm(mpg ~ drive +  poly(hp, degree = 2, raw = TRUE) + drive*poly(hp, degree = 2, raw = TRUE))
summary(fit4)
#Generate fitted values for plot
fitted4_drive0 = predict(fit4, data.frame(drive = "0", hp = pts))
fitted4_drive1 = predict(fit4, data.frame(drive = "1", hp = pts))
fitted4_drive2 = predict(fit4, data.frame(drive = "2", hp = pts))
plot(hp, mpg)
lines(pts, fitted4_drive0, col="red", lwd=2)
lines(pts, fitted4_drive1, col="blue", lwd=2)
lines(pts, fitted4_drive2, col="yellow", lwd=2)
legend(300, 45, legend = c("Forward", "Backward", "All-Wheel"), col = c("red", "blue", "yellow"), lwd=2)
#Test new model vs old model
anova(fit3, fit4)
#P-Value 0.014, reject H0 in favor of enhanced model in fit4

detach(dat)


#######PROBLEM 2
detach(dat)
dat = read.table("http://users.stat.umn.edu/~gary/book/fcdae.data/exmpl8.10", header=TRUE)
tmp = as.data.frame(dat)
dat=tmp
attach(dat)

atemp = as.factor(atemp)
gtemp = as.factor(gtemp)
variety = as.factor(variety)

###PART A.
fitA = lm(y ~ gtemp:variety)
anova(fitA)
#P-value is 0.0008363

require(car)
###PART B.
fitB = lm(y ~ atemp + gtemp + variety + atemp:gtemp + atemp:variety + gtemp:variety)
Anova(fitB, type = 2)
#P-value is 0.0001557


###PART C.
fitC = lm(y ~ atemp + gtemp + variety + atemp:gtemp + atemp:variety + atemp:gtemp:variety + gtemp:variety)
Anova(fitC, type = 3)
#P-value is 0.475537  


detach(dat)

######PROBLEM #3	
####PART A.
require(quantreg)
slope_est_L2_100 = rep(NA,200)
slope_est_L1_100 = rep(NA,200)
slope_est_L2_1000 = rep(NA,200)
slope_est_L1_1000 = rep(NA,200)

for (i in 1:200){
	x = c(rnorm(100,0,1))
	e = c(rnorm(100,0,0.5))
	y = 3*x + e
	fit.L2 = lm(y ~ x)
	fit.L1 = rq(y ~ x)
	slope_est_L2_100[i] = coef(fit.L2)["x"]
	slope_est_L1_100[i] = coef(fit.L1)["x"]
}

for (i in 1:200){
	x = c(rnorm(1000,0,1))
	e = c(rnorm(1000,0,0.5))
	y = 3*x + e
	fit.L2 = lm(y ~ x)
	fit.L1 = rq(y ~ x)
	slope_est_L2_1000[i] = coef(fit.L2)["x"]
	slope_est_L1_1000[i] = coef(fit.L1)["x"]
}


boxplot(slope_est_L2_100, slope_est_L1_100, slope_est_L2_1000, slope_est_L1_1000, names=c("L2(100 Obs)", "L1(100 0bs)", "L2(1000 Obs)", "L1(1000 0bs)"))
title("Slope Estimates for L2 and L1 Regressions with 100 and 1000 Observations", sub="(Errors Standard Normal)")

#COMMENTS: It seems that for a given number of observations and when the errors are normal, least squares produces a smaller variance of the estimator than least absolute value regression. Obviously, more observations result in a smaller variance of the slope. Both estimators seem to be unbiased.


###PART B.
#Now, assume errors are double-exponetially distributed
require(quantreg)
require(smoothmest) #used to randomly generate values from double-exp dist
slope_est_L2_100 = rep(NA,200)
slope_est_L1_100 = rep(NA,200)
slope_est_L2_1000 = rep(NA,200)
slope_est_L1_1000 = rep(NA,200)

for (i in 1:200){
	x = c(rnorm(100,0,1))
	e = c(rdoublex(100,0,1)) 
	y = 3*x + e
	fit.L2 = lm(y ~ x)
	fit.L1 = rq(y ~ x)
	slope_est_L2_100[i] = coef(fit.L2)["x"]
	slope_est_L1_100[i] = coef(fit.L1)["x"]
}

for (i in 1:200){
	x = c(rnorm(1000,0,1))
	e = c(rdoublex(1000,0,1))
	y = 3*x + e
	fit.L2 = lm(y ~ x)
	fit.L1 = rq(y ~ x)
	slope_est_L2_1000[i] = coef(fit.L2)["x"]
	slope_est_L1_1000[i] = coef(fit.L1)["x"]
}

boxplot(slope_est_L2_100, slope_est_L1_100, slope_est_L2_1000, slope_est_L1_1000, names=c("L2(100 Obs)", "L1(100 0bs)", "L2(1000 Obs)", "L1(1000 0bs)"))
title("Slope Estimates for L2 and L1 Regressions with 100 and 1000 Observations", sub="(Errors Double Exponential)")

#now L1 (least absolute value regression) clearly has a smaller variance on the estimates of the slope than least squares.Again, the estimators seem to be unbiased and larger samples yield more precise results.