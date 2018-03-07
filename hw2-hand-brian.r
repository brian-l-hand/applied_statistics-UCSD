#HW 2
#Brian Hand (A08482072)
#2/1/2016


#######PROBLEM 1

#Replace number below with 100, 1000, 10000, 100000
n=10000
	x = c(rnorm(n,0,1))
	error_var = c(rep(NA,n))
	for(i in c(1:n)){
		error_var[i] = ((x[i])^2)/10
		}
	count_ols = rep(NA,200)
	count_wls = rep(NA,200)
	for (i in c(1:200)){
		error = c(rnorm(n,0,(error_var)^.5))
		y = 1 + 2*x + error
		fit_ols = lm(y ~ x)
		fit_wls = lm(y ~ x, weights= 1/(error_var))
		CI_ols=confint(fit_ols, "x")
		CI_wls=confint(fit_wls, "x")
		if (2 > CI_ols[1] & 2 < CI_ols[2]) {
		count_ols[i]=1
		} else if (2 < CI_ols[1] | 2 > CI_ols[2]) {
		count_ols[i]=0
		}
		if (2 > CI_wls[1] & 2 < CI_wls[2]) {
		count_wls[i]=1
		} else if (2 < CI_wls[1] | 2 > CI_wls[2]) {
		count_wls[i]=0
		}
	}
mean(count_ols)
mean(count_wls)

#Yes, this shows substaintially different results between OLS and WLS. WLS is consistently around 0.95, WLS is significantly lower (usually 0.7 - 0.8) 
	
	
	
#######PROBLEM 2
x_0 = c(rep(1,10000))
x_1 = c(seq(from = 0, to = 1, length.out = 10000 ))
condition_num = rep(NA,30)
for (i in c(1:30)){
	reg_matrix = matrix(nrow=10000, ncol=i+1)
	reg_matrix[,1] = x_0
	reg_matrix[,2] = x_1
	if (i>1) {
		for (j in 2:i+1){
			reg_matrix[,j] = x_1^(j-1)
		}
		
	}
	condition_num[i] = kappa(reg_matrix)
}
plot(1:30,condition_num[1:30])
#The condition number seems to rapidly incrase from 20-22. There is a huge jump when p=30



######PROBLEM #3	

Poly.Reg.CB = function(y,x,p){
	fit = lm(y ~ poly(x, degree = p, raw = TRUE))
	pts = seq(min(x), max(x), length.out = 1000)
	predicted = predict(fit, data.frame(x = pts))
	lower = predicted - sqrt((p+1)*qf(.95,p+1,length(x)-p-1)) *sqrt(var(predicted))
	upper = predicted + sqrt((p+1)*qf(.95,p+1,length(x)-p-1))* sqrt(var(predicted))
 	plot(x,y)
 	lines(pts, predicted, col="red", lwd = 3)
 	lines(pts, lower, col="blue", lwd = 3) 	
 	lines(pts, upper, col="blue", lwd = 3)

}


#Test Function on mpg and hp vars from 04cars.rda dataset

load("/Users/brianhand/Dropbox/math282B-Winter16/04cars.rda")
tmp = dat[,c(13,15,16,18,19)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
names(tmp) = c("hp","mpg","wt","len","wd") # abbreviate names
dat = tmp
str(dat)
attach(dat)

Poly.Reg.CB(mpg, hp, 2)
		
detach(dat)