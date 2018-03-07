#HW 4
#Brian Hand (A08482072)
#2/29/2016


#######PROBLEM 1
detach(dat)
load("/Users/brianhand/Dropbox/math282B-Winter16/04cars.version2.rda")
tmp = dat[,c(8,10)] # extract selected variables
tmp = tmp[complete.cases(tmp),] # extracts complete cases
tmp = as.data.frame(tmp)
summary(tmp)
names(tmp) = c("hp","mpg") # abbreviate names
dat = tmp
attach(dat)


require(quantreg) #quantreg package needed for Least Absolute Regression
kSet = c(5,10,nrow(dat))
error.L2.avg = c(rep(NA,3))
error.L1.avg = c(rep(NA,3))
for (i in 1:3){
	k = kSet[i]
	dat = dat[sample(nrow(dat)),]	#shuffle the data
	blocks = cut(seq(1,nrow(dat)), breaks = k, labels = FALSE) 	#divide the data into blocks
	error.L2 = rep(NA, k)
	error.L1 = rep(NA, k)
		for (j in (1:k)){
			test_indexes = which(blocks == j, arr.ind = TRUE)
			test = as.data.frame(dat[test_indexes,])	#specify test data
			train = as.data.frame(dat[-test_indexes,])  #specify train data
			fit.L2 = lm(mpg ~ poly(hp, degree = 2, raw = TRUE), data = train) #Fit LS Reg Model
			fit.L1 = rq(mpg ~ poly(hp, degree = 2, raw = TRUE), data = train) #Fit LA Reg Model
			pred.L2 = predict(fit.L2, test) #Get LS Reg Fitted Values
			pred.L1 = predict(fit.L1, test) #Get LA Reg Fitted Values
			error.L2[j] = sum((test$mpg - pred.L2)^2)/(nrow(test)) #Calculate Error for LS Reg
			error.L1[j] = sum((test$mpg - pred.L1)^2)/(nrow(test)) #Calculate Error for LA Reg
			}
	error.L2.avg[i] = mean(c(error.L2)) #Average Errors over test blocks for LS Reg
	error.L1.avg[i] = mean(c(error.L1)) #Average Errors over test blocks for LA Reg
}
errors = cbind(error.L2.avg, error.L1.avg)
errors #Print errors

####INTERPRETATION: It seems clear that Least Squares Regression is superior to Least Absolute Regression according to the CV error estimates. I would therefore choose the Least Squares model.  Here's a barplot of the CV errors:

#Barplot:
barplot(t(errors), names.arg=c("5 Blocks", "10 Blocks", "N Blocks"), beside = TRUE, col = c("darkblue","red"), main = "Cross Validation Error Estimates with Varying Numbers of Blocks", sub = "Least Squares Regression (L2) vs Least Absolute Regression (L1) ", ylab = "CV Error Estimates" , ylim = c(0,max(errors)+5))
legend("topright", legend = c("L2 Regression", "L1 Regression"), fill = c("darkblue","red"))
detach(dat)


#######PROBLEM 2

backwardSelect = function(X, y){
	Z = cbind(y,X)									#Combine indep. and dep. variables into one matrix, Z
	for (n in 1:ncol(Z)){							#For loop goes through all columns and sets them as numeric
		if (is.numeric(Z[,n]) == FALSE){
			Z[,n] = as.numeric(Z[,n])
		}
	}
	Z = as.data.frame(scale(Z))						#All variables in Z are centered and scaled. Thus, no intercept is necessary. Also, coefficients can now be compared
	
	
	j = 1											#j is a variable that keeps track of what step we are on.
	while (ncol(Z) > 2){							#Run loop as long as there are independent variables remaining (first column of Z is the dependent variable )
		fit0 = lm(y ~ . -1, data = Z)				#Run H0 Regression (with no intercept) with all variables available for current step
		F.Stat = c(rep(NA, ncol(Z)))
		for (i in 2:ncol(Z)){						#Loop through each indep variable in Z (Z[1] is the dep variable)
			fit1 = lm(y ~ . -1, data = Z[,-i])		#Run seperate H1 Regressions, excluding chosen indep variable each time 
			F.Stat[i] = anova(fit0, fit1)$"F"[2]	#Calculate H0 vs H1 F-Stat for each indep variable 
		}

		if (min(F.Stat[-1]) > 4){					#If all F-Stats are greater than 4, break the loop and assign H0 to final.model
			print(paste0("Step ",j,": All the F-Stats are greater than 4. End Backwards Selection process. Final Model listed below:"))
			final.model = fit0
			break
		}
		else {										#Otherwise, drop variable associated with smallest F-Stat from Z, and continue to next step 
			i = which.min(F.Stat)					#Select variable with lowest F-Stat
			print(paste0("Step ",j,": Remove the variable '", colnames(Z[i]), "' from the regression. Associated F-Stat is: ", format(F.Stat[i], digits = 3)))
			Z = Z[,-i]								#Drops chosen indep variable from matrix Z
			j = j + 1								#Add 1 to step counter
		}
	}
	summary(final.model)
}


#Test backwardSelect
detach(dat)
require(MASS)
dat = Boston
attach(dat)
backwardSelect(dat[,-14], medv)
detach(dat)



##########Problem 3
#Fit function trains model on X.train and y.train, and then returns predictions (predicted.test) with X.test
Fit = function(X.train, y.train, X.test){
	Z.train = cbind(y.train, X.train)
	fit = lm(y.train ~ ., data = Z.train)
	predicted.test = predict(fit, data = X.test)
	return(predicted.test)
}

subsampleSelect = function(X, y, Fit, p, B){
	for (n in 1:ncol(X)){							#For loop goes through all columns in X and sets them as numeric
		if (is.numeric(X[,n]) == FALSE){
			X[,n] = as.numeric(X[,n])
		}
	}
	error = c(rep(NA, B))
	for (i in (1:B)){
		train.rows = sample(nrow(X), p*nrow(X))		#Sample row numbers (observation numbers)
		X.train = X[train.rows,]					#specify training observations	
		y.train = y[train.rows]						
		X.test = X[-train.rows,]					#specify test observations
		y.test = y[-train.rows]
		error[i] = sum((y.test - Fit(X.train, y.train, X.test))^2)/(nrow(X.test))		#calculated predicted error
	}
	error_avg = mean(error)							#average errors across B samples
	print(paste("The estimated prediction error is:", format(error_avg, digits = 6)))
}

detach(dat)
require(MASS)
dat = Boston
attach(dat)
#test using Boston data, with medv as the dependent variable
subsampleSelect(dat[,-14], medv, Fit, 0.5, 99)
detach(dat)
