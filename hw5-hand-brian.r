#HW 5
#Brian Hand (A08482072)
#3/12/2016


#######PROBLEM 1
detach(dat)
load("/Users/brianhand/Dropbox/math282B-Winter16/cleveland.rda")
dat = cleveland[,-15] #drop condpluss
model.matrix = model.matrix(~ ., data = dat)
dat = data.frame(model.matrix)
dat = dat[,-1] #remove intercept for now, will include intercept in glm specification
summary(dat)
attach(dat)

#PART A
full.model = glm(condsick ~ ., family = binomial(link = 'logit'), data = dat )
summary(full.model)
require(MASS)
final.model = stepAIC(full.model)
summary(final.model)

#PART B
require(boot)
cv.pred.est = cv.glm(data = dat, final.model, K = 5)$delta[1]
print(paste("The CV estimate for prediction error is :", format(cv.pred.est, digits = 4)))

#PART C
#Parts A-B do not generate an accuate estimate of the prediction error, because the variables are selected using the entire dataset (only the parameters are set with just the training dataset). To simulate prediction error with stepwise selection accurately, we must select the variables and generate the parameter estimates with just a portion of the data, and test it on the remaining data.


#PART D
logisticAIC = function(X, y){
	full.model = glm(y ~ ., family = binomial(link = 'logit'), data = X)
	final.model = stepAIC(full.model)
	fit = function(X.new){
		predict(final.model, newdata = X.new, type = "response") 
	}
	return (fit)
}
	
#PART E	
dat = dat[sample(nrow(dat)),]	#shuffle the data
blocks = cut(seq(1,nrow(dat)), breaks = 5, labels = FALSE) 	#divide the data into 5 blocks	
error = c(rep(NA, 5))

for (i in (1:5)){
	test_indexes = which(blocks == i, arr.ind = TRUE) #specify test indecies
	test = as.data.frame(dat[test_indexes,])	#specify test data
	train = as.data.frame(dat[-test_indexes,])  #specify train data
	f.hat = logisticAIC(train[,-23], train[,23]) #23rd column is condsick
	predictions = f.hat(test[,-23])
	error[i] = mean((test[,23] - predictions)^2)
}
error.avg = mean(c(error))
print(paste("The CV estimate for prediction error is :", format(error.avg, digits = 4)))
	
detach(dat)	
	

#######PROBLEM 2
detach(dat)
load("/Users/brianhand/Dropbox/math282B-Winter16/cleveland.rda")
dat = cleveland[,c(1,14)] #drop all but age and cond
dat[,2] = dat[,2] == 'sick' #set cond to be TRUE if sick or FALSE if buff
dat = as.data.frame(dat)
summary(dat)
attach(dat)

#PART A
require(locfit)
fit = locfit(cond ~ age, data = dat)

#PART B
plot(fit, type = "persp")

#PART C
chance55 = 100 * predict(fit, 55)
print(paste("There is a", format(chance55, digits = 3),"percent chance that a 55 year old has a heart condition"))

#PART D 
cond_binary = model.matrix(~., data = dat) #The folowing three lines add a binary version of cond for scoring purposes
cond_binary = cond_binary[,3]
dat = cbind(dat, cond_binary)
a.values= c(seq(0.15, 1, 0.005))
error.avg = c(rep(NA,length(a.values)))
for (j in 1:length(a.values)){
	dat = dat[sample(nrow(dat)),]	#shuffle the data
	blocks = cut(seq(1,nrow(dat)), breaks = 5, labels = FALSE) 	#divide the data into 5 blocks	
	error = c(rep(NA, 5))
	for (i in (1:5)){
		test_indexes = which(blocks == i, arr.ind = TRUE) #specify test indecies
		test = as.data.frame(dat[test_indexes,])	#specify test data
		train = as.data.frame(dat[-test_indexes,])  #specify train data
		fit = locfit(cond ~ age, data = train, alpha = a.values[j])
		predictions = predict(fit, newdata = test)
		error[i] = mean((test[,3] - predictions)^2) # calculate MSE against cond_binary (binary version of cond)
	}
	error.avg[j] = mean(c(error))
}

plot(a.values,error.avg)
### We Should choose a smoothing parameter larger than 0.6 to minimize the predicted CV


