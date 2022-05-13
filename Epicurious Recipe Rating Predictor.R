##Install Packages
install.packages("glmnet")
library(glmnet)

##Read in dataset
recipe=read.csv('/Users/celindama/Downloads/epi_r.csv', header=TRUE)
recipe

##SLR with untouched dataset
reg<-lm(recipe$rating~recipe$calories+recipe$protein+recipe$fat+recipe$sodium)
summary(reg)

##Finding NAs
sum(is.na(recipe))
sum(apply(recipe, 1, anyNA))
nrow(recipe)

##Omitting NAs
recipe2<-na.omit(recipe)
nrow(recipe2)

##check for all NAs removed
nrow(recipe)-sum(apply(recipe, 1, anyNA))

##adding unique IDs to dataset
recipe3<-cbind(1:nrow(recipe2), recipe2)
colnames(recipe3)[1]<-paste("ID")
recipe3

##Can omit ID and title columns to reduce noise
recipe4 <- recipe3[,-c(1,2)]


##Predictor variable
x <- data.matrix(recipe4[,-1])

##Outcome Variable
y <- recipe4$rating

##Perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

##Find optimal lambda value that minimizes MSE
best_lambda <- cv_model$lambda.min
best_lambda

##Produce plot of test MSE by lambda value
plot(cv_model)

##Fit model with Lasso
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model)

##Variates Removed
length(coef(best_model)[abs(coef(best_model))>0])

y_predicted <-predict(best_model, s = best_lambda, newx = x)

##Find SST and SSE
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)

##R-Squared
r_sq <- 1 - sse/sst
r_sq

##Mean Squared Error
mse<-mean((y-y_predicted)^2)
mse

rmse<-mean(sqrt((y-y_predicted)^2))
rmse

##Number of coefficients used
length(coef(best_model)[abs(coef(best_model))>0])

##Most negative coefficient
##This is the variable with the greatest effect on lowering the rating

rownames(coef(best_model))[which(coef(best_model)==min(coef(best_model)))]

##leftovers decreased the rating the most

##Amount of rating decrease for leftovers

min(coef(best_model))

##decreased rating by -3.430275

##Most positive coefficient
##This is the variable with the greatest effect on increasing the rating

rownames(coef(best_model))[which(coef(best_model)==max(coef(best_model)[-1]))]

##chili increased the rating the most

##Amount of rating increase by chili

max(coef(best_model))

##increased rating by 3.348296