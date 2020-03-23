library(glmnet)

#reading blog train data
blog_data <- read.csv('C:/My data/UB Sem 2/Optimization technique in Machine Learning/Project1/BlogFeedback/blogData_train.csv',header=FALSE,sep=",")

#reading 1st test set of 3rd feb 2012
test_set_feb1 <- read.csv('C:/My data/UB Sem 2/Optimization technique in Machine Learning/Project1/BlogFeedback/blogData_test-2012.02.03.00_00.csv',header=FALSE,sep=",")

#reading 2nd test set of 4th feb 2012
test_set_feb2 <- read.csv('C:/My data/UB Sem 2/Optimization technique in Machine Learning/Project1/BlogFeedback/blogData_test-2012.02.04.00_00.csv',header=FALSE,sep=",")

#reading 1st test set of 3rd mar 2012
test_set_mar1 <-  read.csv('C:/My data/UB Sem 2/Optimization technique in Machine Learning/Project1/BlogFeedback/blogData_test-2012.03.03.00_00.csv',header=FALSE,sep=",")

#reading 2nd test set of 4th mar 2012
test_set_mar2 <- read.csv('C:/My data/UB Sem 2/Optimization technique in Machine Learning/Project1/BlogFeedback/blogData_test-2012.03.04.00_00.csv',header=FALSE,sep=",")

#creating train data for basic features which consists of col 51 to 60 and target column
train_exp1 <- blog_data[ , c(51:60)]

#creating train data for textual features , col 63 to 262
train_exp2 <- blog_data[ , c(63:262)]

#creating all test sets from test data
test_set1 <- test_set_feb1[ , c(51:60)]
test_set2 <- test_set_feb2[ , c(51:60)]
test_set3 <- test_set_mar1[ , c(51:60)]
test_set4 <- test_set_mar2[ , c(51:60)]
test_set5 <- test_set_feb1[ , c(63:262)]
test_set6 <- test_set_feb2[ , c(63:262)]
test_set7 <- test_set_mar1[ , c(63:262)]
test_set8 <- test_set_mar2[ , c(63:262)]

#creating matrix for both training sets
train_exp1_matrix = as.matrix(train_exp1)
train_exp2_matrix = as.matrix(train_exp2)

#creating matrix for testing set
test_basic_matrix1 = as.matrix(test_set1)
test_basic_matrix2 = as.matrix(test_set2)
test_basic_matrix3 = as.matrix(test_set3)
test_basic_matrix4 = as.matrix(test_set4)
test_textual_matrix1 = as.matrix(test_set5)
test_textual_matrix2 = as.matrix(test_set6)
test_textual_matrix3 = as.matrix(test_set7)
test_textual_matrix4 = as.matrix(test_set8)


#defining lambda range
lambda <- 10^seq(10,-2,length = 100)

#ridge regression model for train set 1 (basic features)
ridge_model <- glmnet(train_exp1_matrix,blog_data$V281,alpha = 0 ,lambda = lambda)
summary(ridge_model)

#ridge regression model for train set 2 (textual features)
ridge_model_textual <- glmnet(train_exp2_matrix,blog_data$V281,alpha = 0 ,lambda = lambda)

#cross validating to find best lambda value for basic features
ridge_model_cv = cv.glmnet(train_exp1_matrix,blog_data$V281,alpha = 0)

#cross validating to find best lambda value for textual features
ridge_model_textual_cv = cv.glmnet(train_exp2_matrix,blog_data$V281,alpha = 0)

#selecting lambda that minimizes training MSE for basic features
best_lambda = ridge_model_cv$lambda.min

#selecting lambda that minimizes training MSE for textual features
best_lambda_textual = ridge_model_textual_cv$lambda.min

#ridge prediction on test sets for basic feature model
ridge_pred1 = predict(ridge_model, s = best_lambda, newx = test_basic_matrix1)
ridge_pred2 = predict(ridge_model, s = best_lambda, newx = test_basic_matrix2)
ridge_pred3 = predict(ridge_model, s = best_lambda, newx = test_basic_matrix3)
ridge_pred4 = predict(ridge_model, s = best_lambda, newx = test_basic_matrix4)

#ridge prediction on test sets for textual feature model
ridge_pred5 = predict(ridge_model_textual, s = best_lambda_textual, newx = test_textual_matrix1)
ridge_pred6 = predict(ridge_model_textual, s = best_lambda_textual, newx = test_textual_matrix2)
ridge_pred7 = predict(ridge_model_textual, s = best_lambda_textual, newx = test_textual_matrix3)
ridge_pred8 = predict(ridge_model_textual, s = best_lambda_textual, newx = test_textual_matrix4)

#MSE for test sets on basic feature model
MSE_ridge1 <- mean((ridge_pred1 - test_set_feb1$V281)^2) 
MSE_ridge1

MSE_ridge2 <- mean((ridge_pred2 - test_set_feb2$V281)^2) 
MSE_ridge2

MSE_ridge3 <- mean((ridge_pred3 - test_set_mar1$V281)^2) 
MSE_ridge3

MSE_ridge4 <- mean((ridge_pred4 - test_set_mar2$V281)^2) 
MSE_ridge4

#MSE for test sets on textual feature model
MSE_ridge5 <- mean((ridge_pred5 - test_set_feb1$V281)^2)
MSE_ridge5

MSE_ridge6 <- mean((ridge_pred6 - test_set_feb2$V281)^2)
MSE_ridge6

MSE_ridge7 <- mean((ridge_pred7 - test_set_mar1$V281)^2)
MSE_ridge7

MSE_ridge8 <- mean((ridge_pred8 - test_set_mar2$V281)^2)
MSE_ridge8

#Lasso regression model for train set 1 (basic features)
lasso_model = glmnet(train_exp1_matrix, blog_data$V281,alpha = 1,lambda = lambda) 

#Lasso regression model for train set 2 (textual features)
lasso_model_textual = glmnet(train_exp2_matrix, blog_data$V281,alpha = 1,lambda = lambda)

#Cross validating to find best lambda value for basic features
lasso_model_cv = cv.glmnet(train_exp1_matrix, blog_data$V281, alpha = 1) 

#Cross Validating to find best lambda value for textual features
lasso_model_cv_textual = cv.glmnet(train_exp2_matrix, blog_data$V281, alpha = 1)

#Selecting lambda that minimizes training MSE for basic features
best_lam = lasso_model_cv$lambda.min 

#Selecting lambda that minimizes training MSE for textual features
best_lam_textual = lasso_model_cv_textual$lambda.min

#Lasso prediction on test set  for basic features
lasso_pred1 = predict(lasso_model, s = best_lam, newx = test_basic_matrix1) 
lasso_pred2 = predict(lasso_model, s = best_lam, newx = test_basic_matrix2)
lasso_pred3 = predict(lasso_model, s = best_lam, newx = test_basic_matrix3)
lasso_pred4 = predict(lasso_model, s = best_lam, newx = test_basic_matrix4)

#Lasso prediction on test set for textual features
lasso_pred5 = predict(lasso_model_textual, s = best_lam_textual, newx = test_textual_matrix1)
lasso_pred6 = predict(lasso_model_textual, s = best_lam_textual, newx = test_textual_matrix2)
lasso_pred7 = predict(lasso_model_textual, s = best_lam_textual, newx = test_textual_matrix3)
lasso_pred8 = predict(lasso_model_textual, s = best_lam_textual, newx = test_textual_matrix4)

#Mean square error on test set for basic features
MSE_lasso1 <- mean((lasso_pred1 - test_set_feb1$V281)^2) 
MSE_lasso1

MSE_lasso2 <- mean((lasso_pred2 - test_set_feb2$V281)^2) 
MSE_lasso2

MSE_lasso3 <- mean((lasso_pred3 - test_set_mar1$V281)^2) 
MSE_lasso3

MSE_lasso4 <- mean((lasso_pred4 - test_set_mar2$V281)^2) 
MSE_lasso4

#Mean square error on test set for textual features
MSE_lasso5 <- mean((lasso_pred5 - test_set_feb1$V281)^2) 
MSE_lasso5

MSE_lasso6 <- mean((lasso_pred6 - test_set_feb2$V281)^2) 
MSE_lasso6

MSE_lasso7 <- mean((lasso_pred7 - test_set_mar1$V281)^2) 
MSE_lasso7

MSE_lasso8 <- mean((lasso_pred8 - test_set_mar2$V281)^2) 
MSE_lasso8

