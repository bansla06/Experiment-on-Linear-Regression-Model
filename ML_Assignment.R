

#=================================================================================
 # Data summary and splitting data into test and train  
#=================================================================================

# 1. Inputing the data
     setwd('C:\\Users\\shubh\\Desktop\\MACHINE LEARNING-2\\Assignment')
     house =read.csv("house.csv")
# 2. Summary and view of the data
     summary(house)
     View(house)
     sample = house[1:500,]
# 3. Splitting the data into test and train
     library(caTools)
     set.seed(1) # set seed to ensure you always have same random numbers generated

     train = house[1:400,] 
     
     test = house[401:500,]
     
train_error = c()
test_error = c()
#=================================================================================
# Fitting the polynomial  regression line of order 7 model on different samples 
#=================================================================================
# 1. Model:- 1 ( Fitting on sample of 10)
     train1 = train[1:11,]
     m1 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                    (sqft_living^6)+I(sqft_living^7), train1)
     m1
    
     #PLOTTING THE MODEL OVER THE  TRAIN DATA
     plot(train1$sqft_living,train1$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 10)",
          xlab="Square feet", ylab="Price",sub = "Model-1")
     
     lines(sort(train1$sqft_living), fitted(m1)[order(train1$sqft_living)], col='red', type='l')
    
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     train_error = c(train_error,sum(m1$residuals^2))                 #Train Error for model 1
     pred = predict(m1, newdata=test)
     test_error =  c(test_error,sum((pred-test$price)^2))               #Test Error for model 1
     train_error
     test_error
    
     
# Model :- 2  ( Fitting on sample of 20)
     train2 = train[1:21,]
     m2 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train2)
     m2
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train2$sqft_living,train2$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 20)",
          xlab="Square feet", ylab="price",sub = "Model-2")
     
     lines(sort(train2$sqft_living), fitted(m2)[order(train2$sqft_living)], col='violet', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m2$residuals^2)                            #Train Error for model 2
     pred = predict(m2, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 2
     
     
  # Model :- 3  ( Fitting on sample of 30)
     train3 = train[1:31,]
     m3 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train3)
     m3
     
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train3$sqft_living,train3$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 30)",
          xlab="Square feet", ylab="price",sub = "Model-3")
     
     lines(sort(train3$sqft_living), fitted(m3)[order(train3$sqft_living)], col='black', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m3$residuals^2)                            #Train Error for model 3
     pred = predict(m3, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 3
     
  # Model :- 4  ( Fitting on sample of 40)
     train4 = train[1:41,]
     m4 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train4)
     m4
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train4$sqft_living,train4$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 40)",
          xlab="Square feet", ylab="price",sub = "Model-4")
     lines(sort(train4$sqft_living), fitted(m4)[order(train4$sqft_living)], col='magenta', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m4$residuals^2)                            #Train Error for model 4
     pred = predict(m4, newdata=test)
     sum((pred-test$price)^2)                         #Test Errorfor model 4
     
  # Model :- 5  ( Fitting on sample of 50)
     train5 = train[1:51,]
     m5 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train5)
     m5
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train5$sqft_living,train5$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 50)",
          xlab="Square feet", ylab="price",sub = "Model-5")
     lines(sort(train5$sqft_living), fitted(m5)[order(train5$sqft_living)], col='orange', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m5$residuals^2)                            #Train Error for model 5
     pred = predict(m5, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 5
     
  # Model :- 6  ( Fitting on sample of 70)
     train6 = train[1:71,]
     m6 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train6)
     m6
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train6$sqft_living,train6$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 70)",
          xlab="Square feet", ylab="price",sub = "Model-6")
     lines(sort(train6$sqft_living), fitted(m6)[order(train6$sqft_living)], col='cyan4', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m6$residuals^2)                            #Train Error for model 6
     pred = predict(m6, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 6
     
  # Model :- 7  ( Fitting on sample of 100)
     train7 = train[1:101,]
     m7 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train7)
     m7
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train7$sqft_living,train7$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 100)",
          xlab="Square feet", ylab="price",sub = "Model-7")
     lines(sort(train7$sqft_living), fitted(m7)[order(train7$sqft_living)], col='firebrick', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m7$residuals^2)                             #Train Error for model 7
     pred = predict(m7, newdata=test)
     sum((pred-test$price)^2)                          #Test Error for model 7
     
  # Model :- 8  ( Fitting on sample of 200)
     train8 = train[1:201,]
     m8 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train8)
     m8
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train8$sqft_living,train8$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 200)",
          xlab="Square feet", ylab="price",sub = "Model-8")
     lines(sort(train8$sqft_living), fitted(m8)[order(train8$sqft_living)], col='darkslategrey', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m8$residuals^2)                            #Train Error for model 8
     pred = predict(m8, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 8
     
  # Model :- 9  ( Fitting on sample of 300)
     train9 = train[1:301,]
     m9 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train9)
     m9
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train9$sqft_living,train9$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 300)",
          xlab="Square feet", ylab="price",sub = "Model-9")
     lines(sort(train9$sqft_living), fitted(m9)[order(train9$sqft_living)], col='darkmagenta', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m9$residuals^2)                            #Train Error for model 9
     pred = predict(m9, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 9
     
  # Model :- 10 ( Fitting on sample of 400)
     train10 = train[1:400,]
     m10 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train10)
     m10
     
     #PLOTTING THE MODEL OVER THE DATA
     plot(train10$sqft_living,train10$price, pch=19, cex=1,col="green",main="Price vs Area of living (Sample Of 400)",
          xlab="Square feet", ylab="price",sub = "Model-10")
     lines(sort(train10$sqft_living), fitted(m10)[order(train10$sqft_living)], col='blue3', type='l')
     
     # CALCULATING TRAIN ERROR AND TEST ERROR 
     sum(m10$residuals^2)                           #Train Error for model 10
     pred = predict(m10, newdata=test)
     sum((pred-test$price)^2)                         #Test Error for model 10

     
# Plot between Test Error and sample size
     Sample_Size =c(10,20,30,40,50,70,100,200,300,400)
     Test_Error = c(8.956682e+15,3.846419e+13,2.971447e+15,1.487831e+16,1.396253e+13,
                    1.638232e+13,7.354857e+12,3.149336e+13,8.39453e+13,3.920693e+13)
     plot(Sample_Size,Test_Error,main="Sample Size Vs Test Error",xlab = "Sample Size",ylab="Test  Error")
     lines(Sample_Size,Test_Error,col="red")
     
     plot(Sample_Size,Train_Error,main="Sample Size vs Train Error",xlab="Sample Size",ylab="Train Error")
     Train_Error = c(93299355177, 192241551080,1.357841e+12, 1.632846e+12,2.762721e+12,
                     4.008242e+12,5.163741e+12,8.654125e+12,1.981297e+13,2.520591e+13)
     lines(Sample_Size,Train_Error,col="red")

     
#========================================================================================
            # Fitting the regression line of different order on same samples 
#========================================================================================
# Taking Fixed Sample Size of 20 
     train_f = house[1:21,] 
     
     test_f = house[401:500,]
     
     
# Model -1  (Order -1)
     m11 <- lm(price ~ sqft_living, train_f)
     m11
     
     #PLOTTING THE MODEL OVER THE  TRAIN DATA
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
            main="Price vs Area of living (Order -1)",
            xlab="Square feet", ylab="Price",sub = "Model-1")
     
     lines(sort(train_f$sqft_living), fitted(m11)[order(train_f$sqft_living)], 
            col='red', type='l')
     
     
     sum(m11$residuals^2)                            # Train Error for model 1
     pred1 = predict(m11,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
     
     pred = predict(m11, newdata=test)
     sum((pred-test_f$price)^2)                      # Test Error for model 1
     sqrt(mean(sum((pred-test_f$price)^2)))          # Test RMSE for model 1
     
# Model -2  (Order -2)
     m12 <- lm(price ~ sqft_living+I(sqft_living^2), train_f)
     m12
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living (Order -2)",
          xlab="Square feet", ylab="Price",sub = "Model-2")
     
     lines(sort(train_f$sqft_living), fitted(m12)[order(train_f$sqft_living)], 
           col='black', type='l')
     
     sum(m12$residuals^2)                            #Train Error for model 2
     pred1 = predict(m12,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
     
     pred = predict(m12, newdata=test)
     sum((pred-test_f$price)^2)   
     sqrt(mean(sum((pred-test_f$price)^2)))          # RMSE for model 1
     
# Model -3  (Order -7)
     m13 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7), train_f)
     m13
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living (Order -7)",
          xlab="Square feet", ylab="Price",sub = "Model-3")
     
     lines(sort(train_f$sqft_living), fitted(m13)[order(train_f$sqft_living)], 
           col='Orange', type='l')
     
     
     sum(m13$residuals^2)                            #Train Error for model 2
     pred1 = predict(m13,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
     
     
     pred = predict(m13, newdata=test)
     sum((pred-test_f$price)^2) 
     sqrt(mean(sum((pred-test_f$price)^2)))          # RMSE for model 1
     
# Model -4  (Order -8)
     m14 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7)+I(sqft_living^8), train_f)
     m14
     
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living (Order -8)",
          xlab="Square feet", ylab="Price",sub = "Model-4")
     
     lines(sort(train_f$sqft_living), fitted(m14)[order(train_f$sqft_living)], 
           col='magenta', type='l')
     
     
     sum(m14$residuals^2)                            #Train Error for model 2
     pred1 = predict(m14,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
     
     
     pred = predict(m14, newdata=test)
     sum((pred-test_f$price)^2) 
     sqrt(mean(sum((pred-test_f$price)^2)))         # RMSE for model 1
     
     
     
# Model -5  (Order -9)
     m15 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7)+I(sqft_living^8)+I(sqft_living^9), train_f)
     m15
     
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living (Order -9)",
          xlab="Square feet", ylab="Price",sub = "Model-5")
     
     lines(sort(train_f$sqft_living), fitted(m15)[order(train_f$sqft_living)], 
           col='blue', type='l')
     
     sum(m15$residuals^2)                            #Train Error for model 2
     pred1 = predict(m15,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
  
     pred = predict(m15, newdata=test)
     sum((pred-test_f$price)^2) 
     sqrt(mean(sum((pred-test_f$price)^2)))          # RMSE for model 1
     
# Model -6  (Order -10)
     m16 <- lm(price ~ sqft_living+I(sqft_living^2)+I(sqft_living^3)+I(sqft_living^4)+I(sqft_living^5)+
                (sqft_living^6)+I(sqft_living^7)+I(sqft_living^8)+I(sqft_living^9)+I(sqft_living^10), train_f)
     m16
     
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living (Order -10)",
          xlab="Square feet", ylab="Price",sub = "Model-6")
     
     lines(sort(train_f$sqft_living), fitted(m16)[order(train_f$sqft_living)], 
           col='brown', type='l')
    
     sum(m16$residuals^2)                            #Train Error for model 2
     pred1 = predict(m11,newdata = train_f)
     sqrt(mean(sum((pred1-train_f$price)^2)))        # Train RMSE for model 1
     
     pred = predict(m16, newdata=test)
     sum((pred-test_f$price)^2) 
     sqrt(mean(sum((pred-test_f$price)^2)))          # RMSE for model 1

# Plotting all the model lines in one graph     
     plot(train_f$sqft_living,train_f$price, pch=19, cex=1,col="green",
          main="Price vs Area of living ",
          xlab="Square feet", ylab="Price",sub = "All model")
     
     
     lines(sort(train_f$sqft_living), fitted(m11)[order(train_f$sqft_living)], 
           col='red', type='l')
     lines(sort(train_f$sqft_living), fitted(m12)[order(train_f$sqft_living)], 
           col='black', type='l')
     lines(sort(train_f$sqft_living), fitted(m13)[order(train_f$sqft_living)], 
           col='Orange', type='l')
     lines(sort(train_f$sqft_living), fitted(m14)[order(train_f$sqft_living)], 
           col='magenta', type='l')
     lines(sort(train_f$sqft_living), fitted(m15)[order(train_f$sqft_living)], 
           col='blue', type='l')
     lines(sort(train_f$sqft_living), fitted(m16)[order(train_f$sqft_living)], 
           col='violet', type='l')
     
     
     
     
     
          
# Plotting Graph Between Test Rss vs Complexity
test_rss = c(3.920693e+13,3.920693e+13, 192241551080,191346418096,191345725946,191345725946)
complexity = c(1,2,7,8,9,10)
plot(complexity,test_rss)
lines(complexity,test_rss)     
     
# Calcualting Rmse and plotting graph against Complexity
RMSE_train = c(459257,452246.2,438453.6,438453.6,437431.6,437430.8)
complexity = c(1,2,7,8,9,10)
RMSE_test = c(2079502,2079144,6201951,83843103,54510985,54510985)

plot(complexity,RMSE_train,main="Complexity vs RMSE Train",xlab = "Complexity",ylab = "RMSE Train")
lines(complexity,RMSE_train,col="red")     

plot(complexity,RMSE_test,,main="Complexity vs RMSE Test",xlab = "Complexity",ylab = "RMSE Test")
lines(complexity,RMSE_test,col="blue")
     
     
     
     
     
     
     