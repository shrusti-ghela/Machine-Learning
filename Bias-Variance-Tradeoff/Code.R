set.seed(1)
# generate training data

n <- 40 
x <- runif(n, -1,1)
x <- sort(x)
x[1] <- -1
x[n] <- 1

#the true data generating function is a 5-th order polynomial
#we add Gaussian noise to it

y <- (x-.99)*(x-.4)*(x-.25)*(x+.6)*(x+.8) + .03*rnorm(n)


#generate test data 

n_test <- 40
x_test <- runif(n_test, -1,1)
y_test <- (x_test-.99)*(x_test-.4)*(x_test-.25)*(x_test+.6)*(x_test+.8) + .03*rnorm(n_test)
df_test <- as.data.frame(cbind(x_test, y_test))


t <- runif(1000,-1, 1)
y_true <- (t-.99)*(t-.4)*(t-.25)*(t+.6)*(t+.8)


p <- ggplot()+ 
  geom_line(aes(x=t, y=y_true), size=1.5)+
  ylim(-0.4, 0.2)

q <- ggplot()+ 
  geom_line(aes(x=t, y=y_true), size =1.5)+
  ylim(-0.4,0.2)

r <- ggplot()+ 
  geom_line(aes(x=t, y=y_true), size=1.5)+
  ylim(-0.4, 0.2)

train.3_MSE <- rep(NA, 100)
test.3_MSE <- rep(NA, 100)
train.15_MSE <- rep(NA, 100)
test.15_MSE <- rep(NA, 100)
train.5_MSE <- rep(NA, 100)
test.5_MSE <- rep(NA, 100)

for (i in 1:100){
  
  n <- 40
  x <- runif(n, -1,1)
  x <- sort(x)
  x[1] <- -1
  x[n] <- 1
  df <- as.data.frame(cbind(x,y))
  
  # degree-3 polynomial linear regression
  lm.fit.3 <- lm(y ~ poly(x,3), data = df)
  
  y_hat_train.3 <- predict(lm.fit.3, data=df$x)
  train.3_MSE[i] <- mean((y_hat_train.3-df$y)^2)
  #print(train.3_MSE)
  
  y_hat_test.3 <- predict(lm.fit.3, data=df_test$x_test)
  test.3_MSE[i] <- mean((y_hat_test.3 - df_test$y_test)^2)
  
  #print(test.3_MSE)
  
  # degree-15 polynomial linear regression
  lm.fit.15 <- lm(y ~ poly(x,15), data = df)
  
  y_hat_train.15 <- predict(lm.fit.15, data=df)
  train.15_MSE[i] <- mean((y_hat_train.15-df$y)^2)
  #print(train.15_MSE)
  
  y_hat_test.15 <- predict(lm.fit.15, data=df_test$x_test)
  test.15_MSE[i] <- mean((y_hat_test.15-df_test$y_test)^2)
  #print(test.15_MSE)
  
  # degree-5 polynomial linear regression
  lm.fit.5 <- lm(y ~ poly(x,5), data = df)
  
  y_hat_train.5 <- predict(lm.fit.5, data=df$x)
  train.5_MSE[i] <- mean((y_hat_train.5-df$y)^2)
  #print(train.5_MSE)
  
  y_hat_test.5 <- predict(lm.fit.5, data=df_test$x_test)
  test.5_MSE[i] <- mean((y_hat_test.5 - df_test$y_test)^2)
  
  #print(test.5_MSE)
  
  
  
  r <- r + 
    stat_smooth(method="lm", se=FALSE, fill=NA, 
                formula=y~poly(x,5, raw=TRUE),
                aes_string(x, y), size = 0.1, color="blue") 
  
  p <- p + 
    stat_smooth(method="lm", se=FALSE, fill=NA, 
                formula=y~poly(x,3, raw=TRUE),
                aes_string(x, y), size = 0.1, color="blue") 
  
  
  q <- q + 
    stat_smooth(method="lm", se=FALSE, fill=NA, 
                formula=y~poly(x,15, raw=TRUE), 
                aes_string(x, y), size = 0.1, color="blue") 
  
}

print(p)
print(q)
print(r)
df_MSE <- as.data.frame(cbind(train.3_MSE, test.3_MSE, train.15_MSE,
                              test.15_MSE, train.5_MSE, test.5_MSE))
head(df_MSE)