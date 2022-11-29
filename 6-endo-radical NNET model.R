#####################################################
##############       I. SETUP          ##############
#####################################################


# This section sets prepares the R session for subsequent processes. It also loads the data table from a folder.

# Set the working directory to a folder that includes the data table. Note that "/" may be necessary in place of "\".
setwd("C:/Users/pengp/Dropbox/ML Radical Cyclization/submission for Nature/Newhouse_ML_in_synthesis")
# Install relevant packages. This only has to be done once per computer.
install.packages("caret")
install.packages("nnet")
install.packages("prospectr")
install.packages("cvTools")
install.packages("NeuralNetTools")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages('e1071')

# Load relevant packages. This has to be done once per R session.
library(cvTools)
library(prospectr)
library(caret)
library(e1071)
library(nnet)
library(NeuralNetTools)
library(ggplot2)
library(Rcpp)

# Load the data table. Make sure there is a column titled "Yield".
MLB.table <- read.csv("6-endo-radical library.csv", stringsAsFactors = F)


#####################################################
#############    II. DATA SAMPLING      #############
#####################################################


# split into training/test sets (70%/30%) with Kennard-Stone splitting

set.seed(2020)

dataframe <- as.data.frame(MLB.table)
size <- round(0.70*nrow(dataframe))
training <- kenStone(dataframe[,-c(1,2)], size, .center = T, .scale = T, metric = "euclid")
training_dataframe <- dataframe[training$model, ]
test_dataframe <- dataframe[training$test, ]

###############################################################
##############       III.  PRE-PROCESSING        ##############
##############      INC. FEATURE SELECTION       ##############
###############################################################

scaled <- preProcess(training_dataframe[,-c(1,2)], method = c("YeoJohnson", "center", "scale"))
MLB.scaled <- predict(scaled, MLB.table)
scaled.train <- predict(scaled, training_dataframe)
scaled.test <- predict(scaled, test_dataframe)

Cor1 = findCorrelation(cor(training_dataframe[,-c(1,2)]), cutoff = 0.90)
cor1 <- colnames(training_dataframe[,-c(1,2)])[Cor1]
cor1.train <- training_dataframe[, -which(colnames(training_dataframe) %in% cor1)]
cor1.test <- test_dataframe[, -which(colnames(test_dataframe) %in% cor1)]
MLB.cor1 <- MLB.table[, -which(colnames(MLB.table) %in% cor1)]

pca1 <- preProcess(cor1.train[,-c(1,2)], method = c("YeoJohnson", "center", "scale", "pca"), thresh = 0.90)
pca1.train <- predict(pca1, cor1.train)
pca1.test <- predict(pca1, cor1.test)
MLB.pca1 <- predict(pca1, MLB.cor1)


#####################################################
##############     IV. MODEL TRAINING   #############
##############         AND TESTING      #############
#####################################################

# 1.Train model using the "nnet" function from the nnet package.

# Hyperparameter tunning through Grid search using the "tune" function with 10-fold cross validation.
set.seed(666)
data <- as.data.frame(pca1.train)
tuned=tune.nnet(Yield/100 ~., data = data[,-1], size = seq(4, 20, 1), 
                decay = seq(0, 1, 0.01),
                linout = T, trace = F, maxit = 10000, 
                tunecontrol = tune.control(cross = 10, best.model = T))
tuned$best.parameters
best.size = tuned$best.parameters[1,1]
best.decay = tuned$best.parameters[1,2]

# The result shows that best.size = 10 & best.decay = 0.46 

# Train the nnet model with whole training dataset using the tuned hyperparameters
# Manually setting ¡°size= 10, decay = 0.46¡± would save time by skipping the hyperparameter tunning process.

set.seed(666)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- nnet(Yield/100 ~., data = data[,-1], size = best.size, decay = best.decay,
              linout = T, trace = F, maxit = 10000, MaxNWts = 9999)

# Model performance on testing dataset
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For nnet, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae )

# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.nnet <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.nnet

# Plot neural network
plotnet(MLFit)
# Generate lists or plots of feature importance
olden(MLFit)


######################################################
########### V. Control Experiments for NNET  #########
###########     & Literature Validation     ##########
######################################################

######################################################
###################### 10-fold-CV  ###################
set.seed(666)
cv.error <- NULL
test.cv.r2<- NULL
#test.cv <- NULL
k <- 10
predic.yield <-NULL
reported.yield <- NULL
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
data2 <- as.data.frame(MLB.pca1)
for(i in 1:k){
  index <- sample(1:nrow(data2),round(0.90*nrow(data2)))
  train.cv <- data2[index,]
  test.cv <- data2[-index,]
  nn <- nnet(Yield/100 ~., data=train.cv[,-1], size = 10, decay = 0.46,
  linout = TRUE, trace = F, maxit = 6500, MaxNWts = 9999)
 
  pr.nn <- predict(nn, test.cv[,-1])*100
## adjust predictions in [0-100]  
  pr.nn[pr.nn < 0] <- 0
  pr.nn[pr.nn > 100] <- 100
  print(pr.nn)
  
  predic.yield <- rbind(predic.yield,pr.nn)
  reported.yield <- rbind(reported.yield,test.cv[2])
  
  test.cv.r2[i] <- round(cor(pr.nn, test.cv$Yield)^2, digits=3)
  cv.error[i] <- round(MAE(pr.nn, test.cv$Yield), digits=3)
  print(cv.error[i])
  pbar$step()
}
# errors of each fold
cv.error
mean(cv.error)
#R2 of each fold
test.cv.r2

#calculation of average R2
dp <- data.frame( x = predic.yield,
                  y = reported.yield)
ML.Q2.cv <- round(cor(dp[,1], dp[,2])^2,digits = 3)
ML.Q2.cv

# Visual plot of CV results
boxplot(cv.error,xlab='RMSE CV',col='cyan',
        border='blue',names='CV error (RMSE)',
        main='CV error (RMSE) for NNET',horizontal=TRUE)

pCV <- ggplot(dp, aes(x = dp[,1], y = dp[,2])) +
  theme_classic() +
  geom_point(alpha = 1, size=3, colour='black') +
  scale_x_continuous(breaks = seq(0,100,25), lim=c(-2, 100)) +
  theme(axis.text.x = element_text(size = 16,color="black"),axis.text.y = element_text(size = 16,color="black")) + 
  labs(x='Predicted Yield (%)', y='Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x=0,xend=100,y=0,yend=100), linetype="dashed") +
  geom_smooth(method="glm", se=FALSE, size=1.5, colour='deepskyblue3')
pCV

######################################################
###################### LOO-CV  #######################
set.seed(666)
cv.error <- NULL
test.cv.r<- NULL
test.cv <- NULL
k <- 99
predic.yield <-NULL

library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
data2 <- as.data.frame(MLB.pca1)
for(i in 1:k){
  index <- i
  train.cv <- data2[-index,]
  test.cv <- data2[index,]
  nn <- nnet(Yield/100 ~., data=train.cv[,-1], size = 10, decay = 0.46,# rang = 0.01,
             linout = TRUE, trace = F, maxit = 6500, MaxNWts = 9999)
  
  pr.nn <- predict(nn, test.cv[,-1])*100
## adjust predictions in [0-100]  
  pr.nn[pr.nn < 0] <- 0
  pr.nn[pr.nn > 100] <- 100
  print(pr.nn)
  predic.yield[i] <- pr.nn
  
  cv.error[i] <- round(MAE(pr.nn, test.cv$Yield), digits=3)
  print(cv.error[i])
  pbar$step()
}

# print out errors, and summary of error 
cv.error
summary(cv.error)

# Visual plot of CV results
boxplot(cv.error,xlab='RMSE CV',col='cyan',
        border='blue',names='CV error',
        main='LOO-CV error of NNET',horizontal=TRUE)

##plot LOOCV
dp <- data.frame( x = predic.yield,
                  y = data2$Yield)
ML.Q2.cv <- round(cor(dp[,1], dp[,2])^2,digits = 3)
ML.rmse.cv <- round(RMSE(dp[,1], dp[,2]),digits = 1)
ML.MAE.cv <- round(MAE(dp[,1], dp[,2]),digits = 1)
message("For LOO-CV, Q2 = ", ML.Q2.cv, " and RMSE = ", ML.rmse.cv, " and MAE = ", ML.MAE.cv)

pLOOCV <- ggplot(dp, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size=3, colour='black') +
  scale_x_continuous(breaks = seq(0,100,25), lim=c(-2, 100)) +
  theme(axis.text.x = element_text(size = 16,color="black"),axis.text.y = element_text(size = 16,color="black")) + 
  labs(x='Predicted Yield (%)', y='Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x=0,xend=100,y=0,yend=100), linetype="dashed") +
  geom_smooth(method="glm", se=FALSE, size=1.5, colour='deepskyblue3')
pLOOCV

##################################################### 
# Y-randomization test (average of 10 runs)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)

r2.values <- as.vector(NULL)
rmse.values <- as.vector(NULL) 
mae.values <- as.vector(NULL) 
set.seed(666)
for (n in 1:10){
  all.yields <- c(data$Yield, test$Yield)
  shuffled.yields <- sample(all.yields)
  Ytest.data <- data
  Ytest.data$Yield <- sample(shuffled.yields, size = nrow(data))
  Ytest.test <- test
  Ytest.test$Yield <- sample(shuffled.yields, size = nrow(test))
  MLFit3 <- nnet(Yield/100 ~., data = Ytest.data[,-1], size = 10, decay = 0.46, 
                 linout = T, trace = F, maxit = 4000, MaxNWts = 9999)
  
  ML.pred3 <- predict(MLFit3, newdata = Ytest.test[,-1])*100
  ML.r2_3 <- cor(ML.pred3, Ytest.test$Yield)^2
  r2.values <- append(r2.values, ML.r2_3)
  ML.rmse3 <- RMSE(ML.pred3, Ytest.test$Yield)
  rmse.values <- append(rmse.values, ML.rmse3)
  ML.mae3 <- MAE(ML.pred3, test$Yield)
  mae.values <- append(mae.values, ML.mae3)
}
r2.avg <- round(mean(r2.values),digits = 3)
rmse.avg <- round(mean(rmse.values),digits = 3)
mae.avg <- round(mean(mae.values),digits = 3)

message("For Y-randomization, R2 = ", r2.avg, " and RMSE = ", rmse.avg, " and MAE = ", mae.avg)

# Plot Y-randomization test
df3 <- data.frame(x = ML.pred3,
                 y = Ytest.test$Yield)
p.Y <- ggplot(df3, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.Y

###################################################### 
##  Random data test

set.seed(2020)
random.data <- as.data.frame(training_dataframe)
random.test <- as.data.frame(test_dataframe)
 
random.data[,-c(1,2)] <- lapply(training_dataframe[,-c(1,2)], runif)
random.test[,-c(1,2)] <- lapply(test_dataframe[,-c(1,2)], runif)

random.data.cor <- random.data[, -which(colnames(random.data) %in% cor1)]
random.data.pca <- predict(pca1, random.data.cor)

random.test.cor <- random.test[, -which(colnames(random.test) %in% cor1)]
random.test.pca <- predict(pca1, random.test.cor)

MLFit4 <- nnet(Yield/100 ~., data=random.data.pca[,-1], size = 10, decay = 0.46, 
               linout = T, trace = F, maxit = 4000, MaxNWts = 9999)

ML.pred4 <- predict(MLFit4, random.test.pca[,-1])*100
ML.r2_4 <- round(cor(ML.pred4, random.test$Yield)^2,digits = 3)
ML.rmse4 <- round(RMSE(ML.pred4, random.test$Yield),digits = 3)
ML.mae4 <- round(MAE(ML.pred4, random.test$Yield),digits = 3)

message("For random data, R Squared = ", ML.r2_4, " and RMSE = ", ML.rmse4, " and MAE = ", ML.mae4)
# Plot Random data test
df4 <- data.frame(x = ML.pred4,
                 y = random.test$Yield)
p.R <- ggplot(df4, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size=1.5, colour='deepskyblue3')
p.R

###################################################### 
#  literature validation (extrapolation)
##load data set of literature validation & pre-processing
ML.lit <- read.csv("literature validation.csv", stringsAsFactors = F)
MLB.lit.expB1 <- ML.lit[, -which(colnames(ML.lit) %in% cor1)]
MLB.lit.expB <- predict(pca1, MLB.lit.expB1)
# make the prediction of literature validation
#make sure whatever model you want to use to predict the yields has just been run
exp.lit <- as.data.frame(MLB.lit.expB)
MLB.pred.lit <- predict(MLFit, exp.lit[,-c(1,2)])*100
MLB.pred.lit[MLB.pred.lit < 0] <- 0
MLB.pred.lit[MLB.pred.lit > 100] <- 100
MLB.pred.lit
write.csv(MLB.pred.lit, "predicted yields of literature validation.csv", row.names=T, quote=F)

ML.lit.r2 <- round(cor(MLB.pred.lit, exp.lit$Yield)^2, digits = 3)
ML.lit.rmse <- round(RMSE(MLB.pred.lit, exp.lit$Yield), digits = 1)
ML.lit.mae <- round(MAE(MLB.pred.lit, exp.lit$Yield), digits = 1)
message("For literature validation, R2 = ", ML.lit.r2, " and RMSE = ", ML.lit.rmse, "
        and MAE = ", ML.lit.mae)
### plot of literature validation
df.lit <- data.frame( x = MLB.pred.lit,
                      y = exp.lit$Yield)
p.lit <-  ggplot(df.lit, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, shape = 17, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.lit

######################################################
##########  VI. Make prediction of 6-endo  ###########
######################################################

###### prediction for Clovane substrate refinement ###
##load data set for predictions of 6-endo-trig radical reactions 
ML.Clovane <- read.csv("Clovane substrate refinement.csv", stringsAsFactors = F)

##preprocessing of data
MLB.clovane1 <- ML.Clovane[, -which(colnames(ML.Clovane) %in% cor1)]
MLB.clovane <- predict(pca1, MLB.clovane1)

#make sure whatever model you want to use to predict the yields has just been run

exp <- as.data.frame(MLB.clovane)
MLB.pred.clovane <- predict(MLFit, exp[,-c(1,2)])*100
MLB.pred.clovane[MLB.pred.clovane < 0] <- 0
MLB.pred.clovane[MLB.pred.clovane > 100] <- 100
MLB.pred.clovane

write.csv(MLB.pred.clovane, "predicted yields for clovanes.csv", row.names = T, quote = F)

##############################################################

########## prediction for experimental validation #########
##load data set for predictions of experimental validation 
ML.experimental <- read.csv("experimental validation.csv", stringsAsFactors = F)

##preprocessing of data
MLB.expB <- ML.experimental[, -which(colnames(ML.experimental) %in% cor1)]
MLB.exp <- predict(pca1, MLB.expB)

#make sure whatever model you want to use to predict the yields has just been run

exp <- as.data.frame(MLB.exp)
MLB.pred.exp <- predict(MLFit, exp[,-c(1,2)])*100
MLB.pred.exp[MLB.pred.exp < 0] <- 0
MLB.pred.exp[MLB.pred.exp > 100] <- 100
MLB.pred.exp

write.csv(MLB.pred.exp, "predicted yields for exp-validation.csv", row.names = T, quote = F)

### plot of experimental validation
x = MLB.pred.exp
y = ML.experimental$Yield

df.exp <- data.frame( x,
                      y)

ML.exp.r2 <- round(cor(x, y)^2, digits = 3)
ML.exp.rmse <- round(RMSE(x, y), digits = 1)
ML.exp.mae <- round(MAE(x, y), digits = 1)
message("For Experimental validation, R2 = ", ML.exp.r2, " and RMSE = ", ML.exp.rmse, "
        and MAE = ", ML.exp.mae)

p.exp <-  ggplot(df.exp, aes(x = x, y = y)) +
  theme_classic() +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')+
  geom_point(alpha = 1, size = 4, shape = 18, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Experimental Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed")

p.exp

