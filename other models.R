
#####################################################
##############       I. SETUP          ##############
#####################################################


# This section sets prepares the R session for subsequent processes. It also loads the data table from a folder.

# Set the working directory to a folder that includes the data table. Note that "/" may be necessary in place of "\".
setwd("XX://Newhouse_ML_in_synthesis")

# Install relevant packages. This only has to be done once per computer.
install.packages("caret")
install.packages("prospectr")
install.packages("cvTools")
install.packages('Rcpp')
install.packages("ggplot2")
install.packages('e1071')
install.packages('glment')
# Load relevant packages. This has to be done once per R session.
library(cvTools)
library(prospectr)
library(caret)
library(ggplot2)
library(Rcpp)
library(glmnet)

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

# 2. Train models using the "train" function from the caret package
# 10-fold cross-validation was used for the model tunning.
train_control <- trainControl(method = "cv", number = 10, savePredictions = T)

# (1) glm model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "glm",  tuneLength = 50)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For glm, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)

# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.glm <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color="black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.glm 

# (2) SIMPLS model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "simpls", verbose = F, tuneLength = 50)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For simpls, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)

# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.simpls <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color="black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.simpls 

# (3) kNN model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "knn", verbose = F, tuneLength = 50)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For knn, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)


# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.knn <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color="black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16),axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.knn 


# (4) Random forest model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "rf", verbose = F, tuneLength = 20)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For RF, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)


# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.rf <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method ="glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.rf

# (5) Treebag model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "treebag", verbose = F)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For Treebag, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)

# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.treebag <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.treebag

# (6) GLMNET model
set.seed(2020)
data <- as.data.frame(pca1.train)
test <- as.data.frame(pca1.test)
MLFit <- train(Yield/100 ~ ., data = data[,-1], trControl = train_control, 
               method = "glmnet", tuneLength = 100)

# Model performance on testing dataset  
ML.pred <- predict(MLFit, test[,-1])*100
ML.r2 <- round(cor(ML.pred, test$Yield)^2, digits = 3)
ML.rmse <- round(RMSE(ML.pred, test$Yield), digits = 1)
ML.mae <- round(MAE(ML.pred, test$Yield), digits = 1)
message("For glmnet, R2 = ", ML.r2, " and RMSE = ", ML.rmse, " and MAE = ", ML.mae)

# Plot model performance on test dataset 
df <- data.frame(x = ML.pred,
                 y = test$Yield)
p.glmnet <- ggplot(df, aes(x = x, y = y)) +
  theme_classic() +
  geom_point(alpha = 1, size = 4, colour = 'black') +
  scale_x_continuous(breaks = seq(0, 100, 25), lim = c(-5, 100)) +
  theme(axis.text.x = element_text(size = 16, color = "black"), axis.text.y = element_text(size = 16, color = "black")) + 
  labs(x = 'Predicted Yield (%)', y = 'Reported Yield (%)') + 
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  geom_segment(aes(x = 0, xend = 100, y = 0, yend = 100), linetype = "dashed") +
  geom_smooth(method = "glm", se = F, size = 1.5, colour = 'deepskyblue3')
p.glmnet
