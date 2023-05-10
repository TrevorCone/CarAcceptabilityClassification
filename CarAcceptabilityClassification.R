library(caret)
library(tidyverse)
library(ggplot2)
library(randomForest)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
library(ranger)
#Read in the file from 
#https://www.kaggle.com/datasets/subhajeetdas/car-acceptability-classification-dataset?select=car.csv
fileRead <- read.csv("car.csv")
car_df <- fileRead
#Exploring the data and making sure it is good to go. 
head(car_df)
summary(car_df)
#Check for any hidden NA values
sum(is.na.data.frame(car_df)) # returns 0
#Create each variable to be a factor. 
Price <- as.factor(car_df$Buying_Price)
Maintenance<- as.factor(car_df$Maintenance_Price)
Doors <- as.factor(car_df$No_of_Doors)
Seats <- as.factor(car_df$Person_Capacity)
Cargo <- as.factor(car_df$Size_of_Luggage)
Safety<- as.factor(car_df$Safety)
Acceptability <- as.factor(car_df$Car_Acceptability)
factor_car_df <- data.frame(Price, Maintenance, Doors, Seats, Cargo, Safety, Acceptability)
head(factor_car_df)
summary(factor_car_df)
# Split data into training and test
set.seed(4)
#The split is defined here at 0.6 so we will get a 60/40 split
split <- 0.85
tIndex <- sample(1:nrow(factor_car_df), split * nrow(factor_car_df))
train <- factor_car_df[tIndex, ]
test <- factor_car_df[-tIndex, ]
summary(train)
summary(test)
#forest model method
ForestModel1 <- train(Acceptability ~ Price + Maintenance + Safety + Cargo + Doors + Seats, 
                      data = train, 
                      method = "rf")
ForestModel1
Model1_Prediction <- predict(ForestModel1, test)
Model1_Prediction
cm <- confusionMatrix(Model1_Prediction,test$Acceptability)
heatmap <- as.data.frame(cm$table)
ggplot(heatmap, aes(x = Reference, y = Prediction, fill = Freq)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(16, "RdYlGn")) + 
  ggtitle("Forest: Model 1")
#nnet method
nnetModel2 <- train(Acceptability ~ Price + Maintenance + Safety + Cargo + Doors + Seats, 
                      data = train, 
                      method = "nnet")
nnetModel2
Model2_Prediction <- predict(nnetModel2, test)
Model2_Prediction
cm2 <- confusionMatrix(Model2_Prediction, test$Acceptability)
heatmap2 <- as.data.frame(cm2$table)
ggplot(heatmap2, aes(x = Reference, y = Prediction, fill = Freq)) + 
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(16, "RdYlGn")) + 
  ggtitle("NNET: Model 2")
# ranger method.
Model3 <- train(Acceptability ~ Price + Maintenance + Safety + Cargo + Doors + Seats, 
                    data = train, 
                    method = "ranger")
Model3
Model3_Prediction <- predict(Model3,test)
Model3_Prediction
cm3 <- confusionMatrix(Model3_Prediction, test$Acceptability)
heatmap3 <- as.data.frame(cm3$table)
ggplot(heatmap3, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "black") +
  scale_fill_gradientn(colors = hcl.colors(16, "RdYlGn")) + 
  ggtitle("Ranger: Model 3")
