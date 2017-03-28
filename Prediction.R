library(ggplot2)
library(caTools)
library(randomForest)

train <- read.csv("Documents/GitHub/Big-Mart-Sales-Prediction/Train_UWu5bXk.csv")
test <- read.csv("Documents/GitHub/Big-Mart-Sales-Prediction/Test_u94Q5KV.csv")

# Check rows are all filled? No they aren't
nRows <- nrow(train)
nCompRows <- sum(complete.cases(train))
nCompRows/nRows

# Plotting the dependent variable distribution. It's seems like a half-normal distribution
pl1 <- ggplot(train, aes(Item_Outlet_Sales))
pl1 + geom_density(fill = "blue", alpha = "0.7")

# Check fat levels
train$Item_Fat_Content

# Convert different variations of fat to consistent values
train$Item_Fat_Content <- gsub("LF", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("low fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("Low Fat", "lowfat",train$Item_Fat_Content)
train$Item_Fat_Content <- gsub("reg", "Regular",train$Item_Fat_Content)
train$Item_Fat_Content <- as.factor(train$Item_Fat_Content)
summary(train$Item_Fat_Content)

# Using mean to replace the missing values in Item_Weight variable
MeanItem_Weight <- mean(train$Item_Weight[!is.na(train$Item_Weight)])
train$Item_Weight[is.na(train$Item_Weight)] <- MeanItem_Weight

# Using regression to replace the zeros in Item_visibility variable
train_temp <- train %>% filter(Item_Visibility != 0)
visibility_model <- lm(Item_Visibility ~ Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP +
                         Outlet_Establishment_Year + Outlet_Size +
                         Outlet_Location_Type + Item_Outlet_Sales,
                       data = train_temp)
train$Item_Visibility[train$Item_Visibility == 0] <-
  predict(visibility_model,newdata = train[train$Item_Visibility == 0,])

# Classify missing values in Outlet Size
set.seed(100)
train$Outlet_Size <- as.character(train$Outlet_Size)
Storetypes <- subset(train, Outlet_Size != "")
spl <- sample.split(Storetypes$Outlet_Size, SplitRatio = 0.8)
train_outlet <- subset(Storetypes, spl == TRUE)
test_outlet <- subset(Storetypes, spl == FALSE)
## Using Random Forest for classification
train_outlet$Outlet_Size <- as.factor(train_outlet$Outlet_Size)
test_outlet$Outlet_Size <- as.factor(test_outlet$Outlet_Size)
## Creating the model
SizeForest <- randomForest(Outlet_Size ~.-Item_Outlet_Sales -Item_Identifier,
                           data =  train_outlet,nodesize = 25, ntree = 100)  
## Predicting on the test set
PredictForest <- predict(SizeForest, newdata = test_outlet)
## Confusion matrix
table(test_outlet$Outlet_Size, PredictForest)
# Classify
train$Outlet_Size <- predict(SizeForest, newdata = train)


# Check rows are all filled? Yes they are
nRows <- nrow(train)
nCompRows <- sum(complete.cases(train))
nCompRows/nRows


