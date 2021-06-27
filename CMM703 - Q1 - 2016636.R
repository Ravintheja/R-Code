#Question 01
setwd("E:\\IIT\\Data Analysis\\workspace\\Cw")
 library(caret)
 library(tidyverse)  # data manipulation
 library(cluster)    # clustering algorithms
 library(factoextra) # clustering algorithms & visualization
 library(mlbench)
 library('randomForest')
 library(foreign)

a_data = read.table("A.data", sep=",")
b_data = read.table("B.data", sep=",")
c_data = read.table("C.data", sep=",")
d_data = read.table("D.data", sep=",")

#A - Combining 4 Datasets
main_data <- rbind(a_data,b_data,c_data,d_data)

#Exploration
dim(main_data)
str(main_data)
summary(main_data)

#1. Handling Missing Values
mis_count = 0
var_num = 1



  #1.1 Findings Missing Values
for (val in main_data){
  for(row_num in 1:920){
    if(val[row_num] == '?'){
      mis_count = mis_count + 1
    }
  }
  print(paste('Variable: ', var_num, mis_count))
  mis_count = 0
  var_num = var_num + 1
}

  #1.2 Removing Columns
main_data = subset(main_data, select = -c(V12,V13))
main_data <- as.data.frame(sapply(main_data, as.numeric)) #converting all to num

  #1.3 Exploration
var_num = 1
par(mfrow = c(2,2))
for (val in main_data){
  #print(paste('Variable: ', var_num))
  boxplot(val, main = paste('Variable: ', var_num))
  var_num = var_num + 1
}
par(mfrow = c(1,1))

  #1.4 Outlier Removal
var_num = 1
for (val in main_data){
  
  print(paste('Variable: ', var_num))
  # print(paste('Outliers: ',boxplot.stats(as.integer(val))$out))
  
  Q <- quantile(val, probs=c(.25, .75), na.rm = TRUE)
  print(Q[1])
  print(Q[2])
  var_mean = mean(val, na.rm = TRUE)
  for(row_num in 1:920){
    if(var_num != 12){
      if(!is.na(main_data[row_num,var_num])){
        if(main_data[row_num,var_num] > Q[2] | main_data[row_num,var_num] < Q[1]){
          main_data[row_num,var_num] = var_mean
        }
      }
    }
  }
  var_num = var_num + 1
}
main_data
# print(main_data[376,5])
  #1.4 Replacing Missing Values with  - Not NEEDED
# var_num = 1
# for (val in main_data){
#   for(row_num in 1:920){
#     if(val[row_num] == '?'){
#       main_data[row_num,var_num] = NA
#     }
#    
#   }
#   var_num = var_num + 1
# }


  #1.5 Imputing Missing Values
var_num = 1
for (val in main_data){
  for(row_num in 1:920){
    if(is.na(val[row_num])){
      main_data[row_num,var_num] = mean(as.integer(val), na.rm = TRUE)
    }
  }
  print(mean(as.integer(val), na.rm = TRUE))
  var_num = var_num + 1
}

#1.6 Imputing Response Variable
n = 1
for (val in main_data$V14){
  if(val > 1){
    main_data[n,12] = 1
  }
  n = n + 1
}

main_data
#sum(as.integer(main_data[1:920,12]))


#B - Cluster Analysis

#Total Scaling
main_data_2 <- scale(main_data)

#Selective Scaling
main_data %>% mutate_at(c(1,2,3,4,5,6,7,8,9,10,11), funs(c(scale(.))))

#Distance Calculation
dist <- get_dist(main_data_2)
fviz_dist(dist, gradient = list(low = "white", mid = "yellow", high = "red"))

#K-Means
km <- kmeans(main_data_2, centers = 2, nstart = 25)
str(km)
fviz_cluster(km, data = main_data_2)

main_data <- as.data.frame(main_data)

#C - Dataset Split
set.seed(100)
val_index <- createDataPartition(main_data$V14, p=0.80, list=FALSE)
val_dataset <- main_data[-val_index,]
train_dataset <- main_data[val_index,]

# Model <- train(V14 ~ ., data = train_dataset,
#                method = "lm",
#                na.action = na.omit,
#                preProcess=c("center"),
#                trControl= trainControl(method="none")
# )
# x = train_dataset[,1:11]
# y = train_dataset[,12]
glm.fit <- glm(V14 ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11, data = train_dataset, family = binomial)

summary(glm.fit)

glm.probs <- predict(glm.fit,
                     newdata = val_dataset,
                     type = "response")

glm.pred <- ifelse(glm.probs > 0.5, "1", "0")

#D - Model Assessment
table(glm.pred, val_dataset$V14)

accuracy = mean(glm.pred == val_dataset$V14)
print(paste('Accuracy is:',accuracy))

