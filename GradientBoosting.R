data <- read.csv("C:/Users/Christie Natashia/Desktop/AML-Code/Taxidata.csv")
# data <- read.csv("C:/Users/Christie Natashia/Desktop/AML-Code/yellow_tripdata_2019-07.csv")
data = data[1:1000,]

library(dplyr)
library(lubridate)
# Data Transformation
data = data %>%
  mutate (tpep_pickup_datetime = as.character(tpep_pickup_datetime),
          tpep_dropoff_datetime = as.character(tpep_dropoff_datetime),
          tpep_pickup_datetime = dmy_hm(tpep_pickup_datetime),
          tpep_dropoff_datetime = dmy_hm(tpep_dropoff_datetime),
          VendorID = factor(VendorID),
          passenger_count = factor(passenger_count),
          passenger_count = as.numeric(passenger_count),
  )



# Data Cleaning
data = data %>%
  mutate(fare_amount = ifelse(fare_amount < 1,ave(fare_amount),fare_amount),
         extra = ifelse(extra < 1,ave(extra),extra),
         mta_tax = ifelse(mta_tax < 1,ave(mta_tax),mta_tax),
         tip_amount = ifelse(tip_amount < 1,ave(tip_amount),tip_amount),
         tolls_amount = ifelse(tolls_amount < 1,ave(tolls_amount),tolls_amount),
         total_amount = ifelse(total_amount  < 1,ave(total_amount ),total_amount),
         congestion_surcharge = ifelse(congestion_surcharge < 1,ave(congestion_surcharge),congestion_surcharge),
         tip_amount = ifelse(tip_amount < 1,ave(tip_amount),tip_amount),
         passenger_count = ifelse(passenger_count < 1,mode(passenger_count),passenger_count),
         improvement_surcharge = ifelse(improvement_surcharge < 1,ave(improvement_surcharge),improvement_surcharge),
         trip_distance = ifelse(trip_distance < 1,ave(trip_distance),trip_distance)
  )
summary(data)




# Feature Engineering on Airport pickup and dropoff location
taxi_zone_lookup <- read.csv("https://s3.amazonaws.com/nyc-tlc/misc/taxi+_zone_lookup.csv")

airport_zone_id = c(1,132, 138) #Newark, JFK Airport, LaGuardia Airport
data = data %>%
  mutate(
    airport_pickup = ifelse(PULocationID %in% airport_zone_id, 1, 0),
    airport_dropoff = ifelse(DOLocationID %in% airport_zone_id, 1, 0)
  )

# Feature Engineering on date
library(chron)
library(lubridate)

data = data %>% 
  mutate(
    pickup_hour = hour(tpep_pickup_datetime),
    weekend = is.weekend(tpep_pickup_datetime),
    weekend = ifelse(weekend == TRUE, 1, 0)
  )

# Data Spliting
# install.packages("caTools")
library(caTools)
set.seed(123)

# Spliting
split = sample.split(data$total_amount, SplitRatio = 0.8)

train = subset(data, split == TRUE) # Train data
str(train)

test = subset(data, split == FALSE) # Test data 
str(test)



# GBM
set.seed(123)
# install.packages('gbm')
library(gbm)

train = train %>% select(-tpep_pickup_datetime, -tpep_dropoff_datetime)
test = test %>% select(-tpep_pickup_datetime, -tpep_dropoff_datetime)

# model 1 base model
model_gbm1 = gbm(
  formula = total_amount ~.,
  distribution = "gaussian",
  data = train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 10,
  n.cores = NULL,
  verbose = FALSE
)

print(model_gbm1)
min(model_gbm1$train.error)
model1_CV_RMSE = min(model_gbm1$cv.error)
gbm.perf(model_gbm1, method = "cv")


# model_gbm2
model_gbm2 = gbm(
  formula = total_amount ~.,
  distribution = "gaussian",
  data = train,
  n.trees = 5000,
  interaction.depth = 1,
  shrinkage = 0.1,
  cv.folds = 10,
  n.cores = NULL,
  verbose = FALSE
)

print(model_gbm2)
min(model_gbm2$train.error)

model2_CV_RMSE = min(model_gbm2$cv.error)
gbm.perf(model_gbm2, method = "cv")



# grid search
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = c(1,3,5),
  n.minobsinnode = c(5, 7, 10),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

for(i in 1:nrow(hyper_grid)) {
  set.seed(123)

  gbm.tune <- gbm(
    formula = total_amount ~ .,
    distribution = "gaussian",
    data = train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  arrange(min_RMSE) %>%
  head(10)



# Final model
set.seed(123)

final_model_gbm <- gbm(
  formula = total_amount ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 5878,
  interaction.depth = 1,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = .80, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
print(final_model_gbm)


finalmodel_RMSE = mean(final_model_gbm$train.error)
finalmodel_RMSE 
# [1] 0.1148923
train_RMSE = rbind(model1_RMSE, model2_RMSE, finalmodel_RMSE)


# final prediction
y_pred <- predict(final_model_gbm, n.trees = final_model_gbm$n.trees, test)

RMSE(y_pred, test$total_amount)
# [1] 0.3030436
MAE(y_pred, test$total_amount)
# [1] 0.206971

# to get Train MAE
x_pred = predict(final_model_gbm, n.trees = final_model_gbm$n.trees, train)
MAE(x_pred,train$total_amount)
# [1] 0.2246591
