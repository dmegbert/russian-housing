#Russian Housing!
library(tidyverse)
library(mice)
library(xgboost)
library(Matrix)

setwd("~/Rprojects/russian-housing")

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
full <- bind_rows(train, test)

full <- full[c(1,292,2:291)]

View(full)

Find.NA.Vars <- function(data) {
  missing.vars <- lapply(data, function(x) {
    sum(is.na(x)) / length(data)
  })
  missing.vars <- as.data.frame(missing.vars) 
  missing.vars <- as.data.frame(t(missing.vars))
  return(missing.vars) 
}

missing.vars <- Find.NA.Vars(full)
View(missing.vars)

full$timestamp <- as.Date(full$timestamp)
#full$price_doc <- log(full$price_doc)

ggplot(full) +
  geom_smooth(mapping = aes(x = timestamp, y = price_doc))

VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0 & vars != 'price_doc') %>%
  arrange(desc(V1))

View(VarWNA)
## Do some clean up
##NA Absurd values
## life_sq cannot be > full_sq
full$life_sq[full$life_sq > full$full_sq] <- NA
# 6000 sq meters is > 64,000 sq feet
full$life_sq[full$life_sq > 6000] <- NA

#Remove variables with >50% missing except build year and state
vars.removed <- VarWNA %>%
  filter(V1 > 51) %>%
  select(vars)

full <- full[, !names(full) %in% vars.removed$vars]

##NA full_sq:state when full_sq < 2

full[full$full_sq < 2, 4:12] <- NA

## Suspect floors
suspect.floors <- full %>%
  filter(max_floor < floor)

View(suspect.floors)

suspect.floors[suspect.floors$max_floor < 2, 7] <- NA

suspect.floors <- full %>%
  filter(max_floor < floor) %>%
  mutate(tempmax_floor = floor, tempfloor = max_floor) %>%
  select(tempfloor, tempmax_floor, floor, max_floor)

#switch max floors and floor when max < floor
index <- which(full$max_floor < full$floor)
full[index, "max_floor"] <- suspect.floors$tempmax_floor
full[index, "floor"] <- suspect.floors$tempfloor

#life_sq < 2 NA
full$life_sq[full$life_sq <= 2] <- NA

#Replace Max floors that are outliers
full$max_floor[25941] <- 8
full$max_floor[20723] <- 4
full$max_floor[21735] <- 5
full$max_floor[21853] <- 17

#create penthouse variable 
index2 <- which(full$max_floor > 6 & full$max_floor == full$floor)
full$penthouse <- NA
full[index2, "penthouse"] <- 1
full$penthouse[is.na(full$penthouse)] <- 0

grep("penthouse", colnames(full))
full <- full[c(1:7,289,8:288)]

## NA build year that is > 2016
full$build_year[full$build_year > 2016] <- NA

## NA build year that is > timestamp
full$year <- as.Date(full$timestamp, "%Y")
full <- full[c(290,1:289)]
full$year <- as.character(full$year)
full$year <- substr(full$year, 1, 4)
full$year <- as.integer(full$year)
full$build_year[((full$year < full$build_year) & !is.na(full$build_year))] <- NA


### Remove build year prior to 1860
full$build_year[full$build_year < 1860] <- NA

## clean up apartment state
full$state[10090] <- NA
full$state[8963] <- NA
#collapse state 1 and 2
full$state[full$state < 3 & !is.na(full$state)] <- 1
full$state[full$state == 3 & !is.na(full$state)] <- 2
full$state[full$state == 4 & !is.na(full$state)] <- 3
View(full)

##create proportion for 1000 meter 
grep("product_type", colnames(full))
full <- full[c(1:6, 181, 185:191, 7:181, 182:184, 192:289)]

full[, 9:14] <- lapply(full[, 9:14], function(x) x/full$cafe_count_1000)
full[, 9:14]  lapply(full[, 9:14], function(x){ 
  x[is.nan(x)] <- NA 
  x 
})
full[, 9:14] <- lapply(full[, 9:14], function(x){ 
  x[is.na(x)] <- 0 
  x 
}) 

ggplot(full[1:30471, ]) +
  geom_boxplot(mapping = aes(x = factor(radiation_raion), y = log(price_doc))) +
  geom_hline(yintercept = median(log(train$price_doc)), color = "red")

ggplot(full[1:30471, ]) +
  geom_point(mapping = aes(x = cafe_count_1000_price_4000, y = log(price_doc))) +
  geom_hline(yintercept = median(log(train$price_doc)), color = "red") 

#### Let's pause on the cleaning & feature engineering and do some modeling


#Turn Strings into factors

var.class <- lapply(full, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))
View(var.class)

var.class <- var.class %>%
  mutate(var = row.names(var.class)) %>%
  filter(V1 == 'character') %>%
  select(var) 

# Turn chr into factors
full[var.class$var] <- lapply(full[var.class$var], function(x) as.factor(x))

full$product_type <- as.factor(full$product_type)
full$state <- as.factor(full$state)
str(full)




#remove data prior to May 1, 2014
#full <- full %>%
#  filter(timestamp > "2014-05-01")

##Turn timestamp into numeric
full$timestamp <- as.numeric(as.POSIXct(full$timestamp))

full.mice <- full %>%
  select(-price_doc) 

# Set a random seed
set.seed(555)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full.mice, m = 5, method = 'rf')  

mice.output <- mice::complete(mice_mod)

# Plot histograms for continuous data
par(mfrow = c(1,2))
hist(full$state, freq = F, main = 'kitch_sq: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(mice.output$state, freq = F, main = 'kitch_sq: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))


par(mfrow = c(1,2))
barplot(prop.table(table(full$state)))
barplot(prop.table(table(mice.output$state)))

#vars that have good distro in mice model
mice.vars.replace <- c("full_sq","life_sq","floor","max_floor", 
                       "material", "build_year", "num_room", "kitch_sq",
                       "state", "product_type")

#put in imputed values for the above variables
full[, mice.vars.replace] <- mice.output[, mice.vars.replace]

missing.vars <- Find.NA.Vars(full)

VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0 & vars != 'price_doc') %>%
  arrange(desc(V1))

View(VarWNA)

#now no more NAs
full <- full[, !names(full) %in% VarWNA$vars]

#Prep to convert into sparse matrix

var.class <- lapply(full, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))
View(var.class)

var.class <- var.class %>%
  mutate(var = row.names(var.class)) %>%
  filter(V1 == 'character') %>%
  select(var) 


# Convert to a matrix

# Splitting back to train and test sets
train <- full[1:30471, c(1,3:250)]
test <- full[30472:38133, c(1,4:250)]


train.mtx <- sparse.model.matrix(~., data = train)
test.mtx <- sparse.model.matrix(~., data = test)


bst <- xgboost(data = train.mtx[, -c(1, 3)], label = train.mtx[, c(3)],  nthread = 2, nround = 15, verbose = 1)

# Get the feature real names
names <- dimnames(train.mtx[, -c(1, 3)])[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Plotting
par(mfrow = c(1,1))
xgb.plot.importance(importance_matrix)

# Prediction on test and train sets
pred_xgboost_test <- predict(bst, test.mtx[, -c(1)])
pred_xgboost_train <- predict(bst, train[, -c(1)])

results <- as.data.frame(cbind(train$price_doc, model.rf2$predicted))
results <- results %>%
  mutate(V1 = log(V1),
         V2 = log(V2),
         error = V1 - V2)

View(results)

id.submission <- as.data.frame(full$id[30472:38133])
submission <- as.data.frame(pred_xgboost_test)
submission <- bind_cols(id.submission, submission)

write.csv(submission, file = 'xg.mod.Solution2-6-4-17.csv', row.names = F)

##Let's try it again with only the most important features

important.vars <- importance_matrix %>%
  filter(Importance > .0010043172) %>%
  select(Feature)

full <- full[, names(full) %in% important.vars$Feature]

price.full <- bind_rows(train, test)
price.full <- select(price.full, price_doc)
full <- bind_cols(price.full, full)

# Convert to a matrix

# Splitting back to train and test sets
train <- full[1:30471, ]
test <- full[30472:38133, -c(1)]

train.mtx <- sparse.model.matrix(~., data = train)
test.mtx <- sparse.model.matrix(~., data = test)

bst <- xgboost(data = train.mtx[, -c(1:2)], label = train.mtx[, c(2)],  nthread = 2, nround = 15, verbose = 1)

# Get the feature real names
names <- dimnames(train.mtx[, -c(1, 2)])[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model = bst)

# Plotting
par(mfrow = c(1,1))
xgb.plot.importance(importance_matrix)

# Prediction
pred_xgboost_test <- predict(bst, test.mtx[, -c(1)])

id.submission <- as.data.frame(test$id)
submission <- as.data.frame(pred_xgboost_test)
submission <- bind_cols(id.submission, submission)

write.csv(submission, file = 'xg.mod.Solution3-6-4-17.csv', row.names = F)
