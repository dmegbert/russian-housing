#Russian Housing!
# Use most of all variables
library(tidyverse)
library(mice)
library(randomForest)

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

#Remove Sub area - too many levels for a factor for RF
#remove sub_area
full <- full %>%
  select(-sub_area)

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

full[, 8:14] <- lapply(full[, 8:14], function(x) x/full$cafe_count_1000)
full[, 8:14]  lapply(full[, 8:14], function(x){ 
  x[is.nan(x)] <- NA 
  x 
})
full[, 8:14] <- lapply(full[, 8:14], function(x){ 
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

vars.drop <- VarWNA[VarWNA$V1 > 17, "vars"]
full <- full[, !names(full) %in% vars.drop]

missing.vars <- Find.NA.Vars(full)
VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0 & vars != 'price_doc') %>%
  arrange(desc(V1))


full[, VarWNA$vars] <- lapply(
  full[, VarWNA$vars], 
    function(x) {
      x[is.na(x)] <- median(x, na.rm = TRUE)   
  })

full <- full[, -c(1)]


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
hist(full.mice$state, freq = F, main = 'kitch_sq: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(mice.output$state, freq = F, main = 'kitch_sq: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))


par(mfrow = c(1,2))
barplot(prop.table(table(full.mice$material)))
barplot(prop.table(table(mice.output$material)))

#vars that have good distro in mice model
mice.vars.replace <- c("full_sq","life_sq","floor","max_floor", 
                       "material", "build_year", "num_room", "kitch_sq",
                       "state", "product_type")

#put in imputed values for the above variables
full[, mice.vars.replace] <- mice.output[, mice.vars.replace]
str(full)

## Split the data back into train and test
train <- full[1:30471, ]
test <- full[30472:38133, ]

model.rf <- randomForest(price_doc ~ ., data = train , ntree = 500, do.trace = 10)
#error plot
plot(model.rf)

par(mfrow = c(1,1))
# Variable Importance Plot
varImpPlot(model.rf,
           sort = T,
           main = "Variable Importance",
           n.var = 25)

results <- as.data.frame(cbind(train$price_doc, model.rf$predicted))
results <- results %>%
  mutate(V1 = log(V1 + 1),
         V2 = log(V2 + 1),
         error = (V1 - V2))

View(results)

sqrt(mean(results$error^2))
#Ok, time to predict!


prediction <- predict(model.rf, test)


submission <- as.data.frame(prediction)
submission <- as.data.frame(cbind(test$id, prediction), col.names = c("id", "price_doc"))
View(submission)

submission <- submission %>%
  rename(id = V1, price_doc = prediction)


View(submission)


write.csv(submission, file = "RussianSubmissionAllData-RF2-5-29-17.csv", row.names = FALSE)

##Add the macro data

macro <- read.csv(file = "macro.csv")
View(macro)

#check NAs
missing.vars <- Find.NA.Vars(full)
missing.vars <- Find.NA.Vars(macro)
VarWNA <- missing.vars %>%
mutate(vars = rownames(missing.vars)) %>%
filter(V1 > 0 & vars != 'price_doc') %>%
arrange(desc(V1))
View(VarWNA)

macro$timestamp <- as.Date(macro$timestamp)

var.class <- lapply(macro, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))
View(var.class)

macro$timestamp <- as.numeric(as.POSIXct(macro$timestamp))

# Set a random seed
set.seed(432)

# Perform mice imputation, excluding certain less-than-useful variables:
mice.macro <- mice(macro, m = 3, method = 'rf')  

mice.output <- mice::complete(mice.macro)

# Plot histograms for continuous data
par(mfrow = c(1,2))
hist(macro$employment, freq = F, main = 'employment: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(macro$employment, freq = F, main = 'employment: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))

# grp_growth
par(mfrow = c(1,2))
hist(macro$grp_growth, freq = F, main = 'grp_growth: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(macro$grp_growth, freq = F, main = 'grp_growth: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))

par(mfrow = c(1,2))
barplot(prop.table(table(full.mice$material)))
barplot(prop.table(table(mice.output$material)))

#put in imputed values for all of the vars with NAs
macro[, VarWNA$vars] <- mice.output[, VarWNA$vars]

missing.vars <- Find.NA.Vars(macro)

VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0)

#Get rid of vars that did not get imputed for some reason...
macro <- macro[, !names(macro) %in% VarWNA$vars]

##Turn timestamp back into Date
macro$timestamp <- as.Date(as.POSIXct(macro$timestamp, origin = "1970-01-01"))

## Join to full data now that there are no NA

full.macro <- dplyr::inner_join(full, macro, by = "timestamp")
full.macro <- full.macro[, -c(1)]
View(full.macro)

## Split the data back into train and test
train.macro <- full.macro[1:30471, ]
test.macro <- full.macro[30472:38133, ]

model.rf.macro <- randomForest(price_doc ~ ., data = train.macro , ntree = 500, do.trace = 10)

# Last one - 500 | 7.081e+12    30.99 |

#error plot
plot(model.rf.macro)

par(mfrow = c(1,1))
# Variable Importance Plot
varImpPlot(model.rf.macro,
           sort = T,
           main = "Variable Importance",
           n.var = 30)

results.macro <- as.data.frame(cbind(train.macro$price_doc, model.rf.macro$predicted))
results.macro <- results.macro %>%
  mutate(V1 = log(V1 + 1),
         V2 = log(V2 + 1),
         error = (V1 - V2))

View(results.macro)

sqrt(mean(results.macro$error^2))
#Ok, time to predict!


prediction.macro <- predict(model.rf.macro, test.macro)


submission.macro <- as.data.frame(prediction.macro)
submission.macro <- as.data.frame(cbind(test$id, prediction.macro), col.names = c("id", "price_doc"))
View(submission)

submission <- submission %>%
  rename(id = V1, price_doc = prediction)


View(submission)


#write.csv(submission, file = "RussianSubmissionAllAndMacroData-RF2-5-30-17.csv", row.names = FALSE)

