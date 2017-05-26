#Russian Housing!
library(tidyverse)
library(mice)
library(randomForest)

setwd("~/Rprojects/russian-housing")

train <- read.csv("train.csv", stringsAsFactors = FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)
full <- bind_rows(train, test)

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
full$price_doc <- log(full$price_doc)

ggplot(full) +
  geom_smooth(mapping = aes(x = timestamp, y = price_doc))

VarWNA <- rownames(missing.vars$V1[missing.vars$V1 > 0])

VarWNA <- missing.vars %>%
  mutate(vars = rownames(missing.vars)) %>%
  filter(V1 > 0 & vars != 'price_doc') %>%
  arrange(desc(V1))

View(VarWNA)

ggplot() +
  geom_point(data = full, mapping = aes(x = life_sq, y = price_doc))


#Remove variables with >50% missing except build year and state

vars.removed <- VarWNA %>%
  filter(V1 > 51) %>%
  select(vars)

full <- full[, !names(full) %in% vars.removed$vars]

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

str(full)


#Remove Sub area - too many levels for a factor for RF
#remove sub_area
full <- full %>%
  select(-sub_area)

#remove data prior to May 1, 2014
full <- full %>%
  filter(timestamp > "2014-05-01")

full.mice <- full %>%
  select(-price_doc) %>%
  select(-timestamp)

# Set a random seed
set.seed(555)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full.mice, m = 3, method = 'rf')  

mice_output <- mice::complete(mice_mod)

# Plot histograms for continuous data
par(mfrow = c(1,2))
hist(full.mice$kitch_sq, freq = F, main = 'kitch_sq: Original Data', 
     col = 'darkgreen', ylim = c(0,0.04))
hist(mice_output$kitch_sq, freq = F, main = 'kitch_sq: MICE Output', 
     col = 'lightgreen', ylim = c(0,0.04))


par(mfrow = c(1,2))
barplot(prop.table(table(full.mice$product_type)))
barplot(prop.table(table(mice_output$product_type)))

#vars that have good distro in mice model
mice.vars.replace <- c("full_sq","life_sq","floor","max_floor", 
                       "material", "build_year", "num_room", "kitch_sq",
                       "state", "product_type")

#put in imputed values for the above variables
full[, mice.vars.replace] <- mice_output[, mice.vars.replace]
str(full)

### Do some feature engineering around floor -- maybe penthouse/grd floor, etc
### sky scraper and other bldg features
### tallest building in Russia has 95 stories so any >95 data is bunk
### Clean up outliers and other junk data
bunk <- full[full$max_floor > 95, ]
View(bunk)
#####




summary(full)
md.pattern(full)
View(full)

## Model Variables

model.vars <- c("full_sq","life_sq","floor","max_floor", 
                "material", "build_year", "num_room", "kitch_sq",
                "state", "product_type", "timestamp", "cafe_count_500_price_500",
                "cafe_count_500_price_1000",	
                "cafe_count_500_price_1500",
                "cafe_count_500_price_2500",
                "cafe_count_500_price_4000",
                "cafe_count_500_price_high")

train <- full[!is.na(full$price_doc), c(model.vars, "price_doc")]
train$price_doc <- exp(train$price_doc)
View(train)
model.rf1 <- randomForest(price_doc ~ ., data = train , ntree = 150, do.trace = 10)
model.rf2 <- randomForest(price_doc ~ ., data = train , ntree = 500, do.trace = 10)
#error plot
plot(model.rf1)
plot(model.rf2)

par(mfrow = c(1,1))
# Variable Importance Plot
varImpPlot(model.rf1,
           sort = T,
           main="Variable Importance",
           n.var=17)

# Variable Importance Plot
varImpPlot(model.rf2,
           sort = T,
           main="Variable Importance",
           n.var=17)

results <- as.data.frame(cbind(train$price_doc, model.rf2$predicted))
results <- results %>%
  mutate(V1 = log(V1),
         V2 = log(V2),
         error = V1 - V2)

View(results)

sqrt(mean(results$error^2))
#Ok, time to predict!

test.predict <- full[is.na(full$price_doc), c(model.vars, "price_doc")]



prediction <- predict(model.rf2, test.predict)
View(test)

submission <- as.data.frame(prediction)
submission <- as.data.frame(cbind(test$id, prediction), col.names = c("id", "price_doc"))

submission <- submission %>%
  rename(id = V1, price_doc = prediction)

View(submission)
write.csv(submission, file = "RussianSubmissionRF1-5-25-17.csv", row.names = FALSE)
