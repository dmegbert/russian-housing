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

vars.w.na <- VarWNA$vars
class(vars.w.na)

quick.clean <- train[, !names(train) %in% vars.w.na]
View(quick.clean)

missing.vars <- Find.NA.Vars(quick.clean)
View(missing.vars)

#find chr variables
var.class <- lapply(quick.clean, class)
var.class <- as.data.frame(var.class)
var.class <- as.data.frame(t(var.class))
View(var.class)

var.class <- var.class %>%
  mutate(var = row.names(var.class)) %>%
  filter(V1 == 'character') %>%
  select(var) 

# Turn chr into factors
quick.clean[var.class$var] <- lapply(quick.clean[var.class$var], function(x) as.factor(x))

str(quick.clean)

#remove sub_area
quick.clean <- quick.clean %>%
  select(-sub_area)

ggplot(train, mapping = aes(x = factor(product_type), y = price_doc)) + 
  geom_boxplot() +
  geom_hline(yintercept = median(train$price_doc), color = "red")

model.rf1 <- randomForest(price_doc ~ ., data = quick.clean, ntree = 150, do.trace = 10)
#error plot
plot(model.rf1)

# Variable Importance Plot
varImpPlot(model.rf1,
           sort = T,
           main="Variable Importance",
           n.var=20)

results <- as.data.frame(cbind(quick.clean$price_doc, model.rf1$predicted))
results <- results %>%
  mutate(V1 = log(V1),
         V2 = log(V2),
         error = V1 - V2)

View(results)

sqrt(mean(results$error^2))
#Ok, time to predict!

prediction <- predict(model.rf1, test)
str(prediction)

submission <- as.data.frame(prediction)
submission <- as.data.frame(cbind(test$Id, prediction), col.names = c("Id", "SalePrice"))

submission <- submission %>%
  rename(Id = V1, SalePrice = prediction)

View(submission)
write.csv(submission, file = "submissionRF1-5-14-17.csv", row.names = FALSE)


