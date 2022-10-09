#install.packages("dplyr")
#install.packages("mlbench")

library(dplyr)
library(mlbench)

data("BreastCancer")

df <- BreastCancer
head(df)
glimpse(df)

## --------- Clean data --------- 
# Drop NA
df <- na.omit(df)
mean(complete.cases(df)) #check if mean = 1 so na is dropped

# logistic use to predict factor so label class into 0, 1

df$Class <- factor(df$Class,
                       levels = c("benign", "malignant"),
                       labels = c(0, 1))
glimpse(df)


## --------- Train, Test, Split --------- 
# ---------- Split Data ---------- 
n <- nrow(df)
set.seed(42)
id <- sample(1:n, size=n*0.7) #train 70%, test 30%
train_data <- df[id, ]
test_data <- df[-id, ]

glimpse(train_data)

# ---------- Train Model ---------- 
glm_model <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, data = train_data, family = "binomial")
summary(glm_model)

# Predict Train
train_data$prob_Class <- predict(glm_model, type = "response")
View(train_data)

# because prob_Class is number between 0 - 1 so need to predict if >= 0.5 is maybe cancer
train_data$pred_Class <- ifelse(train_data$prob_Class >= 0.5, 1, 0)

# Confusion matrix

train_conm <- table(train_data$pred_Class, train_data$Class, dnn = c("Predicted", "Actual"))

accuracy <- (train_conm[1, 1]+train_conm[2, 2])/sum(train_conm)
cat("Accuracy : ", accuracy)

precision <- train_conm[2, 2]/(train_conm[2, 1]+train_conm[2, 2])
cat("Precision : ", precision)

recall_conm <- train_conm[2, 2]/(train_conm[1, 2]+train_conm[2, 2])
cat("Recall : ", recall_conm)

f1_conm <- 2*(precision*recall_conm/(precision+recall_conm))
cat("F1 : ", f1_conm)


# ---------- Test Model ---------- 

# Predict Test
test_data$prob_Class <- predict(glm_model, newdata = test_data, type = "response")

test_data$pred_Class <- ifelse(test_data$prob_Class >= 0.5, 1, 0)

# Confusion matrix

test_conm <- table(test_data$pred_Class, test_data$Class, dnn = c("Predicted", "Actual"))

test_accuracy <- (test_conm[1, 1]+test_conm[2, 2])/sum(test_conm)
cat("Accuracy : ", test_accuracy)

test_precision <- test_conm[2, 2]/(test_conm[2, 1]+test_conm[2, 2])
cat("Precision : ", test_precision)

test_recall_conm <- test_conm[2, 2]/(test_conm[1, 2]+test_conm[2, 2])
cat("Recall : ", test_recall_conm)

test_f1_conm <- 2*(test_precision*test_recall_conm/(test_precision+test_recall_conm))
cat("F1 : ", test_f1_conm)

## print summary result to compare train-test
cat("Accuracy : Train = ", accuracy, "Test = ", test_accuracy)
cat("Precision : Train = ", precision, "Test = ", test_precision)
cat("Recall : Train = ", recall_conm, "Test = ", test_recall_conm)
cat("F1 : Train = ", f1_conm, "Test = ", test_f1_conm)


