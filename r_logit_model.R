# Classification template

# Importing the dataset
dataset = read.csv("~/R/_workingDirectory/t1_t2_ANN/diagSet_7p.csv")

# Encoding the target feature as factor
dataset$diabetesType = factor(dataset$diabetesType, levels = c(0, 1))
dataset$ethnicity = factor(dataset$ethnicity)
dataset$sex = factor(dataset$sex)



# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$diabetesType, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[1] = scale(training_set[1])
training_set[4:7] = scale(training_set[4:7])

test_set[1] = scale(test_set[1])
test_set[4:7] = scale(test_set[4:7])

# Fitting Kernel SVM to the Training set
# Create your classifier here

classifier = glm(formula = diabetesType ~.,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-8])

y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred)
print(cm)
accuracy = (cm[1, 1] + cm[2, 2]) / sum(cm)
print(accuracy)


pred <- prediction(prob_pred, test_set[, 8])
roc.perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc.perf)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values


