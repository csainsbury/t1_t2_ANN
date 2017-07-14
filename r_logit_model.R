# Classification template

# Importing the dataset
dataset = read.csv("~/R/_workingDirectory/t1_t2_ANN/diagSet_7p_withID.csv")
testData <- read.csv("~/R/_workingDirectory/t1_t2_ANN/outputFrame.csv")

testDataIDframe <- data.frame(testData$LinkId, testData$bmi); colnames(testDataIDframe) <- c('LinkId', 'bmi')
testDataIDframe$flagInTestSet <- 1

datasetWithFlag <- merge(dataset, testDataIDframe, by.x = c('LinkId', 'bmi'), by.y = c('LinkId', 'bmi'), all.x = T)
datasetWithFlag$flagInTestSet[is.na(datasetWithFlag$flagInTestSet)] <- 0

trainGroupProp = 0.8
testGroupProp = 1 - trainGroupProp

# Encoding the target feature as factor
dataset$diabetesType = factor(dataset$diabetesType, levels = c(0, 1))
dataset$ethnicity = factor(dataset$ethnicity)
dataset$sex = factor(dataset$sex)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)

# find number of rows to select for test group
numberOfRowsForTest <- round((nrow(dataset) * testGroupProp) - (nrow(subset(datasetWithFlag, flagInTestSet == 1))), 0)

# rows not in testset sample:
nonSampleRows <- subset(datasetWithFlag, flagInTestSet == 0)
testSet_withoutSampleRows <- nonSampleRows[sample(nrow(nonSampleRows), numberOfRowsForTest), ]
testSet <- rbind(subset(datasetWithFlag, flagInTestSet == 1), testSet_withoutSampleRows)

# split = sample.split(dataset$diabetesType, SplitRatio = 0.8)
# training_set = subset(dataset, split == TRUE)
# test_set = subset(dataset, split == FALSE)

test_set <- data.frame(testSet$age, testSet$ethnicity, testSet$sex, testSet$hba1c, testSet$sbp, testSet$dbp, testSet$bmi, testSet$diabetesType)
colnames(test_set) <- c('age', 'ethnicity', 'sex', 'hba1c', 'sbp', 'dbp', 'bmi','diabetesType')

# generate training set
testSet$inFullTestSet <- 1
allDataFlaggedForTestSet <- merge(datasetWithFlag, testSet, by.x  = c('LinkId', 'bmi', 'hba1c', 'sbp'), by.y = c('LinkId', 'bmi', 'hba1c', 'sbp'), all.x = T)
allDataFlaggedForTestSet$inFullTestSet[is.na(allDataFlaggedForTestSet$inFullTestSet)] <- 0

trainSet <- subset(allDataFlaggedForTestSet, inFullTestSet == 0)

training_set <- data.frame(trainSet$age.x, trainSet$ethnicity.x, trainSet$sex.x, trainSet$hba1c, trainSet$sbp, trainSet$dbp.x, trainSet$bmi, trainSet$diabetesType.x)
colnames(training_set) <- c('age', 'ethnicity', 'sex', 'hba1c', 'sbp', 'dbp', 'bmi','diabetesType')

    ## for export for ann
    datasetWithFlag_forExport <- data.frame(allDataFlaggedForTestSet$age.x, allDataFlaggedForTestSet$ethnicity.x, allDataFlaggedForTestSet$sex.x, allDataFlaggedForTestSet$hba1c, allDataFlaggedForTestSet$sbp, allDataFlaggedForTestSet$dbp.x, allDataFlaggedForTestSet$bmi, allDataFlaggedForTestSet$inFullTestSet, allDataFlaggedForTestSet$diabetesType.x)
    colnames(datasetWithFlag_forExport) <- c('age', 'ethnicity', 'sex', 'hba1c', 'sbp', 'dbp', 'bmi', 'inFullTestSet','diabetesType')
    
    write.table(datasetWithFlag_forExport, file = "~/R/_workingDirectory/t1_t2_ANN/datasetWithFlag_forExport.csv", sep = ",", row.names = FALSE, col.names = TRUE)

# Feature Scaling
training_set[1] = scale(training_set[1])
training_set[4:7] = scale(training_set[4:7])

test_set[1] = scale(test_set[1])
test_set[4:7] = scale(test_set[4:7])

test_data <- test_set[1:nrow(subset(datasetWithFlag, flagInTestSet == 1)), ]

# Fitting Kernel SVM to the Training set
# Create your classifier here

classifier = glm(formula = diabetesType ~.,
                 family = binomial,
                 data = training_set)

# analyse full test set
# test_set <- test_set
# analyse samples from physician tests
test_set <- test_data

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-8])

y_pred = ifelse(prob_pred > 0.05, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 8], y_pred)
print(cm)
accuracy = (cm[1, 1] + cm[2, 2]) / sum(cm)
print(accuracy)

library(ROCR)

pred <- prediction(prob_pred, test_set[, 8])
roc.perf <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(roc.perf)

auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(roc.perf, pred))

exportLogit <- data.frame(subset(datasetWithFlag, flagInTestSet == 1), prob_pred)
write.table(exportLogit, file = "~/R/_workingDirectory/t1_t2_ANN/logit_output.csv", sep = ",", row.names = FALSE, col.names = TRUE)

performanceAnalysis(y_pred, prob_pred, exportLogit$diabetesType, 0.1)


