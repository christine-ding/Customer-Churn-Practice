# Homework 5

setwd()
churn <- read.csv('DSBchurn.CSV')
str(churn)

# Fix the order of levels of factors
churn$college <- factor(churn$college, levels = c("zero", "one"))
churn$rep_sat <- factor(churn$rep_sat, levels = c("very_unsat", "unsat", "avg", "sat", "very_sat"))
churn$rep_usage <- factor(churn$rep_usage, levels = c("very_little", "little", "avg", "high", "very_high"))
churn$rep_change <- factor(churn$rep_change, levels = c("never_thought", "no", "considering", "perhaps", "actively_looking_into_it"))
colnames(churn)[12] <- "leave" 

# Save the data frame as DSBchurn.Rda
save(churn, file='DSBchurn.Rda')

# Create randomly sampled training and test data sets with
# about 66.7% and 33.3% of the observations.
set.seed(3478)
train = sample(1:nrow(churn), nrow(churn)*0.667)
churn.train = churn[train,]
churn.test = churn[-train,]

# Grow a tree
library(rpart)
library(caret)

fit = rpart(leave ~ .,
            data = churn.train,
            method = "class",
            control=rpart.control(xval=0, minsplit=100))
fit

# Explain rows numbered 1, 10, and 3.
# No.1 is the root node, means no split happen here. The total observation is 13340, 6525 leave and 6815 stay. Stay is the majority here. 
# No.10 is a node with leftover>=24.5. This node comes after house <604440.5 and then overage <97.5. The total observation is 1995 out of 5910, 1221 leave and 774 stay. Leave is the majority. 
# No.3 is a node with house>=604440.5. This node is the first split from root, since house at cutoff value 604440.5 is the feature with best gain. The total observation is 4461 out of 13340, 1379 leave and 3082 stay. Stay is the majority.

# Plot and label the tree
plot(fit, uniform=TRUE, 
     branch=0.5,
     compress=F,
     main="Classification Tree for Churn Prediction", 
     margin=0.1)
text(fit, use.n=TRUE, # show numbers for each class
     all=TRUE, # show data for internal nodes as well
     fancy=F, # draw ovals and boxes
     pretty=T, # , # show split details
     cex=0.8) # compress fonts to 60%) 

# Confusion matrix for the test data set
churn.pred = predict(fit, churn.test, type="class")
churn.actual = churn.test$leave
confusion.matrix = table(churn.pred, churn.actual)
confusion.matrix

confusionMatrix(confusion.matrix, positive='STAY')
# Accuracy: 0.6889
# Hold-out Error rate: 1-0.6889=0.3111
# False Positive Rate: 877/3327=0.2636
# False Negative Rate: 1195/3333=0.3585
# Recall/Sensitivity: 0.6415
# Specificity: 0.7364
# Precision/Pos Pred Value: 0.7091

