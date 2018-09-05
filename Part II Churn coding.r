# Homework 6

setwd("~/path")
load('DSBchurn.Rda')

# 1. Split the data into training and test data sets with 66.7% and 33.3% of the observations
set.seed(3478)
train = sample(1:nrow(churn), nrow(churn)*0.667)
churn.train = churn[train,]
churn.test = churn[-train,]

# ==============================================
# 2. Use rpart to build a large complex tree with a low value of cp and minsplit.
library(rpart)
library(caret)

fit <- rpart(leave ~ .,
            data = churn.train,
            method = "class",
            control=rpart.control(xval=10, minsplit=2, cp=0))

nrow(fit$frame) # 5663 nodes

churn.pred = predict(fit, churn.train, type="class")
churn.actual = churn.train$leave
confusion.matrix = table(churn.pred, churn.actual)
confusion.matrix

confusionMatrix(confusion.matrix, positive='STAY')

# determine the ¡°Big Tree¡± error rates (FPR and FNR) using the test data set
churn.pred1 = predict(fit, churn.test, type="class")
churn.actual1 = churn.test$leave
confusion.matrix1 = table(churn.pred1, churn.actual1)
confusion.matrix1

confusionMatrix(confusion.matrix1, positive='STAY')

# FPR = 1348/(1979 + 1348) = 0.4051698
# FNR = 1187/(1187 + 2146) = 0.3561356

# ==============================================
# 3. Find the best cp value to post-prune the tree
plotcp(fit, upper="size")
fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]

# It appears that lowest error occurs at CP = 0.002298851

# Use the test data set to find the ¡°Pruned Tree¡± error rates
# POST PRUNING
fit.post = prune.rpart(fit, cp=0.002298851)
nrow(fit.post$frame) # 19 nodes

plot(fit.post, uniform=T, branch=0.5, compress=T,
     main="Tree with Post Pruning with best cp (19 Nodes)", margin=0.05)
text(fit.post, splits=T, all=F, use.n=T,
     pretty=T, fancy=F, cex=0.8)

# resubstitution
confusionMatrix(table(predict(fit.post, churn.train, type="class"),
                      churn.train$leave), positive='STAY')

# test set
confusionMatrix(table(predict(fit.post, churn.test, type="class"),
                      churn.test$leave), positive='STAY')

# FPR = 922/(2405 + 922) = 0.2771265
# FNR = 1152/(1152 + 2181) = 0.3456346

# ==============================================
# 4. Use ROCR to find the best threshold
library(ROCR)

churn.pred.train = predict(fit.post, churn.train, type="prob")
churn.pred.score = prediction(churn.pred.train[,2], churn.train$leave) 
churn.pred.perf = performance(churn.pred.score, "tpr", "fpr")

plot(churn.pred.perf,
     colorize=T, # colorize to show cutoff values
     lwd=4) # make the line 4 times thicker than default
abline(0,1) # draw a line with intercept=0, slope = 1
abline(h=1) #draw a horizontal line at y = 1
abline(v=0) #draw a vertical line at x = 0

# area under the curve (AUC)
performance(churn.pred.score, "auc")@y.values # 0.7648281

churn.cost = performance(churn.pred.score, measure="cost", 
                         cost.fn=490000*400, cost.fp=510000*100)
plot(churn.cost)

churn.pred.score@cutoffs[[1]][which.min(churn.cost@y.values[[1]])] # The best cutoff is 0.2068036

churn.pred.test = predict(fit.post, churn.test, type="prob")
churn.pred.test.cutoff =
  ifelse(churn.pred.test[,2] < 0.2068036,'LEAVE','STAY')

cm = confusionMatrix(table(pred = as.factor(churn.pred.test.cutoff),
                           actual = churn.test$leave), positive='STAY')
cm

# profit increases to 490,348,201
541000*1000 - 510000*100 * (1-cm$byClass["Specificity"][[1]]) - 490000*400 * (1-cm$byClass["Sensitivity"][[1]])

# Or using another method
# FPR(a) = 3147/(3147 + 180) = 0.9458972
# FNR(b) = 41/(41 + 3292) = 0.01230123
# Expected	Profit = 541000*1000 ¨C 510000*100 a ¨C 490000*400 b = 490,348,202

# Question 5 shows in the Calculation Document

