

 
library(readr)    #Data Input Functions
library(dplyr)    #Data manipulation functions
library(ggplot2)  #Plotting Functions
library(tidyr)  #Data wranging functions
library(rpart)  #Classification Tree functions
library(ROCR)   #ROC plotting Functions
library(randomForest)  # Bagging/Random Forest Functions
library(gbm)    # Boosted Tree functions
library(class)  #KNN Class Functions
library(MASS)   # LDA/QDA models

# ROC curve plotting function
rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
  #compute area under curve 
  auc <- performance(predob,"auc")   
  ( auc <- unlist(slot(auc, "y.values")) )
}


# Read in the mushrooms data
mush = read.csv("mushrooms.csv", stringsAsFactors = TRUE, na = "?")
summary(mush)

# Some column names are too long, adjusting
colnames(mush)[which(colnames(mush) == "stalk.surface.above.ring")] = "stalk.surface.ar"
colnames(mush)[which(colnames(mush) == "stalk.surface.below.ring")] = "stalk.surface.br"
colnames(mush)[which(colnames(mush) == "stalk.color.above.ring")] = "stalk.color.ar"
colnames(mush)[which(colnames(mush) == "stalk.color.below.ring")] = "stalk.color.br"

# Removing veil.type
mush$veil.type = NULL

# Grouping feature levels
mush$odor = as.factor(ifelse(mush$odor == "n", "No", "Yes"))

# Changing numeric types
mush$ring.number = ifelse(mush$ring.number == "n", 0, ifelse(mush$ring.number == "o", 1, 2))

# Creating a data set w/o NAs
mush_cleaned = na.omit(mush)

# Generating a ~75% training set logical
set.seed(12)
train = sample(c(TRUE, TRUE, TRUE, FALSE), nrow(mush_cleaned), rep=TRUE)


####  rpart Tree model
par(mfrow = c(1,2))
r_tree = rpart(class ~ ., data = mush_cleaned, subset = train, method = "class", cp = 0.00001)
r_tree
plot(r_tree, compress=T,uniform=T,branch=0.4,margin=.10)
text(r_tree)

# Plotting the tree
printcp(r_tree, digits=3)
r_tree$cptable
plotcp(r_tree,las=2)

# Tree effectively bottoms out at 4 splits, which is the minimum CV error.
# Now, essentially repeating the above with the entire data set with the
# stalk.root variable omitted.

mush$stalk.root = NULL
set.seed(1)
train = sample(c(TRUE, TRUE, TRUE, FALSE), nrow(mush), rep=TRUE)

par(mfrow = c(1,2))
r_tree = rpart(class ~ ., data = mush, subset = train, method = "class", cp = 0.00001)
r_tree
plot(r_tree, compress=T,uniform=T,branch=0.4,margin=.10)
text(r_tree)

printcp(r_tree, digits=3)
r_tree$cptable
plotcp(r_tree,las=2)

# pruning back to 7 nodes
par(mfrow = c(1,2))
r_tree_pruned <- prune.rpart(r_tree,0.005)
plot(r_tree_pruned,compress=T,uniform=T,branch=0.4,margin=.10)
text(r_tree_pruned)

r_tree_pred = predict(r_tree_pruned, mush[!train,], type = "class", cp = 0.005)
table(r_tree_pred, mush$class[!train])
mean(r_tree_pred != mush$class[!train])*100

# Plotting the ROC Curve
r_tree_probs = predict(r_tree_pruned, mush[!train,], type="prob", cp = 0.005)
r_tree_probs = r_tree_probs[,2]
actuals = ifelse(mush[!train,1] == "p", 1, 0)
rocplot(r_tree_probs, actuals, col="red")
abline(0,1, col="grey")


#### Bagging Model
set.seed(1)
bag_mush = randomForest(class ~ ., data = data.frame(mush[train,]), mtry = 20, importance = TRUE, ntree=25)
plot(bag_mush)
legend("top", colnames(bag_mush$err.rate),col=1:4,cex=0.8,fill=1:4)
bag_pred = predict(bag_mush, mush[!train,], type = "class")
table(bag_pred, mush[!train,1])
# Bagged ROC
bag_probs = predict(bag_mush, mush[!train,], type="prob")
bag_probs = bag_probs[,2]
actuals = ifelse(mush[!train,1] == "p", 1, 0)
rocplot(bag_probs, actuals, col="red")
abline(0,1, col="grey")


####  Random Forest Model
set.seed(1)
rf_mush = randomForest(class ~ ., data = data.frame(mush[train,]), mtry = sqrt(20), importance = TRUE,ntree=20)
plot(rf_mush)
legend("top", colnames(rf_mush$err.rate),col=1:4,cex=0.8,fill=1:4)
rf_pred = predict(rf_mush, mush[!train,], type = "class")
table(rf_pred, mush[!train,1])
# RF ROC
rf_probs = predict(rf_mush, mush[!train,], type="prob")
rf_probs = rf_probs[,2]
actuals = ifelse(mush[!train,1] == "p", 1, 0)
rocplot(rf_probs, actuals, col="red")
abline(0,1, col="grey")


# Boosted Model
set.seed(1)

# Converting bases to binary numerical classes
mush_class_bin = ifelse(mush$class == "p", 1, 0)
mush_bin = mush
mush_bin$class = mush_class_bin 

# Checking the CV dependence on interaction depth
n_trees = 500
boost_cv = data.frame(tree = 1:n_trees)
for (i in 1:9) {
  boost_mush = gbm(class ~ ., data = mush_bin[train,], distribution = "bernoulli", n.trees = n_trees, interaction.depth = i, cv.folds = 5)
  c_name = paste("Depth", i)
  boost_cv = cbind(boost_cv, c_name=boost_mush$cv.error)
  colnames(boost_cv)[i+1] = c_name
} 
boost_cv = gather(boost_cv, key = "Depth", value = "Value", -tree)

# Plotting the CV dependence curves
ggplot(boost_cv, aes(x = tree, y = Value, col = factor(Depth))) + 
  geom_line(size = 2) +
  scale_x_continuous("Number of Trees") +
  scale_y_continuous("5-Fold CV Error") +
  scale_color_discrete("Interaction Depth")

# Preparing the final boosted model
boost_mush = gbm(class ~ ., data = mush_bin[train,], distribution = "bernoulli", n.trees = 5000, interaction.depth = 6, cv.folds = 5)
plot(boost_mush$cv.error, xlab = "Number of Trees", ylab = "5-Fold CV Error")
summary(boost_mush)
boost_pred = predict(boost_mush, mush_bin[!train,], type = "response", n.trees = 5000)
boost_class = as.factor(ifelse(boost_pred >= 0.3 , "p", "e"))
table(boost_class, mush[!train,1])
# Boosted ROC
actuals = ifelse(mush[!train,1] == "p", 1, 0)
rocplot(boost_pred, actuals, col="red")
abline(0,1, col="grey")



#### Parametric Models
### Logistic Model

# This will not work
glm.fit = glm(class ~ odor + spore.print.color + gill.size + stalk.shape + stalk.surface.br,
              data=mush[train,],
              family=binomial)
summary(glm.fit)

# Omitting an important variable will work
glm.fit = glm(class ~ odor + gill.size + stalk.shape + stalk.surface.br,
              data=mush[train,],
              family=binomial)
summary(glm.fit)


### LDA Model

lda.fit = lda(class ~ odor + spore.print.color + gill.size + stalk.shape + stalk.surface.br,
              data=mush[train,])
lda.probs=predict(lda.fit, mush[!train,], type="response")
table(lda.probs$class, mush$class[!train])
mean(lda.probs$class!=mush$class[!train])*100


### QDA Model

qda.fit = qda(class ~ odor + spore.print.color + gill.size + stalk.shape + stalk.surface.br,
              data=mush[train,])
qda.pred=predict(qda.fit, mush[!train,])
table(qda.pred$class, mush$class[!train])
mean(qda.pred$class==mush$class[!train])

# Changing class to a binary numeric

#KNN Model

mushmat = model.matrix(class ~ odor + spore.print.color + gill.size + stalk.shape + stalk.surface.br,data = mush)
train.X=mushmat[train,-1]
train.Y=mush_bin[train,1]
test.X=mushmat[!train,-1]
test.Y=mush_bin[!train,1]
# Remember: 1 is p, 0 is e
# This will fail due to too many ties
knn.pred=knn(train = train.X, test = test.X, cl = train.Y ,k=101)

# creating a "jitter" matrix to slightly perturb the matrices to prevent ties
set.seed(1)
randmat = matrix(runif(n = ncol(mushmat)*nrow(mushmat), min = 0.01, max = 0.05), ncol = ncol(mushmat))
# Jittering the mushroom matrix:
mushmat = mushmat + randmat
train.X=mushmat[train,-1]
train.Y=mush_bin[train,1]
test.X=mushmat[!train,-1]
test.Y=mush_bin[!train,1]

klist=NULL
for (i in 1:50) {
  set.seed(1)
  #10-fold CV setup
  k = 10
  CV_error = 0
  for (j in 1:k) {
    M = round(nrow(train.X)/k)
    start = ((j-1)*M) + 1
    end = min(c((j*M), nrow(nrow(train.X)))) - 1
    CV_test.x = train.X[start:end,]
    CV_test.y = train.Y[start:end]
    CV_train.x = train.X[-(start:end),]
    CV_train.y = train.Y[-(start:end)]
    knn.pred=knn(train = CV_train.x, test = CV_test.x, cl = CV_train.y ,k=i)
    CV_error = CV_error + mean(knn.pred == CV_test.y)
  }
  klist[i] = CV_error/k
}
plot(klist, xlab = "k", ylab = "1 - CV Error")

knn.pred=knn(train = train.X, test = test.X, cl = train.Y ,k=1)
mean(knn.pred != test.Y)*100

table(knn.pred, test.Y)



