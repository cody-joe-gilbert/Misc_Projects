# Cody Gilbert
# STAT 724
# Final Exam
# 5/18/2018

# Future reader: This code contains multiple uses of copy/pasted code where a loop
# 	would suffice. This was noted during creation and allowed due to academic
#	time constraints.


# Load Libraries\functions----------------------------------------------------


# Libraries
library(ggplot2) #Plotting tools
library(GGally)  #Correlation Plot Tools
library(tidyr)   #Data tidying 
library(dplyr)   #Data Manipulation
library(gridExtra)  #Plotting tools
library(caret)  #Machine learning tools
library(ROCR)   #ROC plotting Functions
library(caTools)  #AUC calculations


rocplot <- function(pred, truth, ...){
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf,...)
  #compute area under curve 
  auc <- performance(predob,"auc")   
  ( auc <- unlist(slot(auc, "y.values")) )
}


# Read In Data ------------------------------------------------------------


# Reading in the sampled HIGGS data;
higgs = read.csv("HIGGSSample.csv", header = TRUE)

# Removing the first index column:
higgs = higgs[,-1]

# Changing class to factors. "H" if Higgs, "B" if background.
higgs$Class = as.factor(ifelse(higgs$Class == 1, "H", "B"))

# Data Summary
summary(higgs)

# Splitting data into dervived and lower level features;
higgs_derived = higgs[, c(1, 23:29)]
higgs_lower = higgs[, 1:22]
summary(higgs_derived)
summary(higgs_lower)


# Data Visualization ------------------------------------------------------

pdf(file = "DataVisualization.pdf", width = 12, height = 6)


# Data Visualization
set.seed(1)
dat_vis_sample = sample(1:nrow(higgs), size = 1000, replace = FALSE)
ggpairs(higgs_derived[dat_vis_sample,-1], title = "Derived Values Correlations" )


# Correlation Matrix of lower-tier features
cor(higgs_lower[,-1])



tidy_higgs_d = gather(higgs_derived[dat_vis_sample,], "Label", "Value", -Class)
derived_plot = ggplot(tidy_higgs_d, aes(x = Value, col = Class)) +
  geom_density() +
  facet_wrap(~ Label) +
  coord_cartesian(xlim = c(0,5), ylim = c(0,1)) +
  labs(title = "Derived Features Distributions ")

tidy_higgs_l = gather(higgs_lower[dat_vis_sample,], "Label", "Value", -Class)
lower_plot = ggplot(tidy_higgs_l, aes(x = Value, col = Class)) +
  geom_density() +
  facet_wrap(~ Label) +
  labs(title = "Lower Features Distributions ")

grid.arrange(lower_plot, derived_plot, ncol = 2 )

# Examing the principle components


set.seed(1)
dat_vis_sample = sample(1:nrow(higgs), size = 2000, replace = FALSE)

pr_derived = prcomp(higgs_derived[, -1], scale = T)
pca_derived = cbind(Class = higgs_derived$Class, data.frame(pr_derived$x))
pca_derived_class = ggplot(pca_derived[dat_vis_sample,], aes(x = PC1, y = PC2, col = Class)) +
  geom_point(alpha = 0.3) +
  labs(col = "Classification", title = "Derived Data PCA Classes")

tidy_pca_d = gather(pca_derived[dat_vis_sample,], "Label", "Value", -Class) %>%
              arrange(Label)
pca_derived_densities = ggplot(tidy_pca_d, aes(x = Value, col = Class)) +
  geom_density() +
  facet_wrap(~ Label) +
  coord_cartesian(ylim = c(0,1))

grid.arrange(pca_derived_class, pca_derived_densities, ncol = 2)

pr_lower = prcomp(higgs_lower[, -1], scale = T)
pca_lower = cbind(Class = higgs_lower$Class, data.frame(pr_lower$x))
pca_lower_class = ggplot(pca_lower[dat_vis_sample,], aes(x = PC1, y = PC2, col = Class)) +
  geom_point(alpha = 0.3) +
  labs(col = "Classification", title = "Lower Data PCA Classes")

tidy_pca_l = gather(pca_lower[dat_vis_sample,], "Label", "Value", -Class) %>%
              arrange(Label)
pca_lower_densities = ggplot(tidy_pca_l, aes(x = Value, col = Class)) +
  geom_density() +
  facet_wrap(~ Label) +
  coord_cartesian(ylim = c(0,1))

grid.arrange(pca_lower_class, pca_lower_densities, ncol = 2)
dev.off()

rm(dat_vis_sample)
rm(pr_derived)
rm(pca_derived)
rm(pca_derived_class)
rm(tidy_pca_d)
rm(pca_derived_densities)
rm(pr_lower)
rm(pca_lower)
rm(pca_lower_class)
rm(tidy_pca_l)
rm(pca_lower_densities)
rm(lower_plot)
rm(derived_plot)
rm(tidy_higgs_l)
rm(tidy_higgs_d)


# Set training/test sets --------------------------------------------------



# Setting training and test indices using caret:
set.seed(1)
train = createDataPartition(higgs$Class, p = 0.75, list = FALSE) # creates a training index with 75% of data

# Removing data to save memory
rm(higgs)
rm(higgs_lower)

# First Data set - Derived ----------------------------------------------------------

### Modeling using the derived data ###
model_data = "DerivedFeatures"
capture.output(print("Start of Modeling..."), file = paste(model_data,"Output.txt", sep = ""), append = FALSE)
model_set = higgs_derived
pdf(file = paste(model_data, ".pdf", sep = ""))

# using caret as a wrapper on feature selection using Recursive Feature Elimination
# with a Random Forest model
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 2,
                      verbose = T)
outcomeName<-'Class'
predictors<-names(model_set)[!names(model_set) %in% outcomeName]
# RFE for predictor selection. Commented out to avoid re-running
#Pred_Profile <- rfe(model_set[train, predictors], model_set[train, outcomeName],
#                    rfeControl = control)

#Pred_Profile
#predictors = predictors(Pred_Profile)
#rm(Pred_Profile)

#All predictors gave the highest accuracy. Using all 7.


# Setting common folds for all caret models:
my_folds = createFolds(model_set[train, outcomeName], k = 5) 

myControl <- trainControl(summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          savePredictions = TRUE,
                          index = my_folds)

##glm model
model_type = "GLM"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='glm',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)
library(caTools)
colAUC(predictions, model_set[-train, outcomeName], plotROC = TRUE)
##LDA model
model_type = "LDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='lda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##QDA model
model_type = "QDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='qda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##KNN model
model_type = "KNN"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(k=c(1, 5, 10, 20, 50, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='knn',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "KNN Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


##random forest model
model_type = "Random Forest"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

grid = expand.grid(mtry=c(2,sqrt(length(predictors)),length(predictors)), #Tuning Grid
                   splitrule=c("gini"),
                   min.node.size = c(10, 20, 50, 100))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='ranger',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Random Forest Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##gbm model
model_type = "Boosted Model"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(n.trees=c(10,50,100,500, 700), #Tuning Grid
                    shrinkage=c(0.01,0.1),
                    n.minobsinnode = c(10,20),
                    interaction.depth=c(1,5))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='gbm',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Boosted Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Linear model
model_type = "SVM Linear"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c( 10, 50, 75, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmLinear',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "SVM Linear Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Polynomial model
# Model not run due to runtime restrictions
# model_type = "SVM Polynomial"
# capture.output(print(paste("********",model_type, "Model********")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# grid = expand.grid(C=c(10, 50, 100),
#                    degree = c(2, 3, 4),
#                    scale = c(TRUE)) #Tuning Grid
# 
# model = train(model_set[train, predictors],  # Training the model
#               model_set[train, outcomeName],
#               method='svmPoly',
#               trControl=myControl,
#               tuneGrid=grid)
# plot(model, main = "SVM Polynomial Model Tuning")
# capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# 
# predictions = predict.train(object=model, # Predict classes on test data
#                             model_set[-train, predictors],
#                             type="raw")
# capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix
# 
# predictions = predict.train(object=model, # Probabilties for ROC curve
#                             model_set[-train, predictors],
#                             type="prob")
# capture.output(print(paste(model_type, "AUC:")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
#                        col="red", 
#                        main = paste(model_data, model_type, "Model", "ROC")),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# abline(a = 0, b = 1)

##SVM Radial model
model_type = "SVM Radial"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c(1, 10, 50, 100),
                   sigma = c(0.001, 0.01, 1, 5, 10)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmRadial',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "CVM Radial Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##Neural Net model
model_type = "Neural Network"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(size = c(10, 50, 100),
                   decay = c(0, 0.1, 1)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='nnet',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Neural Network Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


# Closing plotting PDF
dev.off()

# Second Data set - Lower ----------------------------------------------------------

# Reading in the sampled HIGGS data;
higgs = read.csv("HIGGSSample.csv", header = TRUE)

# Removing the first index column:
higgs = higgs[,-1]

# Changing class to factors. "H" if Higgs, "B" if background.
higgs$Class = as.factor(ifelse(higgs$Class == 1, "H", "B"))
higgs_lower = higgs[, 1:22]
rm(higgs)

### Modeling using the lower tier data ###
model_data = "LowerFeatures"
capture.output(print("Start of Modeling..."), file = paste(model_data,"Output.txt", sep = ""), append = FALSE)
model_set = higgs_lower
pdf(file = paste(model_data, ".pdf", sep = ""))

# using caret as a wrapper on feature selection using Recursive Feature Elimination
# with a Random Forest model
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 2,
                      verbose = T)
outcomeName<-'Class'
predictors<-names(model_set)[!names(model_set) %in% outcomeName]
# RFE for predictor selection. Commented out to avoid re-running
# commented out to reduce run time
#Pred_Profile <- rfe(model_set[train, predictors], model_set[train, outcomeName],
#                    rfeControl = control)

#Pred_Profile
#predictors(Pred_Profile)
# 16 features were selected:
predictors = c( "jet.1.pt","lepton.pT","missing.energy.magnitude","jet.1.b.tag",            
                "jet.2.pt","jet.2.b.tag","jet.3.pt","jet.4.pt",              
                "jet.1.eta","lepton.eta","jet.2.eta","jet.3.b.tag",             
                "jet.3.eta","jet.4.eta","jet.4.b.tag","jet.1.phi" )
#rm(Pred_Profile)

# Setting common folds for all caret models:
my_folds = createFolds(model_set[train, outcomeName], k = 5) 

myControl <- trainControl(summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          savePredictions = TRUE,
                          index = my_folds)

##glm model
model_type = "GLM"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='glm',
              trControl=myControl)
summary(model)
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##LDA model
model_type = "LDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='lda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##QDA model
model_type = "QDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='qda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##KNN model
model_type = "KNN"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(k=c(1, 5, 10, 20, 50, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='knn',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "KNN Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


##random forest model
model_type = "Random Forest"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

grid = expand.grid(mtry=c(2,sqrt(length(predictors)),length(predictors)), #Tuning Grid
                   splitrule=c("gini"),
                   min.node.size = c(10, 20, 50, 100))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='ranger',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Random Forest Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##gbm model
model_type = "Boosted Model"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(n.trees=c(10,50,100,500, 700), #Tuning Grid
                   shrinkage=c(0.01,0.1),
                   n.minobsinnode = c(10,20),
                   interaction.depth=c(1,5))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='gbm',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Boosted Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Linear model
model_type = "SVM Linear"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c( 10, 50, 75, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmLinear',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "SVM Linear Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Polynomial model
# Model not run due to runtime restrictions
# model_type = "SVM Polynomial"
# capture.output(print(paste("********",model_type, "Model********")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# grid = expand.grid(C=c(10, 50, 100),
#                    degree = c(2, 3, 4),
#                    scale = c(TRUE)) #Tuning Grid
# 
# model = train(model_set[train, predictors],  # Training the model
#               model_set[train, outcomeName],
#               method='svmPoly',
#               trControl=myControl,
#               tuneGrid=grid)
# plot(model, main = "SVM Polynomial Model Tuning")
# capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# 
# predictions = predict.train(object=model, # Predict classes on test data
#                             model_set[-train, predictors],
#                             type="raw")
# capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix
# 
# predictions = predict.train(object=model, # Probabilties for ROC curve
#                             model_set[-train, predictors],
#                             type="prob")
# capture.output(print(paste(model_type, "AUC:")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
#                        col="red", 
#                        main = paste(model_data, model_type, "Model", "ROC")),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# abline(a = 0, b = 1)

##SVM Radial model
model_type = "SVM Radial"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c(1, 10, 50, 100),
                   sigma = c(0.001, 0.01, 1, 5, 10)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmRadial',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "CVM Radial Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##Neural Net model
model_type = "Neural Network"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(size = c(10, 50, 100),
                   decay = c(0, 0.1, 1)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='nnet',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Neural Network Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


# Closing plotting PDF
dev.off()

# Third Data set - Derived PC ----------------------------------------------------------
rm(higgs_lower)
# Reading in the sampled HIGGS data;
higgs = read.csv("HIGGSSample.csv", header = TRUE)

# Removing the first index column:
higgs = higgs[,-1]

# Changing class to factors. "H" if Higgs, "B" if background.
higgs$Class = as.factor(ifelse(higgs$Class == 1, "H", "B"))

# Splitting data into dervived and lower level features;
higgs_derived = higgs[, c(1, 23:29)]

pr_derived = prcomp(higgs_derived[, -1], scale = T)
pca_derived = cbind(Class = higgs_derived$Class, data.frame(pr_derived$x))
rm(higgs_derived)
rm(higgs)

### Modeling using the derived PCA data ###
model_data = "DerivedPCA"
capture.output(print("Start of Modeling..."), file = paste(model_data,"Output.txt", sep = ""), append = FALSE)
model_set = pca_derived
pdf(file = paste(model_data, ".pdf", sep = ""))

# using caret as a wrapper on feature selection using Recursive Feature Elimination
# with a Random Forest model
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 2,
                      verbose = T)
outcomeName<-'Class'
predictors<-names(model_set)[!names(model_set) %in% outcomeName]
# RFE for predictor selection. Commented out to avoid re-running
# commented out to reduce run time
#Pred_Profile <- rfe(model_set[train, predictors], model_set[train, outcomeName],
#                    rfeControl = control)

#Pred_Profile
#predictors(Pred_Profile)
#rm(Pred_Profile)
# All 7 features were selected, hower since 5 PCs describe over 95% variance, 5
# will be used.
predictors = predictors[1:5]
# Setting common folds for all caret models:
my_folds = createFolds(model_set[train, outcomeName], k = 5) 

myControl <- trainControl(summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          savePredictions = TRUE,
                          index = my_folds)

##glm model
model_type = "GLM"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='glm',
              trControl=myControl)
summary(model)
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##LDA model
model_type = "LDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='lda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##QDA model
model_type = "QDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='qda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##KNN model
model_type = "KNN"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(k=c(1, 5, 10, 20, 50, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='knn',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "KNN Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


##random forest model
model_type = "Random Forest"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

grid = expand.grid(mtry=c(2,sqrt(length(predictors)),length(predictors)), #Tuning Grid
                   splitrule=c("gini"),
                   min.node.size = c(10, 20, 50, 100))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='ranger',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Random Forest Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##gbm model
model_type = "Boosted Model"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(n.trees=c(10,50,100,500, 700), #Tuning Grid
                   shrinkage=c(0.01,0.1),
                   n.minobsinnode = c(10,20),
                   interaction.depth=c(1,5))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='gbm',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Boosted Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Linear model
model_type = "SVM Linear"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c( 10, 50, 75, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmLinear',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "SVM Linear Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Polynomial model
# Model not run due to runtime restrictions
# model_type = "SVM Polynomial"
# capture.output(print(paste("********",model_type, "Model********")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# grid = expand.grid(C=c(10, 50, 100),
#                    degree = c(2, 3, 4),
#                    scale = c(TRUE)) #Tuning Grid
# 
# model = train(model_set[train, predictors],  # Training the model
#               model_set[train, outcomeName],
#               method='svmPoly',
#               trControl=myControl,
#               tuneGrid=grid)
# plot(model, main = "SVM Polynomial Model Tuning")
# capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# 
# predictions = predict.train(object=model, # Predict classes on test data
#                             model_set[-train, predictors],
#                             type="raw")
# capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix
# 
# predictions = predict.train(object=model, # Probabilties for ROC curve
#                             model_set[-train, predictors],
#                             type="prob")
# capture.output(print(paste(model_type, "AUC:")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
#                        col="red", 
#                        main = paste(model_data, model_type, "Model", "ROC")),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# abline(a = 0, b = 1)

##SVM Radial model
model_type = "SVM Radial"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c(1, 10, 50, 100),
                   sigma = c(0.001, 0.01, 1, 5, 10)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmRadial',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "CVM Radial Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##Neural Net model
model_type = "Neural Network"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(size = c(10, 50, 100),
                   decay = c(0, 0.1, 1)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='nnet',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Neural Network Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


# Closing plotting PDF
dev.off()
# Fourth Data set - Lower PC ----------------------------------------------------------

rm(pca_derived)
# Reading in the sampled HIGGS data;
higgs = read.csv("HIGGSSample.csv", header = TRUE)

# Removing the first index column:
higgs = higgs[,-1]

# Changing class to factors. "H" if Higgs, "B" if background.
higgs$Class = as.factor(ifelse(higgs$Class == 1, "H", "B"))

higgs_lower = higgs[, 1:22]

pr_lower = prcomp(higgs_lower[, -1], scale = T)
pca_lower= cbind(Class = higgs_lower$Class, data.frame(pr_lower$x))
rm(higgs_lower)
rm(higgs)
rm(pca_derived)


### Modeling using the derived PCA data ###
model_data = "LowerPCA"
capture.output(print("Start of Modeling..."), file = paste(model_data,"Output.txt", sep = ""), append = FALSE)
model_set = pca_lower
pdf(file = paste(model_data, ".pdf", sep = ""))

# using caret as a wrapper on feature selection using Recursive Feature Elimination
# with a Random Forest model
control <- rfeControl(functions = rfFuncs,
                      method = "repeatedcv",
                      repeats = 2,
                      verbose = T)
outcomeName<-'Class'
predictors<-names(model_set)[!names(model_set) %in% outcomeName]
# RFE for predictor selection. Commented out to avoid re-running
# commented out to reduce run time
# Pred_Profile <- rfe(model_set[train, predictors], model_set[train, outcomeName],
#                     rfeControl = control)
# 
# Pred_Profile
# predictors(Pred_Profile)
# 16 features were selected:
predictors = c("PC12", "PC1", "PC20", "PC3","PC21", "PC5",  "PC16", "PC17", "PC7",  "PC4",
               "PC6",  "PC15", "PC14", "PC18", "PC9",  "PC13")
rm(Pred_Profile)

# Setting common folds for all caret models:
my_folds = createFolds(model_set[train, outcomeName], k = 5) 

myControl <- trainControl(summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          verboseIter = TRUE,
                          savePredictions = TRUE,
                          index = my_folds)

##glm model
model_type = "GLM"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='glm',
              trControl=myControl)
summary(model)
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##LDA model
model_type = "LDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='lda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##QDA model
model_type = "QDA"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='qda',
              trControl=myControl)

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##KNN model
model_type = "KNN"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(k=c(1, 5, 10, 20, 50, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='knn',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "KNN Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


##random forest model
model_type = "Random Forest"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

grid = expand.grid(mtry=c(2,sqrt(length(predictors)),length(predictors)), #Tuning Grid
                   splitrule=c("gini"),
                   min.node.size = c(10, 20, 50, 100))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='ranger',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Random Forest Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##gbm model
model_type = "Boosted Model"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(n.trees=c(10,50,100,500, 700), #Tuning Grid
                   shrinkage=c(0.01,0.1),
                   n.minobsinnode = c(10,20),
                   interaction.depth=c(1,5))
model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='gbm',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Boosted Model Tuning")

capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Linear model
model_type = "SVM Linear"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c( 10, 50, 75, 100)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmLinear',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "SVM Linear Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##SVM Polynomial model
# Model not run due to runtime restrictions
# model_type = "SVM Polynomial"
# capture.output(print(paste("********",model_type, "Model********")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# grid = expand.grid(C=c(10, 50, 100),
#                    degree = c(2, 3, 4),
#                    scale = c(TRUE)) #Tuning Grid
# 
# model = train(model_set[train, predictors],  # Training the model
#               model_set[train, outcomeName],
#               method='svmPoly',
#               trControl=myControl,
#               tuneGrid=grid)
# plot(model, main = "SVM Polynomial Model Tuning")
# capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# 
# predictions = predict.train(object=model, # Predict classes on test data
#                             model_set[-train, predictors],
#                             type="raw")
# capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix
# 
# predictions = predict.train(object=model, # Probabilties for ROC curve
#                             model_set[-train, predictors],
#                             type="prob")
# capture.output(print(paste(model_type, "AUC:")), 
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
#                        col="red", 
#                        main = paste(model_data, model_type, "Model", "ROC")),
#                file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
# abline(a = 0, b = 1)

##SVM Radial model
model_type = "SVM Radial"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(C=c(1, 10, 50, 100),
                   sigma = c(0.001, 0.01, 1, 5, 10)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='svmRadial',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "CVM Radial Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)

##Neural Net model
model_type = "Neural Network"
capture.output(print(paste("********",model_type, "Model********")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
grid = expand.grid(size = c(10, 50, 100),
                   decay = c(0, 0.1, 1)) #Tuning Grid

model = train(model_set[train, predictors],  # Training the model
              model_set[train, outcomeName],
              method='nnet',
              trControl=myControl,
              tuneGrid=grid)
plot(model, main = "Neural Network Model Tuning")
capture.output(model, file = paste(model_data,"Output.txt", sep = ""), append = TRUE)

predictions = predict.train(object=model, # Predict classes on test data
                            model_set[-train, predictors],
                            type="raw")
capture.output(print("Confusion Matrix"), file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(confusionMatrix(predictions, model_set[-train, outcomeName]),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)  #Confusion matrix

predictions = predict.train(object=model, # Probabilties for ROC curve
                            model_set[-train, predictors],
                            type="prob")
capture.output(print(paste(model_type, "AUC:")), 
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
capture.output(rocplot(predictions$H, model_set[-train, outcomeName],
                       col="red", 
                       main = paste(model_data, model_type, "Model", "ROC")),
               file = paste(model_data,"Output.txt", sep = ""), append = TRUE)
abline(a = 0, b = 1)


# Closing plotting PDF
dev.off()



# Deep learning -----------------------------------------------------------
library(h2o)
h2o.init()

higgs = read.csv("HIGGS.csv", header = FALSE, nrows = 200000)
higgs = higgs[, c(1, 23:29)]
colnames(higgs) = c("Class", "m_jj",	"m_jjj",	"m_lv",
                    "m_jlv",	"m_bb",	"m_wbb", "m_wwbb")

# Changing class to factors. "H" if Higgs, "B" if background.
higgs$Class = as.factor(ifelse(higgs$Class == 1, "H", "B"))
write.csv(higgs, "HIGGSLargeSample.csv")

higgs = h2o.importFile(path = normalizePath("../Final/HIGGSLargeSample.csv"),
                         header = TRUE)
higgs = higgs[,-1]

splits = h2o.splitFrame(higgs, 0.75, seed=1)
train  = h2o.assign(splits[[1]], "train.hex") # 75%
test   = h2o.assign(splits[[2]], "test.hex")  # 25%


outcomeName = 'Class'
predictors = names(higgs)[!names(higgs) %in% outcomeName]

# Hyperparameter tuning grid
hyper_params <- list(
  hidden=list(c(50, 50), c(100,100), c(200,200),
              c(25, 25, 25), c(50, 50, 50),
              c(10, 10, 10, 10), c(50, 50, 50, 50)),
  input_dropout_ratio=c(0,0.05),
  rate=c(0.01,0.02),
  l1=seq(0,1e-4,1e-6),
  l2=seq(0,1e-4,1e-6)
)

# Running a grid search on a sample of the data
# to tune hyperparameters. This method uses a random selection technique.
search_criteria = list(strategy = "RandomDiscrete", max_runtime_secs = 60*20,
                       max_models = 200, seed=1234567, stopping_rounds=5,
                       stopping_tolerance=1e-2)
dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=train[1:50000,],
  nfolds = 5, 
  x=predictors, 
  y=outcomeName,
  epochs=1,
  stopping_metric="logloss",
  stopping_tolerance=1e-2,        ## stop when logloss does not improve by >=1% for 2 scoring events
  stopping_rounds=2,
  score_validation_samples=10000, ## downsample validation set for faster scoring
  score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)                                
grid = h2o.getGrid("dl_grid_random",sort_by="logloss",decreasing=FALSE)
grid

grid@summary_table[1,]
best_model = h2o.getModel(grid@model_ids[[1]]) ## model with lowest logloss
best_model
best_params = best_model@allparameters

# Resetting

# Fitting model on the full data set with the best hyperparameters
dlmodel <- h2o.deeplearning(
  x=predictors,
  y=outcomeName, 
  training_frame=train,
  hidden=best_params$hidden,
  input_dropout_ratio = best_params$input_dropout_ratio,
  rate = best_params$rate,
  epochs=1000000,                     
  score_validation_samples=10000, 
  stopping_rounds=2,
  stopping_metric="misclassification",
  stopping_tolerance=0.01,
  nfolds=5,
  fold_assignment="Modulo" 
)
dlmodel
h2o.performance(dlmodel, train=T)          ## sampled training data (from model building)
h2o.performance(dlmodel, valid=T)          ## sampled validation data (from model building)
h2o.performance(dlmodel, newdata=train)    ## full training data
h2o.performance(dlmodel, newdata=valid)    ## full validation data
h2o.performance(dlmodel, newdata=test)     ## full test data

plot(h2o.performance(dlmodel, newdata=test), type = "roc")

# Plotting Results --------------------------------------------------------

res = read.csv("ResultsTables.csv", header = TRUE) 
colnames(res)[1] = "Model"


ggplot(res, aes(x = Model, y = AUC, col = Feature.Set, group = Feature.Set)) +
  geom_point() +
  geom_line() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
