# Exploration and Predictive Modelling of Breast Cancer
  ## Data is from the 'Breast Cancer Wisconsin (Diagnostic) Data Set' from the UCI ML Repository, and relates to measurements of breast mass samples
  ## Aims: Explore the data and compare modelling approaches

#### Set up ####

# Load required libraries, set default ggplot theme, set seed
library(visdat)
library(ggplot2)
library(GGally)
library(viridis)
library(hrbrthemes)
library(corrplot)
library(patchwork)
library(gridExtra)
library(ggridges)
library(caret)
library(cluster)
library(corrr)
library(psych)
library(magrittr)
library(factoextra)
library(ggfortify)
library(plotly)
library(mclust)
library(caret)
library(car)
library(e1071)
library(randomForest)
library(xgboost)
library(klaR)
library(nnet)
library(ggbump)
library(paletteer)
library(scales)
library(doParallel)
library(pROC)
library(NeuralNetTools)
library(tidyverse)

theme_set(theme_classic())
set.seed(5)

# Allow parallel computation, using all but two cores:
number_cores <- detectCores() - 2 
registerDoParallel(makeCluster(number_cores))

# Create a dataframe to compare models
Model_Comparisons <- 
  data.frame(
    Model_Name = factor(),
    Model_Type = factor(),
    F1_Score = numeric(), 
    Accuracy = numeric(), # Overall prediction accuracy
    Balanced_Accuracy = numeric(), # Average of sensitivity and specificity
    Sensitivity = numeric(), # Proportion of true malignant cases that are accurately predicted
    Specificity  = numeric() # Proportion of true benign cases accurately predicted
  )

# Define functions:
  ## To calculate F1 score, which we will use to evaluate models
F1_score <- function(predictions, actuals, positive_class = "M") {
    # Calculate precision and sensitivity from predictions and actuals:
    precision <- posPredValue(predictions, actuals, positive = positive_class)
    sensitivity <- sensitivity(predictions, actuals, positive = positive_class)
    
    #Calculate and return F1-Score:
    F1 <- ifelse((precision + sensitivity) == 0, 0, 2 * (precision * sensitivity) / (precision + sensitivity))
    return(F1)
  }

  ## To return custom metrics including F1 score, usable with caret as the training metric
trainingMetrics <- function(data, lev = NULL, model = NULL) {
  # Calculate training metrics from predictions and actuals:
  Precision <- round(posPredValue(data$pred, data$obs, positive = lev[1]), 4)
  Sensitivity <- round(sensitivity(data$pred, data$obs, positive = lev[1]), 4)
  Specificity <- round(specificity(data$pred, data$obs, positive = lev[1]), 4)
  Accuracy <- round(mean(data$pred == data$obs), 4)
  Balanced_Accuracy <- round((Sensitivity + Specificity) / 2, 4)
  
  # Calculate F1-score:
  F1 <- round(ifelse((Precision + Sensitivity) == 0, 0, 2 * (Precision * Sensitivity) / (Precision + Sensitivity)), 4)
  
  # Return training metrics as a named vector:
  return(c(F1 = F1, Balanced_Accuracy = Balanced_Accuracy, Accuracy = Accuracy, Specificity = Specificity, Sensitivity = Sensitivity, Precision = Precision))
}

  ## To produce a confusion matrix from model predictions and actuals, extract metrics, and attach to the Model_Comparisons if possible
evaluateModel <- function(predictions, actuals, model_name = NA, model_type = NA, positive_class = "M") {
  # Calculate and print confusion matrix
  confusion_matrix <- confusionMatrix(predictions, actuals, positive = positive_class)
  print(confusion_matrix)
  
  # Calculate F1 score:
  F1 <- F1_score(predictions, actuals, positive_class = positive_class)
  cat("F1 Score: ", round(F1, 4), "\n")

  # If a model_name is provided in the function call:
  if (!is.na(model_name) && exists("Model_Comparisons")) { 
    ## Extract metrics
    acc <- confusion_matrix$overall[["Accuracy"]]
    bal_acc <- confusion_matrix$byClass[["Balanced Accuracy"]]
    sensitivity <- confusion_matrix$byClass[["Sensitivity"]]
    specificity <- confusion_matrix$byClass[["Specificity"]]

    ## Return metrics as a data frame entry
    metrics <- data.frame(
      Model_Name = model_name,
      Model_Type = model_type,
      F1_Score = round(F1, 4),
      Accuracy = round(acc, 4),
      Balanced_Accuracy = round(bal_acc, 4),
      Sensitivity = round(sensitivity, 4),
      Specificity = round(specificity, 4))
    
    ## Append metrics to Model_Comparisons
    Model_Comparisons <<- rbind(Model_Comparisons, metrics)
    print("Model_Comparisons updated")
  } else {
    print("!! Model_Comparisons NOT updated !!")
  }
}

# Load the data
Raw_Breast_Cancer_Data <- read.csv("C:\\Users\\User\\Documents\\Projects\\BreastCancerDetection\\Datasets\\breast+cancer+wisconsin+diagnostic\\data.csv")


#### Exploratory Analysis ####

# Examine data structure:
dim(Raw_Breast_Cancer_Data)
vis_dat(Raw_Breast_Cancer_Data, warn_large_data = FALSE, palette = "qual", sort = FALSE) 
vis_miss(Raw_Breast_Cancer_Data) 
nrow(Raw_Breast_Cancer_Data[duplicated(Raw_Breast_Cancer_Data), ]) 
length(unique(Raw_Breast_Cancer_Data$id)) - length(Raw_Breast_Cancer_Data$id) 
      # 33 columns, 593 observations
      # All but 3 columns are numeric (float); 'diagnosis' is character data, 'id' is integer, and 'X' appears to be entirely NULL
      # There are no duplicated or NA rows (except for in 'X')
      # 'id' is a unique identifier, and therefore redundant
      # None of the values are negative, which is good (as these would be impossible)

# Preprocess data, setting 'id' as rownames, dropping 'id' and 'X' columns, and setting 'diagnosis' to be a factor
Breast_Cancer_Data <- 
  Raw_Breast_Cancer_Data %>% 
  column_to_rownames("id") %>%
  select(-X) %>%
  mutate(diagnosis = as.factor(diagnosis))
# Save diagnoses as a vector
Actual_Diagnoses <- Breast_Cancer_Data$diagnosis



# Explore diagnosis (the target variable)
  ## Plot the frequency of diagnoses
ggplot(
  data = as.data.frame(table(Actual_Diagnoses)), 
  aes(x = Actual_Diagnoses , y = Freq, fill = Actual_Diagnoses)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Freq), vjust = 2) + 
  labs(title = "Distribution of Actual Diagnoses", 
       x = "Diagnosis",
       y = "Frequency") +
  theme(legend.position = "none")
      # Classes are imbalanced: 357 are class 'B', 212 are class 'M'
      # These relate to M = Malignant (indicating presense of cancer) and B = Benign
      # We want to build a model to predict this variable


# Explore feature variables
  ## Plot a grid of histograms for each numeric variable
histogram_list <- list()
for (i in colnames(Breast_Cancer_Data[, -1])) { 
  histogram_list[[i]] <- 
    ggplot(data = Breast_Cancer_Data, aes(x = .data[[i]])) + 
    geom_histogram(fill = "red2", col = "darkred", alpha = 0.15) + 
    theme(axis.title.y = element_blank())
}
wrap_plots(histogram_list, ncol = 6, nrow = 5) + plot_annotation(title = "Histograms of Breast Cancer Dataset Features")
  ## Plot boxplots of the numeric variables
boxplot(Breast_Cancer_Data[, -1], las = 2, main = "Boxplots of Breast Cancer Dataset Features")
      # Many of the variables are on very different scales, from fractel_dimension_se (ranging from 0.00 to 0.03) to area_worst (ranging from 185 to 4254)
      # Many of the variables are heavily positively skewed
        # area_se, radius_se, concativity_se, perimeter_se and fractal_dimension_se all have particularly long right tails
      # There are many positive outliers throughout the data, but fairly few negative outliers

# Explore variables relationships
  ## Encode diagnosis as a numeric variable, then plot correlations between numeric variables
Breast_Cancer_Data.encoded <-   
  Breast_Cancer_Data %>% 
  mutate(diagnosis = if_else(diagnosis == "M", 1, 0)) 
corrplot(cor(Breast_Cancer_Data.encoded), order = "AOE", type = "lower", diag = FALSE)
      # There are some strong positive correlations in the data, indicating multicollinearity
      # There are some fairly strong correlations between 'coded_diagnosis' and several variables, indicating possible linear relationships
        # Perimeter_worst, perimeter_mean, radius_worst, area_worst, area_mean, and radius_mean all have the strongest correlation with a positive ("M") diagnosis

  ## Parallel Coordinates Plot, showing every observation in the data (standardised) and coloured by diagnosis
ggparcoord(Breast_Cancer_Data, column = 2:ncol(Breast_Cancer_Data), groupColumn = 1, order = "anyClass", scale = "std", alpha = 0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "grey90", linetype = "dashed")) +
  labs(title = "PCP of Breast Cancer Observations, Coloured by Diagnosis")
      # radius_se, perimeter_se, area_se, concavity_se and fractal_dimension_se has some very high positive outliers
      # There is clear separation between the diagnosis classifications in many of the variables.
      # This suggests a classification model should be highly effective  

  ## Save column names which have r >= 0.5 with diagnosis
correlated_features <- 
  Breast_Cancer_Data.encoded %>%
  correlate() %>%              
  focus(diagnosis) %>%        
  filter(diagnosis >= 0.5) %>% 
  pull(term)                   
print(correlated_features)

  ## Examine our correlated_features closer:
corrplot(cor(Breast_Cancer_Data.encoded[ ,c("diagnosis", correlated_features)]), 
         method = "number", order = "AOE", type = "lower", diag = FALSE, number.cex = 0.55)
pairs.panels(Breast_Cancer_Data.encoded[,c("diagnosis", correlated_features)],
             method = "pearson", 
             pch = 21, bg = c("red2", "cyan3")[Breast_Cancer_Data$diagnosis], 
             cex = 0.8, cex.cor = 2,
             scale = TRUE, ellipses = FALSE)
      # There are some very strong correlations with diagnosis in these 15 variables
      # There is also a lot of multicollinearity
      # Also evidence of some non-linear relationships between variables
      # Strong evidence of separation by diagnosis throughout these variables
 
  ## Do variable distributions change with diagnosis?
density_list <- list()
for (i in colnames(Breast_Cancer_Data[,-1])) {
  density_list[[i]] <- 
    ggplot(data = Breast_Cancer_Data, aes(x = .data[[i]], group = diagnosis, fill = diagnosis)) + 
    geom_density(alpha = 0.45) + 
    theme(axis.title.y = element_blank()) 
}
wrap_plots(density_list, ncol = 6, nrow = 5, guides = "collect") + 
  plot_annotation(title = "Density Plots of Breast Cancer Dataset Features (by Diagnosis)") 
      # Confirms clear separation by diagnosis in many of the variables
      # Those that are separated tend to have positive association with Malignant diagnosis
      # Some of the variables (e.g. symmetry_mean, texture_mean, smoothness_mean, etc) appear closer to a normal distribution when split by diagnosis
      
# EDA findings:
      # Cleaned data by removing redundant columns
      # Imbalanced classes in the target variable
      # High level of positive skew and outliers throughout the data
      # Multicollinearity is present in the data; 
        ## Some ML models will handle this automatically via feature selection, others won't
        ## Dimension reduction may be successful
      # Clear separation in many of the variables between diagnoses, suggesting a classification algorithm will be effective

# Tidy the workspace
rm(Raw_Breast_Cancer_Data, density_list, histogram_list, i, number_cores, Breast_Cancer_Data.encoded)

#### Unsupervised Learning Exploration, Clustering and Prediction ####
      # High degrees of multicollinearity in the data suggests that dimension reduction may be effective
  
# Dimension Reduction (Principal Component Analysis)
PCA <- prcomp(Breast_Cancer_Data[, -1], scale = TRUE) 
  ## Plot variance explained by each additional Principal Component (PC)
fviz_screeplot(PCA, addlabels = TRUE)
  ## Plot how the variables contribute to different PCs, from the component loadings
fviz_contrib(PCA, choice = "var", axes = 1, top = 10) + fviz_contrib(PCA, choice = "var", axes = 2, top = 10) + fviz_contrib(PCA, choice = "var", axes = 3, top = 10)
  ## Plot the first 3 PCs in 3D space:
plot_ly(
  as.data.frame(PCA$x[,1:3]), 
  x = ~PC1, y = ~PC2, z = ~PC3, 
  type = "scatter3d", mode = "markers", 
  color = Actual_Diagnoses, size = I(130)) %>% 
  layout(paper_bgcolor = "#595c61", 
         scene = list(xaxis = list(color = "#ffffff"), 
                      yaxis = list(color = "#ffffff"), 
                      zaxis = list(color = "white")), 
         legend = list(font = list(color = "white")))
      # A high degree of the information is explained by just 2 (63.3%) or 3 (72.7%) PCs, showing multicollinearity and redundancy in the data
      # PC loadings indicate what the components may relate to:
        ## PC1 appears to relate to size & shape of the mass (dominated by concave.points_mean, concavity_mean, perimeter_mean, radius_worst, etc)
        ## PC2 is dominated by variables related to fractal_dimension, which measures the irregularity of the mass border
        ## PC3 is dominated by '_se' (standard error) variables, indicating it relates to levels of variance within the sample
      # Clear separation of diagnoses in the first two components
      # Even clearer separation in 3 PCs
      # With such strong separations, cluster analysis may be effective at classifying the diagnoses

# Cluster Analysis (K-means)
  ## Scale data and estimate optimal number of clusters using sum of squares silhouette measure
Breast_Cancer_Data.scaled <- scale(Breast_Cancer_Data[,-1]) 
fviz_nbclust(Breast_Cancer_Data.scaled, kmeans, method = "wss") + labs(title = "K-means sum of squares plot")
fviz_nbclust(Breast_Cancer_Data.scaled, kmeans, method = "silhouette") + labs(title = "K-means silhouette plot")
      # Sum of squares: 'Elbow' heuristic suggests that 2 or 3 clusters are optimal
      # Silhouette measure: confirms 2 clusters are optimal
    
  ## Perform k-means cluster analysis using 2 clusters and 25 initial cluster assignments
Kmeans_2c <- kmeans(Breast_Cancer_Data.scaled, 2, nstart = 25) 
Kmeans_2c
table(Actual_Diagnoses, Kmeans_2c$cluster) 
      # Only 32.1% of the variability is explained by this model
      # However cluster 1 aligns closely with Malignant diagnoses, and cluster 2 with Benign diagnoses
  ## Rename clusters to relevant diagnosis and calculate metrics
Kmeans.clusters <- as.factor(ifelse(Kmeans_2c$cluster == 1, "M", "B")) 
evaluateModel(Kmeans.clusters, Actual_Diagnoses, model_name = "Kmeans", model_type = "Unsupervised")
      #91.04% overall accuracy, although this may be skewed by the imbalanced Benign class (when Sensitivity is arguable the most metric in this context)
      #82.55% sensitivity and 96.07% specificity, indicating majority of positive cases are predicted, and very few false positives
      # 89.3% balanced accuracy (combines sensitivity and specificity)

# K-medoid clustering is less sensitive to outliers:
  ## Calculate optimal clusters
fviz_nbclust(Breast_Cancer_Data.scaled, clara, method = "wss") + labs(title = "K-medoid sum of squares plot")
fviz_nbclust(Breast_Cancer_Data.scaled, clara, method = "silhouette") + labs(title = "K-medoid silhouette plot") 
      # Again confirms 2 is optimal
  ## Perform k-medoid clustering and calculate results as before
Kmedoid <- clara(Breast_Cancer_Data.scaled, k=2)
table(Actual_Diagnoses, Kmedoid$clustering)
Kmedoid.clusters <- as.factor(ifelse(Kmedoid$clustering == 1, "M", "B")) 
evaluateModel(Kmedoid.clusters, Actual_Diagnoses, model_name = "Kmedoid", model_type = "Unsupervised")      
      # Specificity is greater than under K-means, but all other metrics are worse!

# Model-based (GMM) clustering can provide more flexible clusters
  ## Calculate clusters and metrics
Model_Cluster_2c <- Mclust(Breast_Cancer_Data.scaled, G = 2)
table(Actual_Diagnoses, Model_Cluster_2c$classification)
Model_Cluster_2c.clusters <- as.factor(ifelse(Model_Cluster_2c$classification == 1, "M", "B")) 
evaluateModel(Model_Cluster_2c.clusters, Actual_Diagnoses, model_name = "Model_Based (GMM)", model_type = "Unsupervised")
      # Overall accuracy, specificity and balanced accuracy are lower than k-means (86.29%, 84.87%, 86.78% respectively)
      # However sensitivity is higher (88.68% vs 82.55%)
      # Context makes this arguably the most important metric, with more of actual positives correctly diagnosed

  ## Compare and plot model-based cluster's prediction status (e.g. true positive, false positive, etc)
Model_Cluster_2c.Cluster_Comparison <- ifelse(Actual_Diagnoses == "M" & Model_Cluster_2c.clusters == "M", "True Pos", 
                            ifelse(Actual_Diagnoses == "B" & Model_Cluster_2c.clusters == "M", "False Pos", 
                                   ifelse(Actual_Diagnoses == "B" & Model_Cluster_2c.clusters == "B", "True Neg", "False Neg"))
                              )
plot_ly(
  as.data.frame(PCA$x[,1:3]), 
  x = ~PC1, y = ~PC2, z = ~PC3, 
  type = "scatter3d", mode = "markers", 
  color = ~Model_Cluster_2c.Cluster_Comparison, size = I(130)) %>% 
  layout(paper_bgcolor = "#595c61", scene = list(xaxis = list(color = "#ffffff"), yaxis = list(color = "#ffffff"), zaxis = list(color = "white")), legend = list(font = list(color = "white")))
        # Majority of the false positives tend to be outliers in the reduced data space
        # False negatives are much closer to other data points, between the two main clusters
        # However they are largely located close together

# Unsupervised Learning Findings:
Model_Comparisons[Model_Comparisons$Model_Type == "Unsupervised",]
      # High proportions of the data can be reduced to 2 or 3 dimensions, aiding visualisation and revealing natural (linearly separable) clusters forming within the PCs
      # Cluster analysis is able to identify clusters that can be used to predict diagnoses with good accuracy.
      # Model-based clustering has higher sensitivity than K-means clustering, although lower accuracy, balanced accuracy and specificity

# Tidy the workspace
rm(Kmeans.clusters, Kmedoid.clusters, Model_Cluster_2c.Cluster_Comparison, Model_Cluster_2c.clusters, Breast_Cancer_Data.scaled)

#### Supervised Learning Prediction ####
      # The presence of clear clusters in the data suggests a classification model should be effective
      # Lets prepare the data for modelling, and then train and compare different modelling approaches
      # General approach will:
          ## Split the data in training and test datasets
          ## Use a grid-search and repeated 5 fold cross-validation to evaluate hyperparameters and train the model
          ## Evaluate optimal model using the test data
          ## Compare modelling approaches
  
# Process the data for modelling
  ## 1) Scale the feature variables
Breast_Cancer_Data.processed <- Breast_Cancer_Data %>% mutate(across(-diagnosis, scale))
  ## 2) Split the data into training and test data (70%/30% split)
split_index <- createDataPartition(Breast_Cancer_Data.processed$diagnosis, p = 0.7, list = FALSE)
training_data <- Breast_Cancer_Data.processed[split_index,]
test_data <- Breast_Cancer_Data.processed[-split_index,]
  ## 3) Visualise the split
bind_rows(
  training_data %>%  mutate(dataset = "Train"), # Recombine the data with a column indicating if it is training or test data
  test_data  %>%  mutate(dataset = "Test")) %>%
  ggplot(aes(x = dataset, fill = factor(diagnosis))) + # Create a stacked bar chart showing the split between test and training, and between diagnoses
  geom_bar(position = "stack") +
  labs(
    title = "Train/Test Split with Diagnosis Breakdown",
    x = "Dataset",
    y = "Count",
    fill = "Diagnosis") 
        #classes are imbalanced, but relatively evenly split between test and training data

# Define training method, using 5-fold repeated cross-validation & grid search for hyperparameters, with parallel computation
train_control = trainControl(method = "repeatedcv", 
                             number = 5,
                             repeats = 5,
                             search = "grid",
                             classProbs = TRUE,
                             summaryFunction = trainingMetrics,
                             savePredictions = "final",
                             allowParallel = TRUE)

# Logistic regression model using elastic net for feature selection:
      # Effective on linearly separable data (which we appear to have)
      # Is computationally inexpensive
      # However it assumes linear relationships and can be ineffective in the presence of multicollinearity (which is present)
     
  ## Define a grid for alpha and lambda (regularisation parameters)
logistic_grid <- expand.grid(alpha = seq(0, 1, by = 0.1),     # 0 = Ridge, 1 = Lasso, in between = Elastic Net
                            lambda = seq(0, 1, length = 21))  # strength of the regularisation penalties

  ## Train the model
logistic_model <- train(diagnosis~.,
                       data = training_data,
                       method = "glmnet",
                       trControl = train_control,
                       tuneGrid = logistic_grid,
                       metric = "F1",
                       family = "binomial")

  ## Evaluate the trained model, test against the test_data, and examine feature importance:
logistic_model$bestTune 
Logistic_Predictions <- predict(logistic_model, newdata = test_data)
evaluateModel(Logistic_Predictions, test_data$diagnosis, "Logistic Model", "Supervised")
ggplot(varImp(logistic_model)) + labs(title = "Feature Importance of Logistic Model")
      # Optimal model has alpha = 0.1 and lambda = 0.02, indicating the model is closer aligned to Ridge regularisation (and so less feature selection)
      # Model performs better than clustering unsupervised techniques across all key metrics
      # Accuracy, Balanced Accuracy, Sensitivity and Specificity all above 95%
      # Two features (concavity_se and compactness_mean) were unselected entirely by the model, with several others having fairly low importance
      # texture_worst, concave.points_worst and radius_worst were the three most importance features


# SVM
      # Works well in high-dimensional datasets in which classes are separable (which has been confirmed by PCA)
      # However it it sensitive to hyperparameters and is not robust to overlapping classes
  ## Calculate values of sigma to test, and create a tuning grid of sigma and C (regularisation term)
sigma_estimates <- kernlab::sigest(diagnosis~., data = training_data)
SVM_grid <- expand.grid(sigma = sigma_estimates,
                        C = c(0.1, 0.25, 0.5, 1, 1.5, 2, 4))

  ## Train the model using grid-search
SVM_model <- train(diagnosis~.,
                  data = training_data,
                  method = "svmRadial",
                  trControl = train_control,
                  tuneGrid = SVM_grid,
                  metric = "F1",
                  )
  ## Evaluate the model training and test metrics, and visualise feature importance
SVM_model
SVM_Predictions <- predict(SVM_model, newdata = test_data)
evaluateModel(SVM_Predictions, test_data$diagnosis, "SVM", "Supervised")
ggplot(varImp(SVM_model)) + labs(title = "Feature Importance of SVM")


# randomForest
      # Handles non-linear relationships well, is robust to multicollinearity 
      # Ensemble method which can help resist over-fitting 
      # However can be computationally intensive, and is less interpretable than other methods
  ## Define grid and train the model
rf_grid <- expand.grid(mtry = c(3, 5, 7, 9, 11, 13, 15),
                      min.node.size = c(1, 5, 10, 15, 20),
                      splitrule = c("gini", "extratrees"))

rf_model <- train(diagnosis~.,
                  data = training_data,
                  method = "ranger",
                  trControl = train_control,
                  tuneGrid = rf_grid,
                  metric = "F1",
                  importance = "permutation" # Allows feature importance to be visualised
                  )
  ## Evaluate the model training and test metrics, and visualise feature importance
rf_model
rf_Predictions <- predict(rf_model, newdata = test_data)
evaluateModel(rf_Predictions, test_data$diagnosis, "randomForest", "Supervised")
ggplot(varImp(rf_model)) + labs(title = "Feature Importance of randomForest")


# K-Nearest Neighbours
      # Non-parametric, not requiring assumptions about the distribution of data
      # Doesn't train an actual model, and is sensitive to low-relevance features
  ## Define hyperparameter grid and train the model
knn_grid <- expand.grid(k = seq(3, 23, by = 4)) #Odd 'k' avoids tie votes

knn_model <- train(diagnosis~.,
                 data = training_data,
                 method = "knn",
                 trControl = train_control,
                 tuneGrid = knn_grid,
                 metric = "F1",
                 )

  ## Evaluate the model training and test metrics, and visualise feature importance
knn_model
knn_Predictions <- predict(knn_model, newdata = test_data)
evaluateModel(knn_Predictions, test_data$diagnosis, "KNN", "Supervised")


# XGBoost
      # Flexible and often high performance, uses gradient descent to repeatedly tune trees and then ensembles them
      # Handles multicollinearity and nonlinearity, and uses regularisation to reduce overfitting
      # However it is computationally expensive and sensitive to hyperparameters
  
  ## Due to computational cost of searching over all possible hyperparameters, allow caret to generate a (granular) grid to search:
XGB_model <- train(diagnosis~.,
                  data = training_data,
                  method = "xgbTree",
                  trControl = train_control,
                  tuneLength = 10, # Set a high granularity for the search
                  metric = "F1"
                  )

  ## Evaluate the model training and test metrics, and visualise feature importance
XGB_model
XGB_model$bestTune
XGB_Predictions <- predict(XGB_model, newdata = test_data)
evaluateModel(XGB_Predictions, test_data$diagnosis, "XGBoost", "Supervised")
ggplot(varImp(XGB_model)) + labs(title = "Feature Importance of XGBoost")


# Naive Bayes
      # Efficient to train, and robust to redundant features
      # However assumes conditional independent (although can still be effective if violated)
      # Requires assumption of Gaussian probability distribution

  ## Define hyperparameter grid and train the model
NBayes_grid <- expand.grid(usekernel = c(TRUE, FALSE),
                          fL = c(0, 1, 2),
                          adjust = c(0.5, 1, 1.5, 2)
                          )

NBayes_model <- train(diagnosis~.,
                     data = training_data,
                     method = "nb",
                     trControl = train_control,
                     tuneGrid = NBayes_grid,
                     metric = "F1")

  ## Evaluate the model training and test metrics, and visualise feature importance
NBayes_model
NBayes_Predictions <- predict(NBayes_model, newdata = test_data)
evaluateModel(NBayes_Predictions, test_data$diagnosis, "Naive Bayes", "Supervised")
ggplot(varImp(NBayes_model)) + labs(title = "Feature Importance of Naive Bayes")


# Neural Network
      # Flexible and effective at capturing complex patterns and non-linear relationships
      # Can overfit, especially if dataset is not large (which this isn't)
      # Black-box means minimal intrepretability 
  
  ## Define hyperparameter grid and train the model
neuralnet_grid <- expand.grid(size = c(1, 3, 5, 7, 10),
                             decay = c(0.001, 0.01, 0.1, 0.5, 1)
                             )

neuralnet <- train(diagnosis~.,
                  data = training_data,
                  method = "nnet",
                  trControl = train_control,
                  tuneGrid = neuralnet_grid,
                  metric = "F1",
                  trace = FALSE)

  ## Evaluate the model training and test metrics, and visualise feature importance
neuralnet
neuralnet_Predictions <- predict(neuralnet, newdata = test_data)
evaluateModel(neuralnet_Predictions, test_data$diagnosis, "Neural Network", "Supervised")
ggplot(varImp(neuralnet)) + labs(title = "Feature Importance of Neural Network")


# Supervised Learning Findings:
Model_Comparisons[Model_Comparisons$Model_Type == "Supervised",]
      # Models have been trained using repeated 5-fold cross validation to reduce the risk of overfitting
      # Hyperparameters have been tuned using grid-search
      # Models varied significantly in processing time and requirements
      # However all produced excellent accuracy across both the training and testing data
      # Supervised models outperformed unsupervised models almost entirely
      # Models tended to have greater Specificity than Sensitivity - perhaps due to the imbalanced classes
      # Training new models using re-sampling techniques to balance classes may prove effective
      
# Tidy the workspace
rm(knn_grid, logistic_grid, NBayes_grid, neuralnet_grid, rf_grid, SVM_grid, train_control, knn_Predictions, NBayes_Predictions, neuralnet_Predictions, rf_Predictions, sigma_estimates, SVM_Predictions, XGB_Predictions)



#### Model Comparison and Findings ####

# Compare all models, plotting the metrics of each:
Model_Comparisons
ggparcoord(
  data=Model_Comparisons, columns = 3:ncol(Model_Comparisons), groupColumn = "Model_Name", showPoints = TRUE, scale="globalminmax", alphaLines = 0.3,
  mapping = ggplot2::aes(size = 4, linewidth = 0.8, linetype = "dashed")) + 
  ggplot2::scale_size_identity() + ggplot2::scale_linewidth_identity() + ggplot2::scale_linetype_identity() +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.75, 1)) +
  labs(title = "Comparison of Model Performance", y = "Value", x = "Metric", colour = "Model Name") + 
  theme_ipsum() +
  scale_colour_paletteer_d("ggthemes::Classic_10")

# Model Evaluation and Findings:
      # Supervised models generally outperform the unsupervised across all metrics
        ## The exception is specificity, where K-means and K-medoid do perform well
        ## When compared with sensitivity scores, this suggests the clustering models are "over-fitting" to negative cases, likely due to imbalanced classes
        ## Model-based clustering performs the best for sensitivity despite performing worse on every other metric, which follows as model-based clustering can handle minority clusters better
      # Except for the model-based clustering, all models perform very well (>= 95%) for specificity, whilst sensitivity is considerably more varied
        ## Again this suggests over-fitting to the majority class.
        ## While F1-score was used to attempt to address these, additional methods such as re-sampling and adjusting misclassification costs may be effective.
      # The neural network, randomForest and XGBoost models stand as the best, although all supervised models perform relatively well.
        ## These models are better equipped to handle non-linear relationships, imbalanced classes and multicollinearity than the other models
      # Overall, the neural network stands out as the optimal model, scoring best across all but Specificity (where randomForest is slightly ahead)

# Further tune neuralnet, increasing grid granularity
neuralnet_grid.granular <- expand.grid(size = c(1, 3, 5, 7, 10, 15, 20),
                              decay = c(0.0001, 0.001, 0.01, 0.1, 0.5, 1, 2, 5, 10)
)

neuralnet.tuned <- train(diagnosis~.,
                    data = training_data,
                    method = "nnet",
                    trControl = train_control,
                    tuneGrid = neuralnet_grid,
                    metric = "F1",
                    trace = FALSE)

neuralnet.tuned_Predictions <- predict(neuralnet.tuned, newdata = test_data)
evaluateModel(neuralnet.tuned_Predictions, test_data$diagnosis, "Neural Network (Tuned)", "Supervised")

# Compare to original neural network:
NeuralNet_Comparison <- 
  Model_Comparisons %>%
  filter(Model_Name %in% c("Neural Network", "Neural Network (Tuned)")) %>%
  pivot_longer(cols = -c(Model_Name,Model_Type),
               names_to = "Metric",
               values_to = "Value")

ggplot(NeuralNet_Comparison, aes(x = Metric, y = Value, fill = Model_Name)) +
    geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = scales::percent(Value, accuracy = 0.1)),
              position = position_dodge(width = 0.9),
              vjust = 1,
              size = 3) +  
    coord_cartesian(ylim=c(0.75, 1)) +
    labs(title = "Model Performance (Zoomed In)",
         y = "Score",
         x = "Metric",
         fill = "Model") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(labels = label_percent(accuracy = 0.1))
      # Although accuracy and F1-score have not changed, and Specificity has actually worsened,
        ## The sensitivity (and balanced accuracy) of the model is now better
      # In the diagnostic context, this is optimal.
      # Therefore the final model has a Sensitivity of 98.4%, and an an overall accuracy of 97.6%!

# Visualise  predictions
  ## Extract test_data actuals, and compare to predictions
Actual_Diagnoses.test <-  test_data$diagnosis
neuralnet.tuned_predictionStatus <- case_when(Actual_Diagnoses.test == "M" & neuralnet.tuned_Predictions == "M" ~ "True Pos", 
                                         Actual_Diagnoses.test == "B" & neuralnet.tuned_Predictions == "M" ~ "False Pos", 
                                         Actual_Diagnoses.test == "B" & neuralnet.tuned_Predictions == "B" ~ "True Neg", 
                                         Actual_Diagnoses.test == "M" & neuralnet.tuned_Predictions == "B" ~ "False Neg")

  ## Perform PCA on test_data, and visualise prediction status
test_data.PCA <-  test_data %>% select(-diagnosis) %>% prcomp(scale = TRUE) 
plot_ly(
  as.data.frame(test_data.PCA$x[,1:3]), 
  x = ~PC1, y = ~PC2, z = ~PC3, 
  type = "scatter3d", mode = "markers", 
  color = ~neuralnet.tuned_predictionStatus, size = I(130)) %>% 
  layout(paper_bgcolor = "#595c61", scene = list(xaxis = list(color = "#ffffff"), yaxis = list(color = "#ffffff"), zaxis = list(color = "white")), legend = list(font = list(color = "white")))
      # All incorrect predictions are on the border between natural clusters
      # Infact, all 3 False Positives are located very close together in the reduced data space

# Extract and plot feature importance:
olden(neuralnet$finalModel, bar_plot = FALSE) %>% 
  rownames_to_column(var = "Feature") %>%
  mutate(Sign = ifelse(importance >= 0, "Positive", "Negative"),
         abs_importance = abs(importance)) %>%
  ggplot(aes(x = reorder(Feature, abs_importance), y = abs_importance, colour = Sign, alpha=abs_importance)) +
  geom_segment(aes(xend = Feature, y = 0, yend = abs_importance), linewidth = 1) +
  geom_point(size = 3) +
  coord_flip() +
  labs(x = "Feature", y = "Absolute Importance", title = "Neural Network Feature Importance") +
  guides(alpha = "none") + 
  scale_colour_manual(values = c("Positive" = "steelblue", "Negative" = "firebrick")) +
  theme_minimal()
      # variables relating to shape and size variability are the most important predictors (such as texture_worst, radius_se, symmetry_worst)
      # This aligns with medical understanding
      # Several variables have inverse relationships with malignancy, although this could be due to multicollinearity & redundant columns

finalModel_Comparisons = Model_Comparisons
save(neuralnet.tuned, finalModel_Comparisons, file="breastCancer_neuralNet_finalModel_ST.Rda")
