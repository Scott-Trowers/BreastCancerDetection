  ## Load required libraries, set default ggplot theme, set seed
library(tidyverse)
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
theme_set(theme_classic())
set.seed(5)
  
  ## Load the data
Raw_Breast_Cancer_Data = read.csv("C:\\Users\\User\\Documents\\Projects\\Datasets\\breast+cancer+wisconsin+diagnostic\\data.csv")


###########
#### Exploratory Analysis
###########

  ## Data structure:
dim(Raw_Breast_Cancer_Data)
    #33 columns, 593 observations
vis_dat(Raw_Breast_Cancer_Data, warn_large_data = F, palette="qual") 
    #All but 3 columns are numeric (float). 'diagnosis' is string data, 'id' is integer, and 'X' appears to be entirely NULL
vis_miss(Raw_Breast_Cancer_Data) #confirms 'X' is entirely NULL, and no other values are
nrow(Raw_Breast_Cancer_Data[duplicated(Raw_Breast_Cancer_Data), ]) #no duplicsted rows in the data
length(unique(Raw_Breast_Cancer_Data$id)) - length(Raw_Breast_Cancer_Data$id) #'id' is a unique identifier, and therefore redundandt
Breast_Cancer_Data = select(Raw_Breast_Cancer_Data, -c(X, id)) #drop the NULL 'X' column
Breast_Cancer_Data$diagnosis = as.factor(Breast_Cancer_Data$diagnosis) #set diagnosis to be a factor
summary(Breast_Cancer_Data) #none of the values are negative, which is good (as these would be impossible)
      #Two columns were redundant, the rest are all complete observations
      #'diagnosis' is a qualitative variable (our target variable). The other 30 are quantitative

 #'diagnosis' is the target (and only categorical) variable
 #explore 'diagnosis':
ggplot(data=as.data.frame(table(Breast_Cancer_Data$diagnosis)), aes(x=Var1, y=Freq, fill=Var1)) + #count the frequency of values in 'diagnosis'
  geom_bar(stat="identity") +
  geom_text(aes(label=Freq), vjust=2) + 
  labs(title = "Distribution of 'Diagnosis'", 
       x = "Diagnosis",
       y = "Frequency") +
  theme(legend.position = "none")
      #Classes are imbalanced: 357 are class 'B', 212 are class 'M'
      #These relate to M = Malignant (indicating precense of cancer) and B = Benign
      #We want to build a model to predict this variable

  #explore feature variables
  #plot 5x6 grid of histograms, 1 for each numeric variable
histogram_list = list() #create empty list
for (i in colnames(Breast_Cancer_Data[,-1])) { #create histogram for each numeric variable, and append to the list
  histogram_list[[i]] = ggplot(data=Breast_Cancer_Data, aes(x=.data[[i]])) + 
    geom_histogram(fill="red2", col="darkred", alpha=0.15) + 
    theme(axis.title.y=element_blank())
}
wrap_plots(histogram_list, ncol=6, nrow=5) + #plot histograms on a 6x5 grid
  plot_annotation(title = "Histograms of Breast Cancer Dataset Features")
  #plot boxplots of the numeric variables
boxplot(Breast_Cancer_Data[,-1])
      #Many of the variables are on very different scales, from fractel_dimension_se (ranging from 0.00 to 0.03) to area_worst (ranging from 185 to 4254)
      #Many of the variables are heavily positively skewed
        #area_se, radius_se, concativity_se, perimeter_se and fractal_dimension_se all have particularly long right tails
      #There are many positive outliers throughout the data, but fairly few negative outliers

  #encode diagnosis as a numeric variable, then plot correlations between numeric variablese
Breast_Cancer_Data.encoded =  Breast_Cancer_Data %>% #encode diagnosis as a numeric variable
  mutate(encoded_diagnosis = if_else(diagnosis == "M", 1, 0)) 
corrplot(cor(Breast_Cancer_Data.encoded[,-1]), order = "AOE", type="lower", diag=F)
      #there are some strong positive correlations in the data, indicating multicollinearity
      #there are some fairly strong correlations between 'coded_diagnosis' and several variables, indicating possible linear relationships
        #perimeter_worst, perimeter_mean, radius_worst, area_worst, area_mean, and radius_mean all have the strongest correlation with a positive ("M") diagnosis
      #save column names which have r >= 0.4 with encoded_diagnosis for later:
correlated_features = Breast_Cancer_Data.encoded[,-1] %>%
  correlate() %>%              # Calculate all pairwise correlations
  focus(encoded_diagnosis) %>%        # Focus on correlations with 'target_var'
  filter(encoded_diagnosis > 0.5) %>% # Filter for correlations greater than 0.5
  pull(term)                   # Extract the column names (the 'term' column)
print(correlated_features)
  #Parallel Coordinates Plot, showing every observation in the data (standardised) and coloured by diagnosis
ggparcoord(Breast_Cancer_Data.encoded, column=2:ncol(Breast_Cancer_Data.encoded), groupColumn=1, order="anyClass", scale="std", alpha=0.4) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_line(color = "grey90", linetype="dashed")  #rotate axis labels and add grid lines for readability
  )      
      #radius_se, perimeter_se, area_se, concavity_se and fractal_dimension_se has some very high positive outliers
      #there is clear separation between the diagnosis classifications in many of the variables.
      #this suggests a classification model should be highly effective
  #let's examine our correlated_features closer:
corrplot(cor(Breast_Cancer_Data.encoded[,c("encoded_diagnosis", correlated_features)]), order = "AOE", type="lower", diag=F)
pairs.panels(Breast_Cancer_Data.encoded[,c("encoded_diagnosis", correlated_features)],
             method="pearson", pch=21, bg=c("red2", "cyan3")[Breast_Cancer_Data.encoded$diagnosis], 
             cex=0.8, cex.cor=2,
             scale=TRUE, ellipses=FALSE)
      #there are some very strong correlations with diagnosis in these 15 variables
      #there is also alot of multicollinearity
      #also evidence of some non-linear relationships between variables
      #strong evidence of seperations by diagnosis throughout these variables
  #when two of these feature variables have a correlation of 0.95, let's select the one that has the highest correlation with diagnosis, and remove the other from our selection


##################
to_remove <- c()  # Initialize vector to store features to drop
cor_matrix <- cor(Breast_Cancer_Data.encoded[,c(correlated_features)])
for (i in 1:(ncol(cor_matrix)-1)) {
  for (j in (i+1):ncol(cor_matrix)) {
    if (abs(cor_matrix[i, j]) >= 0.95) {
      feature_i <- colnames(cor_matrix)[i]
      feature_j <- colnames(cor_matrix)[j]
      
      # Get correlation with target for both
      cor_i <- cor(Breast_Cancer_Data.encoded[[feature_i]], Breast_Cancer_Data.encoded$encoded_diagnosis)
      cor_j <- cor(Breast_Cancer_Data.encoded[[feature_j]], Breast_Cancer_Data.encoded$encoded_diagnosis)
      
      # Remove the one with the smaller correlation with target
      if (abs(cor_i) >= abs(cor_j)) {
        to_remove <- c(to_remove, feature_j)
      } else {
        to_remove <- c(to_remove, feature_i)
      }
    }
  }
}

# Remove duplicates in case a variable appeared multiple times

final_features <- setdiff(correlated_features, to_remove)
######################
#visualise these features

corrplot(cor(Breast_Cancer_Data.encoded[,c("encoded_diagnosis", final_features)]), order = "AOE", type="lower", diag=F)
pairs.panels(Breast_Cancer_Data.encoded[,c("encoded_diagnosis", final_features)],
             method="pearson", pch=21, bg=c("red2", "cyan3")[Breast_Cancer_Data.encoded$diagnosis], 
             cex=0.8, cex.cor=2,
             scale=TRUE, ellipses=FALSE)


#Density plot, split by diagnosis
density_list = list() #create empty list
for (i in colnames(Breast_Cancer_Data[,-1])) { #create density plot for each numeric variable, split by diagnosis and append to the list
  density_list[[i]] = ggplot(data=Breast_Cancer_Data, aes(x=.data[[i]], group=diagnosis, fill=diagnosis)) + 
    geom_density(alpha=0.45) + 
    theme(axis.title.y=element_blank()) 
}
wrap_plots(density_list, ncol=6, nrow=5, guides="collect") + #plot plots on a 6x5 grid
  plot_annotation(title = "Density Plots of Breast Cancer Dataset Features (by Diagnosis)") 
      #Confirms clear separation by diagnosis in the majority of variables
      #Those that are separated tend to have negative association with Benign diagnosis
      #some of the variables (e.g. symmetry_mean, texture_mean, smoothness_mean, etc) appear closer to a normal distribution when split by diagnosis
      
#EDA findings:
  #Cleaned data by removing redundant columns
  #Imbalanced classes in the target variable
  #High level of positive skew and outliers throughout the data
  #multicollinearity is present in the data; some ML models will handle this automatically via feature selection, others won't
    #dimension reduction may be successful
  #clear seperation in many of the variables between diagnosis classification, suggesting a classification algorithm will be effective


###########
### Unsupervised Learning Exploration, Clustering and Prediction
###########
library(factoextra)
library(ggfortify)
library(plotly)
library(mclust)

  #High degrees of multicollinearity in the data suggests that dimension reduction may be affective
  #Dimension Reduction (PCA)
PCA = prcomp(Breast_Cancer_Data[,-1], scale=TRUE) #perform PCA on the feature variables, making sure to scale to prevent variable domination
fviz_screeplot(PCA, addlabels=TRUE) #plot explained variance per principal component (PC)
      #a high degree of the information is explained by just 2 (63.3%) or 3 (72.7%) PCs, showing multicollinearity and redundancy in the data
autoplot(PCA, data=Breast_Cancer_Data, colour="diagnosis", loadings=F) #plot the observations using the first 2 PCs, coloured by diagnosis
      #very clear separation of diagnoses in the first two components
plot_ly(as.data.frame(PCA$x[,1:3]), x=~PC1, y=~PC2, z=~PC3, color=~Breast_Cancer_Data$diagnosis, size=I(130) #plot the first 3 components in 3d space, colour by diagnosis
) %>% layout(paper_bgcolor = "#595c61", scene=list(xaxis=list(color="#ffffff"), yaxis=list(color="#ffffff"), zaxis=list(color="white")), legend=list(font=list(color="white")))
      #even stronger separation in 3 PCs, ?may suggest an SVM may be effective

  #Factor Analysis seeks to model an unseen 'latent' variable in the data that me driving other values
FA = factanal(Breast_Cancer_Data[,-1], factors=3, lower=0.01, scores="regression")
FA #factor likelihood test fails, meaning 3 factors is not enough to explain trends in the data. However 68.4% of variance in the data is accounted for
autoplot(FA, data=Breast_Cancer_Data, colour="diagnosis", loadings=F) #plot the observations using the first 2 Factors, coloured by diagnosis
      #clear separation again, although not as distinct as using PCA.
plot_ly(as.data.frame(FA$scores[,1:3]), x=~Factor1, y=~Factor2, z=~Factor3, color=~Breast_Cancer_Data$diagnosis, size=I(130) #plot the factors in 3d space, colour by diagnosis
        ) %>% layout(paper_bgcolor = "#595c61", scene=list(xaxis=list(color="#ffffff"), yaxis=list(color="#ffffff"), zaxis=list(color="white")), legend=list(font=list(color="white")))
  #Cluster Analysis
  #With such strong separation, cluster analysis may be effective at classifying the diagnoses
  #K-means
Breast_Cancer_Data.scaled = scale(Breast_Cancer_Data[,-1]) #scale the data to prevent domination by higher-scale features
fviz_nbclust(Breast_Cancer_Data.scaled, kmeans, method = "wss") # use sum of squares to estimate required number of clusters
      #using 'elbow' heuristic suggests that 3 clusters may be optimal, although 2 also look possible
fviz_nbclust(Breast_Cancer_Data.scaled, kmeans, method = "silhouette")+ #calculate silhouette measure to confirm optimal number of clusters
  labs(title = "K-means silhouette plot")
      #confirms that 2 is optimal

Kmeans_2c = kmeans(Breast_Cancer_Data.scaled, 2, nstart=25) #perform k-means clustering with 3 cluster and 25 initial cluster assignments
Kmeans_2c 
      #only 32.1% of the variability is explained by this model
table(Breast_Cancer_Data$diagnosis, Kmeans_2c$cluster) #compare diagnoses with clusters
labelled_clusters <- as.factor(ifelse(Kmeans_2c$cluster == 1, "M", "B")) #rename the cluster to align with diagnoses
confusionMatrix(labelled_clusters, Breast_Cancer_Data$diagnosis)  #create confusion matrix and calculate statistics
      #however prediction accuracy is fairly good! Which is more important than the within cluster variance for our purposes
      #91.04% overall accuracy, although this may be skewed by the imbalanced Benign class (when true positive rate and false negative rates are the most impactful in diagnosing)
      #82.55% true positive rate,, 3.03% false negative rate, 17.45% false positive rate, 96.07% true negative rate
fviz_cluster(Kmeans_2c, Breast_Cancer_Data[,-1], ellipse.type = "norm") #visualise clusters using prinicipal components

#k-medoid clustering is less sensitive to outliers:
fviz_nbclust(Breast_Cancer_Data.scaled, clara, method = "silhouette")+ #calculate silhouette measure to confirm optimal number of clusters
  labs(title = "K-medoid silhouette plot") #again confirms 2 is optimal
Kmedoid = clara(Breast_Cancer_Data.scaled, k=2)
table(Breast_Cancer_Data$diagnosis, Kmedoid$clustering)
    #prediction accuracy of Malignent is actually worse under medoids (although Benign is slightly better than k-means)

  #repeat with heirarchical clustering
fviz_nbclust(Breast_Cancer_Data.scaled, hcut, method = "silhouette")+
  labs(title = "Hierarchical") #confirms 2 clusters are still optimal
# Compute distances and hierarchical clustering
euclid = dist(Breast_Cancer_Data.scaled, method = "euclidean") #calculate euclidean distances
hc = hclust(euclid, method = "complete") #calculate heirarchical clusters
fviz_cluster(list(data=Breast_Cancer_Data.scaled, cluster=cutree(hc,2)), ellipse.type = "norm") #plot the clusters
    #only two points are in the second cluster!
plot(as.dendrogram(hc), main = "Iris data - Complete linkage", #plot the clusters as a dendogram
     ylab = "Height", cex = .01)
    #a small number of points are seperated 'early' in the tree.
    #it would require 4 cuts to create a significant second grouping:
fviz_cluster(list(data=Breast_Cancer_Data.scaled, cluster=cutree(hc,4)), ellipse.type = "norm") #plot the clusters
plot_ly(as.data.frame(PCA$x[,1:3]), x=~PC1, y=~PC2, z=~PC3, color=~cutree(hc,4), size=I(130) #plot the first 3 components in 3d space, colour by prediction type (true Positive, False Positive, etc.)
) %>% layout(paper_bgcolor = "#595c61", scene=list(xaxis=list(color="#ffffff"), yaxis=list(color="#ffffff"), zaxis=list(color="white")), legend=list(font=list(color="white")))
    #outliers have been seperated into two groups,  and there are now two main groups
hc_cut = cutree(hc, k=4) #cut the tree at the 4th level
table(Breast_Cancer_Data$diagnosis, hc_cut) #compare to diagnoses
    #cluster 3 appears to align with Benign diagnoses, and all others with Malignant
confusionMatrix(as.factor(ifelse(hc_cut == 3, "B", "M")), Breast_Cancer_Data$diagnosis, positive="M") #create a confusion matrix of predictions
      #prediction accuracy is better than k-medoids but slightly worse than k-means

  #and model based clustering
Model_Cluster_2c = Mclust(Breast_Cancer_Data.scaled, G=2) #train a model-based clustering with 2 clusters
table(Breast_Cancer_Data$diagnosis, Model_Cluster_2c$classification) #compare to diagnoses
confusionMatrix(as.factor(ifelse(Model_Cluster_2c$classification == 2, "B", "M")), Breast_Cancer_Data$diagnosis, positive="M") #create a confusion matrix of predictions
      #although overall accuracy is lower than k-means (86.29%), true positive rate (arguably the most important diagnosis) is highest (88.68%)!
      #however false negative rate is higher (9.74%)

  #plot model-based cluster's predictions vs diagnoses in 3D
#create a dataframe showing prediction status (true positive, false positive, etc)
Diagnoses = as.character(Breast_Cancer_Data$diagnosis)
MC2_Predictions = as.data.frame(cbind(Diagnoses, ifelse(Model_Cluster_2c$classification == 2, "B", "M")))
colnames(MC2_Predictions) = c("Diagnosis", "Prediction")
Cluster_Comparison = ifelse(MC2_Predictions$Diagnosis == "M" &MC2_Predictions$Prediction == "M", "True Pos", 
                            ifelse(MC2_Predictions$Diagnosis == "B" & MC2_Predictions$Prediction == "M", "False Pos", 
                                   ifelse(MC2_Predictions$Diagnosis == "B" & MC2_Predictions$Prediction == "B", "True Neg", "False Neg"))
                              )
plot_ly(as.data.frame(PCA$x[,1:3]), type="scatter3d", mode="markers", x=~PC1, y=~PC2, z=~PC3, color=~Cluster_Comparison,size=I(130) #plot the first 3 components in 3d space, colour by prediction type (true Positive, False Positive, etc.)
          ) %>% layout(paper_bgcolor = "#595c61", scene=list(xaxis=list(color="#ffffff"), yaxis=list(color="#ffffff"), zaxis=list(color="white")), legend=list(font=list(color="white")))
        #most of the false positives tend to be outliers in the reduced dataspace
        #the false negatives are much closer to other datapoints, between the two main clusters
        #however they are largely located close together


  #plot model based cluster with 3 clusters, to see if the uncertain predictions will be clustered.
      #as this would be better than a misdiagnosis, inidicating a requirement for further investigation
#and model based clustering
Model_Cluster_c3 = Mclust(Breast_Cancer_Data.scaled, G=3) #train a model-based clustering with 2 clusters
table(Breast_Cancer_Data$diagnosis, Model_Cluster_c3$classification) #compare to diagnoses
        #Group 1 is uncertain
MC3_Predictions = cbind(Prediction = Model_Cluster_c3$classification, Actual= Breast_Cancer_Data$diagnosis) %>%
  as.data.frame() %>%
  mutate(Prediction = case_when(Prediction == 2 ~ "M", Prediction == 3 ~ "B", Prediction == 1 ~ "U")) %>%
  mutate(Actual = case_when(Actual == 1 ~ "B", Actual == 2 ~ "M")) %>%
  mutate(Prediction_Status = case_when(Prediction == "U" ~ "Uncertain",
                                       Actual == "M" & Actual == Prediction ~ "True Positive",
                                       Actual == "M" & Actual != Prediction ~ "False Positive",
                                       Actual == "B" & Actual == Prediction ~ "True Negative",
                                       Actual == "B" & Actual != Prediction ~ "False Negative",))

table(Breast_Cancer_Data$diagnosis, Model_Cluster_c3$classification) #compare to diagnoses
confusionMatrix(as.factor(MC3_Predictions[MC3_Predictions$Prediction_Status != "Uncertain",]$Prediction), as.factor(MC3_Predictions[MC3_Predictions$Prediction_Status != "Uncertain",]$Actual), positive="M")
plot_ly(as.data.frame(PCA$x[,1:3]), type="scatter3d", mode="markers", x=~PC1, y=~PC2, z=~PC3, color=~MC3_Predictions$Prediction_Status, size=I(130) #plot the first 3 components in 3d space, colour by prediction type (true Positive, False Positive, etc.)
) %>% layout(paper_bgcolor = "#595c61", scene=list(xaxis=list(color="#ffffff"), yaxis=list(color="#ffffff"), zaxis=list(color="white")), legend=list(font=list(color="white")))


X = Mclust(Breast_Cancer_Data.scaled)



###########
### Supervised Learning Prediction
###########
library(caret)
library(car)
        
#The presence of clear clusters in the data suggests a classification model should be effective
        #lets prepare the data for modelling, and then train and compare different modelling approaches
  
  #Process the data for modelling
    #1) Scale and encode the data
Breast_Cancer_Data.processed = Breast_Cancer_Data %>% 
  mutate(encoded_diagnosis = as.factor(diagnosis)) %>% #encode diagnosis
  select(-diagnosis) %>% #drop the original diagnosis column
  mutate(across(-encoded_diagnosis, scale)) #scale the feature variables
    #2) Split the data into training and test date (70%-30%)
split_index = createDataPartition(Breast_Cancer_Data.processed$encoded_diagnosis, p=0.7, list=FALSE)
training_data = Breast_Cancer_Data.processed[split_index,]
test_data = Breast_Cancer_Data.processed[-split_index,]
        #visualise the split
bind_rows(
  training_data |> mutate(dataset = "Train"), #recombine the data with a column indicating if it is training or test data
  test_data  |> mutate(dataset = "Test")
) |>
  ggplot(aes(x = dataset, fill = factor(encoded_diagnosis))) + #create a stacked bar chart showing the split between test and training, and between diagnoses
  geom_bar(position = "stack") +
  labs(
    title = "Train/Test Split with Diagnosis Breakdown",
    x = "Dataset",
    y = "Count",
    fill = "Diagnosis"
  ) 
        #classes are imbalanced, but relatively evenly split between test and training data

  #3) train models

     #3a) logistic regression
        #Assumptions: 
          #binary classification problem (it is, and we have encoded as 0 and 1)
          #independent observations
nrow(training_data[duplicated(training_data), ]) #no duplicates
          #little multicollinearity (we will use elastic net for feature selection) (We can also used our reduced dimensions, however this will drastically degrees feature interpretability)
          #linear relationships between feature variables and log-odds of target variable (we will check after the model is trained)
        #Training:
          #define training method
train_control = trainControl(method="repeatedcv", 
                             number=5,
                             repeats=5,
                             search="grid",
                             classProbs=TRUE,
                             summaryFunction=twoClassSummary)
          #define a grid for alpha (the regularisation parameter)
alpha_grid = expand.grid( 
  alpha = seq(0, 1, by = 0.1),       # 0 = Ridge, 1 = Lasso, in between = Elastic Net
  lambda = 10^seq(-4, 1, length = 20)  # log-spaced lambdas from small to large
)
          #train the model
logistic_model = train(encoded_diagnosis~.,
                       data=training_data,
                       method="glmnet",
                       trControl=train_control,
                       tuneGrid = alpha_grid,
                       metric = "ROC",
                       family = "binomial")

  #evaluate the trained model:
logistic_model$bestTune #optimal model has alpha = 0.1 and lambda = 0.01274, indicating the model is closer aligned to Ridge regularisation (and so less feature selection)


logistic_model$results[ #optimal model metrics
  logistic_model$results$alpha == logistic_model$bestTune$alpha &
  logistic_model$results$lambda == logistic_model$bestTune$lambda,
]         #very strong ROC indicates a very strong ability to discriminate, whilst sensitivity shows an excellent ability to detect true positives (missing 1.1% of cases)
          #specificty is also high, and therefore model is good at avoiding false positives
          #SD of all 3 are also low, indicating a stable model

  #plot feature coefficients:
coefs <- as.data.frame(as.matrix(coef(logistic_model$finalModel, s = logistic_model$bestTune$lambda))) #save coeffiencts as a dataframe
coefs$feature = rownames(coefs) #extract feature names
coefs$feature_status = ifelse(coefs$s1 == 0, "Unselected", ifelse(coefs$s1 > 0, "Positive", "Negative")) #categorise feature coefficients
ggplot(data=coefs, aes(y=reorder(feature, abs(s1)), x=abs(s1))) + #plot the features & coefficients
  geom_point(cex=2, aes(col=feature_status)) + 
  geom_segment(aes(yend=feature, x=0, xend=abs(s1)), col="grey60", linetype="dashed") +
  labs(title="Logistic Model Coefficients", x="Absolute Coefficient Value", y="Feature")
        #as the features are scaled, the absolute values of the coefficient (shown in the chart) also gives the feature importance
        #2 variables have been unselected by the regularisation of the model, texture_se and compactness, whilst several also have very low importance
        #the most important features are texture_worst, radius_se, radius_worst, smootness_worst, concave.points_worst, concave.points_mean, area_worst and area_se; 
            #all having a positive association with Malignent (positive) diagnoses.



  #test the model
logistic_predictions = predict(logistic_model, newdata=test_data) #predict based on test data
confusionMatrix(logistic_predictions, test_data$encoded_diagnosis, positive="M")
        #final model has an accuracy of 98.24%, with correctly diagnosing 96.83% of positive cases!
logistic_predictions.actuals = as.data.frame(cbind(logistic_predictions, test_data$encoded_diagnosis)) %>% #attach predictions to actuals
  setNames(c("Prediction", "Actual")) %>%  #classify each prediction (e.g. true positive, false negative, etc)
  mutate(Prediction_Status = ifelse(Actual == 1 & Prediction==Actual, "True Positive", 
                                    ifelse(Actual == 1 & Prediction!=Actual, "False Negative",
                                           ifelse(Actual == 2 & Prediction==Actual, "True Negative",
                                                  "False Positive"))))

 
  #boosted decision tree
  #random forest
  #KnearestNeighbours
  #XGBoost
  #naive bayes
  #Neural network
  #SVM
  



#compare models
