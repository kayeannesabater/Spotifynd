library(dplyr)
library(ggplot2)
library(corrplot)
library(ipred)
library(rpart)
library(randomForest)
library(caTools)

# 1. DATA CLEANING
df <- read.csv("dataset_cleaned.csv", header = T) # duplicates already removed
df <- df[,-1]
rename_titles <- df[,1]
View(df)

sum(is.na(df)) # to check for any missing values

# 2. DATA TRANSFORMATION
df$artists <- as.factor(df$artists)
df$album_name <- as.factor(df$album_name)
df$track_name <- as.factor(df$track_name)
df$explicit <- as.factor(df$explicit)
df$track_genre <- as.factor(df$track_genre)

# 3. HISTOGRAM DISTRIBUTIONS - BEFORE NORMALIZATION
hist(df$danceability, 
     main = "Distribution of Danceability", 
     col = "lightcoral",
     xlab = "Danceability")
hist(df$energy,
     main = "Distribution of Energy", 
     col = "lightcoral",
     xlab = "Energy")
hist(df$key,
     main = "Distribution of Key", 
     col = "lightcoral",
     xlab = "Key")
hist(df$loudness,
     main = "Distribution of Loudness", 
     col = "lightcoral",
     xlab = "Loudness")
hist(df$mode,
     main = "Distribution of Mode", 
     col = "lightcoral",
     xlab = "Mode")
hist(df$speechiness,
     main = "Distribution of Speechiness", 
     col = "lightcoral",
     xlab = "Speechiness")
hist(df$acousticness,
     main = "Distribution of Acousticness", 
     col = "lightcoral",
     xlab = "Acousticness")
hist(df$instrumentalness,
     main = "Distribution of Instrumentalness", 
     col = "lightcoral",
     xlab = "Instrumentalness")
hist(df$liveness,
     main = "Distribution of Liveness", 
     col = "lightcoral",
     xlab = "Liveness")
hist(df$valence,
     main = "Distribution of Valence", 
     col = "lightcoral",
     xlab = "Valence")
hist(df$tempo,
     main = "Distribution of Tempo", 
     col = "lightcoral",
     xlab = "Tempo")
hist(df$time_signature,
     main = "Distribution of Time Signature", 
     col = "lightcoral",
     xlab = "Signature")

# 4. NORMALIZATION FOR K-MEANS CLUSTERING
df_num_data <- df[sapply(df, is.numeric)]
df_scaled <- scale(df_num_data)
df_scaled <- as.data.frame(scale(df_num_data))
summary(df_scaled)
View(df_scaled)

# 5. CORRELATION MATRIX
pairs(df_scaled)
cor(df_scaled)
corr_matrix <- cor(df_scaled)

# to only display attributes that have moderate-to-strong correlations ≥ 0.5
threshold <- 0.5
high_corr <- which(abs(corr_matrix) > threshold, arr.ind = TRUE)
for (i in 1:nrow(high_corr)) {
  row <- high_corr[i, 1]
  col <- high_corr[i, 2]
  # Avoid self-correlation (diagonal)
  if (row != col) {
    cat(paste0(rownames(corr_matrix)[row], " and ", colnames(corr_matrix)[col], 
               ": ", round(corr_matrix[row, col], 2), "\n"))
  }
}

# 6. HISTOGRAM DISTRIBUTIONS - AFTER NORMALIZATION
hist(df_scaled$danceability, 
     main = "Distribution of Danceability", 
     col = "lightcoral",
     xlab = "Danceability")
hist(df_scaled$energy,
     main = "Distribution of Energy", 
     col = "lightcoral",
     xlab = "Energy")
hist(df_scaled$key,
     main = "Distribution of Key", 
     col = "lightcoral",
     xlab = "Key")
hist(df_scaled$loudness,
     main = "Distribution of Loudness", 
     col = "lightcoral",
     xlab = "Loudness")
hist(df_scaled$mode,
     main = "Distribution of Mode", 
     col = "lightcoral",
     xlab = "Mode")
hist(df_scaled$speechiness,
     main = "Distribution of Speechiness", 
     col = "lightcoral",
     xlab = "Speechiness")
hist(df_scaled$acousticness,
     main = "Distribution of Acousticness", 
     col = "lightcoral",
     xlab = "Acousticness")
hist(df_scaled$instrumentalness,
     main = "Distribution of Instrumentalness", 
     col = "lightcoral",
     xlab = "Instrumentalness")
hist(df_scaled$liveness,
     main = "Distribution of Liveness", 
     col = "lightcoral",
     xlab = "Liveness")
hist(df_scaled$valence,
     main = "Distribution of Valence", 
     col = "lightcoral",
     xlab = "Valence")
hist(df_scaled$tempo,
     main = "Distribution of Tempo", 
     col = "lightcoral",
     xlab = "Tempo")
hist(df_scaled$time_signature,
     main = "Distribution of Time Signature", 
     col = "lightcoral",
     xlab = "Signature")

# 7. HIERACHIAL CLUSTERING (K-MEANS CLUSTERING OF SONGS) 
set.seed(123)
k <- 10 # this determines how many genre clusters we want, either 10 or 20
genre_clusters <- kmeans(df_scaled, centers = k)
View(genre_clusters)

# add cluster label back to the dataset
df$genre_cluster <- as.factor(genre_clusters$cluster)
View(df)

# 8. DIVIDE DATASET (TRAIN-TEST SPLIT)
split <- sample.split(df$genre_cluster, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

selected_features <- c("tempo", "acousticness", "liveness", "key", "mode", "speechiness", 
                       "energy", "loudness", "valence", "danceability")

train_selected <- train[, selected_features]

# 9. RANDOM FOREST CLASSIFIER
set.seed(42)
classifier_RF <- randomForest(
  x = train_selected,
  y = train$genre_cluster,
  ntree = 500
)
print(classifier_RF)

# 10. CLASSIFICATION MODEL: N/A

# 11. EVALUATION OF MODEL EFFECTIVENESS (CONFUSION MATRIX & ACCURACY)
pred <- predict(classifier_RF, test[, selected_features])
conf_mat <- table(Predicted = pred, Actual = test$genre_cluster)

accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
print(conf_mat)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")
