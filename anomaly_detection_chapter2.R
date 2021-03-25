library(FNN)
# View the contents of the wine data
wine <- read.csv("wine.csv",head=T,sep = ";",dec = ",")
str(wine)

# Scatterplot of wine pH against alcohol
plot(pH ~ alcohol, data = wine)

# Calculate the 5 nearest neighbors distance
wine_nn <- get.knn(wine, k = 5)

# View the distance matrix
head(wine_nn$nn.dist)

# Distance from wine 5 to nearest neighbor
wine_nn$nn.dist[5, 1]

# Row index of wine 5's nearest neighbor 
wine_nn$nn.ind[5, 1]

# Return data for wine 5 and its nearest neighbor
wine[c(5,1751 ), ]
# Calculate the 5 nearest neighbors distance
wine_nn <- get.knn(wine, k = 5)

# Create score by averaging distances
wine_nnd <- rowMeans(wine_nn$nn.dist)

# Print row index of the most anomalous point
which.max(wine_nnd)
##########Estandarizando los datos a fin de obtener las mismas escalas###################
# Without standardization, features have different scales
summary(wine)

# Standardize the wine columns
wine_scaled <- scale(wine)

# Standardized features have similar means and quartiles
summary(wine_scaled)
# Print the 5-nearest neighbor distance score
wine_nnd[1:5]

# Append the score as a new column 
wine$score<-wine_nnd

# Scatterplot showing pH, alcohol and kNN score

plot(pH ~ alcohol, data=wine, cex = sqrt(score), pch = 20)
################################################################################################
#LOCAL OUTLIER FACTOR
##################################################################################
library(dbscan)
# Calculate the LOF for wine data
wine_lof <- lof(scale(wine), 5)

# Append the LOF score as a new column
wine$score <- wine_lof
# Scatterplot showing pH, alcohol and LOF score
plot(pH ~ alcohol, data=wine,cex=score,pch=20)
#####################################################################
#lof vs knn
######################################################################
# Scaled wine data
wine_scaled <- scale(wine)

# Calculate and append kNN distance as a new column
wine_nn <- get.knn(wine_scaled, k = 10)
wine$score_knn <- rowMeans(wine_nn$nn.dist)     

# Calculate and append LOF as a new column
wine$score_lof <- lof(wine_scaled, k = 10)

# Find the row location of highest kNN
which.max(wine$score_knn)

# Find the row location of highest LOF
which.max(wine$score_lof)
