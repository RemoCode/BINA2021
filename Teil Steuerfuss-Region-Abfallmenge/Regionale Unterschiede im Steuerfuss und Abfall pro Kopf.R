library(Rtsne) # library for TSNE
library(corrplot) # it is a graphical display of a correlation matrix, confidence interval 
library(gplots)  # for making own color code (e.g. for heatmap representation)
library(kohonen) # functions to train self-organising maps (SOMs)
library(plot3D) # functions for viewing 2-D & 3-D data, including perspective plots, slice plots, scatter plots, etc.
library(factoextra)  # more complexe library than standard R library; all methods for distance & linkage available
library(dendextend) # can be used to represent the data in ???fan??? diagrams
library(ape) # can be used to represent the data in ???fan??? diagrams
library(RColorBrewer)  # for making own color code and show different shades of colors (e.g. for heatmap representation)
library(rmarkdown) # convert R Markdown documents into a variety of formats
library(DataExplorer) # automated data exploration process for analytic tasks and predictive modeling 
library(tidyverse)  # library for SOM
library(kohonen)  # library for SOM
library(checkpoint) # package to solve the problem of package reproducibility in R
library(readxl)



############## K-Means Clustering ###########################
df_abfall <- read_excel("Abfallstatistik Gemeinden 2000 bis 2019_Revidiert.xlsx")
View(df_abfall)

# count the missing values (NA) in the data frame
sum(is.na(df_abfall))

# take year 2019
df_abfall = subset(df_abfall, df_abfall$Jahr == "2019")
View(df_abfall)

# eliminate outlier "Zürich"
df_abfall = subset(df_abfall, df_abfall$Gemeinde != "Zürich")
View(df_abfall)

# select columns
df_abfall = select(df_abfall, -c("Bezirks-Nr.","Gemeinde", "Bezirkbezeichnung", "Jar_Datum", "Jahr", "BFS 20245", "Summe aller Papiere und Karton"))
View(df_abfall)

# check if the columns are numeric
sapply(df_abfall, is.numeric)

df_abfall <- replace(df_abfall,is.na(df_abfall),0)
View(df_abfall)

# scale the data
df_abfall_scaled <- scale(df_abfall)
class(df_abfall_scaled)  # is now a matrix
as_tibble(df_abfall_scaled)


k <- 3 #defining to work with two clusters
km.out <- kmeans(df_abfall_scaled,k,nstart = 50)
plot(df_abfall_scaled, col =(km.out$cluster+1), main ="K-Means Clustering", xlab ="", ylab ="", pch =20, cex =2)
km.out$cluster


# Check the outcome
km.out
km.out$withinss
cat("withinss =", km.out$tot.withinss)

# Find best value for k 
wss <- 0 # intialise
wss_dif <- 0
number_of_clusters_tested <- 20
for (i in 1:number_of_clusters_tested){
  km.out <- kmeans(df_abfall_scaled,i,nstart =50)
  wss[i] <- km.out$tot.withinss
  if(i > 1){ # only enter condition for two clusters and higher
    wss_dif[i-1] <- wss[i-1]-wss[i] # take difference from previous "total within-cluster sum of squares" and current one.
  }
  
}

plot(1:number_of_clusters_tested, wss, type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares")
plot(2:number_of_clusters_tested, wss_dif, type="b", xlab="Number of Clusters",ylab="Difference between Total within-cluster sum of squares")


########## Hierarchical Clustering ########## 
# Calculate distances between data (default method = euclidean):
distances <- dist(df_abfall_scaled, method = "euclidean")

# Compute hierarchical clustering based in distances calculated above:
hc <- hclust(distances)

# Computes dendrogram graphical representation:
dend <- as.dendrogram(hc)

# Graphical representation
plot(dend, main = "Dendogram plot")

# Transpose data:
df_transposed <- t(df_abfall_scaled)

# Calculate distances between data (default method = euclidean):
distances_t <- dist(df_transposed, method = "euclidean")

# Compute hierarchical clustering based in distances calculated above:
hc_t <- hclust(distances_t)

# Computes dendrogram graphical representation:
dend_t <- as.dendrogram(hc_t)

# Graphical representation
plot(dend_t, main = "Transposed Dendogram plot")



############ Principal Component Analysis (PCA) ############
# Compute PCA.
# scale=TRUE to scale the variables to have standard deviation = 1
pca_out = prcomp(df_abfall_scaled, scale=TRUE)

# Show available metrics computed by PCA: 
names(pca_out)

# Access metrics computed by PCA
pca_out$sdev    # show sdev
pca_out$center  # show mean
pca_out$scale   # show scale

# Rotation matrix provides the principal component of the loadings.
dim(pca_out$rotation) # p*p matrix whereas p is the number of variables (loadings)
round(pca_out$rotation, digit=2) # show loadings for each predictor. Round result on 2 digits.

# x matrix provides the principal component of the scores.
dim(pca_out$x) # n * p matrix whereas n is the number of observations and p is the number of principal componentes
# pca_out$x # not executed due to size of the matrix

# Create Biplot 
# scale=0 ensures that the arrows are scaled to represent the loadings; 
# other values for scale give slightly different biplots with different interpretations.
# cex (character expension factor): configures the labeling size of the observations (black) and the predictors (red)
# cex: reduze labeling size of the observations to 0.5 in order to make the plot more readable.
biplot(pca_out,scale=0, cex=c(0.3,0.8)) 
screeplot(pca_out)




########################################### Gemeinden_Steuern_Bev ########################################
df_bev_steu <- read_excel("Abfallstatistik Gemeinden 2000 bis 2019_Revidiert.xlsx")
df_bev_steu$Bev?lkerung_log <- log(df_bev_steu$Bev?lkerung)
View(df_bev_steu)

# count the missing values (NA) in the data frame
sum(is.na(df_bev_steu))

# remove z?rich (outlier)
df_bev_steu = subset(df_bev_steu, df_bev_steu$Gemeinde != "Zürich")

# select columns
df_bev_steu = select(df_bev_steu, -c("BFS", "Gemeinde", "Bevölkerung"))
View(df_bev_steu)

# check if the columns are numeric
sapply(df_bev_steu, is.numeric)

# scale the data
df_scaled_bev_steu <- scale(df_bev_steu)
class(df_scaled_bev_steu)  # is now a matrix
as_tibble(df_scaled_bev_steu)

plot(df_scaled_bev_steu)

k <- 3 #defining to work with two clusters
km.out <- kmeans(df_scaled_bev_steu,k,nstart = 50)
plot(df_scaled_bev_steu, col =(km.out$cluster+1), main ="K-Means Clustering", xlab ="", ylab ="", pch =20, cex =2)

km.out$cluster

# Check the outcome
km.out
km.out$withinss
cat("withinss =", km.out$tot.withinss)

# Find best value for k 
wss <- 0 # intialise
wss_dif <- 0
number_of_clusters_tested <- 20
for (i in 1:number_of_clusters_tested){
  km.out <- kmeans(df_scaled_bev_steu,i,nstart =50)
  wss[i] <- km.out$tot.withinss
  if(i > 1){ # only enter condition for two clusters and higher
    wss_dif[i-1] <- wss[i-1]-wss[i] # take difference from previous "total within-cluster sum of squares" and current one.
  }
  
}

plot(1:number_of_clusters_tested, wss, type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares")
plot(2:number_of_clusters_tested, wss_dif, type="b", xlab="Number of Clusters",ylab="Difference between Total within-cluster sum of squares")



################## Steuerfuss Abfall pro Kopf ###################################
df_bev_steu_abfall <- read_excel("Steuerfuss_Abfall pro Kopf_2019.xlsx")
df_bev_steu_abfall$Bevölkerung_log <- log(df_bev_steu_abfall$Bevölkerung)
View(df_bev_steu_abfall)

# count the missing values (NA) in the data frame
sum(is.na(df_bev_steu_abfall))

# remove z?rich (outlier)
df_bev_steu_abfall = subset(df_bev_steu_abfall, df_bev_steu_abfall$Gemeinde != "Zürich")

# select columns
df_bev_steu_abfall = select(df_bev_steu_abfall, c("Steuerfuss", "Total Abfall pro Kopf"))
View(df_bev_steu_abfall)

# check if the columns are numeric
sapply(df_bev_steu_abfall, is.numeric)

# scale the data
df_scaled_bev_steu_abfall <- scale(df_bev_steu_abfall)
class(df_scaled_bev_steu_abfall)  # is now a matrix
as_tibble(df_scaled_bev_steu_abfall)

plot(df_scaled_bev_steu_abfall)

k <- 3 #defining to work with two clusters
km.out <- kmeans(df_scaled_bev_steu_abfall,k,nstart = 50)
plot(df_scaled_bev_steu_abfall, col =(km.out$cluster+1), main ="K-Means Clustering", xlab ="", ylab ="", pch =20, cex =2)
plot(df_scaled_bev_steu_abfall, col =(km.out$cluster[3]), main ="K-Means Clustering", xlab ="", ylab ="", pch =20, cex =2)

km.out$cluster
km.out$centers

# Check the outcome
km.out
km.out$withinss
cat("withinss =", km.out$tot.withinss)

# Find best value for k 
wss <- 0 # intialise
wss_dif <- 0
number_of_clusters_tested <- 20
for (i in 1:number_of_clusters_tested){
  km.out <- kmeans(df_scaled_bev_steu_abfall,i,nstart =50)
  wss[i] <- km.out$tot.withinss
  if(i > 1){ # only enter condition for two clusters and higher
    wss_dif[i-1] <- wss[i-1]-wss[i] # take difference from previous "total within-cluster sum of squares" and current one.
  }
  
}

plot(1:number_of_clusters_tested, wss, type="b", xlab="Number of Clusters",ylab="Total within-cluster sum of squares")
plot(2:number_of_clusters_tested, wss_dif, type="b", xlab="Number of Clusters",ylab="Difference between Total within-cluster sum of squares")


#############################???


df_bev_steu_abfall <- read_excel("C:/Users/u223227/Downloads/Steuerfuss_Abfall pro Kopf_2019.xlsx")
df_bev_steu_abfall$Bev?lkerung_log <- log(df_bev_steu_abfall$Bev?lkerung)
View(df_bev_steu_abfall)

# count the missing values (NA) in the data frame
sum(is.na(df_bev_steu_abfall))

# remove z?rich (outlier) 
df_bev_steu_abfall = subset(df_bev_steu_abfall, df_bev_steu_abfall$Gemeinde != "Z?rich")

# plot steuerfuss vs total abfall pro kopf
plot(df_bev_steu_abfall$Steuerfuss, df_bev_steu_abfall$`Total Abfall pro Kopf`)

# select columns
df_bev_steu_abfall = select(df_bev_steu_abfall, -c("Summe aller Papier und Karton pro Kopf", "Bev?lkerung_log", "BFS", "Gemeinde", "Bev?lkerung", "Total Abfall", "Total Abfall pro Kopf", "Brennbare Abf?lle und Sperrgut", "Biogene Abf?lle", "Papier", "Karton", "Papier + Karton falls nicht separat angegeben", "Summe aller Papiere und Karton", "Glas", "Metalle", "Alt?l", "Grubengut"))

# creat correlationplot
library(corrplot)
library(RColorBrewer)
M <-cor(df_bev_steu_abfall)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

# calculate linear regression
fm = lm(scale(Steuerfuss) ~ ., data = df_bev_steu_abfall) 
summary(fm)

# find best model based on AIC
fm_step = step(fm)
summary(fm_step)

# check residuals
plot(fm_step)



fm_step$anova
