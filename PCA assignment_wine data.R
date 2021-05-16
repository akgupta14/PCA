
wine_data <- read.csv(file.choose())

summary(wine_data)

library(Hmisc)

describe(wine_data)

colnames(wine_data)

attach(wine_data)

plot(Type,col="Blue")
plot(Alcohol,col="Blue")
plot(Malic,col="Blue")
plot(Ash,col="Blue")
plot(Alcalinity,col="Blue")
plot(Magnesium,col="Blue")
plot(Phenols,col="Blue")
plot(Flavanoids,col="Blue")
plot(Nonflavanoids,col="Blue")
plot(Proanthocyanins,col="Blue")
plot(Color,col="Blue")
plot(Hue,col="Blue")
plot(Dilution,col="Blue")
plot(Proline,col="Blue")

pairs(wine_data)

hist(Type)
hist(Alcohol)
hist(Malic)
hist(Ash)
hist(Alcalinity)
#Variable Ash,Alcanality,Phenols, Flavanoids,Proanthocyanins, Dilution is normally distributed
hist(Magnesium)
hist(Phenols)
hist(Flavanoids)
hist(Nonflavanoids)
hist(Proanthocyanins)
hist(Color)
hist(Hue)
hist(Dilution)
hist(Proline)

boxplot(Alcohol)
boxplot(Malic)

#Variables Malic,Ash,Alcanility,MagnesiumProanthocyanins,Color,Hue has outliers

boxplot(Ash)
boxplot(Alcalinity)
boxplot(Magnesium)
boxplot(Phenols)
boxplot(Flavanoids)
boxplot(Nonflavanoids)
boxplot(Proanthocyanins)
boxplot(Color)
boxplot(Hue)
boxplot(Dilution)
boxplot(Proline)

dim(wine_data)

# Scaling the data

wine_scaled_data <- scale(wine_data[,-1])
colnames(wine_scaled_data)

head(wine_scaled_data)


wine_pca <-princomp(wine_scaled_data,cor = T,covmat = NULL)
summary(wine_pca)

wine_pca$scores
wine_pca$loadings

plot(wine_pca,type="lines")

#Attaching the first 3 scores to the data
wine_newdata <- cbind(wine_data,wine_pca$scores[,1:3])

colnames(wine_newdata)

#Run Cluster analysis on 1st 3 PC's

mydata <- scale(wine_newdata[,-1])

colnames(mydata)
d<- dist(mydata[,14:16],method = "euclidean")

fit <- hclust(d,method="average")
#Building the algorithm try with centroid

plot(fit)

groups <- cutree(fit,k=4)
#cut tree into 4 clusters

#Draw a dendogram with red borders around 4 clusters

rect.hclust(fit, k=4, border = "red")

#K-means cluster

wss <- c()

for (i in 2:15) wss[i] <-sum(kmeans(mydata[,14:16],centers = i)$withinss)

plot(1:15,wss,type = "b",xlab = "No.of clusters",ylab="Avg distance")

#Using K-means clusters ,we can see 3 clusters

wine_cl <- kmeans(mydata[,14:16],3)

wine_cl$cluster
wine_cl$centers

library(animation)

windows()
wine_cl <- kmeans.ani(mydata[,14:16],3)

#K means on scaled data

wss <- c()

for (i in 2:15) wss[i] <-sum(kmeans(wine_scaled_data,centers = i)$withinss)
  
plot(1:15,wss,type = "b",xlab = "No.of clusters",ylab="Avg distance")

winecl <- kmeans(wine_scaled_data,3)

winecl$cluster
winecl$centers

library(cluster)

install.packages("tidyverse")
library(tidyverse)

install.packages("factoextra")
library(factoextra)

install.packages("gridExtra")
library(gridExtra)


head(wine_scaled_data)

cbind(wine_scaled_data,wine_data[,1])

Type <- wine_data[,1]

head(Type)
wine_scaled_data <- cbind(Type,wine_scaled_data)

#computing gap statistics

colnames(wine_scaled_data)

gap_stat <- clusGap(wine_scaled_data[,-1],FUN=kmeans,nstart=25,K.max = 10,B=50)

#Visualization

fviz_gap_stat(gap_stat)

#final Visualization

fviz_cluster(winecl,wine_scaled_data,ellipse.type = "norm")

fviz_pca_ind(winecl,geom.ind = "point",pointshape=21,pointsize=2,fill.ind = wine_scaled_data$Type,col.ind = "black",palette = "jco",addEllipses = T,label="Type",col,Type="black",repel = T,legend.title="Type")

#Optimum number of clusters are 3 using PCA and Cluster analysis in red blue and green respectively.