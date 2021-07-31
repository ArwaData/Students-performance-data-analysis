datasets<-read.csv(file="~/Documents/StudentsPerformance2.csv")
## k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
## prepreocessing 
View(datasets)



## run kmeans clustering to find 4 clusters
kmeans.result <- kmeans(datasets, 3)
kmeans.result
install.packages("factoextra")
library(factoextra) 
#install.packages('NbClust')
#library(NbClust)
##fres.nbclust <- NbClust(datasets, distance="euclidean", min.nc = 2, max.nc = 10, method="kmeans", index="all")



## clustering with PAM
install.packages('cluster')
library(cluster)
# group into 4 clusters
pam.result <- pam(datasets, 1)


## function to compute average silhouette for k clusters using silhouette()
silhouette_score <- function(k)
{  km <- kmeans(df, centers = k,nstart=25) 
ss <- silhouette(km$cluster, dist(df))  
mean(ss[, 3])}
##  k cluster range from 2 to 10
k <- 2:10
##  call  function fore k value
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)