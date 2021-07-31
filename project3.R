#..................Read Dataset..............
datasets<-read.csv(file="~\Users\DELL\Documents\Data mining\StudentsPerformance.csv")
## k-means clustering set a seed for random number generation  to make the results reproducible
set.seed(8953)
## prepreocessing 
View(datasets)

#removing race attribute
names(StudentsPerformance)[2] <- "race"
StudentsPerformance$race <- NULL

#checking missing value
sum(is.na(StudentsPerformance))

#outlier:
install.packages("outliers")
library(outliers)
names(StudentsPerformance)[5] <- "mathScore"
names(StudentsPerformance)[6] <- "readinghScore"
names(StudentsPerformance)[7] <- "writingScore"

##math outlier
OutGrade = outlier(StudentsPerformance $mathScore,logical =TRUE)
sum(OutGrade)
Find_outlier = which(OutGrade ==TRUE, arr.ind = TRUE)
StudentsPerformance= StudentsPerformance[-Find_outlier,]

##reading outlier
OutGradeR = outlier(StudentsPerformance $readinghScore,logical =TRUE)
sum(OutGradeR)
Find_outlier = which(OutGradeR ==TRUE, arr.ind = TRUE)
StudentsPerformance= StudentsPerformance[-Find_outlier,]

##writing outlier
OutGradeW = outlier(StudentsPerformance $writingScore,logical =TRUE)
sum(OutGradeW)
Find_outlier = which(OutGradeW ==TRUE, arr.ind = TRUE)
StudentsPerformance= StudentsPerformance[-Find_outlier,]

#convert attributes to numeric
ds$gender<-sapply(ds$gender,as.numeric)
ds$parental.level.of.education= factor(ds$parental.level.of.education , 
                                       levels = c("some college","associate\'s degree", "high school","some high 
school","master\'s degree","bachelor\'s degree"), 
                                       labels = c(1, 2, 3,4,5,6))
ds$lunch<-sapply(ds$lunch,as.numeric)
ds$test.preparation.course<-sapply(ds$test.preparation.course,as.numeric)



#................DATA MINING TASK: CLUSTERING ................

#................. First Cluster of Size = 2 .................
## run kmeans clustering to find 2 clusters
kmeans.result <- kmeans(datasets, 2)
kmeans.result

## visualize clustering
library(factoextra)
fviz_cluster(kmeans.result, data = datasets)
#............... Second Cluster is of Size = 3 .................

## run kmeans clustering to find 3 clusters
kmeans.result <- kmeans(datasets, 3)
kmeans.result

## visualize clustering
library(factoextra)
fviz_cluster(kmeans.result, data = datasets)

#................. Third Cluster is of size = 5 ...............
## run kmeans clustering to find 3 clusters
kmeans.result <- kmeans(datasets, 3)
kmeans.result

## visualize clustering
library(factoextra)
fviz_cluster(kmeans.result, data = datasets)

#..................... EVALUATION PART ...............
###Silhout for all cluster
##Nb_clust Validation 
install.packages("NbClust")
library(NbClust)
fviz_nbclust(datasets, kmeans, method = "silhouette")+labs(subtitle = "Silhouette method")

### Silhoute for each cluster: 2
## clustering with PAM
install.packages('cluster')
library(cluster)

# group into 2 clusters
pam.result <- pam(datasets, 2)
plot(pam.result)

## function to compute average silhouette for k clusters using silhouette()
install.packages("cluster")
library(cluster)
silhouette_score <- function(k)
{
  kmean<- kmeans(datasets, centers = k,nstart=25)
  ss <- silhouette(kmean$cluster, dist(datasets))
  mean(ss[, 3])}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', 
     frame=FALSE)
### Silhoute for each cluster: 3
## clustering with PAM
install.packages('cluster')
library(cluster)
# group into 3 clusters
pam.result <- pam(datasets, 3)
plot(pam.result)
## function to compute average silhouette for k clusters using silhouette()
install.packages("cluster")
library(cluster)
silhouette_score <- function(k)
{ 
  kmean<- kmeans(datasets, centers = k,nstart=25)
  ss <- silhouette(kmean$cluster, dist(datasets))
  mean(ss[, 3])}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', 
     frame=FALSE)
### Silhoute for each cluster: 5
## clustering with PAM
install.packages('cluster')
library(cluster)
# group into 5 clusters
pam.result <- pam(datasets, 5)
plot(pam.result)
## function to compute average silhouette for k clusters using silhouette()
install.packages("cluster")
library(cluster)
silhouette_score <- function(k)
{ 
  kmean<- kmeans(datasets, centers = k,nstart=25)
  ss <- silhouette(kmean$cluster, dist(datasets))
  mean(ss[, 3])}
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', 
     frame=FALSE)


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