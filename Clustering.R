library(VIM)
library(data.table)
library(clustertend)
library("NbClust") 
library(cluster)    # clustering algorithms
library(factoextra)
library(tidyverse)
library(eeptools)

mydata <- read.csv("movie_metadata.csv", header = TRUE)

# Duplicate rows
sum(duplicated(mydata))
# Delete duplicate rows
mydata <- mydata[!duplicated(mydata),]

# plot of missing values
colSums(sapply(mydata,is.na))
aggr(mydata)

# data exploration
plot(mydata$gross,ylab="gross",col = "dodgerblue3")
plot(mydata$budget,ylab="budget",col = "dodgerblue3")
plot(mydata$movie_facebook_likes,ylab="Facebook likes",col = "dodgerblue3")

summary(mydata)
#filtering out only the movies from the data (year 2005 onwards)

outlierReplace = function(dataframe, cols, rows, newValue = NA)
{
  if (any(rows)) 
{
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(mydata, "title_year", which(mydata$title_year <2005), NA)
outlierReplace(mydata, "duration", which(mydata$duration < 70), NA)
outlierReplace(mydata, "duration", which(mydata$duration > 200), NA)
outlierReplace(mydata, "gross", which(mydata$gross < 40000), NA)
outlierReplace(mydata, "budget", which(mydata$budget < 40000), NA)
mydata=filter(mydata, language=="English")
outlierReplace(mydata, "movie_facebook_likes", which(mydata$movie_facebook_likes <500), NA)

#selecting variables to use

mydata <- mydata[c(9,12,23,28)]
mydata=na.omit(mydata)


summary(mydata)

par(mfrow=c(1,3))
# Explore data
plot(mydata$gross,ylab="gross",col = "dodgerblue3")
plot(mydata$budget,ylab="budget",col = "dodgerblue3")
plot(mydata$movie_facebook_likes,ylab="Facebook likes",col = "dodgerblue3")




b1<- subset(mydata,budget >2.55e+08)
g1<- subset(mydata,gross >5.0e+08)
F1<- subset(mydata,movie_facebook_likes>2.0e+05)
special <- rbind(b1,g1,F1)

# removing movies with extreme values for special analysis
outlierReplace(mydata,"budget", which(mydata$budget> 2.55e+08),NA)
outlierReplace(mydata, "gross", which(mydata$gross > 5.0e+08),NA)
outlierReplace(mydata,"movie_facebook_likes", which(mydata$movie_facebook_likes> 2.0e+05),NA)

mydata=na.omit(mydata)
mydata


#outlier detection
#boxplot(mydata$budget,horizontal = T,col ="dodgerblue3", xlab='budget')
#boxplot(mydata$gross,horizontal = T,col ="dodgerblue3", xlab='gross')
#boxplot(mydata$movie_facebook_likes,horizontal = T,col ="dodgerblue3", xlab='movie facebook likes')
#outliner elimination
#summary(mydata$budget)
#bench <- 60000000 + 1.5*IQR(mydata$budget)
#bench
#bench2<- 13000000 - 1.5*IQR(mydata$budget)
#bench2
#outlierReplace(mydata, "budget", which(mydata$budget > 2.1e+08), NA)
#summary(mydata$gross)
#bench <- 67685175 + 1.5*IQR(mydata$gross)
#bench
#bench2<- 12134420 - 1.5*IQR(mydata$gross)
#bench2
#outlierReplace(mydata, "gross", which(mydata$gross > 4.4e+08), NA)
#mydata=na.omit(mydata)
#mydata
#boxplot(mydata$budget,horizontal = T,col ="dodgerblue3", xlab='budget')
#boxplot(mydata$gross,horizontal = T,col ="dodgerblue3", xlab='gross')

# scatterplot after manual removal of extreme values
plot(mydata$gross, ylab="gross",col = "dodgerblue3")
plot(mydata$budget, ylab="budget",col ="dodgerblue3")
plot(mydata$movie_facebook_likes, ylab="Movie facebook likes",col="dodgerblue3")



par(mfrow=c(1,1))



# subsetting data with only gross and budget to calculate wss
newdata <- mydata[c(1,3)]
newdata
##############################################################################################
#clustering tendency & number of clusters, elbow plot
wss <- (nrow(newdata)-1)*sum(apply(newdata,2,var))

for (i in 2:15) wss[i]<-sum(kmeans(newdata,centers=i)$withinss)

plot(1:15, wss, type="b", xlab= "Number of clusters" , ylab=" Within sum of squares")
##############################################################################################
# clustering the dataset based on gross and budget, with optimal k value found from wss

set.seed(20)
#clusters <- kmeans(mydata[c(1,3)],3, nstart=20)
clusters <- kmeans(mydata[c(1,3)],4, nstart=20)
#save cluster number in the dataset
mydata$cluster <- as.factor(clusters$cluster)
#switch cluster to first column
mydata<- mydata[,c(ncol(mydata),1:(ncol(mydata)-1))]


str(clusters)
clusters$center
clusters$size
#check within sum of squares value (higher the better)
clusters
#plot(newdata, col =(clusters$cluster +9) , main="k-means result with 3 clusters", pch=1, cex=1, las=1)
plot(newdata, col =(clusters$cluster +9) , main="k-means result with 4 clusters", pch=1, cex=1, las=1)
points(clusters$centers, col = "black", pch = 17, cex = 2)
aggregate(data=mydata,movie_facebook_likes~cluster,mean)

# clustering using normalization method
fviz_cluster(clusters,data=mydata[c(2,4)])

aggregate(mydata[c(2,4,5)],by=list(clusters$cluster),FUN = mean)


# extracting only movies from cluster 1 and 3
cluster1x <- subset(mydata, cluster == 1)
cluster3x<- subset(mydata, cluster ==3)

#arranging movies from highest to lowest value
cluster1 <- cluster1x[order(-cluster1x$movie_facebook_likes) , ]
cluster3 <- cluster3x[order(-cluster3x$movie_facebook_likes) , ]

# Selecting top 46 movies in each cluster with the highest facebook likes
Fcluster1<-cluster1[1:46, 2:5]
FCluster3<-cluster3[1:46, 2:5]


# combining the top 46 movies from the two clusters and 8 movies which were removed as outliers
total <- rbind(Fcluster1, FCluster3)
total2<- rbind(total, special) 
# save file as excel
write.csv(total2, file = "Selected 100 movies.csv")
mydata$movie_title<-as.character(mydata$movie_title)
mydata$movie_title = substr(mydata$movie_title,1,nchar(mydata$movie_title)-1)
mydata
