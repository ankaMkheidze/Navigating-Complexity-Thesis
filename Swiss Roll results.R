
library(clusterGeneration)
library(scatterplot3d)
library(kernlab)
library(plot3D)
library(Rtsne)
library(cluster)
library(IDmining)
library(NbClust)
library(NbClust)
library(stats)
library(fpc)
library(cluster)
library(factoextra)
library(clValid)
library(fastcluster)
library(plotrix)

swiss_roll_generation <- function(n, noise){
  n= 5000
  noise = 0.01
  t<-(3* pi /2) * (1 + 2 * runif(n))
  height <- 11 * runif(n)
  
  X <- matrix(0, nrow = n, ncol= 3)
  X[, 1] <- t * cos(t)
  X[, 2] <- height
  X[, 3] <- t * sin(t)
 
  condition_indices <- which(X[, 3] < -5 & X[, 3] > -6)
  X <- X[-condition_indices, ]  # Remove points satisfying the condition
  # Resample points from the removed indices
  numb = length(condition_indices)
  resampled <- X[sample(nrow(X),numb),]
  # Combine the remaining points and the resampled points
  X <- rbind(X, resampled)
  X <- X + matrix(rnorm(3 * n), ncol = 3) * noise
  
  data <- X

  return(data)
}

set.seed(6070)
swiss_data <- swiss_roll_generation(5000, 0.01)
plot(swiss_data[,1], swiss_data[,3], pch = 20)

set.seed(1809)
result_k_sr <- NbClust(data = swiss_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
result_k_sr$Best.nc
set.seed(1809)
result_a_sr <- NbClust(data = swiss_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.1)
result_a_sr$Best.nc




results_evaluation_sr <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_sr) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
k_means_nodr_sr = kmeans(swiss_data,centers = 3)
clusts_nodr_sr <- k_means_nodr_sr$cluster
silhouette_k_means_sr_nodr <- silhouette(clusts_nodr_sr, dist(swiss_data))
silhouette_k_means_sr_nodr <- as.matrix(silhouette_k_means_sr_nodr)
silhouette_k_means_sr_nodr <- silhouette_k_means_sr_nodr[,3]
silhouette_k_means_sr_nodr <- mean(silhouette_k_means_sr_nodr)

# Dunn index
dunn_k_means_sr_nodr <- dunn( distance = NULL, clusts_nodr_sr, Data = swiss_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_sr_nodr <-   calinhara(swiss_data,clusts_nodr_sr,cn=max(clusts_nodr_sr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_sr_nodr <-index.DB(swiss_data, clusts_nodr_sr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[1,] <- c("K-means NoDR", silhouette_k_means_sr_nodr, 
                                dunn_k_means_sr_nodr, calinski_k_means_sr_nodr,
                                davies_k_means_sr_nodr)


#Method with No DR and AGNES
set.seed(123)
dist_sr_nodr <- dist(swiss_data, method= "euclidean")
agnes_sr_nodr <- hclust(dist_sr_nodr, method = "ward.D")
cut_agnes_sr_nodr <- cutree(agnes_sr_nodr, k = 3)


silhouette_agnes_sr_nodr <- silhouette(cut_agnes_sr_nodr, dist(swiss_data))
silhouette_agnes_sr_nodr <- as.matrix(silhouette_agnes_sr_nodr)
silhouette_agnes_sr_nodr <- silhouette_agnes_sr_nodr[,3]
silhouette_agnes_sr_nodr <- mean(silhouette_agnes_sr_nodr)

dunn_agnes_sr_nodr <- dunn( distance =NULL, Data =swiss_data, cut_agnes_sr_nodr, method = "euclidean")
calinski_agnes_sr_nodr <-   calinhara(swiss_data, cut_agnes_sr_nodr,cn=max(cut_agnes_sr_nodr))

davies_agnes_sr_nodr <-index.DB(swiss_data, cut_agnes_sr_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[2,] <- c("AGNES NoDR", silhouette_agnes_sr_nodr, 
                                dunn_agnes_sr_nodr, calinski_agnes_sr_nodr,
                                davies_agnes_sr_nodr)


#pca sr
set.seed(123)
sr_pca = prcomp(swiss_data)
sr_pca = sr_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_sr = kmeans(sr_pca,centers = 3)
clusters_pca_sr <- k_means_pca_sr$cluster
silhouette_k_means_sr_pca <- silhouette(clusters_pca_sr, dist(sr_pca))
silhouette_k_means_sr_pca <- as.matrix(silhouette_k_means_sr_pca)
silhouette_k_means_sr_pca <- silhouette_k_means_sr_pca[,3]
silhouette_k_means_sr_pca <- mean(silhouette_k_means_sr_pca)

# Dunn index
dunn_k_means_sr_pca <- dunn( distance = NULL, clusters_pca_sr, Data = sr_pca, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_sr_pca <-   calinhara(sr_pca,clusters_pca_sr,cn=max(clusters_pca_sr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_sr_pca <-index.DB(sr_pca, clusters_pca_sr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[3,] <- c("k-means PCA", silhouette_k_means_sr_pca, 
                                dunn_k_means_sr_pca, calinski_k_means_sr_pca,
                                davies_k_means_sr_pca)

#AGNES with PCA
set.seed(123)
dist_sr_pca <- dist(sr_pca, method= "euclidean")
agnes_sr_pca  <- hclust(dist_sr_pca, method = "ward.D" )
cut_agnes_sr_pca <- cutree(agnes_sr_pca, k = 3)

silhouette_agnes_sr_pca <- silhouette(cut_agnes_sr_pca, dist(sr_pca))
silhouette_agnes_sr_pca <- as.matrix(silhouette_agnes_sr_pca)
silhouette_agnes_sr_pca <- silhouette_agnes_sr_pca[,3]
silhouette_agnes_sr_pca <- mean(silhouette_agnes_sr_pca)

dunn_agnes_sr_pca <- dunn( distance = NULL, Data =sr_pca, cut_agnes_sr_pca, method = "euclidean")
calinski_agnes_sr_pca <-   calinhara(sr_pca, cut_agnes_sr_pca,cn=max(cut_agnes_sr_pca))

davies_agnes_sr_pca <-index.DB(sr_pca, cut_agnes_sr_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_sr[4,] <- c("AGNES PCA", silhouette_agnes_sr_pca, 
                                dunn_agnes_sr_pca, calinski_agnes_sr_pca,
                                davies_agnes_sr_pca)



#tsne
set.seed(123)
tsne_sr <- Rtsne(swiss_data, dims = 2)
tsne_sr <- tsne_sr$Y
#k-means with t-sne
set.seed(123)

k_means_tsne_sr = kmeans(tsne_sr,centers = 3)
clusters_tsne_sr <- k_means_tsne_sr$cluster

silhouette_k_means_sr_tsne <- silhouette(clusters_tsne_sr, dist(tsne_sr))
silhouette_k_means_sr_tsne <- as.matrix(silhouette_k_means_sr_tsne)
silhouette_k_means_sr_tsne <- silhouette_k_means_sr_tsne[,3]
silhouette_k_means_sr_tsne <- mean(silhouette_k_means_sr_tsne)

# Dunn index
dunn_k_means_sr_tsne <- dunn( distance = NULL, clusters_tsne_sr, Data = tsne_sr, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_sr_tsne <-   calinhara(tsne_sr,clusters_tsne_sr,cn=max(clusters_tsne_sr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_sr_tsne <-index.DB(tsne_sr, clusters_tsne_sr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[5,] <- c("k-means t-SNE", silhouette_k_means_sr_tsne, 
                                dunn_k_means_sr_tsne, calinski_k_means_sr_tsne,
                                davies_k_means_sr_tsne)



#agnes t-sne
set.seed(123)
dist_sr_tsne <- dist(tsne_sr, method= "euclidean")
agnes_sr_tsne  <- hclust(dist_sr_tsne, method = "ward.D" )
cut_agnes_sr_tsne <- cutree(agnes_sr_tsne, k = 3)


silhouette_agnes_sr_tsne <- silhouette(cut_agnes_sr_tsne, dist(tsne_sr))
silhouette_agnes_sr_tsne <- as.matrix(silhouette_agnes_sr_tsne)
silhouette_agnes_sr_tsne <- silhouette_agnes_sr_tsne[,3]
silhouette_agnes_sr_tsne <- mean(silhouette_agnes_sr_tsne)

dunn_agnes_sr_tsne <- dunn( distance = NULL, Data =tsne_sr, cut_agnes_sr_tsne, method = "euclidean")
calinski_agnes_sr_tsne <-   calinhara(tsne_sr, cut_agnes_sr_tsne,cn=max(cut_agnes_sr_tsne))

davies_agnes_sr_tsne <-index.DB(tsne_sr, cut_agnes_sr_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[6,] <- c("AGNES t-SNE", silhouette_agnes_sr_tsne, 
                                dunn_agnes_sr_tsne, calinski_agnes_sr_tsne,
                                davies_agnes_sr_tsne)



#umap
library(uwot)
set.seed(123)
umap_sr <- umap(swiss_data, n_neighbors = 50, min_dist = 0.001, n_components = 2, 
                 init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_sr_umap <- kmeans(umap_sr, centers = 3)


kmeans_cluster_sr_umap <- kmeans_sr_umap$cluster
silhouette_k_means_sr_umap <- silhouette(kmeans_cluster_sr_umap, dist(umap_sr))
silhouette_k_means_sr_umap <- as.matrix(silhouette_k_means_sr_umap)
silhouette_k_means_sr_umap <- silhouette_k_means_sr_umap[,3]
silhouette_k_means_sr_umap <- mean(silhouette_k_means_sr_umap)

# Dunn index
dunn_k_means_sr_umap <- dunn( distance = NULL, kmeans_cluster_sr_umap, Data = umap_sr, method = "euclidean")


# Calinski-Harabasz index
calinski_k_means_sr_umap <-   calinhara(umap_sr,kmeans_cluster_sr_umap,cn=max(kmeans_cluster_sr_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_sr_umap <-index.DB(umap_sr,kmeans_cluster_sr_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_sr[7,] <- c("k-means UMAP", silhouette_k_means_sr_umap, 
                                dunn_k_means_sr_umap, calinski_k_means_sr_umap,
                                davies_k_means_sr_umap)

#Agnes with UMAP 
set.seed(123)
dist_sr_umap <- dist(umap_sr, method= "euclidean")
agnes_cluster_umap_sr  <- hclust(dist_sr_umap, method = "ward.D" )
cut_agnes_umap_sr <- cutree(agnes_cluster_umap_sr, k = 3)



silhouette_agnes_sr_umap <- silhouette(cut_agnes_umap_sr, dist(umap_sr))
silhouette_agnes_sr_umap <- as.matrix(silhouette_agnes_sr_umap)
silhouette_agnes_sr_umap <- silhouette_agnes_sr_umap[,3]
silhouette_agnes_sr_umap<- mean(silhouette_agnes_sr_umap)


dunn_agnes_sr_umap <- dunn( distance = NULL, cut_agnes_umap_sr, Data = umap_sr, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_sr_umap <-   calinhara(umap_sr,cut_agnes_umap_sr,cn=max(cut_agnes_umap_sr))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_sr_umap <-index.DB(umap_sr, cut_agnes_umap_sr, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_sr[8,] <- c("AGNES UMAP", silhouette_agnes_sr_umap, 
                                dunn_agnes_sr_umap, calinski_agnes_sr_umap,
                                davies_agnes_sr_umap)
#2-D visualizations
fviz_cluster(k_means_nodr_sr,  data = sr_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR  Swiss Roll"
)

fviz_cluster(list(data = sr_pca, cluster =cut_agnes_sr_nodr ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR  Swiss Roll"
)


fviz_cluster(k_means_pca_sr,  data = sr_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA  Swiss Roll"
)


fviz_cluster(list(data = sr_pca, cluster =cut_agnes_sr_pca ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA  Swiss Roll"
)


fviz_cluster(k_means_tsne_sr,  data = tsne_sr,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE  Swiss Roll",
             xlab="V1", ylab = "V2"
)

fviz_cluster(list(data = tsne_sr, cluster =cut_agnes_sr_tsne ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE  Swiss Roll",
             xlab="V1", ylab = "V2"
)


fviz_cluster(kmeans_sr_umap,  data = umap_sr,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP  Swiss Roll"
)


fviz_cluster(list(data = umap_sr, cluster = cut_agnes_umap_sr),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP  Swiss Roll"
)




#3-D visualizations

dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[k_means_nodr_sr$cluster], type = "p", main = "kmeans no DR")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
                                                                  ) ,pch = 16)

dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[cut_agnes_sr_nodr], type = "p", main = "AGNES no DR")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)
dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[k_means_pca_sr$cluster], type = "p", main = "kmeans PCA")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)
dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[cut_agnes_sr_pca], type = "p", main = "AGNES PCA")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)
dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[k_means_tsne_sr$cluster], type = "p", main = "kmeans t-SNE")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)

dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[cut_agnes_sr_tsne], type = "p", main = "AGNES t-SNE")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)
dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(6)[kmeans_sr_umap$cluster], type = "p",main = "")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#FFFF00"
) ,pch = 16)

dev.new()
s <- scatterplot3d(swiss_data, pch=20, color = rainbow(3)[cut_agnes_umap_sr], type = "p", main = "AGNES UMAP")
legend( s$xyz.convert(-10, -2, 15), legend =c(1,2,3), col = c("#FF0000", "#00FF00", "#000FFF"
) ,pch = 16)