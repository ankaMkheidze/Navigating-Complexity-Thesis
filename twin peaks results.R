library(uwot)
library(cluster)
library(factoextra)
library(readxl)
library(NbClust)
library(stats)
library(fpc)
library(cluster)
library(factoextra)
library(clValid)
library(clusterSim)
library(Rtsne)
library(ggpubr)
library(fastcluster)
generate_twinpeaks_3d <- function(n, noise) {
  inc <- 1.5 / sqrt(n)
  xx2 <- yy2 <- seq(-1, 1, by = inc)
  xy <- 1 - 2 * matrix(runif(2 * n), ncol = 2)
  X <- matrix(0, nrow = n, ncol = 3)
  X[, 1:2] <- xy
  
  # Adjusting the density of lower and higher peaks
  X[, 3] <- sin(pi * xy[, 1]) * tanh(3 * xy[, 2])
  
  # Scaling the peaks to make them denser
  X[, 3] <- X[, 3] * c(100, 100)[(X[, 3] >= 0) + 1]
  X <-scale(X)
  X <- X + matrix(rnorm(3 * n), ncol = 3) * noise
  
  data <- data.frame(X = X)
  return(data)
}
set.seed(123)
twinpeaks_data <- generate_twinpeaks_3d(5000, 0.01)
plot(twin_peaks_data[,2], twinpeaks_data[,3], pch = 16, col = k_means_nodr_twinpeaks$cluster,
     xlab = "X.2", ylab = "X.3")

set.seed(123)
result_k_tp <- NbClust(data = twinpeaks_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
set.seed(123)
result_a_tp <- NbClust(data = twinpeaks_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.1)

result_k_tp$Best.nc
result_a_tp$Best.nc

results_evaluation_twinpeaks <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_twinpeaks) <- c("Method", "Silhouette", "Dunn", "CH", "DB")

set.seed(123)
k_means_nodr_twinpeaks = kmeans(twinpeaks_data,centers = 4)
clusts_nodr_twinpeaks <- k_means_nodr_twinpeaks$cluster
silhouette_k_means_twinpeaks_nodr <- silhouette(clusts_nodr_twinpeaks, dist(twinpeaks_data))
silhouette_k_means_twinpeaks_nodr <- as.matrix(silhouette_k_means_twinpeaks_nodr)
silhouette_k_means_twinpeaks_nodr <- silhouette_k_means_twinpeaks_nodr[,3]
silhouette_k_means_twinpeaks_nodr <- mean(silhouette_k_means_twinpeaks_nodr)

# Dunn index
dunn_k_means_twinpeaks_nodr <- dunn( distance = NULL, clusts_nodr_twinpeaks, Data = twinpeaks_data)

# Calinski-Harabasz index
calinski_k_means_twinpeaks_nodr <-   calinhara(twinpeaks_data,clusts_nodr_twinpeaks,cn=max(clusts_nodr_twinpeaks))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_twinpeaks_nodr <-index.DB(twinpeaks_data, clusts_nodr_twinpeaks, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[1,] <- c("K-means NoDR", silhouette_k_means_twinpeaks_nodr, 
                                   dunn_k_means_twinpeaks_nodr, calinski_k_means_twinpeaks_nodr,
                                   davies_k_means_twinpeaks_nodr)

fviz_cluster(k_means_nodr_twinpeaks,  data = twinpeaks_data,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR TwinPeaks"
)
#Method with No DR and AGNES
set.seed(123)
dist_twinpeaks_nodr <- dist(twinpeaks_data, method= "euclidean")
agnes_twinpeaks_nodr <- hclust(dist_twinpeaks_nodr, method = "ward.D2")
cut_agnes_twinpeaks_nodr <- cutree(agnes_twinpeaks_nodr, k = 4)


silhouette_agnes_twinpeaks_nodr <- silhouette(cut_agnes_twinpeaks_nodr, dist(twinpeaks_data))
silhouette_agnes_twinpeaks_nodr <- as.matrix(silhouette_agnes_twinpeaks_nodr)
silhouette_agnes_twinpeaks_nodr <- silhouette_agnes_twinpeaks_nodr[,3]
silhouette_agnes_twinpeaks_nodr <- mean(silhouette_agnes_twinpeaks_nodr)

dunn_agnes_twinpeaks_nodr <- dunn( dist(twinpeaks_data), cut_agnes_twinpeaks_nodr)
calinski_agnes_twinpeaks_nodr <-   calinhara(twinpeaks_data, cut_agnes_twinpeaks_nodr,cn=max(cut_agnes_twinpeaks_nodr))

davies_agnes_twinpeaks_nodr <-index.DB(twinpeaks_data, cut_agnes_twinpeaks_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[2,] <- c("AGNES NoDR", silhouette_agnes_twinpeaks_nodr, 
                                   dunn_agnes_twinpeaks_nodr, calinski_agnes_twinpeaks_nodr,
                                   davies_agnes_twinpeaks_nodr)


fviz_cluster(list(data = twinpeaks_data, cluster =cut_agnes_twinpeaks_nodr ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR TwinPeaks"
)



#pca twinpeaks
set.seed(123)

twinpeaks_pca = prcomp(twinpeaks_data)
twinpeaks_pca = twinpeaks_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_twinpeaks = kmeans(twinpeaks_pca,centers = 4)
clusters_pca_twinpeaks <- k_means_pca_twinpeaks$cluster
silhouette_k_means_twinpeaks_pca <- silhouette(clusters_pca_twinpeaks, dist(twinpeaks_pca))
silhouette_k_means_twinpeaks_pca <- as.matrix(silhouette_k_means_twinpeaks_pca)
silhouette_k_means_twinpeaks_pca <- silhouette_k_means_twinpeaks_pca[,3]
silhouette_k_means_twinpeaks_pca <- mean(silhouette_k_means_twinpeaks_pca)

# Dunn index
dunn_k_means_twinpeaks_pca <- dunn( distance = NULL, clusters_pca_twinpeaks, Data = twinpeaks_pca)

# Calinski-Harabasz index
calinski_k_means_twinpeaks_pca <-   calinhara(twinpeaks_pca,clusters_pca_twinpeaks,cn=max(clusters_pca_twinpeaks))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_twinpeaks_pca <-index.DB(twinpeaks_pca, clusters_pca_twinpeaks, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[3,] <- c("k-means PCA", silhouette_k_means_twinpeaks_pca, 
                                   dunn_k_means_twinpeaks_pca, calinski_k_means_twinpeaks_pca,
                                   davies_k_means_twinpeaks_pca)

fviz_cluster(k_means_pca_twinpeaks,  data = twinpeaks_pca,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA TwinPeaks"
)

#AGNES with PCA
set.seed(123)
dist_twinpeaks_pca <- dist(twinpeaks_pca, method= "euclidean")
agnes_twinpeaks_pca  <- hclust(dist_twinpeaks_pca, method = "ward.D2" )
cut_agnes_twinpeaks_pca <- cutree(agnes_twinpeaks_pca, k = 4)

silhouette_agnes_twinpeaks_pca <- silhouette(cut_agnes_twinpeaks_pca, dist(twinpeaks_pca))
silhouette_agnes_twinpeaks_pca <- as.matrix(silhouette_agnes_twinpeaks_pca)
silhouette_agnes_twinpeaks_pca <- silhouette_agnes_twinpeaks_pca[,3]
silhouette_agnes_twinpeaks_pca <- mean(silhouette_agnes_twinpeaks_pca)

dunn_agnes_twinpeaks_pca <- dunn( dist(twinpeaks_pca), cut_agnes_twinpeaks_pca)
calinski_agnes_twinpeaks_pca <-   calinhara(twinpeaks_pca, cut_agnes_twinpeaks_pca,cn=max(cut_agnes_twinpeaks_pca))

davies_agnes_twinpeaks_pca <-index.DB(twinpeaks_pca, cut_agnes_twinpeaks_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_twinpeaks[4,] <- c("AGNES PCA", silhouette_agnes_twinpeaks_pca, 
                                   dunn_agnes_twinpeaks_pca, calinski_agnes_twinpeaks_pca,
                                   davies_agnes_twinpeaks_pca)


fviz_cluster(list(data = twinpeaks_pca, cluster =cut_agnes_twinpeaks_pca ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA TwinPeaks"
)

#tsne
set.seed(123)
tsne_twinpeaks <- Rtsne(twinpeaks_data, dims = 2)
tsne_twinpeaks <- tsne_twinpeaks$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_twinpeaks = kmeans(tsne_twinpeaks,centers = 4)
clusters_tsne_twinpeaks <- k_means_tsne_twinpeaks$cluster

silhouette_k_means_twinpeaks_tsne <- silhouette(clusters_tsne_twinpeaks, dist(tsne_twinpeaks))
silhouette_k_means_twinpeaks_tsne <- as.matrix(silhouette_k_means_twinpeaks_tsne)
silhouette_k_means_twinpeaks_tsne <- silhouette_k_means_twinpeaks_tsne[,3]
silhouette_k_means_twinpeaks_tsne <- mean(silhouette_k_means_twinpeaks_tsne)

# Dunn index
dunn_k_means_twinpeaks_tsne <- dunn( distance = NULL, clusters_tsne_twinpeaks, Data = tsne_twinpeaks)

# Calinski-Harabasz index
calinski_k_means_twinpeaks_tsne <-   calinhara(tsne_twinpeaks,clusters_tsne_twinpeaks,cn=max(clusters_tsne_twinpeaks))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_twinpeaks_tsne <-index.DB(tsne_twinpeaks, clusters_tsne_twinpeaks, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[5,] <- c("k-means t-SNE", silhouette_k_means_twinpeaks_tsne, 
                                   dunn_k_means_twinpeaks_tsne, calinski_k_means_twinpeaks_tsne,
                                   davies_k_means_twinpeaks_tsne)

fviz_cluster(k_means_tsne_twinpeaks,  data = tsne_twinpeaks,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE TwinPeaks",
             xlab="V1", ylab = "V2"
)

#agnes t-sne
set.seed(123)
dist_twinpeaks_tsne <- dist(tsne_twinpeaks, method= "euclidean")
agnes_twinpeaks_tsne  <- hclust(dist_twinpeaks_tsne, method = "ward.D2" )
cut_agnes_twinpeaks_tsne <- cutree(agnes_twinpeaks_tsne, k = 4)


silhouette_agnes_twinpeaks_tsne <- silhouette(cut_agnes_twinpeaks_tsne, dist(tsne_twinpeaks))
silhouette_agnes_twinpeaks_tsne <- as.matrix(silhouette_agnes_twinpeaks_tsne)
silhouette_agnes_twinpeaks_tsne <- silhouette_agnes_twinpeaks_tsne[,3]
silhouette_agnes_twinpeaks_tsne <- mean(silhouette_agnes_twinpeaks_tsne)

dunn_agnes_twinpeaks_tsne <- dunn( dist(tsne_twinpeaks), cut_agnes_twinpeaks_tsne)
calinski_agnes_twinpeaks_tsne <-   calinhara(tsne_twinpeaks, cut_agnes_twinpeaks_tsne,cn=max(cut_agnes_twinpeaks_tsne))

davies_agnes_twinpeaks_tsne <-index.DB(tsne_twinpeaks, cut_agnes_twinpeaks_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[6,] <- c("AGNES t-SNE", silhouette_agnes_twinpeaks_tsne, 
                                   dunn_agnes_twinpeaks_tsne, calinski_agnes_twinpeaks_tsne,
                                   davies_agnes_twinpeaks_tsne)

fviz_cluster(list(data = tsne_twinpeaks, cluster =cut_agnes_twinpeaks_tsne ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE TwinPeaks",
             xlab="V1", ylab = "V2"
)


#umap
library(uwot)
set.seed(123)
umap_twinpeaks <- umap(twinpeaks_data, n_neighbors = 25, min_dist = 0.001, n_components = 2, 
                    init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_twinpeaks_umap <- kmeans(umap_twinpeaks, centers = 4)

fviz_cluster(kmeans_twinpeaks_umap,  data = umap_twinpeaks,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP TwinPeaks"
)

kmeans_cluster_twinpeaks_umap <- kmeans_twinpeaks_umap$cluster
silhouette_k_means_twinpeaks_umap <- silhouette(kmeans_cluster_twinpeaks_umap, dist(umap_twinpeaks))
silhouette_k_means_twinpeaks_umap <- as.matrix(silhouette_k_means_twinpeaks_umap)
silhouette_k_means_twinpeaks_umap <- silhouette_k_means_twinpeaks_umap[,3]
silhouette_k_means_twinpeaks_umap <- mean(silhouette_k_means_twinpeaks_umap)

# Dunn index
dunn_k_means_twinpeaks_umap <- dunn( distance = NULL, kmeans_cluster_twinpeaks_umap, Data = umap_twinpeaks)


# Calinski-Harabasz index
calinski_k_means_twinpeaks_umap <-   calinhara(umap_twinpeaks,kmeans_cluster_twinpeaks_umap,cn=max(kmeans_cluster_twinpeaks_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_twinpeaks_umap <-index.DB(umap_twinpeaks,kmeans_cluster_twinpeaks_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_twinpeaks[7,] <- c("k-means UMAP", silhouette_k_means_twinpeaks_umap, 
                                   dunn_k_means_twinpeaks_umap, calinski_k_means_twinpeaks_umap,
                                   davies_k_means_twinpeaks_umap)

#Agnes with UMAP 
set.seed(123)
dist_twinpeaks_umap <- dist(umap_twinpeaks, method= "euclidean")
agnes_cluster_umap_twinpeaks  <- hclust(dist_twinpeaks_umap, method = "ward.D2" )
cut_agnes_umap_twinpeaks <- cutree(agnes_cluster_umap_twinpeaks, k = 4)

fviz_cluster(list(data = umap_twinpeaks, cluster = cut_agnes_umap_twinpeaks),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP TwinPeaks"
)
#scatterplot3d(twinpeaks_data, color=rainbow(4)[cut_agnes_umap_twinpeaks], main = "agnes after UMAP twinpeaks")


silhouette_agnes_twinpeaks_umap <- silhouette(cut_agnes_umap_twinpeaks, dist(umap_twinpeaks))
silhouette_agnes_twinpeaks_umap <- as.matrix(silhouette_agnes_twinpeaks_umap)
silhouette_agnes_twinpeaks_umap <- silhouette_agnes_twinpeaks_umap[,3]
silhouette_agnes_twinpeaks_umap<- mean(silhouette_agnes_twinpeaks_umap)


dunn_agnes_twinpeaks_umap <- dunn( distance = NULL, cut_agnes_umap_twinpeaks, Data = umap_twinpeaks)


# Calinski-Harabasz index
calinski_agnes_twinpeaks_umap <-   calinhara(umap_twinpeaks,cut_agnes_umap_twinpeaks,cn=max(cut_agnes_umap_twinpeaks))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_twinpeaks_umap <-index.DB(umap_twinpeaks, cut_agnes_umap_twinpeaks, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_twinpeaks[8,] <- c("AGNES UMAP", silhouette_agnes_twinpeaks_umap, 
                                   dunn_agnes_twinpeaks_umap, calinski_agnes_twinpeaks_umap,
                                   davies_agnes_twinpeaks_umap)


dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[k_means_nodr_twinpeaks$cluster], type = "p", main = "kmeans no DR")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[cut_agnes_twinpeaks_nodr], type = "p", main = "AGNES no DR")
legend( s$xyz.convert(-2, -2, 2), legend= c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[k_means_pca_twinpeaks$cluster], type = "p", main = "kmeans PCA")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[cut_agnes_twinpeaks_pca], type = "p", main = "AGNES PCA")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[k_means_tsne_twinpeaks$cluster], type = "p", main = "kmeans t-SNE")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[cut_agnes_twinpeaks_tsne], type = "p", main = "AGNES t-SNE")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[kmeans_twinpeaks_umap$cluster], type = "p", main = "kmeans UMAP")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twinpeaks_data, pch=20, color = rainbow(4)[cut_agnes_umap_twinpeaks], type = "p", main = "AGNES UMAP")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)