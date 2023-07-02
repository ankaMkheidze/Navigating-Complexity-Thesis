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
twin_peaks_data <- generate_twinpeaks_3d(5000, 0.01)


set.seed(123)
result_k_tp <- NbClust(data = twin_peaks_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
set.seed(123)
result_a_tp <- NbClust(data = twin_peaks_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.1)

result_k_tp$Best.nc
result_a_tp$Best.nc

clust_num_tp <- 4

results_evaluation_tp <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_tp) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
k_means_nodr_tp = kmeans(twin_peaks_data,centers  = clust_num_tp)
clusts_nodr_tp <- k_means_nodr_tp$cluster
silhouette_k_means_tp_nodr <- silhouette(clusts_nodr_tp, dist(twin_peaks_data))
silhouette_k_means_tp_nodr <- as.matrix(silhouette_k_means_tp_nodr)
silhouette_k_means_tp_nodr <- silhouette_k_means_tp_nodr[,3]
silhouette_k_means_tp_nodr <- mean(silhouette_k_means_tp_nodr)

# Dunn index
dunn_k_means_tp_nodr <- dunn( distance = NULL, clusts_nodr_tp, Data = twin_peaks_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_tp_nodr <-   calinhara(twin_peaks_data,clusts_nodr_tp,cn=max(clusts_nodr_tp))


# Davies-Bouldin index

davies_k_means_tp_nodr <-index.DB(twin_peaks_data, clusts_nodr_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[1,] <- c("K-means NoDR", silhouette_k_means_tp_nodr, 
                               dunn_k_means_tp_nodr, calinski_k_means_tp_nodr,
                               davies_k_means_tp_nodr)

#Method with No DR and AGNES
set.seed(123)
dist_tp_nodr <- dist(twin_peaks_data, method= "euclidean")
agnes_tp_nodr <- hclust(dist_tp_nodr, method = "ward.D2" )
cut_agnes_tp_nodr <- cutree(agnes_tp_nodr, k  = clust_num_tp)


silhouette_agnes_tp_nodr <- silhouette(cut_agnes_tp_nodr, dist(twin_peaks_data))
silhouette_agnes_tp_nodr <- as.matrix(silhouette_agnes_tp_nodr)
silhouette_agnes_tp_nodr <- silhouette_agnes_tp_nodr[,3]
silhouette_agnes_tp_nodr <- mean(silhouette_agnes_tp_nodr)

dunn_agnes_tp_nodr <- dunn( distance = NULL, Data = twin_peaks_data, cut_agnes_tp_nodr, method ="euclidean")
calinski_agnes_tp_nodr <-   calinhara(twin_peaks_data, cut_agnes_tp_nodr,cn=max(cut_agnes_tp_nodr))

davies_agnes_tp_nodr <-index.DB(twin_peaks_data, cut_agnes_tp_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[2,] <- c("AGNES NoDR", silhouette_agnes_tp_nodr, 
                               dunn_agnes_tp_nodr, calinski_agnes_tp_nodr,
                               davies_agnes_tp_nodr)


#pca tp
set.seed(123)
tp_pca = prcomp(twin_peaks_data)
tp_pca = tp_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_tp = kmeans(tp_pca,centers  = clust_num_tp)
clusters_pca_tp <- k_means_pca_tp$cluster

silhouette_k_means_tp_pca <- silhouette(clusters_pca_tp, dist(tp_pca))
silhouette_k_means_tp_pca <- as.matrix(silhouette_k_means_tp_pca)
silhouette_k_means_tp_pca <- silhouette_k_means_tp_pca[,3]
silhouette_k_means_tp_pca <- mean(silhouette_k_means_tp_pca)

# Dunn index
dunn_k_means_tp_pca <- dunn( distance = NULL, clusters_pca_tp, Data = tp_pca, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_tp_pca <-   calinhara(tp_pca,clusters_pca_tp,cn=max(clusters_pca_tp))


# Davies-Bouldin index
davies_k_means_tp_pca <-index.DB(tp_pca, clusters_pca_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[3,] <- c("k-means PCA", silhouette_k_means_tp_pca, 
                               dunn_k_means_tp_pca, calinski_k_means_tp_pca,
                               davies_k_means_tp_pca)

#AGNES with PCA
set.seed(123)
dist_tp_pca <- dist(tp_pca, method= "euclidean")
agnes_tp_pca  <- hclust(dist_tp_pca, method = "ward.D2" )
cut_agnes_tp_pca <- cutree(agnes_tp_pca, k  = clust_num_tp)

silhouette_agnes_tp_pca <- silhouette(cut_agnes_tp_pca, dist(tp_pca))
silhouette_agnes_tp_pca <- as.matrix(silhouette_agnes_tp_pca)
silhouette_agnes_tp_pca <- silhouette_agnes_tp_pca[,3]
silhouette_agnes_tp_pca <- mean(silhouette_agnes_tp_pca)

dunn_agnes_tp_pca <- dunn( distance = NULL, Data =tp_pca, cut_agnes_tp_pca, method = "euclidean")
calinski_agnes_tp_pca <-   calinhara(tp_pca, cut_agnes_tp_pca,cn=max(cut_agnes_tp_pca))

davies_agnes_tp_pca <-index.DB(tp_pca, cut_agnes_tp_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_tp[4,] <- c("AGNES PCA", silhouette_agnes_tp_pca, 
                               dunn_agnes_tp_pca, calinski_agnes_tp_pca,
                               davies_agnes_tp_pca)



#tsne
set.seed(123)
tsne_tp <- Rtsne(twin_peaks_data, dims = 2)
tsne_tp <- tsne_tp$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_tp = kmeans(tsne_tp,centers  = clust_num_tp)
clusters_tsne_tp <- k_means_tsne_tp$cluster

silhouette_k_means_tp_tsne <- silhouette(clusters_tsne_tp, dist(tsne_tp))
silhouette_k_means_tp_tsne <- as.matrix(silhouette_k_means_tp_tsne)
silhouette_k_means_tp_tsne <- silhouette_k_means_tp_tsne[,3]
silhouette_k_means_tp_tsne <- mean(silhouette_k_means_tp_tsne)

# Dunn index
dunn_k_means_tp_tsne <- dunn( distance = NULL, clusters_tsne_tp, Data = tsne_tp, method= "euclidean")

# Calinski-Harabasz index
calinski_k_means_tp_tsne <-   calinhara(tsne_tp,clusters_tsne_tp,cn=max(clusters_tsne_tp))


# Davies-Bouldin index

davies_k_means_tp_tsne <-index.DB(tsne_tp, clusters_tsne_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[5,] <- c("k-means t-SNE", silhouette_k_means_tp_tsne, 
                               dunn_k_means_tp_tsne, calinski_k_means_tp_tsne,
                               davies_k_means_tp_tsne)


#agnes t-sne
set.seed(123)
dist_tp_tsne <- dist(tsne_tp, method= "euclidean")
agnes_tp_tsne  <- hclust(dist_tp_tsne, method = "ward.D2" )
cut_agnes_tp_tsne <- cutree(agnes_tp_tsne, k  = clust_num_tp)


silhouette_agnes_tp_tsne <- silhouette(cut_agnes_tp_tsne, dist(tsne_tp))
silhouette_agnes_tp_tsne <- as.matrix(silhouette_agnes_tp_tsne)
silhouette_agnes_tp_tsne <- silhouette_agnes_tp_tsne[,3]
silhouette_agnes_tp_tsne <- mean(silhouette_agnes_tp_tsne)

dunn_agnes_tp_tsne <- dunn( distance = NULL, Data = tsne_tp, cut_agnes_tp_tsne, method = "euclidean")
calinski_agnes_tp_tsne <-   calinhara(tsne_tp, cut_agnes_tp_tsne,cn=max(cut_agnes_tp_tsne))

davies_agnes_tp_tsne <-index.DB(tsne_tp, cut_agnes_tp_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[6,] <- c("AGNES t-SNE", silhouette_agnes_tp_tsne, 
                               dunn_agnes_tp_tsne, calinski_agnes_tp_tsne,
                               davies_agnes_tp_tsne)


#umap
library(uwot)
set.seed(123)
umap_tp <- umap(twin_peaks_data, n_neighbors = 25, min_dist = 0.001, n_components = 2, 
                init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_tp_umap <- kmeans(umap_tp, centers  = clust_num_tp)

kmeans_cluster_tp_umap <- kmeans_tp_umap$cluster
silhouette_k_means_tp_umap <- silhouette(kmeans_cluster_tp_umap, dist(umap_tp))
silhouette_k_means_tp_umap <- as.matrix(silhouette_k_means_tp_umap)
silhouette_k_means_tp_umap <- silhouette_k_means_tp_umap[,3]
silhouette_k_means_tp_umap <- mean(silhouette_k_means_tp_umap)

# Dunn index
dunn_k_means_tp_umap <- dunn( distance = NULL, kmeans_cluster_tp_umap, Data = umap_tp, method ="euclidean")


# Calinski-Harabasz index
calinski_k_means_tp_umap <-   calinhara(umap_tp,kmeans_cluster_tp_umap,cn=max(kmeans_cluster_tp_umap))


# Davies-Bouldin index

davies_k_means_tp_umap <-index.DB(umap_tp,kmeans_cluster_tp_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp[7,] <- c("k-means UMAP", silhouette_k_means_tp_umap, 
                               dunn_k_means_tp_umap, calinski_k_means_tp_umap,
                               davies_k_means_tp_umap)

#Agnes with UMAP 
set.seed(123)
dist_tp_umap <- dist(umap_tp, method= "euclidean")
agnes_cluster_umap_tp  <- hclust(dist_tp_umap, method = "ward.D2" )
cut_agnes_umap_tp <- cutree(agnes_cluster_umap_tp, k  = clust_num_tp)


silhouette_agnes_tp_umap <- silhouette(cut_agnes_umap_tp, dist(umap_tp))
silhouette_agnes_tp_umap <- as.matrix(silhouette_agnes_tp_umap)
silhouette_agnes_tp_umap <- silhouette_agnes_tp_umap[,3]
silhouette_agnes_tp_umap<- mean(silhouette_agnes_tp_umap)


dunn_agnes_tp_umap <- dunn( distance = NULL, cut_agnes_umap_tp, Data = umap_tp, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_tp_umap <-   calinhara(umap_tp,cut_agnes_umap_tp,cn=max(cut_agnes_umap_tp))


# Davies-Bouldin index

davies_agnes_tp_umap <-index.DB(umap_tp, cut_agnes_umap_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_tp[8,] <- c("AGNES UMAP", silhouette_agnes_tp_umap, 
                               dunn_agnes_tp_umap, calinski_agnes_tp_umap,
                               davies_agnes_tp_umap)


#Results in 3-Dimensions
results_evaluation_tp_3d <-matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_tp_3d) <- c("Method", "Silhouette", "Dunn", "CH", "DB")

results_evaluation_tp_3d[1,] <- c("No DR K", silhouette_k_means_tp_nodr, 
                                  dunn_k_means_tp_nodr, calinski_k_means_tp_nodr,
                                  davies_k_means_tp_nodr)

results_evaluation_tp_3d[2,] <- c("No DR A", silhouette_agnes_tp_nodr, 
                                  dunn_agnes_tp_nodr, calinski_agnes_tp_nodr,
                                  davies_agnes_tp_nodr)

silhouette_k_means_tp_pca_3d <- silhouette(clusters_pca_tp, dist(twin_peaks_data))
silhouette_k_means_tp_pca_3d <- as.matrix(silhouette_k_means_tp_pca_3d)
silhouette_k_means_tp_pca_3d <- silhouette_k_means_tp_pca_3d[,3]
silhouette_k_means_tp_pca_3d <- mean(silhouette_k_means_tp_pca_3d)

# Dunn index
dunn_k_means_tp_pca_3d <- dunn( distance = NULL, clusters_pca_tp, Data = twin_peaks_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_tp_pca_3d <-   calinhara(twin_peaks_data,clusters_pca_tp,cn=max(clusters_pca_tp))


# Davies-Bouldin index
davies_k_means_tp_pca_3d <-index.DB(twin_peaks_data, clusters_pca_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp_3d[3,] <- c("k-means PCA", silhouette_k_means_tp_pca_3d, 
                                  dunn_k_means_tp_pca_3d, calinski_k_means_tp_pca_3d,
                                  davies_k_means_tp_pca_3d)


silhouette_agnes_tp_pca_3d <- silhouette(cut_agnes_tp_pca, dist(twin_peaks_data))
silhouette_agnes_tp_pca_3d <- as.matrix(silhouette_agnes_tp_pca_3d)
silhouette_agnes_tp_pca_3d <- silhouette_agnes_tp_pca_3d[,3]
silhouette_agnes_tp_pca_3d <- mean(silhouette_agnes_tp_pca_3d)

dunn_agnes_tp_pca_3d <- dunn( distance = NULL, Data = twin_peaks_data, cut_agnes_tp_pca, method = "euclidean")
calinski_agnes_tp_pca_3d <-   calinhara(twin_peaks_data, cut_agnes_tp_pca,cn=max(cut_agnes_tp_pca))

davies_agnes_tp_pca_3d <-index.DB(twin_peaks_data, cut_agnes_tp_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_tp_3d[4,] <- c("PCA A", silhouette_agnes_tp_pca_3d, 
                                  dunn_agnes_tp_pca_3d, calinski_agnes_tp_pca_3d,
                                  davies_agnes_tp_pca_3d)


silhouette_k_means_tp_tsne_3d <- silhouette(clusters_tsne_tp, dist(twin_peaks_data))
silhouette_k_means_tp_tsne_3d <- as.matrix(silhouette_k_means_tp_tsne_3d)
silhouette_k_means_tp_tsne_3d <- silhouette_k_means_tp_tsne_3d[,3]
silhouette_k_means_tp_tsne_3d <- mean(silhouette_k_means_tp_tsne_3d)

# Dunn index
dunn_k_means_tp_tsne_3d <- dunn( distance = NULL, clusters_tsne_tp, Data = twin_peaks_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_tp_tsne_3d <-   calinhara(twin_peaks_data,clusters_tsne_tp,cn=max(clusters_tsne_tp))


# Davies-Bouldin index

davies_k_means_tp_tsne_3d <-index.DB(twin_peaks_data, clusters_tsne_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp_3d[5,] <- c("t-SNE K", silhouette_k_means_tp_tsne_3d, 
                                  dunn_k_means_tp_tsne_3d, calinski_k_means_tp_tsne_3d,
                                  davies_k_means_tp_tsne_3d)


silhouette_agnes_tp_tsne_3d <- silhouette(cut_agnes_tp_tsne, dist(twin_peaks_data))
silhouette_agnes_tp_tsne_3d <- as.matrix(silhouette_agnes_tp_tsne_3d)
silhouette_agnes_tp_tsne_3d <- silhouette_agnes_tp_tsne_3d[,3]
silhouette_agnes_tp_tsne_3d <- mean(silhouette_agnes_tp_tsne_3d)

dunn_agnes_tp_tsne_3d <- dunn( distance = NULL, cut_agnes_tp_tsne, Data = twin_peaks_data, method = "euclidean")
calinski_agnes_tp_tsne_3d <-   calinhara(twin_peaks_data, cut_agnes_tp_tsne,cn=max(cut_agnes_tp_tsne))

davies_agnes_tp_tsne_3d <-index.DB(twin_peaks_data, cut_agnes_tp_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp_3d[6,] <- c("t-SNE A", silhouette_agnes_tp_tsne_3d, 
                                  dunn_agnes_tp_tsne_3d, calinski_agnes_tp_tsne_3d,
                                  davies_agnes_tp_tsne_3d)


silhouette_k_means_tp_umap_3d <- silhouette(kmeans_cluster_tp_umap, dist(twin_peaks_data))
silhouette_k_means_tp_umap_3d <- as.matrix(silhouette_k_means_tp_umap_3d)
silhouette_k_means_tp_umap_3d <- silhouette_k_means_tp_umap_3d[,3]
silhouette_k_means_tp_umap_3d <- mean(silhouette_k_means_tp_umap_3d)

# Dunn index
dunn_k_means_tp_umap_3d <- dunn( distance = NULL, kmeans_cluster_tp_umap, Data = twin_peaks_data, method = "euclidean")


# Calinski-Harabasz index
calinski_k_means_tp_umap_3d <-   calinhara(twin_peaks_data,kmeans_cluster_tp_umap,cn=max(kmeans_cluster_tp_umap))


# Davies-Bouldin index

davies_k_means_tp_umap_3d <-index.DB(twin_peaks_data,kmeans_cluster_tp_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_tp_3d[7,] <- c("k-means UMAP", silhouette_k_means_tp_umap_3d, 
                                  dunn_k_means_tp_umap_3d, calinski_k_means_tp_umap_3d,
                                  davies_k_means_tp_umap_3d)


silhouette_agnes_tp_umap_3d <- silhouette(cut_agnes_umap_tp, dist(twin_peaks_data))
silhouette_agnes_tp_umap_3d <- as.matrix(silhouette_agnes_tp_umap_3d)
silhouette_agnes_tp_umap_3d <- silhouette_agnes_tp_umap_3d[,3]
silhouette_agnes_tp_umap_3d<- mean(silhouette_agnes_tp_umap_3d)


dunn_agnes_tp_umap_3d <- dunn( distance = NULL, cut_agnes_umap_tp, Data = twin_peaks_data, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_tp_umap_3d <-   calinhara(twin_peaks_data,cut_agnes_umap_tp,cn=max(cut_agnes_umap_tp))


# Davies-Bouldin index

davies_agnes_tp_umap_3d <-index.DB(twin_peaks_data, cut_agnes_umap_tp, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_tp_3d[8,] <- c("AGNES UMAP", silhouette_agnes_tp_umap_3d, 
                                  dunn_agnes_tp_umap_3d, calinski_agnes_tp_umap_3d,
                                  davies_agnes_tp_umap_3d)

#Visualizations 2-D
fviz_cluster(k_means_nodr_tp,  data = twin_peaks_data,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR TwinPeaks"
)

fviz_cluster(list(data = twin_peaks_data, cluster =cut_agnes_tp_nodr ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with no DR TwinPeaks"
)

fviz_cluster(k_means_pca_tp,  data = tp_pca,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA TwinPeaks"
)

fviz_cluster(list(data = tp_pca, cluster =cut_agnes_tp_pca ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with PCA TwinPeaks"
)

fviz_cluster(k_means_tsne_tp,  data = tsne_tp,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE TwinPeaks",
             xlab="V1", ylab = "V2"
)

fviz_cluster(list(data = tsne_tp, cluster =cut_agnes_tp_tsne ),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with t-SNE TwinPeaks",
             xlab="V1", ylab = "V2"
)

fviz_cluster(kmeans_tp_umap,  data = umap_tp,
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP TwinPeaks"
)

fviz_cluster(list(data = umap_tp, cluster = cut_agnes_umap_tp),
             palette = c("#FF0000", "#FFFF00","#000FFF", "#00FF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP TwinPeaks"
)

#Visualizations 3-D
dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[k_means_nodr_tp$cluster], type = "p", main = "kmeans no DR")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[cut_agnes_tp_nodr], type = "p", main = "AGNES no DR")
legend( s$xyz.convert(-2, -2, 2), legend= c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[k_means_pca_tp$cluster], type = "p", main = "kmeans PCA")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                            "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[cut_agnes_tp_pca], type = "p", main = "AGNES PCA")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[k_means_tsne_tp$cluster], type = "p", main = "kmeans t-SNE")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[cut_agnes_tp_tsne], type = "p", main = "AGNES t-SNE")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[kmeans_tp_umap$cluster], type = "p", main = "kmeans UMAP")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)
dev.new()
s <- scatterplot3d(twin_peaks_data, pch=20, color = rainbow(4)[cut_agnes_umap_tp], type = "p", main = "AGNES UMAP")
legend( s$xyz.convert(-2, -2, 2), legend =c(1,2,3,4), col = c("#FF0000", "#7852A9", 
                                                              "#00FF00","#00FFFF") ,pch = 16)