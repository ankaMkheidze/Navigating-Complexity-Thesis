
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

helix_data_generation <- function(n , rmajor = 2, rminor = 1, nwinds = 5) {
  u <- seq(-pi, pi, length.out = n)
  w <- rmajor + (rminor * cos(nwinds * u))
  x <- w * cos(u)
  y <- w * sin(u)
  z <- rminor * sin(nwinds * u)
  
  df <- data.frame(x, y, z)
  
  dist <- sqrt(rowSums(df ^ 2))
 
  
  df
}

#Generate Data
set.seed(123)
helix <- helix_data_generation(n = 5000)
helix_data <- helix

#Find optimal number of Clusters
set.seed(123)
result_k_helix <- NbClust(data = helix_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
set.seed(123)
result_a_helix_ward2 <- NbClust(data = helix_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.1)

result_k_helix$Best.nc
result_a_helix_duda <- NbClust(helix_data, distance = "euclidean", method = "ward.D2", min.nc = 2, max.nc = 10, index = "pseudot2")

#Initilize results matrix
results_evaluation_helix <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_helix) <- c("Method", "Silhouette", "Dunn", "CH", "DB")

#Input optimal number of clusters
clust_num_helix <- 5

#Method without DR and k-means
set.seed(123)
k_means_nodr_helix = kmeans(helix_data,centers  = clust_num_helix)
clusts_nodr_helix <- k_means_nodr_helix$cluster
silhouette_k_means_helix_nodr <- silhouette(clusts_nodr_helix, dist(helix_data))
silhouette_k_means_helix_nodr <- as.matrix(silhouette_k_means_helix_nodr)
silhouette_k_means_helix_nodr <- silhouette_k_means_helix_nodr[,3]
silhouette_k_means_helix_nodr <- mean(silhouette_k_means_helix_nodr)

# Dunn index
dunn_k_means_helix_nodr <- dunn( distance = NULL, clusts_nodr_helix, Data = helix_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_helix_nodr <-   calinhara(helix_data,clusts_nodr_helix,cn=max(clusts_nodr_helix))


# Davies-Bouldin index

davies_k_means_helix_nodr <-index.DB(helix_data, clusts_nodr_helix, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[1,] <- c("K-means NoDR", silhouette_k_means_helix_nodr, 
                               dunn_k_means_helix_nodr, calinski_k_means_helix_nodr,
                               davies_k_means_helix_nodr)

#Method with No DR and AGNES
set.seed(123)
dist_helix_nodr <- dist(helix_data, method= "euclidean")
agnes_helix_nodr <- hclust(dist_helix_nodr, method = "ward.D2" )
cut_agnes_helix_nodr <- cutree(agnes_helix_nodr, k  = clust_num_helix)


silhouette_agnes_helix_nodr <- silhouette(cut_agnes_helix_nodr, dist(helix_data))
silhouette_agnes_helix_nodr <- as.matrix(silhouette_agnes_helix_nodr)
silhouette_agnes_helix_nodr <- silhouette_agnes_helix_nodr[,3]
silhouette_agnes_helix_nodr <- mean(silhouette_agnes_helix_nodr)

dunn_agnes_helix_nodr <- dunn( distance = NULL, Data = helix_data, cut_agnes_helix_nodr, method ="euclidean")
calinski_agnes_helix_nodr <-   calinhara(helix_data, cut_agnes_helix_nodr,cn=max(cut_agnes_helix_nodr))

davies_agnes_helix_nodr <-index.DB(helix_data, cut_agnes_helix_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[2,] <- c("AGNES NoDR", silhouette_agnes_helix_nodr, 
                               dunn_agnes_helix_nodr, calinski_agnes_helix_nodr,
                               davies_agnes_helix_nodr)


#pca helix
set.seed(123)
helix_pca = prcomp(helix_data)
helix_pca = helix_pca$x[,1:2]
#kmeans PCA
set.seed(123)
k_means_pca_helix = kmeans(helix_pca,centers  = clust_num_helix)
clusters_pca_helix <- k_means_pca_helix$cluster

silhouette_k_means_helix_pca <- silhouette(clusters_pca_helix, dist(helix_pca))
silhouette_k_means_helix_pca <- as.matrix(silhouette_k_means_helix_pca)
silhouette_k_means_helix_pca <- silhouette_k_means_helix_pca[,3]
silhouette_k_means_helix_pca <- mean(silhouette_k_means_helix_pca)

# Dunn index
dunn_k_means_helix_pca <- dunn( distance = NULL, clusters_pca_helix, Data = helix_pca, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_helix_pca <-   calinhara(helix_pca,clusters_pca_helix,cn=max(clusters_pca_helix))


# Davies-Bouldin index
davies_k_means_helix_pca <-index.DB(helix_pca, clusters_pca_helix, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[3,] <- c("k-means PCA", silhouette_k_means_helix_pca, 
                               dunn_k_means_helix_pca, calinski_k_means_helix_pca,
                               davies_k_means_helix_pca)

#AGNES with PCA
set.seed(123)
dist_helix_pca <- dist(helix_pca, method= "euclidean")
agnes_helix_pca  <- hclust(dist_helix_pca, method = "ward.D2" )
cut_agnes_helix_pca <- cutree(agnes_helix_pca, k  = clust_num_helix)

silhouette_agnes_helix_pca <- silhouette(cut_agnes_helix_pca, dist(helix_pca))
silhouette_agnes_helix_pca <- as.matrix(silhouette_agnes_helix_pca)
silhouette_agnes_helix_pca <- silhouette_agnes_helix_pca[,3]
silhouette_agnes_helix_pca <- mean(silhouette_agnes_helix_pca)

dunn_agnes_helix_pca <- dunn( distance = NULL, Data =helix_pca, cut_agnes_helix_pca, method = "euclidean")
calinski_agnes_helix_pca <-   calinhara(helix_pca, cut_agnes_helix_pca,cn=max(cut_agnes_helix_pca))

davies_agnes_helix_pca <-index.DB(helix_pca, cut_agnes_helix_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_helix[4,] <- c("AGNES PCA", silhouette_agnes_helix_pca, 
                               dunn_agnes_helix_pca, calinski_agnes_helix_pca,
                               davies_agnes_helix_pca)



#tsne
set.seed(123)
tsne_helix <- Rtsne(helix_data, dims = 2)
tsne_helix <- tsne_helix$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_helix = kmeans(tsne_helix,centers  = clust_num_helix)
clusters_tsne_helix <- k_means_tsne_helix$cluster

silhouette_k_means_helix_tsne <- silhouette(clusters_tsne_helix, dist(tsne_helix))
silhouette_k_means_helix_tsne <- as.matrix(silhouette_k_means_helix_tsne)
silhouette_k_means_helix_tsne <- silhouette_k_means_helix_tsne[,3]
silhouette_k_means_helix_tsne <- mean(silhouette_k_means_helix_tsne)

# Dunn index
dunn_k_means_helix_tsne <- dunn( distance = NULL, clusters_tsne_helix, Data = tsne_helix, method= "euclidean")

# Calinski-Harabasz index
calinski_k_means_helix_tsne <-   calinhara(tsne_helix,clusters_tsne_helix,cn=max(clusters_tsne_helix))


# Davies-Bouldin index

davies_k_means_helix_tsne <-index.DB(tsne_helix, clusters_tsne_helix, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[5,] <- c("k-means t-SNE", silhouette_k_means_helix_tsne, 
                               dunn_k_means_helix_tsne, calinski_k_means_helix_tsne,
                               davies_k_means_helix_tsne)


#agnes t-sne
set.seed(123)
dist_helix_tsne <- dist(tsne_helix, method= "euclidean")
agnes_helix_tsne  <- hclust(dist_helix_tsne, method = "ward.D2" )
cut_agnes_helix_tsne <- cutree(agnes_helix_tsne, k  = clust_num_helix)


silhouette_agnes_helix_tsne <- silhouette(cut_agnes_helix_tsne, dist(tsne_helix))
silhouette_agnes_helix_tsne <- as.matrix(silhouette_agnes_helix_tsne)
silhouette_agnes_helix_tsne <- silhouette_agnes_helix_tsne[,3]
silhouette_agnes_helix_tsne <- mean(silhouette_agnes_helix_tsne)

dunn_agnes_helix_tsne <- dunn( distance = NULL, Data = tsne_helix, cut_agnes_helix_tsne, method = "euclidean")
calinski_agnes_helix_tsne <-   calinhara(tsne_helix, cut_agnes_helix_tsne,cn=max(cut_agnes_helix_tsne))

davies_agnes_helix_tsne <-index.DB(tsne_helix, cut_agnes_helix_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[6,] <- c("AGNES t-SNE", silhouette_agnes_helix_tsne, 
                               dunn_agnes_helix_tsne, calinski_agnes_helix_tsne,
                               davies_agnes_helix_tsne)


#umap
library(uwot)
set.seed(123)
umap_helix <- umap(helix_data, n_neighbors = 25, min_dist = 0.05, n_components = 2, 
                init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_helix_umap <- kmeans(umap_helix, centers  = clust_num_helix)

kmeans_cluster_helix_umap <- kmeans_helix_umap$cluster
silhouette_k_means_helix_umap <- silhouette(kmeans_cluster_helix_umap, dist(umap_helix))
silhouette_k_means_helix_umap <- as.matrix(silhouette_k_means_helix_umap)
silhouette_k_means_helix_umap <- silhouette_k_means_helix_umap[,3]
silhouette_k_means_helix_umap <- mean(silhouette_k_means_helix_umap)

# Dunn index
dunn_k_means_helix_umap <- dunn( distance = NULL, kmeans_cluster_helix_umap, Data = umap_helix, method ="euclidean")


# Calinski-Harabasz index
calinski_k_means_helix_umap <-   calinhara(umap_helix,kmeans_cluster_helix_umap,cn=max(kmeans_cluster_helix_umap))


# Davies-Bouldin index

davies_k_means_helix_umap <-index.DB(umap_helix,kmeans_cluster_helix_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_helix[7,] <- c("k-means UMAP", silhouette_k_means_helix_umap, 
                               dunn_k_means_helix_umap, calinski_k_means_helix_umap,
                               davies_k_means_helix_umap)

#Agnes with UMAP 
set.seed(123)
dist_helix_umap <- dist(umap_helix, method= "euclidean")
agnes_cluster_umap_helix  <- hclust(dist_helix_umap, method = "ward.D2" )
cut_agnes_umap_helix <- cutree(agnes_cluster_umap_helix, k  = clust_num_helix)


silhouette_agnes_helix_umap <- silhouette(cut_agnes_umap_helix, dist(umap_helix))
silhouette_agnes_helix_umap <- as.matrix(silhouette_agnes_helix_umap)
silhouette_agnes_helix_umap <- silhouette_agnes_helix_umap[,3]
silhouette_agnes_helix_umap<- mean(silhouette_agnes_helix_umap)


dunn_agnes_helix_umap <- dunn( distance = NULL, cut_agnes_umap_helix, Data = umap_helix, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_helix_umap <-   calinhara(umap_helix,cut_agnes_umap_helix,cn=max(cut_agnes_umap_helix))


# Davies-Bouldin index

davies_agnes_helix_umap <-index.DB(umap_helix, cut_agnes_umap_helix, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_helix[8,] <- c("AGNES UMAP", silhouette_agnes_helix_umap, 
                               dunn_agnes_helix_umap, calinski_agnes_helix_umap,
                               davies_agnes_helix_umap)




#Visualizations 2-D
fviz_cluster(k_means_nodr_helix,  data = helix_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR Helix"
)

fviz_cluster(list(data = helix_pca, cluster =cut_agnes_helix_nodr ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with no DR Helix"
)

fviz_cluster(k_means_pca_helix,  data = helix_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA Helix"
)

fviz_cluster(list(data = helix_pca, cluster =cut_agnes_helix_pca ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with PCA Helix"
)

fviz_cluster(k_means_tsne_helix,  data = tsne_helix,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE Helix",
             xlab="V1", ylab = "V2"
)

fviz_cluster(list(data = tsne_helix, cluster =cut_agnes_helix_tsne ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with t-SNE Helix",
             xlab="V1", ylab = "V2"
)

fviz_cluster(kmeans_helix_umap,  data = umap_helix,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP Helix"
)

fviz_cluster(list(data = umap_helix, cluster = cut_agnes_umap_helix),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP Helix"
)

#Visualizations 3-D
dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(6)[k_means_nodr_helix$cluster], type = "p", main = "kmeans no DR")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                               "#00FF00","#00FFFF") ,pch = 16)

dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[cut_agnes_helix_nodr], type = "p", main = "AGNES no DR")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                   "#00FFFF", "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[k_means_pca_helix$cluster], type = "p", main = "kmeans PCA")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                   "#00FFFF", "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[cut_agnes_helix_pca], type = "p", main = "AGNES PCA")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                   "#00FFFF", "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[k_means_tsne_helix$cluster], type = "p", main = "kmeans t-SNE")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5,6), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                   "#00FFFF", "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[cut_agnes_helix_tsne], type = "p", main = "AGNES t-SNE")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                   "#00FFFF", "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[kmeans_helix_umap$cluster], type = "p", main = "kmeans UMAP")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                "#FF00FF", "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(helix_data, pch=20, color = rainbow(5)[cut_agnes_umap_helix], type = "p", main = "AGNES UMAP")
legend( s$xyz.convert(-3, -3, 0), legend =c(1,2,3,4,5), col = c("#FF0000", "#FFFF00", "#000FFF",
                                                                "#FF00FF", "#00FF00") ,pch = 16)
