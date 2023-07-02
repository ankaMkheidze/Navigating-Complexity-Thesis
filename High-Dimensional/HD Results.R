library(clusterGeneration)
library(scatterplot3d)
library(kernlab)
library(plot3D)
library(Rtsne)
library(cluster)
library(IDmining)
library(NbClust)
library(stats)
library(fastcluster)
library(fpc)
library(cluster) 
library(plotrix)
library(factoextra)
library(clValid)
library(clusterSim)

#this data set was imported from Excel which was generated in Matlab. Please modify the file path to match yours.
library(readxl)
hd_data <- read_excel("C:/Users/Eka/Desktop/High Dimensional data.xlsx", 
                                    col_names = FALSE)


set.seed(1408)
result_k_hd <- NbClust(data = hd_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
set.seed(1)
result_a_hd <- NbClust(data = hd_data, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.2)

result_k_hd$Best.nc
result_a_hd$Best.nc


clust_num_hd <- 3

results_evaluation_hd <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_hd) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
k_means_nodr_hd = kmeans(hd_data,centers  = clust_num_hd)
clusts_nodr_hd <- k_means_nodr_hd$cluster
silhouette_k_means_hd_nodr <- silhouette(clusts_nodr_hd, dist(hd_data))
silhouette_k_means_hd_nodr <- as.matrix(silhouette_k_means_hd_nodr)
silhouette_k_means_hd_nodr <- silhouette_k_means_hd_nodr[,3]
silhouette_k_means_hd_nodr <- mean(silhouette_k_means_hd_nodr)

# Dunn index
dunn_k_means_hd_nodr <- dunn( distance = NULL, clusts_nodr_hd, Data = hd_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_hd_nodr <-   calinhara(hd_data,clusts_nodr_hd,cn=max(clusts_nodr_hd))


# Davies-Bouldin index

davies_k_means_hd_nodr <-index.DB(hd_data, clusts_nodr_hd, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[1,] <- c("K-means NoDR", silhouette_k_means_hd_nodr, 
                               dunn_k_means_hd_nodr, calinski_k_means_hd_nodr,
                               davies_k_means_hd_nodr)

#Method with No DR and AGNES
set.seed(123)
dist_hd_nodr <- dist(hd_data, method= "euclidean")
agnes_hd_nodr <- hclust(dist_hd_nodr, method = "ward.D2" )
cut_agnes_hd_nodr <- cutree(agnes_hd_nodr, k  = clust_num_hd)


silhouette_agnes_hd_nodr <- silhouette(cut_agnes_hd_nodr, dist(hd_data))
silhouette_agnes_hd_nodr <- as.matrix(silhouette_agnes_hd_nodr)
silhouette_agnes_hd_nodr <- silhouette_agnes_hd_nodr[,3]
silhouette_agnes_hd_nodr <- mean(silhouette_agnes_hd_nodr)

dunn_agnes_hd_nodr <- dunn( distance = NULL, Data = hd_data, cut_agnes_hd_nodr, method ="euclidean")
calinski_agnes_hd_nodr <-   calinhara(hd_data, cut_agnes_hd_nodr,cn=max(cut_agnes_hd_nodr))

davies_agnes_hd_nodr <-index.DB(hd_data, cut_agnes_hd_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[2,] <- c("AGNES NoDR", silhouette_agnes_hd_nodr, 
                               dunn_agnes_hd_nodr, calinski_agnes_hd_nodr,
                               davies_agnes_hd_nodr)


#pca hd
set.seed(123)
hd_pca = prcomp(hd_data)
hd_pca = hd_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_hd = kmeans(hd_pca,centers  = clust_num_hd)
clusters_pca_hd <- k_means_pca_hd$cluster

silhouette_k_means_hd_pca <- silhouette(clusters_pca_hd, dist(hd_pca))
silhouette_k_means_hd_pca <- as.matrix(silhouette_k_means_hd_pca)
silhouette_k_means_hd_pca <- silhouette_k_means_hd_pca[,3]
silhouette_k_means_hd_pca <- mean(silhouette_k_means_hd_pca)

# Dunn index
dunn_k_means_hd_pca <- dunn( distance = NULL, clusters_pca_hd, Data = hd_pca, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_hd_pca <-   calinhara(hd_pca,clusters_pca_hd,cn=max(clusters_pca_hd))


# Davies-Bouldin index
davies_k_means_hd_pca <-index.DB(hd_pca, clusters_pca_hd, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[3,] <- c("k-means PCA", silhouette_k_means_hd_pca, 
                               dunn_k_means_hd_pca, calinski_k_means_hd_pca,
                               davies_k_means_hd_pca)

#AGNES with PCA
set.seed(123)
dist_hd_pca <- dist(hd_pca, method= "euclidean")
agnes_hd_pca  <- hclust(dist_hd_pca, method = "ward.D2" )
cut_agnes_hd_pca <- cutree(agnes_hd_pca, k  = clust_num_hd)

silhouette_agnes_hd_pca <- silhouette(cut_agnes_hd_pca, dist(hd_pca))
silhouette_agnes_hd_pca <- as.matrix(silhouette_agnes_hd_pca)
silhouette_agnes_hd_pca <- silhouette_agnes_hd_pca[,3]
silhouette_agnes_hd_pca <- mean(silhouette_agnes_hd_pca)

dunn_agnes_hd_pca <- dunn( distance = NULL, Data =hd_pca, cut_agnes_hd_pca, method = "euclidean")
calinski_agnes_hd_pca <-   calinhara(hd_pca, cut_agnes_hd_pca,cn=max(cut_agnes_hd_pca))

davies_agnes_hd_pca <-index.DB(hd_pca, cut_agnes_hd_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_hd[4,] <- c("AGNES PCA", silhouette_agnes_hd_pca, 
                               dunn_agnes_hd_pca, calinski_agnes_hd_pca,
                               davies_agnes_hd_pca)



#tsne
set.seed(123)
tsne_hd <- Rtsne(hd_data, dims = 2)
tsne_hd <- tsne_hd$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_hd = kmeans(tsne_hd,centers  = clust_num_hd)
clusters_tsne_hd <- k_means_tsne_hd$cluster

silhouette_k_means_hd_tsne <- silhouette(clusters_tsne_hd, dist(tsne_hd))
silhouette_k_means_hd_tsne <- as.matrix(silhouette_k_means_hd_tsne)
silhouette_k_means_hd_tsne <- silhouette_k_means_hd_tsne[,3]
silhouette_k_means_hd_tsne <- mean(silhouette_k_means_hd_tsne)

# Dunn index
dunn_k_means_hd_tsne <- dunn( distance = NULL, clusters_tsne_hd, Data = tsne_hd, method= "euclidean")

# Calinski-Harabasz index
calinski_k_means_hd_tsne <-   calinhara(tsne_hd,clusters_tsne_hd,cn=max(clusters_tsne_hd))


# Davies-Bouldin index

davies_k_means_hd_tsne <-index.DB(tsne_hd, clusters_tsne_hd, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[5,] <- c("k-means t-SNE", silhouette_k_means_hd_tsne, 
                               dunn_k_means_hd_tsne, calinski_k_means_hd_tsne,
                               davies_k_means_hd_tsne)


#agnes t-sne
set.seed(123)
dist_hd_tsne <- dist(tsne_hd, method= "euclidean")
agnes_hd_tsne  <- hclust(dist_hd_tsne, method = "ward.D2" )
cut_agnes_hd_tsne <- cutree(agnes_hd_tsne, k  = clust_num_hd)


silhouette_agnes_hd_tsne <- silhouette(cut_agnes_hd_tsne, dist(tsne_hd))
silhouette_agnes_hd_tsne <- as.matrix(silhouette_agnes_hd_tsne)
silhouette_agnes_hd_tsne <- silhouette_agnes_hd_tsne[,3]
silhouette_agnes_hd_tsne <- mean(silhouette_agnes_hd_tsne)

dunn_agnes_hd_tsne <- dunn( distance = NULL, Data = tsne_hd, cut_agnes_hd_tsne, method = "euclidean")
calinski_agnes_hd_tsne <-   calinhara(tsne_hd, cut_agnes_hd_tsne,cn=max(cut_agnes_hd_tsne))

davies_agnes_hd_tsne <-index.DB(tsne_hd, cut_agnes_hd_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[6,] <- c("AGNES t-SNE", silhouette_agnes_hd_tsne, 
                               dunn_agnes_hd_tsne, calinski_agnes_hd_tsne,
                               davies_agnes_hd_tsne)


#umap
library(uwot)
set.seed(123)
umap_hd <- umap(hd_data, n_neighbors = 15, min_dist = 0.001, n_components = 2, 
                init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_hd_umap <- kmeans(umap_hd, centers  = clust_num_hd)

kmeans_cluster_hd_umap <- kmeans_hd_umap$cluster
silhouette_k_means_hd_umap <- silhouette(kmeans_cluster_hd_umap, dist(umap_hd))
silhouette_k_means_hd_umap <- as.matrix(silhouette_k_means_hd_umap)
silhouette_k_means_hd_umap <- silhouette_k_means_hd_umap[,3]
silhouette_k_means_hd_umap <- mean(silhouette_k_means_hd_umap)

# Dunn index
dunn_k_means_hd_umap <- dunn( distance = NULL, kmeans_cluster_hd_umap, Data = umap_hd, method ="euclidean")


# Calinski-Harabasz index
calinski_k_means_hd_umap <-   calinhara(umap_hd,kmeans_cluster_hd_umap,cn=max(kmeans_cluster_hd_umap))


# Davies-Bouldin index

davies_k_means_hd_umap <-index.DB(umap_hd,kmeans_cluster_hd_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_hd[7,] <- c("k-means UMAP", silhouette_k_means_hd_umap, 
                               dunn_k_means_hd_umap, calinski_k_means_hd_umap,
                               davies_k_means_hd_umap)

#Agnes with UMAP 
set.seed(123)
dist_hd_umap <- dist(umap_hd, method= "euclidean")
agnes_cluster_umap_hd  <- hclust(dist_hd_umap, method = "ward.D2" )
cut_agnes_umap_hd <- cutree(agnes_cluster_umap_hd, k  = clust_num_hd)


silhouette_agnes_hd_umap <- silhouette(cut_agnes_umap_hd, dist(umap_hd))
silhouette_agnes_hd_umap <- as.matrix(silhouette_agnes_hd_umap)
silhouette_agnes_hd_umap <- silhouette_agnes_hd_umap[,3]
silhouette_agnes_hd_umap<- mean(silhouette_agnes_hd_umap)


dunn_agnes_hd_umap <- dunn( distance = NULL, cut_agnes_umap_hd, Data = umap_hd, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_hd_umap <-   calinhara(umap_hd,cut_agnes_umap_hd,cn=max(cut_agnes_umap_hd))


# Davies-Bouldin index

davies_agnes_hd_umap <-index.DB(umap_hd, cut_agnes_umap_hd, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_hd[8,] <- c("AGNES UMAP", silhouette_agnes_hd_umap, 
                               dunn_agnes_hd_umap, calinski_agnes_hd_umap,
                               davies_agnes_hd_umap)



#Visualizations 2-D
fviz_cluster(k_means_nodr_hd,  data = hd_data,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR High Dimensional"
)

fviz_cluster(list(data = hd_data, cluster =cut_agnes_hd_nodr ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with no DR High Dimensional"
)

fviz_cluster(k_means_pca_hd,  data = hd_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA High Dimensional"
)

fviz_cluster(list(data = hd_pca, cluster =cut_agnes_hd_pca ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with PCA High Dimensional"
)

fviz_cluster(k_means_tsne_hd,  data = tsne_hd,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE High Dimensional",
             xlab="V1", ylab = "V2"
)

fviz_cluster(list(data = tsne_hd, cluster =cut_agnes_hd_tsne ),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with t-SNE High Dimensional",
             xlab="V1", ylab = "V2"
)

fviz_cluster(kmeans_hd_umap,  data = umap_hd,
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP High Dimensional"
)

fviz_cluster(list(data = umap_hd, cluster = cut_agnes_umap_hd),
             palette = c("#FF0000", "#FFFF00", "#000FFF",
                         "#00FFFF", "#FF00FF", "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP High Dimensional"
)