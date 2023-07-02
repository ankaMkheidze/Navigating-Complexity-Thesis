
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


set.seed(123)
twin_peaks_data <- generate_twinpeaks_3d(5000, 0.01)

#n_neighbors <- c(15, 50, 35, 50)
n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_tp_15 = c()
# Initialize results
results_k_tp_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_tp_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_tp_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_tp_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

#Optimal Number of Clusters from NbClust
clust_num_tp<-4

row_idx <- 1
n=15
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_15 = cbind(umaps_tp_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


umaps_tp_25 = c()
# Initialize results
results_k_tp_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_tp_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_tp_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_tp_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(twin_peaks_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_25 = cbind(umaps_tp_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


set.seed(123)
umaps_tp_50 = c()
# Initialize results
results_k_tp_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_tp_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_tp_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_tp_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(twin_peaks_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp_50 = cbind(umaps_tp_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_tp)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_tp <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_tp <- as.matrix(silhouette_k_tp)
silhouette_k_tp <- silhouette_k_tp[,3]
silhouette_k_tp <- mean(silhouette_k_tp)

# Dunn index
dunn_k_tp <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_tp <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_tp <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_a_tpgnes <- cutree(agnes_cluster, k= clust_num_tp)
silhouette_a_tp <- silhouette(cut_a_tpgnes, dist(umap_embed))
silhouette_a_tp <- as.matrix(silhouette_a_tp)
silhouette_a_tp <- silhouette_a_tp[,3]
silhouette_a_tp<- mean(silhouette_a_tp)

# Dunn index
dunn_a_tp <- dunn(Data = umap_embed, distance = NULL, cut_a_tpgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_tp <-   calinhara(umap_embed,cut_a_tpgnes,cn=max(cut_a_tpgnes))

# Davies-Bouldin index
davies_a_tp <-index.DB(umap_embed, cut_a_tpgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k_tp, dunn_k_tp, calinski_k_tp, davies_k_tp)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a_tp, dunn_a_tp, calinski_a_tp, davies_a_tp)

row_idx <- row_idx + 1



silhouettes_k_tp <- c(results_k_tp_15[,4], results_k_tp_25[,4], results_k_tp_50[,4])
silhouettes_a_tp <- c(results_a_tp_15[,4], results_a_tp_25[,4], results_a_tp_50[,4])
top3_sk <- order(silhouettes_k_tp, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_tp, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_tp <-c(results_k_tp_15[,5], results_k_tp_25[,5], results_k_tp_50[,5])
dunn_a_tp <-c(results_a_tp_15[,5], results_a_tp_25[,5], results_a_tp_50[,5])
top3_dktp <- order(dunn_k_tp, decreasing = TRUE)[1:5]
print(top3_dktp)
top3_datp <- order(dunn_a_tp, decreasing = TRUE)[1:5]
print(top3_datp)

calin_k_tp <-c(results_k_tp_15[,6], results_k_tp_25[,6], results_k_tp_50[,6])
calin_a_tp <-c(results_a_tp_15[,6], results_a_tp_25[,6], results_a_tp_50[,6])
top3_cktp <- order(calin_k_tp, decreasing = TRUE)[1:5]
print(top3_cktp)
top3_catp <- order(calin_a_tp, decreasing = TRUE)[1:5]
print(top3_catp)

db_k_tp <- c(results_k_tp_15[,7], results_k_tp_25[,7], results_k_tp_50[,7])
db_a_tp <- c(results_a_tp_15[,7], results_a_tp_25[,7], results_a_tp_50[,7])
top3_dbktp <- order(db_k_tp, decreasing = FALSE)[1:5]
print(top3_dbktp)
top3_dbatp <- order(db_a_tp, decreasing = FALSE)[1:5]
print(top3_dbatp)