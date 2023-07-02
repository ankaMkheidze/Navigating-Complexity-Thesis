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

hd_data <- hd_data

n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_hd_15 = c()
# Initialize results
results_k_hd_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_hd_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_hd_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_hd_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

#Optimal Number of Clusters from NbClust
clust_num_hd<-3

row_idx <- 1
n=15
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_15 = cbind(umaps_hd_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_15[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_15[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


umaps_hd_25 = c()
# Initialize results
results_k_hd_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_hd_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_hd_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_hd_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(hd_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_25 = cbind(umaps_hd_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_25[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_25[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


set.seed(123)
umaps_hd_50 = c()
# Initialize results
results_k_hd_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_hd_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_hd_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_hd_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(hd_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(hd_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_hd_50 = cbind(umaps_hd_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_hd)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_hd <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_hd <- as.matrix(silhouette_k_hd)
silhouette_k_hd <- silhouette_k_hd[,3]
silhouette_k_hd <- mean(silhouette_k_hd)

# Dunn index
dunn_k_hd <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_hd <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_hd <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_hd_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_hd_data_umap_tuning, method = "ward.D2" )  
cut_a_hdgnes <- cutree(agnes_cluster, k= clust_num_hd)
silhouette_a_hd <- silhouette(cut_a_hdgnes, dist(umap_embed))
silhouette_a_hd <- as.matrix(silhouette_a_hd)
silhouette_a_hd <- silhouette_a_hd[,3]
silhouette_a_hd<- mean(silhouette_a_hd)

# Dunn index
dunn_a_hd <- dunn(Data = umap_embed, distance = NULL, cut_a_hdgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_hd <-   calinhara(umap_embed,cut_a_hdgnes,cn=max(cut_a_hdgnes))

# Davies-Bouldin index
davies_a_hd <-index.DB(umap_embed, cut_a_hdgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_hd_50[row_idx, ] <- c(n, md, lr, silhouette_k_hd, dunn_k_hd, calinski_k_hd, davies_k_hd)
results_a_hd_50[row_idx, ] <- c(n, md, lr, silhouette_a_hd, dunn_a_hd, calinski_a_hd, davies_a_hd)

row_idx <- row_idx + 1



silhouettes_k_hd <- c(results_k_hd_15[,4], results_k_hd_25[,4], results_k_hd_50[,4])
silhouettes_a_hd <- c(results_a_hd_15[,4], results_a_hd_25[,4], results_a_hd_50[,4])
top3_sk <- order(silhouettes_k_hd, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_hd, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_hd <-c(results_k_hd_15[,5], results_k_hd_25[,5], results_k_hd_50[,5])
dunn_a_hd <-c(results_a_hd_15[,5], results_a_hd_25[,5], results_a_hd_50[,5])
top3_dkhd <- order(dunn_k_hd, decreasing = TRUE)[1:5]
print(top3_dkhd)
top3_dahd <- order(dunn_a_hd, decreasing = TRUE)[1:5]
print(top3_dahd)

calin_k_hd <-c(results_k_hd_15[,6], results_k_hd_25[,6], results_k_hd_50[,6])
calin_a_hd <-c(results_a_hd_15[,6], results_a_hd_25[,6], results_a_hd_50[,6])
top3_ckhd <- order(calin_k_hd, decreasing = TRUE)[1:5]
print(top3_ckhd)
top3_cahd <- order(calin_a_hd, decreasing = TRUE)[1:5]
print(top3_cahd)

db_k_hd <- c(results_k_hd_15[,7], results_k_hd_25[,7], results_k_hd_50[,7])
db_a_hd <- c(results_a_hd_15[,7], results_a_hd_25[,7], results_a_hd_50[,7])
top3_dbkhd <- order(db_k_hd, decreasing = FALSE)[1:5]
print(top3_dbkhd)
top3_dbahd <- order(db_a_hd, decreasing = FALSE)[1:5]
print(top3_dbahd)