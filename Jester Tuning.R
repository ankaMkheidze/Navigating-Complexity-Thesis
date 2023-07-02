
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

set.seed(687)
jest <- jester_comp[sample(nrow(jester_comp),5000),]
jest <-scale(jest)
jest <- jest[,-1]

jest_data <- jest
#n_neighbors <- c(15, 50, 35, 50)
n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_jest_15 = c()
# Initialize results
results_k_jest_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_jest_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_jest_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_jest_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

#Optimal Number of Clusters from NbClust
clust_num_jest <-3

row_idx <- 1
n=15
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_15 = cbind(umaps_jest_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_15[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_15[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


umaps_jest_25 = c()
# Initialize results
results_k_jest_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_jest_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_jest_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_jest_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(jest_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_25 = cbind(umaps_jest_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_25[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_25[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


set.seed(123)
umaps_jest_50 = c()
# Initialize results
results_k_jest_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_jest_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_jest_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_jest_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(jest_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(jest_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_jest_50 = cbind(umaps_jest_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_jest)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_jest <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_jest <- as.matrix(silhouette_k_jest)
silhouette_k_jest <- silhouette_k_jest[,3]
silhouette_k_jest <- mean(silhouette_k_jest)

# Dunn index
dunn_k_jest <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_jest <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_jest <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_jest_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_jest_data_umap_tuning, method = "ward.D2" )  
cut_a_jestgnes <- cutree(agnes_cluster, k= clust_num_jest)
silhouette_a_jest <- silhouette(cut_a_jestgnes, dist(umap_embed))
silhouette_a_jest <- as.matrix(silhouette_a_jest)
silhouette_a_jest <- silhouette_a_jest[,3]
silhouette_a_jest<- mean(silhouette_a_jest)

# Dunn index
dunn_a_jest <- dunn(Data = umap_embed, distance = NULL, cut_a_jestgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_jest <-   calinhara(umap_embed,cut_a_jestgnes,cn=max(cut_a_jestgnes))

# Davies-Bouldin index
davies_a_jest <-index.DB(umap_embed, cut_a_jestgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_jest_50[row_idx, ] <- c(n, md, lr, silhouette_k_jest, dunn_k_jest, calinski_k_jest, davies_k_jest)
results_a_jest_50[row_idx, ] <- c(n, md, lr, silhouette_a_jest, dunn_a_jest, calinski_a_jest, davies_a_jest)

row_idx <- row_idx + 1



silhouettes_k_jest <- c(results_k_jest_15[,4], results_k_jest_25[,4], results_k_jest_50[,4])
silhouettes_a_jest <- c(results_a_jest_15[,4], results_a_jest_25[,4], results_a_jest_50[,4])
top3_sk <- order(silhouettes_k_jest, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_jest, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_jest <-c(results_k_jest_15[,5], results_k_jest_25[,5], results_k_jest_50[,5])
dunn_a_jest <-c(results_a_jest_15[,5], results_a_jest_25[,5], results_a_jest_50[,5])
top3_dkjest <- order(dunn_k_jest, decreasing = TRUE)[1:5]
print(top3_dkjest)
top3_dajest <- order(dunn_a_jest, decreasing = TRUE)[1:5]
print(top3_dajest)

calin_k_jest <-c(results_k_jest_15[,6], results_k_jest_25[,6], results_k_jest_50[,6])
calin_a_jest <-c(results_a_jest_15[,6], results_a_jest_25[,6], results_a_jest_50[,6])
top3_ckjest <- order(calin_k_jest, decreasing = TRUE)[1:5]
print(top3_ckjest)
top3_cajest <- order(calin_a_jest, decreasing = TRUE)[1:5]
print(top3_cajest)

db_k_jest <- c(results_k_jest_15[,7], results_k_jest_25[,7], results_k_jest_50[,7])
db_a_jest <- c(results_a_jest_15[,7], results_a_jest_25[,7], results_a_jest_50[,7])
top3_dbkjest <- order(db_k_jest, decreasing = FALSE)[1:5]
print(top3_dbkjest)
top3_dbajest <- order(db_a_jest, decreasing = FALSE)[1:5]
print(top3_dbajest)