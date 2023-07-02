
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
helix_data <- helix_data_generation(n = 5000)

#n_neighbors <- c(15, 50, 35, 50)
n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_helix_15 = c()
# Initialize results
results_k_helix_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

#Optimal Number of Clusters from NbClust
clust_num_helix<-5

row_idx <- 1
n=15
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_15 = cbind(umaps_helix_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


umaps_helix_25 = c()
# Initialize results
results_k_helix_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(helix_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_25 = cbind(umaps_helix_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


set.seed(123)
umaps_helix_50 = c()
# Initialize results
results_k_helix_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(helix_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(helix_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix_50 = cbind(umaps_helix_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num_helix)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_helix <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_helix <- as.matrix(silhouette_k_helix)
silhouette_k_helix <- silhouette_k_helix[,3]
silhouette_k_helix <- mean(silhouette_k_helix)

# Dunn index
dunn_k_helix <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_helix <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_helix <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_helix_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_helix_data_umap_tuning, method = "ward.D2" )  
cut_a_helixgnes <- cutree(agnes_cluster, k= clust_num_helix)
silhouette_a_helix <- silhouette(cut_a_helixgnes, dist(umap_embed))
silhouette_a_helix <- as.matrix(silhouette_a_helix)
silhouette_a_helix <- silhouette_a_helix[,3]
silhouette_a_helix<- mean(silhouette_a_helix)

# Dunn index
dunn_a_helix <- dunn(Data = umap_embed, distance = NULL, cut_a_helixgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_helix <-   calinhara(umap_embed,cut_a_helixgnes,cn=max(cut_a_helixgnes))

# Davies-Bouldin index
davies_a_helix <-index.DB(umap_embed, cut_a_helixgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k_helix, dunn_k_helix, calinski_k_helix, davies_k_helix)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a_helix, dunn_a_helix, calinski_a_helix, davies_a_helix)

row_idx <- row_idx + 1



silhouettes_k_helix <- c(results_k_helix_15[,4], results_k_helix_25[,4], results_k_helix_50[,4])
silhouettes_a_helix <- c(results_a_helix_15[,4], results_a_helix_25[,4], results_a_helix_50[,4])
top3_sk <- order(silhouettes_k_helix, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_helix, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_helix <-c(results_k_helix_15[,5], results_k_helix_25[,5], results_k_helix_50[,5])
dunn_a_helix <-c(results_a_helix_15[,5], results_a_helix_25[,5], results_a_helix_50[,5])
top3_dkhelix <- order(dunn_k_helix, decreasing = TRUE)[1:5]
print(top3_dkhelix)
top3_dahelix <- order(dunn_a_helix, decreasing = TRUE)[1:5]
print(top3_dahelix)

calin_k_helix <-c(results_k_helix_15[,6], results_k_helix_25[,6], results_k_helix_50[,6])
calin_a_helix <-c(results_a_helix_15[,6], results_a_helix_25[,6], results_a_helix_50[,6])
top3_ckhelix <- order(calin_k_helix, decreasing = TRUE)[1:5]
print(top3_ckhelix)
top3_cahelix <- order(calin_a_helix, decreasing = TRUE)[1:5]
print(top3_cahelix)

db_k_helix <- c(results_k_helix_15[,7], results_k_helix_25[,7], results_k_helix_50[,7])
db_a_helix <- c(results_a_helix_15[,7], results_a_helix_25[,7], results_a_helix_50[,7])
top3_dbkhelix <- order(db_k_helix, decreasing = FALSE)[1:5]
print(top3_dbkhelix)
top3_dbahelix <- order(db_a_helix, decreasing = FALSE)[1:5]
print(top3_dbahelix)