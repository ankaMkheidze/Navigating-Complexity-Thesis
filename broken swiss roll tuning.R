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

set.seed(1408)
broken_swiss_roll <- generate_data_broken_swiss(5000, 0.01)
bs_data <- cbind(broken_swiss_roll$X.1, broken_swiss_roll$X.2, broken_swiss_roll$X.3)
colnames(bs_data) <- c("Dim1", "Dim2", "Dim3")

n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_bsr_15 = c()
# Initialize results
results_k_bsr_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

#Optimal Number of Clusters from NbClust
clust_num <-3

row_idx <- 1
n=15
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_15 = cbind(umaps_bsr_15, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


umaps_bsr_25 = c()
# Initialize results
results_k_bsr_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(bs_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_25 = cbind(umaps_bsr_25, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


set.seed(123)
umaps_bsr_50 = c()
# Initialize results
results_k_bsr_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(bs_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.1
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


md=0.05
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1


lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

md=0.001
lr =0.5
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =1.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1

lr =5.0
set.seed(123)
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_50 = cbind(umaps_bsr_50, umap_embed)
set.seed(123)
kmeans_cluster <- kmeans(umap_embed, centers= clust_num)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr <- as.matrix(silhouette_k_bsr)
silhouette_k_bsr <- silhouette_k_bsr[,3]
silhouette_k_bsr <- mean(silhouette_k_bsr)

# Dunn index
dunn_k_bsr <- dunn(Data = umap_embed, distance = NULL, kmeans_cluster, method = "euclidean")

# Calinski-Harabasz index
calinski_k_bsr <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))

# Davies-Bouldin index
davies_k_bsr <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsrgnes <- cutree(agnes_cluster, k= clust_num)
silhouette_a_bsr <- silhouette(cut_a_bsrgnes, dist(umap_embed))
silhouette_a_bsr <- as.matrix(silhouette_a_bsr)
silhouette_a_bsr <- silhouette_a_bsr[,3]
silhouette_a_bsr<- mean(silhouette_a_bsr)

# Dunn index
dunn_a_bsr <- dunn(Data = umap_embed, distance = NULL, cut_a_bsrgnes, method = "euclidean")

# Calinski-Harabasz index
calinski_a_bsr <-   calinhara(umap_embed,cut_a_bsrgnes,cn=max(cut_a_bsrgnes))

# Davies-Bouldin index
davies_a_bsr <-index.DB(umap_embed, cut_a_bsrgnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB

# Store results
results_k_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr, dunn_k_bsr, calinski_k_bsr, davies_k_bsr)
results_a_bsr_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr, dunn_a_bsr, calinski_a_bsr, davies_a_bsr)

row_idx <- row_idx + 1



silhouettes_k_bsr <- c(results_k_bsr_15[,4], results_k_bsr_25[,4], results_k_bsr_50[,4])
silhouettes_a_bsr <- c(results_a_bsr_15[,4], results_a_bsr_25[,4], results_a_bsr_50[,4])
top3_sk <- order(silhouettes_k_bsr, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_bsr, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_bsr <-c(results_k_bsr_15[,5], results_k_bsr_25[,5], results_k_bsr_50[,5])
dunn_a_bsr <-c(results_a_bsr_15[,5], results_a_bsr_25[,5], results_a_bsr_50[,5])
top3_dkbsr <- order(dunn_k_bsr, decreasing = TRUE)[1:5]
print(top3_dkbsr)
top3_dabsr <- order(dunn_a_bsr, decreasing = TRUE)[1:5]
print(top3_dabsr)

calin_k_bsr <-c(results_k_bsr_15[,6], results_k_bsr_25[,6], results_k_bsr_50[,6])
calin_a_bsr <-c(results_a_bsr_15[,6], results_a_bsr_25[,6], results_a_bsr_50[,6])
top3_ckbsr <- order(calin_k_bsr, decreasing = TRUE)[1:5]
print(top3_ckbsr)
top3_cabsr <- order(calin_a_bsr, decreasing = TRUE)[1:5]
print(top3_cabsr)

db_k_bsr <- c(results_k_bsr_15[,7], results_k_bsr_25[,7], results_k_bsr_50[,7])
db_a_bsr <- c(results_a_bsr_15[,7], results_a_bsr_25[,7], results_a_bsr_50[,7])
top3_dbkbsr <- order(db_k_bsr, decreasing = FALSE)[1:5]
print(top3_dbkbsr)
top3_dbabsr <- order(db_a_bsr, decreasing = FALSE)[1:5]
print(top3_dbabsr)