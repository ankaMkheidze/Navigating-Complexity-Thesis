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

#n_neighbors <- c(15, 50, 35, 50)
n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_bsr_3_15 = c()
# Initialize results
results_k_bsr_3_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_3_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_3_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_3_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(bs_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

set.seed(123)
row_idx <- 1
n=15
md=0.5
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap <- dist(umap_embed, method= "euclidean")
agnes_cluster_umap_embed  <- hclust(dist_bs_data_umap, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_15 = cbind(umaps_bsr_3_15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_15[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



umaps_bsr_3_25 = c()
# Initialize results
results_k_bsr_3_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_3_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_3_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_3_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(bs_data, n_neighbors = 25, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap <- dist(umap_embed, method= "euclidean")
agnes_cluster_umap_embed  <- hclust(dist_bs_data_umap, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_25 = cbind(umaps_bsr_3_25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_25[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



set.seed(123)
umaps_bsr_3_50 = c()
# Initialize results
results_k_bsr_3_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_bsr_3_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_bsr_3_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_bsr_3_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(bs_data, n_neighbors = 15, min_dist = 0.5, n_components = 2,
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap <- dist(umap_embed, method= "euclidean")
agnes_cluster_umap_embed  <- hclust(dist_bs_data_umap, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(bs_data, n_neighbors = n, min_dist = md, n_components = 2,
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_bsr_3_50 = cbind(umaps_bsr_3_50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 3)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k_bsr_3 <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k_bsr_3 <- as.matrix(silhouette_k_bsr_3)
silhouette_k_bsr_3 <- silhouette_k_bsr_3[,3]
silhouette_k_bsr_3 <- mean(silhouette_k_bsr_3)

# Dunn index
dunn_k_bsr_3 <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k_bsr_3 <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_bsr_3 <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_bs_data_umap_tuning <- dist(umap_embed, method= "euclidean")
agnes_cluster<- hclust(dist_bs_data_umap_tuning, method = "ward.D2" )  
cut_a_bsr_3gnes <- cutree(agnes_cluster, k = 3)
silhouette_a_bsr_3 <- silhouette(cut_a_bsr_3gnes, dist(umap_embed))
silhouette_a_bsr_3 <- as.matrix(silhouette_a_bsr_3)
silhouette_a_bsr_3 <- silhouette_a_bsr_3[,3]
silhouette_a_bsr_3<- mean(silhouette_a_bsr_3)

# Dunn index
dunn_a_bsr_3 <- dunn( dist(umap_embed), cut_a_bsr_3gnes)


# Calinski-Harabasz index
calinski_a_bsr_3 <-   calinhara(umap_embed,cut_a_bsr_3gnes,cn=max(cut_a_bsr_3gnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a_bsr_3 <-index.DB(umap_embed, cut_a_bsr_3gnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_k_bsr_3, dunn_k_bsr_3, calinski_k_bsr_3, davies_k_bsr_3)
results_a_bsr_3_50[row_idx, ] <- c(n, md, lr, silhouette_a_bsr_3, dunn_a_bsr_3, calinski_a_bsr_3, davies_a_bsr_3)

row_idx <- row_idx + 1




silhouettes_k_bsr_3 <- c(results_k_bsr_3_15[,4], results_k_bsr_3_25[,4], results_k_bsr_3_50[,4])
silhouettes_a_bsr_3 <- c(results_a_bsr_3_15[,4], results_a_bsr_3_25[,4], results_a_bsr_3_50[,4])
top3_sk <- order(silhouettes_k_bsr_3, decreasing = TRUE)[1:5]
print(top3_sk)
top3_sa <- order(silhouettes_a_bsr_3, decreasing = TRUE)[1:5]
print(top3_sa)

dunn_k_bsr_3 <-c(results_k_bsr_3_15[,5], results_k_bsr_3_25[,5], results_k_bsr_3_50[,5])
dunn_a_bsr_3 <-c(results_a_bsr_3_15[,5], results_a_bsr_3_25[,5], results_a_bsr_3_50[,5])
top3_dkbsr_3 <- order(dunn_k_bsr_3, decreasing = TRUE)[1:5]
print(top3_dkbsr_3)
top3_dabsr_3 <- order(dunn_a_bsr_3, decreasing = TRUE)[1:5]
print(top3_dabsr_3)

calin_k_bsr_3 <-c(results_k_bsr_3_15[,6], results_k_bsr_3_25[,6], results_k_bsr_3_50[,6])
calin_a_bsr_3 <-c(results_a_bsr_3_15[,6], results_a_bsr_3_25[,6], results_a_bsr_3_50[,6])
top3_ckbsr_3 <- order(calin_k_bsr_3, decreasing = TRUE)[1:5]
print(top3_ckbsr_3)
top3_cabsr_3 <- order(calin_a_bsr_3, decreasing = TRUE)[1:5]
print(top3_cabsr_3)

db_k_bsr_3 <- c(results_k_bsr_3_15[,7], results_k_bsr_3_25[,7], results_k_bsr_3_50[,7])
db_a_bsr_3 <- c(results_a_bsr_3_15[,7], results_a_bsr_3_25[,7], results_a_bsr_3_50[,7])
top3_dbkbsr_3 <- order(db_k_bsr_3, decreasing = FALSE)[1:5]
print(top3_dbkbsr_3)
top3_dbabsr_3 <- order(db_a_bsr_3, decreasing = FALSE)[1:5]
print(top3_dbabsr_3)


#WORST UMAPS

worst_sk <- order(silhouettes_k_bsr_3, decreasing = FALSE)[1:5]
print(worst_sk)
worst_sa <- order(silhouettes_a_bsr_3, decreasing = FALSE)[1:5]
print(worst_sa)

worst_dk_bsr_3 <- order(dunn_k_bsr_3, decreasing = FALSE)[1:5]
print(worst_dk_bsr_3)
worst_da_bsr_3 <- order(dunn_a_bsr_3, decreasing = FALSE)[1:5]
print(worst_da_bsr_3)


worst_ck_bsr_3 <- order(calin_k_bsr_3, decreasing = FALSE)[1:5]
print(worst_ck_bsr_3)
worst_ca_bsr_3 <- order(calin_a_bsr_3, decreasing = FALSE)[1:5]
print(worst_ca_bsr_3)


worst_db_k_bsr_3 <- order(db_k_bsr_3, decreasing = TRUE)[1:5]
print(worst_db_k_bsr_3)
worst_db_a_bsr_3 <- order(db_a_bsr_3, decreasing = TRUE)[1:5]
print(worst_db_a_bsr_3)



