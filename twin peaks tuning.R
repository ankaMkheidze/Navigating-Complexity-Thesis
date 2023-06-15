
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

umaps_tp15 = c()
# Initialize results
results_k_tp_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_tp_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_tp_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_tp_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(twin_peaks_data, n_neighbors = 15, min_dist = 0.5, n_components = 2, 
                 init = "laplacian", learning_rate = 0.5)

set.seed(123)
row_idx <- 1
n=15
md=0.5
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_twin_peaks_data_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp15 = cbind(umaps_tp15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



umaps_tp25 = c()
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
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_twin_peaks_data_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp25 = cbind(umaps_tp25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



set.seed(123)
umaps_tp50 = c()
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
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_twin_peaks_data_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(twin_peaks_data, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_tp50 = cbind(umaps_tp50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 4)
kmeans_cluster <-kmeans_cluster$cluster
silhouette_k <- silhouette(kmeans_cluster, dist(umap_embed))
silhouette_k <- as.matrix(silhouette_k)
silhouette_k <- silhouette_k[,3]
silhouette_k <- mean(silhouette_k)

# Dunn index
dunn_k <- dunn( dist(umap_embed), kmeans_cluster)


# Calinski-Harabasz index
calinski_k <-   calinhara(umap_embed,kmeans_cluster,cn=max(kmeans_cluster))


# Davies-Bouldin index
#KLEOBAA ES
davies_k <-index.DB(umap_embed,kmeans_cluster, d=NULL, centrotypes="centroids", p=2, q=2)$DB





# AGNES clustering
dist_twin_peaks_data_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_twin_peaks_data_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 4)
silhouette_a <- silhouette(cut_agnes, dist(umap_embed))
silhouette_a <- as.matrix(silhouette_a)
silhouette_a <- silhouette_a[,3]
silhouette_a<- mean(silhouette_a)

# Dunn index
dunn_a <- dunn( dist(umap_embed), cut_agnes)


# Calinski-Harabasz index
calinski_a <-   calinhara(umap_embed,cut_agnes,cn=max(cut_agnes))


# Davies-Bouldin index
#KLEOBAA ES
davies_a <-index.DB(umap_embed, cut_agnes, d=NULL, centrotypes="centroids", p=2, q=2)$DB


# Evaluation metric (replace with your own evaluation metric)


# Store results
results_k_tp_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_tp_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




silhouettes_k_tp <- c(results_k_tp_15[,4], results_k_tp_25[,4], results_k_tp_50[,4])
silhouettes_a_tp <- c(results_a_tp_15[,4], results_a_tp_25[,4], results_a_tp_50[,4])
top3_sk_tp <- order(silhouettes_k_tp, decreasing = TRUE)[1:5]
print(top3_sk_tp)
top3_sa_tp <- order(silhouettes_a_tp, decreasing = TRUE)[1:5]
print(top3_sa_tp)

dunn_k_tp <-c(results_k_tp_15[,5], results_k_tp_25[,5], results_k_tp_50[,5])
dunn_a_tp <-c(results_a_tp_15[,5], results_a_tp_25[,5], results_a_tp_50[,5])
top3_dk_tp <- order(dunn_k_tp, decreasing = TRUE)[1:5]
print(top3_dk_tp)
top3_da_tp <- order(dunn_a_tp, decreasing = TRUE)[1:5]
print(top3_da_tp)

calin_k_tp <-c(results_k_tp_15[,6], results_k_tp_25[,6], results_k_tp_50[,6])
calin_a_tp <-c(results_a_tp_15[,6], results_a_tp_25[,6], results_a_tp_50[,6])
top3_ck_tp <- order(calin_k_tp, decreasing = TRUE)[1:5]
print(top3_ck_tp)
top3_ca_tp <- order(calin_a_tp, decreasing = TRUE)[1:5]
print(top3_ca_tp)

db_k_tp <- c(results_k_tp_15[,7], results_k_tp_25[,7], results_k_tp_50[,7])
db_a_tp <- c(results_a_tp_15[,7], results_a_tp_25[,7], results_a_tp_50[,7])
top3_dbk_tp <- order(db_k_tp, decreasing = FALSE)[1:5]
print(top3_dbk_tp)
top3_dba_tp <- order(db_a_tp, decreasing = FALSE)[1:5]
print(top3_dba_tp)
