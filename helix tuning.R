
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
helix <- helix_data_generation(n = 5000)

#n_neighbors <- c(15, 50, 35, 50)
n_neighbors <- c(15)
min_dist <- c(0.5, 0.1, 0.05, 0.001)
learning_rate <- c(0.5, 1.0, 5.0)

umaps_helix15 = c()
# Initialize results
results_k_helix_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_15 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_15) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(helix, n_neighbors = 15, min_dist = 0.5, n_components = 2, 
                 init = "laplacian", learning_rate = 0.5)

set.seed(123)
row_idx <- 1
n=15
md=0.5
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_helix_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix15 = cbind(umaps_helix15, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_15[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_15[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



umaps_helix25 = c()
# Initialize results
results_k_helix_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_25 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_25) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(helix, n_neighbors = 25, min_dist = 0.5, n_components = 2, 
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=25
md=0.5
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_helix_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix25 = cbind(umaps_helix25, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_25[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_25[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



set.seed(123)
umaps_helix50 = c()
# Initialize results
results_k_helix_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_k_helix_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")

results_a_helix_50 <- matrix(ncol = 7, nrow = length(n_neighbors) * length(min_dist) * length(learning_rate))
colnames(results_a_helix_50) <- c("n_neighbors", "min_dist", "learning_rate", "silhouette", "dunn", "calinski", "db")
umap_obj <- umap(helix, n_neighbors = 15, min_dist = 0.5, n_components = 2, 
                 init = "laplacian", learning_rate = 0.5)

row_idx <- 1
n=50
md=0.5
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap <- dist(umap_embed, method= "euclidean") 
agnes_cluster_umap_embed  <- hclust(dist_helix_umap, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


md=0.1
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" ) 
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.05
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1



md=0.001
lr =0.5
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)
kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1


lr =1.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




lr =5.0
umap_obj <- umap(helix, n_neighbors = n, min_dist = md, n_components = 2, 
                 init = "laplacian", learning_rate = lr)
umap_embed <- umap_obj
umaps_helix50 = cbind(umaps_helix50, umap_embed)

kmeans_cluster <- kmeans(umap_embed, centers = 5)
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
dist_helix_umap_tuning <- dist(umap_embed, method= "euclidean") 
agnes_cluster<- hclust(dist_helix_umap_tuning, method = "ward.D2" )  
cut_agnes <- cutree(agnes_cluster, k = 5)
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
results_k_helix_50[row_idx, ] <- c(n, md, lr, silhouette_k, dunn_k, calinski_k, davies_k)
results_a_helix_50[row_idx, ] <- c(n, md, lr, silhouette_a, dunn_a, calinski_a, davies_a)

row_idx <- row_idx + 1




silhouettes_k_helix <- c(results_k_helix_15[,4], results_k_helix_25[,4], results_k_helix_50[,4])
silhouettes_a_helix <- c(results_a_helix_15[,4], results_a_helix_25[,4], results_a_helix_50[,4])
top3_sk_helix <- order(silhouettes_k_helix, decreasing = TRUE)[1:5]
print(top3_sk_helix)
top3_sa_helix <- order(silhouettes_a_helix, decreasing = TRUE)[1:5]
print(top3_sa_helix)

dunn_k_helix <-c(results_k_helix_15[,5], results_k_helix_25[,5], results_k_helix_50[,5])
dunn_a_helix <-c(results_a_helix_15[,5], results_a_helix_25[,5], results_a_helix_50[,5])
top3_dk_helix <- order(dunn_k_helix, decreasing = TRUE)[1:5]
print(top3_dk_helix)
top3_da_helix <- order(dunn_a_helix, decreasing = TRUE)[1:5]
print(top3_da_helix)

calin_k_helix <-c(results_k_helix_15[,6], results_k_helix_25[,6], results_k_helix_50[,6])
calin_a_helix <-c(results_a_helix_15[,6], results_a_helix_25[,6], results_a_helix_50[,6])
top3_ck_helix <- order(calin_k_helix, decreasing = TRUE)[1:5]
print(top3_ck_helix)
top3_ca_helix <- order(calin_a_helix, decreasing = TRUE)[1:5]
print(top3_ca_helix)

db_k_helix <- c(results_k_helix_15[,7], results_k_helix_25[,7], results_k_helix_50[,7])
db_a_helix <- c(results_a_helix_15[,7], results_a_helix_25[,7], results_a_helix_50[,7])
top3_dbk_helix <- order(db_k_helix, decreasing = FALSE)[1:5]
print(top3_dbk_helix)
top3_dba_helix <- order(db_a_helix, decreasing = FALSE)[1:5]
print(top3_dba_helix)
