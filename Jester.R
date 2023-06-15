#installs
install.packages("NbClust")
install.packages("fpc")
install.packages("cluster.stats")
install.packages("factoextra")
install.packages("cluster")
install.packages("clValid")  
install.packages("RDRToolbox")
install.packages("clusterSim")
install.packages("Rtsne")
install.packages("ellipsis")
install.packages("pillar")



#Libraries
library(readxl)
library(NbClust)
library(stats)
library(fpc)
library(cluster)
library(factoextra)
library(clValid)
library(clusterSim)
library(RDRToolbox)
library(Rtsne)
library(ggpubr)

#Load Data
jester_data_1 <- read_excel("C:/Users/568785am/Desktop/jester-data-1.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester_data_2 <- read_excel("C:/Users/568785am/Desktop/jester-data-2.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester_data_3 <- read_excel("C:/Users/568785am/Desktop/jester-data-3.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester <- rbind(jester_data_1,  jester_data_2)
jester_comp <- jester[complete.cases(jester),]
jester_comp <- scale(jester_comp)
jester_comp <- scale(jester_comp)

#pretty good
set.seed(112)
jest <- jester_comp[sample(nrow(jester_comp),5000),]
jest <-scale(jest)
jest <- jest[,-1]

set.seed(123)
result_ward.D_euclidean <- NbClust(data = jest, distance = "euclidean", min.nc = 2, 
                                   max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.1)
set.seed(6747)
result_kmeans_euclidean <- NbClust(data = jest, distance = "euclidean", min.nc = 2, 
                                   max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)


result_ward.D_euclidean$Best.nc
result_kmeans_euclidean$Best.nc
#K-Means for regular data
jester_data <- jest
results_evaluation_jester <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_jester) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
k_means_nodr_jester = kmeans(jester_data,centers = 3)
clusts_nodr_jester <- k_means_nodr_jester$cluster
silhouette_k_means_jester_nodr <- silhouette(clusts_nodr_jester, dist(jester_data))
silhouette_k_means_jester_nodr <- as.matrix(silhouette_k_means_jester_nodr)
silhouette_k_means_jester_nodr <- silhouette_k_means_jester_nodr[,3]
silhouette_k_means_jester_nodr <- mean(silhouette_k_means_jester_nodr)

# Dunn index
dunn_k_means_jester_nodr <- dunn( distance = NULL, clusts_nodr_jester, Data = jester_data)

# Calinski-Harabasz index
calinski_k_means_jester_nodr <-   calinhara(jester_data,clusts_nodr_jester,cn=max(clusts_nodr_jester))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester_nodr <-index.DB(jester_data, clusts_nodr_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[1,] <- c("K-means NoDR", silhouette_k_means_jester_nodr, 
                               dunn_k_means_jester_nodr, calinski_k_means_jester_nodr,
                               davies_k_means_jester_nodr)

fviz_cluster(k_means_nodr_jester,  data = jester_data,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR  Jester"
)
#Method with No DR and AGNES
set.seed(123)
dist_jester_nodr <- dist(jester_data, method= "euclidean")
agnes_jester_nodr <- hclust(dist_jester_nodr, method = "ward.D")
cut_agnes_jester_nodr <- cutree(agnes_jester_nodr, k = 3)


silhouette_agnes_jester_nodr <- silhouette(cut_agnes_jester_nodr, dist(jester_data))
silhouette_agnes_jester_nodr <- as.matrix(silhouette_agnes_jester_nodr)
silhouette_agnes_jester_nodr <- silhouette_agnes_jester_nodr[,3]
silhouette_agnes_jester_nodr <- mean(silhouette_agnes_jester_nodr)

dunn_agnes_jester_nodr <- dunn( dist(jester_data), cut_agnes_jester_nodr)
calinski_agnes_jester_nodr <-   calinhara(jester_data, cut_agnes_jester_nodr,cn=max(cut_agnes_jester_nodr))

davies_agnes_jester_nodr <-index.DB(jester_data, cut_agnes_jester_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[2,] <- c("AGNES NoDR", silhouette_agnes_jester_nodr, 
                               dunn_agnes_jester_nodr, calinski_agnes_jester_nodr,
                               davies_agnes_jester_nodr)


fviz_cluster(list(data = jester_data, cluster =cut_agnes_jester_nodr ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR  Jester"
)



#pca jester
set.seed(123)

jester_pca = prcomp(jester_data)
jester_pca = jester_pca$x[,1:2]
#kmeans
set.seed(123)

k_means_pca_jester = kmeans(jester_pca,centers = 3)
clusters_pca_jester <- k_means_pca_jester$cluster
silhouette_k_means_jester_pca <- silhouette(clusters_pca_jester, dist(jester_pca))
silhouette_k_means_jester_pca <- as.matrix(silhouette_k_means_jester_pca)
silhouette_k_means_jester_pca <- silhouette_k_means_jester_pca[,3]
silhouette_k_means_jester_pca <- mean(silhouette_k_means_jester_pca)

# Dunn index
dunn_k_means_jester_pca <- dunn( distance = NULL, clusters_pca_jester, Data = jester_pca)

# Calinski-Harabasz index
calinski_k_means_jester_pca <-   calinhara(jester_pca,clusters_pca_jester,cn=max(clusters_pca_jester))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester_pca <-index.DB(jester_pca, clusters_pca_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[3,] <- c("k-means PCA", silhouette_k_means_jester_pca, 
                               dunn_k_means_jester_pca, calinski_k_means_jester_pca,
                               davies_k_means_jester_pca)

fviz_cluster(k_means_pca_jester,  data = jester_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA  Jester"
)

#AGNES with PCA
set.seed(123)


dist_jester_pca <- dist(jester_pca, method= "euclidean")
agnes_jester_pca  <- hclust(dist_jester_pca, method = "ward.D" )
cut_agnes_jester_pca <- cutree(agnes_jester_pca, k = 3)

silhouette_agnes_jester_pca <- silhouette(cut_agnes_jester_pca, dist(jester_pca))
silhouette_agnes_jester_pca <- as.matrix(silhouette_agnes_jester_pca)
silhouette_agnes_jester_pca <- silhouette_agnes_jester_pca[,3]
silhouette_agnes_jester_pca <- mean(silhouette_agnes_jester_pca)

dunn_agnes_jester_pca <- dunn( dist(jester_pca), cut_agnes_jester_pca)
calinski_agnes_jester_pca <-   calinhara(jester_pca, cut_agnes_jester_pca,cn=max(cut_agnes_jester_pca))

davies_agnes_jester_pca <-index.DB(jester_pca, cut_agnes_jester_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester[4,] <- c("AGNES PCA", silhouette_agnes_jester_pca, 
                               dunn_agnes_jester_pca, calinski_agnes_jester_pca,
                               davies_agnes_jester_pca)


fviz_cluster(list(data = jester_pca, cluster =cut_agnes_jester_pca ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA  Jester"
)

#tsne
set.seed(123)

tsne_jester <- Rtsne(jester_data, dims = 2)
tsne_jester <- tsne_jester$Y
#k-means with t-sne
set.seed(123)

k_means_tsne_jester = kmeans(tsne_jester,centers = 3)
clusters_tsne_jester <- k_means_tsne_jester$cluster

silhouette_k_means_jester_tsne <- silhouette(clusters_tsne_jester, dist(tsne_jester))
silhouette_k_means_jester_tsne <- as.matrix(silhouette_k_means_jester_tsne)
silhouette_k_means_jester_tsne <- silhouette_k_means_jester_tsne[,3]
silhouette_k_means_jester_tsne <- mean(silhouette_k_means_jester_tsne)

# Dunn index
dunn_k_means_jester_tsne <- dunn( distance = NULL, clusters_tsne_jester, Data = tsne_jester)

# Calinski-Harabasz index
calinski_k_means_jester_tsne <-   calinhara(tsne_jester,clusters_tsne_jester,cn=max(clusters_tsne_jester))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester_tsne <-index.DB(tsne_jester, clusters_tsne_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[5,] <- c("k-means t-SNE", silhouette_k_means_jester_tsne, 
                               dunn_k_means_jester_tsne, calinski_k_means_jester_tsne,
                               davies_k_means_jester_tsne)

fviz_cluster(k_means_tsne_jester,  data = tsne_jester,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE  Jester",
             xlab="V1", ylab = "V2"
)

#agnes t-sne
set.seed(123)


dist_jester_tsne <- dist(tsne_jester, method= "euclidean")
agnes_jester_tsne  <- hclust(dist_jester_tsne, method = "ward.D" )
cut_agnes_jester_tsne <- cutree(agnes_jester_tsne, k = 3)


silhouette_agnes_jester_tsne <- silhouette(cut_agnes_jester_tsne, dist(tsne_jester))
silhouette_agnes_jester_tsne <- as.matrix(silhouette_agnes_jester_tsne)
silhouette_agnes_jester_tsne <- silhouette_agnes_jester_tsne[,3]
silhouette_agnes_jester_tsne <- mean(silhouette_agnes_jester_tsne)

dunn_agnes_jester_tsne <- dunn( dist(tsne_jester), cut_agnes_jester_tsne)
calinski_agnes_jester_tsne <-   calinhara(tsne_jester, cut_agnes_jester_tsne,cn=max(cut_agnes_jester_tsne))

davies_agnes_jester_tsne <-index.DB(tsne_jester, cut_agnes_jester_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[6,] <- c("AGNES t-SNE", silhouette_agnes_jester_tsne, 
                               dunn_agnes_jester_tsne, calinski_agnes_jester_tsne,
                               davies_agnes_jester_tsne)

fviz_cluster(list(data = tsne_jester, cluster =cut_agnes_jester_tsne ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE  Jester",
             xlab="V1", ylab = "V2"
)


#umap
library(uwot)
set.seed(123)

umap_jester <- umap(jester_data, n_neighbors = 50, min_dist = 0.001, n_components = 2, 
                init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)

kmeans_jester_umap <- kmeans(umap_jester, centers = 3)

fviz_cluster(kmeans_jester_umap,  data = umap_jester,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP  Jester"
)

kmeans_cluster_jester_umap <- kmeans_jester_umap$cluster
silhouette_k_means_jester_umap <- silhouette(kmeans_cluster_jester_umap, dist(umap_jester))
silhouette_k_means_jester_umap <- as.matrix(silhouette_k_means_jester_umap)
silhouette_k_means_jester_umap <- silhouette_k_means_jester_umap[,3]
silhouette_k_means_jester_umap <- mean(silhouette_k_means_jester_umap)

# Dunn index
dunn_k_means_jester_umap <- dunn( distance = NULL, kmeans_cluster_jester_umap, Data = umap_jester)


# Calinski-Harabasz index
calinski_k_means_jester_umap <-   calinhara(umap_jester,kmeans_cluster_jester_umap,cn=max(kmeans_cluster_jester_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester_umap <-index.DB(umap_jester,kmeans_cluster_jester_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[7,] <- c("k-means UMAP", silhouette_k_means_jester_umap, 
                               dunn_k_means_jester_umap, calinski_k_means_jester_umap,
                               davies_k_means_jester_umap)

#Agnes with UMAP 
set.seed(123)


dist_jester_umap <- dist(umap_jester, method= "euclidean")
agnes_cluster_umap_jester  <- hclust(dist_jester_umap, method = "ward.D" )
cut_agnes_umap_jester <- cutree(agnes_cluster_umap_jester, k = 3)

fviz_cluster(list(data = umap_jester, cluster = cut_agnes_umap_jester),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP  Jester"
)
#scatterplot3d(jester_data, color=rainbow(4)[cut_agnes_umap_jester], main = "agnes after UMAP jester")


silhouette_agnes_jester_umap <- silhouette(cut_agnes_umap_jester, dist(umap_jester))
silhouette_agnes_jester_umap <- as.matrix(silhouette_agnes_jester_umap)
silhouette_agnes_jester_umap <- silhouette_agnes_jester_umap[,3]
silhouette_agnes_jester_umap<- mean(silhouette_agnes_jester_umap)


dunn_agnes_jester_umap <- dunn( distance = NULL, cut_agnes_umap_jester, Data = umap_jester)


# Calinski-Harabasz index
calinski_agnes_jester_umap <-   calinhara(umap_jester,cut_agnes_umap_jester,cn=max(cut_agnes_umap_jester))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_jester_umap <-index.DB(umap_jester, cut_agnes_umap_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester[8,] <- c("AGNES UMAP", silhouette_agnes_jester_umap, 
                               dunn_agnes_jester_umap, calinski_agnes_jester_umap,
                               davies_agnes_jester_umap)






# Jester with 2 Clusters
set.seed(123)
results_evaluation_jester2 <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_jester) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
jester2_data <- jester_data
k_means_nodr_jester2 = kmeans(jester2_data,centers = 2)
clusts_nodr_jester2 <- k_means_nodr_jester2$cluster
silhouette_k_means_jester2_nodr <- silhouette(clusts_nodr_jester2, dist(jester2_data))
silhouette_k_means_jester2_nodr <- as.matrix(silhouette_k_means_jester2_nodr)
silhouette_k_means_jester2_nodr <- silhouette_k_means_jester2_nodr[,3]
silhouette_k_means_jester2_nodr <- mean(silhouette_k_means_jester2_nodr)

# Dunn index
dunn_k_means_jester2_nodr <- dunn( distance = NULL, clusts_nodr_jester2, Data = jester2_data)

# Calinski-Harabasz index
calinski_k_means_jester2_nodr <-   calinhara(jester2_data,clusts_nodr_jester2,cn=max(clusts_nodr_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_nodr <-index.DB(jester2_data, clusts_nodr_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[1,] <- c("K-means NoDR", silhouette_k_means_jester2_nodr, 
                                   dunn_k_means_jester2_nodr, calinski_k_means_jester2_nodr,
                                   davies_k_means_jester2_nodr)

fviz_cluster(k_means_nodr_jester2,  data = jester2_data,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR Jester with 2 Clusters"
)
#Method with No DR and AGNES
set.seed(123)
dist_jester2_nodr <- dist(jester2_data, method= "euclidean")
agnes_jester2_nodr <- hclust(dist_jester2_nodr, method = "ward.D")
cut_agnes_jester2_nodr <- cutree(agnes_jester2_nodr, k = 2)


silhouette_agnes_jester2_nodr <- silhouette(cut_agnes_jester2_nodr, dist(jester2_data))
silhouette_agnes_jester2_nodr <- as.matrix(silhouette_agnes_jester2_nodr)
silhouette_agnes_jester2_nodr <- silhouette_agnes_jester2_nodr[,3]
silhouette_agnes_jester2_nodr <- mean(silhouette_agnes_jester2_nodr)

dunn_agnes_jester2_nodr <- dunn( dist(jester2_data), cut_agnes_jester2_nodr)
calinski_agnes_jester2_nodr <-   calinhara(jester2_data, cut_agnes_jester2_nodr,cn=max(cut_agnes_jester2_nodr))

davies_agnes_jester2_nodr <-index.DB(jester2_data, cut_agnes_jester2_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[2,] <- c("AGNES NoDR", silhouette_agnes_jester2_nodr, 
                                   dunn_agnes_jester2_nodr, calinski_agnes_jester2_nodr,
                                   davies_agnes_jester2_nodr)


fviz_cluster(list(data = jester2_data, cluster =cut_agnes_jester2_nodr ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR Jester with 2 Clusters"
)



#pca jester2
set.seed(123)

jester2_pca = prcomp(jester2_data)
jester2_pca = jester2_pca$x[,1:2]
#kmeans
set.seed(123)

k_means_pca_jester2 = kmeans(jester2_pca,centers = 2)
clusters_pca_jester2 <- k_means_pca_jester2$cluster
silhouette_k_means_jester2_pca <- silhouette(clusters_pca_jester2, dist(jester2_pca))
silhouette_k_means_jester2_pca <- as.matrix(silhouette_k_means_jester2_pca)
silhouette_k_means_jester2_pca <- silhouette_k_means_jester2_pca[,3]
silhouette_k_means_jester2_pca <- mean(silhouette_k_means_jester2_pca)

# Dunn index
dunn_k_means_jester2_pca <- dunn( distance = NULL, clusters_pca_jester2, Data = jester2_pca)

# Calinski-Harabasz index
calinski_k_means_jester2_pca <-   calinhara(jester2_pca,clusters_pca_jester2,cn=max(clusters_pca_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_pca <-index.DB(jester2_pca, clusters_pca_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[3,] <- c("k-means PCA", silhouette_k_means_jester2_pca, 
                                   dunn_k_means_jester2_pca, calinski_k_means_jester2_pca,
                                   davies_k_means_jester2_pca)

fviz_cluster(k_means_pca_jester2,  data = jester2_pca,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA Jester with 2 Clusters"
)

#AGNES with PCA
set.seed(123)


dist_jester2_pca <- dist(jester2_pca, method= "euclidean")
agnes_jester2_pca  <- hclust(dist_jester2_pca, method = "ward.D" )
cut_agnes_jester2_pca <- cutree(agnes_jester2_pca, k = 2)

silhouette_agnes_jester2_pca <- silhouette(cut_agnes_jester2_pca, dist(jester2_pca))
silhouette_agnes_jester2_pca <- as.matrix(silhouette_agnes_jester2_pca)
silhouette_agnes_jester2_pca <- silhouette_agnes_jester2_pca[,3]
silhouette_agnes_jester2_pca <- mean(silhouette_agnes_jester2_pca)

dunn_agnes_jester2_pca <- dunn( dist(jester2_pca), cut_agnes_jester2_pca)
calinski_agnes_jester2_pca <-   calinhara(jester2_pca, cut_agnes_jester2_pca,cn=max(cut_agnes_jester2_pca))

davies_agnes_jester2_pca <-index.DB(jester2_pca, cut_agnes_jester2_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester2[4,] <- c("AGNES PCA", silhouette_agnes_jester2_pca, 
                                   dunn_agnes_jester2_pca, calinski_agnes_jester2_pca,
                                   davies_agnes_jester2_pca)


fviz_cluster(list(data = jester2_pca, cluster =cut_agnes_jester2_pca ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA Jester with 2 Clusters"
)

#tsne
set.seed(123)

tsne_jester2 <- Rtsne(jester2_data, dims = 2)
tsne_jester2 <- tsne_jester2$Y
#k-means with t-sne
set.seed(123)

k_means_tsne_jester2 = kmeans(tsne_jester2,centers = 2)
clusters_tsne_jester2 <- k_means_tsne_jester2$cluster

silhouette_k_means_jester2_tsne <- silhouette(clusters_tsne_jester2, dist(tsne_jester2))
silhouette_k_means_jester2_tsne <- as.matrix(silhouette_k_means_jester2_tsne)
silhouette_k_means_jester2_tsne <- silhouette_k_means_jester2_tsne[,3]
silhouette_k_means_jester2_tsne <- mean(silhouette_k_means_jester2_tsne)

# Dunn index
dunn_k_means_jester2_tsne <- dunn( distance = NULL, clusters_tsne_jester2, Data = tsne_jester2)

# Calinski-Harabasz index
calinski_k_means_jester2_tsne <-   calinhara(tsne_jester2,clusters_tsne_jester2,cn=max(clusters_tsne_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_tsne <-index.DB(tsne_jester2, clusters_tsne_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[5,] <- c("k-means t-SNE", silhouette_k_means_jester2_tsne, 
                                   dunn_k_means_jester2_tsne, calinski_k_means_jester2_tsne,
                                   davies_k_means_jester2_tsne)

fviz_cluster(k_means_tsne_jester2,  data = tsne_jester2,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE Jester with 2 Clusters",
             xlab="V1", ylab = "V2"
)

#agnes t-sne
set.seed(123)


dist_jester2_tsne <- dist(tsne_jester2, method= "euclidean")
agnes_jester2_tsne  <- hclust(dist_jester2_tsne, method = "ward.D" )
cut_agnes_jester2_tsne <- cutree(agnes_jester2_tsne, k = 2)


silhouette_agnes_jester2_tsne <- silhouette(cut_agnes_jester2_tsne, dist(tsne_jester2))
silhouette_agnes_jester2_tsne <- as.matrix(silhouette_agnes_jester2_tsne)
silhouette_agnes_jester2_tsne <- silhouette_agnes_jester2_tsne[,3]
silhouette_agnes_jester2_tsne <- mean(silhouette_agnes_jester2_tsne)

dunn_agnes_jester2_tsne <- dunn( dist(tsne_jester2), cut_agnes_jester2_tsne)
calinski_agnes_jester2_tsne <-   calinhara(tsne_jester2, cut_agnes_jester2_tsne,cn=max(cut_agnes_jester2_tsne))

davies_agnes_jester2_tsne <-index.DB(tsne_jester2, cut_agnes_jester2_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[6,] <- c("AGNES t-SNE", silhouette_agnes_jester2_tsne, 
                                   dunn_agnes_jester2_tsne, calinski_agnes_jester2_tsne,
                                   davies_agnes_jester2_tsne)

fviz_cluster(list(data = tsne_jester2, cluster =cut_agnes_jester2_tsne ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE Jester with 2 Clusters",
             xlab="V1", ylab = "V2"
)


#umap
library(uwot)
set.seed(123)

umap_jester2 <- umap(jester2_data, n_neighbors = 50, min_dist = 0.001, n_components = 2, 
                    init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)

kmeans_jester2_umap <- kmeans(umap_jester2, centers = 2)

fviz_cluster(kmeans_jester2_umap,  data = umap_jester2,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP Jester with 2 Clusters"
)

kmeans_cluster_jester2_umap <- kmeans_jester2_umap$cluster
silhouette_k_means_jester2_umap <- silhouette(kmeans_cluster_jester2_umap, dist(umap_jester2))
silhouette_k_means_jester2_umap <- as.matrix(silhouette_k_means_jester2_umap)
silhouette_k_means_jester2_umap <- silhouette_k_means_jester2_umap[,3]
silhouette_k_means_jester2_umap <- mean(silhouette_k_means_jester2_umap)

# Dunn index
dunn_k_means_jester2_umap <- dunn( distance = NULL, kmeans_cluster_jester2_umap, Data = umap_jester2)


# Calinski-Harabasz index
calinski_k_means_jester2_umap <-   calinhara(umap_jester2,kmeans_cluster_jester2_umap,cn=max(kmeans_cluster_jester2_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_umap <-index.DB(umap_jester2,kmeans_cluster_jester2_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[7,] <- c("k-means UMAP", silhouette_k_means_jester2_umap, 
                                   dunn_k_means_jester2_umap, calinski_k_means_jester2_umap,
                                   davies_k_means_jester2_umap)

#Agnes with UMAP 
set.seed(123)


dist_jester2_umap <- dist(umap_jester2, method= "euclidean")
agnes_cluster_umap_jester2  <- hclust(dist_jester2_umap, method = "ward.D" )
cut_agnes_umap_jester2 <- cutree(agnes_cluster_umap_jester2, k = 2)

fviz_cluster(list(data = umap_jester2, cluster = cut_agnes_umap_jester2),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP Jester with 2 Clusters"
)
#scatterplot3d(jester2_data, color=rainbow(4)[cut_agnes_umap_jester2], main = "agnes after UMAP jester2")


silhouette_agnes_jester2_umap <- silhouette(cut_agnes_umap_jester2, dist(umap_jester2))
silhouette_agnes_jester2_umap <- as.matrix(silhouette_agnes_jester2_umap)
silhouette_agnes_jester2_umap <- silhouette_agnes_jester2_umap[,3]
silhouette_agnes_jester2_umap<- mean(silhouette_agnes_jester2_umap)


dunn_agnes_jester2_umap <- dunn( distance = NULL, cut_agnes_umap_jester2, Data = umap_jester2)


# Calinski-Harabasz index
calinski_agnes_jester2_umap <-   calinhara(umap_jester2,cut_agnes_umap_jester2,cn=max(cut_agnes_umap_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_jester2_umap <-index.DB(umap_jester2, cut_agnes_umap_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester2[8,] <- c("AGNES UMAP", silhouette_agnes_jester2_umap, 
                                   dunn_agnes_jester2_umap, calinski_agnes_jester2_umap,
                                   davies_agnes_jester2_umap)

jester2_data <- jester_data
k_means_nodr_jester2 = kmeans(jester2_data,centers = 2)
clusts_nodr_jester2 <- k_means_nodr_jester2$cluster
silhouette_k_means_jester2_nodr <- silhouette(clusts_nodr_jester2, dist(jester2_data))
silhouette_k_means_jester2_nodr <- as.matrix(silhouette_k_means_jester2_nodr)
silhouette_k_means_jester2_nodr <- silhouette_k_means_jester2_nodr[,3]
silhouette_k_means_jester2_nodr <- mean(silhouette_k_means_jester2_nodr)

# Dunn index
dunn_k_means_jester2_nodr <- dunn( distance = NULL, clusts_nodr_jester2, Data = jester2_data)

# Calinski-Harabasz index
calinski_k_means_jester2_nodr <-   calinhara(jester2_data,clusts_nodr_jester2,cn=max(clusts_nodr_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_nodr <-index.DB(jester2_data, clusts_nodr_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[1,] <- c("K-means NoDR", silhouette_k_means_jester2_nodr, 
                                    dunn_k_means_jester2_nodr, calinski_k_means_jester2_nodr,
                                    davies_k_means_jester2_nodr)

fviz_cluster(k_means_nodr_jester2,  data = jester2_data,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR Jester with 2 Clusters"
)
#Method with No DR and AGNES
set.seed(123)
dist_jester2_nodr <- dist(jester2_data, method= "euclidean")
agnes_jester2_nodr <- hclust(dist_jester2_nodr, method = "ward.D")
cut_agnes_jester2_nodr <- cutree(agnes_jester2_nodr, k = 2)


silhouette_agnes_jester2_nodr <- silhouette(cut_agnes_jester2_nodr, dist(jester2_data))
silhouette_agnes_jester2_nodr <- as.matrix(silhouette_agnes_jester2_nodr)
silhouette_agnes_jester2_nodr <- silhouette_agnes_jester2_nodr[,3]
silhouette_agnes_jester2_nodr <- mean(silhouette_agnes_jester2_nodr)

dunn_agnes_jester2_nodr <- dunn( dist(jester2_data), cut_agnes_jester2_nodr)
calinski_agnes_jester2_nodr <-   calinhara(jester2_data, cut_agnes_jester2_nodr,cn=max(cut_agnes_jester2_nodr))

davies_agnes_jester2_nodr <-index.DB(jester2_data, cut_agnes_jester2_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[2,] <- c("AGNES NoDR", silhouette_agnes_jester2_nodr, 
                                    dunn_agnes_jester2_nodr, calinski_agnes_jester2_nodr,
                                    davies_agnes_jester2_nodr)


fviz_cluster(list(data = jester2_data, cluster =cut_agnes_jester2_nodr ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR Jester with 2 Clusters"
)



#pca jester2
set.seed(123)

jester2_pca = prcomp(jester2_data)
jester2_pca = jester2_pca$x[,1:2]
#kmeans
set.seed(123)

k_means_pca_jester2 = kmeans(jester2_pca,centers = 2)
clusters_pca_jester2 <- k_means_pca_jester2$cluster
silhouette_k_means_jester2_pca <- silhouette(clusters_pca_jester2, dist(jester2_pca))
silhouette_k_means_jester2_pca <- as.matrix(silhouette_k_means_jester2_pca)
silhouette_k_means_jester2_pca <- silhouette_k_means_jester2_pca[,3]
silhouette_k_means_jester2_pca <- mean(silhouette_k_means_jester2_pca)

# Dunn index
dunn_k_means_jester2_pca <- dunn( distance = NULL, clusters_pca_jester2, Data = jester2_pca)

# Calinski-Harabasz index
calinski_k_means_jester2_pca <-   calinhara(jester2_pca,clusters_pca_jester2,cn=max(clusters_pca_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_pca <-index.DB(jester2_pca, clusters_pca_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[3,] <- c("k-means PCA", silhouette_k_means_jester2_pca, 
                                    dunn_k_means_jester2_pca, calinski_k_means_jester2_pca,
                                    davies_k_means_jester2_pca)

fviz_cluster(k_means_pca_jester2,  data = jester2_pca,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA Jester with 2 Clusters"
)

#AGNES with PCA
set.seed(123)


dist_jester2_pca <- dist(jester2_pca, method= "euclidean")
agnes_jester2_pca  <- hclust(dist_jester2_pca, method = "ward.D" )
cut_agnes_jester2_pca <- cutree(agnes_jester2_pca, k = 2)

silhouette_agnes_jester2_pca <- silhouette(cut_agnes_jester2_pca, dist(jester2_pca))
silhouette_agnes_jester2_pca <- as.matrix(silhouette_agnes_jester2_pca)
silhouette_agnes_jester2_pca <- silhouette_agnes_jester2_pca[,3]
silhouette_agnes_jester2_pca <- mean(silhouette_agnes_jester2_pca)

dunn_agnes_jester2_pca <- dunn( dist(jester2_pca), cut_agnes_jester2_pca)
calinski_agnes_jester2_pca <-   calinhara(jester2_pca, cut_agnes_jester2_pca,cn=max(cut_agnes_jester2_pca))

davies_agnes_jester2_pca <-index.DB(jester2_pca, cut_agnes_jester2_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester2[4,] <- c("AGNES PCA", silhouette_agnes_jester2_pca, 
                                    dunn_agnes_jester2_pca, calinski_agnes_jester2_pca,
                                    davies_agnes_jester2_pca)


fviz_cluster(list(data = jester2_pca, cluster =cut_agnes_jester2_pca ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA Jester with 2 Clusters"
)

#tsne
set.seed(123)

tsne_jester2 <- Rtsne(jester2_data, dims = 2)
tsne_jester2 <- tsne_jester2$Y
#k-means with t-sne
set.seed(123)

k_means_tsne_jester2 = kmeans(tsne_jester2,centers = 2)
clusters_tsne_jester2 <- k_means_tsne_jester2$cluster

silhouette_k_means_jester2_tsne <- silhouette(clusters_tsne_jester2, dist(tsne_jester2))
silhouette_k_means_jester2_tsne <- as.matrix(silhouette_k_means_jester2_tsne)
silhouette_k_means_jester2_tsne <- silhouette_k_means_jester2_tsne[,3]
silhouette_k_means_jester2_tsne <- mean(silhouette_k_means_jester2_tsne)

# Dunn index
dunn_k_means_jester2_tsne <- dunn( distance = NULL, clusters_tsne_jester2, Data = tsne_jester2)

# Calinski-Harabasz index
calinski_k_means_jester2_tsne <-   calinhara(tsne_jester2,clusters_tsne_jester2,cn=max(clusters_tsne_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_tsne <-index.DB(tsne_jester2, clusters_tsne_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[5,] <- c("k-means t-SNE", silhouette_k_means_jester2_tsne, 
                                    dunn_k_means_jester2_tsne, calinski_k_means_jester2_tsne,
                                    davies_k_means_jester2_tsne)

fviz_cluster(k_means_tsne_jester2,  data = tsne_jester2,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE Jester with 2 Clusters",
             xlab="V1", ylab = "V2"
)

#agnes t-sne
set.seed(123)


dist_jester2_tsne <- dist(tsne_jester2, method= "euclidean")
agnes_jester2_tsne  <- hclust(dist_jester2_tsne, method = "ward.D" )
cut_agnes_jester2_tsne <- cutree(agnes_jester2_tsne, k = 2)


silhouette_agnes_jester2_tsne <- silhouette(cut_agnes_jester2_tsne, dist(tsne_jester2))
silhouette_agnes_jester2_tsne <- as.matrix(silhouette_agnes_jester2_tsne)
silhouette_agnes_jester2_tsne <- silhouette_agnes_jester2_tsne[,3]
silhouette_agnes_jester2_tsne <- mean(silhouette_agnes_jester2_tsne)

dunn_agnes_jester2_tsne <- dunn( dist(tsne_jester2), cut_agnes_jester2_tsne)
calinski_agnes_jester2_tsne <-   calinhara(tsne_jester2, cut_agnes_jester2_tsne,cn=max(cut_agnes_jester2_tsne))

davies_agnes_jester2_tsne <-index.DB(tsne_jester2, cut_agnes_jester2_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[6,] <- c("AGNES t-SNE", silhouette_agnes_jester2_tsne, 
                                    dunn_agnes_jester2_tsne, calinski_agnes_jester2_tsne,
                                    davies_agnes_jester2_tsne)

fviz_cluster(list(data = tsne_jester2, cluster =cut_agnes_jester2_tsne ),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE Jester with 2 Clusters",
             xlab="V1", ylab = "V2"
)


#umap
library(uwot)
set.seed(123)

umap_jester2 <- umap(jester2_data, n_neighbors = 50, min_dist = 0.001, n_components = 2, 
                     init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)

kmeans_jester2_umap <- kmeans(umap_jester2, centers = 2)

fviz_cluster(kmeans_jester2_umap,  data = umap_jester2,
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP Jester with 2 Clusters"
)

kmeans_cluster_jester2_umap <- kmeans_jester2_umap$cluster
silhouette_k_means_jester2_umap <- silhouette(kmeans_cluster_jester2_umap, dist(umap_jester2))
silhouette_k_means_jester2_umap <- as.matrix(silhouette_k_means_jester2_umap)
silhouette_k_means_jester2_umap <- silhouette_k_means_jester2_umap[,3]
silhouette_k_means_jester2_umap <- mean(silhouette_k_means_jester2_umap)

# Dunn index
dunn_k_means_jester2_umap <- dunn( distance = NULL, kmeans_cluster_jester2_umap, Data = umap_jester2)


# Calinski-Harabasz index
calinski_k_means_jester2_umap <-   calinhara(umap_jester2,kmeans_cluster_jester2_umap,cn=max(kmeans_cluster_jester2_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_jester2_umap <-index.DB(umap_jester2,kmeans_cluster_jester2_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester2[7,] <- c("k-means UMAP", silhouette_k_means_jester2_umap, 
                                    dunn_k_means_jester2_umap, calinski_k_means_jester2_umap,
                                    davies_k_means_jester2_umap)

#Agnes with UMAP 
set.seed(123)


dist_jester2_umap <- dist(umap_jester2, method= "euclidean")
agnes_cluster_umap_jester2  <- hclust(dist_jester2_umap, method = "ward.D" )
cut_agnes_umap_jester2 <- cutree(agnes_cluster_umap_jester2, k = 2)

fviz_cluster(list(data = umap_jester2, cluster = cut_agnes_umap_jester2),
             palette = c("#FF0000", "#FFFF00"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP Jester with 2 Clusters"
)
#scatterplot3d(jester2_data, color=rainbow(4)[cut_agnes_umap_jester2], main = "agnes after UMAP jester2")


silhouette_agnes_jester2_umap <- silhouette(cut_agnes_umap_jester2, dist(umap_jester2))
silhouette_agnes_jester2_umap <- as.matrix(silhouette_agnes_jester2_umap)
silhouette_agnes_jester2_umap <- silhouette_agnes_jester2_umap[,3]
silhouette_agnes_jester2_umap<- mean(silhouette_agnes_jester2_umap)


dunn_agnes_jester2_umap <- dunn( distance = NULL, cut_agnes_umap_jester2, Data = umap_jester2)


# Calinski-Harabasz index
calinski_agnes_jester2_umap <-   calinhara(umap_jester2,cut_agnes_umap_jester2,cn=max(cut_agnes_umap_jester2))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_jester2_umap <-index.DB(umap_jester2, cut_agnes_umap_jester2, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester2[8,] <- c("AGNES UMAP", silhouette_agnes_jester2_umap, 
                                    dunn_agnes_jester2_umap, calinski_agnes_jester2_umap,
                                    davies_agnes_jester2_umap)



