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
library(Rtsne)
library(ggpubr)

#Load Data & Adjust the path to jester excel files!
jester_data_1 <- read_excel("C:/Users/ankam/OneDrive/Desktop/jester-data-1.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester_data_2 <- read_excel("C:/Users/ankam/OneDrive/Desktop/jester-data-2.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester_data_3 <- read_excel("C:/Users/ankam/OneDrive/Desktop/jester-data-3.xls",
                            col_names = FALSE, na = c("99", "99.00"))

jester <- rbind(jester_data_1,  jester_data_2, jester_data_3)
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

clust_num_jester <- 3

#Method without DR and k-means
results_evaluation_jester <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_jester) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
k_means_nodr_jester = kmeans(jester_data,centers  = clust_num_jester)
clusts_nodr_jester <- k_means_nodr_jester$cluster
silhouette_k_means_jester_nodr <- silhouette(clusts_nodr_jester, dist(jester_data))
silhouette_k_means_jester_nodr <- as.matrix(silhouette_k_means_jester_nodr)
silhouette_k_means_jester_nodr <- silhouette_k_means_jester_nodr[,3]
silhouette_k_means_jester_nodr <- mean(silhouette_k_means_jester_nodr)

# Dunn index
dunn_k_means_jester_nodr <- dunn( distance = NULL, clusts_nodr_jester, Data = jester_data, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_jester_nodr <-   calinhara(jester_data,clusts_nodr_jester,cn=max(clusts_nodr_jester))


# Davies-Bouldin index

davies_k_means_jester_nodr <-index.DB(jester_data, clusts_nodr_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[1,] <- c("K-means NoDR", silhouette_k_means_jester_nodr, 
                               dunn_k_means_jester_nodr, calinski_k_means_jester_nodr,
                               davies_k_means_jester_nodr)

#Method with No DR and AGNES
set.seed(123)
dist_jester_nodr <- dist(jester_data, method= "euclidean")
agnes_jester_nodr <- hclust(dist_jester_nodr, method = "ward.D2" )
cut_agnes_jester_nodr <- cutree(agnes_jester_nodr, k  = clust_num_jester)


silhouette_agnes_jester_nodr <- silhouette(cut_agnes_jester_nodr, dist(jester_data))
silhouette_agnes_jester_nodr <- as.matrix(silhouette_agnes_jester_nodr)
silhouette_agnes_jester_nodr <- silhouette_agnes_jester_nodr[,3]
silhouette_agnes_jester_nodr <- mean(silhouette_agnes_jester_nodr)

dunn_agnes_jester_nodr <- dunn( distance = NULL, Data = jester_data, cut_agnes_jester_nodr, method ="euclidean")
calinski_agnes_jester_nodr <-   calinhara(jester_data, cut_agnes_jester_nodr,cn=max(cut_agnes_jester_nodr))

davies_agnes_jester_nodr <-index.DB(jester_data, cut_agnes_jester_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[2,] <- c("AGNES NoDR", silhouette_agnes_jester_nodr, 
                               dunn_agnes_jester_nodr, calinski_agnes_jester_nodr,
                               davies_agnes_jester_nodr)


#pca jester
set.seed(123)
jester_pca = prcomp(jester_data)
jester_pca = jester_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_jester = kmeans(jester_pca,centers  = clust_num_jester)
clusters_pca_jester <- k_means_pca_jester$cluster

silhouette_k_means_jester_pca <- silhouette(clusters_pca_jester, dist(jester_pca))
silhouette_k_means_jester_pca <- as.matrix(silhouette_k_means_jester_pca)
silhouette_k_means_jester_pca <- silhouette_k_means_jester_pca[,3]
silhouette_k_means_jester_pca <- mean(silhouette_k_means_jester_pca)

# Dunn index
dunn_k_means_jester_pca <- dunn( distance = NULL, clusters_pca_jester, Data = jester_pca, method = "euclidean")

# Calinski-Harabasz index
calinski_k_means_jester_pca <-   calinhara(jester_pca,clusters_pca_jester,cn=max(clusters_pca_jester))


# Davies-Bouldin index
davies_k_means_jester_pca <-index.DB(jester_pca, clusters_pca_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[3,] <- c("k-means PCA", silhouette_k_means_jester_pca, 
                               dunn_k_means_jester_pca, calinski_k_means_jester_pca,
                               davies_k_means_jester_pca)

#AGNES with PCA
set.seed(123)
dist_jester_pca <- dist(jester_pca, method= "euclidean")
agnes_jester_pca  <- hclust(dist_jester_pca, method = "ward.D2" )
cut_agnes_jester_pca <- cutree(agnes_jester_pca, k  = clust_num_jester)

silhouette_agnes_jester_pca <- silhouette(cut_agnes_jester_pca, dist(jester_pca))
silhouette_agnes_jester_pca <- as.matrix(silhouette_agnes_jester_pca)
silhouette_agnes_jester_pca <- silhouette_agnes_jester_pca[,3]
silhouette_agnes_jester_pca <- mean(silhouette_agnes_jester_pca)

dunn_agnes_jester_pca <- dunn( distance = NULL, Data =jester_pca, cut_agnes_jester_pca, method = "euclidean")
calinski_agnes_jester_pca <-   calinhara(jester_pca, cut_agnes_jester_pca,cn=max(cut_agnes_jester_pca))

davies_agnes_jester_pca <-index.DB(jester_pca, cut_agnes_jester_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester[4,] <- c("AGNES PCA", silhouette_agnes_jester_pca, 
                               dunn_agnes_jester_pca, calinski_agnes_jester_pca,
                               davies_agnes_jester_pca)



#tsne
set.seed(123)
tsne_jester <- Rtsne(jester_data, dims = 2)
tsne_jester <- tsne_jester$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_jester = kmeans(tsne_jester,centers  = clust_num_jester)
clusters_tsne_jester <- k_means_tsne_jester$cluster

silhouette_k_means_jester_tsne <- silhouette(clusters_tsne_jester, dist(tsne_jester))
silhouette_k_means_jester_tsne <- as.matrix(silhouette_k_means_jester_tsne)
silhouette_k_means_jester_tsne <- silhouette_k_means_jester_tsne[,3]
silhouette_k_means_jester_tsne <- mean(silhouette_k_means_jester_tsne)

# Dunn index
dunn_k_means_jester_tsne <- dunn( distance = NULL, clusters_tsne_jester, Data = tsne_jester, method= "euclidean")

# Calinski-Harabasz index
calinski_k_means_jester_tsne <-   calinhara(tsne_jester,clusters_tsne_jester,cn=max(clusters_tsne_jester))


# Davies-Bouldin index

davies_k_means_jester_tsne <-index.DB(tsne_jester, clusters_tsne_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[5,] <- c("k-means t-SNE", silhouette_k_means_jester_tsne, 
                               dunn_k_means_jester_tsne, calinski_k_means_jester_tsne,
                               davies_k_means_jester_tsne)


#agnes t-sne
set.seed(123)
dist_jester_tsne <- dist(tsne_jester, method= "euclidean")
agnes_jester_tsne  <- hclust(dist_jester_tsne, method = "ward.D2" )
cut_agnes_jester_tsne <- cutree(agnes_jester_tsne, k  = clust_num_jester)


silhouette_agnes_jester_tsne <- silhouette(cut_agnes_jester_tsne, dist(tsne_jester))
silhouette_agnes_jester_tsne <- as.matrix(silhouette_agnes_jester_tsne)
silhouette_agnes_jester_tsne <- silhouette_agnes_jester_tsne[,3]
silhouette_agnes_jester_tsne <- mean(silhouette_agnes_jester_tsne)

dunn_agnes_jester_tsne <- dunn( distance = NULL, Data = tsne_jester, cut_agnes_jester_tsne, method = "euclidean")
calinski_agnes_jester_tsne <-   calinhara(tsne_jester, cut_agnes_jester_tsne,cn=max(cut_agnes_jester_tsne))

davies_agnes_jester_tsne <-index.DB(tsne_jester, cut_agnes_jester_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[6,] <- c("AGNES t-SNE", silhouette_agnes_jester_tsne, 
                               dunn_agnes_jester_tsne, calinski_agnes_jester_tsne,
                               davies_agnes_jester_tsne)


#umap
library(uwot)
set.seed(123)
umap_jester <- umap(jester_data, n_neighbors = 15, min_dist = 0.001, n_components = 2, 
                init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_jester_umap <- kmeans(umap_jester, centers  = clust_num_jester)

kmeans_cluster_jester_umap <- kmeans_jester_umap$cluster
silhouette_k_means_jester_umap <- silhouette(kmeans_cluster_jester_umap, dist(umap_jester))
silhouette_k_means_jester_umap <- as.matrix(silhouette_k_means_jester_umap)
silhouette_k_means_jester_umap <- silhouette_k_means_jester_umap[,3]
silhouette_k_means_jester_umap <- mean(silhouette_k_means_jester_umap)

# Dunn index
dunn_k_means_jester_umap <- dunn( distance = NULL, kmeans_cluster_jester_umap, Data = umap_jester, method ="euclidean")


# Calinski-Harabasz index
calinski_k_means_jester_umap <-   calinhara(umap_jester,kmeans_cluster_jester_umap,cn=max(kmeans_cluster_jester_umap))


# Davies-Bouldin index

davies_k_means_jester_umap <-index.DB(umap_jester,kmeans_cluster_jester_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_jester[7,] <- c("k-means UMAP", silhouette_k_means_jester_umap, 
                               dunn_k_means_jester_umap, calinski_k_means_jester_umap,
                               davies_k_means_jester_umap)

#Agnes with UMAP 
set.seed(123)
dist_jester_umap <- dist(umap_jester, method= "euclidean")
agnes_cluster_umap_jester  <- hclust(dist_jester_umap, method = "ward.D2" )
cut_agnes_umap_jester <- cutree(agnes_cluster_umap_jester, k  = clust_num_jester)


silhouette_agnes_jester_umap <- silhouette(cut_agnes_umap_jester, dist(umap_jester))
silhouette_agnes_jester_umap <- as.matrix(silhouette_agnes_jester_umap)
silhouette_agnes_jester_umap <- silhouette_agnes_jester_umap[,3]
silhouette_agnes_jester_umap<- mean(silhouette_agnes_jester_umap)


dunn_agnes_jester_umap <- dunn( distance = NULL, cut_agnes_umap_jester, Data = umap_jester, method = "euclidean")


# Calinski-Harabasz index
calinski_agnes_jester_umap <-   calinhara(umap_jester,cut_agnes_umap_jester,cn=max(cut_agnes_umap_jester))


# Davies-Bouldin index

davies_agnes_jester_umap <-index.DB(umap_jester, cut_agnes_umap_jester, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_jester[8,] <- c("AGNES UMAP", silhouette_agnes_jester_umap, 
                               dunn_agnes_jester_umap, calinski_agnes_jester_umap,
                               davies_agnes_jester_umap)





#Vizualizations

fviz_cluster(k_means_nodr_jester,  data = jester_data,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR  Jester"
)

fviz_cluster(list(data = jester_data, cluster =cut_agnes_jester_nodr ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with no DR  Jester"
)

fviz_cluster(k_means_pca_jester,  data = jester_pca,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA  Jester"
)

fviz_cluster(list(data = jester_pca, cluster =cut_agnes_jester_pca ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with PCA  Jester"
)

fviz_cluster(k_means_tsne_jester,  data = tsne_jester,
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE  Jester",
             xlab="V1", ylab = "V2"
)


fviz_cluster(list(data = tsne_jester, cluster =cut_agnes_jester_tsne ),
             palette = c("#FF0000", "#FFFF00", "#000FFF"
             ), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "AGNES with t-SNE  Jester",
             xlab="V1", ylab = "V2"
)


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

