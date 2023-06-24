library(clusterGeneration)
library(scatterplot3d)
library(kernlab)
library(plot3D)
library(Rtsne)
library(cluster)
library(IDmining)
library(NbClust)
library(stats)
library(fastcluster)
library(fpc)
library(cluster) 
library(plotrix)
library(factoextra)
library(clValid)
library(clusterSim)


generate_data_broken_swiss <- function(n, noise) {
  t1 <- c((3 * pi / 2) * (1 + 2 * runif(ceiling(n / 2)) * 0.4))
  t2 <- c((3 * pi / 2) * (1 + 2 * (runif(floor(n / 2)) * 0.4 + 0.6)))
  height1 <- 11 * runif(ceiling(n / 2)) 
  height2 <- 11 * runif(floor(n / 2))   # Increase the height of the second part
  
  t <- c(t1, t2)
  tm<- c(t1/2, t2*2)
  height <- c(height1, height2)
  O <- matrix(0, nrow = n, ncol = 3)
  X <- matrix(0, nrow = n, ncol = 3)
  X[, 1] <- tm * cos(t)
  X[, 2] <- height
  X[, 3] <- 3*tm * sin(t)
  
  X <-  scale(X)

  #condition_indices <- which(X[,2] > - 0.5 & X[,2] < 0.5)
  #X <- X[-condition_indices, ] 
  condition_indices2 <- which( X[, 3] > -0.8 & X[, 3] < 0.5 & X[, 1] > 0)
  X <- X[-condition_indices2, ] 
  # Remove points satisfying the condition
  # Resample points from the removed indices
  #numb = length(condition_indices)
  #resampled <- X[sample(nrow(X),numb),]
  numb2 = length(condition_indices2)
  resampled2 <- X[sample(nrow(X),numb2),]
  # Combine the remaining points and the resampled points
  X <- rbind(X, resampled2)
  X <- X + matrix(rnorm(3 * n), ncol = 3) * noise  # Increase the distance bebseen the bso Swiss rolls
  #X[, 2] <- X[, 2] *10

  data <- data.frame(X = X)
  return(data)
}

km <- kmeans(X, centers = 6)

plot(broken_swiss_roll16, col = kmeans_bsr_umap$cluster)

worst_db_a_bsr_3

set.seed(1408)
broken_swiss_roll <- generate_data_broken_swiss(5000, 0.01)
plot(broken_swiss_roll[,2], broken_swiss_roll[,3], pch = 16)

bs_data <- cbind(broken_swiss_roll$X.1, broken_swiss_roll$X.2, broken_swiss_roll$X.3)
colnames(bs_data) <- c("Dim1", "Dim2", "Dim3")


set.seed(87)
result_k_bs <- NbClust(data = broken_swiss_roll, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans", index  ="all", alphaBeale = 0.1)
set.seed(1)
result_a_bs <- NbClust(data = broken_swiss_roll, distance = "euclidean", min.nc = 2, max.nc = 10, method = "ward.D2", index  ="all", alphaBeale = 0.2)

result_k_bs$Best.nc
result_a_bs$Best.nc


results_evaluation_bsr <- matrix(ncol = 5, nrow = 8)
colnames(results_evaluation_bsr) <- c("Method", "Silhouette", "Dunn", "CH", "DB")
#Method without DR and k-means
set.seed(123)
bsr_data <- broken_swiss_roll
k_means_nodr_bsr = kmeans(bsr_data,centers = 3)
clusts_nodr_bsr <- k_means_nodr_bsr$cluster
silhouette_k_means_bsr_nodr <- silhouette(clusts_nodr_bsr, dist(bsr_data))
silhouette_k_means_bsr_nodr <- as.matrix(silhouette_k_means_bsr_nodr)
silhouette_k_means_bsr_nodr <- silhouette_k_means_bsr_nodr[,3]
silhouette_k_means_bsr_nodr <- mean(silhouette_k_means_bsr_nodr)

# Dunn index
dunn_k_means_bsr_nodr <- dunn( distance = NULL, clusts_nodr_bsr, Data = bsr_data)

# Calinski-Harabasz index
calinski_k_means_bsr_nodr <-   calinhara(bsr_data,clusts_nodr_bsr,cn=max(clusts_nodr_bsr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_bsr_nodr <-index.DB(bsr_data, clusts_nodr_bsr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[1,] <- c("K-means NoDR", silhouette_k_means_bsr_nodr, 
                                dunn_k_means_bsr_nodr, calinski_k_means_bsr_nodr,
                                davies_k_means_bsr_nodr)

fviz_cluster(k_means_nodr_bsr,  data = bsr_data,
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with no DR Broken Swiss Roll"
)
#Method with No DR and AGNES
set.seed(123)
dist_bsr_nodr <- dist(bsr_data, method= "euclidean")
agnes_bsr_nodr <- hclust(dist_bsr_nodr, method = "ward.D2" )
cut_agnes_bsr_nodr <- cutree(agnes_bsr_nodr, k = 3)


silhouette_agnes_bsr_nodr <- silhouette(cut_agnes_bsr_nodr, dist(bsr_data))
silhouette_agnes_bsr_nodr <- as.matrix(silhouette_agnes_bsr_nodr)
silhouette_agnes_bsr_nodr <- silhouette_agnes_bsr_nodr[,3]
silhouette_agnes_bsr_nodr <- mean(silhouette_agnes_bsr_nodr)

dunn_agnes_bsr_nodr <- dunn( dist(bsr_data), cut_agnes_bsr_nodr)
calinski_agnes_bsr_nodr <-   calinhara(bsr_data, cut_agnes_bsr_nodr,cn=max(cut_agnes_bsr_nodr))

davies_agnes_bsr_nodr <-index.DB(bsr_data, cut_agnes_bsr_nodr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[2,] <- c("AGNES NoDR", silhouette_agnes_bsr_nodr, 
                                dunn_agnes_bsr_nodr, calinski_agnes_bsr_nodr,
                                davies_agnes_bsr_nodr)


fviz_cluster(list(data = bsr_data, cluster =cut_agnes_bsr_nodr ),
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with no DR Broken Swiss Roll", xlab = "D", ylab = "dim"
)



#pca bsr
set.seed(123)
bsr_pca = prcomp(bsr_data)
bsr_pca = bsr_pca$x[,1:2]
#kmeans
set.seed(123)
k_means_pca_bsr = kmeans(bsr_pca,centers = 3)
clusters_pca_bsr <- k_means_pca_bsr$cluster
silhouette_k_means_bsr_pca <- silhouette(clusters_pca_bsr, dist(bsr_pca))
silhouette_k_means_bsr_pca <- as.matrix(silhouette_k_means_bsr_pca)
silhouette_k_means_bsr_pca <- silhouette_k_means_bsr_pca[,3]
silhouette_k_means_bsr_pca <- mean(silhouette_k_means_bsr_pca)

# Dunn index
dunn_k_means_bsr_pca <- dunn( distance = NULL, clusters_pca_bsr, Data = bsr_pca)

# Calinski-Harabasz index
calinski_k_means_bsr_pca <-   calinhara(bsr_pca,clusters_pca_bsr,cn=max(clusters_pca_bsr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_bsr_pca <-index.DB(bsr_pca, clusters_pca_bsr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[3,] <- c("k-means PCA", silhouette_k_means_bsr_pca, 
                                dunn_k_means_bsr_pca, calinski_k_means_bsr_pca,
                                davies_k_means_bsr_pca)

fviz_cluster(k_means_pca_bsr,  data = bsr_pca,
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with PCA Broken Swiss Roll"
)

#AGNES with PCA
set.seed(123)
dist_bsr_pca <- dist(bsr_pca, method= "euclidean")
agnes_bsr_pca  <- hclust(dist_bsr_pca, method = "ward.D2" )
cut_agnes_bsr_pca <- cutree(agnes_bsr_pca, k = 3)

silhouette_agnes_bsr_pca <- silhouette(cut_agnes_bsr_pca, dist(bsr_pca))
silhouette_agnes_bsr_pca <- as.matrix(silhouette_agnes_bsr_pca)
silhouette_agnes_bsr_pca <- silhouette_agnes_bsr_pca[,3]
silhouette_agnes_bsr_pca <- mean(silhouette_agnes_bsr_pca)

dunn_agnes_bsr_pca <- dunn( dist(bsr_pca), cut_agnes_bsr_pca)
calinski_agnes_bsr_pca <-   calinhara(bsr_pca, cut_agnes_bsr_pca,cn=max(cut_agnes_bsr_pca))

davies_agnes_bsr_pca <-index.DB(bsr_pca, cut_agnes_bsr_pca, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_bsr[4,] <- c("AGNES PCA", silhouette_agnes_bsr_pca, 
                                dunn_agnes_bsr_pca, calinski_agnes_bsr_pca,
                                davies_agnes_bsr_pca)


fviz_cluster(list(data = bsr_pca, cluster =cut_agnes_bsr_pca ),
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with PCA Broken Swiss Roll"
)

#tsne
set.seed(123)
tsne_bsr <- Rtsne(bsr_data, dims = 2)
tsne_bsr <- tsne_bsr$Y
#k-means with t-sne
set.seed(123)
k_means_tsne_bsr = kmeans(tsne_bsr,centers = 3)
clusters_tsne_bsr <- k_means_tsne_bsr$cluster

silhouette_k_means_bsr_tsne <- silhouette(clusters_tsne_bsr, dist(tsne_bsr))
silhouette_k_means_bsr_tsne <- as.matrix(silhouette_k_means_bsr_tsne)
silhouette_k_means_bsr_tsne <- silhouette_k_means_bsr_tsne[,3]
silhouette_k_means_bsr_tsne <- mean(silhouette_k_means_bsr_tsne)

# Dunn index
dunn_k_means_bsr_tsne <- dunn( distance = NULL, clusters_tsne_bsr, Data = tsne_bsr)

# Calinski-Harabasz index
calinski_k_means_bsr_tsne <-   calinhara(tsne_bsr,clusters_tsne_bsr,cn=max(clusters_tsne_bsr))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_bsr_tsne <-index.DB(tsne_bsr, clusters_tsne_bsr, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[5,] <- c("k-means t-SNE", silhouette_k_means_bsr_tsne, 
                                dunn_k_means_bsr_tsne, calinski_k_means_bsr_tsne,
                                davies_k_means_bsr_tsne)

fviz_cluster(k_means_tsne_bsr,  data = tsne_bsr,
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "k-means with t-SNE Broken Swiss Roll",
             xlab="V1", ylab = "V2"
)

#agnes t-sne
set.seed(123)
dist_bsr_tsne <- dist(tsne_bsr, method= "euclidean")
agnes_bsr_tsne  <- hclust(dist_bsr_tsne, method = "ward.D2" )
cut_agnes_bsr_tsne <- cutree(agnes_bsr_tsne, k = 3)


silhouette_agnes_bsr_tsne <- silhouette(cut_agnes_bsr_tsne, dist(tsne_bsr))
silhouette_agnes_bsr_tsne <- as.matrix(silhouette_agnes_bsr_tsne)
silhouette_agnes_bsr_tsne <- silhouette_agnes_bsr_tsne[,3]
silhouette_agnes_bsr_tsne <- mean(silhouette_agnes_bsr_tsne)

dunn_agnes_bsr_tsne <- dunn( dist(tsne_bsr), cut_agnes_bsr_tsne)
calinski_agnes_bsr_tsne <-   calinhara(tsne_bsr, cut_agnes_bsr_tsne,cn=max(cut_agnes_bsr_tsne))

davies_agnes_bsr_tsne <-index.DB(tsne_bsr, cut_agnes_bsr_tsne, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[6,] <- c("AGNES t-SNE", silhouette_agnes_bsr_tsne, 
                                dunn_agnes_bsr_tsne, calinski_agnes_bsr_tsne,
                                davies_agnes_bsr_tsne)

fviz_cluster(list(data = tsne_bsr, cluster =cut_agnes_bsr_tsne ),
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "agnes with t-SNE Broken Swiss Roll",
             xlab="V1", ylab = "V2"
)


#umap
library(uwot)
set.seed(123)
umap_bsr <- umap(bsr_data, n_neighbors = 50, min_dist = 0.001, n_components = 2, 
                 init = "laplacian", learning_rate = 5.0)
#Kmeans Umap
set.seed(123)
kmeans_bsr_umap <- kmeans(umap_bsr, centers = 3)

fviz_cluster(kmeans_bsr_umap,  data = umap_bsr,
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "k-means with UMAP Broken Swiss Roll"
)

kmeans_cluster_bsr_umap <- kmeans_bsr_umap$cluster
silhouette_k_means_bsr_umap <- silhouette(kmeans_cluster_bsr_umap, dist(umap_bsr))
silhouette_k_means_bsr_umap <- as.matrix(silhouette_k_means_bsr_umap)
silhouette_k_means_bsr_umap <- silhouette_k_means_bsr_umap[,3]
silhouette_k_means_bsr_umap <- mean(silhouette_k_means_bsr_umap)

# Dunn index
dunn_k_means_bsr_umap <- dunn( distance = NULL, kmeans_cluster_bsr_umap, Data = umap_bsr)


# Calinski-Harabasz index
calinski_k_means_bsr_umap <-   calinhara(umap_bsr,kmeans_cluster_bsr_umap,cn=max(kmeans_cluster_bsr_umap))


# Davies-Bouldin index
#KLEOBAA ES
davies_k_means_bsr_umap <-index.DB(umap_bsr,kmeans_cluster_bsr_umap, d=NULL, centrotypes="centroids", p=2, q=2)$DB

results_evaluation_bsr[7,] <- c("k-means UMAP", silhouette_k_means_bsr_umap, 
                                dunn_k_means_bsr_umap, calinski_k_means_bsr_umap,
                                davies_k_means_bsr_umap)

#Agnes with UMAP 
set.seed(123)
dist_bsr_umap <- dist(umap_bsr, method= "euclidean")
agnes_cluster_umap_bsr  <- hclust(dist_bsr_umap, method = "ward.D2" )
cut_agnes_umap_bsr <- cutree(agnes_cluster_umap_bsr, k = 3)

fviz_cluster(list(data = umap_bsr, cluster = cut_agnes_umap_bsr),
             palette = c("#FF0000",  "#000FFF",
                          "#00FF00"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             xlab = "D1",
             ylab = "D2",
             main = "AGNES with UMAP Broken Swiss Roll"
)
#scatterplot3d(bsr_data, color=rainbow(4)[cut_agnes_umap_bsr], main = "agnes after UMAP bsr")


silhouette_agnes_bsr_umap <- silhouette(cut_agnes_umap_bsr, dist(umap_bsr))
silhouette_agnes_bsr_umap <- as.matrix(silhouette_agnes_bsr_umap)
silhouette_agnes_bsr_umap <- silhouette_agnes_bsr_umap[,3]
silhouette_agnes_bsr_umap<- mean(silhouette_agnes_bsr_umap)


dunn_agnes_bsr_umap <- dunn( distance = NULL, cut_agnes_umap_bsr, Data = umap_bsr)


# Calinski-Harabasz index
calinski_agnes_bsr_umap <-   calinhara(umap_bsr,cut_agnes_umap_bsr,cn=max(cut_agnes_umap_bsr))


# Davies-Bouldin index
#KLEOBAA ES
davies_agnes_bsr_umap <-index.DB(umap_bsr, cut_agnes_umap_bsr, d=NULL, centrotypes="centroids", p=2, q=2)$DB
results_evaluation_bsr[8,] <- c("AGNES UMAP", silhouette_agnes_bsr_umap, 
                                dunn_agnes_bsr_umap, calinski_agnes_bsr_umap,
                                davies_agnes_bsr_umap)





dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[k_means_nodr_bsr$cluster], type = "p", main = "kmeans no DR")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000", "#000FFF", 
                                                                   "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[cut_agnes_bsr_nodr], type = "p", main = "")
legend( s$xyz.convert(-2, -2, 3), legend= c(1,2,3), col = c("#FF0000", "#000FFF", 
                                                     "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[k_means_pca_bsr$cluster], type = "p", main = "kmeans PCA")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                            "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[cut_agnes_bsr_pca], type = "p", main = "AGNES PCA")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                             "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[k_means_tsne_bsr$cluster], type = "p", main = "kmeans t-SNE")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                             "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[cut_agnes_bsr_tsne], type = "p", main = "AGNES t-SNE")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                                                  "#00FF00") ,pch = 16)

dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[kmeans_bsr_umap$cluster], type = "p", main = "kmeans UMAP")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                             "#00FF00") ,pch = 16)
dev.new()
s <- scatterplot3d(bs_data, pch=20, color = rainbow(3)[cut_agnes_umap_bsr], type = "p", main = "AGNES UMAP")
legend( s$xyz.convert(-2, -2, 3), legend =c(1,2,3), col = c("#FF0000",  "#000FFF",
                                                            "#00FF00") ,pch = 16)