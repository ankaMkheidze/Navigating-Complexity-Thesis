# Navigating-Complexity-Thesis
This repository contains code and workspaces used to create the Paper "Navigating Complexity: Evaluating the Effectiveness of Dimension Reduction and Clustering Approaches on Challenging Datasets" as a Bachelor's thesis for EUR. 

The Files contain 6 data sets and all the steps taken to produce results for the thesis. 
The Dimension Reduction techniques used for each data set are PCA, t-SNE, and UMAP. 
The clustering techniques are k-means and AGNES. 

## Data Sets
The data sets are a sample of 5000 observations from Jester Dataset, Swiss roll with 3 clusters, Broken Swiss roll, Helix, Twinpeaks, and High-Dimensional data set. The artificial data sets are highly inspired by [Matlab-toolbox-for-Dimensionality-Reduction](https://github.com/UMD-ISL/Matlab-Toolbox-for-Dimensionality-Reduction).

## Steps:

For every data set, there are 2 files: the results, and UMAP tuning. 
1. Open the results file and generate/load the data set. 
2. Run the two NbClust functions that will indicate the optimal number of clusters.
3. Go to the tuning file and input the number of clusters that were given as optimal and run the whole code. As output, you will get the top 5 performing combinations of different hyperparameters and choose the best overall one.
4. Go back to the results file and run the rest of the code, which will yield different visualizations for data sets and the values for internal validation indexes in a matrix results_evaluation. 

## Note:
1. The final results that have been produced from this research and the runs can be found in the workspace of the dataset with the file being too large for Github, so upon request, it can be shared.
2. There are some necessary libraries that need installation and they will be given in the results file of every function.
3. The high dimensional data set uses Matlab code for data generation and an Excel file with the final resulting data set given as well.
4. The seeds for every function are preset, and changing the will alter the results.
