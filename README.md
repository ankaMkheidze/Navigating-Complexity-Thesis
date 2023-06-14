# Navigating-Complexity-Thesis
This repository contains code and workspaces used to create Paper "Navigating Complexity: Evaluating the Effectiveness of Dimension Reduction and Clustering Approaches on Challenging Datasets" as a Bachelor's thesis for EUR. 

The Files contain 6 data sets and all the steps taken to produce results for the thesis. 
The Dimension Reduction techniques used for each data set are PCA, t-SNE and UMAP. 
The clustering techniques are k-means and AGNES. 

##Data Sets
The data sets are: sample of 5000 observations from Jester Dataset, Swiss roll with 3 clusters, Broken swiss roll, Helix, Twinpeaks, and High-Dimensional data set. The artificial data sets are highly inspired by [Matlab-toolbox-for-Dimensionality-Reduction](https://github.com/UMD-ISL/Matlab-Toolbox-for-Dimensionality-Reduction).

## Steps:

For every data set there are 3 files: the results, umap tuning and R workspace. 
1. Open the results file and generate / load the data set. 
2. Run the two NbClust functions that will indicate what the optimal number of clusters are.
3. Go to tuning file and input the number of clusters that were given as optimal and run the whole code. As output you will get the top 5 performing combination of different hyperparameters and choose the best overall one.
4. Go back to results file and run the rest of the code, which will yield different visualizations for data sets and the values for internal validation indexes in a matrix results_evaluation. 

## Note:
1. The final results that have been produced from this reseach and the runs can be found in each workspace of dataset.
2. There are some necessary libraries that need instalation and they will be given in the results file of every function.
3. The high dimensional data set uses Matlab code for data generation and an excel file with the final resulting data set given aswell.
4. The seeds for every function are preset, and changing the will alter the results.
