install.packages("devtools")

# Commands to install the LIMMA package
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("limma") # install the limma package
BiocManager::install("ROC") # install the ROC package
