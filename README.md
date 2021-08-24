# MoSBi - Molecular signatures using Biclustering

This package is a implementation of biclustering ensemble 
method MoSBi (Molecular signature Identification from Biclustering).
MoSBi provides standardized interfaces for biclustering results and 
can combine their 
results with a multi-algorithm ensemble approach to compute robust 
ensemble biclusters on molecular omics data.
This is done by computing similarity networks of biclusters and 
filtering for overlaps using a custom error model.
After that, the louvain modularity it used to extract bicluster 
communities from the similarity network, which can then be converted 
to ensemble biclusters.
Additionally, MoSBi includes several network visualization methods 
to give an intuitive and scalable overview of the results.
MoSBi comes with several biclustering algorithms, but can be easily 
extended to new biclustering algorithms.

Installation instructions:
``` r
# install.packages("devtools")
devtools::install_github("tdrose/mosbi")
```
