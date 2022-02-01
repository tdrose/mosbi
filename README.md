# MoSBi - Molecular signatures using Biclustering

R package of the biclustering ensemble algorithm MoSBi.


For information about the algorithm please have a look at the publication:

>MoSBi: Automated signature mining for molecular stratification and subtyping
>Tim Daniel Rose, Thibault Bechtler, Octavia-Andreea Ciora, Kim Anh Lilian Le, Florian Molnar, Nikolai Koehler, Jan Baumbach, Richard Röttger, Josch Konstantin Pauling
>bioRxiv 2021.09.30.462567; 


doi: [https://doi.org/10.1101/2021.09.30.462567](https://doi.org/10.1101/2021.09.30.462567)

### Installation

The easiest way to install MoSBi is from Bioconductor.

You can find the package [here](https://bioconductor.org/packages/mosbi/) and install is with this command:

```
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("mosbi")
```

For experienced users, the package can be installed directly from GitHub:

```
# install.packages("devtools")
devtools::install_github("tdrose/mosbi")
```


### License

This software is published under the AGPLv3 license.

![AGPLv3 logo](https://www.gnu.org/graphics/agplv3-with-text-162x68.png)
