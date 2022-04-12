# MoSBi - Molecular signatures using Biclustering

R package of the biclustering ensemble algorithm MoSBi.


For information about the algorithm please have a look at the publication:

>MoSBi: Automated signature mining for molecular stratification and subtyping
>
>Tim Daniel Rose, Thibault Bechtler, Octavia-Andreea Ciora, Kim Anh Lilian Le, Florian Molnar, Nikolai Koehler, Jan Baumbach, Richard Röttger, Josch Konstantin Pauling
>
>Proceedings of the National Academy of Sciences, 2022; 119 (16): e2118210119; 


doi: [https://doi.org/10.1073/pnas.2118210119](https://doi.org/10.1073/pnas.2118210119)

### Installation

The easiest way to install MoSBi is from Bioconductor.

You can find the package [here](https://bioconductor.org/packages/mosbi/) and install is with this command:

``` r
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("mosbi")
```

For experienced users, the package can be installed directly from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("tdrose/mosbi")
```


### License

This software is published under the AGPLv3 license.

![AGPLv3 logo](https://www.gnu.org/graphics/agplv3-with-text-162x68.png)
