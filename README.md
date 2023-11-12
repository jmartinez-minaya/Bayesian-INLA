
Bayesian Inference using INLA
================
This is a course for everyone who want to start learning Bayesian Inference using INLA. It has been constructed combining material of different sources: tutorial and books which are referenced at the end of each presentation.


# Program
The course is divided in three parts combining theoretical and practical aspects, and normally is taught in two classes (4 hours per class)

**class1-PART 1: An Introduction to Bayesian inference**. Bayes theorem. Posterior distribution of the parameters. Prediction. Hierarchical Bayesian models.
 
**class1-PART 2: Bayesian inference using the integrated nested Laplace approximation (INLA)** Latent Gaussian models (LGMs). Laplace approximation. Gaussian Markov random fields (GMRFs). Fitting GLMMs using INLA. Structured temporal and spatial random effects.
 
**class2-PART 3: Geostatistics using INLA and SPDE**. Geostatistics in the context of LGMs. The Stochastic partial differential equation (SPDE). 
 

# Software

To take full advantage of the course, it is necessary that everyone has the following programs installed:

- version 4.0.0 of [R](https://cran.r-project.org/) or posterior, and
- [RStudio](https://www.rstudio.com/products/rstudio/download/), or
- [Quarto](https://quarto.org/docs/get-started/), or
- [Visual Studio Code](https://code.visualstudio.com/download)


# R packages

This will be the packages required for the course

```r
install.packages(pkgs = c("ggplot2", "gridExtra", "maptools", "sf", "spdep", "lattice", "latticeExtra", "viridis", "splancs", "terra", "lattice", "fields", "plotKML", "raster", "sp"))

```

The R-INLA package can be downloaded directly from the webpage https://www.r-inla.org/download-install

```r
### --- INLA --- ###
install.packages("INLA", repos="https://inla.r-inla-download.org/R/stable")
```

Also, other packages from Bioconductor
```r
BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
```

