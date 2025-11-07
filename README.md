
# Bayesian Inference using INLA and inlabru

Welcome to the course **“Bayesian Inference using INLA and inlabru”**, a 10-hour workshop designed to introduce participants to **Bayesian inference** for **latent Gaussian models (LGMs)** using the **INLA** and **inlabru** frameworks in R.  

The course provides both a **theoretical foundation** and **practical applications** for fitting hierarchical and spatial models using deterministic Bayesian computation.

---

## 🧠 Course Overview

This course combines **conceptual understanding** with **hands-on experience** using real datasets and examples from ecology, medicine, and spatial statistics.  
It is structured into **two main parts**:

### **PART I — Bayesian inference using INLA**
- Why INLA?  
- Latent Gaussian models (LGMs)  
- Gaussian Markov Random Fields (GMRFs)  
- Laplace approximations  
- Bayesian inference via INLA  
- Model selection and diagnostics  
- Applications: GLMMs, temporal smoothing, and hierarchical models  

### **PART II — Spatial Bayesian modeling with inlabru**
- Spatial data and types of spatial models  
- Disease mapping with Besag models  
- Geostatistical modeling and continuous spatial fields  
- Penalized complexity priors (PC-priors)  
- Linking INLA with inlabru for spatial inference  

---

## 💻 Software Requirements

To follow the practical sessions, please ensure that the following software is installed:

- [R (version 4.5.1 or later)](https://cran.r-project.org/)  
- [RStudio](https://posit.co/download/rstudio-desktop/)  

### **INLA Installation**

The examples use the **INLA** package version:

```
This is INLA_25.06.07 built 2025-06-11 18:54:45 UTC; unix.
- See www.r-inla.org/contact-us for how to get help.
- List available models/likelihoods/etc with inla.list.models()
- Use inla.doc(<NAME>) to access documentation
- Consider upgrading R-INLA to testing[25.10.28] or stable[25.10.19]
```

You can install it directly from the official INLA repository:

```r
install.packages("INLA", repos = c(getOption("repos"), INLA = "https://inla.r-inla-download.org/R/stable"))
```

For more information, visit the official installation guide:  
🔗 [https://www.r-inla.org/download-install](https://www.r-inla.org/download-install)

---

## 📦 R Packages

The following R packages are required for the course:

```r
install.packages(c(
  "INLA", "inlabru", "Matrix", "sp", "sf", "spdep",
  "tmap", "raster", "terra", "ggplot2", "dplyr", "readxl", "kableExtra"
))
```

> Additional dependencies may be installed automatically when loading the main packages.

---

## 📁 Folder Structure

The repository is organized as follows:

```
Bayesian-INLA/
│
├── Theory/
│   ├── S1_inla.html
│   ├── S2_spatial.html
│
├── Practical-lessons/
│   ├── S1-INLA-KAKHI-VALENCIA/
│   ├── S1-INLA-measurement_agreement_COPD/
│   ├── S1-INLA-rain/
│   ├── S1-INLA-seeds/
│   ├── S2-inlabru-geostatistics/
│   └── S2-INLA-disease-mapping/
│
└── README.md
```

- **Theory/**: Contains the main lecture slides (HTML format) introducing the theory behind INLA and inlabru.  
- **Practical-lessons/**: Includes practical examples applying the concepts:
  - **S1-INLA-seeds**: Generalized Linear Mixed Model (GLMM) using INLA  
  - **S1-INLA-measurement_agreement_COPD**: Mixed model for measurement agreement  
  - **S1-INLA-rain**: Temporal smoothing of binomial data  
  - **S2-INLA-disease-mapping**: Spatial disease risk modeling  
  - **S2-inlabru-geostatistics**: Geostatistical modeling using inlabru  
  - **S1-INLA-KAKHI-VALENCIA**: Introductory INLA analysis (Valencia dataset)

---

## 🎯 Learning Outcomes

By the end of this course, participants will be able to:

- Understand the structure of **latent Gaussian models (LGMs)**.  
- Apply the **INLA methodology** for fast, deterministic Bayesian inference.  
- Use **GMRFs** and **Laplace approximations** to build hierarchical models.  
- Fit and interpret **spatial models** with **inlabru** and **INLA**.  
- Evaluate models using DIC, WAIC, and CPO.  
- Visualize and interpret posterior distributions, predictive maps, and uncertainty.  

---

## 👨‍🏫 Instructor

**Joaquín Martínez-Minaya**  
Applied Statistics and Operations Research and Quality (DEIOAC)  
Universitat Politècnica de València (UPV)

📧 **Email:** [jmarmin@eio.upv.es](mailto:jmarmin@eio.upv.es)  
🌐 **Website:** [https://github.com/jmartinez-minaya](https://github.com/jmartinez-minaya)  

---

## 📚 References

1. Bachl, F. E., Lindgren, F., Borchers, D. L., & Illian, J. B. (2019). *inlabru: An R package for Bayesian spatial modelling from ecological survey data.* *Methods in Ecology and Evolution, 10*(6), 760–766. [DOI:10.1111/2041-210X.13168](https://doi.org/10.1111/2041-210X.13168)  

2. Blangiardo, M., & Cameletti, M. (2015). *Spatial and Spatio-temporal Bayesian Models with R-INLA.* Wiley.  

3. Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2013). *Bayesian Data Analysis* (3rd ed.). Chapman and Hall/CRC.  

4. Gómez-Rubio, V. (2020). *Bayesian Inference with INLA.* CRC Press.  

5. Krainski, E., Gómez-Rubio, V., Bakka, H., Lenzi, A., Rue, H., & Lindgren, F. (2019). *Advanced Spatial Modeling with Stochastic Partial Differential Equations Using R and INLA.* Chapman and Hall/CRC.  

6. Lawson, A. B. (2018). *Bayesian Disease Mapping: Hierarchical Modeling in Spatial Epidemiology* (3rd ed.). CRC Press.  

7. Moraga, P. (2019). *Geospatial Health Data: Modeling and Visualization with R-INLA and Shiny.* Chapman and Hall/CRC. [https://www.paulamoraga.com/book-geospatial/](https://www.paulamoraga.com/book-geospatial/)  

8. Moraga, P. (2023). *Spatial Statistics for Data Science: Theory and Applications in R.* Chapman and Hall/CRC. [https://www.paulamoraga.com/book-spatial/](https://www.paulamoraga.com/book-spatial/)  

9. Rue, H., Martino, S., & Chopin, N. (2009). *Approximate Bayesian Inference for Latent Gaussian Models by Using Integrated Nested Laplace Approximations.* *Journal of the Royal Statistical Society: Series B (Statistical Methodology), 71*(2), 319–392.  

---

## 🔗 License

All materials are provided for **educational and research purposes**.  
If you reuse or adapt any part of this course, please acknowledge the author.

---
