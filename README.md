# A Spatial Hierarchical Item Response Theory for Soccer Shot Analysis

This GitHub repository accompanies the paper:

> Das, S. and Mondal, D. (2026). A Spatial Hierarchical Item Response Theory for Soccer Shot Analysis

---

## Repository Structure

### `data/`

This directory contains the soccer data analyzed in the article.

* **`Soccer_data.csv`**

  Contains spatial locations and outcomes of the shots analyzed in the article.

---

### `illustration/`

This folder contains codes to replicate figures. The scripts are titled based on the figure.

* **`Estimates.R`**

  Scripts to reproduce all the posterior means, probabilities, and ratios in **Section 4** of the article.

* **`Figure1.m`, `Figure3.R`, `Figure4.R`, `Figure5.R`, `Figure6.R`,**

  Scripts to reproduce **Figures 1**, **3**, **4**, **5**, and **6**, respectively (**Figures 2** is not a generated figure).

---

### `mcmc/`

This folder contains scripts to run the MCMC simulation:

* **`design.txt`**

  Calculates design matrices.
  
  
* **`rkslambda.cpp`**

  Helper function to simulate from Kolmogorov-Smirnov distribution.
  
  
* **`Run.R`**

  Scripts to reproduce the MCMC outputs.

---

### `output/`

This folder contains outputs from the MCMC simulation:

* **`Beta1.rds`**

  Posterior samples corresponding to the non-spatial effects from `mcmc/Run.R`.

  
* **`Beta2.rds`**

  Posterior samples corresponding to the spatial effects from `mcmc/Run.R`.
  
---

## Requirements

* R (>= 4.0)
* A C++11-compatible compiler (e.g., GCC, Clang, or Rtools on Windows)

---

## Citation

> Das, S. and Mondal, D. (2026). A Spatial Hierarchical Item Response Theory for Soccer Shot Analysis

---

