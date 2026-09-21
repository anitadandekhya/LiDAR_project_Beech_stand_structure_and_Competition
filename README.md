# Beech Stand Structure and Competition from Mobile Laser Scanning (MLS)
Author: Anita Dandekhya
## 1. Introduction

This repository contains the R-based workflow on **beech forest stand structure and tree competition using Mobile Laser Scanning (MLS) point-cloud data**.

The analysis focuses on three main aspects:

* **Stem density** – number of detected trees per plot
* **DBH distribution** – diameter distribution of individual trees
* **Tree competition** – competition around individual target trees

The workflow combines **TreeLS** for stand-structure analysis and **TreeCompR** for calculating competition indices.

## 2. Data

The study uses **MLS point-cloud data from Site 9** among different Beech stand structure study sites in Bavaria. The dataset contains 18 plots each consisting of:

* a neighbourhood point cloud representing the surrounding forest plot
* a manually segmented target-tree point cloud

The analysis includes **18 plots**. The neighbourhood plots cover **30 × 30 m (900 m² = 0.09 ha)**.

### Data ownership

The original MLS point-cloud data from Site 9 are owned by **Dr. Julia Rieder**. 
The original MLS data are **not included in this repository** because of data-ownership restrictions

## 3. R Packages

The analysis was performed in **R** using the following packages.

### Stand Structure Analysis

* lidR – LiDAR point-cloud processing
* TreeLS – tree and stem detection and tree inventory
* tidyverse – data manipulation and visualisation
* patchwork – combining plots and figures

### Competition Analysis

* Rfast – supporting numerical computations
* devtools – installation of TreeCompR from GitHub
* TreeCompR – calculation of tree competition indices
* tidyverse – data processing/ manipulation and visualisation
* rgl – 3D point-cloud visualisation

## Workflow

### Stand Structure – TreeLS

The TreeLS workflow processes the neighbourhood point clouds to identify tree stems, estimate DBH and describe the stand structure.

The main processing workflow is:

1. **MLS point cloud**
2. **Voxel-based point-cloud thinning**
3. **Tree mapping**
4. **Tree-point extraction**
5. **Stem detection**
6. **Stem radius estimation**
7. **DBH calculation**
8. **Stand inventory**
9. **Stem density and DBH distribution**

The workflow includes:

* Reading the MLS neighbourhood point clouds
* Voxel-based sampling with a voxel size of **0.01 m**
* Tree mapping using a **Hough-based approach**
* Cropping and extracting tree points
* Stem detection using a **Hough-based method**
* Fitting stem cross-sections using a **circular shape model with RANSAC**
* Calculating DBH from the estimated stem radius
* Calculating the number of stems and stem density for each plot
* Analysing and visualising DBH distributions

### Competition – TreeCompR

TreeCompR is used to calculate competition indices for the **18 target trees** using their corresponding target-tree and neighbourhood point clouds.

Two competition approaches are calculated:

* **Cone-based competition index**
* **Cylinder-based competition index**

The workflow:

1. Matches each target-tree point cloud with its corresponding neighbourhood point cloud.
2. Applies the TreeCompR competition analysis to all **18 target trees**.
3. Calculates both cone-based and cylinder-based competition indices.
4. Summarises and compares the resulting competition indices.
5. Calculates the Pearson correlation and regression between the two approaches.
6. Produces a 3D visualisation of the target tree, surrounding vegetation and competition regions.

## 5. Results

The results show the main outputs generated from the MLS point-cloud analysis. The results are divided into **stand structure** and **tree competition**.

### Stand Structure (TreeLS)

The TreeLS analysis provides results describing the structural characteristics of the investigated beech plots, including:

* Tree inventory with detected stems and estimated DBH values
* Number of stems per plot
* Stem density per hectare, based on the 30 × 30 m (0.09 ha) neighbourhood plots
* DBH distributions showing the variation in tree diameter within the investigated plots
* DBH distribution figures combining the overall distribution with individual tree observations

### Competition (TreeCompR)

The TreeCompR analysis provides competition measurements for each target tree. The results include:

* TreeCompR competition indices
* Cone-based and cylinder-based competition comparison
* Pearson correlation and regression results
* Competition summary statistics
* 3D visualisation of the target tree and its competing neighbour
