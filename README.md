# COVID-19 Clustering and U.S. State-Level Funding Analysis

This project investigates global COVID-19 trends through unsupervised clustering and examines the distribution of U.S. state-level COVID-19 grant funding during the year 2020. The analysis is conducted in R using publicly available datasets, with a focus on quantitative insights and reproducibility.

## Project Overview

The analysis is structured in two main parts:

Part 1: Global Country Clustering
- Clustering of over 100 countries using standardized totals for confirmed COVID-19 cases, deaths, and case fatality rates.
- Methods include k-means and hierarchical clustering.
- Dimensionality reduction is performed using principal component analysis (PCA).
- Clustering validity is assessed using the Elbow and Silhouette methods.

Part 2: U.S. State-Level Funding Disparity Analysis
- Integration of datasets on state-level COVID-19 cases, population, and HHS grant funding.
- Calculation of the following metrics:
  - Funding per capita
  - COVID-19 cases per capita
  - Funding per reported COVID-19 case
- Visual representation of funding distribution and public health burden.

## Data Sources

- Johns Hopkins University CSSE COVID-19 time series data
- Our World in Data COVID-19 vaccination records
- U.S. Department of Health and Human Services (HHS) TAGGS COVID-19 grant report
- U.S. Census Bureau state population estimates (2020)
- The New York Times COVID-19 state-level case data
