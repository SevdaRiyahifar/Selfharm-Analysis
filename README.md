# Self-Harm Statistical Modeling Project

## Overview
This repository contains R code for statistical analysis of self-harm (SH) behavior using advanced count data models and mediation frameworks.

The study investigates the association between attachment styles, emotion regulation, and self-harm behavior using both traditional and mixture modeling approaches.

---

## Research Objectives
- To model self-harm count data with excess zeros
- To compare Poisson, Negative Binomial, ZIP, and ZINB models
- To estimate mediation effects using Sims-based framework (MZIP/MZIDP)
- To validate findings using Structural Equation Modeling (SEM)

---

## Data
The dataset is derived from a cross-sectional study and includes:

- **Outcome:** Self-harm count (SH)
- **Exposure:** Attachment-related variables
- **Mediator:** Emotion regulation scales
- **Covariates:** Age, Gender, Marital status, Occupation

> Note: Raw data are not shared due to privacy restrictions.

---

## Methods

### 1. Count Data Models
- Poisson regression
- Negative Binomial regression
- Zero-Inflated Poisson (ZIP)
- Zero-Inflated Negative Binomial (ZINB)
- Model selection based on AIC, BIC, and Vuong tests

### 2. Mediation Analysis (Sims Framework)
- Implemented using `mzipmed`
- Estimation of:
  - Natural Direct Effect (NDE)
  - Natural Indirect Effect (NIE)
  - Total Effect (TE)
- Results reported as Incidence Rate Ratios (IRR)

### 3. Advanced Mixture Models
- Mixture Zero-Inflated Double Poisson (MZIDP)
- Maximum likelihood estimation
- Bootstrap-based inference

### 4. Structural Equation Modeling
- Implemented using `lavaan`
- Direct and indirect pathways simultaneously estimated

---

## Software Requirements
- R (≥ 4.0)

### Required Packages:
```r
pscl
MASS
mzipmed
mediation

ggplot2
openxlsx
haven
