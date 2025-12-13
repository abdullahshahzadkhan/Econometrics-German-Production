# Applied Econometrics: Modeling German Production in R

## 📌 Project Overview
This project bridges the gap between theoretical econometrics and practical application. Using data from the **Penn World Table (10.0)**, I modeled the aggregate production function of the German economy from 1950 to 2019.

The goal was not just to fit a model, but to replicate the theoretical progression of a graduate-level econometrics course: starting with standard assumptions, diagnosing violations (autocorrelation), and implementing robust nonlinear solutions.

## 🚀 Key Features
* **Data Pipeline:** Cleaning and transforming `pwt10` data for time-series analysis.
* **Iterative Modeling:**
    * **OLS (Log-Linear):** Initial estimation of elasticities.
    * **Diagnostics:** Formal testing for Multicollinearity (VIF), Heteroskedasticity (Goldfeld-Quandt), and Autocorrelation (Durbin-Watson).
    * **GLS (Generalized Least Squares):** Correcting for serial correlation using an AR(1) structure.
    * **NLS (Nonlinear Least Squares):** Estimating the raw multiplicative Cobb-Douglas function.
* **"Under the Hood" Implementation:** To demonstrate mathematical proficiency, I manually replicated the OLS, GLS, and Gauss-Newton optimization algorithms using **raw matrix algebra** in R, verifying the standard library outputs.

## 🛠️ Technologies
* **Language:** R
* **Libraries:** `pwt10`, `nlme`, `car`, `lmtest`, `dplyr`
* **Concepts:** Matrix Algebra, Maximum Likelihood, Optimization Algorithms, Time Series.

## 📊 Sample Insight
Initial OLS modeling suggested Returns to Scale of **2.06**, which is theoretically improbable for a mature economy. By diagnosing autocorrelation and switching to a GLS estimator, we corrected the standard errors and coefficients, revealing that the OLS estimates were inflated by persistent trends in the residuals.

---
*This project was created to practically apply theoretical concepts from Econometrics I course at the Georg August University of Göttingen.*
