rm(list = ls())

# The Dynamics of Production: From Linear OLS to Nonlinear MLE

library(pwt10)
library(dplyr)

# Filter for Germany and select relevant columns
germany_data <- pwt10.0 %>% 
  filter(country == "Germany") %>% 
  select(year, rgdpo, emp, rnna) %>% 
  rename(gdp = rgdpo, labor = emp, capital = rnna)
head(germany_data)
str(germany_data)

# Add log of variables
germany_data <- germany_data %>% 
  mutate(log_Y = log(gdp)) %>% 
  mutate(log_L = log(labor)) %>% 
  mutate(log_K = log(capital))
head(germany_data)

# First Model (Linear): OLS - Line of Best Fit
model1 <- lm(log_Y ~ log_L + log_K, data = germany_data)
summary(model1)
# log_L = 1.19 and log_K 0.87 (total: 2.06) indicate extremely high returns to scale (more than double) 
# which is suspicious in economics. High chance of presence of multicollinearity.

# So, now we check for multicollinearity using VIF
library(car)
vif(model1)
# VIF of 3.4 indicates that multicollinearity isnt the cause of such high elasticities. Maybe something else.

# Hypothesis Testing
# H0: βlabor + βcapital = 1 | H1: βlabor + βcapital != 1
linearHypothesis(model1, "log_L + log_K = 1")
# We get an F-Statistic of 8.4 and a p-value much less than 0.05. This means our hypothesis must be rejected
# Now we move on to dive further and break assumptions
# OLS asssumes that variance of errors is constant over time (Heteroskedasticity) and there is not autocorrelation (et != es)

# Test for Heteroskedasticity
plot(germany_data$year, model1$residuals)
abline(h=0, col = "red")
abline(v=1990, col = "blue")
# mid_year <- round(min(germany_data$year) + ((max(germany_data$year) - min(germany_data$year))/2))
# abline(v=mid_year, col = "blue")
# Errors follow a trend which indicate autocorrelation and the variance seems like increasing too. Lets confirm.

# Heteroskedasticity (Goldfeld-Quandt Test)
library(lmtest)
gqtest(model1, order.by = ~year, data = germany_data)

# Autocorrelation (Durbin-Watson Test)
dwtest(model1)
# These tests confirm there is no heteroskedasticity but high autocorrelation exists
# GQ Test's p-value: 0.6 | DW Test's value: 0.04 & p-value: 0.000...

# Since there is autocorrelation present,  we move on from OLS to GLS
library(nlme)
gls(log_Y ~ log_L + log_K, data = germany_data, correlation = corAR1())
# A drop in both coefficients confirms that there was indeed autocorrelation which has now been taken care of.

# Nonlinear Estimation
# Initialize Values for Betas
y_int <- model1$coefficients[names(model1$coefficients) == "(Intercept)"]
beta_labor <- model1$coefficients[names(model1$coefficients) == "log_L"]
beta_capital <- model1$coefficients[names(model1$coefficients) == "log_K"]

model_nls <- nls(gdp ~ A * (labor^beta_L) * (capital^beta_K), 
    data = germany_data,
    start = list(A = exp(y_int), beta_L = beta_labor, beta_K = beta_capital),
    control = nls.control(maxiter = 500))
summary(model_nls)
# increase in both coeffs. 

# Prediction (Simulated Data - 5 Years)
# Assuming labor stays constant while capital grows 2% every year
labor_last <- germany_data[nrow(germany_data),3]
capital_last <- germany_data[nrow(germany_data),4]
year_last <- germany_data[nrow(germany_data),1]

# paste(labor_last, capital_last, year_last)

labor_vals <- rep(labor_last, 5)

capital_vals <- capital_last * 1.02^(1:5)

year_vals <- (year_last+1):(year_last+5)


future_data <- data.frame(year = year_vals,
           labor = labor_vals,
           capital = capital_vals)

# Lets Predict on our Simulated Data
future_data$predicted_gdp <- predict(model_nls, newdata = future_data)
future_data

# Bind 2019
last_history_row <- data.frame(
  year = year_last,
  labor = labor_last,
  capital = capital_last,
  predicted_gdp = germany_data$gdp[nrow(germany_data)] # Use actual GDP to start the line
)

# Bind them together so the red line has an anchor point
plot_data <- rbind(last_history_row, future_data)


# Lets Plot historical and Predicted Line
plot(germany_data$year, germany_data$gdp,
     type = "l", col = "black", lwd = 2,
     main = "German Production: History vs. NLS Forecast",
     xlab = "Year", ylab = "GDP",
     xlim = c(min(germany_data$year), max(future_data$year)),
     ylim = c(0, max(c(germany_data$gdp, future_data$predicted_gdp)))
     )
# Add forecast line
lines(plot_data$year, plot_data$predicted_gdp, col = "red", lwd = 3, lty = 2)

legend("topleft", legend = c("Historical Data", "NLS Forecast (2% Cap. Growth"),
       col = c("black", "red"), lty = c(1,2), lwd = c(2,3))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Redoing everything by hand
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hand Coding OLS, GLS and NLS
# Filter for Germany and select relevant columns
germany_data <- pwt10.0 %>% 
  filter(country == "Germany") %>% 
  select(year, rgdpo, emp, rnna) %>% 
  rename(gdp = rgdpo, labor = emp, capital = rnna)
head(germany_data)
str(germany_data)

# Transform data into Y = XB + e
Y <- matrix(log(germany_data$gdp), ncol = 1)
X <- matrix(c(rep(1, nrow(germany_data)),
              log(germany_data$labor),
              log(germany_data$capital)),
            ncol = 3, byrow = FALSE)
X
dim(Y)
dim(X)

# Calculate b = (X'X)^-1 X'Y
p1 <- solve(t(X) %*% X)
p2 <- t(X) %*% Y
b_hand = p1 %*% p2

# Compare both estimators
print(b_hand)
coef(model1)

# Variance and Standard Erros
e_hand <- Y - (X%*%b_hand)
SSR <- (t(e_hand) %*% e_hand)
T <- nrow(X)
K <- ncol(X)
sigma2_hand <- (SSR/(T - K))

cov_b <- as.numeric(sigma2_hand) * solve(t(X) %*% X)

# Standard Errors (Square root of diagonal of Cov(b) matrix)
se_hand <- sqrt(diag(cov_b))

#Compare with our model
print(se_hand)
print(summary(model1))

# T-Statistics and P-Values
t_stats <- b_hand/se_hand
p_values <- 2 * pt(-abs(t_stats), T - K)

m <- cbind(t_stats, p_values)
colnames(m) <- c("t_stat", "p_value")
m
summary(model1)

# Manual Hypothesis Testing (F-Test)
R <- matrix(c(0,1,1), nrow = 1, ncol = 3)
r <- 1
J <- 1

Rb_min_r <- (R %*% b_hand) - r
mid_value <- solve(R %*% solve((t(X) %*% X)) %*% t(R))

F_stat_hand <- (t(Rb_min_r) %*% mid_value %*% (Rb_min_r)) / (J*sigma2_hand)

p_value <- 1 - pf(F_stat_hand, J, T-K)

paste(F_stat_hand, p_value)


# GLS - bGLS = (X′Ψ^−1X)^−1 X′Ψ^−1y
# rho = regress today's residual on yesterday's residual
e_t <- e_hand[2:nrow(e_hand)]
e_t_minus_1 <- e_hand[1:nrow(e_hand)-1]

numerator <- sum(e_t * e_t_minus_1)
denominator <- sum((e_t_minus_1^2))

rho <- numerator/denominator
print(rho)

# Create the correlation matrix
T <- nrow(X)

exponent_matrix <- abs(outer(1:T, 1:T, "-"))
head(exponent_matrix, 3)

psi <- rho^exponent_matrix
head(psi, 3)

psi_inv <- solve(psi)
head(psi_inv, 3)

print(psi_inv[1:5, 1:5])

# Calculate b_GLS
b_gls <- solve(t(X) %*% psi_inv %*% X) %*% (t(X) %*% psi_inv %*% Y)
print(b_gls)

# Non Linear Least Squares
# βnew=βold+Δβ
# Δβ=(Z′Z)^−1Z′e
# Keep updating β until it stops changing (converges)
Y_raw <- germany_data$gdp
L_raw <- germany_data$labor
K_raw <- germany_data$capital

beta_current <- c(exp(b_hand[1]), b_hand[2], b_hand[3])

tol <- 1e-6
max_iter <- 50
diff <- 100
iter <- 0

while (diff > tol && iter < max_iter) {
  iter <- iter + 1
  
  A_curr  <- beta_current[1]
  bL_curr <- beta_current[2]
  bK_curr <- beta_current[3]
  
  # f=A L^βL K^βK
  pred <- A_curr * (L_raw ^ bL_curr) * (K_raw ^ bK_curr)
  
  resid_nls <- Y_raw - pred
  
  d_A  <- pred / A_curr
  d_bL <- pred * log(L_raw)
  d_bK <- pred * log(K_raw)
  Z <- cbind(d_A, d_bL, d_bK)
  
  ZtZ <- t(Z) %*% Z
  Zte <- t(Z) %*% resid_nls
  delta_beta <- solve(ZtZ) %*% Zte
  
  beta_new <- beta_current + as.vector(delta_beta)
  
  diff <- max(abs(beta_new - beta_current))
  beta_current <- beta_new
  
  # Optional: Print progress
  if(iter %% 5 == 0) cat("Iteration:", iter, "Diff:", diff, "\n")
}
cat("Final Parameters:", beta_current)
model_nls
