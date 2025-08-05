# Create the dataset
row = rep(1:4, each = 4)  # Define rows
column = rep(1:4, times = 4)  # Define columns
trt = c("B", "D", "C", "A", "C", "A", "D", "B", "A", "C", "B", "D", "D", "B", "A", "C")  # Treatments
obs = c(1.64, 1.210, 1.425, 1.345, 1.475, 1.185, 1.4, 1.29, 1.670, 0.710, 1.665, 1.18, 1.565, 1.290, 1.655, 0.66)  # Observations

data <- data.frame(row, column, trt, obs)
data

# Calculate treatment sums
treat.sum <- tapply(data$obs, data$trt, sum)

# Calculate row sums
row.sum <- tapply(data$obs, data$row, sum)

# Calculate column sums
col.sum <- tapply(data$obs, data$column, sum)

# Total sum of observations
total <- sum(data$obs)

# Constants for calculations
N <- 16  # Total number of observations
p <- 4   # Number of rows/columns

# Correction Factor
CF <- (total^2) / N

# Total Sum of Squares (SST)
SST <- sum((data$obs)^2) - CF

# Treatment Sum of Squares (SSTr)
SSTr <- sum(treat.sum^2 / p) - CF

# Row Sum of Squares (SSR)
SSR <- sum(row.sum^2 / p) - CF

# Column Sum of Squares (SSC)
SSC <- sum(col.sum^2 / p) - CF

# Error Sum of Squares (SSE)
SSE <- SST - SSTr - SSR - SSC

# Degrees of freedom
dfr <- p - 1           # Degrees of freedom for rows
dfc <- p - 1           # Degrees of freedom for columns
dft <- N - 1           # Total degrees of freedom
dftr <- p - 1          # Degrees of freedom for treatments
dfe <- (p - 1) * (p - 2)  # Degrees of freedom for error

# Mean Squares
mst <- SST / dft       # Mean square total
msr <- SSR / dfr       # Mean square for rows
msc <- SSC / dfc       # Mean square for columns
mstr <- SSTr / dftr    # Mean square for treatments
mse <- SSE / dfe       # Mean square for error

# F-Statistic
F <- mstr / mse  # F-statistic for treatments
F

# Critical F-Value
F.not <- qf(0.95, dftr, dfe)  # Critical value at 95% confidence level
F.not

# Decision Rule
ifelse(F.not < F, "Rejected", "Cannot be rejected")

# Perform ANOVA
data$row <- as.factor(data$row)
data$column <- as.factor(data$column)
analysis <- aov(obs ~ trt + row + column, data)
summary(analysis)



# Practice data
data <- data.frame(
  row = factor(rep(c("N", "NC", "SC", "S"), each = 4)),  # Factor for rows
  column = factor(rep(c("E", "EC", "WC", "W"), times = 4)),  # Factor for columns
  treat = factor(c("C", "A", "B", "D", 
                   "A", "B", "D", "C", 
                   "B", "D", "C", "A", 
                   "D", "C", "A", "B")),  # Factor for treatments
  obs = c(26.7, 19.7, 29, 29.8, 
          23.1, 21.7, 24.9, 29, 
          29.3, 20.1, 29, 27.3, 
          25.1, 17.4, 28.7, 35.1)  # Observations
)
