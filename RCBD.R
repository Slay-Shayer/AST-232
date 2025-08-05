# Define raw data
raw_data <- c(5.1, 5.4, 5.3, 4.7, 5.3, 6, 4.7, 4.3, 5.3, 5.7, 5.5, 4.7, 5.2, 4.8, 5, 4.4, 4.8, 4.8, 4.4, 4.7, 5.3, 4.5, 4.9, 4.1)

# Define treatments and blocks
treat <- rep(c(25, 50, 75, 100, 125, 150), each = 4)  # 6 treatments, 4 replicates each
block <- rep(c(1, 2, 3, 4), times = 6)  # 4 blocks repeated for each treatment

# Create data frame
data <- data.frame(raw_data, treat, block)
data

# Set dimensions for the design
a <- 6  # Number of treatments
b <- 4  # Number of blocks
N <- a * b  # Total number of observations

# Calculate correction factor
cf <- (sum(raw_data))^2 / N

# Calculate total sum of squares (SST)
sst <- sum((raw_data)^2) - cf

# Calculate treatment sum of squares (SSTreat)
treat_tot <- tapply(raw_data, treat, sum)  # Total per treatment
sstreat <- sum(treat_tot^2 / b) - cf

# Calculate block sum of squares (SSB)
block_tot <- tapply(raw_data, block, sum)  # Total per block
ssb <- sum(block_tot^2 / a) - cf

# Calculate error sum of squares (SSE)
sse <- sst - sstreat - ssb

# Degrees of freedom
df1 <- a - 1        # Degrees of freedom for treatments
df2 <- b - 1        # Degrees of freedom for blocks
df_e <- (a - 1) * (b - 1)  # Degrees of freedom for error
df_tot <- N - 1     # Total degrees of freedom

# Mean squares
mstot <- sst / df_tot  # Total mean square
mse <- sse / df_e      # Error mean square
mst <- sstreat / df1   # Treatment mean square
msb <- ssb / df2       # Block mean square

# F-values
f_not <- mst / mse  # F-value for treatments
f_not
f_tab <- qf(0.05, df1, df_e, lower.tail = FALSE)  # Critical F-value for treatments
f_tab

# Decision for treatments
treatment_decision <- ifelse(f_not > f_tab, "H0 may be rejected", "H0 may not be rejected")

# F-value for blocks
f_not_b <- msb / mse  # F-value for blocks
f_tab_b <- qf(0.05, df2, df_e, lower.tail = FALSE)  # Critical F-value for blocks

# Decision for blocks
block_decision <- ifelse(f_not_b > f_tab_b, "H0 may be rejected", "H0 may not be rejected")

# ANOVA summary using built-in function
data$block <- as.factor(data$block)  # Convert block to a factor
data$treat <- as.factor(data$treat)  # Convert treat to a factor

# Perform ANOVA
analysis <- aov(raw_data ~ treat + block, data)
summary(analysis)

# Graft Experiment
load("graft_data.RData")
graft_data

is.factor(graft_data$pressure)
is.factor(graft_data$batch)

graft_data$batch <- as.factor(graft_data$batch)
is.factor(graft_data$batch)

analysis <- aov(strength ~ pressure + batch, graft_data)
summary(analysis)

plot(analysis)

# Overall mean
overall_mean <- mean(graft_data$strength)

# Estimating treatment effect
treat_mean <- tapply(graft_data$strength, graft_data$pressure, mean)  # Total per treatment
treat_effect <- treat_mean - overall_mean

# Estimating block effect
block_mean <- tapply(graft_data$strength, graft_data$batch, mean)  # Total per block
block_effect <- block_mean - overall_mean

# Residual
treat <- rep(treat_mean, each = 6)  
block <- rep(block_mean, times = 4)  
residual <- graft_data$strength - treat - block + overall_mean

residuals(analysis)

# Practice data
# Define raw data: Observations for analysis
raw_data <- c(5.1, 5.4, 5.3, 4.7, 5.3, 6, 4.7, 4.3, 5.3, 5.7, 
              5.5, 4.7, 5.2, 4.8, 5.0, 4.4, 4.8, 4.8, 4.4, 4.7, 
              5.3, 4.5, 4.9, 4.1)

# Define treatments: 6 treatment levels (25, 50, ..., 150) with 4 replicates each
treat <- rep(c(25, 50, 75, 100, 125, 150), each = 4)

# Define blocks: 4 blocks repeated for each treatment
block <- rep(c(1, 2, 3, 4), times = 6)

# Combine the data into a data frame
data <- data.frame(raw_data, treat, block)

