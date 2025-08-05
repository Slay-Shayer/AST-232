############################# Example 1 ##################################

# Dataset creating
treat <- rep(c(1,2,3), each=3)
obs <- c(4,5,6,9,10,11,8,11,8)
data <- data.frame(treat, obs)
data

a <- 3
n <- 3
N <- a*n

# For each treatment sum and mean calculating
treat.sum <- tapply(obs, treat, sum)
treat.sum

treat.mean <- tapply(obs, treat, mean)
treat.mean

# Treatment effect
treat.mean - mean(data$obs)

############################# Example 2 ##################################
treat <- rep(c("A","B","C"), each=4)
obs <- c(23, 36, 31, 33, 42, 26, 47, 34, 47, 43, 43, 39)
data <- data.frame(treat, obs)
data

a <- 3
n <- 4
N <- a*n

# Calculation
cf <- (sum(obs))^2/ N
cf

sst <- sum(obs^2)- cf
sst

treat.sum <- tapply(obs, treat, sum)
treat.sum
sstreat <- sum((treat.sum^2)/n)- cf
sstreat

sse <- sst-sstreat
sse

dft <- N-1
dftreat <- a-1
dfe <- N-a

mse <- sse/dfe
mstreat <- sstreat/dftreat

f <- mstreat/mse
f
f.tab <- qf(0.05, dftreat, dfe) #alpha=0.05
ifelse(f>f.tab, "Reject the null", "May not reject")

# Intuitive idea
help(boxplot)
boxplot(obs~treat, data)

# Direct calculation
data$treat<-as.factor(data$treat)
analysis <- aov(obs~treat)
summary(analysis)

############### Example 3: Unequal observation ##########################
res <- c(2,2.2,1.8,2.3,1.7,1.7,1.9,1.5,2,2.4,2.7,2.5,2.4,2.1,2.2,2.2,1.9)
treat <- rep(c("A","B","C","D"),c(5,3,5,4))
data <- data.frame(treat, res)
data
a<- 4
n<- c(5,3,5,4)
N <- length(res)

cf <- (sum(res))^2/N
sst <- sum(res^2)-cf

treat_total <- tapply(res,treat,sum)
ss_treat<- sum((treat_total^2)/n) - cf

sse <- sst- ss_treat

ms_treat <- ss_treat/(a-1)
mse <- sse/ (N-a)

f_cal <- ms_treat/mse
f_tab_5 <- qf(0.05,3,16,lower.tail=F) ##5%
f_tab_1 <- qf(0.01,3,16,lower.tail=F) ##1%

ifelse (f_cal>f_tab_5, "Reject H0","fail to reject H0")


############### Example 4: Etch Rate ##########################
load("Etch.RData")
Etch

# Boxplot for Visual Analysis
boxplot(etch_rate ~ power, data = Etch)

# Perform ANOVA for CRD
crd_result <- aov(etch_rate ~ power, data = Etch)

# Display ANOVA Table
summary(crd_result)

# Model Adequacy Check
treat_mean <- tapply(Etch$etch_rate, Etch$power, mean)
treat_estimate <- rep(treat_mean, each = 5)
residual <- Etch$etch_rate - treat_estimate
residual

residuals(crd_result) # Short

qqnorm(residual)
qqline(residual)

plot(fitted(crd_result), residuals(crd_result))

# Mean Comparisons
# Tukey's Honest Significant Difference Test
TukeyHSD(crd_result)
