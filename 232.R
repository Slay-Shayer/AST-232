
# Unequal CRD
res <- c(2,2.2,1.8,2.3,1.7,1.7,1.9,1.5,2,2.4,2.7,2.5,2.4,2.1,2.2,2.2,1.9)
treat <- rep(c('A', 'B', 'C', 'D'), c(5, 3, 5, 4))
unequal_data <- data.frame(treat, res)
unequal_data

treat.sum <- tapply(res, treat, sum)
treat.mean <- tapply(res, treat, mean)

n <- c(5, 3, 5, 4)
a <- 4
N <- length(res)

cf <- sum(res) ^ 2 / N


sst <- sum(res^2) - cf
sst

sstreat <- sum((treat.sum^2) / n) - cf
sstreat

dftreat <- a - 1
dfe <- N - a

mstreat <- sstreat / dftreat
mstreat

sse <- sst - sstreat
mse <- sse / dfe


f.calc <- mstreat / mse
f.calc

f.tab <- qf(0.05, dftreat, dfe)
f.tab

ifelse(f.calc > f.tab, 'Rejected', 'Not Rejected')

# direct calculation

unequal_data$treat <- as.factor(unequal_data$treat)
analysis <- aov(res ~ treat, unequal_data)
summary(analysis)


# RCBD

raw_data <- c(5.1, 5.4, 5.3, 4.7, 5.3, 6, 4.7, 4.3, 5.3, 5.7, 5.5, 4.7, 5.2, 4.8, 5, 4.4, 4.8, 4.8, 4.4, 4.7, 5.3, 4.5, 4.9, 4.1)
treat <- rep(c(25, 50, 75, 100, 125, 150), each = 4)
block <- rep(c(1, 2, 3, 4), times = 6)
a <- 6
b <- 4
N <- a*b

rcbd <- data.frame(treat, block, raw_data)
rcbd

treat.sum <- tapply(raw_data, treat, sum)
block.sum <- tapply(raw_data, block, sum)
cf <- sum(raw_data) ^ 2 / N

sstot <- sum(raw_data^2) - cf
sstot

sst <- sum((treat.sum ^ 2) / b) - cf
sst


ssb <- sum((block.sum ^ 2) / a) - cf 
ssb


sse <- sstot - sst - ssb
sse


dft <- a -1
dfb  <- b - 1
dfe <- dft*dfb

mst <- sst / dft
msb <- ssb / dfb
mse <- sse / dfe

fa <- mst / mse
fb <- msb / mse

fcalc.1 <- qf(0.05, dft, dfe)
fcalc.2 <- qf(0.05, dfb, dfe)

ifelse(fa > fcalc.1, 'Treatment hypothesis rejected', 'May not Reject Treatment hypothesis')
ifelse(fb > fcalc.1, 'Block hypothesis rejected', 'May not Reject Block hypothesis')


rcbd$treat <- as.factor(rcbd$treat)
rcbd$block <- as.factor(rcbd$block)
analysis_rcbd <- aov(raw_data ~ treat + block, rcbd)
summary(analysis_rcbd)