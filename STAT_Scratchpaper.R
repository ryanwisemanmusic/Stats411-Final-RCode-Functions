### Thank you to Javier Hernandez for giving me this integral document that
### is the basis for everything we've gone over this semester. It means the
### amount of hunting I need to do to find good R-Code approaches is a minimal


############### Scratch Paper




############## Scratch End

# ===============================
# STAT 411 — R FUNCTIONS
# ===============================

x<-c(2,3,5,3,4,6)
y<-c(125,138,116,121,136,115)

# You can plot using
plot(x,y)
# or
plot(x,y,
     xlab = "age",
     ylab = "weight",
     main = "Scatter")

# find the correlation
cor(x,y)
# to square it use
cor(x,y)*cor(x,y)

# shows the intercept and slope
model<-lm(y~x)
model

# --------------------------------
# CORE RULE (ALL DISTRIBUTIONS)
# --------------------------------
# d___ → PDF / PMF (exact value)
# p___ → CDF (P(X ≤ x))
# q___ → inverse (find x from probability)
# r___ → random generation


# --------------------------------
# NORMAL DISTRIBUTION
# X ~ N(mu, sigma)
# --------------------------------

# P(X ≤ x)
pnorm(x, mean=mu, sd=sigma)

# P(X > x)
1 - pnorm(x, mean=mu, sd=sigma)

# P(a < X < b)
pnorm(b, mu, sigma) - pnorm(a, mu, sigma)

# PDF (density at x)
dnorm(x, mean=mu, sd=sigma)

# Find z or x given probability
qnorm(p, mean=mu, sd=sigma)

# Random values
rnorm(n, mean=mu, sd=sigma)


# --------------------------------
# STANDARD NORMAL (Z ~ N(0,1))
# --------------------------------

pnorm(z)        # P(Z ≤ z)
1 - pnorm(z)    # P(Z > z)
qnorm(p)        # find z from probability


# --------------------------------
# BINOMIAL DISTRIBUTION
# X ~ Bin(n, p)
# --------------------------------

# P(X = x)
dbinom(x, size=n, prob=p)

# P(X ≤ x)
pbinom(x, size=n, prob=p)

# P(X > x)
1 - pbinom(x, n, p)

# Inverse
qbinom(p, size=n, prob=p)


# --------------------------------
# UNIFORM DISTRIBUTION
# X ~ U(a, b)
# --------------------------------

# PDF
dunif(x, min=a, max=b)

# P(X ≤ x)
punif(x, min=a, max=b)

# P(a < X < b)
punif(b, a, b) - punif(a, a, b)

# Inverse
qunif(p, min=a, max=b)


# --------------------------------
# EXPONENTIAL DISTRIBUTION
# X ~ Exp(lambda)
# --------------------------------

dexp(x, rate=lambda)
pexp(x, rate=lambda)
qexp(p, rate=lambda)


# --------------------------------
# POISSON DISTRIBUTION
# X ~ Pois(lambda)
# --------------------------------

dpois(x, lambda=lambda)
ppois(x, lambda=lambda)
1 - ppois(x, lambda)
qpois(p, lambda=lambda)


# --------------------------------
# t DISTRIBUTION
# T ~ t(df)
# --------------------------------

pt(x, df)            # P(T ≤ x)
1 - pt(x, df)        # P(T > x)
qt(p, df)            # critical t value


# --------------------------------
# CHI-SQUARE DISTRIBUTION
# X^2 ~ ChiSq(df)
# --------------------------------

pchisq(x, df)
1 - pchisq(x, df)
qchisq(p, df)


# --------------------------------
# F DISTRIBUTION
# F ~ F(df1, df2)
# --------------------------------

pf(x, df1, df2)
1 - pf(x, df1, df2)
qf(p, df1, df2)


# --------------------------------
# COMMON CRITICAL VALUES
# --------------------------------

# Z critical values
qnorm(0.90)    # ~1.28 (80% CI)
qnorm(0.95)    # ~1.645 (90% CI)
qnorm(0.975)   # ~1.96 (95% CI)
qnorm(0.995)   # ~2.576 (99% CI)

# t critical value
qt(0.975, df)  # 95% CI

# Chi-square critical
qchisq(p, df)


# --------------------------------
# QUICK PROBABILITY PATTERNS
# --------------------------------

# Less than
pnorm(a, mu, sigma)

# Greater than
1 - pnorm(a, mu, sigma)

# Between
pnorm(b, mu, sigma) - pnorm(a, mu, sigma)

###################### NOTES
# Normal distribution probability
mean <- 5
sd <- 1
x <- 3

pnorm(x, mean, sd)

######
pnorm(15, 17.64, 12.1) - pnorm(12,17.64, 12.1)

#standardization of x, standard normal distribution
# P(z <= 2) = pnorm(2)
pnorm(.029, lower.tail = FALSE)



################### Scratch Paper Ends Here ###################



## group data EXAMPLE

text_data <- "
Lower   Upper   Freq
8       12      5
13      17      9
18      22      14
23      27      8
28      32      4
"

data <- read.table(text = text_data, header = TRUE)

# set limits/ Can also import from a table
lower <-data$Lower
upper <-data$Upper
f     <-data$Freq # set frequency 

m <- (lower + upper) / 2 # midpoints
m # print midpoint
n <- sum(f) # summation of frequency (sample size)

m*f # multiply midpoint by frequency
sum(m*f) # sums the m*f data
n<- sum(f) # sets n to the sum of total frequencies
xbar_g <- sum(f * m) / n # gets the sample group mean
xbar_g # prints SAMPLE GROUP MEAN
m - xbar_g # midpoint - xbar(mean)
(m - xbar_g)^2 #squared deviations
(m - xbar_g)^2 * f # weighted squares: f*(m - x̄)^2
SS <- sum((m - xbar_g)^2 * f) # SS = Σ f*(m - x̄)^2
SS # check squared sums (SS)
s2 <- SS / (n - 1) # sample variance: s^2 = SS/(n-1)
s2 # GROUP SAMPLE VARIANCE
s  <- sqrt(s2) # sample standard deviation: s = √s^2
s # GROUP STANDARD DEVIATION

###############################################################
# STAT 411 R FORMULAS
# Organized by problem type: raw data, grouped data, proportions,
# probability, counting, and quick templates for tables.
#
# Notes:
# - var(x) and sd(x) in R are SAMPLE variance/SD (divide by n-1).
# - Population variance/SD are shown as conversions.
# - For quartiles/percentiles, your class often uses type=2.
###############################################################

###############################################################
# 0) Quick Setup (optional)
###############################################################
rm(list = ls())     # clear environment
options(scipen=999) # avoid scientific notation when possible

###############################################################
# 1) Creating & Viewing Data (RAW DATA)
###############################################################
# Paste a dataset as a vector:
x <- c(16,18,20,21,23,23,24,32,36,42)
x

# Basic checks:
length(x)     # n
sort(x)       # sorted
summary(x)    # min, quartiles, median, mean, max

###############################################################
# 2) Measures of Location (Center)
###############################################################
mean(x)                 # arithmetic mean
median(x)               # median

# Trimmed mean: trim = proportion removed from EACH tail
mean(x, trim = 0.2)     # 20% trimmed mean (10% from each tail)

# Mode (works for one or multiple modes)
tbl <- table(x)
names(tbl)[tbl == max(tbl)]   # modes (could return multiple)

###############################################################
# 3) Measures of Dispersion (Spread)
###############################################################
min(x)
max(x)
range(x)                # returns (min, max)
diff(range(x))          # range width

var(x)                  # SAMPLE variance (n-1)
sd(x)                   # SAMPLE standard deviation

# Population variance and SD (convert from sample):
pop_var <- var(x) * (length(x) - 1) / length(x)
pop_sd  <- sqrt(pop_var)

pop_var
pop_sd

# Mean absolute deviation from the mean (MAD-from-mean, not R's mad())
mad_mean <- mean(abs(x - mean(x)))
mad_mean

###############################################################
# 4) Coefficient of Variation (Relative Spread)
###############################################################
cv_percent <- sd(x) / mean(x) * 100
cv_percent

###############################################################
# 5) Quantiles / Percentiles / Relative Position
###############################################################
# Your course often uses type=2; keep it consistent unless told otherwise.
quantile(x, probs = 0.25, type = 2)   # Q1
quantile(x, probs = 0.50, type = 2)   # median (same as median(x))
quantile(x, probs = 0.75, type = 2)   # Q3
quantile(x, probs = 0.80, type = 2)   # 80th percentile

# Percentile rank of a value using ECDF:
F <- ecdf(x)
F(23) * 100

# IQR (be careful: base IQR() ignores type=2; do it manually for type=2)
Q1 <- quantile(x, 0.25, type = 2)
Q3 <- quantile(x, 0.75, type = 2)
IQR_type2 <- Q3 - Q1
IQR_type2

###############################################################
# 6) Outliers (1.5 x IQR Rule)
###############################################################
lower_fence <- Q1 - 1.5 * IQR_type2
upper_fence <- Q3 + 1.5 * IQR_type2
lower_fence
upper_fence

outliers <- x[x < lower_fence | x > upper_fence]
outliers

###############################################################
# 7) Z-Scores
###############################################################
# Use sample mean/SD for sample-based z:
z_score <- (x[1] - mean(x)) / sd(x)
z_score

# Or for known population parameters:
x_val <- 85
mu <- 70
sigma <- 8
z <- (x_val - mu) / sigma
z

###############################################################
# 8) Chebyshev’s Theorem (at least proportion within k SDs)
###############################################################
# If k is known:
k <- 2
at_least_within <- 1 - 1/(k^2)
at_least_within

# If an interval [lower, upper] is given around mean:
mean_val <- 3.60
sd_val <- 0.09
lower_bound <- 3.33
upper_bound <- 3.87

k_from_interval <- min(abs(lower_bound - mean_val), abs(upper_bound - mean_val)) / sd_val
1 - 1/(k_from_interval^2)

###############################################################
# 9) Empirical Rule (Normal Data Only) - quick intervals
###############################################################
mu <- mean(x)
sigma <- sd(x)

c(mu - sigma,  mu + sigma)       # ~68%
c(mu - 2*sigma, mu + 2*sigma)    # ~95%
c(mu - 3*sigma, mu + 3*sigma)    # ~99.7%

###############################################################
# 10) Frequency Tables (Raw Data)
###############################################################
x2 <- c(1,1,2,3,3,3)
table(x2)                 # frequency
prop.table(table(x2))     # relative frequency
cumsum(table(x2))         # cumulative frequency

###############################################################
# 11) Weighted Mean
###############################################################
weights <- c(0.2, 0.3, 0.1, 0.4)
observations <- c(79, 85, 90, 91)
weighted.mean(observations, weights)

###############################################################
# 12) GROUPED DATA (Class Intervals + Frequencies)
# Covers grouped mean, grouped sample variance/SD, and population versions.
###############################################################

# --- TEMPLATE A: Paste grouped table using read.table (spaces) ---
text_data <- "
Lower   Upper   Freq
115.6   119.4   14
119.5   123.3   18
123.4   127.2   18
127.3   131.1   42
131.2   135.0   16
"
g <- read.table(text = text_data, header = TRUE, sep = "")
g

lower <- g$Lower
upper <- g$Upper
f     <- g$Freq

m <- (lower + upper) / 2      # midpoints
n <- sum(f)                   # total frequency (sample size)

# Grouped mean:
xbar_g <- sum(f * m) / n
xbar_g

# Grouped SAMPLE variance (deviation form):
s2_g_dev <- sum(f * (m - xbar_g)^2) / (n - 1)
s2_g_dev

# Grouped SAMPLE variance (computational shortcut):
s2_g_comp <- (sum(f * m^2) - (sum(f * m)^2) / n) / (n - 1)
s2_g_comp

# Grouped SAMPLE SD:
s_g <- sqrt(s2_g_comp)
s_g

# Grouped POPULATION variance/SD (treating grouped as population):
sigma2_g <- sum(f * (m - xbar_g)^2) / n
sigma_g  <- sqrt(sigma2_g)

sigma2_g
sigma_g

# Sanity check: the two sample variance methods should match (up to rounding)
all.equal(s2_g_dev, s2_g_comp)

# --- TEMPLATE B: Paste grouped table using tabs (sep="\\t") ---
text_data_tab <- "
Lower\tUpper\tFreq
10\t19\t3
20\t29\t7
30\t39\t6
40\t49\t4
"
g2 <- read.table(text = text_data_tab, header = TRUE, sep = "\t")
g2

###############################################################
# 13) GROUPED DATA (Replication method) - If instructor wants it
# This turns grouped midpoints into a raw-like vector by repeating each midpoint f times.
###############################################################
# WARNING: Only appropriate if the problem/instructor accepts midpoint-replication.
# For very large frequencies, this can be slow/memory-heavy.

data_grouped <- rep(m, f)
mean(data_grouped)
var(data_grouped)  # sample variance of replicated data
sd(data_grouped)

###############################################################
# 14) Missing Value from Known Mean
###############################################################
known_mean <- 39.77
known_values <- c(46.06,39.70,47.29,39.13,44.66,38.53,46.89)
n_total <- 8

missing_value <- (known_mean * n_total) - sum(known_values)
missing_value

###############################################################
# 15) Proportions and Percentages
###############################################################
# Sample proportion:
x_success <- 78
n_total <- 240
p_hat <- x_success / n_total
p_hat
p_hat * 100   # percent

# Percent change:
old <- 48
new <- 60
pct_change <- (new - old) / old * 100
pct_change

###############################################################
# 16) INTRO PROBABILITY (RANDOMNESS + BASIC)
###############################################################
# Complement:
P_A <- 0.27
P_Ac <- 1 - P_A
P_Ac

# Addition rule:
P_A <- 0.55
P_B <- 0.40
P_A_and_B <- 0.18
P_A_or_B <- P_A + P_B - P_A_and_B
P_A_or_B

# Multiplication rule (independent):
P_A <- 0.65
P_all_7 <- P_A^7
P_all_7

# Multiplication rule (dependent example, without replacement):
P_both_red <- (5/10) * (4/9)
P_red_then_blue <- (5/10) * (3/9)
P_both_red
P_red_then_blue

# Independence check:
P_A <- 0.6
P_B <- 0.5
P_A_and_B <- 0.25
is_independent <- abs(P_A_and_B - (P_A * P_B)) < 1e-12
is_independent

###############################################################
# 17) Counting: Permutations & Combinations
###############################################################
# n!:
factorial(7)

# nPr = n! / (n-r)!
n <- 10; r <- 3
nPr <- factorial(n) / factorial(n - r)
nPr

# nCr = n! / (r!(n-r)!)
nCr <- choose(14, 4)
nCr

# Multiset / repetition: number of length-k strings from m symbols:
m <- 10
k <- 4
m^k

# "At least one" via complement (e.g., at least one 7 in 4 digits):
total <- 10^4
no7 <- 9^4
at_least_one7 <- total - no7
at_least_one7

###############################################################
# 18) Bayes' Theorem (Template)
###############################################################
# P(A|B) = P(B|A)P(A) / P(B)
# Example template:
P_A <- 0.02          # prior
P_B_given_A <- 0.95  # sensitivity
P_B_given_notA <- 0.08  # false positive rate

P_B <- P_B_given_A * P_A + P_B_given_notA * (1 - P_A)
P_A_given_B <- (P_B_given_A * P_A) / P_B

P_B
P_A_given_B

###############################################################
# 19) Data Frames & Two-Way Tables (Categorical)
###############################################################
aosi_data <- data.frame(
  Gender = c("Female", "Male", "Female", "Male", "Female", "Male"),
  Study_Site = c("PHI", "SEA", "STL", "UNC", "PHI", "SEA")
)

xtabs(~ Gender + Study_Site, data = aosi_data)

###############################################################
# 20) Quick Plots (shape checks)
###############################################################
hist(x)
boxplot(x)

###############################################################
# 21) Safe NA Handling
###############################################################
y <- c(1, 2, NA, 4)
mean(y, na.rm = TRUE)
var(y, na.rm = TRUE)
sd(y,  na.rm = TRUE)

###############################################################
# 1) DISCRETE PROBABILITY DISTRIBUTION FROM (x, p)
###############################################################

# Example: fair die
x <- 1:6
p <- rep(1/6, 6)

# Check distribution validity:
all(p >= 0 & p <= 1)         # must be TRUE
sum(p)                       # must be 1 (or very close due to rounding)

# Expected Value (Mean of a discrete RV): mu = E(X) = Σ x p(x)
mu <- sum(x * p)
mu

# Variance (definition): Var(X) = Σ (x - mu)^2 p(x)
var_def <- sum((x - mu)^2 * p)
var_def

# Variance (computational shortcut): Var(X) = Σ x^2 p(x) - mu^2
var_comp <- sum(x^2 * p) - mu^2
var_comp

# Standard deviation:
sd_pop <- sqrt(var_def)
sd_pop

###############################################################
# 2) CUMULATIVE PROBABILITIES FROM A TABLE (DISCRETE)
###############################################################

# P(X < 4) = P(1)+P(2)+P(3)
sum(p[x < 4])

# P(X >= 5)
sum(p[x >= 5])

# P(2 <= X <= 5)
sum(p[x >= 2 & x <= 5])

###############################################################
# 3) EXPECTED VALUE & VARIANCE FOR A GENERAL TABLE (YOUR 7.2 STYLE)
###############################################################

# Investment example template:
profit <- c(-2000, 0, 1000, 2000, 4000)
prob   <- c(0.2,   0.1, 0.3,  0.3,  0.1)

sum(prob)                    # should be 1
muA <- sum(profit * prob)
muA

varA <- sum((profit - muA)^2 * prob)
sdA  <- sqrt(varA)
varA
sdA

# Shortcut variance:
varA2 <- sum(profit^2 * prob) - muA^2
varA2

###############################################################
# 4) CHECK IF A FUNCTION DEFINES A VALID PMF (LIKE EX 7.1.9)
###############################################################

# Example: P(X=x)=x^2/30 for x=1,2,3,4
x <- 1:4
p <- x^2 / 30

p
sum(p)                        # must be 1
all(p >= 0 & p <= 1)          # must be TRUE

###############################################################
# 5) GEOMETRIC DISTRIBUTION (EX 7.1.7)
###############################################################
# X = number of trials until first success
# PMF: P(X=x) = p * (1-p)^(x-1) for x=1,2,3,...

p_success <- 1/6

# P(X = 1), P(X = 2), ...
x <- 1:8
px_manual <- p_success * (1 - p_success)^(x - 1)
px_manual

# R's geometric distribution uses a DIFFERENT convention:
# In R, dgeom(k, p) is P(#failures before first success = k), k=0,1,2,...
# If X = trial number of first success, then k = X-1.
# So: P(X = x) = dgeom(x-1, p)

x <- 1:8
px_r <- dgeom(x - 1, prob = p_success)
cbind(x, px_manual, px_r)

# Example from notes: P(X < 4) = P(X=1)+P(X=2)+P(X=3)
p_less_4_manual <- sum(p_success * (1 - p_success)^( (1:3) - 1 ))
p_less_4_manual

# Using R: P(X < 4) means X <= 3  => k <= 2 failures
# P(X <= 3) = P(k <= 2) = pgeom(2, p)
p_less_4_r <- pgeom(2, prob = p_success)
p_less_4_r

###############################################################
# 6) BINOMIAL DISTRIBUTION (dbinom, pbinom, qbinom, rbinom)
###############################################################
# Binomial: X = number of successes in n trials, success prob = p
# PMF: P(X = x) = dbinom(x, size=n, prob=p)
# CDF: P(X <= x) = pbinom(x, size=n, prob=p)

n <- 10
p <- 0.3

# P(X = 4)
dbinom(4, size = n, prob = p)

# P(X <= 4)
pbinom(4, size = n, prob = p)

# P(X < 4) = P(X <= 3)
pbinom(3, size = n, prob = p)

# P(X >= 4) = 1 - P(X <= 3)
1 - pbinom(3, size = n, prob = p)

# P(2 <= X <= 6) = P(X<=6) - P(X<=1)
pbinom(6, n, p) - pbinom(1, n, p)

# Vector of probabilities for x=0..n
x <- 0:n
dbinom(x, n, p)

# Quantile: smallest x such that P(X <= x) >= 0.95
qbinom(0.95, size = n, prob = p)

# Simulate 10000 binomial observations
set.seed(1)
sim <- rbinom(10000, size = n, prob = p)
mean(sim)      # should be close to E(X)=n*p
var(sim)       # sample variance ~ n*p*(1-p)

###############################################################
# 7) POISSON DISTRIBUTION (dpois, ppois, qpois, rpois) RATES
###############################################################
# used for when you need to take in the rate at which something
# happens. 
# Poisson: X = number of events in a fixed interval
# Parameter lambda = expected count (mean)
# E(X)=lambda, Var(X)=lambda

lambda <- 2

# P(X = 5) when lambda=2
dpois(5, lambda = lambda)

# P(X <= 5)
ppois(5, lambda = lambda)

# P(X < 5) = P(X <= 4)
ppois(4, lambda = lambda)

# P(X >= 5) = 1 - P(X <= 4)
1 - ppois(4, lambda = lambda)

# Quantile (95th percentile)
qpois(0.95, lambda = lambda)

# Simulation
set.seed(1)
simP <- rpois(10000, lambda = lambda)
mean(simP)     # ~ lambda
var(simP)      # ~ lambda (sample version)

###############################################################
# 8) HYPERGEOMETRIC (dhyper, phyper, qhyper, rhyper)
###############################################################
# Hypergeometric models sampling WITHOUT replacement.
# R args: dhyper(x, m, n, k)
# m = # of "success" items in population
# n = # of "failure" items in population
# k = sample size
# x = # of successes in the sample

m <- 8   # successes in population
n <- 12  # failures in population
k <- 5   # draws

# P(X = 3 successes)
dhyper(3, m = m, n = n, k = k)

# P(X <= 3)
phyper(3, m = m, n = n, k = k)

# P(X >= 3) = 1 - P(X <= 2)
1 - phyper(2, m = m, n = n, k = k)

# Simulation
set.seed(1)
simH <- rhyper(10000, m = m, n = n, k = k)
mean(simH)
var(simH)

###############################################################
# 9) UNIFORM DISCRETE (like fair die) - quick helper
###############################################################
# If X is equally likely over values v:
v <- 1:6
p <- rep(1/length(v), length(v))
sum(v*p)             # expected value
sum((v - sum(v*p))^2 * p)  # variance

###############################################################
# 10) BUILD EMPIRICAL DISTRIBUTION FROM DATA (7.1.5 IDEA)
###############################################################
data_vals <- c(5,5,6,4,7,6,9,6,8,7,5,5,6,11,3,8,8,7,2,5)

# Frequency table
freq <- table(data_vals)
freq

# Empirical probabilities
p_emp <- prop.table(freq)
p_emp

# Expected value from empirical distribution:
x_vals <- as.numeric(names(p_emp))
p_vals <- as.numeric(p_emp)

mu_emp <- sum(x_vals * p_vals)
mu_emp

# Variance from empirical distribution:
var_emp <- sum((x_vals - mu_emp)^2 * p_vals)
sd_emp  <- sqrt(var_emp)
var_emp
sd_emp

###############################################################
# 11) GROUPED DATA (MEAN, VAR, SD) - MIDPOINT METHOD
###############################################################

text_data <- "
Lower   Upper   Freq
8       12      5
13      17      9
18      22      14
23      27      8
28      32      4
"
g <- read.table(text = text_data, header = TRUE, sep = "")
lower <- g$Lower
upper <- g$Upper
f     <- g$Freq

m <- (lower + upper) / 2
n <- sum(f)

# Grouped mean:
xbar_g <- sum(f * m) / n
xbar_g

# Grouped SAMPLE variance:
s2_g <- sum(f * (m - xbar_g)^2) / (n - 1)
s_g  <- sqrt(s2_g)
s2_g
s_g

# Grouped POPULATION variance:
sigma2_g <- sum(f * (m - xbar_g)^2) / n
sigma_g  <- sqrt(sigma2_g)
sigma2_g
sigma_g

###############################################################
# 12) QUICK “WHAT FUNCTION DO I USE?” GUIDE
###############################################################
# Binomial:
#   P(X=x)        -> dbinom(x, n, p)
#   P(X<=x)       -> pbinom(x, n, p)
#   P(X>=x)       -> 1 - pbinom(x-1, n, p)
#
# Poisson:
#   P(X=x)        -> dpois(x, lambda)
#   P(X<=x)       -> ppois(x, lambda)
#   P(X>=x)       -> 1 - ppois(x-1, lambda)
#
# Geometric (R uses failures k):
#   P(X = x trials until first success)
#                -> dgeom(x-1, p)
#   P(X <= x)    -> pgeom(x-1, p)
#   P(X < x)     -> pgeom(x-2, p)
#
# Hypergeometric:
#   P(X=x)        -> dhyper(x, m, n, k)
#   P(X<=x)       -> phyper(x, m, n, k)
#   P(X>=x)       -> 1 - phyper(x-1, m, n, k)
###############################################################
## Which has the least risk
# Plan A
xA <- c(20000, 25000, 75000) #value
pA <- c(0.12, 0.69, 0.19) #probability

# Plan B
xB <- c(50000, 75000, 100000)
pB <- c(0.25, 0.45, 0.3)

# function to compute EV, Var, SD for a discrete distribution
disc_stats <- function(x, p) {
  mu  <- sum(x * p)
  var <- sum((x - mu)^2 * p)      # population variance of the distribution
  sd  <- sqrt(var)
  list(mu = mu, var = var, sd = sd)
}

A <- disc_stats(xA, pA)
B <- disc_stats(xB, pB)

A
B

# Which has least risk (smaller SD)?
if (A$sd < B$sd) "Plan A has less risk" else "Plan B has less risk"
#################################################################
### Determining the expected amount of money they will make on a venture
### Its really just finding the expected value 

x <- c(0,1,2,3) # values, losses are denotes as negatives
p <- c(.25,.25,.25,.25) # probabilities

EV <- sum(x * p)

round(EV, 3) # rounding to the nearest dollar

### Determining the outcome of coin tosses
#first take the coin tosses and find the total outcomes 2^x x=tosses
#next determine how many outcomes you need to achieve, example exactly 2 heads
# HHH HHT HTT HTH TTT TTH THT TTT
# this is a 3/8 outcome
# second outcome is the compliment so 5/8
x <- c(85, -47) ### the values win or losses
p <- c(3/8, 5/8) 

EV <- sum(x * p)
round(EV, 2)
EV* 685 # if you played 685 times 
round((EV* 685), 2)
######################
#Solve the possibility of a coin and a head flip
# P(T and die >= 2)
P_coin <- 1/2
P_die_ge_2 <- 5/6   # {2,3,4,5,6} = 5 outcomes out of 6

prob <- P_coin * P_die_ge_2
round(prob, 4)
###################
#Delivery days problems
total_days <- length(1:11) # 1:12 represents 1,2,3,4,...,12
away_days  <- length(4:6)

prob <- away_days / total_days
round(prob, 4)
##################################################################
##### Discrete random variables
x <- c(1,2,3,4,5,6,7,8,9,10) ### value = x
p <- rep(.5) ### p(X-x) probability ##use rep(prob,howmany) if a number repeats

#### Expected value
mu <- sum(x * p)
mu
round(mu, 1)

#### Variance (population variance of the distribution)
variance <- sum((x - mu)^2 * p)
variance
round(variance, 1)

#### Standard deviation
sd_val <- sqrt(variance)
round(sd_val, 3) ## in case you need to round

#### Finding the value of (P(X>x)) Cumulative Probability discrete random variables
prob <- sum(p[x < -2]) ## change the operator < > <= >= and the second operand as needed
round(prob, 1)

#############################################################
# Binomial random variable
# Use dbinom when you want EXACTLY a specific amount for x
# Use pbinom when you want < > <= >= at most, at least, fewer, etc

######## Binomial random variables expected values
# E(X)
n <- 4
p <- 1/4

expected_value <- n * p
expected_value

#### Binomial Random Variables Standard Deviation
sd_value <- sqrt(n * p * (1 - p))
round(sd_value, 2)

#### compute the probability of a plant striking
n <- 97 # number of plants
p <- 0.2383292 # probability of a strike (indepdendent)
x <- 1 # x number of plants may strike
round(97/407, 4)


probability <- dbinom(x, n, p)
round(probability, 4) # round to 4

## Expected Value of X at plants
expected_value <- n * p
expected_value

## standard deviation at plants
sd_value <- sqrt(n * p * (1 - p))
round(sd_value, 3)

##### Probability of invoices will be paid in x days
n <- 19 # sample size/trial size
p <- 0.5 # percent/probability

probability <- pbinom(3, n, p) # less than 4 days = 3
round(probability, 4)

### Car Color probability would prefer a color
n <- 10 # sample size
p <- 0.5 # suppose 50% of pop likes green
x <- 4 # probability we are looking for (2/5th)
probability <- dbinom(x, n, p)
round(probability, 4)

### real estate agent properties
n <- 17 # properties
p <- 0.4 # chance

probability <- pbinom(1, n, p) 
round(probability, 4)
### Real estate agent selling at least x number
n <- 17
p <- 0.30

prob <- 1 - pbinom(6, n, p)   # P(X >= 2)
round(prob, 4)


### Template #########
n <- 14 # number of trials
p <- 0.1 # chance
x <- 1 # number you are aiming for, less than, equal too etc

# use for less than or more than
probability <- pbinom(x, n, p)
round(probability, 4)
# use for when looking for the exact
probability <- dbinom(x, n, p)
round(probability, 4)
# expected value
expected_value <- n * p
expected_value
# standard deviation
sd_value <- sqrt(n * p * (1 - p))
round(sd_value, 3)

# probability for x < y < z
n <- 14
p <- 0.20
# This is for more than 3 but less than 6, use 4 and 5 in the x spot
probability <- dbinom(6, n, p) + dbinom(7, n, p)
round(probability, 4)

############################################
# BINOMIAL DISTRIBUTION TEMPLATE
############################################

# 1. PARAMETERS
n <- 18      # number of trials
p <- 0.5     # probability of success
x <- 15       # specific number of successes

############################################
# 2. EXPECTED VALUE
############################################

EV <- n * p
EV

############################################
# 3. VARIANCE
############################################

VAR <- n * p * (1 - p)
VAR

############################################
# 4. STANDARD DEVIATION
############################################

SD <- sqrt(VAR)
SD

############################################
# 5. EXACT PROBABILITY
# P(X = x)
############################################

prob_exact <- dbinom(x, n, p)
round(prob_exact, 4)

############################################
# 6. CUMULATIVE PROBABILITY
# P(X ≤ x)
############################################

prob_cumulative <- pbinom(x, n, p)
prob_cumulative

############################################
# 7. LESS THAN
# P(X < x)
############################################

prob_less_than <- pbinom(x - 1, n, p)
round(prob_less_than, 4)

############################################
# 8. AT LEAST
# P(X ≥ x)
############################################

prob_at_least <- 1 - pbinom(x - 1, n, p)
round(prob_at_least, 4)

############################################
# 9. MORE THAN
# P(X > x)
############################################

prob_more_than <- 1 - pbinom(x, n, p)
prob_more_than

############################################
# 10. RANGE PROBABILITY
# P(a ≤ X ≤ b)
############################################

a <- 2
b <- 5

prob_range <- pbinom(b, n, p) - pbinom(a - 1, n, p)
prob_range

############################################
# 11. FULL DISTRIBUTION TABLE
############################################

values <- 0:n
distribution <- dbinom(values, n, p)

data.frame(x = values, P_of_X = distribution)
######################
#Poisson Distribution
######################
# Cars enter a car wash at a mean rate of 4 cars per half an hour. 
# What is the probability that, in any hour, exactly 2 cars will enter the 
# car wash? Round your answer to four decimal places.
# Mean rate per half hour
lambda <- 4 * 2

# Probability of exactly 3 cars
probability <- dpois(2, lambda)

# Display rounded result
round(probability, 4)

#########
# A company produces optical-fiber cable with a mean of 0.4 flaws per 100 feet. 
# What is the probability that there will be exactly 2 flaws in 800 feet of cable? 
# Round your answer to four decimal places.
# Mean number of flaws in 800 feet
lambda <- 0.3 * (900 / 100)

# Probability of exactly 2 flaws
probability <- dpois(3, lambda)

# Rounded result
round(probability, 4)

########
# The auto parts department of an automotive dealership sends out a mean of 6.6 
# special orders daily. What is the probability that, for any day, the number of 
# special orders sent out will be exactly 3? Round your answer to four decimal places.
# Mean special orders per day
lambda <- 6.2

# Probability of exactly 3 orders
probability <- dpois(4, lambda)

# Rounded result
round(probability, 4)

########
# Suppose that on the average, 5 students enrolled in a small liberal arts 
# college have their automobiles stolen during the semester. What is the 
# probability that less than 3 students will have their automobiles stolen 
# during the current semester? Round your answer to four decimal places.
# Mean per semester
lambda <- 8

# Probability of fewer than 3 stolen cars
probability <- ppois(1, lambda)

# Rounded result
round(probability, 4)

##########
# The computer that controls a bank's automatic teller machine crashes a mean 
# of 0.4 times per day. What is the probability that, in any seven-day week, 
# the computer will crash more than 2 times? Round your answer to four decimal places.
# Mean crashes per week
lambda <- 0.4 * 7

# Probability of more than 2 crashes
probability <- 1 - ppois(2, lambda)

# Rounded result
round(probability, 4)

##########
# The number of calls received by an office on Monday morning between 8:00 AM 
# and 9:00 AM has a mean of 5. Calculate the probability of getting no more than 
# 4 calls between eight and nine in the morning. Round your answer to four decimal 
# places.

# # Mean calls per hour
lambda <- 2

# Probability of no more than 4 calls
probability <- ppois(1, lambda)

# Rounded result
round(probability, 4)

##########
# At the Fidelity Credit Union, a mean of 4.5 customers arrive hourly at the 
# drive-through window. What is the probability that, in any hour, more than 
# 4 customers will arrive? Round your answer to four decimal places.
# Mean customers per hour
lambda <- 6.1

# Probability of more than 4 customers
probability <- 1 - ppois(4, lambda)
# OR 
# Probability of fewer than 2 customers
probability <- ppois(1, lambda)

# Rounded result
round(probability, 4)

#########
# The number of weaving errors in a twenty-foot by ten-foot roll of carpet has 
# a mean of 0.8. What is the probability of observing less than 5 errors in the 
# carpet? Round your answer to four decimal places.
# Mean errors per roll
lambda <- 0.5

# Probability of fewer than 5 errors ( 5 - 1)
probability <- ppois(1, lambda)

# Rounded result
round(probability, 4)

#########
# Lost-time accidents occur in a company at a mean rate of 0.4 per day. What is 
# the probability that the number of lost-time accidents occurring over a period 
# of 9 days will be at least 3? Round your answer to four decimal places.
# Mean accidents over 9 days
lambda <- 0.8 * 9

# Probability of at least 3 accidents 
probability <- 1 - ppois(4, lambda)
# OR
# Probability of no more than 4 accidents
probability <- ppois(4, lambda)

# Rounded result
round(probability, 4)

#########
# Lost-time accidents occur in a company at a mean rate of 0.4 per day. What is the probability that the number of lost-time accidents occurring over a period of 9 days will be at least 3? Round your answer to four decimal places.
# Mean accidents over 9 days
lambda <- 0.4 * 9

# Probability of at least 3 accidents
probability <- 1 - ppois(2, lambda)

# Rounded result
round(probability, 4)

#########
# A well-mixed cookie dough will produce cookies with a mean of 5 chocolate chips apiece. What is the probability of getting a cookie with at least 4 chips? Round your answer to four decimal places.
# Mean chips per cookie
lambda <- 5

# Probability of at least 4 chips (4 - 1)
probability <- 1 - ppois(3, lambda)

# Rounded result
round(probability, 4)

##########
# A town recently dismissed 7 employees in order to meet their new budget 
# reductions. The town had 8 employees over 50 years of age and 16 under 50. 
# If the dismissed employees were selected at random, what is the probability 
# that exactly 1 employee was over 50? Express your answer as a fraction or a 
# decimal number rounded to four decimal places.
# Population values
N <- 24      # total employees
K <- 8       # over 50
n <- 7       # dismissed

# Probability exactly 1 over 50
probability <- dhyper(1, K, N - K, n)

# Rounded result
round(probability, 4)

##########
# There are 5 black balls and 9 red balls in an urn. If 4 balls are drawn 
# without replacement, what is the probability that exactly 3 black balls are 
# drawn? Express your answer as a fraction or a decimal number rounded to four 
# decimal places.
# 5 black, 9 red; draw 4 without replacement; P(exactly 3 black)

N <- 14          # total balls
K <- 5           # black balls
n <- 4           # balls drawn
k <- 3           # exactly 3 black

probability <- dhyper(k, K, N - K, n)

round(probability, 4)

#########
# Unknown to a medical researcher, 4 out of 21 patients have a heart problem 
# that will result in death if they receive the test drug. 9 patients are 
# randomly selected to receive the drug and the rest receive a placebo. 
# What is the probability that exactly 1 patient will die? Express your answer 
# as a fraction or a decimal number rounded to four decimal places.
# Population values
N <- 21 # total patients
K <- 4 # patients with condition
n <- 9 # patients randomly selected for test drug

# Probability exactly 1 patient with heart problem
probability <- dhyper(1, K, N - K, n)

round(probability, 4)

########
# A town recently dismissed 6 employees in order to meet their new budget 
# reductions. The town had 7 employees over 50 years of age and 17 under 50. 
# If the dismissed employees were selected at random, what is the probability 
# that at least 5 employees were over 50? Express your answer as a fraction or 
# a decimal number rounded to four decimal places.
# Total number of employees (7 + 17)
N <- 24

# Number of employees over 50 (successes in population)
K <- 7

# Number of employees dismissed (sample size)
n <- 6

# Probability that at least 5 dismissed were over 50
probability <- dhyper(5, K, N - K, n) +
  dhyper(6, K, N - K, n)

round(probability, 4)

########
# Naval intelligence reports that 9 enemy vessels in a fleet of 23 are 
# carrying nuclear weapons. If 10 vessels are randomly targeted and destroyed, 
# what is the probability that no more than 1 vessel transporting nuclear 
# weapons was destroyed? Express your answer as a fraction or a decimal number 
# rounded to four decimal places.
# Total number of vessels
N <- 23

# Number carrying nuclear weapons (successes in population)
K <- 9

# Number of vessels destroyed (sample size)
n <- 10

# Probability that no more than 1 nuclear vessel was destroyed
probability <- dhyper(0, K, N - K, n) +
  dhyper(1, K, N - K, n)

round(probability, 4)

##########
# Unknown to a medical researcher, 4 out of 23 patients have a heart problem
# that will result in death if they receive the test drug. 7 patients are 
# randomly selected to receive the drug and the rest receive a placebo. What 
# is the probability that more than 1 patient will die? Express your answer as 
# a fraction or a decimal number rounded to four decimal places.
# Total number of patients
N <- 23

# Number with fatal heart condition (successes in population)
K <- 4

# Number receiving the drug (sample size)
n <- 7

# Probability that more than 1 patient dies
probability <- 1 - (dhyper(0, K, N - K, n) +
                      dhyper(1, K, N - K, n))

round(probability, 4)
############
# Suppose a batch of 80 light bulbs contains 6 light bulbs that are defective. 
# Let X be the number of defective light bulbs in a random sample of 30 light 
# bulbs (where the sample is taken without replacement).
# Step 1 of 2 : Find the expected number of defective bulbs. Round your answer 
# to two decimal places, if necessary. 
# Total number of bulbs
N <- 100

# Number of defective bulbs (successes in population)
K <- 5

# Sample size
n <- 10

# Expected number of defective bulbs
expected_value <- n * (K / N)

round(expected_value, 2)

# Step 2 Find the probability that at least 1 of the bulbs sampled will be defective. 
# Round your answer to four decimal places, if necessary.
# Probability that at least 1 bulb is defective
probability <- 1 - dhyper(0, K, N - K, n)

round(probability, 4)

########
# A bank has to repossess 100 homes. Forty-eight of the repossessed homes have 
# market values that are less than the outstanding mortgage balance. An auditor 
# randomly selects 8 of the repossessed homes (without replacement) and records 
# the number of homes that have market values less than the outstanding mortgage 
# balance.
# Step 1 of 2 : What is the probability that all of the audited homes will have 
# market values in excess of their outstanding mortgage balance? Round your 
# answer to four decimal places, if necessary.
# Total number of homes
N <- 100

# Number with market value less than mortgage (successes in population)
K <- 48

# Sample size
n <- 8

# Probability that all 8 are worth MORE than mortgage
# (i.e., 0 from the 48 low-value homes)
probability <- dhyper(0, K, N - K, n)

round(probability, 4)
#  Step 2 of 2 : What is the probability that none of the audited homes will 
# have market values in excess of their outstanding mortgage balances? Round 
# your answer to four decimal places, if necessary.
# Probability that all 8 homes are worth LESS than mortgage
probability <- dhyper(8, K, N - K, n)

round(probability, 4)

##########
# A small liberal arts college in the Northeast has 150 freshmen. Forty-five 
# of the freshmen are education majors. Suppose twenty freshmen are randomly 
# selected (without replacement).
# Step 1 of 2 : Find the expected number of education majors in the sample. 
# Round your answer to two decimal places, if necessary.
# Total number of freshmen
N <- 150

# Number of education majors (successes in population)
K <- 45

# Sample size
n <- 20

# Expected number of education majors in the sample
expected_value <- n * (K / N)

round(expected_value, 2)
# Step 2 of 2 : Find the standard deviation of the number of education majors 
# in the sample. Round your answer to two decimal places, if necessary.
# Hypergeometric variance
variance <- n * (K / N) * (1 - K / N) * ((N - n) / (N - 1))

# Standard deviation
sd_value <- sqrt(variance)

round(sd_value, 2)

#########
# Suppose a batch of 80 light bulbs contains 5 light bulbs that are defective. 
# Let X be the number of defective light bulbs in a random sample of 30 light 
# bulbs (where the sample is taken without replacement).
# Step 1 of 2 : Find the probability that at most 4 of the bulbs sampled will 
# be defective. Round your answer to four decimal places, if necessary.
# Total number of bulbs
N <- 80

# Number of defective bulbs (successes in population)
K <- 5

# Sample size
n <- 30

# Probability that at most 4 bulbs are defective
probability <- 1 - dhyper(5, K, N - K, n)

round(probability, 4)
#  Step 2 of 2 :Find the probability that more than 5 of the bulbs sampled will 
# be defective. Round your answer to four decimal places, if necessary.
# Probability that more than 5 bulbs are defective
probability <- 0

round(probability, 4)

########
# There are 6 black balls and 7 red balls in an urn. If 4 balls are drawn 
# without replacement, what is the probability that less than 3 black balls 
# are drawn? Express your answer as a fraction or a decimal number rounded to 
# four decimal places.
# Total number of balls
N <- 13

# Number of black balls (successes in population)
K <- 6

# Number of balls drawn
n <- 4

# Probability that fewer than 3 black balls are drawn
probability <- phyper(2, K, N - K, n)

round(probability, 4)

###########
# A pharmacist receives a shipment of 25 bottles of a drug and has 3 of the 
# bottles tested. If 4 of the 25 bottles are contaminated, what is the 
# probability that no more than 1 of the tested bottles is contaminated? 
# Express your answer as a fraction or a decimal number rounded to four decimal 
# places.
# Total number of bottles
N <- 25

# Number of contaminated bottles (successes in population)
K <- 4

# Number of bottles tested (sample size)
n <- 3

# Probability that no more than 1 tested bottle is contaminated: P(X <= 1)
probability <- dhyper(0, K, N - K, n) +
  dhyper(1, K, N - K, n)

round(probability, 4)

###########
# A bank has to repossess 100 homes. Ninety-two of the repossessed homes have 
# market values that are less than the outstanding balance of the mortgage. 
# An auditor randomly selects 25 of the repossessed homes (without replacement) 
# and records the number of homes that have market values less than the outstanding 
# balance of the mortgage.
# Step 1 of 2 : Find the expected number of homes the auditor will find with 
# market values less than the outstanding balance of the mortgage. Round your 
# answer to the nearest whole number, if necessary.
# Total number of homes
N <- 100

# Number of homes worth less than mortgage (successes in population)
K <- 92

# Sample size
n <- 25

# Expected number in the sample
expected_value <- n * (K / N)

round(expected_value)
#  Step 2 of 2 :Find the standard deviation of the number of homes the auditor 
# will find with market values less than the outstanding balance of the mortgage. 
# Round your answer to four decimal places, if necessary.
# Total number of homes
N <- 100

# Number of homes worth less than mortgage
K <- 92

# Sample size
n <- 25

# Hypergeometric variance
variance <- n * (K / N) * (1 - K / N) * ((N - n) / (N - 1))

# Standard deviation
sd_value <- sqrt(variance)

round(sd_value, 4)

##########
# Naval intelligence reports that 4 enemy vessels in a fleet of 16 are carrying 
# nuclear weapons. If 6 vessels are randomly targeted and destroyed, 
# what is the probability that more than 1 vessel transporting nuclear weapons 
# was destroyed? Express your answer as a fraction or a decimal number rounded 
# to four decimal places.
# Total number of vessels
N <- 16

# Number carrying nuclear weapons (successes in population)
K <- 4

# Number of vessels destroyed (sample size)
n <- 6

# Probability that more than 1 nuclear vessel was destroyed: P(X > 1)
probability <- 1 - (dhyper(0, K, N - K, n) +
                      dhyper(1, K, N - K, n))

round(probability, 4)

###########
# A pharmacist receives a shipment of 15 bottles of a drug and has 4 of the 
# bottles tested. If 4 of the 15 bottles are contaminated, what is the 
# probability that more than 1 of the tested bottles is contaminated? 
# Express your answer as a fraction or a decimal number rounded to four 
# decimal places.
# Total number of bottles
N <- 15

# Number of contaminated bottles (successes in population)
K <- 4

# Number of bottles tested (sample size)
n <- 4

# Probability that more than 1 tested bottle is contaminated: P(X > 1)
probability <- 1 - (dhyper(0, K, N - K, n) +
                    dhyper(1, K, N - K, n))

round(probability, 4)

#######
# A bank has to repossess 100 homes. Forty of the repossessed homes have market values that are less than the outstanding balance of the mortgage. An auditor randomly selects 30 of the repossessed homes (without replacement) and records the number of homes that have market values less than the outstanding balance of the mortgage.
# Step 1 of 2 : Find the expected number of homes the auditor will find with market values less than the outstanding balance of the mortgage. Round your answer to the nearest whole number, if necessary.
# Total number of homes
N <- 100

# Number of homes worth less than mortgage
K <- 40

# Sample size
n <- 30

# Expected number in the sample
expected_value <- n * (K / N)

round(expected_value)

#  Step 2 of 2 : Find the standard deviation of the number of homes the auditor will find with market values less than the outstanding balance of the mortgage. Round your answer to four decimal places, if necessary.
# Hypergeometric variance
variance <- n * (K / N) * (1 - K / N) * ((N - n) / (N - 1))

# Standard deviation
sd_value <- sqrt(variance)

round(sd_value, 4)

#######
# A pharmacist receives a shipment of 24 bottles of a drug and has 3 of the 
# bottles tested. If 6 of the 24 bottles are contaminated, what is the 
# probability that less than 2 of the tested bottles are contaminated? Express 
# your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of bottles
N <- 24

# Number of contaminated bottles (successes in population)
K <- 6

# Number of bottles tested (sample size)
n <- 3

# Probability that fewer than 2 tested bottles are contaminated: P(X < 2) = P(X <= 1)
probability <- dhyper(0, K, N - K, n) +
  dhyper(1, K, N - K, n)

round(probability, 4)

#########
# Naval intelligence reports that 9 enemy vessels in a fleet of 23 are carrying nuclear weapons. If 9 vessels are randomly targeted and destroyed, what is the probability that at least 8 vessels transporting nuclear weapons were destroyed? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of vessels
N <- 23

# Number carrying nuclear weapons (successes in population)
K <- 9

# Number of vessels destroyed (sample size)
n <- 9

# Probability that at least 8 nuclear vessels were destroyed: P(X >= 8) = P(8) + P(9)
probability <- dhyper(8, K, N - K, n) +
  dhyper(9, K, N - K, n)

round(probability, 4)

##########
# A pharmacist receives a shipment of 18 bottles of a drug and has 3 of the bottles tested. If 6 of the 18 bottles are contaminated, what is the probability that at least 2 of the tested bottles are contaminated? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of bottles
N <- 18

# Number of contaminated bottles (successes in population)
K <- 6

# Number of bottles tested (sample size)
n <- 3

# Probability that at least 2 tested bottles are contaminated: P(X >= 2) = P(2) + P(3)
probability <- dhyper(2, K, N - K, n) +
  dhyper(3, K, N - K, n)

round(probability, 4)

#########
# Unknown to a medical researcher, 4 out of 23 patients have a heart problem that will result in death if they receive the test drug. 6 patients are randomly selected to receive the drug and the rest receive a placebo. What is the probability that exactly 1 patient will die? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of patients
N <- 23

# Number with fatal heart condition (successes in population)
K <- 4

# Number receiving the drug (sample size)
n <- 6

# Exactly 1 patient with the condition in the drug group
k <- 1

# Probability of exactly 1 death
probability <- dhyper(k, K, N - K, n)

round(probability, 4)

########
# A bank has to repossess 100 homes. Forty-four of the repossessed homes have market values that are less than the outstanding mortgage balance. An auditor randomly selects 5 of the repossessed homes (without replacement) and records the number of homes that have market values less than the outstanding mortgage balance.
# Step 1 of 2 : What is the probability that all of the audited homes will have market values in excess of their outstanding mortgage balance? Round your answer to four decimal places, if necessary.
# Total number of homes
N <- 100

# Number of homes worth less than mortgage (successes in population)
K <- 47

# Number of homes selected (sample size)
n <- 5

# Probability that all 5 are worth MORE than mortgage (X = 0)
probability <- dhyper(0, K, N - K, n)

round(probability, 4)

#  Step 2 of 2 : What is the probability that none of the audited homes will have market values in excess of their outstanding mortgage balances? Round your answer to four decimal places, if necessary.
# Total number of homes
N <- 100

# Number of homes worth less than mortgage (successes in population)
K <- 47

# Number of homes selected (sample size)
n <- 5

# Probability that all 5 are worth LESS than mortgage (X = 5)
probability <- dhyper(5, K, N - K, n)

round(probability, 4)

##############
# A town recently dismissed 9 employees in order to meet their new budget reductions. The town had 7 employees over 50 years of age and 16 under 50. If the dismissed employees were selected at random, what is the probability that no more than 1 employee was over 50? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of employees
N <- 23

# Number of employees over 50 (successes in population)
K <- 7

# Number of employees dismissed (sample size)
n <- 9

# Probability that no more than 1 dismissed employee was over 50: P(X <= 1)
probability <- dhyper(0, K, N - K, n) +
  dhyper(1, K, N - K, n)

round(probability, 4)

##########
# There are 8 black balls and 8 red balls in an urn. If 3 balls are drawn without replacement, what is the probability that exactly 1 black ball is drawn? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of balls
N <- 16

# Number of black balls (successes in population)
K <- 8

# Number of balls drawn (sample size)
n <- 3

# Exactly 1 black ball drawn
k <- 1

# Probability of exactly 1 black ball
probability <- dhyper(k, K, N - K, n)

round(probability, 4)

##########
# Unknown to a medical researcher, 9 out of 18 patients have a heart problem that will result in death if they receive the test drug. 9 patients are randomly selected to receive the drug and the rest receive a placebo. What is the probability that more than 1 patient will die? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of patients
N <- 18

# Number with fatal heart condition (successes in population)
K <- 9

# Number receiving the drug (sample size)
n <- 9

# Probability that more than 1 patient dies: P(X > 1) = 1 - P(X <= 1)
probability <- 1 - (dhyper(0, K, N - K, n) +
                      dhyper(1, K, N - K, n))

round(probability, 4)

############
# Suppose a batch of 30 light bulbs contains 4 light bulbs that are defective. Let X be the number of defective light bulbs in a random sample of 10 light bulbs (where the sample is taken without replacement).
# Step 1 of 2 : Find the probability that at most 3 of the bulbs sampled will be defective. Round your answer to four decimal places, if necessary.
# Total number of bulbs
N <- 30

# Number of defective bulbs (successes in population)
K <- 4

# Sample size
n <- 10

# Probability that at most 3 bulbs are defective
probability <- 1 - dhyper(4, K, N - K, n)

round(probability, 4)

#  Step 2 of 2 : Find the probability that more than 4 of the bulbs sampled will be defective. Round your answer to four decimal places, if necessary.
# Probability that more than 4 bulbs are defective
probability <- 0

round(probability, 4)

###########
# Naval intelligence reports that 9 enemy vessels in a fleet of 21 are carrying nuclear weapons. If 5 vessels are randomly targeted and destroyed, what is the probability that less than 4 vessels transporting nuclear weapons were destroyed? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of vessels
N <- 21

# Number carrying nuclear weapons (successes in population)
K <- 9

# Number of vessels destroyed (sample size)
n <- 5

# Probability that fewer than 4 nuclear vessels were destroyed: P(X <= 3)
probability <- 1 - (dhyper(4, K, N - K, n) +
                      dhyper(5, K, N - K, n))

round(probability, 4)

########
# Suppose a batch of 80 light bulbs contains 8 light bulbs that are defective. Let X be the number of defective light bulbs in a random sample of 30 light bulbs (where the sample is taken without replacement).
# Step 1 of 2 : Find the expected number of defective bulbs. Round your answer to two decimal places, if necessary. 
# Total number of bulbs
N <- 80

# Number of defective bulbs (successes in population)
K <- 8

# Sample size
n <- 30

# Expected number of defective bulbs
expected_value <- n * (K / N)

round(expected_value, 2)

#  Step 2 of 2 : Find the probability that at least 1 of the bulbs sampled will be defective. Round your answer to four decimal places, if necessary.
# Probability that at least 1 bulb is defective
probability <- 1 - dhyper(0, K, N - K, n)

round(probability, 4)

#########
# A town recently dismissed 8 employees in order to meet their new budget reductions. The town had 4 employees over 50 years of age and 19 under 50. If the dismissed employees were selected at random, what is the probability that exactly 1 employee was over 50? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of employees
N <- 23

# Number of employees over 50 (successes in population)
K <- 4

# Number of employees dismissed (sample size)
n <- 8

# Exactly 1 employee over 50
k <- 1

# Probability of exactly 1
probability <- dhyper(k, K, N - K, n)

round(probability, 4)

############
# There are 5 black balls and 6 red balls in an urn. If 4 balls are drawn without replacement, what is the probability that more than 1 black ball is drawn? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total number of balls
N <- 11

# Number of black balls (successes in population)
K <- 5

# Number of balls drawn (sample size)
n <- 4

# Probability that more than 1 black ball is drawn
probability <- 1 - (dhyper(0, K, N - K, n) +
                      dhyper(1, K, N - K, n))

round(probability, 4)

###########
# A quality control inspector has drawn a sample of 19 light bulbs from a recent production lot. If the number of defective bulbs is 1 or more, the lot fails inspection. Suppose 20% of the bulbs in the lot are defective. What is the probability that the lot will fail inspection? Round your answer to four decimal places.
# Sample size
n <- 19

# Probability a bulb is defective
p <- 0.20

# Probability the lot fails inspection (at least 1 defective)
probability <- 1 - dbinom(0, n, p)

round(probability, 4)

##############
# You order some accessories online and get an estimated delivery date of June 1–June 9. You know you will be out of town June 4th, 5th, and 6th and are a little concerned about the package arriving when you are away. Assuming the delivery date follows a discrete uniform distribution, what is the likelihood your package will be delivered while you are out of town? Round your answer to four decimal places, if necessary.
# Total possible delivery days
total_days <- 9

# Days out of town
away_days <- 3

# Probability delivery occurs while away
probability <- away_days / total_days

round(probability, 4)

# Given the following discrete uniform probability distribution, find the expected value and standard deviation of the random variable. Round your final answer to three decimal places, if necessary.
# x	0	1	2	3	4	5	6	7	8	9	10	11	12
# P(X=x)	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13	1/13
# Lower and upper bounds
a <- 0
b <- 12

# Expected value
expected_value <- (a + b) / 2

# Variance and standard deviation
variance <- ((b - a + 1)^2 - 1) / 12
sd_value <- sqrt(variance)

round(expected_value, 3)
round(sd_value, 3)

#####
# Consider the following data:
# x : 6,	7,	8,	9,	10
# P(X=x) : 0.3, 0.2, 0.1, 0.2, 0.2
#  Step 1 of 5 : Find the expected value E(X). Round your answer to one decimal place.
# Values of x
x <- c(6, 7, 8, 9, 10)

# Corresponding probabilities
p <- c(0.3, 0.2, 0.1, 0.2, 0.2)

# Expected value
expected_value <- sum(x * p)

round(expected_value, 1)

#  Step 2 of 5 : Find the variance. Round your answer to one decimal place.
# Expected value
mu <- sum(x * p)

# Expected value of X^2
EX2 <- sum((x^2) * p)

# Variance
variance <- EX2 - mu^2

round(variance, 1)

#  Step 3 of 5 : Find the standard deviation. Round your answer to one decimal place.
# Standard deviation
sd_value <- sqrt(variance)

round(sd_value, 1)
#  Step 4 of 5 : Find the value of P(X≤9). Round your answer to one decimal place.
# P(X <= 9)
probability <- sum(p[x <= 9])

round(probability, 1)

#  Step 5 of 5 : Find the value of P(X<8). Round your answer to one decimal place.
# P(X < 8)
probability <- sum(p[x < 8])

round(probability, 1)

##################
# In manufacturing integrated circuits, the yield of the manufacturing process is the percentage of good chips produced by the process. The probability that an integrated circuit manufactured by the Ace Electronics Company will be defective is p=0.03. If a random sample of 18 circuits is selected for testing, answer the following questions.
# Step 1 of 2 : What is the probability that no more than one integrated circuit will be defective in the sample? Round your answer to four decimal places, if necessary.
# Sample size
n <- 18

# Probability a circuit is defective
p <- 0.03

# Probability that no more than 1 is defective
probability <- pbinom(1, n, p)

round(probability, 4)

#  Step 2 of 2 : What is the expected number of defective integrated circuits in the sample? Round your answer to two decimal places, if necessary.
# Expected number of defective circuits
expected_value <- n * p

round(expected_value, 2)

#################
# Given the following discrete uniform probability distribution, find the expected value and standard deviation of the random variable. Round your final answer to three decimal places, if necessary.
# x : 0	1	2	3	4	5	6	7
# : P(X = x): 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8, 1/8

# Lower and upper bounds
a <- 0
b <- 7

# Expected value
expected_value <- (a + b) / 2

# Variance and standard deviation
variance <- ((b - a + 1)^2 - 1) / 12
sd_value <- sqrt(variance)

round(expected_value, 3)
round(sd_value, 3)

################
# There are 10 black balls and 10 red balls in an urn. If 5 balls are drawn without replacement, what is the probability that at least 4 black balls are drawn? Express your answer as a fraction or a decimal number rounded to four decimal places.
# Total balls
N <- 20

# Black balls (successes in population)
K <- 10

# Balls drawn
n <- 5

# Probability at least 4 black balls
probability <- dhyper(4, K, N - K, n) +
  dhyper(5, K, N - K, n)

round(probability, 4)

############
# An entrepreneur is considering investing in a new venture. If the venture is successful, he will make $40,000. However, if the venture is not successful, he will lose his investment of $10,000. Based on past experience, he believes that there is a 60% chance that the venture will be successful.
# Step 1 of 2 : Determine the expected amount of money the entrepreneur will make on the venture. Round to the nearest dollar, if necessary.
# Profit if successful
success_profit <- 40000

# Loss if unsuccessful
failure_loss <- -10000

# Probabilities
p_success <- 0.60
p_failure <- 0.40

# Expected value
expected_value <- success_profit * p_success +
  failure_loss * p_failure

round(expected_value, 0)

############
# The computer that controls a bank's automatic teller machine crashes a mean of 0.5 times per day. What is the probability that, in any seven-day week, the computer will crash exactly 1 time? Round your answer to four decimal places.
# Mean crashes per day
lambda_day <- 0.5

# 7-day week mean
lambda <- lambda_day * 7

# Probability of exactly 1 crash
probability <- dpois(1, lambda)

round(probability, 4)

############
# Assume the random variable X has a binomial distribution with the given probability of obtaining a success. Find the following probability, given the number of trials and the probability of obtaining a success. Round your answer to four decimal places.
# P(X<2), n=6, p=0.8
# Number of trials
n <- 6

# Probability of success
p <- 0.8

# Probability that X < 2
probability <- pbinom(1, n, p)

round(probability, 4)

################
# Polar Bear Frozen Foods manufactures frozen French fries for sale to grocery store chains. The final package weight is thought to be a uniformly distributed random variable. Assume X, the weight of French fries, has a uniform distribution between 59 ounces and 71 ounces. What is the mean weight for a package? What is the standard deviation for the weight of a package? Round your answers to four decimal places, if necessary.
a <- 59
b <- 71

# Mean and SD for Uniform(a,b)
mu <- (a + b) / 2
sigma <- (b - a) / sqrt(12)

# Print to 4 decimals
round(mu, 4)
round(sigma, 4)

############
# Polar Bear Frozen Foods manufactures frozen French fries for sale to grocery store chains. The final package weight is thought to be a uniformly distributed random variable. Assume X, the weight of French fries, has a uniform distribution between 57 ounces and 69 ounces. What is the mean weight for a package? What is the standard deviation for the weight of a package? Round your answers to four decimal places, if necessary.
a <- 57
b <- 69

mean <- (a + b) / 2
sd <- (b - a) / sqrt(12)

round(mean, 4)
round(sd, 4)

###############
# A particular employee arrives at work sometime between 8:00 a.m. and 8:50 a.m. Based on past experience the company has determined that the employee is equally likely to arrive at any time between 8:00 a.m. and 8:50 a.m. Find the probability that the employee will arrive between 8:05 a.m. and 8:45 a.m. Round your answer to four decimal places, if necessary.
round(punif(45, min = 0, max = 50) - punif(5, min = 0, max = 50), 4)

############
# The annual increase in height of cedar trees is believed to be distributed uniformly between five and eleven inches. Find the probability that a randomly selected cedar tree will grow less than 7 inches in a given year. Round your answer to four decimal places, if necessary.
round(punif(7, min = 5, max = 11), 4)

##########
# The annual increase in height of cedar trees is believed to be distributed uniformly between seven and twelve inches. Find the probability that a randomly selected cedar tree will grow more than 9 inches in a given year. Round your answer to four decimal places, if necessary.
round(1 - punif(9, min = 7, max = 12), 4)

#########
# A particular employee arrives at work sometime between 8:00 a.m. and 8:40 a.m. Based on past experience the company has determined that the employee is equally likely to arrive at any time between 8:00 a.m. and 8:40 a.m. Find the probability that the employee will arrive between 8:10 a.m. and 8:35 a.m. Round your answer to four decimal places, if necessary.
round(punif(35, min = 0, max = 40) - punif(10, min = 0, max = 40), 4)

##########
# Suppose a continuous random variable is uniformly distributed between 20 and 50.
#Step 1 of 3 : Determine the mean and standard deviation of the distribution. Round your answer to four decimal places, if necessary.
a <- 20
b <- 80

round((a + b) / 2, 4)
round((b - a) / sqrt(12), 4)

#  Step 2 of 3 : What is the probability that a randomly selected value will be above 30? Round your answer to four decimal places, if necessary.
round(1 - punif(45, min = a, max = b), 4)

#  Step 3 of 3 : What is the probability that a randomly selected value will be exactly equal to 29? Round your answer to four decimal places, if necessary.
round(0, 4)

#########
# A particular employee arrives at work sometime between 8:00 a.m. and 8:40 a.m. Based on past experience the company has determined that the employee is equally likely to arrive at any time between 8:00 a.m. and 8:40 a.m. Find the probability that the employee will arrive between 8:15 a.m. and 8:20 a.m. Round your answer to four decimal places, if necessary.
a <- 0
b <- 40
c <- 15
d <- 20

round(punif(d, min = a, max = b) - punif(c, min = a, max = b), 4)

#########
# Polar Bear Frozen Foods manufactures frozen French fries for sale to grocery store chains. The final package weight is thought to be a uniformly distributed random variable. Assume X, the weight of French fries, has a uniform distribution between 59 ounces and 67 ounces. What is the mean weight for a package? What is the standard deviation for the weight of a package? Round your answers to four decimal places, if necessary.
a <- 59
b <- 67

round((a + b) / 2, 4)
round((b - a) / sqrt(12), 4)

########
# A particular employee arrives at work sometime between 8:00 a.m. and 8:30 a.m. Based on past experience the company has determined that the employee is equally likely to arrive at any time between 8:00 a.m. and 8:30 a.m. Find the probability that the employee will arrive between 8:15 a.m. and 8:25 a.m. Round your answer to four decimal places, if necessary.
a <- 0
b <- 30
c <- 15
d <- 25

round(punif(d, min = a, max = b) - punif(c, min = a, max = b), 4)

###########
# Polar Bear Frozen Foods manufactures frozen French fries for sale to grocery store chains. The final package weight is thought to be a uniformly distributed random variable. Assume X, the weight of French fries, has a uniform distribution between 57 ounces and 59 ounces. What is the mean weight for a package? What is the standard deviation for the weight of a package? Round your answers to four decimal places, if necessary.
a <- 57
b <- 59

round((a + b) / 2, 4)
round((b - a) / sqrt(12), 4)

###########
# Given X= x, μ=mu , and σ=sigma, indicate on the curve where the given X value would be.
# Just move it to the X value

#########
# The following is a graph of two normal distributions plotted on the same x-axis.
# If they have the same shape and size but are shifted by a few then
# The two have means that differ by x units and equal standard deviations

##########
# What value of z divides the standard normal distribution so that half the area is on one side and half is on the other?  Round your answer to two decimal places.
qnorm(0.5)

###################
# Find the value of z such that 0.03 of the area lies to the left of z.  Round your answer to two decimal places.
round(qnorm(0.03), 2)

############
# Find the value of z such that 0.7286 of the area lies between −z and z.  Round your answer to two decimal places.
area_between <- 0.7286
area_left <- (1 + area_between) / 2
z <- qnorm(area_left)
round(z, 2)

########
# Find the value of z such that 0.9282 of the area lies between −z and z.  Round your answer to two decimal places.
area_between <- 0.9282
area_left <- (1 + area_between) / 2
z <- qnorm(area_left)
round(z, 2)

############
# Find the area under the standard normal curve to the left of z=0.24.  Round your answer to four decimal places, if necessary.
z <- 0.24
round(pnorm(z), 4)

########
# Find the value of z such that 0.08 of the area lies to the right of z.  Round your answer to two decimal places.
area_right <- 0.08
area_left <- 1 - area_right
z <- qnorm(area_left)
round(z, 2)

##########
# Find the area under the standard normal curve to the right of z=1.56.  Round your answer to four decimal places, if necessary.
z <- 1.56
round(1 - pnorm(z), 4)

########
# Find the value of z such that 0.07 of the area lies to the right of z.  Round your answer to two decimal places.
area_right <- 0.07
z <- qnorm(1 - area_right)
round(z, 2)

##########
# Find the area under the standard normal curve between z=−0.62 and z=1.47.  Round your answer to four decimal places, if necessary.
z1 <- -0.62
z2 <- 1.47
round(pnorm(z2) - pnorm(z1), 4)

#########
# Find the area under the standard normal curve to the left of z=−0.95.  Round your answer to four decimal places, if necessary.
z <- -0.95
round(pnorm(z), 4)

###########
# Find the area under the standard normal curve to the left of z=−0.84.  Round your answer to four decimal places, if necessary.
z <- -0.84
round(pnorm(z), 4)

###########
# Find the area under the standard normal curve between z=−2.97 and z=2.67.  Round your answer to four decimal places, if necessary.
z1 <- -2.77
z2 <- -2.22
round(pnorm(z2) - pnorm(z1), 4)

#######
# Find the area under the standard normal curve to the right of z=−2.3.  Round your answer to four decimal places, if necessary
z <- -2.3
round(1 - pnorm(z), 4)

###########
# Find the area under the standard normal curve to the right of z=2.09.  Round your answer to four decimal places, if necessary.
z <- 2.09
round(1 - pnorm(z), 4)

##########
# Find the area under the standard normal curve to the left of z=−1.15.  Round your answer to four decimal places, if necessary.
z <- -1.15
round(pnorm(z), 4)

########
# What value of z divides the standard normal distribution so that half the area is on one side and half is on the other?  Round your answer to two decimal places.
# 0

##########
# Find the area under the standard normal curve to the right of z=0.74.  Round your answer to four decimal places, if necessary.
z <- 0.74
round(1 - pnorm(z), 4)

########
# Find the value of z such that 0.5762 of the area lies between −z and z.  Round your answer to two decimal places.
area_between <- 0.5762
area_left <- (1 + area_between) / 2
round(qnorm(area_left), 2)

#######
# Find the value of z such that 0.09 of the area lies to the right of z.  Round your answer to two decimal places.
area_right <- 0.09
z <- qnorm(1 - area_right)
round(z, 2)

##########
# Find the area under the standard normal curve between z=−1.97 and z=−0.79.  Round your answer to four decimal places, if necessary.
z1 <- -1.97
z2 <- -0.79
round(pnorm(z2) - pnorm(z1), 4)

#########
# Find the area under the standard normal curve between z=−2.9 and z=0.28.  Round your answer to four decimal places, if necessary.
z1 <- -2.9
z2 <- 0.28
round(pnorm(z2) - pnorm(z1), 4)

########
# Find the area under the standard normal curve to the left of z=0.21.  Round your answer to four decimal places, if necessary.
z <- 0.21
round(pnorm(z), 4)

######
# Find the area under the standard normal curve to the right of z=1.39.  Round your answer to four decimal places, if necessary.
z <- 1.39
round(1 - pnorm(z), 4)

###########
# Find the area under the standard normal curve to the left of z=2.17.  Round your answer to four decimal places, if necessary.
z <- 2.17
round(pnorm(z), 4)

########
# Find the area under the standard normal curve to the right of z=−0.91.  Round your answer to four decimal places, if necessary.
z <- -0.91
round(1 - pnorm(z), 4)

######### 8.4 ############
# The weights of steers in a herd are distributed normally.  The standard deviation is 200lbs and the mean steer weight is 1500lbs.  Find the probability that the weight of a randomly selected steer is less than 1800lbs. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 1500
sigma <- 200
x <- 1800

# Probability that weight is less than x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

############
#The Arc Electronic Company had an income of 29 million dollars last year.  Suppose the mean income of firms in the same industry as Arc for a year is 25 million dollars with a standard deviation of 7 million dollars.  If incomes for this industry are distributed normally, what is the probability that a randomly selected firm will earn less than Arc did last year? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 25      # mean income (millions)
sigma <- 7    # standard deviation (millions)
x <- 29       # Arc's income

# Probability a randomly selected firm earns less than x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

#########
# The life of light bulbs is distributed normally.  The variance of the lifetime is 100 and the mean lifetime of a bulb is 550 hours.  Find the probability of a bulb lasting for at most 568 hours. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 550
variance <- 100
sigma <- sqrt(variance)
x <- 568

# Probability that lifetime is at most x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

##########
# The time spent waiting in the line is approximately normally distributed.  The mean waiting time is 7 minutes and the variance of the waiting time is 9.  Find the probability that a person will wait for more than 8 minutes. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 7
variance <- 9
sigma <- sqrt(variance)
x <- 8

# Probability that waiting time is greater than x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

##########
# The Arc Electronic Company had an income of 59 million dollars last year.  Suppose the mean income of firms in the same industry as Arc for a year is 45 million dollars with a standard deviation of 7 million dollars.  If incomes for this industry are distributed normally, what is the probability that a randomly selected firm will earn more than Arc did last year? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 45
sigma <- 7
x <- 59

# Probability that income is greater than x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

#########
#A psychology professor assigns letter grades on a test according to the following scheme.
#A:  Top 6% of scores
#B:  Scores below the top 6% and above the bottom 62%
#C:  Scores below the top 38% and above the bottom 24%
#D:  Scores below the top 76% and above the bottom 6%
#F:  Bottom 6% of scores
#Scores on the test are normally distributed with a mean of 72.2 and a standard deviation of 8.6.  Find the numerical limits for a D grade.  Round your answers to the nearest whole number, if necessary.
# Parameters (change these if needed)
mu <- 72.2
sigma <- 8.6

lower_prob <- 0.06
upper_prob <- 0.24

# Numerical limits for a D grade
lower_limit <- qnorm(lower_prob, mean = mu, sd = sigma)
upper_limit <- qnorm(upper_prob, mean = mu, sd = sigma)

# Round to nearest whole number
round(lower_limit)
round(upper_limit)

##########
# According to the Bureau of Labor Statistics, the mean weekly earnings for people working in a sales related profession in 2018 was $870. Assume that the weekly earnings are approximately normally distributed with a standard deviation of $105. If a salesperson was randomly selected, find the probability that his or her weekly earnings exceed $920. Round your answer to four decimal places, if necessary.
# Parameters (change these if needed)
mu <- 870
sigma <- 105
x <- 920

# Probability earnings exceed x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

#########
# Calculate the z-score of the given X value, X=66.8, where μ=60.6 and σ=61.7 and indicate on the curve where z will be located.  Round the z-score to two decimal places.
# Parameters
x <- 66.8
mu <- 60.6
sigma <- 61.7

# z-score
z <- (x - mu) / sigma

round(z, 2)

###########
# The diameters of bolts produced in a machine shop are normally distributed with a mean of 5.74 millimeters and a standard deviation of 0.07 millimeters.  Find the two diameters that separate the top 5% and the bottom 5%.  These diameters could serve as limits used to identify which bolts should be rejected.  Round your answer to the nearest hundredth, if necessary.
# Parameters (change these if needed)
mu <- 5.74
sigma <- 0.07

lower_prob <- 0.05
upper_prob <- 0.95

# Diameter cutoffs
lower_diameter <- qnorm(lower_prob, mean = mu, sd = sigma)
upper_diameter <- qnorm(upper_prob, mean = mu, sd = sigma)

# Round to nearest hundredth
round(lower_diameter, 2)
round(upper_diameter, 2)

##############
# A sociology professor assigns letter grades on a test according to the following scheme.
# A:  Top 7% of scores
# B:  Scores below the top 7% and above the bottom 60%
# C:  Scores below the top 40% and above the bottom 25%
# D:  Scores below the top 75% and above the bottom 10%
# F:  Bottom 10% of scores
# Scores on the test are normally distributed with a mean of 67 and a standard deviation of 9.9.  Find the minimum score required for an A grade.  Round your answer to the nearest whole number, if necessary.
# Parameters (change if needed)
mu <- 67
sigma <- 9.9

top_percent <- 0.07
prob <- 1 - top_percent

# Minimum score for an A
A_cutoff <- qnorm(prob, mean = mu, sd = sigma)

round(A_cutoff)

#############
# Monitors manufactured by TSI Electronics have life spans that have a normal distribution with a standard deviation of 1800 hours and a mean life span of 20,000 hours.  If a monitor is selected at random, find the probability that the life span of the monitor will be more than 17,659 hours. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 20000
sigma <- 1800
x <- 17659

# Probability that lifespan is greater than x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

###############
# Suppose the mean income of firms in the industry for a year is 75 million dollars with a standard deviation of 9 million dollars.  If incomes for the industry are distributed normally, what is the probability that a randomly selected firm will earn between 66 and 90 million dollars? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 75
sigma <- 9

lower <- 66
upper <- 90

# Probability between lower and upper
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

#################
# Trucks in a delivery fleet travel a mean of 100 miles per day with a standard deviation of 23 miles per day.  The mileage per day is distributed normally.  Find the probability that a truck drives between 86 and 125 miles in a day. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 100
sigma <- 23

lower <- 86
upper <- 125

# Probability between lower and upper
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

##############
# A soft drink machine outputs a mean of 25 ounces per cup.  The machine's output is normally distributed with a standard deviation of 4 ounces.  What is the probability of filling a cup between 27 and 31 ounces? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 25
sigma <- 4

lower <- 27
upper <- 31

# Probability between lower and upper
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

#############
# A soft drink machine outputs a mean of 28 ounces per cup.  The machine's output is normally distributed with a standard deviation of 3 ounces.  What is the probability of filling a cup between 30 and 32 ounces? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 28
sigma <- 3

lower <- 30
upper <- 32

# Probability between lower and upper
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

############
# Monitors manufactured by TSI Electronics have life spans that have a normal distribution with a variance of 1,440,000 and a mean life span of 14,000 hours.  If a monitor is selected at random, find the probability that the life span of the monitor will be more than 14,960 hours. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 14000
variance <- 1440000
sigma <- sqrt(variance)
x <- 14960

# Probability that lifespan is greater than x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)


########
# The Arc Electronic Company had an income of 117 million dollars last year.  Suppose the mean income of firms in the same industry as Arc for a year is 95 million dollars with a standard deviation of 11 million dollars.  If incomes for this industry are distributed normally, what is the probability that a randomly selected firm will earn less than Arc did last year? Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 95
sigma <- 11
x <- 117

# Probability income is less than x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

###########
# Trucks in a delivery fleet travel a mean of 130 miles per day with a standard deviation of 32 miles per day.  The mileage per day is distributed normally.  Find the probability that a truck drives between 171 and 181 miles in a day. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 130
sigma <- 32

lower <- 171
upper <- 181

# Probability between lower and upper
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

############
# According to the Bureau of Labor Statistics, the mean weekly earnings for people working in a sales related profession in 2020 was $640. Assume that the weekly earnings are approximately normally distributed with a standard deviation of $40. If a salesperson was randomly selected, find the probability that his or her weekly earnings exceed $700. Round your answer to four decimal places, if necessary.
# Parameters (change these if needed)
mu <- 640
sigma <- 40
x <- 700

# Probability earnings exceed x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

###############
# A sociology professor assigns letter grades on a test according to the following scheme.
# A:  Top 9% of scores
# B:  Scores below the top 9% and above the bottom 59%
# C:  Scores below the top 41% and above the bottom 17%
# D:  Scores below the top 83% and above the bottom 7%
# F:  Bottom 7% of scores
# Scores on the test are normally distributed with a mean of 67.2 and a standard deviation of 8.5.  Find the minimum score required for an A grade.  Round your answer to the nearest whole number, if necessary.
# Parameters (change if needed)
mu <- 67.2
sigma <- 8.5

top_percent <- 0.09
prob <- 1 - top_percent

# Minimum score for an A
A_cutoff <- qnorm(prob, mean = mu, sd = sigma)

# Round to nearest whole number
round(A_cutoff)

############
# The lengths of nails produced in a factory are normally distributed with a mean of 4.88 centimeters and a standard deviation of 0.05 centimeters.  Find the two lengths that separate the top 4% and the bottom 4%.  These lengths could serve as limits used to identify which nails should be rejected.  Round your answer to the nearest hundredth, if necessary.
# Parameters (change if needed)
mu <- 4.88
sigma <- 0.05

lower_prob <- 0.04
upper_prob <- 0.96

# Rejection limits
lower_length <- qnorm(lower_prob, mean = mu, sd = sigma)
upper_length <- qnorm(upper_prob, mean = mu, sd = sigma)

# Round to nearest hundredth
round(lower_length, 2)
round(upper_length, 2)

#########
# The life of light bulbs is distributed normally.  The standard deviation of the lifetime is 30 hours and the mean lifetime of a bulb is 520 hours.  Find the probability of a bulb lasting for at most 544 hours. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 520
sigma <- 30
x <- 544

# Probability that lifetime is at most x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

###############
# A psychology professor assigns letter grades on a test according to the following scheme.
# A:  Top 10% of scores
# B:  Scores below the top 10% and above the bottom 65%
# C:  Scores below the top 35% and above the bottom 25%
# D:  Scores below the top 75% and above the bottom 9%
# F:  Bottom 9% of scores
# Scores on the test are normally distributed with a mean of 66.9 and a standard deviation of 9.  Find the numerical limits for a D grade.  Round your answers to the nearest whole number, if necessary.
# Parameters
mu <- 66.9
sigma <- 9

# Probabilities defining the D grade
lower_prob <- 0.09
upper_prob <- 0.25

# Compute score cutoffs
lower_limit <- qnorm(lower_prob, mean = mu, sd = sigma)
upper_limit <- qnorm(upper_prob, mean = mu, sd = sigma)

# Round to nearest whole number
round(lower_limit)
round(upper_limit)

############
# The time spent waiting in the line is approximately normally distributed.  The mean waiting time is 6 minutes and the variance of the waiting time is 1.  Find the probability that a person will wait for more than 7 minutes. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 6
variance <- 1
sigma <- sqrt(variance)
x <- 7

# Probability that waiting time is greater than x
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

###########
# The weights of steers in a herd are distributed normally.  The variance is 40,000 and the mean steer weight is 1100lbs.  Find the probability that the weight of a randomly selected steer is less than 1540lbs. Round your answer to four decimal places.
# Parameters (change these if needed)
mu <- 1100
variance <- 40000
sigma <- sqrt(variance)
x <- 1540

# Probability that weight is less than x
prob <- pnorm(x, mean = mu, sd = sigma)

# Round to four decimals
round(prob, 4)

########### 8.6
# Consider the probability that no more than 83 out of 150 CDs will not be defective.  Assume the probability that a given CD will not be defective is 57%.
# Specify whether the normal curve can be used as an approximation to the binomial probability by verifying the necessary conditions.
# Yes 

#########
# Consider the probability that greater than 32 out of 113 students will not graduate on time.  Assume the probability that a given student will not graduate on time is 96%.
# Specify whether the normal curve can be used as an approximation to the binomial probability by verifying the necessary conditions.
# No

#######
# Consider the probability that no more than 34 out of 474 students will not graduate on time.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# Area to the left of 34.5

########
# The accounting department of a large corporation checks the addition of expense reports submitted by executives before paying them. Historically, they have found that 15%of the reports contain addition errors. An auditor randomly selects 80 expense reports and audits them for addition errors.
# Use the normal approximation to the binomial distribution with a continuity correction to find the probability that between 5 and 10 (inclusive) of the sampled reports will have addition errors. Round your answer to four decimal places.
# Parameters
n <- 80
p <- 0.15
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

lower <- 4.5
upper <- 10.5

# Normal approximation probability
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

round(prob, 4)

#########
# Management at a small engineering company is considering the addition of a company cafeteria area. A random sample of 190 persons out of the total number of persons employed by the firm will be surveyed to see if they are in favor of the addition. Assume that the true percentage of persons that favor the addition is 75%. 
# Use the normal approximation to the binomial distribution with a continuity correction to determine the probability that more than 146 of the employees in the sample will favor the cafeteria. Round your answer to four decimal places.
# Parameters
n <- 190
p <- 0.75
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 146.5

# Probability more than 146
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

round(prob, 4)

###########
# Consider the probability that no more than 14 out of 157 registered voters will not vote in the presidential election.  Assume the probability that a given registered voter will not vote in the presidential election is 14%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
# Parameters
n <- 157
p <- 0.14
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 14.5

# Probability approximation
prob <- pnorm(x, mean = mu, sd = sigma)

round(prob, 4)

###########
# Consider the probability that less than 18 out of 145 students will not pass their college placement exams.  Assume the probability that a given student will not pass their college placement exam is 11%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
# Parameters
n <- 145
p <- 0.11
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 17.5

# Normal approximation probability
prob <- pnorm(x, mean = mu, sd = sigma)

round(prob, 4)

###########
# Consider the probability that exactly 100 out of 159 people will not get the flu this winter.  Assume the probability that a given person will not get the flu this winter is 65%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
# Parameters
n <- 159
p <- 0.65
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

lower <- 99.5
upper <- 100.5

# Normal approximation probability
prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

round(prob, 4)

###########
# Consider the probability that greater than 89 out of 153 software users will not call technical support.  Assume the probability that a given software user will not call technical support is 59%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
# Parameters
n <- 152
p <- 0.61
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 90

# Probability greater than 89
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

round(prob, 4)

############
# Consider the probability that at least 38 out of 279 CDs will be defective.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# The area to the right of 37.5

#############
# Consider the probability that exactly 68 out of 279 CDs will be defective.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# Area between 67.5 and 68.5

#############
# Consider the probability that fewer than 42 out of 361 CDs will be defective.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# Area to the left of 41.5

##########
# Consider the probability that no less than 87 out of 153 software users will not call technical support.  Assume the probability that a given software user will not call technical support is 63%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
# Parameters
n <- 153
p <- 0.63
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 86.5

# Normal approximation probability
prob <- 1 - pnorm(x, mean = mu, sd = sigma)

round(prob, 4)

############
# A local electronics store purchased a market research study which suggests that 43% of all homes have a video doorbell. A sample of 170 homes is selected to confirm the study’s findings.
# If the market study is correct, what is the probability that at most 70 of the sampled homes will have a video doorbell? Use the normal approximation to the binomial distribution with a continuity correction. Round your answer to four decimal places.
n <- 160
p <- 0.48
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 80.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- pnorm(z)

round(prob, 4)

#############
# Consider the probability that no more than 78 out of 109 DVDs will work correctly.  Assume the probability that a given DVD will work correctly is 29%.
# Specify whether the normal curve can be used as an approximation to the binomial probability by verifying the necessary conditions.
# Yes

############
# Management at a small engineering company is considering the addition of a company cafeteria area. A random sample of 200 persons out of the total number of persons employed by the firm will be surveyed to see if they are in favor of the addition. Assume that the true percentage of persons that favor the addition is 66%. 
# Use the normal approximation to the binomial distribution with a continuity correction to determine the probability that more than 137 of the employees in the sample will favor the cafeteria. Round your answer to four decimal places.
n <- 200
p <- 0.66
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 137.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- 1 - pnorm(z)

round(prob, 4)

###########
# A local electronics store purchased a market research study which suggests that 43% of all homes have a video doorbell. A sample of 170 homes is selected to confirm the study’s findings.
# If the market study is correct, what is the probability that at most 70 of the sampled homes will have a video doorbell? Use the normal approximation to the binomial distribution with a continuity correction. Round your answer to four decimal places.
n <- 170
p <- 0.43
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 70.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- pnorm(z)

round(prob, 4)

##########
# Consider the probability that fewer than 78 out of 261 people have been in a car accident.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# The area to the left of 77.5 under the normal curve.

############
# Consider the probability that no less than 96 out of 149 registered voters will vote in the presidential election.  Assume the probability that a given registered voter will vote in the presidential election is 55%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
n <- 149
p <- 0.55
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 95.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- 1 - pnorm(z)

round(prob, 4)

##############
# Consider the probability that no more than 14 out of 147 students will not graduate on time.  Assume the probability that a given student will not graduate on time is 11%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
n <- 147
p <- 0.11
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 14.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- pnorm(z)

round(prob, 4)

############
# Consider the probability that no more than 49 out of 140 students will not graduate on time.  Assume the probability that a given student will not graduate on time is 98%.
# Specify whether the normal curve can be used as an approximation to the binomial probability by verifying the necessary conditions.
# No

###########
# Consider the probability that no more than 76 out of 504 computers will crash in a day.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# The area to the left of 76.5

##########
# Consider the probability that exactly 62 out of 503 registered voters will not vote in the presidential election.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# The area between 61.5 and 62.5

###########
# Consider the probability that greater than 93 out of 156 software users will not call technical support.  Assume the probability that a given software user will not call technical support is 63%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
n <- 156
p <- 0.63
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 93.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- 1 - pnorm(z)

round(prob, 4)

#############
# Consider the probability that no less than 42 out of 361 CDs will be defective.
# Choose the best description of the area under the normal curve that would be used to approximate binomial probability.
# The area to the right of 41.5

###########
# Consider the probability that fewer than 19 out of 160 DVDs will malfunction.  Assume the probability that a given DVD will malfunction is 13%.
# Approximate the probability using the normal distribution. Round your answer to four decimal places.
n <- 160
p <- 0.13
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

x <- 18.5

z <- (x - mu) / sigma
z <- round(z, 2)

prob <- pnorm(z)

round(prob, 4)

########### 8 review
# The annual increase in height of cedar trees is believed to be distributed uniformly between six and eleven inches. Find the probability that a randomly selected cedar tree will grow less than 8 inches in a given year. Round your answer to four decimal places, if necessary.
# Parameters
a <- 6
b <- 11
x <- 8

prob <- (x - a) / (b - a)

round(prob, 4)

###########
# Find the value of z such that 0.516 of the area lies between −z and z.  Round your answer to two decimal places.
area <- 0.516

p <- 0.5 + area/2

z <- qnorm(p)

round(z, 2)

###########
# An English professor assigns letter grades on a test according to the following scheme.
# A:  Top 13% of scores
# B:  Scores below the top 13% and above the bottom 61%
# C:  Scores below the top 39% and above the bottom 22%
# D:  Scores below the top 78% and above the bottom 6%
# F:  Bottom 6% of scores
# Scores on the test are normally distributed with a mean of 75.1 and a standard deviation of 9.1.  Find the numerical limits for a D grade.  Round your answers to the nearest whole number, if necessary.
mu <- 75.1
sigma <- 9.1

lower_prob <- 0.06
upper_prob <- 0.22

lower_limit <- qnorm(lower_prob, mean = mu, sd = sigma)
upper_limit <- qnorm(upper_prob, mean = mu, sd = sigma)

round(lower_limit)
round(upper_limit) 

###########
# A soft drink machine outputs a mean of 23 ounces per cup.  The machine's output is normally distributed with a standard deviation of 3 ounces.  What is the probability of filling a cup between 27 and 29 ounces? Round your answer to four decimal places.
mu <- 23
sigma <- 3

lower <- 27
upper <- 29

prob <- pnorm(upper, mean = mu, sd = sigma) - pnorm(lower, mean = mu, sd = sigma)

round(prob, 4)

#############
# The accounting department of a large corporation checks the addition of expense reports submitted by executives before paying them. Historically, they have found that 17%of the reports contain addition errors. An auditor randomly selects 100 expense reports and audits them for addition errors.
# Use the normal approximation to the binomial distribution with a continuity correction to find the probability that between 10 and 15 (inclusive) of the sampled reports will have addition errors. Round your answer to four decimal places.
n <- 100
p <- 0.17
q <- 1 - p

mu <- n * p
sigma <- sqrt(n * p * q)

lower <- 9.5
upper <- 15.5

z1 <- round((lower - mu) / sigma, 2)
z2 <- round((upper - mu) / sigma, 2)

prob <- pnorm(z2) - pnorm(z1)

round(prob, 4)

############
# Consider the probability that at least 28 out of 144 people will get the flu this winter.  Assume the probability that a given person will get the flu this winter is 98%.
# Specify whether the normal curve can be used as an approximation to the binomial probability by verifying the necessary conditions.
# No

##########
# Find the value of z such that 0.7286 of the area lies between −z and z.  Round your answer to two decimal places.
p <- (1 + 0.7286) / 2
z <- qnorm(p)

round(z, 2)

########
# Select all of the following indicators which would cause you to reject normality for a sample of data.
# The data has four outliers present
# The data has three outliers present
# A normal probability plot follows a curved shape

##########
# Find the value of z such that 0.11 of the area lies to the right of z.  Round your answer to two decimal places.
p <- 1 - 0.11
z <- qnorm(p)

round(z, 2)

#############
# Calculate the z-score of the given X value, X=39.2, where μ=35.3 and σ=30.6 and indicate on the curve where z will be located.  Round the z-score to two decimal places.
x <- 39.2
mu <- 35.3
sigma <- 30.6

z <- (x - mu) / sigma

round(z, 2)

############
# Find the area under the standard normal curve between z=−0.89 and z=2.56.  Round your answer to four decimal places, if necessary.
p <- pnorm(2.56) - pnorm(-0.89)
round(p, 4)

######## 9.2
# An operation manager at an electronics company wants to test their amplifiers.  The design engineer claims they have a mean output of 113 watts with a variance of 100.  
# What is the probability that the mean amplifier output would be greater than 112.5 watts in a sample of 43 amplifiers if the claim is true? Round your answer to four decimal places.
# Population parameters
mu <- 113            # population mean output (watts)
var <- 100           # population variance
sigma <- sqrt(var)   # population standard deviation
# Sample information
n <- 43              # sample size (sample of amplifiers)
# Value being tested
x <- 112.5           # sample mean threshold (watts)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean is greater than 112.5 watts
prob <- 1 - pnorm(x, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

########
# The mean weight of an adult is 65 kilograms with a standard deviation of 13 kilograms.  
# If 92 adults are randomly selected, what is the probability that the sample mean would be greater than 62.7 kilograms? Round your answer to four decimal places.
# Population parameters
mu <- 65              # population mean weight (kg)
sigma <- 13           # population standard deviation (kg)

# Sample information
n <- 92               # sample size (number of adults selected)

# Value being tested
x <- 62.7             # sample mean threshold (kg)

# Standard error of the sample mean
se <- sigma / sqrt(n)

# Probability that the sample mean is greater than 62.7 kg
prob <- 1 - pnorm(x, mean = mu, sd = se)

# Rounded answer
round(prob, 4)

#######
# A study on the latest fad diet claimed that the amounts of weight lost by all people on this diet had a mean of 20.8 pounds and a standard deviation of 4.2 pounds. 
# Step 1 of 2 : If a sampling distribution is created using samples of the amounts of weight lost by 89 people on this diet, what would be the mean of the sampling distribution of sample means? Round to two decimal places, if necessary.
# Population parameters
mu <- 20.8            # population mean weight loss (pounds)
sigma <- 4.2          # population standard deviation (pounds)

# Sample information
n <- 89               # sample size (number of people in each sample)

# Mean of the sampling distribution of sample means
sampling_mean <- mu

# Rounded answer
round(sampling_mean, 2)

#  Step 2 of 2 : If a sampling distribution is created using samples of the amounts of weight lost by 89 people on this diet, what would be the standard deviation of the sampling distribution of sample means? Round to two decimal places, if necessary.
# Standard deviation of the sampling distribution of the sample mean (standard error)
se <- sigma / sqrt(n)
# Rounded answer
round(se, 2)

############
# The mean life of a television set is 141 months with a variance of 256.
# If a sample of 71  televisions is randomly selected, what is the probability that the sample mean would differ from the true mean by greater than  4.7  months? Round your answer to four decimal places.
# Population parameters
mu <- 141                 # population mean life of televisions (months)
var <- 256                # population variance
sigma <- sqrt(var)        # population standard deviation
# Sample information
n <- 71                   # sample size (number of televisions)
# Difference from the true mean being tested
d <- 4.7                  # amount the sample mean differs from the population mean
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean differs from the true mean by more than 4.7 months
prob <- 2 * (1 - pnorm(mu + d, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

#############
# he cost of 5 gallons of ice cream has a standard deviation of 8 dollars with a mean of 29 dollars during the summer.
# What is the probability that the sample mean would differ from the true mean by less than  1.9  dollars if a sample of 92  5-gallon pails is randomly selected? Round your answer to four decimal places.
# Population parameters
mu <- 29              # population mean cost of 5-gallon ice cream pails (dollars)
sigma <- 8            # population standard deviation (dollars)
# Sample information
n <- 92               # sample size (number of 5-gallon pails sampled)
# Allowed difference from the true mean
d <- 1.9              # maximum difference from the population mean (dollars)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Lower and upper bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the true mean by less than 1.9 dollars
prob <- pnorm(upper, mean = mu, sd = se) - pnorm(lower, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

############
# The quality control manager at a computer manufacturing company believes that the mean life of a computer is 88 months, with a variance of 81.  
# If he is correct, what is the probability that the mean of a sample of 84 computers would differ from the population mean by less than 1.39 months? Round your answer to four decimal places.
# Population parameters
mu <- 88                 # population mean life of computers (months)
var <- 81                # population variance
sigma <- sqrt(var)       # population standard deviation
# Sample information
n <- 84                  # sample size (number of computers)
# Allowed difference from the true mean
d <- 1.39                # maximum difference from the population mean (months)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Lower and upper bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the true mean by less than 1.39 months
prob <- pnorm(upper, mean = mu, sd = se) - pnorm(lower, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

#########
# Thompson and Thompson is a steel bolts manufacturing company.  Their current steel bolts have a mean diameter of 148 millimeters, and a standard deviation of 7 millimeters.  
# If a random sample of 31 steel bolts is selected, what is the probability that the sample mean would differ from the population mean by more than 2.2 millimeters? Round your answer to four decimal places.
# Population parameters
mu <- 148               # population mean bolt diameter (millimeters)
sigma <- 7              # population standard deviation (millimeters)
# Sample information
n <- 31                 # sample size (number of steel bolts)
# Difference from the true mean being tested
d <- 2.2                # amount the sample mean differs from the population mean (millimeters)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Lower and upper bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the true mean by more than 2.2 mm
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

##############
# A racing car consumes a mean of 87 gallons of gas per race with a standard deviation of 6 gallons.  
# If 41 racing cars are randomly selected, what is the probability that the sample mean would differ from the population mean by more than 0.6 gallons? Round your answer to four decimal places.
# Population parameters
mu <- 87               # population mean gas consumption per race (gallons)
sigma <- 6             # population standard deviation (gallons)
# Sample information
n <- 41                # sample size (number of racing cars)
# Difference from the true mean being tested
d <- 0.6               # amount the sample mean differs from the population mean (gallons)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Lower and upper bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the true mean by more than 0.6 gallons
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

##########
# Intelligence Quotient (IQ) scores are often reported to be normally distributed with μ=100.0 and σ=15.0. A random sample of 57 people is taken.
# Step 1 of 2 : What is the probability of a random person on the street having an IQ score of less than 99? Round your answer to 4 decimal places, if necessary.
# Population parameters
mu <- 100          # population mean IQ
sigma <- 15        # population standard deviation of IQ scores
# Value being tested
x <- 98           # IQ score threshold
# Probability a random person has IQ less than 99
prob <- pnorm(x, mean = mu, sd = sigma)
# Rounded answer
round(prob, 4)

#  Step 2 of 2 : What is the probability that the mean IQ score of people in the sample is less than 99? Round your answer to 4 decimal places, if necessary.
# Population parameters
mu <- 100          # population mean IQ
sigma <- 15        # population standard deviation of IQ scores
# Sample information
n <- 63            # sample size (number of people in the sample)
# Value being tested
x <- 99            # sample mean IQ threshold
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean IQ is less than 99
prob <- pnorm(x, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

##########
# Calculate the standard score (z-score) of the given sample mean. Round your answer to two decimal places.
# μ=44 and σ=11; n=51; x¯=46
# Population parameters
mu <- 44            # population mean
sigma <- 11         # population standard deviation
# Sample information
n <- 51             # sample size
xbar <- 46          # sample mean
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Z-score of the sample mean
z <- (xbar - mu) / se
# Rounded answer
round(z, 2)

##########
# The mean cost of a five pound bag of shrimp is 50 dollars with a standard deviation of 8 dollars.  
# If a sample of 56  bags of shrimp is randomly selected, what is the probability that the sample mean would be less than  49.1  dollars? Round your answer to four decimal places.
# Population parameters
mu <- 50              # population mean cost of shrimp bags (dollars)
sigma <- 8            # population standard deviation (dollars)
# Sample information
n <- 56               # sample size (number of shrimp bags)
# Value being tested
xbar <- 49.1          # sample mean threshold (dollars)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean is less than 49.1 dollars
prob <- pnorm(xbar, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

#############
# The mean life of a television set is 138 months with a variance of 324.
# If a sample of 83  televisions is randomly selected, what is the probability that the sample mean would be less than  143.4  months? Round your answer to four decimal places.
# Population parameters
mu <- 138             # population mean life of televisions (months)
var <- 324            # population variance
sigma <- sqrt(var)    # population standard deviation
# Sample information
n <- 83               # sample size (number of televisions)
# Value being tested
xbar <- 143.4         # sample mean threshold (months)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean is less than 143.4 months
prob <- pnorm(xbar, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

########
# The mean weight of an adult is 70 kilograms with a standard deviation of 8 kilograms.  
# If 87 adults are randomly selected, what is the probability that the sample mean would be greater than 70.8 kilograms? Round your answer to four decimal places.
# Population parameters
mu <- 70              # population mean weight (kg)
sigma <- 8            # population standard deviation (kg)
# Sample information
n <- 87               # sample size (number of adults)
# Value being tested
xbar <- 70.8          # sample mean threshold (kg)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Probability that the sample mean is greater than 70.8 kg
prob <- 1 - pnorm(xbar, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

########
# A study on the latest fad diet claimed that the amounts of weight lost by all people on this diet had a mean of 22.1 pounds and a standard deviation of 5.5 pounds. 
# Step 1 of 2 :   If a sampling distribution is created using samples of the amounts of weight lost by 76 people on this diet, what would be the mean of the sampling distribution of sample means? Round to two decimal places, if necessary.
# Population parameters
mu <- 22.1           # population mean weight loss (pounds)
sigma <- 5.5         # population standard deviation (pounds)
# Sample information
n <- 76              # sample size (number of people in each sample)
# Mean of the sampling distribution of sample means
sampling_mean <- mu  # sampling distribution mean equals population mean
# Rounded answer
round(sampling_mean, 2)

#  Step 2 of 2 : If a sampling distribution is created using samples of the amounts of weight lost by 76 people on this diet, what would be the standard deviation of the sampling distribution of sample means? Round to two decimal places, if necessary.
# Population parameters
mu <- 22.1           # population mean weight loss (pounds)
sigma <- 5.5         # population standard deviation (pounds)
# Sample information
n <- 76              # sample size (number of people in each sample)
# Standard deviation of the sampling distribution of the sample mean (standard error)
se <- sigma / sqrt(n)
# Rounded answer
round(se, 2)

##########
# The mean weight of an adult is 66 kilograms with a variance of 144.  
# If 123 adults are randomly selected, what is the probability that the sample mean would differ from the population mean by more than 0.8 kilograms? Round your answer to four decimal places.
# Population parameters
mu <- 66               # population mean weight (kg)
var <- 144             # population variance
sigma <- sqrt(var)     # population standard deviation
# Sample information
n <- 123               # sample size (number of adults)
# Difference from the population mean being tested
d <- 0.8               # amount the sample mean differs from the population mean (kg)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Lower and upper bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the population mean by more than 0.8 kg
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

########
# Decide if the statement is True or False.
# A sampling distribution of sample means has a mean equal to the population mean, μ, divided by the square root of the sample size.
# FALSE

###########
# The mean cost of a five pound bag of shrimp is 42 dollars with a variance of 49.  
# If a sample of 54  bags of shrimp is randomly selected, what is the probability that the sample mean would differ from the true mean by less than  1.4  dollars? Round your answer to four decimal places.
# Population parameters
mu <- 42              # population mean cost of shrimp bags (dollars)
var <- 49             # population variance
sigma <- sqrt(var)    # population standard deviation
# Sample information
n <- 54               # sample size (number of shrimp bags)
# Allowed difference from the population mean
d <- 1.4              # maximum difference from the true mean (dollars)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the true mean by less than 1.4 dollars
prob <- pnorm(upper, mean = mu, sd = se) - pnorm(lower, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

############
# Calculate the standard score (z-score) of the given sample mean. Round your answer to two decimal places.
# μ=59 and σ=14; n=36; x¯=65
# Population parameters
mu <- 59            # population mean
sigma <- 14         # population standard deviation
# Sample information
n <- 36             # sample size
xbar <- 65          # sample mean
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Z-score of the sample mean
z <- (xbar - mu) / se
# Rounded answer
round(z, 2)

###########
# The operation manager at a tire manufacturing company believes that the mean mileage of a tire is 32,268 miles, with a standard deviation of 3345 miles.  
# What is the probability that the sample mean would differ from the population mean by less than 466 miles in a sample of 185 tires if the manager is correct? Round your answer to four decimal places.
# Population parameters
mu <- 32268           # population mean tire mileage (miles)
sigma <- 3345         # population standard deviation (miles)
# Sample information
n <- 185              # sample size (number of tires)
# Allowed difference from the population mean
d <- 466              # maximum difference from the true mean (miles)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the population mean by less than 466 miles
prob <- pnorm(upper, mean = mu, sd = se) - pnorm(lower, mean = mu, sd = se)
# Rounded answer
round(prob, 4)

# Problem: Tire company mean mileage probability; mu=28631, variance=16834610, n=178, find P(xbar > 28455)

mu = 28631
sigma = sqrt(16834610)
n = 178
xbar = 28455

SE = sigma / sqrt(n)

z = (xbar - mu) / SE

prob = 1 - pnorm(z)

round(prob, 4)

#######
# Thompson and Thompson is a steel bolts manufacturing company.  Their current steel bolts have a mean diameter of 143 millimeters, and a standard deviation of 5 millimeters.  
# If a random sample of 47 steel bolts is selected, what is the probability that the sample mean would differ from the population mean by more than 0.2 millimeters? Round your answer to four decimal places.
# Population parameters
mu <- 143              # population mean bolt diameter (millimeters)
sigma <- 5             # population standard deviation (millimeters)
# Sample information
n <- 47                # sample size (number of steel bolts)
# Difference from the population mean being tested
d <- 0.2               # amount the sample mean differs from the population mean (millimeters)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the population mean by more than 0.2 mm
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

##########
# The mean per capita income is 23,221 dollars per annum with a standard deviation of 419 dollars per annum.
# What is the probability that the sample mean would differ from the true mean by greater than  35  dollars if a sample of 348  persons is randomly selected? Round your answer to four decimal places.
# Population parameters
mu <- 23221            # population mean per capita income (dollars)
sigma <- 419           # population standard deviation (dollars)
# Sample information
n <- 348               # sample size (number of persons)
# Difference from the population mean being tested
d <- 35                # amount the sample mean differs from the population mean (dollars)
# Standard error of the sample mean
se <- sigma / sqrt(n)
# Bounds for the sample mean
lower <- mu - d
upper <- mu + d
# Probability that the sample mean differs from the population mean by more than 35 dollars
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
# Rounded answer
round(prob, 4)

########## 9.3
# A researcher believes that 6% of females smoke cigarettes.  
# If the researcher is correct, what is the probability that the proportion of smokers in a sample of 531 females would be greater than 4%? Round your answer to four decimal places.
# Population proportion
p <- 0.06              # true proportion of females who smoke
# Sample size
n <- 531               # number of females sampled
# Sample proportion threshold
phat <- 0.04           # proportion being tested
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score for the sample proportion
z <- (phat - p) / se
# Probability that the sample proportion is greater than 0.04
prob <- 1 - pnorm(z)
# Rounded result
round(prob, 4)

##############
# Suppose 58% of politicians are lawyers.  
# If a random sample of size 681 is selected, what is the probability that the proportion of politicians who are lawyers will be less than 55%? Round your answer to four decimal places.
# Population proportion
p <- 0.58              # true proportion of politicians who are lawyers
# Sample size
n <- 681               # number of politicians sampled
# Sample proportion threshold
phat <- 0.55           # proportion being tested
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score for the sample proportion
z <- (phat - p) / se
# Probability that the sample proportion is less than 0.55
prob <- pnorm(z)
# Rounded result
round(prob, 4)

###########
# A hotel manager believes that 17% of the hotel rooms are booked.  
# If the manager is right, what is the probability that the proportion of rooms booked in a sample of 859 rooms would be less than 14%? Round your answer to four decimal places.
# Population proportion
p <- 0.17              # true proportion of rooms booked
# Sample size
n <- 859               # number of rooms sampled
# Sample proportion threshold
phat <- 0.14           # proportion being tested
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score for the sample proportion
z <- (phat - p) / se
# Probability that the sample proportion is less than 0.14
prob <- pnorm(z)
# Rounded result
round(prob, 4)

##########
# A film distribution manager calculates that 8% of the films released are flops.  
# If the manager is correct, what is the probability that the proportion of flops in a sample of 564 released films would differ from the population proportion by greater than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.08              # true proportion of films that are flops
# Sample size
n <- 564               # number of films sampled
# Difference from population proportion
d <- 0.03              # difference threshold (3%)
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 3%
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

##########
# Goofy's fast food center wishes to estimate the proportion of people in its city that will purchase its products.  Suppose the true proportion is 0.06.  
# If 473 are sampled, what is the probability that the sample proportion will differ from the population proportion by less than 0.03? Round your answer to four decimal places.
# Population proportion
p <- 0.06              # true proportion of people who purchase products
# Sample size
n <- 473               # number of people sampled
# Allowed difference from population proportion
d <- 0.03              # difference threshold
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.03
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

#########
# Suppose 41% of the registered voters in a country are Republican.  
# If a sample of 488 voters is selected, what is the probability that the sample proportion of Republicans will be greater than 44%? Round your answer to four decimal places.
# Population proportion
p <- 0.41
# Sample size
n <- 488
# Sample proportion threshold
phat <- 0.44
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score
z <- (phat - p) / se
# Probability that the sample proportion is greater than 0.44
prob <- 1 - pnorm(z)
# Rounded result
round(prob, 4)

###########
# A courier service company wishes to estimate the proportion of people in various states that will use its services.  Suppose the true proportion is 0.06.  
# If 235 are sampled, what is the probability that the sample proportion will differ from the population proportion by greater than 0.04? Round your answer to four decimal places.
# Population proportion
p <- 0.06
# Sample size
n <- 235
# Difference threshold
d <- 0.04
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.04
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

#########
#  Step 1 of 2 : Given the following parameters for a sampling distribution of sample proportions, calculate the sample proportion. Round your answer to two decimal places.
# p=0.59, x=64, n=100
# Number of successes in the sample
x <-28
# Sample size
n <- 200
# Sample proportion
phat <- x / n
# Rounded result
round(phat, 2)

#  Step 2 of 2 : 
# Given the following parameters for a sampling distribution of sample proportions, calculate the standard score of the sample proportion. Round your answer to two decimal places.
# p=0.59,x=64,n=100
# Population proportion
p <- 0.21
# Number of successes in the sample
x <- 28
# Sample size
n <- 200
# Sample proportion
phat <- x / n
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score (standard score)
z <- (phat - p) / se
# Rounded result
round(z, 2)

#########
# The opera theater manager believes that 23% of the opera tickets for tonight's show have been sold.  
# If the manager is accurate, what is the probability that the proportion of tickets sold in a sample of 869 tickets would differ from the population proportion by less than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.23
# Sample size
n <- 869
# Allowed difference from population proportion
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.03
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

##########
# Suppose a large shipment of laptop computers contained 4% defectives.  
# If a sample of size 241 is selected, what is the probability that the sample proportion will differ from the population proportion by less than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.18
# Sample size
n <- 238
# Allowed difference from population proportion
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.03
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

# Suppose a large shipment of microwave ovens contained 18% defectives. If a sample of size 238 is selected, what is the probability that the sample proportion will differ from the population proportion by less than 3%?

p <- 0.18
n <- 238
se <- sqrt(p * (1 - p) / n)

lower <- p - 0.03
upper <- p + 0.03

prob <- pnorm(upper, mean = p, sd = se) - pnorm(lower, mean = p, sd = se)

round(prob, 4)




########
# A film distribution manager calculates that 4% of the films released are flops.  
# If the manager is right, what is the probability that the proportion of flops in a sample of 463 released films would be greater than 5%? Round your answer to four decimal places.
# Population proportion
p <- 0.04
# Sample size
n <- 463
# Sample proportion threshold
phat <- 0.05
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score
z <- (phat - p) / se
# Probability that the sample proportion is greater than 0.05
prob <- 1 - pnorm(z)
# Rounded result
round(prob, 4)

########
# A scientist claims that 7% of viruses are airborne.  
# If the scientist is accurate, what is the probability that the proportion of airborne viruses in a sample of 600 viruses would differ from the population proportion by more than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.07
# Sample size
n <- 600
# Difference threshold
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.03
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

########
# Suppose 53% of the population are more than 6 feet tall.  
# If a random sample of size 534 is selected, what is the probability that the proportion of persons more than 6 feet tall will differ from the population proportion by less than 6%? Round your answer to four decimal places.
# Population proportion
p <- 0.53
# Sample size
n <- 534
# Allowed difference from population proportion
d <- 0.06
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.06
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

#######
# Suppose 42% of politicians are lawyers.  
# If a random sample of size 628 is selected, what is the probability that the proportion of politicians who are lawyers will differ from the total politicians proportion by more than 5%? Round your answer to four decimal places.
# Population proportion
p <- 0.42
# Sample size
n <- 628
# Difference threshold
d <- 0.05
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.05
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

########
# Suppose 44% of politicians are lawyers.  
# If a random sample of size 411 is selected, what is the probability that the proportion of politicians who are lawyers will differ from the total politicians proportion by more than 7%? Round your answer to four decimal places.
# Population proportion
p <- 0.44
# Sample size
n <- 411
# Difference threshold
d <- 0.07
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.07
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

#######
# Suppose 48% of the banks in Switzerland are private organizations.  
# If a sample of 475 banks is selected, what is the probability that the sample proportion of private banks will be greater than 53%? Round your answer to four decimal places.
# Population proportion
p <- 0.48
# Sample size
n <- 475
# Sample proportion threshold
phat <- 0.53
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score
z <- (phat - p) / se
# Probability that the sample proportion is greater than 0.53
prob <- 1 - pnorm(z)
# Rounded result
round(prob, 4)

######
#  Step 1 of 2 : 
# Given the following parameters for a sampling distribution of sample proportions, calculate the sample proportion. Round your answer to two decimal places.
# p=0.43,x=37,n=100
# Number of successes in the sample
x <- 37
# Sample size
n <- 100
# Sample proportion
phat <- x / n
# Rounded result
round(phat, 2)

# Step 2 of 2 : Given the following parameters for a sampling distribution of sample proportions, calculate the standard score of the sample proportion. Round your answer to two decimal places.
# p=0.43,x=37,n=100
# Population proportion
p <- 0.43
# Number of successes
x <- 37
# Sample size
n <- 100
# Sample proportion
phat <- x / n
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score
z <- (phat - p) / se
# Rounded result
round(z, 2)

#######
# A telephone exchange operator assumes that 9% of the phone calls are wrong numbers.  
# If the operator is correct, what is the probability that the proportion of wrong numbers in a sample of 448 phone calls would differ from the population proportion by more than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.09
# Sample size
n <- 448
# Difference threshold
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.03
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

#####
# A direct mail company wishes to estimate the proportion of people on a large mailing list that will purchase a product.  Suppose the true proportion is 0.07.  
# If 411 are sampled, what is the probability that the sample proportion will differ from the population proportion by less than 0.03? Round your answer to four decimal places.
# Population proportion
p <- 0.07
# Sample size
n <- 411
# Difference threshold
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.03
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

######
# A courier service company wishes to estimate the proportion of people in various states that will use its services.  Suppose the true proportion is 0.06.  
# If 228 are sampled, what is the probability that the sample proportion will differ from the population proportion by more than 0.03? Round your answer to four decimal places.
# Population proportion
p <- 0.06
# Sample size
n <- 228
# Difference threshold
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.03
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

#####
# A fifth grade teacher assumes that 24% of her students are late for class.  
# If the teacher is right, what is the probability that the proportion of late students in a sample of 430 students would differ from the population proportion by less than 4%? Round your answer to four decimal places.
# Population proportion
p <- 0.24
# Sample size
n <- 430
# Difference threshold
d <- 0.04
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.04
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

#########
# A scientist claims that 4% of viruses are airborne.  
# If the scientist is accurate, what is the probability that the proportion of airborne viruses in a sample of 648 viruses would be greater than 5%? Round your answer to four decimal places.
# Population proportion
p <- 0.04
# Sample size
n <- 648
# Sample proportion threshold
phat <- 0.05
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score
z <- (phat - p) / se
# Probability that the sample proportion is greater than 0.05
prob <- 1 - pnorm(z)
# Rounded result
round(prob, 4)

#########
# Suppose 52% of the population has a college degree.  
# If a random sample of size 563 is selected, what is the probability that the proportion of persons with a college degree will differ from the population proportion by less than 5%? Round your answer to four decimal places.
# Population proportion
p <- 0.52
# Sample size
n <- 563
# Difference threshold
d <- 0.05
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.05
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

########
# Suppose a large shipment of compact discs contained 19% defectives.  
# If a sample of size 343 is selected, what is the probability that the sample proportion will differ from the population proportion by less than 4%? Round your answer to four decimal places.
# Population proportion
p <- 0.19
# Sample size
n <- 343
# Difference threshold
d <- 0.04
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by less than 0.04
prob <- pnorm(z) - pnorm(-z)
# Rounded result
round(prob, 4)

#######
# A statistician calculates that 8% of Americans are vegetarians.  
# If the statistician is correct, what is the probability that the proportion of vegetarians in a sample of 594 Americans would differ from the population proportion by greater than 3%? Round your answer to four decimal places.
# Population proportion
p <- 0.08
# Sample size
n <- 594
# Difference threshold
d <- 0.03
# Standard error of the sample proportion
se <- sqrt(p * (1 - p) / n)
# Z-score corresponding to the difference
z <- d / se
# Probability that the sample proportion differs from p by more than 0.03
prob <- 2 * (1 - pnorm(z))
# Rounded result
round(prob, 4)

############
# Problem:
# Suppose a batch of metal shafts produced in a manufacturing company have a variance of 2.89
# and a mean diameter of 211 inches.
#
# If 86 shafts are sampled at random from the batch, what is the probability that the mean
# diameter of the sample shafts would be less than 210.8 inches?
# Given values
mu <- 211
variance <- 2.89
sigma <- sqrt(variance)
n <- 86
x_bar <- 210.8
# Standard error
SE <- sigma / sqrt(n)
# z-score
z <- (x_bar - mu) / SE
# Probability
round(pnorm(z), 4)

#########
# Problem:
# Thompson and Thompson is a steel bolts manufacturing company. Their current steel bolts
# have a mean diameter of 131 millimeters and a standard deviation of 7 millimeters.
#
# If a random sample of 31 steel bolts is selected, what is the probability that the
# sample mean would be less than 132.2 millimeters?

# Given values
mu <- 131
sigma <- 7
n <- 31
x_bar <- 132.2
# Standard error
SE <- sigma / sqrt(n)
# z-score
z <- (x_bar - mu) / SE
# Probability
round(pnorm(z), 4)

###########
# Problem:
# A direct mail company wishes to estimate the proportion of people on a large
# mailing list that will purchase a product. Suppose the true proportion is 0.07.
#
# If 216 are sampled, what is the probability that the sample proportion
# will be less than 0.11?
# Given values
p <- 0.07
n <- 216
p_hat <- 0.11
# Standard error
SE <- sqrt((p * (1 - p)) / n)
# z-score
z <- (p_hat - p) / SE
# Probability (rounded to four decimals)
round(pnorm(z), 4)

##########
# Problem:
# Given the following parameters for a sampling distribution of sample proportions,
# calculate the sample proportion.
#
# p = 0.78, x = 249, n = 300
# Round the answer to two decimal places.
# Given values
p <- 0.78
x <- 249
n <- 300
# Sample proportion
p_hat <- x / n
# Rounded result
round(p_hat, 2)

#########
# Problem:
# Given the following parameters for a sampling distribution of sample proportions,
# calculate the standard score of the sample proportion.
#
# p = 0.78, x = 249, n = 300
# Round the answer to two decimal places.

# Given values
p <- 0.78
x <- 249
n <- 300
# Sample proportion
p_hat <- x / n
# Standard error
SE <- sqrt((p * (1 - p)) / n)
# Standard score
z <- (p_hat - p) / SE
# Rounded result
round(z, 2)

##########
# Problem:
# The mean cost of a five pound bag of shrimp is 42 dollars with a variance of 49.
# If a sample of 54 bags is randomly selected, find the probability that the
# sample mean differs from the true mean by less than 1.4 dollars.

# Given values
mu <- 42
variance <- 49
sigma <- sqrt(variance)
n <- 54
difference <- 1.4
# Standard error
SE <- sigma / sqrt(n)
# z-score
z <- difference / SE
# Probability
prob <- 2 * pnorm(z) - 1
# Rounded answer
round(prob, 4)

############
# Identify the sampling technique used for the following study.
# After a college student has enumerated a sampling frame, he picks a random starting point and chooses every fifteenth member.
# Systematic Sampling

#########
# Problem:
# A carpet expert believes that 5% of Persian carpets are counterfeits.
# If the expert is right, what is the probability that the proportion
# of counterfeits in a sample of 460 carpets would be less than 6%?
# Given values
p <- 0.05
n <- 460
p_hat <- 0.06
# Standard error
SE <- sqrt((p * (1 - p)) / n)
# z-score
z <- (p_hat - p) / SE
# Probability
round(pnorm(z), 4)

########
# Problem:
# Suppose that the walking step lengths of adult males are normally distributed
# with a mean of 2.8 feet and a standard deviation of 0.4 feet. A sample of
# 75 men’s step lengths is taken.
#
# Step 1 of 2 :
# Find the probability that an individual man’s step length is less than
# 2.4 feet. Round your answer to 4 decimal places, if necessary.
# Given values
mu <- 2.8
sigma <- 0.4
x <- 2.4
# z-score
z <- (x - mu) / sigma
# probability
round(pnorm(z), 4)
# Step 2 of 2 :
# Find the probability that the mean of the sample taken is less than 2.4 feet. Round your answer to 4 decimal places, if necessary.
# Given values
mu <- 2.8
sigma <- 0.4
n <- 75
x_bar <- 2.4
# Standard error
SE <- sigma / sqrt(n)
# z-score
z <- (x_bar - mu) / SE
# Probability
round(pnorm(z), 4)

###############
# Problem:
# Thompson and Thompson is a steel bolts manufacturing company.  Their current steel bolts have a mean diameter of 131 millimeters, and a standard deviation of 7 millimeters.  
#
# If a random sample of 31 steel bolts is selected, what is the probability that the sample mean would be less than 132.2 millimeters? Round your answer to four decimal places.
# Given values
mu <- 131
sigma <- 7
n <- 31
x_bar <- 132.2
# Standard error
SE <- sigma / sqrt(n)
# z-score
z <- (x_bar - mu) / SE
# Rounded probability
round(pnorm(z), 4)

############
# Problem:
# A direct mail company wishes to estimate the proportion of people on a large mailing list that will purchase a product.  Suppose the true proportion is 0.07.  
#
# If 216 are sampled, what is the probability that the sample proportion will be less than 0.11? Round your answer to four decimal places.
# Given values
p <- 0.07
n <- 216
p_hat <- 0.11
# Standard error
SE <- sqrt((p * (1 - p)) / n)
# z-score
z <- (p_hat - p) / SE
# Rounded probability
round(pnorm(z), 4)

##########
# Problem:
# The mean cost of a five pound bag of shrimp is 42 dollars with a variance of 49.  
#
# If a sample of 54  bags of shrimp is randomly selected, what is the probability that the sample mean would differ from the true mean by less than  1.4  dollars? Round your answer to four decimal places.
# Given values
mu <- 42
variance <- 49
sigma <- sqrt(variance)
n <- 54
difference <- 1.4
# Standard error
SE <- sigma / sqrt(n)
# z-score bound
z <- difference / SE
# Rounded probability
round(2 * pnorm(z) - 1, 4)

###########
# Problem:
# A carpet expert believes that 5% of Persian carpets are counterfeits.  
#
# If the expert is right, what is the probability that the proportion of counterfeits in a sample of 460 Persian carpets would be less than 6%? Round your answer to four decimal places
# Given values
p <- 0.05
n <- 460
p_hat <- 0.06
# Standard error
SE <- sqrt((p * (1 - p)) / n)
# z-score
z <- (p_hat - p) / SE
# Rounded probability
round(pnorm(z), 4)

##########
# Problem:
# A courier service company wishes to estimate the proportion of people in various states that will use its services.  Suppose the true proportion is 0.05.  
#
# If 330 are sampled, what is the probability that the sample proportion will differ from the population proportion by greater than 0.03? Round your answer to four decimal places.
p <- 0.05
n <- 330
difference <- 0.03
SE <- sqrt((p*(1-p))/n)
z <- difference/SE
round(2*(1-pnorm(z)),4)

########
# Identify the sampling technique used for the following study.
# A political strategist randomly selects five hundred phone numbers from a region to determine which issues are most important to the citizens of that region.
# simple random sampling

########
# Problem:
# Suppose a batch of steel rods produced at a steel plant have a mean length of 150 millimeters, and a variance of 100.  
# If 275 rods are sampled at random from the batch, what is the probability that the mean length of the sample rods would differ from the population mean by greater than 0.57 millimeters? Round your answer to four decimal places.
mu <- 150
variance <- 100
sigma <- sqrt(variance)
n <- 275
difference <- 0.57
SE <- sigma/sqrt(n)
z <- difference/SE
round(2*(1-pnorm(z)),4)

##########
# Problem:
# Suppose 53% of American singers are Grammy award winners.  
# If a random sample of size 834 is selected, what is the probability that the proportion of Grammy award winners will differ from the singers proportion by less than 4%? Round your answer to four decimal places.
p <- 0.53
n <- 834
difference <- 0.04
SE <- sqrt((p*(1-p))/n)
z <- difference/SE
round(2*pnorm(z)-1,4)

##########
# Problem:
# Suppose that a study of elementary school students reports that the mean age at which children begin reading is 5.7 years with a standard deviation of 0.7 years.
# Step 1 of 2 :
# If a sampling distribution is created using samples of the ages at which 55 children begin reading, what would be the mean of the sampling distribution of sample means? Round to two decimal places, if necessary.
mu <- 5.7
round(mu,2)
# Step 2 of 2 :
# If a sampling distribution is created using samples of the ages at which 55 children begin reading, what would be the standard deviation of the sampling distribution of sample means? Round to two decimal places, if necessary.
sigma <- 0.7
n <- 55
SE <- sigma/sqrt(n)
round(SE,2)

#########
# Identify the sampling technique used for the following study.
# A pollster interviews each member from each of the ten randomly chosen math classes at a university.
# Cluster

##########
# Identify the sampling technique used for the following study.
# A school administrator selects fifty students by randomly selecting names out of the list of all students enrolled.
# simple random sampling

#######
# Suppose babies born in a large hospital have a mean weight of 3225 grams, and a standard deviation of 535 grams.
# If 106 babies are sampled at random from the hospital, what is the probability that the mean weight of the sample babies would be less than 3178 grams? Round your answer to four decimal places.
x <- 3178
mu <- 3225
sigma <- 535
n <- 106
se <- sigma / sqrt(n)
prob <- pnorm(x, mean = mu, sd = se)
round(prob, 4)

###########
# Suppose 41% of American singers are Grammy award winners.
# If a random sample of size 860 is selected, what is the probability that
# the proportion of Grammy award winners will differ from the singers proportion
# by less than 5%? Round your answer to four decimal places.

# p = population proportion of Grammy award winners
p <- 0.41

# n = sample size (number of singers sampled)
n <- 860

# difference allowed from the population proportion (5%)
diff <- 0.05

# lower bound for the sample proportion
lower <- p - diff

# upper bound for the sample proportion
upper <- p + diff

# standard deviation (standard error) of the sample proportion
se <- sqrt((p * (1 - p)) / n)

# probability that the sample proportion lies within the bounds
prob <- pnorm(upper, mean = p, sd = se) - pnorm(lower, mean = p, sd = se)

# round answer to four decimal places
round(prob, 4)

###########
# Suppose that a study of elementary school students reports that the mean age 
# at which children begin reading is 5.6 years with a standard deviation of 0.7 years.
# Step 1 of 2:
# If a sampling distribution is created using samples of the ages at which 49 children begin reading,
# what would be the mean of the sampling distribution of sample means?
# mu = population mean age at which children begin reading
mu <- 5.6
# n = sample size (number of children in each sample)
n <- 49
# The mean of the sampling distribution of the sample mean equals the population mean
mu_xbar <- mu
# round to two decimal places
round(mu_xbar, 2)

##########
# Step 2 of 2:
# If a sampling distribution is created using samples of the ages at which 49 children begin reading,
# what would be the standard deviation of the sampling distribution of sample means?
# sigma = population standard deviation of reading age
sigma <- 0.7
# n = sample size (number of children in each sample)
n <- 49
# standard deviation of the sampling distribution of the sample mean (standard error)
se <- sigma / sqrt(n)
# round to two decimal places
round(se, 2)

##########
# After a college student has assigned identification numbers to a population, he picks a random starting point and chooses every tenth member. 
# Systematic sampling

###########
# Suppose babies born in a large hospital have a mean weight of 3758 grams,
# and a variance of 204,304.
# If 89 babies are sampled at random from the hospital,
# what is the probability that the mean weight of the sample babies
# would differ from the population mean by more than 48 grams?
# mu = population mean baby weight
mu <- 3758
# variance = population variance of baby weights
variance <- 204304
# sigma = population standard deviation
sigma <- sqrt(variance)
# n = sample size
n <- 89
# difference allowed from the mean
diff <- 48
# standard error of the sample mean
se <- sigma / sqrt(n)
# lower bound
lower <- mu - diff
# upper bound
upper <- mu + diff
# probability outside the interval
prob <- pnorm(lower, mean = mu, sd = se) + (1 - pnorm(upper, mean = mu, sd = se))
round(prob, 4)

#############
# The mean per capita income is 16,445 dollars per annum with a standard deviation of 397 dollars per annum.
# What is the probability that the sample mean would differ from the true mean by less than 38 dollars if a sample of 208 persons is randomly selected?
# mu = population mean income
mu <- 16445
# sigma = population standard deviation
sigma <- 397
# n = sample size
n <- 208
# diff = maximum difference allowed from the population mean
diff <- 38
# standard error of the sample mean
se <- sigma / sqrt(n)
# lower bound of sample mean
lower <- mu - diff
# upper bound of sample mean
upper <- mu + diff
# probability that the sample mean lies within the bounds
prob <- pnorm(upper, mean = mu, sd = se) - pnorm(lower, mean = mu, sd = se)
# round to four decimals
round(prob, 4)

###########
# Step 1 of 2:
# Given the following parameters for a sampling distribution of sample proportions, calculate the sample proportion. Round your answer to two decimal places.
# p = 0.22 (population proportion, not needed for Step 1)
# x = 51 (number of successes observed in the sample)
# n = 300 (sample size)
# x = number of successes in the sample
x <- 51
# n = sample size
n <- 300
# sample proportion
p_hat <- x / n
# round to two decimal places
round(p_hat, 2)

# Step 2 of 2: Given the following parameters for a sampling distribution of sample proportions, calculate the standard score of the sample proportion. Round your answer to two decimal places. p=0.22, x=51, n=300
# p = population proportion
p <- 0.22
# x = number of successes in the sample
x <- 51
# n = sample size
n <- 300
# p_hat = sample proportion
p_hat <- x / n
# standard error of the sample proportion
se <- sqrt((p * (1 - p)) / n)
# z = standard score of the sample proportion
z <- (p_hat - p) / se
# round to two decimal places
round(z, 2)

############
# A film distribution manager calculates that 6% of the films released are flops. If the manager is right, what is the probability that the proportion of flops in a sample of 540 released films would be less than 8%? Round your answer to four decimal places.
# p = population proportion of flops
p <- 0.06
# n = sample size
n <- 540
# p_hat = sample proportion threshold (8%)
p_hat <- 0.08
# standard error of the sample proportion
se <- sqrt((p * (1 - p)) / n)
# probability that the sample proportion is less than 0.08
prob <- pnorm(p_hat, mean = p, sd = se)
# round to four decimal places
round(prob, 4)

##########
# Identify the sampling technique used for the following study.
# A market researcher interviews each member from each of the ten randomly chosen zoning districts in a town.
# cluster sampling

#########
# Thompson and Thompson is a steel bolts manufacturing company. Their current steel bolts have a mean diameter of 135 millimeters, and a variance of 64. If a random sample of 32 steel bolts is selected, what is the probability that the sample mean would be less than 133 millimeters? Round your answer to four decimal places.
# mu = population mean bolt diameter
mu <- 135
# variance = population variance
variance <- 64
# sigma = population standard deviation
sigma <- sqrt(variance)
# n = sample size
n <- 32
# x_bar = sample mean threshold
x_bar <- 133
# standard error of the sample mean
se <- sigma / sqrt(n)
# probability that the sample mean is less than 133
prob <- pnorm(x_bar, mean = mu, sd = se)
# round to four decimals
round(prob, 4)

########### Ch 10

#########
# Consider a sample of paper clips. Their weights are measured and found to have a standard deviation of 2.18. Give a point estimate for the population standard deviation in weights of paper clips. Round your answer to two decimal places, if necessary.
# s = sample standard deviation (point estimator for population standard deviation sigma)
s <- 2.18
# point estimate for population standard deviation (sigma_hat)
sigma_hat <- s
# round to two decimal places
round(sigma_hat, 2)

#########
# What is the best point estimate for the population's variance if the sample standard deviation is 14.7? Round your answer to one decimal place, if necessary.
# s = sample standard deviation
s <- 14.7
# variance estimate (sigma^2 hat) is s^2
variance_hat <- s^2
# round to one decimal place
round(variance_hat, 1)

#########
# At the local college, a study found that students earned an average of 13.8 credit hours per semester. A sample of 118 students was taken. What is the best point estimate for the average number of credit hours per semester for all students at the local college?
# x_bar = sample mean (best point estimator for population mean mu)
x_bar <- 8.3
# point estimate for population mean (mu_hat)
mu_hat <- x_bar
# no rounding needed
mu_hat

########
# The best estimator of a population parameter is one that
# Is unbiased

#########
# The mean GPA for 96 residents of the local apartment complex is 1.8. What is the best point estimate for the mean GPA for all residents of the local apartment complex?
# x_bar = sample mean (best point estimator for population mean mu)
x_bar <- 1.8
# point estimate for population mean (mu_hat)
mu_hat <- x_bar
# no rounding needed
mu_hat

#########
# In a sample of 115 curtains, the average length was found to be 32.2 in. with a standard deviation of 0.8. Give a point estimate for the population standard deviation of the length of the curtains. Round your answer to two decimal places, if necessary.
# s = sample standard deviation (best point estimator for population standard deviation sigma)
s <- 0.8
# point estimate for population standard deviation (sigma_hat)
sigma_hat <- s
# round to two decimal places
round(sigma_hat, 2)

#########
# A sample of 5 rivets is randomly selected and the following diameters are measured in millimeters: 7.39, 7.56, 7.41, 7.46, 7.49. Give a point estimate for the population standard deviation. Round your answer to three decimal places.
# data = sample diameters
data <- c(7.39, 7.56, 7.41, 7.46, 7.49)
# s = sample standard deviation (best point estimator for population standard deviation sigma)
s <- sd(data)
# point estimate for population standard deviation (sigma_hat)
sigma_hat <- s
# round to three decimal places
round(sigma_hat, 3)

#########
# Suppose a sample of 227 tankers is drawn. Of these ships, 47 had spills. Using the data, estimate the proportion of oil tankers that had spills. Enter your answer as a fraction or a decimal number rounded to three decimal places.
# x = number of tankers with spills
x <- 47
# n = sample size
n <- 227
# p_hat = sample proportion (best point estimator for population proportion p)
p_hat <- x / n
# round to three decimal places
round(p_hat, 3)

#########
# Suppose a sample of 2649 new car buyers is drawn. Of those sampled, 805 preferred foreign over domestic cars. Using the data, estimate the proportion of new car buyers who prefer foreign cars. Enter your answer as a simplified fraction or a decimal number rounded to three decimal places.
# x = number of buyers who prefer foreign cars
x <- 805
# n = sample size
n <- 2649
# p_hat = sample proportion (best point estimator for population proportion p)
p_hat <- x / n
# round to three decimal places
round(p_hat, 3)

#########
# A sample of 6 sewing needles is randomly selected and the following diameters are measured in millimeters: 1.51, 1.66, 1.66, 1.65, 1.63, 1.63. Give a point estimate for the population variance. Round your answer to three decimal places.
# data = sample diameters
data <- c(1.51, 1.66, 1.66, 1.65, 1.63, 1.63)
# s^2 = sample variance (best point estimator for population variance sigma^2)
variance_hat <- var(data)
# round to three decimal places
round(variance_hat, 3)

######
# defines an upper and lower boundary for an interval that will hopefully contain the population parameter.
# An Interval Estimate

#########
# What is the best point estimate for the population's variance if the sample standard deviation is 15.9? Round your answer to one decimal place, if necessary.
# s = sample standard deviation
s <- 15.9
# variance estimate (sigma^2 hat) is s^2
variance_hat <- s^2
# round to one decimal place
round(variance_hat, 1)

#########
# The mean monthly water bill for 62 residents of the local apartment complex is $137. What is the best point estimate for the mean monthly water bill for all residents of the local apartment complex?
# x_bar = sample mean (best point estimator for population mean mu)
x_bar <- 137
# point estimate for population mean (mu_hat)
mu_hat <- x_bar
# no rounding needed
mu_hat

#########
# In a sample of 138 tables, the average height was found to be 34.3 in. with a variance of 0.7. Give a point estimate for the population variance of the height of the tables. Round your answer to two decimal places, if necessary.
# s^2 = sample variance (best point estimator for population variance sigma^2)
s_squared <- 0.7
# point estimate for population variance (sigma^2 hat)
variance_hat <- s_squared
# round to two decimal places
round(variance_hat, 2)

### Critical Values
# 80% = 1.28
# 90% = 1.645
# 95% = 1.96
# 98% = 2.33
# 99% = 2.575

#########
# A research scholar wants to know how many times per hour a certain strand of virus reproduces. The mean is found to be 6.4 reproductions and the population standard deviation is known to be 2.4. If a sample of 955 was used for the study, construct the 99% confidence interval for the true mean number of reproductions per hour for the virus. Round your answers to one decimal place.
# x_bar = sample mean
x_bar <- 11.3
# sigma = population standard deviation
sigma <- 2.3
# n = sample size
n <- 1083
# z = critical value for 99% confidence
z <- 2.33
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to one decimal place
round(c(lower, upper), 1)

#########
# Given the following confidence interval for a population mean, compute the margin of error, E: 17.00 < mu < 17.70
# lower = lower bound of confidence interval
lower <- 17.00
# upper = upper bound of confidence interval
upper <- 17.70
# margin of error E = (upper - lower) / 2
E <- (upper - lower) / 2
# result
E

#########
# The chief purchaser for the State Education Commission is reviewing test data for a metal link chain which will be used on children’s swing sets in elementary school playgrounds. The average breaking strength for a random sample of 40 pieces of chain is 5000 pounds. Based on past experience, the breaking strength of metal chains is known to be normally distributed with a standard deviation of 100 pounds. Estimate the actual mean breaking strength of the metal link chain with 98% confidence. Round the endpoints of the interval to the nearest whole number.
# x_bar = sample mean
x_bar <- 5000
# sigma = population standard deviation
sigma <- 100
# n = sample size
n <- 40
# z = critical value for 98% confidence
z <- 2.33
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to nearest whole number
round(c(lower, upper), 0)

#########
# A soft drink manufacturer wishes to know how many soft drinks adults drink each week. They want to construct a 85% confidence interval for the mean and are assuming that the population standard deviation for the number of soft drinks consumed each week is 0.9. The study found that for a sample of 256 adults the mean number of soft drinks consumed per week is 6.4. Construct the desired confidence interval. Round your answers to one decimal place.
# x_bar = sample mean
x_bar <- 6.4
# sigma = population standard deviation
sigma <- 0.9
# n = sample size
n <- 256
# z = critical value for 85% confidence (alpha/2 = 0.075)
z <- qnorm(1 - 0.075)
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to one decimal place
round(c(lower, upper), 1)

#########
# An educational psychologist wishes to know the mean number of words a third grader can read per minute. She wants to make an estimate at the 90% level of confidence. For a sample of 292 third graders, the mean words per minute read was 34.4. Assume a population standard deviation of 2.5. Construct the confidence interval for the mean number of words a third grader can read per minute. Round your answers to one decimal place.
# x_bar = sample mean
x_bar <- 34.4
# sigma = population standard deviation
sigma <- 2.5
# n = sample size
n <- 292
# z = critical value for 90% confidence
z <- 1.645
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to one decimal place
round(c(lower, upper), 1)

#########
# Given the following confidence interval for a population mean, compute the margin of error, E: 16.39 < mu < 18.25
# lower = lower bound of confidence interval
lower <- 16.39
# upper = upper bound of confidence interval
upper <- 18.25
# margin of error E = (upper - lower) / 2
E <- (upper - lower) / 2
# result
E

#########
# The water works commission needs to know the mean household usage of water by the residents of a small town in gallons per day. Assume that the population standard deviation is 1.3 gallons. The mean water usage per family was found to be 15 gallons per day for a sample of 499 families. Construct the 99% confidence interval for the mean usage of water. Round your answers to one decimal place.
# x_bar = sample mean
x_bar <- 15
# sigma = population standard deviation
sigma <- 1.3
# n = sample size
n <- 499
# z = critical value for 99% confidence
z <- 2.575
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to one decimal place
round(c(lower, upper), 1)

#######
# Consider the value of t such that 0.01 of the area under the curve is to the left of t.  
# Step 1 of 2 : Select the graph which best represents the given description of t.
# it was the one with the shaded in part at the very far left 
#########
# Consider the value of t such that 0.01 of the area under the curve is to the left of t. Assuming the degrees of freedom equals 14, select the t-value from the t-distribution table.
# p = area to the left of t
p <- 0.01
# df = degrees of freedom
df <- 14
# t = critical value from t-distribution
t <- qt(p, df)
# round to three decimal places
round(t, 3)

#########
# A hospital would like to determine the mean length of stay for its patients having abdominal surgery. A sample of 24 patients revealed a sample mean of 5.8 days and a sample standard deviation of 1.6 days. Assume that the lengths of stay are approximately normally distributed. Find a 90% confidence interval for the mean length of stay for patients with abdominal surgery. Round the endpoints to two decimal places, if necessary.
# x_bar = sample mean
x_bar <- 5.8
# s = sample standard deviation
s <- 1.6
# n = sample size
n <- 24
# df = degrees of freedom
df <- n - 1
# t = critical value for 90% confidence (two-tailed)
t <- qt(1 - 0.05, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# An independent group of food service personnel conducted a survey on tipping practices in a large metropolitan area. They collected information on the percentage of the bill left as a tip for 20 randomly selected bills. The average tip was 10.3% of the bill with a standard deviation of 2.2%. Assume that the tips are approximately normally distributed. Construct an interval to estimate the true average tip (as a percent of the bill) with 99% confidence. Round the endpoints to two decimal places, if necessary.
# x_bar = sample mean
x_bar <- 10.3
# s = sample standard deviation
s <- 2.2
# n = sample size
n <- 20
# df = degrees of freedom
df <- n - 1
# t = critical value for 99% confidence (two-tailed)
t <- qt(1 - 0.005, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# An FDA representative randomly selects 12 packages of ground chuck from a grocery store and measures the fat content (as a percent) of each package. Assume that the fat contents have an approximately normal distribution. The resulting measurements are given below. Step 1 of 2: Calculate the sample mean and the sample standard deviation of the fat contents. Round your answers to two decimal places, if necessary.
# data = fat content percentages
data <- c(12, 15, 17, 19, 14, 19, 12, 17, 15, 12, 12, 16)
# x_bar = sample mean
x_bar <- mean(data)
# s = sample standard deviation
s <- sd(data)
# round to two decimal places
round(c(x_bar, s), 2)
#########
# Step 2 of 2: Construct a 99% confidence interval for the true mean fat content of all the packages of ground beef. Round the endpoints to two decimal places, if necessary.
# data = fat content percentages
data <- c(12, 15, 17, 19, 14, 19, 12, 17, 15, 12, 12, 16)
# x_bar = sample mean
x_bar <- mean(data)
# s = sample standard deviation
s <- sd(data)
# n = sample size
n <- length(data)
# df = degrees of freedom
df <- n - 1
# t = critical value for 99% confidence (two-tailed)
t <- qt(1 - 0.005, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# A computer software company would like to estimate how long it will take a beginner to become proficient at creating a graph using their new spreadsheet package. Past experience has indicated that the time required for a beginner to become proficient with a particular function of the new software product has an approximately normal distribution with a standard deviation of 25 minutes. Find the sample size necessary to estimate the true average time required for a beginner to become proficient at creating a graph with the new spreadsheet package to within 6 minutes with 90% confidence.
# sigma = population standard deviation
sigma <- 25
# E = desired margin of error
E <- 6
# z = critical value for 90% confidence
z <- 1.645
# required sample size formula n = (z * sigma / E)^2
n <- (z * sigma / E)^2
# round up to next whole number
ceiling(n)

###########
# Considering the following scenario, which method would be most appropriate when calculating the margin of error for the population mean?
# A market surveyor wants to estimate the mean per capita income (in thousands) in a major city in Alaska.  He obtains information from 24 workers in the city and finds the mean to be 9.8.  The population is assumed to be normally distributed.
# Students T distribution

#########
# A travel agent is interested in the average price of a hotel room during the summer in a resort community. The agent randomly selects 16 hotels from the community and determines the price of a regular room with a king size bed. The average price of the room for the sample was $115 with a standard deviation of $35. Assume the prices are normally distributed. Construct an interval to estimate the true average price of a regular room with a king size bed in the resort community with 99% confidence. Round the endpoints to two decimal places, if necessary.
# x_bar = sample mean
x_bar <- 115
# s = sample standard deviation
s <- 35
# n = sample size
n <- 16
# df = degrees of freedom
df <- n - 1
# t = critical value for 99% confidence (two-tailed)
t <- qt(1 - 0.005, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# A technician working for the Chase-National Food Additive Company would like to estimate the preserving ability of a new additive. This additive will be used for Auntie’s brand preserves. Based on past tests, it is believed that the time to spoilage for this additive has a standard deviation of 5 days. To be 90% confident of the true mean time to spoilage, what sample size will be needed to estimate the mean time to spoilage with an accuracy of one day?
# sigma = population standard deviation
sigma <- 5
# E = desired margin of error (accuracy)
E <- 1
# z = critical value for 90% confidence
z <- 1.645
# required sample size formula n = (z * sigma / E)^2
n <- (z * sigma / E)^2
# round up to next whole number
ceiling(n)

#########
# Consider the value of t such that the area to the left of -|t| plus the area to the right of |t| equals 0.1. Assuming the degrees of freedom equals 22, select the t-value from the t-distribution table.
# alpha = total area in both tails
alpha <- 0.1
# df = degrees of freedom
df <- 22
# t = positive critical value for a two-tailed t distribution
t <- qt(1 - alpha / 2, df)
# round to three decimal places
round(t, 3)


########
# Considering the following scenario, which method would be most appropriate when calculating the margin of error for the population mean?
# A research scientist wants to know how many times per hour a certain strand of virus reproduces.  She obtains information from 78 strands of this particular virus and finds the mean to be 8.7 with a population standard deviation known to be 2.2.  The population is assumed to be skewed to the right.
# Normal Z distribution

#########
# Consider the value of t such that the area under the curve between -|t| and |t| equals 0.98. Assuming the degrees of freedom equals 16, select the t-value from the t-distribution table.
# middle_area = area between -t and t
middle_area <- 0.98
# alpha = total area in both tails
alpha <- 1 - middle_area
# df = degrees of freedom
df <- 16
# t = positive critical value for a two-tailed t distribution
t <- qt(1 - alpha / 2, df)
# round to three decimal places
round(t, 3)

#########
# An FDA representative randomly selects 12 packages of ground chuck from a grocery store and measures the fat content (as a percent) of each package. Assume that the fat contents have an approximately normal distribution. The resulting measurements are given below. Step 1 of 2: Calculate the sample mean and the sample standard deviation of the fat contents. Round your answers to two decimal places, if necessary.
# data = fat content percentages
data <- c(15, 14, 14, 17, 18, 11, 18, 17, 16, 14, 14, 12)
# x_bar = sample mean
x_bar <- mean(data)
# s = sample standard deviation
s <- sd(data)
# round to two decimal places
round(c(x_bar, s), 2)

#########
# An FDA representative randomly selects 12 packages of ground chuck from a grocery store and measures the fat content (as a percent) of each package. Assume that the fat contents have an approximately normal distribution. The resulting measurements are given below. Step 2 of 2: Construct a 99% confidence interval for the true mean fat content of all the packages of ground beef. Round the endpoints to two decimal places, if necessary.
# data = fat content percentages
data <- c(15, 14, 14, 17, 18, 11, 18, 17, 16, 14, 14, 12)
# x_bar = sample mean
x_bar <- mean(data)
# s = sample standard deviation
s <- sd(data)
# n = sample size
n <- length(data)
# df = degrees of freedom
df <- n - 1
# t = critical value for 99% confidence (two-tailed)
t <- qt(1 - 0.005, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# Consider the value of t such that 0.005 of the area under the curve is to the left of t. Assuming the degrees of freedom equals 10, select the t-value from the t-distribution table.
# p = area to the left of t
p <- 0.005
# df = degrees of freedom
df <- 10
# t = critical value from t-distribution
t <- qt(p, df)
# round to three decimal places
round(t, 3)

#########
# Consider the value of t such that the area to the left of -|t| plus the area to the right of |t| equals 0.1. Assuming the degrees of freedom equals 30, select the t-value from the t-distribution table.
# alpha = total area in both tails
alpha <- 0.1
# df = degrees of freedom
df <- 30
# t = positive critical value for a two-tailed t distribution
t <- qt(1 - alpha / 2, df)
# round to three decimal places
round(t, 3)

#########
# A computer software company would like to estimate how long it will take a beginner to become proficient at creating a graph using their new spreadsheet package. Past experience has indicated that the time required for a beginner to become proficient with a particular function of the new software product has an approximately normal distribution with a standard deviation of 20 minutes. Find the sample size necessary to estimate the true average time required for a beginner to become proficient at creating a graph with the new spreadsheet package to within 5 minutes with 90% confidence.
# sigma = population standard deviation
sigma <- 20
# E = desired margin of error
E <- 5
# z = critical value for 90% confidence
z <- 1.645
# required sample size formula n = (z * sigma / E)^2
n <- (z * sigma / E)^2
# round up to next whole number
ceiling(n)

#########
# An independent group of food service personnel conducted a survey on tipping practices in a large metropolitan area. They collected information on the percentage of the bill left as a tip for 40 randomly selected bills. The average tip was 11.7% of the bill with a standard deviation of 2.1%. Assume that the tips are approximately normally distributed. Construct an interval to estimate the true average tip (as a percent of the bill) with 95% confidence. Round the endpoints to two decimal places, if necessary.
# x_bar = sample mean
x_bar <- 11.7
# s = sample standard deviation
s <- 2.1
# n = sample size
n <- 40
# df = degrees of freedom
df <- n - 1
# t = critical value for 95% confidence (two-tailed)
t <- qt(1 - 0.025, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# A hospital would like to determine the mean length of stay for its patients having abdominal surgery. A sample of 20 patients revealed a sample mean of 5.4 days and a sample standard deviation of 1.5 days. Assume that the lengths of stay are approximately normally distributed. Find a 90% confidence interval for the mean length of stay for patients with abdominal surgery. Round the endpoints to two decimal places, if necessary.
# x_bar = sample mean
x_bar <- 5.4
# s = sample standard deviation
s <- 1.5
# n = sample size
n <- 20
# df = degrees of freedom
df <- n - 1
# t = critical value for 90% confidence (two-tailed)
t <- qt(1 - 0.05, df)
# standard error
se <- s / sqrt(n)
# margin of error
E <- t * se
# lower and upper bounds
lower <- x_bar - E
upper <- x_bar + E
# round to two decimal places
round(c(lower, upper), 2)

#########
# A technician working for the Chase-National Food Additive Company would like to estimate the preserving ability of a new additive. This additive will be used for Auntie’s brand preserves. Based on past tests, it is believed that the time to spoilage for this additive has a standard deviation of 10 days. To be 98% confident of the true mean time to spoilage, what sample size will be needed to estimate the mean time to spoilage with an accuracy of one day?
# sigma = population standard deviation
sigma <- 10
# E = desired margin of error (accuracy)
E <- 1
# z = critical value for 98% confidence
z <- 2.33
# required sample size formula n = (z * sigma / E)^2
n <- (z * sigma / E)^2
# round up to next whole number
ceiling(n)

#########
# The FBI wants to determine the effectiveness of their 10 Most Wanted list. To do so, they need to find out the fraction of people who appear on the list that are actually caught. In an earlier study, the population proportion was estimated to be 0.27. How large a sample would be required in order to estimate the fraction of people who are captured after appearing on the 10 Most Wanted list at the 99% confidence level with an error of at most 0.04? Round your answer up to the next integer.
# p_hat = estimated population proportion
p_hat <- 0.27
# q_hat = 1 - p_hat
q_hat <- 1 - p_hat
# E = desired margin of error
E <- 0.04
# z = critical value for 99% confidence
z <- 2.575
# required sample size formula n = (z^2 * p_hat * q_hat) / E^2
n <- (z^2 * p_hat * q_hat) / (E^2)
# round up to next whole number
ceiling(n)

#########
# The state education commission wants to estimate the fraction of tenth grade students that have reading skills at or below the eighth grade level. In an earlier study, the population proportion was estimated to be 0.22. How large a sample would be required in order to estimate the fraction of tenth graders reading at or below the eighth grade level at the 99% confidence level with an error of at most 0.03? Round your answer up to the next integer.
# p_hat = estimated population proportion
p_hat <- 0.22
# q_hat = 1 - p_hat
q_hat <- 1 - p_hat
# E = desired margin of error
E <- 0.03
# z = critical value for 99% confidence
z <- 2.575
# required sample size formula n = (z^2 * p_hat * q_hat) / E^2
n <- (z^2 * p_hat * q_hat) / (E^2)
# round up to next whole number
ceiling(n)

#########
# Acid rain accumulations in lakes and streams in the northeastern part of the United States are a major environmental concern. A researcher wants to know what fraction of lakes contain hazardous pollution levels. From 400 randomly selected lakes, it is determined that 75 of them have an unsafe concentration of acid rain pollution. Step 1 of 2 : Determine a 98% confidence interval for the population proportion of lakes that have unsafe concentrations of acid rain pollution. Round any intermediate calculations to no less than six decimal places and round the endpoints of the interval to four decimal places, if necessary.
# x = number of lakes with unsafe pollution levels
x <- 75
# n = sample size
n <- 400
# p_hat = sample proportion
p_hat <- x / n
# z = critical value for 98% confidence
z <- 2.33
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round endpoints to four decimal places
round(c(lower, upper), 4)

#########
# NASA is conducting an experiment to find out the fraction of people who black out at G forces greater than 6. Suppose a sample of 409 people is drawn. Of these people, 139 passed out at G forces greater than 6. Using the data, estimate the proportion of people who pass out at more than 6 Gs. Enter your answer as a fraction or a decimal number rounded to three decimal places.
# x = number of people who passed out
x <- 139
# n = sample size
n <- 409
# p_hat = sample proportion
p_hat <- x / n
# round to three decimal places
round(p_hat, 3)

#########
# NASA is conducting an experiment to find out the fraction of people who black out at G forces greater than 6. Step 2 of 2 : Suppose a sample of 409 people is drawn. Of these people, 139 passed out. Using the data, construct the 80% confidence interval for the population proportion of people who black out at G forces greater than 6. Round your answers to three decimal places.
# x = number of people who passed out
x <- 139
# n = sample size
n <- 409
# p_hat = sample proportion
p_hat <- x / n
# z = critical value for 80% confidence
z <- 1.28
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round to three decimal places
round(c(lower, upper), 3)

##########
# NASA experiment: n=323, 194 did not pass out → 129 passed out; 85% CI for population proportion

n <- 323
x <- 323 - 194
phat <- x / n
z <- qnorm(1 - 0.15/2)
SE <- sqrt(phat * (1 - phat) / n)
lower <- phat - z * SE
upper <- phat + z * SE
round(c(lower, upper), 3)

#########
# The clinical testing of drugs involves many factors. For example, patients that have been given placebos, which are harmless compounds that have no effect on the patient, often will still report that they feel better. Assume that in a study of 300 random subjects conducted by the Poppins Sucre Drug Company, the percentage of patients reporting improvement when given a placebo was 42%. Step 1 of 3 : Determine a 98% confidence interval for the true proportion of patients who exhibit the placebo effect. Round any intermediate calculations to no less than six decimal places and round the endpoints of the interval to four decimal places, if necessary.
# n = sample size
n <- 300
# p_hat = sample proportion
p_hat <- 0.42
# z = critical value for 98% confidence
z <- 2.33
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round endpoints to four decimal places
round(c(lower, upper), 4)

#########
# The clinical testing of drugs involves many factors. For example, patients that have been given placebos, which are harmless compounds that have no effect on the patient, often will still report that they feel better. Assume that in a study of 300 random subjects conducted by the Poppins Sucre Drug Company, the percentage of patients reporting improvement when given a placebo was 42%. Step 2 of 3 : Determine a 95% confidence interval for the true proportion of patients who exhibit the placebo effect. Round any intermediate calculations to no less than six decimal places and round the endpoints of the interval to four decimal places, if necessary.
# n = sample size
n <- 300
# p_hat = sample proportion
p_hat <- 0.42
# z = critical value for 95% confidence
z <- 1.96
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round endpoints to four decimal places
round(c(lower, upper), 4)

#########
# Out of 199 randomly selected adults in the United States who were surveyed, 88 exercise on a regular basis. Construct a 90% confidence interval for the proportion of all adults in the United States who exercise on a regular basis. Round to three decimal places.
# x = number who exercise regularly
x <- 825
# n = sample size
n <- 199
# p_hat = sample proportion
p_hat <- x / n
# z = critical value for 90% confidence
z <- 1.645
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round to three decimal places
round(c(lower, upper), 3)

#########
# The state education commission wants to estimate the fraction of tenth grade students that have reading skills at or below the eighth grade level. Suppose a sample of 2552 tenth graders is drawn. Of the students sampled, 2119 read above the eighth grade level. Using the data, estimate the proportion of tenth graders reading at or below the eighth grade level. Enter your answer as a fraction or a decimal number rounded to three decimal places.
# n = total number of students
n <- 2552
# above = number reading above eighth grade level
above <- 2119
# x = number reading at or below eighth grade level
x <- n - above
# p_hat = sample proportion
p_hat <- x / n
# round to three decimal places
round(p_hat, 3)

#########
# The state education commission wants to estimate the fraction of tenth grade students that have reading skills at or below the eighth grade level. Step 2 of 2 : Suppose a sample of 2552 tenth graders is drawn. Of the students sampled, 2119 read above the eighth grade level. Using the data, construct the 99% confidence interval for the population proportion of tenth graders reading at or below the eighth grade level. Round your answers to three decimal places.
# n = total number of students
n <- 2552
# above = number reading above eighth grade level
above <- 2119
# x = number reading at or below eighth grade level
x <- n - above
# p_hat = sample proportion
p_hat <- x / n
# z = critical value for 99% confidence
z <- 2.575
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# lower and upper bounds
lower <- p_hat - E
upper <- p_hat + E
# round to three decimal places
round(c(lower, upper), 3)

#################
# Construct the confidence interval for the population standard deviation for the given values. Round your answers to one decimal place. n=8, s=1.9, and c=0.9
# n = sample size
n <- 8
# s = sample standard deviation
s <- 1.9
# c = confidence level
c <- 0.9

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for sigma
lower <- sqrt((df * s^2) / chi_left)
upper <- sqrt((df * s^2) / chi_right)

# round to one decimal place
round(c(lower, upper), 1)

##########
# A conservative investor would like to invest some money in a bond fund. The investor is concerned about the safety of her principal (the original money invested). Colonial Funds claims to have a bond fund which has maintained a consistent share price of $17. They claim that this share price has not varied by more than $0.6 on average since its inception. To test this claim, the investor randomly selects 29 days during the last year and determines the share price for the bond fund. The average share price of the sample is $16 with a standard deviation of $0.55. Assuming that the share prices of the bond fund have an approximately normal distribution, construct a 98% confidence interval for the standard deviation of the share price of the bond fund. Round any intermediate calculations to no less than six decimal places and round the endpoints of the interval to four decimal places.

# n = sample size
n <- 29
# s = sample standard deviation
s <- 0.55
# c = confidence level
c <- 0.98

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for sigma
lower <- sqrt((df * s^2) / chi_left)
upper <- sqrt((df * s^2) / chi_right)

# round to four decimal places
round(c(lower, upper), 4)

#########
# The thicknesses of 13 randomly selected linoleum tiles were found to have a standard deviation of 4.76. Construct the 90% confidence interval for the population standard deviation of the thicknesses of all linoleum tiles in this factory. Round your answers to two decimal places.

# n = sample size
n <- 13
# s = sample standard deviation
s <- 4.76
# c = confidence level
c <- 0.90

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for sigma
lower <- sqrt((df * s^2) / chi_left)
upper <- sqrt((df * s^2) / chi_right)

# round to two decimal places
round(c(lower, upper), 2)

#########
# A frozen food company uses a machine that packages green beans in twelve ounce portions. A sample of 56 packages of green beans has a standard deviation of 0.15. Construct the 80% confidence interval to estimate the standard deviation of the weights of the packages prepared by the machine. Round your answers to two decimal places.

# n = sample size
n <- 56
# s = sample standard deviation
s <- 0.15
# c = confidence level
c <- 0.80

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for sigma
lower <- sqrt((df * s^2) / chi_left)
upper <- sqrt((df * s^2) / chi_right)

# round to two decimal places
round(c(lower, upper), 2)

#########
# Determine the critical values for the confidence interval for the population variance from the given values. Round your answers to three decimal places. n=14 and α=0.01.

# n = sample size
n <- 14
# alpha = significance level
alpha <- 0.01

# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(alpha/2, df)
chi_right <- qchisq(1 - alpha/2, df)

# round to three decimal places
round(c(chi_left, chi_right), 3)

######
# The following sample of weights was taken from 9 bags of chips off the assembly line. Construct the 90% confidence interval for the population variance for all bags of chips that come off the assembly line. Round your answers to two decimal places. 3.9,4.0,3.9,4.5,3.6,4.1,4.5,3.6,3.7

# data = sample values
x <- c(3.9,4.0,3.9,4.5,3.6,4.1,4.5,3.6,3.7)

# n = sample size
n <- length(x)
# s2 = sample variance
s2 <- var(x)
# c = confidence level
c <- 0.90

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for variance
lower <- (df * s2) / chi_left
upper <- (df * s2) / chi_right

# round to two decimal places
round(c(lower, upper), 2)

###########
# A manufacturer of automobile batteries is concerned about the life of the batteries that are produced. The manufacturer is comfortable with the average life of the batteries but more concerned about the standard deviation. Research has shown that the average life of the automobile batteries is 50 months. However, the manufacturer would like the standard deviation of the life of the automobile batteries to be relatively small, say, approximately seven months. To determine a reliable range of the standard deviation of the batteries currently being produced, the manufacturer took a random sample of 16 batteries and found that the average life was 48 months with a standard deviation of nine months. Assuming that the life of batteries produced by the automobile manufacturer has an approximately normal distribution, construct a 95% confidence interval for the standard deviation of the life of their automobile batteries. Round any intermediate calculations to no less than six decimal places and round the endpoints of the interval to four decimal places.

# n = sample size
n <- 16
# s = sample standard deviation
s <- 9
# c = confidence level
c <- 0.95

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for sigma
lower <- sqrt((df * s^2) / chi_left)
upper <- sqrt((df * s^2) / chi_right)

# round to four decimal places
round(c(lower, upper), 4)

#######
# Construct the confidence interval for the population variance for the given values. Round your answers to one decimal place. n=18, s2=46.9, and c=0.95

# n = sample size
n <- 18
# s2 = sample variance
s2 <- 46.9
# c = confidence level
c <- 0.95

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for variance
lower <- (df * s2) / chi_left
upper <- (df * s2) / chi_right

# round to one decimal place
round(c(lower, upper), 1)

##########
# An FDA representative randomly selects 10 packages of ground chuck from a grocery store and measures the fat content (as a percent) of each package. Assume that the fat contents have an approximately normal distribution. The resulting measurements are given below. Fat Contents (%) 12,15,15,15,11,11,15,12,19,15 Step 1 of 2 : Calculate the sample mean and the sample standard deviation of the fat contents. Round your answers to two decimal places, if necessary.

# data = fat contents
x <- c(12,15,15,15,11,11,15,12,19,15)

# sample mean
mean_x <- mean(x)

# sample standard deviation
sd_x <- sd(x)

# round to two decimal places
round(c(mean_x, sd_x), 2)

########
# An FDA representative randomly selects 10 packages of ground chuck from a grocery store and measures the fat content (as a percent) of each package. Assume that the fat contents have an approximately normal distribution. The resulting measurements are given below. Fat Contents (%) 12,15,15,15,11,11,15,12,19,15 Step 2 of 2 : Construct a 95% confidence interval for the true mean fat content of all the packages of ground beef. Round the endpoints to two decimal places, if necessary.

# data = fat contents
x <- c(12,15,15,15,11,11,15,12,19,15)

# n = sample size
n <- length(x)
# sample mean
mean_x <- mean(x)
# sample standard deviation
s <- sd(x)

# confidence level
c <- 0.95
# alpha
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# t critical value
t_star <- qt(1 - alpha/2, df)

# standard error
se <- s / sqrt(n)

# margin of error
E <- t_star * se

# confidence interval
lower <- mean_x - E
upper <- mean_x + E

# round to two decimal places
round(c(lower, upper), 2)

##########
# A hospital would like to determine the mean length of stay for its patients having abdominal surgery. A sample of 25 patients revealed a sample mean of 5.2 days and a sample standard deviation of 1.6 days. Assume that the lengths of stay are approximately normally distributed. Find a 95% confidence interval for the mean length of stay for patients with abdominal surgery. Round the endpoints to two decimal places, if necessary.

# n = sample size
n <- 25
# sample mean
mean_x <- 5.2
# sample standard deviation
s <- 1.6
# confidence level
c <- 0.95

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# t critical value
t_star <- qt(1 - alpha/2, df)

# standard error
se <- s / sqrt(n)

# margin of error
E <- t_star * se

# confidence interval
lower <- mean_x - E
upper <- mean_x + E

# round to two decimal places
round(c(lower, upper), 2)

##########
# The thicknesses of 85 randomly selected pencil leads were found to have a variance of 2.86. Construct the 98% confidence interval for the population variance of the thicknesses of all pencil leads in this factory. Round your answers to two decimal places.

# n = sample size
n <- 85
# s2 = sample variance
s2 <- 2.86
# c = confidence level
c <- 0.98

# alpha = significance level
alpha <- 1 - c
# degrees of freedom
df <- n - 1

# chi-square critical values
chi_left <- qchisq(1 - alpha/2, df)
chi_right <- qchisq(alpha/2, df)

# lower and upper bounds for variance
lower <- (df * s2) / chi_left
upper <- (df * s2) / chi_right

# round to two decimal places
round(c(lower, upper), 2)

########
# The FBI wants to determine the effectiveness of their 10 Most Wanted list. Step 1 of 2 : Suppose a sample of 359 suspected criminals is drawn. Of these people, 205 were not captured. Using the data, estimate the proportion of people who were caught after being on the 10 Most Wanted list. Enter your answer as a fraction or a decimal number rounded to three decimal places.

# n = total number of people
n <- 359
# not_captured = number not captured
not_captured <- 205
# x = number captured
x <- n - not_captured

# sample proportion
p_hat <- x / n

# round to three decimal places
round(p_hat, 3)

########
# The FBI wants to determine the effectiveness of their 10 Most Wanted list. Step 2 of 2 : Suppose a sample of 359 suspected criminals is drawn. Of these people, 205 were not captured. Using the data, construct the 95% confidence interval for the population proportion of people who are captured after appearing on the 10 Most Wanted list. Round your answers to three decimal places.

# n = total number of people
n <- 359
# not_captured = number not captured
not_captured <- 205
# x = number captured
x <- n - not_captured

# sample proportion
p_hat <- x / n

# z = critical value for 95% confidence
z <- 1.96

# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)

# margin of error
E <- z * se

# confidence interval
lower <- p_hat - E
upper <- p_hat + E

# round to three decimal places
round(c(lower, upper), 3)

######
# The state education commission wants to estimate the fraction of tenth grade students that have reading skills at or below the eighth grade level. In an earlier study, the population proportion was estimated to be 0.18. How large a sample would be required in order to estimate the fraction of tenth graders reading at or below the eighth grade level at the 98% confidence level with an error of at most 0.02? Round your answer up to the next integer.
# p = estimated proportion
p <- 0.18
# q = 1 - p
q <- 1 - p
# E = margin of error
E <- 0.02
# z = critical value for 98% confidence
z <- 2.326
# sample size formula
n <- (z^2 * p * q) / (E^2)
# round up to next integer
ceiling(n)

######
# The mean monthly car payment for 118 residents of the local apartment complex is $603. What is the best point estimate for the mean monthly car payment for all residents of the local apartment complex?
# sample mean (point estimate for population mean)
mean_x <- 603
# answer
mean_x

######
# NASA is conducting an experiment to find out the fraction of people who black out at G forces greater than 6. Step 1 of 2 : Suppose a sample of 581 people is drawn. Of these people, 197 passed out at G forces greater than 6. Using the data, estimate the proportion of people who pass out at more than 6 Gs. Enter your answer as a fraction or a decimal number rounded to three decimal places.
# n = total number of people
n <- 581
# x = number who passed out
x <- 197
# sample proportion
p_hat <- x / n
# round to three decimal places
round(p_hat, 3)

######
# NASA is conducting an experiment to find out the fraction of people who black out at G forces greater than 6. Step 2 of 2 : Suppose a sample of 581 people is drawn. Of these people, 197 passed out. Using the data, construct the 95% confidence interval for the population proportion of people who black out at G forces greater than 6. Round your answers to three decimal places.
# n = total number of people
n <- 581
# x = number who passed out
x <- 197
# sample proportion
p_hat <- x / n
# z = critical value for 95% confidence
z <- 1.96
# standard error
se <- sqrt(p_hat * (1 - p_hat) / n)
# margin of error
E <- z * se
# confidence interval
lower <- p_hat - E
upper <- p_hat + E
# round to three decimal places
round(c(lower, upper), 3)

######
# Determine the critical values for the confidence interval for the population variance from the given values. Round your answers to three decimal places. n=21 and α=0.02.
# n = sample size
n <- 21
# alpha = significance level
alpha <- 0.02
# degrees of freedom
df <- n - 1
# chi-square critical values
chi_left <- qchisq(alpha/2, df)
chi_right <- qchisq(1 - alpha/2, df)
# round to three decimal places
round(c(chi_left, chi_right), 3)

######
# An educational psychologist wishes to know the mean number of words a third grader can read per minute. She wants to make an estimate at the 80% level of confidence. For a sample of 870 third graders, the mean words per minute read was 37.6. Assume a population standard deviation of 5.5. Construct the confidence interval for the mean number of words a third grader can read per minute. Round your answers to one decimal place.
# n = sample size
n <- 870
# sample mean
mean_x <- 37.6
# sigma = population standard deviation
sigma <- 5.5
# c = confidence level
c <- 0.80
# alpha = significance level
alpha <- 1 - c
# z = critical value for 80% confidence
z <- qnorm(1 - alpha/2)
# standard error
se <- sigma / sqrt(n)
# margin of error
E <- z * se
# confidence interval
lower <- mean_x - E
upper <- mean_x + E
# round to one decimal place
round(c(lower, upper), 1)

######
# A technician working for the Chase-National Food Additive Company would like to estimate the preserving ability of a new additive. This additive will be used for Auntie’s brand preserves. Based on past tests, it is believed that the time to spoilage for this additive has a standard deviation of 5 days. To be 99% confident of the true mean time to spoilage, what sample size will be needed to estimate the mean time to spoilage with an accuracy of one day?
# sigma = population standard deviation
sigma <- 5
# E = margin of error
E <- 1
# z = critical value for 99% confidence
z <- 2.576
# sample size formula
n <- (z^2 * sigma^2) / (E^2)
# round up to next integer
ceiling(n)

############# Chapter 5
# Correlation Template ( cor temp )
x<-c()
y<-c()
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

# A manufacturing company which produces laminate for countertops is interested in studying the relationship between the number of hours of training an employee receives and the number of defects per countertop produced. Ten employees are randomly selected. The number of hours of training which each employee has received is recorded and the number of defects on the most recent countertop produced is determined. The results are as follows.
# Hours of training (x)
x <- c(37, 53, 61, 65, 69)

# Defects per countertop (y)
y <- c(355, 350, 340, 325, 320)

# Correlation coefficient
r <-cor(x,y)
# Print result
round(r,3)

########
# The following data gives the number of hours 10 students spent studying and their corresponding grades on their midterm exams.
#Hours spent studying
x<-c(0, 1.5, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6)
# Midterm grades
y<-c(63,	66,	69,	78,	81,	84,	87,	90,	93,	99)

# relation
r<-cor(x,y)
round(r, 3)

########
# The following table gives the average number of hours 7 junior high students were left unsupervised each day and their corresponding overall grade averages.

x<-c(0,0.5,1,2.5,3.5,4.5,5)
y<-c(96,92,88,80,76,72,68)
r<-cor(x,y)
round(r, 3) # rounds to 3s place

#######
# A personnel director is interested in studying the relationship (if any) between age and salary. Sixteen employees are randomly selected, and their ages and salaries are recorded. 
# age
x<-c(55,28,21,36,30,39,46,59,56,59,25,20,25,32,44,53)
# salary
y<-c(40000,60000,22000,21000,55000,57000,28000,35000,44000,24000,31000,19000,44000,22000,47000,35000)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

########
# The following table compares the completion percentage and interception percentage of 5 NFL quarterbacks.
x<-c(55,56,57.5,59.5,60)
y<-c(4.5,4,2.5,2,1.5)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

######
# The following data gives the number of hours 10 students spent studying and their corresponding grades on their midterm exams.
x<-c(0,	1,	1.5,	2,	3,	3.5,	4,	4.5,	5.5,	6) # hours spent
y<-c(60,	63,	69,	72,	75,	78,	81,	84,	87,	90) # midterm grades 
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

########
# Determine if the correlation between the two given variables is likely to be positive or negative, or if they are not likely to display a linear relationship.
# A child's age and the number of hours spent napping
# Negative

#######
# Determine if the correlation between the two given variables is likely to be positive or negative, or if they are not likely to display a linear relationship.
# The number of hours you spend studying for a test and your grade on the test
# 

#####
# Consider the relationship between the number of bids an item on eBay received and the item's selling price. The following is a sample of 5 items sold through an auction.
x<-c(140,	150,	160,	180,	190)
y<-c(10,	11,	16,	18,	20)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

##########
# Consider the relationship between the number of bids an item on eBay received and the item's selling price. The following is a sample of 5 items sold through an auction.
x<-c(23,29,38,41,47)
y<-c(10,13,16,17,19)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)


#######
# A study of bone density on 5 random women at a hospital produced the following results.
# draw a scatter plot
x<-c(33,45,57,61,65)
y<-c(360,355,345,335,315)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

#######
# A manufacturing company which produces laminate for countertops is interested in studying the relationship between the number of hours of training an employee receives and the number of defects per countertop produced. Ten employees are randomly selected. The number of hours of training which each employee has received is recorded and the number of defects on the most recent countertop produced is determined. The results are as follows.
# Determine the correlation coefficient. Round your answer to three decimal places.
x<-c(5,8,9,8,1,9,2,4,6,3)
y<-c(6,2,1,3,7,2,7,6,6,4)
r<-cor(x,y)
round(r, 3) # rounds to 3s place
plot(x,y)

#######
# A statistics professor would like to build a model relating student scores on the first test to the scores on the second test. The test scores from a random sample of 21 students who have previously taken the course are given in the table.
# Using statistical software, estimate the parameters of the model
# Second Test Grade=β0+β1(First Test Grade)+εi.
# Enter a negative estimate as a negative number in the regression model. Round your answers to 4 decimal places, if necessary.

# First Test and Second Test scores
x <- c(67,97,42,69,56,66,99,79,88,63,61,83,89,43,85,40,47,93,93,56,50)
y <- c(70,81,61,72,63,72,90,73,78,67,65,77,85,57,76,64,66,79,80,67,67)

# Fit linear regression model
model <- lm(y ~ x)

# Extract coefficients
round(coef(model), 4)

######
# A statistics professor would like to build a model relating student scores on the first test to the scores on the second test. The test scores from a random sample of 21 students who have previously taken the course are given in the table.
# Interpret the coefficient of the first test grade in the model.

# For each additional one unit increase in the first test grade, the second test grade is expected to increase by approximately 0.4024.

#######
# The following table shows students’ test scores on the first two tests in an introductory biology class.
#  Step 1 of 2 : Find an equation of the least-squares regression line. Round your answer to three decimal places, if necessary.
# First test (x) and second test (y)
x <- c(44,93,70,88,81,51,76,58,42,81,67,85)
y <- c(52,100,69,89,79,62,80,59,50,88,69,86)

# Fit regression model
model <- lm(y ~ x)

# Coefficients rounded to 3 decimals
round(coef(model), 3)

#######
# The following table shows students’ test scores on the first two tests in an introductory biology class.
#  Step 2 of 2 : If a student scored a 43 on his first test, make a prediction for his score on the second test. Assume the regression equation is appropriate for prediction. Round your answer to two decimal places, if necessary.
# Data
x <- c(44,93,70,88,81,51,76,58,42,81,67,85)
y <- c(52,100,69,89,79,62,80,59,50,88,69,86)

# Fit model
model <- lm(y ~ x)

# Predict when x = 43
round(predict(model, newdata = data.frame(x = 43)), 2)

#######
# Suppose that a company wishes to predict sales volume based on the amount of advertising expenditures. The sales manager thinks that sales volume and advertising expenditures are modeled according to the following linear equation. Both sales volume and advertising expenditures are in thousands of dollars.
# Estimated Sales Volume=46.79+0.49(Advertising Expenditures)
# If the company has a target sales volume of $125,000, how much should the sales manager allocate for advertising in the budget? Round your answer to the nearest dollar.
# Target sales (in thousands)
y <- 125

# Solve for advertising expenditures (in thousands)
x <- (y - 46.79) / 0.49

# Convert to dollars and round
round(x * 1000)

#######
# Suppose the following estimated regression equation was determined to predict salary based on years of experience.
# Estimated Salary=23,646.98+2721.60(Years of Experience)
# What is the estimated salary for an employee with 29 years of experience? 
# Years of experience
x <- 29

# Estimated salary (no rounding)
salary <- 23646.98 + 2721.60 * x

salary

########
# Suppose the following estimated regression equation was determined to predict salary based on years of experience.
# Estimated Salary=29,608.40+2632.98(Years of Experience)
# What is the estimated salary for an employee with 18 years of experience? 
# Estimated Salary = 29,608.40 + 2632.98(Years of Experience)

x <- 18
salary <- 29608.40 + 2632.98 * x

salary

######
# Suppose we want to use data on square footage to predict home sale prices. An equation that fits the data below reasonably well is
# Predicted Selling Price=15.99+0.12(Square Footage).
# In order to find the sum of squared errors for this data, we have constructed the following table.
# Given model: y_hat = 15.99 + 0.12 * x
x <- 1219
y_obs <- 183.8
# predicted value
y_hat <- 15.99 + 0.12 * x
# error
error <- y_obs - y_hat
# squared error
sq_error <- error^2
round(c(y_hat, error, sq_error), 2)

########
# A statistics professor would like to build a model relating student scores on the first test to the scores on the second test. The test scores from a random sample of 21 students who have previously taken the course are given in the table.
# Step 1 of 2 : Using statistical software, estimate the parameters of the model
# Second Test Grade=β0+β1(First Test Grade)+εi.
# Enter a negative estimate as a negative number in the regression model. Round your answers to 4 decimal places, if necessary.
# First Test and Second Test scores
x <- c(67,97,42,69,56,66,99,79,88,63,61,83,89,43,85,40,47,93,93,56,50)
y <- c(70,81,61,72,63,72,90,73,78,67,65,77,85,57,76,64,66,79,80,67,67)

# Fit linear regression model
model <- lm(y ~ x)

# Extract coefficients (rounded to 4 decimals)
round(coef(model), 4)
# step 2

#######
# The table below is a record of the number of miles driven between stops for gas and the amount of money spent to fill up the gas tank in Emily’s car over several months.
#  Step 1 of 2 : Find an equation of the least-squares regression line. Round your answer to three decimal places, if necessary.
# Miles driven (x) and cost to fill up (y)
x <- c(279,331,302,286,316,313,344)
y <- c(23.09,27.59,24.52,23.21,25.21,26.19,27.49)
# Fit least-squares regression line
model <- lm(y ~ x)
# Get intercept and slope rounded to 3 decimals
b0 <- round(coef(model)[1], 3)
b1 <- round(coef(model)[2], 3)
b0
b1

#######
# The table below is a record of the number of miles driven between stops for gas and the amount of money spent to fill up the gas tank in Emily’s car over several months.
# Emily’s Gas Fill Up Log Miles Driven, x 	279 	331 	302 	286 	316 	313 	344
# Cost to Fill up ($), y 	23.09 	27.59 	24.52 	23.21 	25.21 	26.19 	27.49
# Step 2 of 2 : If Emily needs to drive 319 miles home from college and leaves with a full tank, how much should she budget to fill up when she gets home? Assume the regression equation is appropriate for prediction. Round your answer to the nearest cent.
# Data
x <- c(279,331,302,286,316,313,344)
y <- c(23.09,27.59,24.52,23.21,25.21,26.19,27.49)

# Fit model
model <- lm(y ~ x)

# Predict cost for 319 miles
round(predict(model, newdata = data.frame(x = 319)), 2)

#######
# Suppose that a company wishes to predict sales volume based on the amount of advertising expenditures. The sales manager thinks that sales volume and advertising expenditures are modeled according to the following linear equation. Both sales volume and advertising expenditures are in thousands of dollars.
# Estimated Sales Volume=46.41+0.45(Advertising Expenditures)
# If the company has a target sales volume of $200,000, how much should the sales manager allocate for advertising in the budget? Round your answer to the nearest dollar.
# Target sales (in thousands)
y <- 200
# Solve for advertising (in thousands)
x <- (y - 46.41) / 0.45
# Convert to dollars and round
round(x * 1000)

#######
# The table below displays the latitude and average high temperature in March for nine randomly selected U.S. cities. Using this data, consider the equation of the regression line, yˆ=b0+b1x for predicting an average high temperature in March from the latitude of the city.
#  Step 1 of 3 : Write the estimated regression equation using the least squares estimates for b0 and b1. Round your answer to four decimal places, if necessary.
# Use the summation values given in the problem's solution table
n <- 9
sum_x <- 333.3
sum_y <- 540.6
sum_xy <- 19983.5
sum_x2 <- 12511.17

# Least-squares estimates
b1 <- (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x^2)
b0 <- (sum_y - b1 * sum_x) / n

round(b0, 4)
round(b1, 4)

######
# The table below displays the latitude and average high temperature in March for nine randomly selected U.S. cities. Using this data, consider the equation of the regression line, yˆ=b0+b1x for predicting an average high temperature in March from the latitude of the city.
#  Step 2 of 3 : What is the coefficient of determination for the model? Round your answer to four decimal places, if necessary.
# Latitude (x) and Temperature (y)
x <- c(33.6,36.1,35.9,34.6,35.4,41.9,43.6,42.3,29.9)
y <- c(66.4,72.7,74.8,46.0,42.7,47.9,60.5,66.1,63.5)
# Fit model
model <- lm(y ~ x)
# Coefficient of determination (R^2)
round(summary(model)$r.squared, 4)

#  Step 3 of 3 : What percent of the variation in average high temperature in March is explained by the latitude? Round your answer to two decimal places, if necessary.
# Latitude (x) and Temperature (y)
x <- c(33.6,36.1,35.9,34.6,35.4,41.9,43.6,42.3,29.9)
y <- c(66.4,72.7,74.8,46.0,42.7,47.9,60.5,66.1,63.5)
# Fit model
model <- lm(y ~ x)
# Percent of variation explained
round(summary(model)$r.squared * 100, 2)

#########
# The table below gives the list price and the number of bids received for five randomly selected items sold through online auctions. Using this data, consider the equation of the regression line, yˆ=b0+b1x, for predicting the number of bids an item will receive based on the list price. Keep in mind, the correlation coefficient may or may not be statistically significant for the data given. Remember, in practice, it would not be appropriate to use the regression line to make a prediction if the correlation coefficient is not statistically significant.
# Price in Dollars	20	33	38	44	50
# Number of Bids	3	5	7	8	9
# Step 1 of 6 : Find the estimated slope. Round your answer to three decimal places.
# Price (x) and Number of Bids (y)
x <- c(20,33,38,44,50)
y <- c(3,5,7,8,9)

# Fit regression model
model <- lm(y ~ x)

# Estimated slope rounded to 3 decimals
round(coef(model)[2], 3)

#  Step 2 of 6 : Find the estimated y-intercept. Round your answer to three decimal places.
# Price (x) and Number of Bids (y)
x <- c(20,33,38,44,50)
y <- c(3,5,7,8,9)

# Fit regression model
model <- lm(y ~ x)

# Estimated intercept rounded to 3 decimals
round(coef(model)[1], 3)

#  Step 3 of 6 : Determine if the statement "Not all points predicted by the linear model fall on the same line" is true or false.
# false

#  Step 4 of 6 : Substitute the values you found in steps 1 and 2 into the equation for the regression line to find the estimated linear model. According to this model, if the value of the independent variable is increased by one unit, then find the change in the dependent variable yˆ.
# Price (x) and Number of Bids (y)
x <- c(20,33,38,44,50)
y <- c(3,5,7,8,9)

# Fit model
model <- lm(y ~ x)

# Coefficients
b0 <- round(coef(model)[1], 3)
b1 <- round(coef(model)[2], 3)

# Regression equation
b0
b1

# Change in y-hat for +1 increase in x
b1


#  Step 5 of 6 : Find the estimated value of y when x=33. Round your answer to three decimal places.
# Price (x) and Number of Bids (y)
x <- c(20,33,38,44,50)
y <- c(3,5,7,8,9)

# Fit model
model <- lm(y ~ x)

# Predict y when x = 33
round(predict(model, newdata = data.frame(x = 33)), 3)

#  Step 6 of 6 : Find the value of the coefficient of determination. Round your answer to three decimal places.
# Price (x) and Number of Bids (y)
x <- c(20,33,38,44,50)
y <- c(3,5,7,8,9)

# Fit model
model <- lm(y ~ x)

# Coefficient of determination (R^2)
round(summary(model)$r.squared, 3)

########
# The table below gives the number of hours five randomly selected students spent studying and their corresponding midterm exam grades. Using this data, consider the equation of the regression line, y=b0+b1x , for predicting the midterm exam grade that a student will earn based on the number of hours spent studying. Keep in mind, the correlation coefficient may or may not be statistically significant for the data given. Remember, in practice, it would not be appropriate to use the regression line to make a prediction if the correlation coefficient is not statistically significant. 
# Step 1 of 3 : Write the estimated regression equation using the least squares estimates for b0 and b1. Round your answer to four decimal places, if necessary.
# Hours studying (x) and grades (y)
x <- c(0,2,4,5,6)
y <- c(76,86,87,91,99)

# Fit regression model
model <- lm(y ~ x)

# Coefficients rounded to 4 decimals
round(coef(model), 4)

#  Step 2 of 3 : What is the coefficient of determination for the model? Round your answer to four decimal places, if necessary.
# Hours studying (x) and grades (y)
x <- c(0,2,4,5,6)
y <- c(76,86,87,91,99)

# Fit regression model
model <- lm(y ~ x)

# Coefficient of determination (R^2)
round(summary(model)$r.squared, 4)

#  Step 3 of 3 :What percent of the variation in midterm grades is explained by the hours spent studying? Round your answer to two decimal places, if necessary.
# Hours studying (x) and grades (y)
x <- c(0,2,4,5,6)
y <- c(76,86,87,91,99)

# Fit regression model
model <- lm(y ~ x)

# Percent of variation explained
round(summary(model)$r.squared * 100, 2)

#######
# An economist is studying the relationship between income and savings. The income and savings data from seven randomly selected subjects are shown in the table. Use a simple linear regression model to predict savings based on annual income.
# Step 1 of 3 : Write the estimated regression equation using the least squares estimates for b0 and b1. Round your answer to 4 decimal places, if necessary.
# Income (x) and Savings (y)
x <- c(30,28,45,51,90,38,70)
y <- c(2.4,2.2,3.4,3.1,9.0,3.0,5.6)

# Fit regression model
model <- lm(y ~ x)

# Coefficients rounded to 4 decimals
round(coef(model), 4)

#  Step 2 of 3 : What is the coefficient of determination for the model? Round your answer to 4 decimal places, if necessary.
# Income (x) and Savings (y)
x <- c(30,28,45,51,90,38,70)
y <- c(2.4,2.2,3.4,3.1,9.0,3.0,5.6)

# Fit regression model
model <- lm(y ~ x)

# Coefficient of determination (R^2)
round(summary(model)$r.squared, 4)

#  Step 3 of 3 : What percent of the variation in savings is explained by the amount of income? Round your answer to two decimal places, if necessary.
# Income (x) and Savings (y)
x <- c(30,28,45,51,90,38,70)
y <- c(2.4,2.2,3.4,3.1,9.0,3.0,5.6)

# Fit regression model
model <- lm(y ~ x)

# Percent of variation explained
round(summary(model)$r.squared * 100, 2)

#######
# The table below displays the latitude and average high temperature in March for nine randomly selected U.S. cities. Using this data, consider the equation of the regression line, yˆ=b0+b1x for predicting an average high temperature in March from the latitude of the city.
# Step 1 of 3 : Write the estimated regression equation using the least squares estimates for b0 and b1. Round your answer to four decimal places, if necessary.
# Latitude (x) and Temperature (y) — EXACT order from table
x <- c(29.9,33.6,36.1,35.9,34.6,35.4,41.9,43.6,42.3)
y <- c(66.4,72.7,74.8,46.0,42.7,47.9,60.5,66.1,63.5)

# Fit model
model <- lm(y ~ x)

# Coefficients rounded to 4 decimals
round(coef(model), 4)

#  Step 2 of 3 : What is the coefficient of determination for the model? Round your answer to four decimal places, if necessary.
# Latitude (x) and Temperature (y)
x <- c(29.9,33.6,36.1,35.9,34.6,35.4,41.9,43.6,42.3)
y <- c(66.4,72.7,74.8,46.0,42.7,47.9,60.5,66.1,63.5)

# Fit model
model <- lm(y ~ x)

# Coefficient of determination (R^2)
round(summary(model)$r.squared, 4)

#  Step 3 of 3 : What percent of the variation in average high temperature in March is explained by the latitude? Round your answer to two decimal places, if necessary.
# Latitude (x) and Temperature (y)
x <- c(29.9,33.6,36.1,35.9,34.6,35.4,41.9,43.6,42.3)
y <- c(66.4,72.7,74.8,46.0,42.7,47.9,60.5,66.1,63.5)

# Fit model
model <- lm(y ~ x)

# Percent of variation explained
round(summary(model)$r.squared * 100, 2)

##########
# Consider the following monthly revenue data for an up-and-coming service company. Month Revenue (Thousands of Dollars): 1=132, 2=197, 3=254, 4=350, 5=385, 6=626, 7=629, 8=622, 9=799, 10=821, 11=835, 12=811, 13=826, 14=869, 15=871. Regression Statistics: Multiple R=0.934094482, R Square=0.872532501, Adjusted R Square=0.862727309, Standard Error=58.84105367, Observations=15. ANOVA: Regression df=1 SS=308096.228571 MS=308096.22857 F=88.98678163; Residual df=13 SS=45009.504762 MS=3462.269597; Total df=14 SS=353105.733333. Coefficients: Intercept b0=442.495238 SE=31.971767 t=13.840187 p=3.70989E-09; Month b1=33.171429 SE=3.516426 t=9.433281 p=3.52522E-07. 
# Step 1 of 3: Write the estimated regression equation using the least squares estimates for b0 and b1. Round to four decimal places.
b0 <- 442.4952 # Intercept Coefficient
b1 <- 33.1714 # Month Coefficient
cat("y-hat =", round(b0, 4), "+", round(b1, 4), "* x\n") # just find the Intercepts in the data

#  Step 2 of 3 : What is the mean square error for the model shown in the output? Round to four decimal places, if necessary.
MSE <- 3462.269597 # Residual MS from ANOVA table JUST find the MS Residual from Anova
round(MSE, 4)

#  Step 3 of 3 : What is the standard error for the model shown in the output? Round to four decimal places, if necessary.
SE <- 58.8411 # Standard Error from Regression Statistics table
round(SE, 4)

##########
# Consider the following table containing unemployment rates for a 10-year period. Year: 1-10, Unemployment Rate (%): 8.2, 5.8, 4.7, 4.9, 5.9, 8.5, 6.4, 9.5, 10.6, 9.1. Step 1 of 2: Given the model Estimated Unemployment Rate = b0 + b1(Year) + ei, write the estimated regression equation using the least squares estimates for b0 and b1. Round your answers to two decimal places.
year <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Year
rate <- c(8.2, 5.8, 4.7, 9.8, 9.6, 8.5, 6.4, 9.5, 10.6, 9.1) # Unemployment Rate (%)
n <- length(year) # number of observations
b1 <- round((n * sum(year * rate) - sum(year) * sum(rate)) / (n * sum(year^2) - sum(year)^2), 2) # Slope Coefficient rounded first
b0 <- round((1/n) * (sum(rate) - b1 * sum(year)), 2) # Intercept Coefficient using rounded b1
cat("Estimated Unemployment Rate =", b0, "+", b1, "* Year\n")

#######
# Consider the following table containing unemployment rates for a 10-year period. Year: 1-10, Unemployment Rate (%): 6.1, 6.4, 5.8, 3.9, 6.4, 4.7, 5.3, 11.9, 8.1, 11.3. Step 1 of 2: Given the model Estimated Unemployment Rate = b0 + b1(Year) + ei, write the estimated regression equation using the least squares estimates for b0 and b1. Round your answers to two decimal places.
year <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Year
rate <- c(6.1, 6.4, 5.8, 3.9, 6.4, 4.7, 5.3, 11.9, 8.1, 11.3) # Unemployment Rate (%)
n <- length(year) # number of observations
b1 <- round((n * sum(year * rate) - sum(year) * sum(rate)) / (n * sum(year^2) - sum(year)^2), 2) # Slope Coefficient rounded first
b0 <- round((1/n) * (sum(rate) - b1 * sum(year)), 2) # Intercept Coefficient using rounded b1
cat("Estimated Unemployment Rate =", b0, "+", b1, "* Year\n")

#######
# Consider the following table containing unemployment rates for a 10-year period. Year: 1-10, Unemployment Rate (%): 6.1, 6.4, 5.8, 3.9, 6.4, 4.7, 5.3, 11.9, 8.1, 11.3. Step 2 of 2: What is the coefficient of determination for the regression model? Round your answers to two decimal places.
year <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Year
rate <- c(6.1, 6.4, 5.8, 3.9, 6.4, 4.7, 5.3, 11.9, 8.1, 11.3) # Unemployment Rate (%)
model <- lm(rate ~ year) # fit linear regression
r_squared <- round(summary(model)$r.squared, 2) # Coefficient of Determination (R^2)
cat("R^2 =", r_squared, "\n")

#########
# Consider the following monthly revenue data for an up-and-coming cyber security company. Month: 1-14, Revenue (Thousands of Dollars): 325, 479, 535, 458, 564, 668, 587, 708, 739, 819, 831, 840, 846, 862. Regression Statistics: Multiple R=0.942446926, R Square=0.888206208, Adjusted R Square=0.878890058, Standard Error=54.60868117, Observations=14. ANOVA: Regression df=1 SS=284315.631868 MS=284315.631868 F=95.34048609; Residual df=12 SS=35785.296703 MS=2982.108059; Total df=13 SS=320100.928571. Coefficients: Intercept b0=430.934066 SE=30.827620 t=13.978830 p=8.67969E-09; Month b1=35.351648 SE=3.620520 t=9.764245 p=4.63158E-07. Step 1 of 3: Write the estimated regression equation using the least squares estimates for b0 and b1. Round to four decimal places, if necessary.
#  Step 1 of 3 : Write the estimated regression equation using the least squares estimates for b0 and b1. Round to four decimal places, if necessary.
b0 <- 430.9341 # Intercept Coefficient
b1 <- 35.3516 # Month Coefficient
cat("y-hat =", round(b0, 4), "+", round(b1, 4), "* Month\n")

# Step 2 of 3 :   Using the model from the previous step, predict the company’s revenue for the 15thfifteenth month. Round to four decimal places, if necessary.
round(b0 + b1 * 15, 4)

#  Step 3 of 3 : What percent of the variation in revenue is explained by the linear time trend model? Round to two decimal places, if necessary.
r_squared <- 0.888206208 # R Square from Regression Statistics
round(r_squared * 100, 2) # Convert to percentage

###########
# how to use lm()
# lm(y~x) fits a linear model for the data and after you use it you can 
# then do something like coef(model)[1] to pull b0(intercept) with 1 and b1(slope) with 2


##########
# Consider the following data on beer production (in million barrels) in the United States from 2010 to 2021. Year: 2010-2021, Production (million barrels): 194.28, 192.25, 195.04, 191.77, 193.01, 190.97, 189.52, 185.08, 183.35, 180.74, 179.64, 180.24. 
# Step 1 of 3: Using statistical software, determine the estimated regression equation for the data. Round the coefficients to three decimal places.
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021) # Year
production <- c(194.28, 192.25, 195.04, 191.77, 193.01, 190.97, 189.52, 185.08, 183.35, 180.74, 179.64, 180.24) # Production (million barrels)
model <- lm(production ~ year) # fit linear regression
b0 <- round(coef(model)[1], 3) # Intercept Coefficient
b1 <- round(coef(model)[2], 3) # Year Coefficient
cat("Estimated Production =", b0, "+", b1, "* Year\n")
plot(model)

# Step 2 of 3: Based on the coefficient of determination, is this model a good fit to the data? Explain your answer.
r_squared <- round(summary(model)$r.squared, 3) # Coefficient of Determination (R^2)
cat("R^2 =", r_squared, "\n") # if > .90 then strong | .70 <= R^2 <= .90 moderate fit | below .70 is a weak fit

#  Step 3 of 3 : Predict the beer production for the year 2022. Round your answer to two decimal places.
round(b0 + b1 * 2022 ,2)

#############
# Consider the following time series data collected for three variables: Mean Sea Level (meters), Air Pollution NOx (1000 tons), and Active Facebook Users Worldwide (millions). Time Period 1-12, Mean Sea Level: 0.056, 0.059, 0.088, 0.117, 0.131, 0.147, 0.156, 0.165, 0.172, 0.176, 0.181, 0.235. NOx (1000 tons): 27741, 27341, 22066, 21093, 14404, 11679, 27742, 26093, 25237, 23999, 7479, 8343. Facebook Users (millions): 2509, 2444, 2432, 2394, 2192, 2234, 2302, 2323, 2160, 2081, 2035, 1972. 
# Step 1 of 3: Plot each variable against Time Period and identify the best candidate for a linear time trend model.
time <- c(1:12) # Time Period
sea_level <- c(0.056, 0.059, 0.088, 0.117, 0.131, 0.147, 0.156, 0.165, 0.172, 0.176, 0.181, 0.235) # Mean Sea Level (meters)
nox <- c(27741, 27341, 22066, 21093, 14404, 11679, 27742, 26093, 25237, 23999, 7479, 8343) # Air Pollution NOx (1000 tons)
facebook <- c(2509, 2444, 2432, 2394, 2192, 2234, 2302, 2323, 2160, 2081, 2035, 1972) # Active Facebook Users (millions)
par(mfrow = c(1, 3)) # display all 3 plots side by side
plot(time, sea_level, main = "Mean Sea Level", xlab = "Time Period", ylab = "Meters", type = "b")
plot(time, nox, main = "Air Pollution NOx", xlab = "Time Period", ylab = "1000 Tons", type = "b")
plot(time, facebook, main = "Facebook Users", xlab = "Time Period", ylab = "Millions", type = "b")

# Step 2 of 3: Fit a linear model to the data for Mean Sea Level (meters). Write the estimated regression equation. Round the coefficients to three decimal places.
model_sea <- lm(sea_level ~ time) # fit linear regression for Mean Sea Level
b0 <- round(coef(model_sea)[1], 3) # Intercept Coefficient
b1 <- round(coef(model_sea)[2], 3) # Time Period Coefficient
cat("Estimated Mean Sea Level =", b0, "+", b1, "* Time\n")

# Step 3 of 3: Using the model from the previous step, predict the value for the 14th time period. Round your answer to three decimal places.
round(b0 + b1 * 14, 3) # Predicted Mean Sea Level for time period 14

########
# Consider the following data on beer production (in million barrels) in the United States from 2010 to 2021. Year: 2010-2021, Production (million barrels): 194.22, 192.25, 195.12, 191.82, 192.11, 191.56, 191.35, 186.14, 183.56, 180.82, 179.01, 180.47. Step 1 of 3: Using statistical software, determine the estimated regression equation for the data. Round the coefficients to three decimal places.
year <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021) # Year
production <- c(194.22, 192.25, 195.12, 191.82, 192.11, 191.56, 191.35, 186.14, 183.56, 180.82, 179.01, 180.47) # Production (million barrels)
model <- lm(production ~ year) # fit linear regression
b0 <- round(coef(model)[1], 3) # Intercept Coefficient
b1 <- round(coef(model)[2], 3) # Year Coefficient
cat("Estimated Production =", b0, "+", b1, "* Year\n")

# Step 2 of 3: Based on the coefficient of determination, is this model a good fit to the data? Explain your answer.
r_squared <- round(summary(model)$r.squared, 3) # Coefficient of Determination (R^2)
cat("R^2 =", r_squared, "\n")

# Step 3 of 3: Predict the beer production for the year 2024. Round your answer to two decimal places.
round(b0 + b1 * 2024, 2) # Predicted beer production for 2024

######
# Consider the following time series data collected for three variables: Mean Sea Level (meters), Air Pollution NOx (1000 tons), and Active Facebook Users Worldwide (millions). Time Period 1-12, Mean Sea Level: 0.056, 0.059, 0.072, 0.106, 0.131, 0.140, 0.141, 0.164, 0.164, 0.171, 0.192, 0.219. NOx (1000 tons): 26879, 26936, 22778, 19794, 14209, 12101, 27544, 26372, 24695, 23918, 7617, 8626. Facebook Users (millions): 2498, 2467, 2425, 2380, 2178, 2220, 2301, 2337, 2135, 2055, 2015, 1925. Step 1 of 3: Plot each variable against Time Period and identify the best candidate for a linear time trend model.
time <- c(1:12) # Time Period
sea_level <- c(0.056, 0.059, 0.072, 0.106, 0.131, 0.140, 0.141, 0.164, 0.164, 0.171, 0.192, 0.219) # Mean Sea Level (meters)
nox <- c(26879, 26936, 22778, 19794, 14209, 12101, 27544, 26372, 24695, 23918, 7617, 8626) # Air Pollution NOx (1000 tons)
facebook <- c(2498, 2467, 2425, 2380, 2178, 2220, 2301, 2337, 2135, 2055, 2015, 1925) # Active Facebook Users (millions)
par(mfrow = c(1, 3)) # display all 3 plots side by side
plot(time, sea_level, main = "Mean Sea Level", xlab = "Time Period", ylab = "Meters", type = "b")
plot(time, nox, main = "Air Pollution NOx", xlab = "Time Period", ylab = "1000 Tons", type = "b")
plot(time, facebook, main = "Facebook Users", xlab = "Time Period", ylab = "Millions", type = "b")


# Step 2 of 3: Fit a linear model to the data for Mean Sea Level (meters). Write the estimated regression equation. Round the coefficients to three decimal places.
model_sea <- lm(sea_level ~ time) # fit linear regression for Mean Sea Level
b0 <- round(coef(model_sea)[1], 3) # Intercept Coefficient
b1 <- round(coef(model_sea)[2], 3) # Time Period Coefficient
cat("Estimated Mean Sea Level =", b0, "+", b1, "* Time\n")

# Step 3 of 3: Using the model from the previous step, predict the value for the 14th time period. Round your answer to three decimal places.
round(b0 + b1 * 14, 3) # Predicted Mean Sea Level for time period 14


######
# Consider the following monthly revenue data for an up-and-coming technology company. Summary output from regression analysis provided. Step 1 of 3: Write the estimated regression equation using the least squares estimates for b0 and b1. Round to four decimal places, if necessary.
b0 <- 479.4474 # Intercept Coefficient
b1 <- 25.8098 # Month Coefficient
cat("y-hat =", round(b0, 4), "+", round(b1, 4), "* Month\n")

# Step 2 of 3: Using the model from the previous step, predict the company's revenue for the 21st month. Round to four decimal places, if necessary.
round(b0 + b1 * 21, 4) # Predicted revenue for month 21

# Step 3 of 3: What percent of the variation in revenue is explained by the linear time trend model? Round to two decimal places, if necessary.
r_squared <- 0.861125304 # R Square from Regression Statistics
round(r_squared * 100, 2) # Convert to percentage

# Consider the following table containing unemployment rates for a 10-year period. Step 1 of 2: Given the model Estimated Unemployment Rate = b0 + b1(Year) + ei, write the estimated regression equation using the least squares estimates for b0 and b1. Round your answers to two decimal places.
year <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) # Year
rate <- c(4.7, 6.8, 10.8, 11.5, 6.9, 11.7, 8.7, 7.4, 10.7, 11.1) # Unemployment Rate (%)
n <- length(year) # number of observations
b1 <- round((n * sum(year * rate) - sum(year) * sum(rate)) / (n * sum(year^2) - sum(year)^2), 2) # Slope Coefficient rounded first
b0 <- round((1/n) * (sum(rate) - b1 * sum(year)), 2) # Intercept Coefficient using rounded b1
cat("Estimated Unemployment Rate =", b0, "+", b1, "* Year\n")

# Step 2 of 2: What is the coefficient of determination for the regression model? Round your answers to two decimal places.
model <- lm(rate ~ year) # fit linear regression
r_squared <- round(summary(model)$r.squared, 2) # Coefficient of Determination (R^2)
cat("R^2 =", r_squared, "\n")


####### Chapter 11
# Templates

#######
# A manufacturer of chocolate chips would like to know whether its bag filling machine works correctly at the 405 gram setting.  Is there sufficient evidence at the 0.1 level that the bags are underfilled?  Assume the population is normally distributed.
# State the null and alternative hypotheses for the above scenario.
# H0: μ = 405
# H1: μ < 405

######
# In preparation for upcoming wage negotiations with the union, the managers for the Bevel Hardware Company want to establish the time required to assemble a kitchen cabinet. A first line supervisor believes that the job should take 61 minutes on average to complete. A random sample of 28 cabinets has an average assembly time of 55 minutes with a population standard deviation of 13 minutes. Is there overwhelming evidence to contradict the first line supervisor’s belief at a 0.05 significance level? Assume the population of assembly times is approximately normally distributed.
#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to two decimal places.
# use Z test
xbar <- 55 # average sample time
mu <- 61 # on average estimate
sigma <- 13 # SD 
n <- 28 # sample size
z <- (xbar - mu) / (sigma / sqrt(n))
round(z,2)

########
# Using traditional methods it takes 94 hours to receive an advanced driving license. A new training technique using Computer Aided Instruction (CAI) has been proposed. A researcher used the technique on 120 students and observed that they had a mean of 92 hours. Assume the population standard deviation is known to be 8. Is there evidence at the 0.05 level that the technique reduces the training time?
# Step 1 of 6 : State the null and alternative hypotheses.
# h0: mu=94 | ha: mu<94

#  Step 2 of 6 : Find the value of the test statistic. Round your answer to two decimal places.
mu <- 94 # old time
n <- 120 # sample size
xbar <- 92 # new mean
sigma <- 8 # sd
z <- ((xbar - mu) / (sigma / sqrt(n)))
round(z,2)

#  Step 3 of 6 : Specify if the test is one-tailed or two-tailed.
# One tailed because its only testing if its less time only

#  Step 4 of 6 : Find the P-value of the test statistic. Round your answer to four decimal places.
round(pnorm(z), 4)

#  Step 5 of 6 : Identify the level of significance for the hypothesis test.
# level of signifigance = alpha = 0.05

# Step 6 of 6 : idk i forgot
# Reject Null Hypo

#######
# An engineer has designed a valve that will regulate water pressure on an automobile engine. The valve was tested on 180 engines and the mean pressure was 6.0 pounds/square inch (psi). Assume the population variance is 0.81. The engineer designed the valve such that it would produce a mean pressure of 6.2 psi. It is believed that the valve does not perform to the specifications. A level of significance of 0.05 will be used. Find the P-value of the test statistic. Round your answer to four decimal places.
n <- 180 # sample size
mu <- 6.2 # mean presssure
xbar <- 6.0 # new pressure
var <- .81 # variance
sigma <- sqrt(var) # sd
z <- (xbar - mu) / (sigma / sqrt(n)) # zscore
round(2 * pnorm(z), 4) # P-value it is two tailed so we use 2 *

#######
# Government regulations restrict the amount of pollutants that can be released to the atmosphere through industrial smokestacks. To demonstrate that their smokestacks are releasing pollutants below the mandated limit of 5 parts per billion pollutants, REM Industries collects a random sample of 19 readings. The mean pollutant level for the sample is 4.65 parts per billion with a population standard deviation of 0.8 parts per billion. Do the data support the claim that the average pollutants produced by REM Industries are below the mandated level at a 0.01 significance level? Assume the population of readings is approximately normally distributed.
# Step 1 of 3 :   State the null and alternative hypotheses for the test. Fill in the blank below. 
# mu < 5 because we only care that its below 5

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to two decimal places.
n <- 19 # sample size
mu <- 5 # parts be billion pollutants limit
xbar <- 4.65 # sample mean
sigma <- 0.8 #sd
alpha <- 0.01
z <- (xbar - mu) / (sigma/sqrt(n))
round(z, 2)

#  Step 3 of 3 : Draw a conclusion and interpret the decision.
# if p < alpha : reject H0 (sufficient evidence)
# if p >= alpha Failed to reject H0 (insufficient evidence)
p <- round(pnorm(z), 4)
z
p
if (p < alpha) {
  print("Reject Null Hypo")
} else {
  print("Failed to Reject Null")
}

#########
# A horticulturist for a large plant nursery is conducting experiments on the growth rate of a new shrub. Based on previous research, the horticulturist feels the average daily growth rate of the new shrub is 1cm per day with a standard deviation of 0.13cm. The daily growth rate for a random sample of 12 shrubs is shown in the table below. Is there sufficient evidence at the 0.10 significance level to support the claim that the growth rate is less than 1cm per day? Assume that the daily growth rate is approximately normally distributed. 
x<- c(0.93, 	0.94, 	0.98, 	0.93, 	0.87, 	0.86, # growth rates
      0.87, 	0.93, 	1.04, 	1.00, 	1.05, 	0.92)
n <- 12 # sample size
sigma <- .13 # sd
mu <- 1 #cm per day
sigLev <-.10 #sig level 
#  Step 1 of 3 : State the null and alternative hypotheses for the test. Fill in the blank below. 
# mu < 1

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to two decimal places.
xbar <- mean(x)
z <- (xbar - mu) / (sigma / sqrt(n))
round(z, 2)

#  Step 3 of 3 : Draw a conclusion and interpret the decision.
p <- pnorm(z)
p
z
if (p < sigLev) {
  print("Reject Null for significant data")
} else {
  print("Fail to reject for insignificant data")
}

#######
# The U.S. Energy Information Administration claimed that U.S. residential customers used an average of 10,368 kilowatt hours (kWh) of electricity this year. A local power company believes that residents in their area use more electricity on average than EIA's reported average. To test their claim, the company chooses a random sample of 128 of their customers and calculates that these customers used an average of 10,745kWh of electricity last year. Assuming that the population standard deviation is 2198kWh, is there sufficient evidence to support the power company's claim at the 0.05 level of significance?
# Step 1 of 3 :  State the null and alternative hypotheses for the test. Fill in the blank below. 
# mu > 10368 because we only care that we test for over usage

############################
##### IMPORTANT !!!!!!!!!!!!!!!!!!!!!!!
#If the question has some dataset, you can easily copypaste the dataset in r and include this code
text_data <- "
Student FirstTestGrade	SecondTestGrade
1	67	70
2	97	81
3	42	61
4	69	72
5	56	63
6	66	72
7	99	90
8	79	73
9	88	78
10	63	67
11	61	65
12	83	77
13	89	85
14	43	57
15	85	76
16	40	64
17	47	66
18	93	79
19	93	80
20	56	67
21	50	67
"
data <- read.table(text = text_data, header = TRUE, sep = "\t")

print(data)
x<-data$Student.FirstTestGrade
y<-data$SecondTestGrade
x
y
plot(x,y)
model<-lm(y~x)
summary(model)
anova(model)




###########
Scratch

# A professor wants to estimate how many hours per week her students study. A simple random sample of 66 students had a mean of 15 hours of studying per week. Construct a 90% confidence interval for the mean number of hours a student studies per week. Assume that the population standard deviation is known to be 2.1 hours per week. Round to two decimal places.

x_bar <- 15      # sample mean
sigma <- 2.1     # population standard deviation
n <- 66          # sample size
conf <- 0.90     # confidence level

alpha <- 1 - conf
z <- qnorm(1 - alpha / 2)
SE <- sigma / sqrt(n)
ME <- z * SE

lower <- x_bar - ME
upper <- x_bar + ME

round(lower, 2)
round(upper, 2)

#########
# Birth weights (x) and lengths (y)
x <- c(11, 6, 5, 5, 3, 5, 8, 9, 4, 3)
y <- c(19, 17, 15, 18, 16, 15, 17, 18, 16, 15)

# Fit least-squares regression line
model <- lm(y ~ x)

# Coefficients (intercept and slope)
coef(model)

# Rounded to three decimals
round(coef(model), 3)


########
# Birth weights (x) and lengths (y)
x <- c(11, 6, 5, 5, 3, 5, 8, 9, 4, 3)
y <- c(19, 17, 15, 18, 16, 15, 17, 18, 16, 15)

# Fit linear regression model
model <- lm(y ~ x)

# Predict length for a 9-pound baby
pred <- predict(model, newdata = data.frame(x = 9))

# Round to two decimals
round(pred, 2)

#########

# Hours unsupervised (x) and overall grades (y)
x <- c(1, 1.5, 2, 2.5, 3, 4.5, 5.5)
y <- c(99, 93, 83, 75, 70, 68, 66)

# Compute slope b1
b1 <- cov(x, y) / var(x)

# Round to three decimals
round(b1, 3)

#########
# Hours unsupervised vs overall grades — find estimated y-intercept (b0)
x <- c(1, 1.5, 2, 2.5, 3, 4.5, 5.5)  # hours unsupervised
y <- c(99, 93, 83, 75, 70, 68, 66)   # overall grades

model <- lm(y ~ x)
b0 <- coef(model)[1]  # intercept

round(b0, 3)
plot(x,y)

########
# Hours unsupervised and overall grades
x <- c(1, 1.5, 2, 2.5, 3, 4.5, 5.5) # hours unsupervised
y <- c(99, 93, 83, 75, 70, 68, 66) # overall grades

# Fit linear regression model
model <- lm(y ~ x)

# Coefficient of determination (R^2)
r2 <- summary(model)$r.squared

# Round to three decimals
round(r2, 3)

########
285.6 

########
alpha <- 0.05
df <- 27 - 1
t_crit <- qt(1 - alpha/2, df)
round(t_crit, 3)

######
# A random sample of 27 fields of spring wheat has a mean yield of 44.2 bushels per acre and standard deviation of 8.74. Construct a 95% confidence interval.

n <- 27
xbar <- 44.2
s <- 8.74
df <- n - 1
alpha <- 0.05

t_crit <- qt(1 - alpha/2, df)
SE <- s / sqrt(n)
ME <- t_crit * SE

lower <- xbar - ME
upper <- xbar + ME

round(c(lower, upper), 1)

x<- c(56,	57,	57.5,	58.5,	60)
y<- c(4,	3.5,	2.5,	2,	1.5)
plot(x,y)

# correlation coefficient
r <- cor(x, y)

# round to three decimals
round(r, 3)

###############
# A manufacturer of chocolate chips would like to know whether its bag filling machine works correctly at the 401 gram setting. It is believed that the machine is underfilling the bags. A 14 bag sample had a mean of 396 grams with a variance of 225. Assume the population is normally distributed. A level of significance of 0.02 will be used. Specify the type of hypothesis test.
# Left tailed because you are just checking for if they are being underfilled, if they are then its just left tailed

############
# A manufacturer of chocolate chips would like to know whether its bag filling machine works correctly at the 423 gram setting. It is believed that the machine is underfilling the bags. A 26 bag sample had a mean of 415 grams with a variance of 256. Assume the population is normally distributed. A level of significance of 0.02 will be used. Find the P-value of the test statistic.
mu <- 423 # target setting
xbar <- 415 # sample mean
var <- 256 # variance
s <- sqrt(var) # standard deviation
n <- 26 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic (variance known from sample, not population)
p_value <- round(pt(t, df = n - 1), 4) # left-tailed P-value
cat("t =", round(t, 4), "\n")
cat("P-value =", p_value, "\n")

########
# The nutrition label for a new barbecue sauce states that one bottle of sauce has 890mg of sodium. A local consumer group claims the sodium content is higher than reported. 12 bottles selected. Significance level 0.005. 
# Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
sodium <- c(881, 901, 913, 890, 899, 898, 878, 887, 882, 915, 913, 893) # Sodium Content (mg)
mu <- 890 # claimed population mean
n <- length(sodium) # sample size
xbar <- mean(sodium) # sample mean
s <- sd(sodium) # sample standard deviation
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = FALSE), 4) # right-tailed P-value
alpha <- 0.005 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that sodium content is higher than 890mg\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that sodium content is higher than 890mg\n")
}

#######
# Community college instructors' salaries in one state are very low, so low that educators in that state regularly complain about their compensation. The national mean is $45,439, but instructors from Mississippi claim that the mean in their state is significantly lower. They survey a simple random sample of 33 colleges in the state and calculate a mean salary of $40,634 with a standard deviation of $12,698. Test the instructors' claim at the 0.005 level of significance.
# Step 1 of 3 : State the null and alternative hypotheses for the test. Fill in the blank below. 
# It is < because we are comparing the national average as the mu to the local salary

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to three decimal places.
mu <- 45439 # national population mean
xbar <- 40634 # sample mean
s <- 12698 # sample standard deviation
n <- 33 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value
alpha <- 0.005 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that mean salary in Mississippi is lower than $45,439\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that mean salary in Mississippi is lower than $45,439\n")
}

##########
# The admitting office at Sisters of Mercy Hospital wants to be able to inform patients of the average level of expenses they can expect per day. Historically, the average has been approximately $1280. The office would like to know if there is evidence of a decrease in the average daily billing. Seventy-one randomly selected patients have an average daily charge of $1233 with a standard deviation of $288. Conduct a hypothesis test to determine whether there is evidence that average daily charges have decreased at a significance level of α=0.02. Assume the population of daily hospital charges is approximately normally distributed.
# Step 1 of 3 :  State the null and alternative hypotheses for the test. Fill in the blank below. 
# <

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to three decimal places.
mu <- 1280 # historical population mean
xbar <- 1233 # sample mean
s <- 288 # sample standard deviation
n <- 71 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# # Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value
alpha <- 0.02 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that average daily charges have decreased below $1280\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that average daily charges have decreased below $1280\n")
}

###########
# A pizza delivery chain advertises that it will deliver your pizza in 25 minutes from when the order is placed. Being a skeptic, you decide to test and see if the mean delivery time is actually more than 25 minutes. For the simple random sample of 5 customers who record the amount of time it takes for each of their pizzas to be delivered, the mean is 28.4 minutes with a standard deviation of 7.4 minutes. Assume that the population distribution is approximately normal. Perform a hypothesis test using a 0.005 level of significance.
#  Step 1 of 3 : State the null and alternative hypotheses for the test. Fill in the blank below. 
# More than, so we use >

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to three decimal places.
mu <- 25 # advertised delivery time
xbar <- 28.4 # sample mean
s <- 7.4 # sample standard deviation
n <- 5 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3 : Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = FALSE), 4) # right-tailed P-value
alpha <- 0.005 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that mean delivery time is more than 25 minutes\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that mean delivery time is more than 25 minutes\n")
}

#########
# A pizza delivery chain advertises delivery in 35 minutes. Testing if mean delivery time is more than 35 minutes. Sample of 12 customers with mean 38.8 minutes and standard deviation 7.8 minutes. Significance level 0.005. Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
mu <- 35 # advertised delivery time
xbar <- 38.8 # sample mean
s <- 7.8 # sample standard deviation
n <- 12 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = FALSE), 4) # right-tailed P-value
alpha <- 0.005 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that mean delivery time is more than 35 minutes\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that mean delivery time is more than 35 minutes\n")
}

#####
# Community college instructors' salaries. National mean is $49,574. 32 colleges surveyed with mean $43,457 and standard deviation $13,881. Test at 0.025 significance level. Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
mu <- 49574 # national population mean
xbar <- 43457 # sample mean
s <- 13881 # sample standard deviation
n <- 32 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value
alpha <- 0.025 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that mean salary in Mississippi is lower than $49,574\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that mean salary in Mississippi is lower than $49,574\n")
}

######
# The nutrition label for a new barbecue sauce states one bottle has 890mg of sodium. Local consumer group claims sodium content is higher. FDA selects 12 bottles. Significance level 0.005. Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
sodium <- c(881, 901, 913, 890, 899, 898, 878, 887, 882, 915, 913, 893) # Sodium Content (mg)
mu <- 890 # claimed population mean
n <- length(sodium) # sample size
xbar <- mean(sodium) # sample mean
s <- sd(sodium) # sample standard deviation
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = FALSE), 4) # right-tailed P-value
alpha <- 0.005 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that sodium content is higher than 890mg\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that sodium content is higher than 890mg\n")
}

########
# The admitting office at Sisters of Mercy Hospital wants to know if there is evidence of a decrease in average daily billing. Historical mean is $1170. 77 patients sampled with mean $1103 and standard deviation $254. Significance level 0.05. Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
mu <- 1170 # historical population mean
xbar <- 1103 # sample mean
s <- 254 # sample standard deviation
n <- 77 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value
alpha <- 0.05 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that average daily charges have decreased below $1170\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that average daily charges have decreased below $1170\n")
}

######
# Officials want to determine if average time per move for top chess players has remained under 7 minutes. Sample of 48 moves with mean 6.5 minutes and standard deviation 1.3 minutes. Significance level 0.05. Step 2 of 3: Compute the value of the test statistic. Round to three decimal places.
mu <- 7 # claimed population mean
xbar <- 6.5 # sample mean
s <- 1.3 # sample standard deviation
n <- 48 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
round(t, 3)

# Step 3 of 3: Draw a conclusion and interpret the decision.
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value
alpha <- 0.05 # level of significance
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that average time per move is under 7 minutes\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that average time per move is under 7 minutes\n")
}

########
# A manufacturer of chocolate chips would like to know whether its bag filling machine works correctly at the 428 gram setting. It is believed that the machine is underfilling the bags. A 26 bag sample had a mean of 420 grams with a standard deviation of 17. Significance level 0.01. Is there sufficient evidence to support the claim that the bags are underfilled?
mu <- 428 # target setting
xbar <- 420 # sample mean
s <- 17 # sample standard deviation
n <- 26 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value (underfilling)
alpha <- 0.01 # level of significance
cat("H0: mu = 428\n")
cat("Ha: mu < 428\n")
cat("t =", round(t, 3), "\n")
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the claim that the bags are underfilled\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the claim that the bags are underfilled\n")
}

#########
# A manufacturer of potato chips would like to know whether its bag filling machine works correctly at the 420 gram setting. It is believed that the machine is underfilling the bags. A 23 bag sample had a mean of 411 grams with a variance of 676. Significance level 0.02. Find the P-value of the test statistic.
mu <- 420 # target setting
xbar <- 411 # sample mean
var <- 676 # variance
s <- sqrt(var) # sample standard deviation
n <- 23 # sample size
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic
p_value <- round(pt(t, df = n - 1, lower.tail = TRUE), 4) # left-tailed P-value (underfilling)
cat("t =", round(t, 3), "\n")
cat("P-value =", p_value, "\n")

##################################################
# Chapter 11.3 Template - One Sample t-Test

# ---- INPUT VARIABLES ----
mu <- 0 # claimed/target population mean (from problem statement)
xbar <- 0 # sample mean
s <- 0 # sample standard deviation (if variance given, use sqrt(variance))
n <- 0 # sample size
alpha <- 0 # level of significance (e.g. 0.01, 0.02, 0.05)
tail <- "left" # direction of test: "left" = underfilling/decrease/lower, "right" = overfilling/increase/higher, "two" = not equal/different

# ---- CALCULATIONS ----
t <- (xbar - mu) / (s / sqrt(n)) # t test statistic

p_value <- if(tail == "left") {
  round(pt(t, df = n - 1, lower.tail = TRUE), 4)   # left-tailed P-value
} else if(tail == "right") {
  round(pt(t, df = n - 1, lower.tail = FALSE), 4)  # right-tailed P-value
} else {
  round(2 * pt(-abs(t), df = n - 1), 4)            # two-tailed P-value
}

# ---- OUTPUT ----
cat("t =", round(t, 3), "\n")
cat("P-value =", p_value, "\n")
if(p_value < alpha) {
  cat("Reject H0: Sufficient evidence to support the alternative claim\n")
} else {
  cat("Fail to reject H0: Insufficient evidence to support the alternative claim\n")
}
###################################################

########
# A sample of 1700 computer chips revealed that 81% do not fail in the first 1000 hours.
# Stated proportion is 79%. Test if actual percentage is different (two-tailed). Alpha = 0.02.

n <- 1700        # sample size
p_hat <- 0.81    # sample proportion
p0 <- 0.79       # claimed proportion (null hypothesis)
alpha <- 0.02    # significance level

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)          # z test statistic
p_value <- round(2 * pnorm(abs(z), lower.tail = FALSE), 4)  # two-tailed p-value

cat("z =", round(z, 3), "\n")
cat("P-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence to support the manager's claim.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence to support the manager's claim.\n")
}

#######
# The mayor proposed annexation. A sample of 1000 voters found 57% in favor.
# Test the claim that more than 53% favor annexation (right-tailed). 

n <- 1000        # sample size
p_hat <- 0.57    # sample proportion
p0 <- 0.53       # claimed proportion (null hypothesis)
alpha <- 0.05    # significance level (not stated, default)

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)           # z test statistic
p_value <- round(pnorm(z, lower.tail = FALSE), 4)       # right-tailed p-value

cat("z =", round(z, 3), "\n")
cat("P-value =", p_value, "\n")

########
# A sample of 200 passengers, 123 had carry-on luggage.
# Test claim that carry-on usage has increased from 56%. Alpha = 0.10 (right-tailed).

n <- 200            # sample size
x <- 123            # number of passengers with carry-on luggage
p0 <- 0.56          # historical proportion (null hypothesis)
p_hat <- x / n      # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)    # z test statistic

cat("p_hat =", round(p_hat, 4), "\n")
cat("z =", round(z, 2), "\n")

#  Step 3 of 3 : Draw a conclusion and interpret the decision.
n <- 200            # sample size
x <- 123            # number of passengers with carry-on luggage
p0 <- 0.56          # historical proportion (null hypothesis)
alpha <- 0.10       # significance level
p_hat <- x / n      # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)    # z test statistic
p_value <- round(pnorm(z, lower.tail = FALSE), 4) # right-tailed p-value

cat("z =", round(z, 2), "\n")
cat("P-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence of an increase in carry-on luggage.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence of an increase in carry-on luggage.\n")
}

# Step 2 of 3: Compute the value of the test statistic.

n <- 160            # sample size
x <- 34             # number of qualified applicants
p0 <- 0.29          # historical proportion (null hypothesis)
p_hat <- x / n      # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)    # z test statistic

cat("p_hat =", round(p_hat, 4), "\n")
cat("z =", round(z, 2), "\n")

# Step 3 of 3: Draw a conclusion and interpret the decision.

n <- 160            # sample size
x <- 34             # number of qualified applicants
p0 <- 0.29          # historical proportion (null hypothesis)
alpha <- 0.025      # significance level
p_hat <- x / n      # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)     # z test statistic
p_value <- round(pnorm(z, lower.tail = TRUE), 4)  # left-tailed p-value

cat("z =", round(z, 2), "\n")
cat("P-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the percentage of qualified applicants is less than 29%.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the percentage of qualified applicants is less than 29%.\n")
}

##########
# An environmentalist wishes to conduct a hypothesis test on the percentage of cars driven in the city that are hybrids. Is it sufficient for him to use a simple random sample of 145 cars if hybrids currently account for 7% of the car sales in the country and he claims that the percentage of hybrids in the city is higher than that?

n <- 145        # sample size
p0 <- 0.07      # national proportion (null hypothesis)

# Check normal approximation conditions
np <- n * p0            # expected successes
nq <- n * (1 - p0)      # expected failures

cat("np =", np, "\n")
cat("n(1-p) =", nq, "\n")

if (np >= 5 & nq >= 5) {
  cat("Conditions MET: Sample size is sufficient.\n")
} else {
  cat("Conditions NOT MET: Sample size is not sufficient.\n")
}

##############
# The mayor of a town has proposed a plan for the construction of an adjoining bridge. A political study took a sample of 1300 voters in the town and found that 38% of the residents favored construction. Using the data, a political strategist wants to test the claim that the percentage of residents who favor construction is above 35%. Find the value of the test statistic. Round your answer to two decimal places.

n <- 1300       # sample size
p_hat <- 0.38   # sample proportion
p0 <- 0.35      # claimed proportion (null hypothesis)

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)    # z test statistic

cat("z =", round(z, 2), "\n")

############
# Ships arriving in US ports are inspected by customs officials for contaminated cargo. Assume, for a certain port, that 24% of the ships arriving in the previous year contained cargo that was contaminated. A random selection of 50 ships in the current year included seven that had contaminated cargo. Does the data suggest that the proportion of ships arriving in the port with contaminated cargo has decreased in the current year at α=0.01?

# H0: p = 0.24
# Ha: p < 0.24

n <- 50         # sample size
x <- 7          # number of ships with contaminated cargo
p0 <- 0.24      # historical proportion (null hypothesis)
alpha <- 0.01   # significance level
p_hat <- x / n  # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)     # z test statistic
p_value <- round(pnorm(z, lower.tail = TRUE), 4)  # left-tailed p-value

cat("p_hat =", round(p_hat, 4), "\n")
cat("z =", round(z, 2), "\n")
cat("P-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the proportion of contaminated cargo has decreased.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the proportion of contaminated cargo has decreased.\n")
}

#########
# A hospital director is told that 55% of the treated patients are insured. The director wants to test the claim that the percentage of insured patients is less than the expected percentage. A sample of 380 patients found that 190 were insured. Find the value of the test statistic. Round your answer to two decimal places.

n <- 380        # sample size
x <- 190        # number of insured patients
p0 <- 0.55      # claimed proportion (null hypothesis)
p_hat <- x / n  # sample proportion

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)    # z test statistic

cat("p_hat =", round(p_hat, 4), "\n")
cat("z =", round(z, 2), "\n")

########
# A publisher reports that 59% of their readers own a particular make of car. A marketing executive wants to test the claim that the percentage is actually different from the reported percentage. A random sample of 280 found that 55% of the readers owned a particular make of car. Determine the P-value of the test statistic. Round your answer to four decimal places.

n <- 280        # sample size
p_hat <- 0.55   # sample proportion
p0 <- 0.59      # reported proportion (null hypothesis)

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)            # z test statistic
p_value <- round(2 * pnorm(abs(z), lower.tail = FALSE), 4)  # two-tailed p-value

cat("z =", round(z, 2), "\n")
cat("P-value =", p_value, "\n")

#########################################################
# 11.5 Template - One Proportion Z-Test
# IMPORTANT decide the hypothesis and then uncomment the Pvalue needed

# H0: p = [p0]
# Ha: p [</>/≠] [p0]

n <- NULL        # sample size
x <- NULL        # number of successes (use if p_hat not given directly)
p0 <- NULL       # claimed proportion (null hypothesis)
alpha <- NULL    # significance level
p_hat <- x / n  # sample proportion (replace with direct value if given)

z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)                    # z test statistic
p_value <- round(pnorm(z, lower.tail = TRUE), 4)                 # left-tailed  (p <)
# p_value <- round(pnorm(z, lower.tail = FALSE), 4)              # right-tailed (p >)
# p_value <- round(2 * pnorm(abs(z), lower.tail = FALSE), 4)     # two-tailed   (p ≠)

cat("p_hat =", round(p_hat, 4), "\n")
cat("z =", round(z, 2), "\n")
cat("P-value =", p_value, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence to support the claim.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence to support the claim.\n")
}
#####################################################

# The refrigeration coolers in a local grocery store must stay at the same daily temperature with little variance to ensure the quality of the items placed in it. Daily temperatures are measured in degrees Fahrenheit (℉), and the store manager assumes the standard deviation in daily temperatures is 2.9℉. The assistant manager claims that the standard deviation is more than 2.9℉ and decides to test the claim using a hypothesis test. For a random sample of 30 days, the assistant manager finds that the standard deviation in the daily temperatures for one cooler is 3.5℉. At the 0.1 level of significance, does the evidence support the claim that the standard deviation in the daily temperatures for the cooler is more than 2.9℉? Assume the population is normally distributed.

# H0: σ = 2.9
# Ha: σ > 2.9

alpha <- 0.1    # significance level

# Step 3 of 5 : Compute the value of the test statistic. Round your answer to two decimal places.
n <- 30         # sample size
s <- 3.5        # sample standard deviation
sigma0 <- 2.9   # claimed population standard deviation (null hypothesis)

chi2 <- (n - 1) * s^2 / sigma0^2    # chi-square test statistic

cat("chi-square =", round(chi2, 2), "\n")

#  Step 4 of 5 : Determine the P-value of the test statistic you calculated. Round your answer to 3 decimal places.
# The refrigeration coolers in a local grocery store must stay at the same daily temperature with little variance to ensure the quality of the items placed in it. Daily temperatures are measured in degrees Fahrenheit (℉), and the store manager assumes the standard deviation in daily temperatures is 2.9℉. The assistant manager claims that the standard deviation is more than 2.9℉ and decides to test the claim using a hypothesis test. For a random sample of 30 days, the assistant manager finds that the standard deviation in the daily temperatures for one cooler is 3.5℉. At the 0.1 level of significance, does the evidence support the claim that the standard deviation in the daily temperatures for the cooler is more than 2.9℉? Assume the population is normally distributed.

n <- 30         # sample size
s <- 3.5        # sample standard deviation
sigma0 <- 2.9   # claimed population standard deviation (null hypothesis)

chi2 <- (n - 1) * s^2 / sigma0^2                        # chi-square test statistic
p_value <- round(pchisq(chi2, df = n - 1, lower.tail = FALSE), 3)  # right-tailed p-value

cat("chi-square =", round(chi2, 2), "\n")
cat("P-value =", p_value, "\n")

# Step 5 of 5 : Draw a conclusion and interpret the decision.

n <- 30         # sample size
s <- 3.5        # sample standard deviation
sigma0 <- 2.9   # claimed population standard deviation (null hypothesis)
alpha <- 0.1    # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                                    # chi-square test statistic
p_value_display <- round(pchisq(chi2, df = n - 1, lower.tail = FALSE), 3)  # rounded for display
p_value <- pchisq(chi2, df = n - 1, lower.tail = FALSE)             # unrounded for comparison

cat("chi-square =", round(chi2, 2), "\n")
cat("P-value =", p_value_display, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the standard deviation in daily temperatures is more than 2.9℉.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the standard deviation in daily temperatures is more than 2.9℉.\n")
}

###############
# Colonial Funds claims to have a bond fund which has maintained a mean share price of $18.00. They claim that the variance of the share price is 0.19. To test this claim, the investor randomly selects 17 days during the last year. He finds an average share price of $17.80 with a standard deviation of 0.3357. Can the investor conclude that the share price of the bond fund varies from Colonial Funds claims at α=0.01?

# H0: σ = sqrt(0.19) = 0.4359
# Ha: σ ≠ 0.4359

n <- 17             # sample size
s <- 0.3357         # sample standard deviation
sigma0 <- sqrt(0.19) # claimed population standard deviation (null hypothesis)
alpha <- 0.01       # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                                      # chi-square test statistic
p_value_display <- round(2 * pchisq(chi2, df = n - 1, lower.tail = TRUE), 4)  # two-tailed p-value rounded for display
p_value <- 2 * pchisq(chi2, df = n - 1, lower.tail = TRUE)            # unrounded for comparison

cat("sigma0 =", round(sigma0, 4), "\n")
cat("chi-square =", round(chi2, 2), "\n")
cat("P-value =", p_value_display, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the share price variance differs from Colonial Funds claims.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the share price variance differs from Colonial Funds claims.\n")
}

# Step 2 of 5: Determine the critical value(s) of the test statistic.

n <- 17             # sample size
alpha <- 0.01       # significance level

cv_lower <- round(qchisq(alpha / 2, df = n - 1, lower.tail = TRUE), 3)   # lower critical value (two-tailed)
cv_upper <- round(qchisq(alpha / 2, df = n - 1, lower.tail = FALSE), 3)  # upper critical value (two-tailed)

cat("Lower critical value =", cv_lower, "\n")
cat("Upper critical value =", cv_upper, "\n")

# Step 3 of 5: Determine the value of the test statistic.

n <- 17              # sample size
s <- 0.3357          # sample standard deviation
sigma0 <- sqrt(0.19) # claimed population standard deviation (null hypothesis)

chi2 <- round((n - 1) * s^2 / sigma0^2, 3)    # chi-square test statistic

cat("chi-square =", chi2, "\n")

# Step 4 of 5: Make the decision.

n <- 17              # sample size
s <- 0.3357          # sample standard deviation
sigma0 <- sqrt(0.19) # claimed population standard deviation (null hypothesis)
alpha <- 0.01        # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                                      # chi-square test statistic
cv_lower <- qchisq(alpha / 2, df = n - 1, lower.tail = TRUE)          # lower critical value
cv_upper <- qchisq(alpha / 2, df = n - 1, lower.tail = FALSE)         # upper critical value

cat("chi-square =", round(chi2, 3), "\n")
cat("Lower critical value =", round(cv_lower, 3), "\n")
cat("Upper critical value =", round(cv_upper, 3), "\n")

if (chi2 < cv_lower | chi2 > cv_upper) {
  cat("Decision: Reject H0\n")
} else {
  cat("Decision: Fail to reject H0\n")
}

# Step 5 of 5: What is the conclusion?

n <- 17              # sample size
s <- 0.3357          # sample standard deviation
sigma0 <- sqrt(0.19) # claimed population standard deviation (null hypothesis)
alpha <- 0.01        # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                              # chi-square test statistic
p_value_display <- round(2 * pchisq(chi2, df = n - 1, lower.tail = TRUE), 4)  # two-tailed p-value rounded for display
p_value <- 2 * pchisq(chi2, df = n - 1, lower.tail = TRUE)    # unrounded for comparison

cat("chi-square =", round(chi2, 3), "\n")
cat("P-value =", p_value_display, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the share price variance differs from Colonial Funds claims.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the share price variance differs from Colonial Funds claims.\n")
}

#########
# A drug, which is used for treating cancer, has potentially dangerous side effects if it is taken in doses which are larger than the required dosage for the treatment. The tablet should contain 56.21mg and the variance should be 0.04. 10 tablets are randomly selected and the amount of drug in each tablet is measured. The sample has a mean of 56.205mg and a variance of 0.0002mg. Does the data suggest at α=0.025 that the tablets vary by less than the desired amount?
# Step 1 of 5: State the hypotheses in terms of the standard deviation.

# H0: σ = sqrt(0.04) = 0.2
# Ha: σ < 0.2

# Step 2 of 5: Determine the critical value(s) of the test statistic.

n <- 10             # sample size
alpha <- 0.025      # significance level

cv <- round(qchisq(alpha, df = n - 1, lower.tail = TRUE), 3)    # left-tailed critical value

cat("Critical value =", cv, "\n")

# Step 3 of 5: Determine the value of the test statistic.

n <- 10              # sample size
s2 <- 0.0002         # sample variance
sigma0 <- sqrt(0.04) # claimed population standard deviation (null hypothesis)

chi2 <- round((n - 1) * s2 / sigma0^2, 3)    # chi-square test statistic

cat("chi-square =", chi2, "\n")

# Step 4 of 5: Make the decision.

n <- 10              # sample size
s2 <- 0.0002         # sample variance
sigma0 <- sqrt(0.04) # claimed population standard deviation (null hypothesis)
alpha <- 0.025       # significance level

chi2 <- (n - 1) * s2 / sigma0^2                               # chi-square test statistic
cv <- qchisq(alpha, df = n - 1, lower.tail = TRUE)            # left-tailed critical value

cat("chi-square =", round(chi2, 3), "\n")
cat("Critical value =", round(cv, 3), "\n")

if (chi2 < cv) {
  cat("Decision: Reject H0\n")
} else {
  cat("Decision: Fail to reject H0\n")
}

# Step 5 of 5: What is the conclusion?

n <- 10              # sample size
s2 <- 0.0002         # sample variance
sigma0 <- sqrt(0.04) # claimed population standard deviation (null hypothesis)
alpha <- 0.025       # significance level

chi2 <- (n - 1) * s2 / sigma0^2                                        # chi-square test statistic
p_value_display <- round(pchisq(chi2, df = n - 1, lower.tail = TRUE), 4)  # left-tailed p-value rounded for display
p_value <- pchisq(chi2, df = n - 1, lower.tail = TRUE)                 # unrounded for comparison

cat("chi-square =", round(chi2, 3), "\n")
cat("P-value =", p_value_display, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the tablets vary by less than the desired amount.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the tablets vary by less than the desired amount.\n")
}

#################
# A standardized test is given to a sixth grade class. Historically the mean score has been 170 with a standard deviation of 19. The superintendent believes that the standard deviation of performance may have recently increased. She randomly sampled 24 students and found a mean of 160 with a standard deviation of 28.9849. Is there evidence that the standard deviation has increased at the α=0.025 level?

# H0: σ = 19
# Ha: σ > 19

n <- 24              # sample size
s <- 28.9849         # sample standard deviation
sigma0 <- 19         # claimed population standard deviation (null hypothesis)
alpha <- 0.025       # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                                        # chi-square test statistic
cv <- round(qchisq(alpha, df = n - 1, lower.tail = FALSE), 3)           # right-tailed critical value
p_value_display <- round(pchisq(chi2, df = n - 1, lower.tail = FALSE), 4)  # right-tailed p-value rounded for display
p_value <- pchisq(chi2, df = n - 1, lower.tail = FALSE)                 # unrounded for comparison

cat("chi-square =", round(chi2, 3), "\n")
cat("Critical value =", cv, "\n")
cat("P-value =", p_value_display, "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the standard deviation has increased.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the standard deviation has increased.\n")
}

#############
# A bolt manufacturer is very concerned about the consistency with which his machines produce bolts. The bolts should be 0.19 centimeters in diameter. The variance of the bolts should be 0.025. A random sample of 24 bolts has an average diameter of 0.18cm with a standard deviation of 0.246. Can the manufacturer conclude that the bolts vary by more than the required variance at α=0.05 level?

# H0: σ = sqrt(0.025) = 0.1581
# Ha: σ > 0.1581

n <- 24              # sample size
s <- 0.246           # sample standard deviation
sigma0 <- sqrt(0.025) # claimed population standard deviation (null hypothesis)
alpha <- 0.05        # significance level

chi2 <- (n - 1) * s^2 / sigma0^2                                        # chi-square test statistic
cv <- round(qchisq(alpha, df = n - 1, lower.tail = FALSE), 3)           # right-tailed critical value
p_value_display <- round(pchisq(chi2, df = n - 1, lower.tail = FALSE), 4)  # right-tailed p-value rounded for display
p_value <- pchisq(chi2, df = n - 1, lower.tail = FALSE)                 # unrounded for comparison

cat("Step 1 - Hypotheses:\n")
cat("H0: σ =", round(sigma0, 4), "\n")
cat("Ha: σ >", round(sigma0, 4), "\n\n")

cat("Step 2 - Critical Value:\n")
cat("CV =", cv, "\n\n")

cat("Step 3 - Test Statistic:\n")
cat("chi-square =", round(chi2, 3), "\n\n")

cat("Step 4 - Decision:\n")
if (chi2 > cv) {
  cat("chi-square > CV: Reject H0\n\n")
} else {
  cat("chi-square < CV: Fail to reject H0\n\n")
}

cat("Step 5 - Conclusion:\n")
cat("P-value =", p_value_display, "\n")
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence that the bolts vary by more than the required variance.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence that the bolts vary by more than the required variance.\n")
}


######################################################
# 11.6 Template - Chi-Square Test for Standard Deviation/Variance
# manually enter the 

# H0: σ = [sigma0]
# Ha: σ [</>/≠] [sigma0]

n <- NULL                # sample size
s <- NULL                # sample standard deviation (if given variance, use sqrt(variance))
sigma0 <- NULL           # claimed population standard deviation (if given variance, use sqrt(variance))
alpha <- NULL            # significance level
Ha <- NULL               # alternative hypothesis symbol: "<", ">", or "!="

chi2 <- (n - 1) * s^2 / sigma0^2    # chi-square test statistic

if (Ha == ">") {
  cv <- round(qchisq(alpha, df = n - 1, lower.tail = FALSE), 3)
  p_value <- pchisq(chi2, df = n - 1, lower.tail = FALSE)
} else if (Ha == "<") {
  cv <- round(qchisq(alpha, df = n - 1, lower.tail = TRUE), 3)
  p_value <- pchisq(chi2, df = n - 1, lower.tail = TRUE)
} else if (Ha == "!=") {
  cv_lower <- round(qchisq(alpha / 2, df = n - 1, lower.tail = TRUE), 3)
  cv_upper <- round(qchisq(alpha / 2, df = n - 1, lower.tail = FALSE), 3)
  p_value <- 2 * pchisq(chi2, df = n - 1, lower.tail = TRUE)
}

cat("Step 1 - Hypotheses:\n")
cat("H0: σ =", round(sigma0, 4), "\n")
cat("Ha: σ", Ha, round(sigma0, 4), "\n\n")

cat("Step 2 - Critical Value(s):\n")
if (Ha == "!=") {
  cat("CV lower =", cv_lower, "\n")
  cat("CV upper =", cv_upper, "\n\n")
} else {
  cat("CV =", cv, "\n\n")
}

cat("Step 3 - Test Statistic:\n")
cat("chi-square =", round(chi2, 3), "\n\n")

cat("Step 4 - Decision:\n")
reject <- if (Ha == ">") chi2 > cv else if (Ha == "<") chi2 < cv else (chi2 < cv_lower | chi2 > cv_upper)
if (reject) {
  cat("Reject H0\n\n")
} else {
  cat("Fail to reject H0\n\n")
}

cat("Step 5 - Conclusion:\n")
cat("P-value =", round(p_value, 4), "\n")
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There IS sufficient evidence to support the claim.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is NOT sufficient evidence to support the claim.\n")
}

###############
# An economist is studying the relationship between income and savings. The income and savings data from seven randomly selected subjects are shown in the table. Use a simple linear regression model to predict savings based on annual income.

x <- c(29, 85, 48, 54, 60, 41, 100)   # income in thousands of dollars
y <- c(2.3, 8.5, 3.6, 3.2, 4.8, 3.3, 10)   # savings in thousands of dollars

model <- lm(y ~ x)
round(coef(model), 4)

# Step 2: Coefficient of determination (R^2)

x <- c(29, 85, 48, 54, 60, 41, 100)
y <- c(2.3, 8.5, 3.6, 3.2, 4.8, 3.3, 10)
model <- lm(y ~ x)
r_squared <- summary(model)$r.squared
round(r_squared, 4)

# Step 3: Percent of variation explained

x <- c(29, 85, 48, 54, 60, 41, 100)
y <- c(2.3, 8.5, 3.6, 3.2, 4.8, 3.3, 10)
model <- lm(y ~ x)
percent_explained <- summary(model)$r.squared * 100
round(percent_explained, 2)


#########
# Suppose the estimated regression equation is Salary = 24551.80 + 2389.87*(Years of Experience). Find the estimated salary for 3 years of experience.

b0 <- 24551.80   # intercept
b1 <- 2389.87    # slope
x <- 3           # years of experience

y_hat <- b0 + b1 * x
round(y_hat, 2)

#########
# Consider the following table containing unemployment rates for a 10-year period. Given the model Estimated Unemployment Rate = beta0 + beta1(Year) + error, write the estimated regression equation using the least squares estimates for beta0 and beta1. Round your answers to two decimal places.

x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)   # year
y <- c(4.9, 5.2, 8.8, 3.8, 5.7, 4.1, 9.6, 7.2, 6.2, 11.1)   # unemployment rate

model <- lm(y ~ x)
round(coef(model), 2)

# Step 2: Coefficient of determination (R^2) for unemployment rate regression model
x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(4.9, 5.2, 8.8, 3.8, 5.7, 4.1, 9.6, 7.2, 6.2, 11.1)
model <- lm(y ~ x)
r_squared <- summary(model)$r.squared
round(r_squared, 2)

########
# Determine the correlation coefficient between hours of training and defects per countertop
x <- c(2, 6, 3, 2, 7, 3, 3, 5, 1, 4)   # hours of training
y <- c(7, 2, 5, 6, 0, 3, 4, 4, 8, 2)   # defects
r <- cor(x, y)
round(r, 3)

#########
# Predicted Selling Price = 12.25 + 0.14*(Square Footage)
# Find predicted price, error, and squared error for observed price = 293.2 and square footage = 1956

b0 <- 12.25      # intercept (thousands of dollars)
b1 <- 0.14       # slope (increase in price per square foot)
x <- 1956        # square footage
y_obs <- 293.2   # observed selling price (thousands of dollars)

y_pred <- b0 + b1 * x        # predicted selling price
error <- y_obs - y_pred      # residual (observed - predicted)
sq_error <- error^2          # squared error

round(c(y_pred, error, sq_error), 2)

#####

# A sample of 1100 computer chips revealed that 64% fail in the first 1000 hours. The company claims 61% fail. Test at 0.01 level if the actual percentage is different from the stated percentage.

# --- Given Values ---
n       <- 1100   # sample size
p_hat   <- 0.64   # sample proportion
p0      <- 0.61   # claimed proportion (null)
alpha   <- 0.01   # significance level (two-tailed)

# --- Test Statistic ---
z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
cat("Test Statistic (z):", round(z, 4), "\n")

# --- Critical Value (two-tailed) ---
z_crit <- qnorm(1 - alpha / 2)
cat("Critical Value (±z):", round(z_crit, 4), "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence that p ≠ 0.61\n")
} else {
  cat("Decision: Fail to Reject H0 — Not sufficient evidence that p ≠ 0.61\n")
}

#####

# A manufacturer of banana chips wants to know if the bag filling machine works correctly at the 437g setting. It is believed the machine is underfilling. A 17 bag sample had a mean of 433g with a standard deviation of 22. Test at 0.05 significance level.

# --- Given Values ---
n       <- 17     # sample size
x_bar   <- 433    # sample mean
mu0     <- 437    # claimed mean (null)
s       <- 22     # sample standard deviation
alpha   <- 0.05   # significance level (left-tailed)

# --- Test Statistic ---
t <- (x_bar - mu0) / (s / sqrt(n))
cat("Test Statistic (t):", round(t, 4), "\n")

# --- Degrees of Freedom ---
df <- n - 1
cat("Degrees of Freedom:", df, "\n")

# --- Critical Value (left-tailed) ---
t_crit <- qt(alpha, df)
cat("Critical Value (-t):", round(t_crit, 4), "\n")

# --- P-Value (left-tailed) ---
p_value <- pt(t, df)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence that the bags are underfilled\n")
} else {
  cat("Decision: Fail to Reject H0 — Not sufficient evidence that the bags are underfilled\n")
}

#####

# An engineer designed a valve to produce a mean pressure of 4.5 psi. Tested on 170 engines with a sample mean of 4.7 psi. Population variance is 1.00. Test at 0.05 significance level if the valve does not perform to specifications. Find the P-value.

# --- Given Values ---
n       <- 170    # sample size
x_bar   <- 4.7   # sample mean
mu0     <- 4.5   # claimed mean (null)
sigma2  <- 1.00  # population variance (known)
sigma   <- sqrt(sigma2)  # population standard deviation
alpha   <- 0.05  # significance level (two-tailed)

# --- Test Statistic (z, since population variance is known) ---
z <- (x_bar - mu0) / (sigma / sqrt(n))
cat("Test Statistic (z):", round(z, 4), "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pnorm(abs(z), lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence that the valve does not perform to specifications\n")
} else {
  cat("Decision: Fail to Reject H0 — Not sufficient evidence that the valve does not perform to specifications\n")
}

#####

# The director of research and development is testing a new drug to know if there is evidence at the 0.02 level that the drug stays in the system for more than 339 minutes. She rejects the null hypothesis. What is the conclusion?

# --- Given Values ---
mu0     <- 339    # claimed mean (null)
alpha   <- 0.02   # significance level (right-tailed)

# --- Hypotheses ---
cat("H0: mu = 339\n")
cat("Ha: mu > 339 (right-tailed)\n\n")

# --- Decision & Conclusion ---
decision <- "Reject H0"
cat("Decision:", decision, "\n\n")

if (decision == "Reject H0") {
  cat("Conclusion: There is sufficient evidence at the 0.02 level of significance that the drug stays in the system for more than 339 minutes.\n")
} else {
  cat("Conclusion: There is not sufficient evidence at the 0.02 level of significance that the drug stays in the system for more than 339 minutes.\n")
}

#####

# High-power experimental engines are being developed by Stevens Motor Company with a calculated max HP of 630. A sample of 25 engines had a mean of 650 HP and standard deviation of 60 HP. Calculate a 99% confidence interval for the average maximum HP. Assume normal population.

# --- Given Values ---
n       <- 25     # sample size
x_bar   <- 650    # sample mean
s       <- 60     # sample standard deviation
alpha   <- 0.01   # significance level
conf    <- 1 - alpha  # confidence level = 0.99

# --- Degrees of Freedom ---
df <- n - 1
cat("Degrees of Freedom:", df, "\n")

# --- Critical Value (t, since population SD is unknown) ---
t_crit <- qt(1 - alpha / 2, df)
cat("Critical Value (t):", round(t_crit, 4), "\n")

# --- Margin of Error ---
me <- t_crit * (s / sqrt(n))
cat("Margin of Error:", round(me, 2), "\n")

# --- Confidence Interval ---
lower <- x_bar - me
upper <- x_bar + me
cat("99% Confidence Interval: (", round(lower, 2), ",", round(upper, 2), ")\n")

#####

# Same Stevens Motor Company engine problem. Step 2: Use the 99% confidence interval to determine if the average maximum HP is significantly different from the engineers' calculated value of 630 HP.

# --- Given Values ---
n       <- 25     # sample size
x_bar   <- 650    # sample mean
s       <- 60     # sample standard deviation
alpha   <- 0.01   # significance level
mu0     <- 630    # hypothesized mean (engineers' calculated value)

# --- Degrees of Freedom ---
df <- n - 1

# --- Critical Value ---
t_crit <- qt(1 - alpha / 2, df)

# --- Confidence Interval ---
me    <- t_crit * (s / sqrt(n))
lower <- x_bar - me
upper <- x_bar + me
cat("99% Confidence Interval: (", round(lower, 2), ",", round(upper, 2), ")\n")

# --- CI Approach Decision ---
cat("Hypothesized mean (mu0):", mu0, "\n")

if (mu0 < lower | mu0 > upper) {
  cat("mu0 =", mu0, "does NOT fall in the interval\n")
  cat("Decision: Reject H0 — Sufficient evidence that average max HP is different from 630\n")
} else {
  cat("mu0 =", mu0, "falls INSIDE the interval\n")
  cat("Decision: Fail to Reject H0 — Not sufficient evidence that average max HP is different from 630\n")
}

################ Chapter 12

######
# The manager of a city bus system is trying to assess commuter use of a particular bus line. He suspects that, on a weekday morning at 8 a.m., more passengers ride that line on the Northbound route than the Southbound route. The manager asks his Northbound driver and his Southbound driver to count how many passengers are on their 8 a.m. weekday routes for two weeks. The resulting data is shown below. Assume that the population standard deviation for passengers on the Northbound route is 3.2 and that the population standard deviation for passengers on the Southbound route is 3.9. Is there sufficient evidence at the 0.02 level of significance to say that, on a weekday morning at 8 a.m., more passengers ride the bus line on the Northbound route than the Southbound route? Assume that both populations are approximately normally distributed. Let passengers on the Northbound route be Population 1 and let passengers on the Southbound route be Population 2.
# Northbound	40	30	34	31	34	33	37	35	33	31
# Southbound	28	27	29	34	31	27	31	34	35	33
# The manager of a city bus system is testing whether the mean number of 8 a.m. weekday passengers is greater on the Northbound route than on the Southbound route using a two-sample z-test with known population standard deviations.

northbound <- c(40, 30, 34, 31, 34, 33, 37, 35, 33, 31)  # passenger counts on the Northbound route
southbound <- c(28, 27, 29, 34, 31, 27, 31, 34, 35, 33)  # passenger counts on the Southbound route

sigma1 <- 3.2  # population standard deviation for Northbound passengers
sigma2 <- 3.9  # population standard deviation for Southbound passengers

xbar1 <- mean(northbound)  # sample mean for Northbound
xbar2 <- mean(southbound)  # sample mean for Southbound

n1 <- length(northbound)  # sample size for Northbound
n2 <- length(southbound)  # sample size for Southbound

z <- (xbar1 - xbar2) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))  # two-sample z test statistic

round(z, 2)

# Step 2 of 5 : Compute the value of the test statistic. Round your answer to two decimal places.

xbar1 <- 85  # sample mean for smokers
xbar2 <- 83  # sample mean for non-smokers

sigma1 <- 8  # population standard deviation for smokers
sigma2 <- 5  # population standard deviation for non-smokers

n1 <- 73  # sample size for smokers
n2 <- 56  # sample size for non-smokers

z <- (xbar1 - xbar2) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))  # test statistic

round(z, 2)

#  Step 3 of 5 : Find the p-value associated with the test statistic. Round your answer to four decimal places.
# Compute the p-value for a two-tailed z-test

z <- 1.74  # test statistic from previous step

p_value <- 2 * (1 - pnorm(abs(z)))  # two-tailed p-value

round(p_value, 4)

# Step 4 Make decision for a two-tailed z-test at alpha = 0.05

z <- 1.74            # test statistic
alpha <- 0.05        # significance level
p_value <- 2 * (1 - pnorm(abs(z)))  # two-tailed p-value
if (p_value <= alpha) {
  print("Reject H0")
} else {
  print("Fail to reject H0")
}

#  Step 5 of 5 : State the conclusion of the hypothesis test.
if (p_value <= alpha) {
  print("Reject H0: There is sufficient evidence at the 0.05 level to conclude that the mean pulse rates of smokers and non-smokers are different.")
} else {
  print("Fail to reject H0: There is insufficient evidence at the 0.05 level to conclude that the mean pulse rates of smokers and non-smokers are different.")
}

#########
# Two friends, Karen and Jodi, work different shifts for the same ambulance service. They wonder if the different shifts average different numbers of calls. Looking at past records, Karen determines from a random sample of 32 shifts that she had a mean of 5.3 calls per shift. She knows that the population standard deviation for her shift is 1.1 calls. Jodi calculates from a random sample of 39 shifts that her mean was 4.6 calls per shift. She knows that the population standard deviation for her shift is 1.4 calls. Test the claim that there is a difference between the mean numbers of calls for the two shifts at the 0.05 level of significance. Let Karen's shifts be Population 1 and let Jodi's shifts be Population 2.
# Step 1 of 3 : State the null and alternative hypotheses for the test. Fill in the blank below. 
# not equal

#  Step 2 of 3 : Compute the value of the test statistic. Round your answer to two decimal places.
xbar1 <- 5.3  # mean calls for Karen (Population 1)
xbar2 <- 4.6  # mean calls for Jodi (Population 2)

sigma1 <- 1.1  # population standard deviation for Karen
sigma2 <- 1.4  # population standard deviation for Jodi

n1 <- 32  # sample size for Karen
n2 <- 39  # sample size for Jodi

z <- (xbar1 - xbar2) / sqrt((sigma1^2 / n1) + (sigma2^2 / n2))  # z test statistic

round(z, 2)

#  Step 3 of 3 : Draw a conclusion and interpret the decision.
z <- (5.3 - 4.6) / sqrt((1.1^2 / 32) + (1.4^2 / 39))  # test statistic
alpha <- 0.05
p_value <- 2 * (1 - pnorm(abs(z)))  # two-tailed p-value
if (p_value <= alpha) {
  print("Reject H0: There is sufficient evidence at the 0.05 level to conclude that the mean number of calls differs between Karen's and Jodi's shifts.")
} else {
  print("Fail to reject H0: There is insufficient evidence at the 0.05 level to conclude that the mean number of calls differs between Karen's and Jodi's shifts.")
}

#########
# A researcher compares the effectiveness of two different instructional methods for teaching anatomy.  A sample of 163 students using Method 1 produces a testing average of 73.8.  A sample of 179 students using Method 2 produces a testing average of 76.2.  Assume the standard deviation is known to be 9.17 for Method 1 and 15.25 for Method 2.  Determine the 95% confidence interval for the true difference between testing averages for students using Method 1 and students using Method 2.
# Step 1 of 2 :  Find the critical value that should be used in constructing the confidence interval.
alpha <- 0.05 # 95% is the CI, so alpha is .05
z_critical <- qnorm(1 - alpha/2)  # critical z-value
round(z_critical, 3)

#  Step 2 of 2 : Construct the 95% confidence interval. Round your answers to one decimal place.
xbar1 <- 73.8   # sample mean for Method 1
xbar2 <- 76.2   # sample mean for Method 2
sigma1 <- 9.17  # population standard deviation for Method 1
sigma2 <- 15.25 # population standard deviation for Method 2
n1 <- 163       # sample size for Method 1
n2 <- 179       # sample size for Method 2
alpha <- 0.05   # significance level for 95% confidence interval
z_crit <- qnorm(1 - alpha/2)  # critical z-value
SE <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))  # standard error of difference
diff_means <- xbar1 - xbar2  # difference in sample means
lower <- diff_means - z_crit * SE  # lower bound of confidence interval
upper <- diff_means + z_crit * SE  # upper bound of confidence interval
round(lower, 1)
round(upper, 1)

########
# Find the critical value for a 95% confidence interval (two-tailed z)
alpha <- 0.05  # significance level for 95% CI
z_crit <- qnorm(1 - alpha/2)  # critical z-value
round(z_crit, 2)

#  Step 2 of 2 : Construct the 95% confidence interval. Round your answers to two decimal places.
xbar1 <- 7.72   # sample mean age of cars owned by students
xbar2 <- 6.10   # sample mean age of cars owned by faculty

sigma1 <- 2.73  # population standard deviation for students
sigma2 <- 3.80  # population standard deviation for faculty

n1 <- 59        # sample size for students
n2 <- 49        # sample size for faculty

alpha <- 0.05   # significance level for 95% CI

z_crit <- qnorm(1 - alpha/2)  # critical z-value

SE <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))  # standard error

diff_means <- xbar1 - xbar2  # difference in sample means

lower <- diff_means - z_crit * SE  # lower bound
upper <- diff_means + z_crit * SE  # upper bound

round(lower, 2)
round(upper, 2)


# ============================================================
#  12.1 TEMPLATE — Two-Sample Z-Test & Confidence Interval
#  Plug in values below. Everything else runs automatically.
# ============================================================


# ── INPUTS: PLUG IN YOUR VALUES HERE ────────────────────────

xbar1  <- 33.2   # sample mean, Population 1
xbar2  <- 30.9   # sample mean, Population 2

sigma1 <- 3.2    # known population std dev, Population 1
sigma2 <- 3.9    # known population std dev, Population 2

n1     <- 10     # sample size, Population 1
n2     <- 10     # sample size, Population 2

alpha  <- 0.02   # significance level (e.g., 0.05, 0.02, 0.01)

# Tail type: "right" (μ1 > μ2), "left" (μ1 < μ2), "two" (μ1 ≠ μ2)
tail   <- "right"

# Confidence interval? TRUE or FALSE
run_CI <- TRUE
CI_level <- 0.95   # e.g., 0.95 for 95% CI (only used if run_CI = TRUE)


# ── HYPOTHESIS TEST ──────────────────────────────────────────

SE <- sqrt((sigma1^2 / n1) + (sigma2^2 / n2))   # standard error
z  <- (xbar1 - xbar2) / SE                       # test statistic

# p-value based on tail direction
p_value <- switch(tail,
                  right = 1 - pnorm(z),
                  left  = pnorm(z),
                  two   = 2 * (1 - pnorm(abs(z)))
)

# Critical value
z_crit <- switch(tail,
                 right = qnorm(1 - alpha),
                 left  = qnorm(alpha),
                 two   = qnorm(1 - alpha / 2)
)

# Decision
decision <- if (p_value <= alpha) "Reject H0" else "Fail to Reject H0"


# ── CONFIDENCE INTERVAL (optional) ───────────────────────────

if (run_CI) {
  z_ci    <- qnorm(1 - (1 - CI_level) / 2)
  lower   <- (xbar1 - xbar2) - z_ci * SE
  upper   <- (xbar1 - xbar2) + z_ci * SE
}


# ── OUTPUT ───────────────────────────────────────────────────

cat("============================================================\n")
cat(" 12.1 — Two-Sample Z-Test Results\n")
cat("============================================================\n\n")

cat("--- Hypotheses ---\n")
cat(switch(tail,
           right = "H0: μ1 = μ2    Ha: μ1 > μ2  (right-tailed)\n",
           left  = "H0: μ1 = μ2    Ha: μ1 < μ2  (left-tailed)\n",
           two   = "H0: μ1 = μ2    Ha: μ1 ≠ μ2  (two-tailed)\n"
))

cat("\n--- Sample Info ---\n")
cat(sprintf("  x̄1 = %.4f    x̄2 = %.4f    Difference = %.4f\n",
            xbar1, xbar2, xbar1 - xbar2))
cat(sprintf("  σ1 = %.4f    σ2 = %.4f\n", sigma1, sigma2))
cat(sprintf("  n1 = %d        n2 = %d\n", n1, n2))
cat(sprintf("  SE = %.4f\n", SE))

cat("\n--- Test Results ---\n")
cat(sprintf("  z  = %.2f\n", round(z, 2)))
cat(sprintf("  z* = %.2f  (critical value, α = %.3f)\n", round(z_crit, 2), alpha))
cat(sprintf("  p  = %.4f\n", round(p_value, 4)))
cat(sprintf("  α  = %.4f\n", alpha))

cat("\n--- Decision ---\n")
cat(sprintf("  p-value (%s) %s α (%s)  →  %s\n",
            round(p_value, 4),
            if (p_value <= alpha) "<=" else ">",
            alpha,
            decision))

cat("\n--- Conclusion ---\n")
if (decision == "Reject H0") {
  cat(sprintf("  There IS sufficient evidence at the %.2f level to support Ha.\n", alpha))
} else {
  cat(sprintf("  There is NOT sufficient evidence at the %.2f level to support Ha.\n", alpha))
}

if (run_CI) {
  cat(sprintf("\n--- %d%% Confidence Interval for (μ1 - μ2) ---\n",
              round(CI_level * 100)))
  cat(sprintf("  z* = %.3f\n", round(z_ci, 3)))
  cat(sprintf("  ( %.2f ,  %.2f )\n", round(lower, 2), round(upper, 2)))
}

cat("\n============================================================\n")

########## exam 10
college_gpa  <- c(2.00, 3.45, 3.38, 3.59, 2.90, 3.45)
hs_gpa       <- c(3.49, 3.77, 4.64, 2.23, 3.45, 3.75)

y_hat <- 3.26 + (-0.0361) * hs_gpa

residuals <- college_gpa - y_hat

SSE <- sum(residuals^2)
round(SSE, 4)

##########
# Data
x <- c(30.4, 33.8, 39.0, 34.8, 28.5, 37.7, 27.4, 32.8, 30.5)
y <- c(74.8, 64.7, 54.4, 66.2, 79.2, 59.8, 78.6, 66.0, 74.9)

# Fit model
model <- lm(y ~ x)
round(coef(model), 4)

round

##### 
x <- c(30.4, 33.8, 39.0, 34.8, 28.5, 37.7, 27.4, 32.8, 30.5)
y <- c(74.8, 64.7, 54.4, 66.2, 79.2, 59.8, 78.6, 66.0, 74.9)

model <- lm(y ~ x)
summary(model)$r.squared
r2 <- cor(x, y)^2
round(r2 * 100, 2)

########
# Given values
observed <- 211.9
sq_ft <- 1008

# Step 1: Predicted value
predicted <- 37.44 + 0.16 * sq_ft

# Step 2: Error = Observed - Predicted
error <- observed - predicted

# Step 3: Squared Error
squared_error <- error^2

# Output rounded to 2 decimal places
round(predicted, 2)
round(error, 2)
round(squared_error, 2)

##########
# Data
time <- 1:12
sea_level <- c(0.059, 0.064, 0.083, 0.118, 0.120, 0.134,
               0.155, 0.157, 0.167, 0.170, 0.188, 0.222)
nox       <- c(26880, 26966, 22705, 20007, 14565, 10810,
               26914, 26684, 24717, 25069,  7632,  8329)
fb_users  <- c(2503, 2443, 2436, 2392, 2190, 2232,
               2304, 2325, 2117, 2085, 2038, 1975)

# 1×3 panel plot
par(mfrow = c(1, 3), mar = c(4, 4, 3, 1))

plot(time, sea_level, type = "b", pch = 16, col = "steelblue",
     xlab = "Time Period", ylab = "Meters",
     main = "Mean Sea Level")
abline(lm(sea_level ~ time), col = "red", lty = 2)

plot(time, nox, type = "b", pch = 16, col = "darkorange",
     xlab = "Time Period", ylab = "1000 Tons",
     main = "Air Pollution NOx")
abline(lm(nox ~ time), col = "red", lty = 2)

plot(time, fb_users, type = "b", pch = 16, col = "darkgreen",
     xlab = "Time Period", ylab = "Millions",
     main = "Active Facebook Users")
abline(lm(fb_users ~ time), col = "red", lty = 2)

par(mfrow = c(1, 1))

t <- 1:12
y <- c(0.059, 0.064, 0.083, 0.118, 0.12, 0.134, 0.155, 0.157, 0.167, 0.17, 0.188, 0.222)

model <- lm(y ~ t)
round(coef(model), 3)

######### chapter 12.2

# A pharmaceutical company needs to know if its new cholesterol drug, Praxor, is effective at lowering cholesterol levels. It believes that people who take Praxor will average a greater decrease in cholesterol level than people taking a placebo. After the experiment is complete, the researchers find that the 48 participants in the treatment group lowered their cholesterol levels by a mean of 21.5 points with a standard deviation of 2.5 points. The 40 participants in the control group lowered their cholesterol levels by a mean of 20.9 points with a standard deviation of 4.1 points. Assume that the population variances are not equal and test the company’s claim at the 0.10 level. Let the treatment group be Population 1 and let the control group be Population 2.
# Step 1 of 3 :  State the null and alternative hypotheses for the test. Fill in the blank below. 
# >

# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 48    # sample size for treatment group
xbar1   <- 21.5  # sample mean decrease for treatment group
s1      <- 2.5   # sample standard deviation for treatment group

n2      <- 40    # sample size for control group
xbar2   <- 20.9  # sample mean decrease for control group
s2      <- 4.1   # sample standard deviation for control group

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 3), "\n")

#  Step 3 of 3 : Draw a conclusion and interpret the decision.

# --- Degrees of Freedom (Welch-Satterthwaite) ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- P-Value (right-tailed) ---
p_value <- pt(t, df, lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.10 level to support the claim that people who take Praxor average a greater decrease in cholesterol level than people taking a placebo.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.10 level to support the claim that people who take Praxor average a greater decrease in cholesterol level than people taking a placebo.\n")
}

#########
# A manufacturer fills soda bottles. Periodically the company tests to see if there is a difference between the mean amounts of soda put in bottles of regular cola and diet cola. A random sample of 13 bottles of regular cola has a mean of 500.1 mL of soda with a standard deviation of 2.6 mL. A random sample of 18 bottles of diet cola has a mean of 497.4 mL of soda with a standard deviation of 3.1 mL. Test the claim that there is a difference between the mean fill levels for the two types of soda using a 0.05 level of significance. Assume that both populations are approximately normal and that the population variances are not equal. Let bottles of regular cola be Population 1 and let bottles of diet cola be Population 2.
# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 13     # sample size for regular cola
xbar1   <- 500.1  # sample mean for regular cola
s1      <- 2.6    # sample standard deviation for regular cola

n2      <- 18     # sample size for diet cola
xbar2   <- 497.4  # sample mean for diet cola
s2      <- 3.1    # sample standard deviation for diet cola

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 3), "\n")


# --- Given Values ---
n1      <- 13     # sample size for regular cola
xbar1   <- 500.1  # sample mean for regular cola
s1      <- 2.6    # sample standard deviation for regular cola

n2      <- 18     # sample size for diet cola
xbar2   <- 497.4  # sample mean for diet cola
s2      <- 3.1    # sample standard deviation for diet cola

alpha   <- 0.05   # significance level (two-tailed)

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 4), "\n")

# --- Degrees of Freedom ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pt(-abs(t), df)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.05 level to conclude there is a difference between the mean fill amounts.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.05 level to conclude there is a difference between the mean fill amounts.\n")
}

# Step 3 of 3: Draw a conclusion and interpret the decision.

# --- Given Values ---
n1      <- 13     # sample size for regular cola
xbar1   <- 500.1  # sample mean for regular cola
s1      <- 2.6    # sample standard deviation for regular cola

n2      <- 18     # sample size for diet cola
xbar2   <- 497.4  # sample mean for diet cola
s2      <- 3.1    # sample standard deviation for diet cola

alpha   <- 0.05   # significance level (two-tailed)

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 4), "\n")

# --- Degrees of Freedom ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pt(-abs(t), df)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.05 level to conclude there is a difference between the mean fill amounts.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.05 level to conclude there is a difference between the mean fill amounts.\n")
}

########
# An internal auditor for Tiger Enterprises has been asked to determine if there is a difference in the average amount charged for daily expenses by two top salespeople, Mrs. Ellis and Mr. Ford. The auditor randomly selects 43 days and determines the daily expenses for each of the salespeople. Let Population 1 be the daily expenses of Mrs. Ellis and Population 2 be the daily expenses of Mr. Ford.
# Step 1 of 2: Calculate a 99% confidence interval for the difference in the average amounts charged for daily expenses between Mrs. Ellis and Mr. Ford, assuming that the population standard deviations are not equal.

# --- Given Values ---
n1      <- 43    # sample size for Mrs. Ellis
xbar1   <- 54    # sample mean for Mrs. Ellis
s1      <- 6     # sample standard deviation for Mrs. Ellis

n2      <- 43    # sample size for Mr. Ford
xbar2   <- 61    # sample mean for Mr. Ford
s2      <- 3     # sample standard deviation for Mr. Ford

alpha   <- 0.01  # significance level for 99% CI

# --- Difference in Means ---
diff <- xbar1 - xbar2
cat("Difference in Means (xbar1 - xbar2):", diff, "\n")

# --- Standard Error ---
se <- sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Standard Error:", round(se, 4), "\n")

# --- Degrees of Freedom (Welch) ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- Critical Value ---
t_crit <- qt(1 - alpha/2, df)
cat("Critical Value (t):", round(t_crit, 4), "\n")

# --- Margin of Error ---
me <- t_crit * se
cat("Margin of Error:", round(me, 2), "\n")

# --- Confidence Interval ---
lower <- diff - me
upper <- diff + me

cat("99% Confidence Interval: (", round(lower, 2), ",", round(upper, 2), ")\n")

# Step 2 of 2: Interpret the confidence interval obtained in Step 1.

# --- Given Values ---
lower <- -9.72   # lower endpoint of CI (from Step 1)
upper <- -4.28   # upper endpoint of CI (from Step 1)

# --- Interpretation ---
if (lower > 0 & upper > 0) {
  cat("Conclusion: 99% CI is entirely above 0 → mu1 > mu2 (Mrs. Ellis spends more)\n")
} else if (lower < 0 & upper < 0) {
  cat("Conclusion: 99% CI is entirely below 0 → mu1 < mu2 (Mrs. Ellis spends less)\n")
  cat("Correct Choice: We are 99% confident that the average amount charged for daily expenses is between $4.28 and $9.72 smaller for Mrs. Ellis compared to Mr. Ford.\n")
} else {
  cat("Conclusion: CI contains 0 → No significant difference\n")
}

###########
# A school system is concerned about how often they must replace the kick balls for the elementary school gymnasiums. They are interested in switching to either GoFit kick balls or Sports Factor kick balls. The school purchaser decided to select the brand of ball that would bounce the highest when dropped from 3 feet. The data below was collected by dropping either a GoFit kick ball or a Sports Factor kick ball from 3 feet and measuring the bounce in inches. Each brand’s bounce was measured 10 times. One of the gym teachers in the school system claims that the GoFit kick balls will bounce higher. Test that claim using a 0.05 level of significance. Let GoFit kick balls be Population 1 and let Sports Factor kick balls be Population 2. Assume that both populations are approximately normal and that the population variances are not equal since they are different brands.
# Step 1 of 3: State the null and alternative hypotheses for the test.

# --- Given Values ---
claim <- "GoFit kick balls will bounce higher"   # claim in words
pop1  <- "GoFit"                                 # Population 1
pop2  <- "Sports Factor"                         # Population 2

# --- Hypotheses ---
cat("H0: mu1 = mu2\n")
cat("Ha: mu1 > mu2\n")

# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
gofit <- c(29, 31, 28, 32, 30, 29, 32, 28, 32, 32)   # GoFit bounce heights
sports <- c(29, 31, 28, 30, 29, 30, 32, 29, 30, 28)  # Sports Factor bounce heights

# --- Sample Statistics ---
n1 <- length(gofit)
n2 <- length(sports)

xbar1 <- mean(gofit)
xbar2 <- mean(sports)

s1 <- sd(gofit)
s2 <- sd(sports)

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 3), "\n")

# Step 3 of 3: Draw a conclusion and interpret the decision.

# --- Degrees of Freedom (Welch-Satterthwaite) ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- P-Value (right-tailed) ---
p_value <- pt(t, df, lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.05 level of significance to support the teacher's claim that GoFit kick balls bounce higher.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.05 level of significance to support the teacher's claim that GoFit kick balls bounce higher.\n")
}

#########
# A professor is concerned that the two sections of college algebra that he teaches are not performing at the same level. To test his claim, he looks at the mean exam score for a random sample of students from each of his classes. In Class 1, the mean exam score for 17 students is 81.8 with a standard deviation of 6.4. In Class 2, the mean exam score for 12 students is 77.9 with a standard deviation of 5.1. Test the professor's claim at the 0.01 level of significance. Assume that both populations are approximately normal and that the population variances are equal. Let Class 1 be Population 1 and let Class 2 be Population 2.
# Step 1 of 3: State the null and alternative hypotheses for the test.

# --- Given Values ---
claim <- "the two sections are not performing at the same level"   # claim in words
pop1  <- "Class 1"                                                 # Population 1
pop2  <- "Class 2"                                                 # Population 2

# --- Hypotheses ---
cat("H0: mu1 = mu2\n")
cat("Ha: mu1 != mu2\n")
cat("Correct Choice: !=\n")

# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 17     # sample size for Class 1
xbar1   <- 81.8   # sample mean for Class 1
s1      <- 6.4    # sample standard deviation for Class 1

n2      <- 12     # sample size for Class 2
xbar2   <- 77.9   # sample mean for Class 2
s2      <- 5.1    # sample standard deviation for Class 2

# --- Pooled Standard Deviation ---
sp <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))
cat("Pooled Standard Deviation (sp):", round(sp, 4), "\n")

# --- Test Statistic (pooled t-test) ---
t <- (xbar1 - xbar2) / (sp * sqrt(1/n1 + 1/n2))
cat("Test Statistic (t):", round(t, 3), "\n")

# Step 3 of 3: Draw a conclusion and interpret the decision.

# --- Degrees of Freedom ---
df <- n1 + n2 - 2
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pt(-abs(t), df)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.01 level of significance to say that the mean exam scores for the two classes are different.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.01 level of significance to say that the mean exam scores for the two classes are different.\n")
}

##########
# The State Environmental Board wants to compare pollution levels in two of its major cities. Sunshine City thrives on the tourist industry and Service City thrives on the service industry. The environmental board randomly selects several areas within the cities and measures the pollution levels in parts per million with the following results. Assume that the population variances are not equal and that the assumptions of normality have been satisfied. Let Population 1 be Sunshine City and Population 2 be Service City.
# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 13     # sample size for Sunshine City
xbar1   <- 8.2    # sample mean for Sunshine City
s1      <- 0.55   # sample standard deviation for Sunshine City

n2      <- 14     # sample size for Service City
xbar2   <- 7.9    # sample mean for Service City
s2      <- 0.46   # sample standard deviation for Service City

# --- Test Statistic (Welch's t) ---
t <- (xbar1 - xbar2) / sqrt((s1^2 / n1) + (s2^2 / n2))
cat("Test Statistic (t):", round(t, 3), "\n")

# Step 3 of 3: Draw a conclusion and interpret the decision.
# --- Degrees of Freedom (Welch-Satterthwaite) ---
df <- ((s1^2 / n1 + s2^2 / n2)^2) / (((s1^2 / n1)^2 / (n1 - 1)) + ((s2^2 / n2)^2 / (n2 - 1)))
cat("Degrees of Freedom:", round(df, 4), "\n")

# --- P-Value (right-tailed, since mu1 > mu2) ---
p_value <- pt(t, df, lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at a 0.05 level of significance to say that Service City has a lower pollution level on average than Sunshine City.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at a 0.05 level of significance to say that Service City has a lower pollution level on average than Sunshine City.\n")
}

#########
# A new small business wants to know if its current radio advertising is effective. The owners decide to look at the mean number of customers who make a purchase in the store on days immediately following days when the radio ads are played as compared to the mean for those days following days when no radio advertisements are played. They found that for 6 days following no advertisements, the mean was 16.2 purchasing customers with a standard deviation of 1.6 customers. On 15 days following advertising, the mean was 17.5 purchasing customers with a standard deviation of 1.2 customers. Test the claim, at the 0.05 level, that the mean number of customers who make a purchase in the store is lower for days following no advertising compared to days following advertising. Assume that both populations are approximately normal and that the population variances are equal. Let days following no advertisements be Population 1 and let days following advertising be Population 2.
# Step 1 of 3: State the null and alternative hypotheses for the test.

# --- Given Values ---
claim <- "the mean number of customers is lower for days following no advertising compared to days following advertising"  # claim in words
pop1  <- "No advertising"   # Population 1
pop2  <- "Advertising"      # Population 2

# --- Hypotheses ---
cat("H0: mu1 = mu2\n")
cat("Ha: mu1 < mu2\n")
cat("Correct Choice: <\n")

# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 6      # sample size (no advertising)
xbar1   <- 16.2   # sample mean (no advertising)
s1      <- 1.6    # sample standard deviation (no advertising)

n2      <- 15     # sample size (advertising)
xbar2   <- 17.5   # sample mean (advertising)
s2      <- 1.2    # sample standard deviation (advertising)

# --- Pooled Standard Deviation ---
sp <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))
cat("Pooled Standard Deviation (sp):", round(sp, 4), "\n")

# --- Test Statistic ---
t <- (xbar1 - xbar2) / (sp * sqrt(1/n1 + 1/n2))
cat("Test Statistic (t):", round(t, 3), "\n")
# Step 3 of 3: Draw a conclusion and interpret the decision.

# --- Given Values ---
n1      <- 6
xbar1   <- 16.2
s1      <- 1.6

n2      <- 15
xbar2   <- 17.5
s2      <- 1.2

alpha   <- 0.05   # left-tailed test (mu1 < mu2)

# --- Pooled Standard Deviation ---
sp <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))
cat("Pooled Standard Deviation (sp):", round(sp, 4), "\n")

# --- Test Statistic ---
t <- (xbar1 - xbar2) / (sp * sqrt(1/n1 + 1/n2))
cat("Test Statistic (t):", round(t, 4), "\n")

# --- Degrees of Freedom ---
df <- n1 + n2 - 2
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (left-tailed) ---
p_value <- pt(t, df)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — Sufficient evidence at the 0.05 level to support the claim that the mean number of customers is lower for days following no advertising.\n")
} else {
  cat("Decision: Fail to Reject H0 — Insufficient evidence at the 0.05 level to support the claim that the mean number of customers is lower for days following no advertising.\n")
}

#########
# A Hollywood studio believes that a movie that is considered a drama will draw a larger crowd on average than a movie that is considered a comedy. To test this theory, the studio randomly selects several movies that are classified as dramas and several movies that are classified as comedies and determines the box office revenue for each movie. The results of the survey are as follows. Do the data substantiate the studio's belief that dramas will draw a larger crowd on average than comedies at alpha = 0.01? Let dramas be Population 1 and comedies be Population 2. Assume that the population variances are approximately equal.
# Step 1 of 3: State the null and alternative hypotheses for the test.

# --- Given Values ---
claim <- "dramas will draw a larger crowd on average than comedies"   # claim in words
pop1  <- "Drama"                                                      # Population 1
pop2  <- "Comedy"                                                     # Population 2

# --- Hypotheses ---
cat("H0: mu1 - mu2 = 0\n")
cat("Ha: mu1 - mu2 > 0\n")
cat("Correct Choice: >\n")

# Step 2 of 3: Compute the value of the test statistic.

# --- Given Values ---
n1      <- 14     # sample size for Drama
xbar1   <- 150    # sample mean for Drama
s1      <- 50     # sample standard deviation for Drama

n2      <- 16     # sample size for Comedy
xbar2   <- 120    # sample mean for Comedy
s2      <- 20     # sample standard deviation for Comedy

# --- Pooled Standard Deviation ---
sp <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))
cat("Pooled Standard Deviation (sp):", round(sp, 4), "\n")

# --- Test Statistic ---
t <- (xbar1 - xbar2) / (sp * sqrt(1/n1 + 1/n2))
cat("Test Statistic (t):", round(t, 3), "\n")

# Step 3 of 3: Make the decision and state the conclusion in terms of the original question.

# --- Given Values ---
n1      <- 14     # sample size for Drama
xbar1   <- 150    # sample mean for Drama
s1      <- 50     # sample standard deviation for Drama

n2      <- 16     # sample size for Comedy
xbar2   <- 120    # sample mean for Comedy
s2      <- 20     # sample standard deviation for Comedy

alpha   <- 0.01   # significance level (right-tailed)

# --- Pooled Standard Deviation ---
sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
cat("Pooled Standard Deviation (sp):", round(sp, 4), "\n")

# --- Test Statistic ---
t <- (xbar1 - xbar2) / (sp * sqrt(1/n1 + 1/n2))
cat("Test Statistic (t):", round(t, 4), "\n")

# --- Degrees of Freedom ---
df <- n1 + n2 - 2
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (right-tailed) ---
p_value <- pt(t, df, lower.tail = FALSE)
cat("P-Value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0 — There is sufficient evidence at alpha = 0.01 to say that dramas will draw a larger crowd on average than comedies.\n")
} else {
  cat("Decision: Fail to Reject H0 — There is insufficient evidence at alpha = 0.01 to say that dramas will draw a larger crowd on average than comedies.\n")
}

######### chapter 12.3
# Students at a major university believe they can save money buying textbooks online rather than at the local bookstores. In order to test this theory, they randomly sampled 25 textbooks on the shelves of the local bookstores. The students then found the "best" available price for the same textbooks via online retailers. Based on the data, is it less expensive for the students to purchase textbooks from the online retailers than from local bookstores? Use alpha = 0.10. Let prices at local bookstores represent Population 1 and prices at online retailers represent Population 2.

# --- Given Values ---

bookstore <- c(91, 102, 91, 62, 86, 124, 96, 98, 64, 91, 134, 129, 149,
               121, 98, 98, 135, 143, 110, 70, 116, 84, 137, 60, 127)   # local bookstore prices

online <- c(102, 86, 105, 59, 78, 123, 82, 107, 43, 81, 120, 121, 148,
            117, 86, 95, 138, 134, 85, 79, 96, 95, 146, 68, 104)        # online retailer prices

alpha <- 0.10   # significance level

# Since the same textbooks were compared at both locations, this is a paired t-test.
# Let d = bookstore - online
# Claim: online is cheaper than bookstore  <=>  bookstore - online > 0

# --- Step 1: Hypotheses ---
cat("H0: mu_d = 0\n")
cat("Ha: mu_d > 0\n\n")

# --- Step 2: Test Statistic, P-Value, Decision ---
test_result <- t.test(bookstore, online,
                      paired = TRUE,
                      alternative = "greater",
                      conf.level = 1 - alpha)

t_stat <- unname(test_result$statistic)   # t test statistic
df <- unname(test_result$parameter)       # degrees of freedom
p_value <- test_result$p.value            # p-value

cat("Test Statistic (t):", round(t_stat, 4), "\n")
cat("Degrees of Freedom:", df, "\n")
cat("P-Value:", round(p_value, 4), "\n\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.10 level to support the claim that online retailers are less expensive than local bookstores.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.10 level to support the claim that online retailers are less expensive than local bookstores.\n")
}

# --- Optional: Sample Mean Difference ---
d <- bookstore - online
cat("\nMean of paired differences (bookstore - online):", round(mean(d), 4), "\n")

#######################
# An SAT prep course claims to increase student scores by more than 60 points, on average. To test this claim, 9 students who have previously taken the SAT are randomly chosen to take the prep course. Their SAT scores before and after completing the prep course are listed. Test the claim at the 0.01 level of significance assuming the population distribution of the paired differences is approximately normal. Let d = (after scores) - (before scores).

# --- Given Values ---

before <- c(1270, 1420, 1300, 1170, 1150, 1230, 1220, 1180, 1280)  # scores before prep
after  <- c(1480, 1540, 1420, 1200, 1150, 1310, 1310, 1370, 1560)  # scores after prep

alpha <- 0.01   # significance level
mu0 <- 60       # hypothesized mean difference

# --- Step 1: Hypotheses ---
cat("H0: mu_d = 60\n")
cat("Ha: mu_d > 60\n\n")

# --- Step 2: Compute Differences ---
d <- after - before
n <- length(d)
d_bar <- mean(d)
s_d <- sd(d)

# --- Test Statistic ---
t_stat <- (d_bar - mu0) / (s_d / sqrt(n))
df <- n - 1

cat("Mean Difference (d_bar):", round(d_bar, 4), "\n")
cat("Std Dev of Differences (s_d):", round(s_d, 4), "\n")
cat("Test Statistic (t):", round(t_stat, 4), "\n")
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (right-tailed) ---
p_value <- 1 - pt(t_stat, df)
cat("P-Value:", round(p_value, 4), "\n\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.01 level to support the claim that the mean increase is more than 60 points.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.01 level to support the claim that the mean increase is more than 60 points.\n")
}

#####################################
# To test the effect of a physical fitness course, the number of sit-ups a person could do in one minute before and after the course was recorded for 10 individuals. Let d = x2 - x1 (after - before). Step 1: Find the mean of the paired differences.

# --- Given Values ---

before <- c(30, 36, 22, 56, 28, 23, 33, 33, 29, 28)   # sit-ups before course
after  <- c(42, 43, 30, 58, 44, 32, 44, 47, 47, 43)   # sit-ups after course

# --- Step 1: Compute Paired Differences ---
d <- after - before   # differences (after - before)

# --- Mean of Differences ---
d_bar <- mean(d)

cat("Paired Differences:", d, "\n")
cat("Mean of Paired Differences (d_bar):", round(d_bar, 1), "\n")

# Step 2: Construct the 95% confidence interval for the true mean difference in sit-ups (d = after - before)

# --- Given Values ---

before <- c(30, 36, 22, 56, 28, 23, 33, 33, 29, 28)   # sit-ups before course
after  <- c(42, 43, 30, 58, 44, 32, 44, 47, 47, 43)   # sit-ups after course

alpha <- 0.05   # for 95% confidence

# --- Paired Differences ---
d <- after - before
n <- length(d)
d_bar <- mean(d)
s_d <- sd(d)

# --- Critical Value (t*) ---
df <- n - 1
t_star <- qt(1 - alpha/2, df)

# --- Margin of Error ---
ME <- t_star * (s_d / sqrt(n))

# --- Confidence Interval ---
lower <- d_bar - ME
upper <- d_bar + ME

cat("Mean Difference (d_bar):", round(d_bar, 1), "\n")
cat("Standard Deviation (s_d):", round(s_d, 4), "\n")
cat("t* Critical Value:", round(t_star, 4), "\n")
cat("Margin of Error:", round(ME, 4), "\n\n")

cat("95% Confidence Interval: (",
    round(lower, 1), ",",
    round(upper, 1), ")\n")

##############################
# The manufacturer of a new eye cream claims that the cream reduces the appearance of fine lines and wrinkles after 14 days. Let d = (after - before). Step 1: State hypotheses and perform paired t-test at alpha = 0.05.

# --- Given Values ---

before <- c(11, 11, 13, 15, 11, 13, 15, 14, 10, 15)   # number of lines before treatment
after  <- c(9, 12, 11, 14, 7, 12, 13, 13, 10, 15)    # number of lines after treatment

alpha <- 0.05

# --- Step 1: Hypotheses ---
# Claim: cream reduces wrinkles ⇒ after < before ⇒ d = after - before < 0
cat("H0: mu_d = 0\n")
cat("Ha: mu_d < 0\n\n")

# --- Step 2: Paired Differences ---
d <- after - before
n <- length(d)
d_bar <- mean(d)
s_d <- sd(d)

# --- Test Statistic ---
t_stat <- d_bar / (s_d / sqrt(n))
df <- n - 1

cat("Mean Difference (d_bar):", round(d_bar, 4), "\n")
cat("Std Dev (s_d):", round(s_d, 4), "\n")
cat("Test Statistic (t):", round(t_stat, 4), "\n")
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (left-tailed) ---
p_value <- pt(t_stat, df)
cat("P-Value:", round(p_value, 4), "\n\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.05 level to support the claim that the cream reduces fine lines and wrinkles.\n")
} else {
  cat("Decision: Fail to Reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.05 level to support the claim that the cream reduces fine lines and wrinkles.\n")
}

#################
# A sleep disorder specialist records hours of sleep with and without a new drug for 9 patients. Let d = (with drug) - (without drug). Step 1: Find the mean of the paired differences.

# --- Given Values ---

without_drug <- c(5.4, 2.4, 5.8, 1.9, 3.1, 5.7, 3.4, 7.6, 2.5)  # hours without drug
with_drug    <- c(7.3, 4.8, 8.3, 4.6, 4.6, 7.0, 5.9, 8.4, 5.2)  # hours with drug

# --- Step 1: Compute Paired Differences ---
d <- with_drug - without_drug   # differences

# --- Mean of Differences ---
d_bar <- mean(d)

cat("Paired Differences:", d, "\n")
cat("Mean of Paired Differences (d_bar):", round(d_bar, 2), "\n")

# Step 2: Construct the 90% confidence interval for the true mean difference in hours of sleep (d = with drug - without drug)

# --- Given Values ---

without_drug <- c(5.4, 2.4, 5.8, 1.9, 3.1, 5.7, 3.4, 7.6, 2.5)  # hours without drug
with_drug    <- c(7.3, 4.8, 8.3, 4.6, 4.6, 7.0, 5.9, 8.4, 5.2)  # hours with drug

alpha <- 0.10   # for 90% confidence

# --- Paired Differences ---
d <- with_drug - without_drug
n <- length(d)
d_bar <- mean(d)
s_d <- sd(d)

# --- Critical Value (t*) ---
df <- n - 1
t_star <- qt(1 - alpha/2, df)

# --- Margin of Error ---
ME <- t_star * (s_d / sqrt(n))

# --- Confidence Interval ---
lower <- d_bar - ME
upper <- d_bar + ME

cat("Mean Difference (d_bar):", round(d_bar, 2), "\n")
cat("Standard Deviation (s_d):", round(s_d, 4), "\n")
cat("t* Critical Value:", round(t_star, 4), "\n")
cat("Margin of Error:", round(ME, 4), "\n\n")

cat("90% Confidence Interval: (",
    round(lower, 2), ",",
    round(upper, 2), ")\n")

##############
# An auto dealer tests whether there is a difference in braking distances between Model A and Model B using the same 6 drivers. Step 1: State hypotheses and perform paired t-test at alpha = 0.05.

# --- Given Values ---

model_A <- c(151, 153, 156, 147, 145, 147)  # braking distances for Model A
model_B <- c(151, 153, 158, 150, 148, 148)  # braking distances for Model B

alpha <- 0.05

# --- Step 1: Hypotheses ---
# Let d = A - B
# Two-tailed test for difference
cat("H0: mu_d = 0\n")
cat("Ha: mu_d != 0\n\n")

# --- Step 2: Paired Differences ---
d <- model_A - model_B
n <- length(d)
d_bar <- mean(d)
s_d <- sd(d)

# --- Test Statistic ---
t_stat <- d_bar / (s_d / sqrt(n))
df <- n - 1

cat("Mean Difference (d_bar):", round(d_bar, 4), "\n")
cat("Std Dev (s_d):", round(s_d, 4), "\n")
cat("Test Statistic (t):", round(t_stat, 4), "\n")
cat("Degrees of Freedom:", df, "\n")

# --- P-Value (two-tailed) ---
p_value <- 2 * pt(-abs(t_stat), df)
cat("P-Value:", round(p_value, 4), "\n\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.05 level to conclude that there is a difference in braking distances between the two models.\n")
} else {
  cat("Decision: Fail to Reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.05 level to conclude that there is a difference in braking distances between the two models.\n")
}

#################### chapter 12.4
# A fundraiser believes that people on the west side of town are more likely to say “Yes” when asked to donate than people on the east side.
# 180 people from the west side with 18 “Yes” responses.
# 150 people from the east side with 12 “Yes” responses.
# Construct a 90% confidence interval for the difference in proportions (p1 - p2).

# --- Given Values ---
n1 <- 180   # sample size (west)
x1 <- 18    # number of "Yes" (west)

n2 <- 150   # sample size (east)
x2 <- 12    # number of "Yes" (east)

alpha <- 0.10  # significance level for 90% CI

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

cat("Sample proportion (West):", round(p1_hat, 4), "\n")
cat("Sample proportion (East):", round(p2_hat, 4), "\n")

# --- Critical Value (z*) ---
z_star <- qnorm(1 - alpha/2)
cat("Critical z* value:", round(z_star, 4), "\n")

# --- Standard Error ---
SE <- sqrt((p1_hat * (1 - p1_hat)) / n1 + (p2_hat * (1 - p2_hat)) / n2)
cat("Standard Error:", round(SE, 6), "\n")

# --- Confidence Interval ---
lower <- (p1_hat - p2_hat) - z_star * SE
upper <- (p1_hat - p2_hat) + z_star * SE

cat("90% Confidence Interval for (p1 - p2): (",
    round(lower, 4), ",", round(upper, 4), ")\n")

# Step 2 of 2 : Interpret the confidence interval found in Step 1.
# --- Decision / Interpretation ---
if (lower <= 0 && upper >= 0) {
  cat("Decision: Since the confidence interval contains 0, the data do not provide evidence that the population proportions are unequal at the 90% confidence level.\n")
} else {
  cat("Decision: Since the confidence interval does not contain 0, the data provide evidence that the population proportions are unequal at the 90% confidence level.\n")
}

#################

# A newspaper story claims that more houses are purchased by singles now than singles 5 years ago. To test this claim, two studies were conducted on the buying habits of singles over the past 5 years. In the first study, 500 house purchases in the current year were randomly selected and 100 of those were made by singles. In the second study, again 500 house purchases were randomly selected from 5 years ago and 77 of those were made by single people. Test the newspaper’s claim using a 0.01 level of significance. Is there sufficient evidence to support the newspaper’s claim? Let singles now be Population 1 and let singles 5 years ago be Population 2.
# Step 1 of 3 : State the null and alternative hypotheses for the test. Fill in the blank below.
# >

# Study 1 (now): n1 = 500, x1 = 100
# Study 2 (5 years ago): n2 = 500, x2 = 77
# Compute the test statistic for a two-proportion z-test at alpha = 0.01.

# --- Given Values ---
n1 <- 500   # sample size (now)
x1 <- 100   # singles (now)

n2 <- 500   # sample size (5 years ago)
x2 <- 77    # singles (5 years ago)

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

cat("Sample proportion (now):", round(p1_hat, 4), "\n")
cat("Sample proportion (5 years ago):", round(p2_hat, 4), "\n")

# --- Pooled Proportion ---
p_pool <- (x1 + x2) / (n1 + n2)
cat("Pooled proportion:", round(p_pool, 4), "\n")

# --- Standard Error (pooled) ---
SE <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))
cat("Standard Error:", round(SE, 6), "\n")

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE
cat("Test Statistic (z):", round(z, 2), "\n")

# Step 3 of 3 :  Draw a conclusion and interpret the decision.

# --- Given Values ---
n1 <- 500   # sample size (now)
x1 <- 100   # singles (now)

n2 <- 500   # sample size (5 years ago)
x2 <- 77    # singles (5 years ago)

alpha <- 0.01

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# --- Pooled Proportion ---
p_pool <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_pool * (1 - p_pool) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE
cat("Test Statistic (z):", round(z, 4), "\n")

# --- Critical Value (right-tailed) ---
z_crit <- qnorm(1 - alpha)
cat("Critical Value:", round(z_crit, 4), "\n")

# --- P-value ---
p_value <- 1 - pnorm(z)
cat("P-value:", round(p_value, 4), "\n")

# --- Decision ---
if (z > z_crit) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.01 level of significance to support the newspaper’s claim that singles now purchase more houses than singles 5 years ago.\n")
} else {
  cat("Decision: Fail to Reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.01 level of significance to support the newspaper’s claim that singles now purchase more houses than singles 5 years ago.\n")
}

########################
# The manufacturer of a new racecar engine study:
# New engines: n1 = 170, x1 = 16
# Old engines: n2 = 205, x2 = 12
# Find the sample proportions p1_hat and p2_hat (round to three decimals)

# --- Given Values ---
n1 <- 170   # sample size (new engines)
x1 <- 16    # failures (new engines)

n2 <- 205   # sample size (old engines)
x2 <- 12    # failures (old engines)

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# --- Output ---
cat("p1_hat:", round(p1_hat, 3), "\n")
cat("p2_hat:", round(p2_hat, 3), "\n")

#  Step 3 of 6 : Compute the weighted estimate of p, p‾. Round your answer to three decimal places.
# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Output ---
cat("Pooled estimate (p_bar):", round(p_bar, 3), "\n")

# The manufacturer of a new racecar engine study:
# New engines: n1 = 170, x1 = 16
# Old engines: n2 = 205, x2 = 12
# Compute the test statistic for a two-proportion z-test (round to two decimals)

# --- Given Values ---
n1 <- 170   # sample size (new engines)
x1 <- 16    # failures (new engines)

n2 <- 205   # sample size (old engines)
x2 <- 12    # failures (old engines)

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE

# --- Output ---
cat("Test Statistic (z):", round(z, 2), "\n")

# Step 5 of 6 : Find the P-value for the hypothesis test. Round your answer to four decimal places.

# --- Given Values ---
n1 <- 170   # sample size (new engines)
x1 <- 16    # failures (new engines)

n2 <- 205   # sample size (old engines)
x2 <- 12    # failures (old engines)

alpha <- 0.05  # significance level

# --- Sample Proportions ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE

# --- P-value (right-tailed test: Ha: p1 > p2) ---
p_value <- 1 - pnorm(z)

# --- Output ---
cat("P-value:", round(p_value, 4), "\n")

# Step 6 of 6 : Make the decision to reject or fail to reject the null hypothesis.

# --- Given Values ---
n1 <- 170   # sample size (new racecar engines)
x1 <- 16    # number of overheating failures (new racecar engines)

n2 <- 205   # sample size (old engines)
x2 <- 12    # number of overheating failures (old engines)

alpha <- 0.05   # significance level

# --- Sample Proportions ---
p1_hat <- x1 / n1   # sample proportion for new engines
p2_hat <- x2 / n2   # sample proportion for old engines

# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE
cat("Test Statistic (z):", round(z, 4), "\n")

# --- P-value ---
p_value <- 1 - pnorm(z)   # right-tailed test
cat("P-value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject the null hypothesis.\n")
  cat("Conclusion: NASCAR has sufficient evidence, at the 0.05 level of significance, to reject the racecar engine manufacturer's claim.\n")
} else {
  cat("Decision: Fail to reject the null hypothesis.\n")
  cat("Conclusion: NASCAR does not have sufficient evidence, at the 0.05 level of significance, to reject the racecar engine manufacturer's claim.\n")
}

################
# A poll is conducted to determine if US citizens think that there should be a national health care system in the U.S. The results of the poll were as follows: 60% of the 270 Nevadans surveyed and 52% of the 250 New Yorkers surveyed think that there should be a national health care system in the U.S. Is there sufficient evidence to conclude at α = 0.02 that Nevadans and New Yorkers feel differently about this issue? Let Population 1 be Nevadans and Population 2 be New Yorkers.
# Step 1 of 3: State the null and alternative hypotheses for the test.

# --- Step 1: Hypotheses ---
cat("H0: p1 = p2\n")
cat("Ha: p1 != p2\n")

# A poll is conducted to determine if US citizens think that there should be a national health care system in the U.S. The results of the poll were as follows: 60% of the 270 Nevadans surveyed and 52% of the 250 New Yorkers surveyed think that there should be a national health care system in the U.S. Is there sufficient evidence to conclude at α = 0.02 that Nevadans and New Yorkers feel differently about this issue? Let Population 1 be Nevadans and Population 2 be New Yorkers.
# Step 2 of 3: Compute the value of the test statistic. Round your answer to two decimal places.

# --- Given Values ---
n1 <- 270          # sample size (Nevadans)
p1_hat <- 0.60     # sample proportion (Nevadans)

n2 <- 250          # sample size (New Yorkers)
p2_hat <- 0.52     # sample proportion (New Yorkers)

# Convert proportions to counts (needed for pooled estimate)
x1 <- p1_hat * n1
x2 <- p2_hat * n2

# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE

# --- Output ---
cat("Test Statistic (z):", round(z, 2), "\n")

# Step 3 of 3 : Draw a conclusion and interpret the decision.

# --- Given Values ---
n1 <- 270
p1_hat <- 0.60

n2 <- 250
p2_hat <- 0.52

alpha <- 0.02

# Convert to counts
x1 <- p1_hat * n1
x2 <- p2_hat * n2

# --- Pooled Proportion ---
p_bar <- (x1 + x2) / (n1 + n2)

# --- Standard Error ---
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

# --- Test Statistic ---
z <- (p1_hat - p2_hat) / SE
cat("Test Statistic (z):", round(z, 4), "\n")

# --- P-value (two-tailed) ---
p_value <- 2 * (1 - pnorm(abs(z)))
cat("P-value:", round(p_value, 4), "\n")

# --- Decision ---
if (p_value < alpha) {
  cat("Decision: Reject the null hypothesis.\n")
  cat("Conclusion: There is sufficient evidence at the 0.02 level of significance to conclude that Nevadans and New Yorkers feel differently about this issue.\n")
} else {
  cat("Decision: Fail to reject the null hypothesis.\n")
  cat("Conclusion: There is insufficient evidence at the 0.02 level of significance to conclude that Nevadans and New Yorkers feel differently about this issue.\n")
}

#############################
# A manufacturer is comparing shipments of machine parts from two suppliers. The parts from Supplier A are less expensive; however, the manufacturer is concerned that the parts may be of a lower quality than those from Supplier B. The manufacturer has decided that he will purchase his supplies from Supplier A unless he can show that the proportion of defective parts is significantly higher for Supplier A than for Supplier B. He randomly selects parts from each supplier and inspects them for defects. The results are as follows. Which supplier will the manufacturer choose at α = 0.10? Explain. Let Population 1 be Supplier A parts and Population 2 be Supplier B parts.
# Supplier A: n1 = 500, x1 = 10
# Supplier B: n2 = 600, x2 = 10
# Step 1 of 3: State the null and alternative hypotheses.
# Step 2 of 3: Compute the test statistic.
# Step 3 of 3: Make the decision and interpret.

# --- Given Values ---
n1 <- 500   # sample size (Supplier A)
x1 <- 10    # defective parts (Supplier A)

n2 <- 600   # sample size (Supplier B)
x2 <- 10    # defective parts (Supplier B)

alpha <- 0.10

# --- Step 1: Hypotheses ---
cat("Step 1: Hypotheses\n")
cat("H0: p1 = p2\n")
cat("Ha: p1 > p2\n\n")

# --- Step 2: Test Statistic ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

p_bar <- (x1 + x2) / (n1 + n2)
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

z <- (p1_hat - p2_hat) / SE

cat("Step 2: Test Statistic\n")
cat("p1_hat:", round(p1_hat, 4), "\n")
cat("p2_hat:", round(p2_hat, 4), "\n")
cat("Test Statistic (z):", round(z, 2), "\n\n")

# --- Step 3: Decision ---
p_value <- 1 - pnorm(z)

cat("Step 3: Decision\n")
cat("P-value:", round(p_value, 4), "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: Supplier A has a significantly higher defect rate. Choose Supplier B.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is not sufficient evidence that Supplier A has a higher defect rate. Choose Supplier A.\n")
}

####################
# A study was performed to determine the percentage of people who wear life vests while out on the water. A researcher believed that the percentage was different for those who rode jet skis compared to those who were in boats. Out of 500 randomly selected people who rode a jet ski, 90.6% wore life vests. Out of 200 randomly selected boaters, 95% wore life vests. Using a 0.01 level of significance, test the claim that the proportion of people who wear life vests while riding a jet ski is not the same as the proportion of people who wear life vests while riding in a boat. Let jet skiers be Population 1 and let boaters be Population 2.
# Step 1 of 3: State the null and alternative hypotheses.
# Step 2 of 3: Compute the test statistic.
# Step 3 of 3: Make the decision and interpret.

# --- Given Values ---
n1 <- 500        # sample size (jet skiers)
p1_hat <- 0.906  # sample proportion (jet skiers)

n2 <- 200        # sample size (boaters)
p2_hat <- 0.95   # sample proportion (boaters)

alpha <- 0.01

# Convert proportions to counts
x1 <- p1_hat * n1
x2 <- p2_hat * n2

# --- Step 1: Hypotheses ---
cat("Step 1: Hypotheses\n")
cat("H0: p1 = p2\n")
cat("Ha: p1 != p2\n\n")

# --- Step 2: Test Statistic ---
p_bar <- (x1 + x2) / (n1 + n2)
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

z <- (p1_hat - p2_hat) / SE

cat("Step 2: Test Statistic\n")
cat("p1_hat:", round(p1_hat, 4), "\n")
cat("p2_hat:", round(p2_hat, 4), "\n")
cat("Test Statistic (z):", round(z, 2), "\n\n")

# --- Step 3: Decision ---
p_value <- 2 * (1 - pnorm(abs(z)))

cat("Step 3: Decision\n")
cat("P-value:", round(p_value, 4), "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence at the 0.01 level of significance to conclude that the proportions are different.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence at the 0.01 level of significance to conclude that the proportions are different.\n")
}


###########################
# Suppose you have recently become interested in photography and are shopping on Amazon for a digital single-lens reflex (DSLR) camera. You’ve narrowed your choice down to two cameras and are leaning towards purchasing the Nikon, unless the Canon has a significantly higher proportion of 5-Star ratings. Given the Amazon rating distributions for the two cameras, which will you choose at α = 0.05? Explain. Let the Nikon D3500 reviews be Population 1 and the Canon Rebel T7 reviews be Population 2.
# Nikon: 5★=450, 4★=96, 3★=20, 2★=10, 1★=62
# Canon: 5★=194, 4★=15, 3★=12, 2★=8, 1★=24
# Step 1 of 3: State the null and alternative hypotheses.
# Step 2 of 3: Compute the test statistic.
# Step 3 of 3: Make the decision and interpret.

# --- Given Values ---
# Nikon (Population 1)
x1 <- 450   # number of 5-star reviews
n1 <- 450 + 96 + 20 + 10 + 62   # total reviews

# Canon (Population 2)
x2 <- 194   # number of 5-star reviews
n2 <- 194 + 15 + 12 + 8 + 24    # total reviews

alpha <- 0.05

# --- Step 1: Hypotheses ---
cat("Step 1: Hypotheses\n")
cat("H0: p1 = p2\n")
cat("Ha: p1 < p2\n\n")   # Canon higher proportion means p2 > p1

# --- Step 2: Test Statistic ---
p1_hat <- x1 / n1
p2_hat <- x2 / n2

p_bar <- (x1 + x2) / (n1 + n2)
SE <- sqrt(p_bar * (1 - p_bar) * (1/n1 + 1/n2))

z <- (p1_hat - p2_hat) / SE

cat("Step 2: Test Statistic\n")
cat("p1_hat:", round(p1_hat, 4), "\n")
cat("p2_hat:", round(p2_hat, 4), "\n")
cat("Test Statistic (z):", round(z, 2), "\n\n")

# --- Step 3: Decision ---
p_value <- pnorm(z)   # left-tailed test

cat("Step 3: Decision\n")
cat("P-value:", round(p_value, 4), "\n")

if (p_value < alpha) {
  cat("Decision: Reject H0\n")
  cat("Conclusion: There is sufficient evidence that Canon has a higher proportion of 5-star ratings. Choose Canon.\n")
} else {
  cat("Decision: Fail to reject H0\n")
  cat("Conclusion: There is insufficient evidence that Canon has a higher proportion of 5-star ratings. Choose Nikon.\n")
}