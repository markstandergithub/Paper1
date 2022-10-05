############################################
#
# Set the working directory; yours will be different
#
my_wd <- "/Users/julianstander/Mark_Stander"
#
setwd(my_wd)
#
############################################
#
# Produce postscript figures
#
make_postscript <- TRUE
#
############################################
#
# Packages
#
library(tidyverse)
library(latex2exp)
library(readxl)
library(scales)
#
############################################
#
# Functions
#
# Gini mean
#
Gini_mean <- function(r, s, x){
  #
  log_G <- (log(sum(x^(r + s))) - log(sum(x^s))) / r
  #
  G <- exp(log_G)
  G
}
#
# ------------------------------------------
#
# Estimate the second largest value
#
sum_power <- function(p, x){
  #
  sum(x^p)
  #
}
#
# ------------------------------------------
#
x_max_hat <- function(p, x,
                      round_up = FALSE){
  #
  estimate <- sum_power(p, x) / sum_power(p - 1, x)
  #
  # Round up
  #
  if(round_up){
    estimate <- ceiling(estimate)
  }
  #
  estimate
}
#
# ------------------------------------------
#
x_second_max_hat <- function(p, 
                             p_prime, 
                             x, 
                             correct_over_estimation = TRUE){
  #
  x_max_hat_p <- x_max_hat(p, x)
  #
  S_1 <- sum_power(p_prime, x) - x_max_hat_p^(p_prime)
  S_2 <- sum_power(p_prime - 1, x) - x_max_hat_p^(p_prime - 1)
  #
  x_second_max_hat <- S_1 / S_2
  #
  # If x_second_max_hat is bigger than x_ma_hat_p, then just return x_max_hat_p
  #
  if(correct_over_estimation & x_second_max_hat > x_max_hat_p){
    #
    x_second_max_hat <- x_max_hat_p
  }
  #
  x_second_max_hat
  #
}
#
# ------------------------------------------
#
# ****** Using rounding method
#
x_second_max_using_rounding_hat <- function(p, 
                                            x, 
                                            correct_over_estimation = TRUE){
  #
  x_max_hat_p <- x_max_hat(p, x,
                           round_up = TRUE)
  #
  S_1 <- sum_power(p, x) - x_max_hat_p^p
  S_2 <- sum_power(p - 1, x) - x_max_hat_p^(p - 1)
  #
  x_second_max_hat <- ceiling(S_1 / S_2)
  #
  # If x_second_max_hat is bigger than x_ma_hat_p, then just return x_max_hat_p
  #
  if(correct_over_estimation & x_second_max_hat > x_max_hat_p){
    #
    x_second_max_hat <- x_max_hat_p
  }
  #
  x_second_max_hat
  #
}
#
############################################
############################################
#
# Use with **** real data ****
#
# Read in the data
#
business_data <- read_csv("business_data.csv")
#
head(business_data)
#
dim(business_data)
#
# Extract the project revenues
#
project_revenue <- business_data$project_revenue
#
############################################
############################################
#
# Simulation study
#
# Sample size
#
n <- 20
#
# p sequence
#
p_max <- 20
#
p_seq <- seq(from = 1, to = p_max, by = 0.2)
p_seq
#
p_prime_seq <- p_seq
#
N <- length(p_seq)
N
#
# Now repeat for many data samples
#
N_rep <- 250
#
x_max_percentage_distance_matrix <- matrix(NA,
                                           nrow = N,
                                           ncol = N_rep)
#
x_second_max_percentage_distance_array <- array(NA,
                                                dim = c(N, N, N_rep),
                                                dimnames = list(p_seq, p_prime_seq, 1:N_rep))
#
for(k in 1:N_rep){
  #
  if(k %% 10 == 0) {print(paste0("Iteration number ", k))}
  #
  # Generate some data by taking a sample of size n, with replacement, from the real data
  #
  x <- sample(project_revenue,
              size = n,
              replace = TRUE)
  #
  x_max <- sort(x)[n]
  x_second_max <- sort(x)[n - 1]
  #
  for(i in 1:N){
   #
   x_max_estimate <- x_max_hat(p_seq[i], x)
   #
   x_max_percentage_distance_matrix[i, k] <- 100 * (x_max - x_max_estimate) / x_max  
   #
   for(j in 1:N){
     #
     x_second_max_estimate <- x_second_max_hat(p_seq[i], p_prime_seq[j], x)
     #
     x_second_max_percentage_distance_array[i, j, k] <- 100 * (x_second_max - x_second_max_estimate) / x_second_max
   }
  }
  #
}
#
# Summarize the results
#
# Work out mean across repetitions
#
x_max_percentage_distance_matrix_mean <- apply(x_max_percentage_distance_matrix, 
                                               MARGIN = 1,
                                               FUN = mean)
#
x_second_max_abs_percentage_distance_array_mean <- apply(abs(x_second_max_percentage_distance_array),
                                  MARGIN = c(1, 2),
                                  FUN = mean)
#
# --------------------------------------
#
# Specific example with p = 3 and p' = 4 (or whatever)
#
x_max_error_specific <- rep(NA, N_rep)
x_second_max_error_specific <- rep(NA, N_rep)
#
p <- 3
p_prime <- 4
#
for(i in 1:N_rep){
  #
  # Generate some data by taking a sample of size n, with replacement, from the real data
  #
  x <- sample(project_revenue,
              size = n,
              replace = TRUE)
  #
  x_max <- sort(x)[n]
  x_second_max <- sort(x)[n - 1]
  #
  x_max_estimate <- x_max_hat(p, x)
  #
  x_max_error_specific[i] <- 100 * (x_max - x_max_estimate) / x_max  
  #
  x_second_max_estimate <- x_second_max_hat(p, p_prime, x)
  #
  x_second_max_error_specific[i] <- 100 * (x_second_max - x_second_max_estimate) / x_second_max
  #
}
#
# ------------------------------------------
#
# Specific example with p = 14 and p' = 8 (or whatever)
#
x_max_error_specific_2 <- rep(NA, N_rep)
x_second_max_error_specific_2 <- rep(NA, N_rep)
#
p_2 <- 14 
p_prime_2 <- 8 
#
for(i in 1:N_rep){
  #
  #
  # Generate some data by taking a sample of size n, with replacement, from the real data
  #
  x <- sample(project_revenue,
              size = n,
              replace = TRUE)
  #
  x_max <- sort(x)[n]
  x_second_max <- sort(x)[n - 1]
  #
  x_max_estimate <- x_max_hat(p_2, x)
  #
  x_max_error_specific_2[i] <- 100 * (x_max - x_max_estimate) / x_max  
  #
  x_second_max_estimate <- x_second_max_hat(p_2, p_prime_2, x)
  #
  x_second_max_error_specific_2[i] <- 100 * (x_second_max - x_second_max_estimate) / x_second_max
  #
}
#
################################################
#
# Figure 1
#
if(make_postscript){
  #
  # Set up for postscript
  #
  setEPS()
  #
  postscript("Figure_1.eps",
             width = 14,
             height = 14)
#
}
#
my_cex <- 2
my_cex_main <- 2.5
#
par(mfrow = c(2,2),
    mar = c(5.1 + 2.5, 4.1 + 3.1, 4.1, 2.1))
#
par(pty = "m")
#
plot(p_seq,
     x_max_percentage_distance_matrix_mean,
     ylim = range(0, x_max_percentage_distance_matrix_mean),
     type = "b",
     pch = 4,
     axes = FALSE,
     xlab = TeX("Power $p$"),
     ylab = TeX("Mean of $100 \\, \\left(x_{(n)} - \\hat{x}_{(n)}^{(p)}\\right) \\, / \\, x_{(n)} \\%$"),
     main = "(a)",
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
axis(1, at = c(1, 5, 10, 15, 20),
     cex.axis = my_cex)
#
axis(2,
     cex.axis = my_cex)
#
box()
#
abline(h = 0)
#
abline(v = 1:p_max,
       col = "lightgrey")
#
# ----------------------------
#
par(pty = "s")
#
range(x_second_max_abs_percentage_distance_array_mean)
#
contour(p_seq,
        p_prime_seq,
        x_second_max_abs_percentage_distance_array_mean,
        xlab = TeX("Power $p$"),
        ylab = TeX("Power $p^{\\prime}$"),
        axes = FALSE,
        main = "(b)",
        levels = seq(from = 0,
                    to = max(x_second_max_abs_percentage_distance_array_mean, na.rm = TRUE),
                    by = 1),
      cex.main = my_cex_main,
      cex.lab = my_cex,
      labcex = my_cex * 0.6)
#
axis(1, at = c(1, 5, 10, 15, 20), 
     cex.axis = my_cex)
#
axis(2, at = c(1, 5, 10, 15, 20), 
     cex.axis = my_cex)
#
box()
#
abline(v = 1:p_max,
       h = 1:p_max,
       col = "lightgrey")
#
# The grid obscures the contour, so replot
#
contour(p_seq,
        p_prime_seq,
        x_second_max_abs_percentage_distance_array_mean,
        levels = seq(from = 0,
                    to = max(x_second_max_abs_percentage_distance_array_mean, na.rm = TRUE),
                    by = 1),
        axes = FALSE,
        add = TRUE,
        labcex = my_cex * 0.6)
#
abline(0, 1,
       lwd = 2)
#
# ----------------------------------------
#
par(pty = "m")
#
h <- hist(x_second_max_error_specific,
          xlab = "",
          main = "(c)",
          xlim = range(0, x_second_max_error_specific),
          nclass = 25,
          cex.main = my_cex_main,
          cex.axis = my_cex,
          cex.lab = my_cex,
          plot = FALSE)
#
hist(x_second_max_error_specific,
     xlab = "",
     main = "(c)",
     xlim = range(0, x_second_max_error_specific),
     ylim = c(0, max(h$counts) * 1.1),
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
rug(x_second_max_error_specific)
#
title(xlab = TeX("$100 \\, \\left( x_{(n-1)} -\\hat{x}_{(n-1)}^{(p,p')} \\right) \\, / \\, x_{(n-1)} \\%$"),
      cex.lab = my_cex,
      line = 5)
#
title(sub = paste0("p = ", p, ", p' = ", p_prime),
      cex.sub = my_cex,
      line = 6)
#
abline(v = 0,
       lwd = 3)
#
text(x = 0,
     y = max(h$counts) * 1.1,
     "Under",
     cex = my_cex,
     adj = 0)

text(x = 0,
     y = max(h$counts) * 0.8,
     "Over",
     cex = my_cex,
     adj = 1)
#
# ----------------------------------------
#
h <- hist(x_second_max_error_specific_2,
          xlab = "",
          main = "(d)",
          xlim = range(0, x_second_max_error_specific, x_second_max_error_specific_2),
          nclass = 25,
          cex.main = my_cex_main,
          cex.axis = my_cex,
          cex.lab = my_cex,
          plot = FALSE)
#
hist(x_second_max_error_specific_2,
     xlab = "",
     main = "(d)",
     xlim = range(0, x_second_max_error_specific, x_second_max_error_specific_2),
     ylim = c(0, max(h$counts) * 1.1),
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
rug(x_second_max_error_specific_2)
#
title(xlab = TeX("$100 \\, \\left( x_{(n-1)} -\\hat{x}_{(n-1)}^{(p,p')} \\right) \\, / \\, x_{(n-1)} \\%$"),
      cex.lab = my_cex,
      line = 5)
#
title(sub = paste0("p = ", p_2, ", p' = ", p_prime_2),
      cex.sub = my_cex,
      line = 6)
#
abline(v = 0,
       lwd = 3)
#
text(x = 0,
     y = max(h$counts) * 1.1,
     "Under",
     cex = my_cex,
     adj = 0)

text(x = 0,
     y = max(h$counts) * 0.8,
     "Over",
     cex = my_cex,
     adj = 1)
#
# ----------------------------------
#
if(make_postscript){
  #
  dev.off() # For EPS
  #
}
#
###############################################################
###############################################################
#
# Figure 2
#
if(make_postscript){
  #
  setEPS()
  #
  postscript("Figure_2.eps",
             width = 14,
             height = 14)
#
}
#
# Based on integer values, with rounding to the nearest 1000
#
# Simulation study
#
p_max <- 20
#
p_seq <- seq(from = 1, to = p_max, by = 0.2)
p_seq
#
N <- length(p_seq)
N
#
# Now repeat for many data samples
#
N_rep <- 250
#
x_max_percentage_distance_matrix <- matrix(NA,
                                           nrow = N,
                                           ncol = N_rep)
#
x_second_max_percentage_distance_array <- matrix(NA,
                                           nrow = N,
                                           ncol = N_rep)
#
# Round to the nearest 1000
#
pp <- 3
#
for(k in 1:N_rep){
  #
  if(k %% 10 == 0) {print(paste0("Iteration number ", k))}
  #
  # Generate some data by taking a sample of size n, with replacement, from the real data
  #
  x <- sample(project_revenue,
              size = n,
              replace = TRUE)
  #
  # Round to nearest 1000
  #
  x <- round(x, -pp) / (10^pp)
  #
  #
  x_max <- sort(x)[n]
  x_second_max <- sort(x)[n - 1]
  #
  for(i in 1:N){
   #
   x_max_estimate <- x_max_hat(p_seq[i], x,
                               round_up = TRUE)
   #
   x_max_percentage_distance_matrix[i, k] <- 100 * (x_max - x_max_estimate) / x_max  
   #
     #
     x_second_max_estimate <- x_second_max_using_rounding_hat(p_seq[i], x)
     #
     x_second_max_percentage_distance_array[i, k] <- 100 * (x_second_max - x_second_max_estimate) / x_second_max
  }
  #
}
#
# Summarize the results
#
# x_max
#
# Work out mean across repetitions
#
x_max_percentage_distance_matrix_mean <- apply(abs(x_max_percentage_distance_matrix), 
                                               MARGIN = 1,
                                               FUN = mean)
#
x_second_max_abs_percentage_distance_array_mean <- apply(abs(x_second_max_percentage_distance_array),
                                                         MARGIN = 1,
                                                         FUN = mean)
#
##########################
#
# Figure
#
my_cex <- 2
my_cex_main <- 2.5
#
par(mfrow = c(3, 1),
    mar = c(5.1 + 2.5, 4.1 + 3.1, 4.1, 2.1))
#
par(pty = "m")
#
matplot(p_seq,
     cbind(x_max_percentage_distance_matrix_mean, x_second_max_abs_percentage_distance_array_mean),
     ylim = range(0, x_max_percentage_distance_matrix_mean, x_second_max_abs_percentage_distance_array_mean),
     type = "b",
     pch = c(4, 3),
     axes = FALSE,
     xlab = TeX("Power $p$"),
     ylab = "Mean percentage absolute error",
     main = "(a)",
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
legend("topright",
       legend = c(TeX("$\\hat{x}_{(n)}$"), TeX("$\\hat{x}_{(n-1)}$")),
       lty = 1,
       col = 1:2,
       pch = c(4, 3),
       cex = 2)
#
axis(1, at = c(1, 5, 10, 15, 20),
     cex.axis = my_cex)
#
axis(2,
     cex.axis = my_cex)
#
box()
#
abline(h = 0)
#
abline(v = 1:p_max,
       col = "lightgrey")
#
# Specific example with p = 4, was 5 (it's the 16, was 21 element)
#
p_seq
#
sub_script <- 16
#
x_max_error_specific <- x_max_percentage_distance_matrix[sub_script,]
#
x_second_max_error_specific <- x_second_max_percentage_distance_array[sub_script,]
#
#
h <- hist(x_max_error_specific,
     xlab = "",
     main = "(b)",
     xlim = range(0, x_max_error_specific, x_second_max_error_specific), 
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex,
     plot = FALSE)
#
hist(x_max_error_specific,
     xlab = "",
     main = "(b)",
     xlim = range(0, x_max_error_specific, x_second_max_error_specific), 
     ylim = c(0, max(h$counts) * 1.1),
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
rug(x_max_error_specific)
#
title(xlab = TeX("$100 \\, \\left( x_{(n)} -\\hat{x}_{(n)}^{(p)} \\right) \\, / \\, x_{(n)} \\%$"),
      cex.lab = my_cex,
      line = 5)
#
title(sub = paste0("p = ", p_seq[sub_script]),
      cex.sub = my_cex,
      line = 6)
#
abline(v = 0,
       lwd = 3)
#
text(x = 0,
     y = max(h$counts) * 1.1,
     "Under",
     cex = my_cex,
     adj = 0)

text(x = 0,
     y = max(h$counts) * 0.8,
     "Over",
     cex = my_cex,
     adj = 1)
#
# -------------------------------
#
h <- hist(x_second_max_error_specific,
     xlab = "",
     main = "(c)",
     xlim = range(0, x_max_error_specific, x_second_max_error_specific),
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex,
     plot = FALSE)
#
hist(x_second_max_error_specific,
     xlab = "",
     main = "(c)",
     xlim = range(0, x_max_error_specific, x_second_max_error_specific), 
     ylim = c(0, max(h$counts) * 1.1),
     nclass = 25,
     cex.main = my_cex_main,
     cex.axis = my_cex,
     cex.lab = my_cex)
#
rug(x_second_max_error_specific)
#
title(xlab = TeX("$100 \\, \\left( x_{(n-1)} -\\hat{x}_{(n-1)}^{(p)} \\right) \\, / \\, x_{(n-1)} \\%$"),
      cex.lab = my_cex,
      line = 5)
#
title(sub = paste0("p = ", p_seq[sub_script]),
      cex.sub = my_cex,
      line = 6)
#
abline(v = 0,
       lwd = 3)
#
text(x = 0,
     y = max(h$counts) * 1.1,
     "Under",
     cex = my_cex,
     adj = 0)

text(x = 0,
     y = max(h$counts) * 0.8,
     "Over",
     cex = my_cex,
     adj = 1)
#
# ----------------------------------
#
if(make_postscript){
  #
  dev.off() # For EPS
  #
}
#
###############################################################
###############################################################