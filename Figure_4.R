############################################
#
# Set the working directory; yours will be different
#
# This just defines where R will put 
# the encapsulated PostScript figure
#
my_wd <- "/Users/julianstander/Mark_Stander"

#
setwd(my_wd)
#
##########################################
#
# Packages
#
library(VineCopula) # For simulating from t-copula
#
library(latex2exp) # For mathematical labels on the plot
#
########################
#
# Function to generate realizations from
# a t-copula, parameteried by
# Kendall's tau
#
# We use a t-copula in the simulation study
# because it has tail dependence, which a normal copula lacks
#
rtcopula <- function(n, # Sample size
                     tau, # Kendall's tau
                     nu) { # Degrees of freedom
  #
  rho <- sin(pi * tau / 2)
  #
  # Simulate from the t-copula
  #
  u <- BiCopSim(n,
                family = 2, 
                par = rho, 
                par2 = nu)
  #
  return(u)
}
#
# --------------
#
# Test
#
u_v <- rtcopula(1000, 0.6, 3)
#
plot(u_v)
#
cor(u_v,
    method = "kendall")
#
#########################
#
# Function to generate realizations,
# via a t-copula, with log-normal margins
#
rlnorm_2_t_copula <- function(n = 10000, # Sample size
                              meanlog_1 = 0, # Parameters of first log-normal margin
                              sdlog_1 = 1,
                              meanlog_2 = 0, # Parameters of second log-normal margin
                              sdlog_2 = 1,
                              tau = 0, # Kendall's tau
                              nu = 10){ # Degrees of freedom
  #
  # Generate data from t-copula
  #
  u_v <- rtcopula(n = n, 
                  tau = tau,
                  nu = nu)
  #
  # Transform margins to the required log-normals
  #
  u <- u_v[,1]
  v <- u_v[,2]
  #
  x <- qlnorm(u, meanlog_1, sdlog_1)
  y <- qlnorm(v, meanlog_2, sdlog_2)
  #
  # Put together in a matrix
  #
  x_y <- cbind(x, y)
  #
  return(x_y)
}
#
# --------------
#
# Test
#
x_y <- rlnorm_2_t_copula(n = 1000, 
                         meanlog_1 = 0, 
                         sdlog_1 = 1, 
                         meanlog_2 = 0, 
                         sdlog_2 = 1, 
                         tau = 0.5, 
                         nu = 4)
#
par(mfrow = c(2,2))
#
hist(x_y[,1])
#
hist(x_y[,2])
#
plot(x_y)
#
par(mfrow = c(1,1))
#
cor(x_y,
    method = "kendall")
#
############################################
#
# Two useful functions
#
# Find \sum_{i = 1}^p x_i^p
#
sum_power <- function(p, # power p
                      x){ # x_1,...,x_n
  #
  sum(x^p)
  #
}
#
# ------------------------------------------
#
# Get the approximation to x_{(n)}
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
################################
################################
#
# ****** Simulation Study ******
#
# Sample size
#
n <- 20
#
#  Parameters of the log-normal marginal distributions
#
meanlog_1 <- 10
sdlog_1 <- 2
#
meanlog_2 <- 10
sdlog_2 <- 2 
#
nu <- 10 # Degrees of freedom
#
# Sequence of correlation values
#
tau_seq <- c(-0.4, 0, 0.4, 0.8)
#
# Length of tau sequence
#
N_tau <- length(tau_seq)
#
# -----
#
# We will repeat for many data samples
#
N_samples <- 10000 # For example
#
# -----
#
# We will consider many values of p
#
# p sequence
#
p_max <- 20
#
p_seq <- seq(from = 1, 
             to = p_max, 
             by = 0.2)
#
# Length of p sequence
#
N_p <- length(p_seq)
#
# -----
#
# Space for the results
#
# x and y separately
#
x_max_percentage_distance_array <- array(NA,
                                         dim = c(N_tau,
                                                N_samples, 
                                                N_p),
                                         dimnames = list(tau_seq, 
                                                         1:N_samples, 
                                                         p_seq))
#
y_max_percentage_distance_array <-  array(NA,
                                         dim = c(N_tau,
                                                N_samples, 
                                                N_p),
                                         dimnames = list(tau_seq, 
                                                         1:N_samples, 
                                                         p_seq))
#
# Combied
#
x_y_min <- array(NA,
                 dim = c(N_tau, 
                         N_samples, 
                         N_p),
                 dimnames = list(tau_seq, 
                                 1:N_samples, 
                                 p_seq))

#
# Loops
#
for(i in 1:N_tau) { # Loop over tau_seq
  #
  print(paste0(i, "th element of tau sequence out of ", N_tau, 
               " with tau = ", tau_seq[i]))
  #
  for (j in 1:N_samples) { # Loop over data samples
    #
    # Generate data, with tau = tau_seq[i]
    #
    x_y <- rlnorm_2_t_copula(n = n, 
                             meanlog_1 = meanlog_1, 
                             sdlog_1 = sdlog_1, 
                             meanlog_2 = meanlog_2, 
                             sdlog_2 = sdlog_2, 
                             tau = tau_seq[i], 
                             nu = nu)
    #
    # Extract the maximum value for x and for y
    #
    x <- x_y[,1]
    y <- x_y[,2]
    #
    x_max <- sort(x)[n]
    y_max <- sort(y)[n]
    #
    # Approximate for various values of p
    #
    for (k in 1:N_p) { # loop over p_seq
      #
      # Get approximations
      #
      x_max_estimate <- x_max_hat(p_seq[k], x)
      y_max_estimate <- x_max_hat(p_seq[k], y)
      #
      # Get errors
      #
      percentage_distance_x <- 100 * (x_max - x_max_estimate) / x_max
      percentage_distance_y <- 100 * (y_max - y_max_estimate) / y_max
      #
      # Save
      #
      x_max_percentage_distance_array[i, j, k] <- percentage_distance_x
      y_max_percentage_distance_array[i, j, k] <- percentage_distance_y
      #
      # Combine to get minimum distance
      #
      x_y_min[i, j, k] <- min(percentage_distance_x, 
                              percentage_distance_y)
      #
    } # End of loop over p_seq
    #
  } # End of loop over data samples
  #
} # End of loop over tau_seq
#
# --------------------
# --------------------
#
# Average over result arrays
#
# To get results split down by tau and p
#
x_distance_mean <- apply(x_max_percentage_distance_array, 
                         MARGIN = c(1, 3),
                         FUN = mean)
#
y_distance_mean <- apply(y_max_percentage_distance_array, 
                         MARGIN = c(1, 3),
                         FUN = mean)
#
x_y_min_mean <- apply(x_y_min, 
                      MARGIN = c(1, 3),
                      FUN = mean)
#
x_y_min_mean
#
# Plots
#
par(mfrow = c(2,1))
#
# These are quick plots
#
# They are improved upon below
#
matplot(p_seq, 
        t(x_y_min_mean), 
        main = "min",
        type = "l", 
        lty = 1,
        col = c("red", "black", "blue", "green"),
        log = "y")
#
abline(v = 1:p_max,
       col = "lightgrey")
#
matplot(p_seq, 
        t(x_y_min_mean) - t(x_y_min_mean)[,2], 
        main = "min",
        type = "l", 
        lty = 1,
        col = c("red", "black", "blue", "green"))
#
abline(v = 1:p_max,
       col = "lightgrey")
#
############################################
#
# Plots for paper
#
make_postscript <- TRUE
#
if(make_postscript){
  #
  # Set up for postscript
  #
  setEPS()
  #
  postscript("Figure_bivariate.eps",
             width = 14,
             height = 14)
#
}
#
# Graphical character expansion parameters
#
my_cex <- 2
my_cex_main <- 2.5
cex_legend <- 1.25
#
par(mfrow = c(1,2),
    mar = c(5.1 + 2.5, 4.1 + 3.1, 4.1, 2.1), # Space for labels etc
    pty = "m") # As large as possible
#
# Single plot: Errors for x (correlation doesn't play a part here) 
# and x_y_min_mean (three different correlations)
#
error <- t(rbind(x_distance_mean[2,], 
                 x_y_min_mean))
#
matplot(p_seq, 
        error,
        lwd = c(4, 3, 4, 3, 3),
        type = "l",
        lty = 1,
        pch = 4,
        axes = FALSE,
        xlab = TeX("Power $p$"),
        # ylab = TeX("Mean of $100 \\, \\left(x_{(n)} - \\hat{x}_{(n)}^{(p)}\\right) \\, / \\, x_{(n)} \\%$"),
        ylab = TeX("Protection $Pro^{(p)}_{\\tau}$ (log scale)"),
        main = "(a)",
        cex.main = my_cex_main,
        cex.axis = my_cex,
        cex.lab = my_cex,
        col = c("grey50", "red", "black", "green", "blue"),
        log = "y")
#
axis(1, 
     at = c(1, 5, 10, 15, 20),
     cex.axis = my_cex)
#
axis(2,
     at = c(0.2, 1, 5, 20, 100),
     labels = c(0.2, 1, 5, 20, 100),
     cex.axis = my_cex)
#
box()
#
abline(v = 1:p_max,
       col = "lightgrey")
#
legend("bottomleft",
       title = TeX("Bivariate data with correlation \\tau"),
       legend = rev(tau_seq),
       col = rev(c("red", "black", "green", "blue")),
       lty= 1,
       lwd = rev(c(2, 3, 2, 2) + 1),
       cex = cex_legend)

legend("topright",
       title = TeX("Univariate data"),
       legend = TeX("Based on $x_1,\\ldots,x_n$"),
       col = "grey50",
       lty = 1,
       lwd = 3,
       cex = cex_legend)
#
# Max: difference
#
matplot(p_seq, 
        t(x_y_min_mean) - t(x_y_min_mean)[,2],
        type = "l",
        lty = 1,
        lwd = c(3, 4, 3, 3),
        pch = 4,
        axes = FALSE,
        xlab = TeX("Power $p$"),
        ylab = TeX("$\\Delta^{(p)}_{\\tau}  = Pro^{(p)}_{\\tau} - Pro^{(p)}_{0}$"),
        main = "(b)",
        cex.main = my_cex_main,
        cex.axis = my_cex,
        cex.lab = my_cex,
        col = c("red", "black", "green", "blue"))
#
abline(v = 1:p_max,
       col = "lightgrey")
#
axis(1, 
     at = c(1, 5, 10, 15, 20),
     cex.axis = my_cex)
#
axis(2,
     at = c(0, 0.5, 1, 1.5, 2, 2.5),
     cex.axis = my_cex)

box()
#
#
# ----------------------------------
#
if(make_postscript){
  #
  dev.off() # For EPS
  #
}
#
#########################################
#########################################
