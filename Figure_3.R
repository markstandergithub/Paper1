#####################################
#
# Set the working directory; yours will be different
#
my_wd <- "/Users/julianstander/Mark_Stander"
#
setwd(my_wd)
#
#####################################
#
# Load moments package
#
library(moments)
#
# Load the tidyverse for ggplot
#
library(tidyverse)
#
#####################################
#
# Define the moment function
#
sample_raw_moment_synth <- function(order_no, data_input_3) {
  #
  return(moment(data_input_3,
                order = order_no, 
                central = FALSE))
  #
}
#
####################################
#
# Set counters to 0
#
number_counter <- 0
moment_counter <- 0  
#
####################################
#
# Define the Lehmer mean estimate function
# Rounds up L(p) / L(p-1)
#
x_estimate <- function(moment_input,
                       data_input){   
  #
  # If data_input is a vector of 0s return 0
  #
  if(all(data_input == 0)){
    #
    return(0)
    #
    } else {
      #
    return(ceiling(sample_raw_moment_synth(moment_input, data_input) / sample_raw_moment_synth((moment_input - 1), data_input)))
  #
      }
 #     
}
#
####################################
#
data_checker <- function(length_input, # Number of elements in the data set
                         moment_input_2, # Number of moments to consider
                         data_input_2){ 
  #
  # Looping through each item
  #
  for(k in 1:length_input){
    #
    # Reducing each data item after estimating it correctly
    #
    # Check to see if the estimate is correct
    #
    if(x_estimate(moment_input_2, data_input_2) == data_input_2[length(data_input_2)]){
      #
      # if it has looped through all values of the synth data and made correct estimates
      #
      if(k == length_input){
        #
        # Returns TRUE is all values have been correctly estimated
        #
        return(TRUE)  
        #
      } # End of k loop
      #
    } else { # else statement x_estimate(moment_input_2, data_input_2) != data_input_2[length(data_input_2)]
      #
      # Returns FALSE is all values have not been correctly estimated
      #
      return(FALSE) 
      #
    } # End of else statement x_estimate(moment_input_2, data_input_2) != data_input_2[length(data_input_2)]
    #
    data_input_2 <- data_input_2[1:(length(data_input_2) - 1)] 
    #
  } # End of k in 1:length_input
} # End of function
#
####################################
#
# Maximum sample size
#
max_sample_size <- 100
#
# Number of repetitions
#
N_rep <- 250
#
# Initialize a space for all the results and grow it (not best practice, but fine here)
#
results_all <- data.frame(sample_size = numeric(max_sample_size * N_rep),
                          largest_moment = numeric(max_sample_size * N_rep),
                          Poisson_mean = character(max_sample_size * N_rep))
#
results_all
#
# For different Poisson means
#
results_all_2 <- results_all
results_all_3 <- results_all
results_all_4 <- results_all
results_all_5 <- results_all
#
####################################
#
Poisson_mean <- 10
#
# Declaring the results data-frame
#
results_df <- data.frame(sample_size = numeric(max_sample_size),
                        largest_moment = numeric(max_sample_size))
#
# Repeat many times
#
for(rep in 1:N_rep){
#
  for (number_counter in 1:max_sample_size){ # number of elements in data set
  #
  # Generating the data set, according to a Poisson distribution with mean Poisson_mean
  #
  data <- rpois(number_counter, Poisson_mean)  
  #
  # Sorting the data ascending
  #
  data <- sort(data)      
  #
  # Looping through the moment
  #
  for (moment_counter in 1:500){   
    #
    # Checking if the full data set can be generated from those moments
    #
    if(data_checker(number_counter, # Number of elements in the data set
                    moment_counter, # Number of moments to consider
                    data)){ # data_checker returns TRUE or FALSE  
      #
      # Adding number of elements in data set, and moment required to generate the full data set
      #
      results_df[number_counter,] = c(number_counter, moment_counter)  
      #
      # If the full data set can be reproduced, break out.  Otherwise, increase the moment_counter
      #
      break
    }
    }
  }
#
results_all[(1:max_sample_size) + (rep - 1) *  max_sample_size, ] <- data.frame(results_df, 
                                                                                Poisson_mean = Poisson_mean)
#
# End of repetition
#
}
#
results_all
#
dim(results_all)
#
head(results_all)
#
tail(results_all)
#
####################################
#
Poisson_mean <- 25
#
# Declaring the results data-frame
#
results_df <- data.frame(sample_size = numeric(max_sample_size),
                        largest_moment = numeric(max_sample_size))
#
# Repeat many times
#
for(rep in 1:N_rep){
#
  for (number_counter in 1:max_sample_size){ # number of elements in data set
  #
  # Generating the data set, according to a Poisson distribution with mean Poisson_mean
  #
  data <- rpois(number_counter, Poisson_mean)  
  #
  # Sorting the data ascending
  #
  data <- sort(data)      
  #
  # Looping through the moment
  #
  for (moment_counter in 1:500){   
    #
    # Checking if the full data set can be generated from those moments
    #
    if(data_checker(number_counter, # Number of elements in the data set
                    moment_counter, # Number of moments to consider
                    data)){ # data_checker returns TRUE or FALSE  
      #
      # Adding number of elements in data set, and moment required to generate the full data set
      #
      results_df[number_counter,] = c(number_counter, moment_counter)  
      #
      # If the full data set can be reproduced, break out.  Otherwise, increase the moment_counter
      #
      break
    }
    }
  }
#
results_all_2[(1:max_sample_size) + (rep - 1) *  max_sample_size, ] <- data.frame(results_df, Poisson_mean = Poisson_mean)
#
# End of repetition
#
}
#
results_all_2
#
dim(results_all_2)
#
head(results_all_2)
#
tail(results_all_2)
#
####################################
#
Poisson_mean <- 50
#
# Declaring the results data-frame
#
results_df <- data.frame(sample_size = numeric(max_sample_size),
                        largest_moment = numeric(max_sample_size))
#
# Repeat many times
#
for(rep in 1:N_rep){
#
  for (number_counter in 1:max_sample_size){ # number of elements in data set
  #
  # Generating the data set, according to a Poisson distribution with mean Poisson_mean
  #
  data <- rpois(number_counter, Poisson_mean)  
  #
  # Sorting the data ascending
  #
  data <- sort(data)      
  #
  # Looping through the moment
  #
  for (moment_counter in 1:500){   
    #
    # Checking if the full data set can be generated from those moments
    #
    if(data_checker(number_counter, # Number of elements in the data set
                    moment_counter, # Number of moments to consider
                    data)){ # data_checker returns TRUE or FALSE  
      #
      # Adding number of elements in data set, and moment required to generate the full data set
      #
      results_df[number_counter,] = c(number_counter, moment_counter)  
      #
      # If the full data set can be reproduced, break out.  Otherwise, increase the moment_counter
      #
      break
    }
    }
  }
#
results_all_3[(1:max_sample_size) + (rep - 1) *  max_sample_size, ] <- data.frame(results_df, Poisson_mean = Poisson_mean)
#
# End of repetition
#
}
#
results_all_3
#
dim(results_all_3)
#
head(results_all_3)
#
tail(results_all_3)
#
####################################
#
Poisson_mean <- 75
#
# Declaring the results data-frame
#
results_df <- data.frame(sample_size = numeric(max_sample_size),
                        largest_moment = numeric(max_sample_size))
#
# Repeat many times
#
for(rep in 1:N_rep){
#
  for (number_counter in 1:max_sample_size){ # number of elements in data set
  #
  # Generating the data set, according to a Poisson distribution with mean Poisson_mean
  #
  data <- rpois(number_counter, Poisson_mean)  
  #
  # Sorting the data ascending
  #
  data <- sort(data)      
  #
  # Looping through the moment
  #
  for (moment_counter in 1:500){   
    #
    # Checking if the full data set can be generated from those moments
    #
    if(data_checker(number_counter, # Number of elements in the data set
                    moment_counter, # Number of moments to consider
                    data)){ # data_checker returns TRUE or FALSE  
      #
      # Adding number of elements in data set, and moment required to generate the full data set
      #
      results_df[number_counter,] = c(number_counter, moment_counter)  
      #
      # If the full data set can be reproduced, break out.  Otherwise, increase the moment_counter
      #
      break
    }
    }
  }
#
results_all_4[(1:max_sample_size) + (rep - 1) *  max_sample_size, ] <- data.frame(results_df, Poisson_mean = Poisson_mean)
#
# End of repetition
#
}
#
results_all_4
#
dim(results_all_4)
#
head(results_all_4)
#
tail(results_all_4)
#
####################################
#
# Combine the results
#
results_all_five_Poisson_means <- rbind(results_all, 
                                        results_all_2,
                                        results_all_3,
                                        results_all_4)
#
# -----------------------------
#
# Impose an ordering to Poisson_mean
#
results_all_five_Poisson_means <- results_all_five_Poisson_means %>%
                                     mutate(Poisson_mean = factor(Poisson_mean,
                                                                  levels = c(10, 25, 50, 75),
                                                                  labels = c(expression("mu == 10"),
                                                                             expression("mu == 25"),
                                                                             expression("mu == 50"),
                                                                             expression("mu == 75"))))
#
results_all_five_Poisson_means$Poisson_mean
#
####################################
#
ggplot(results_all_five_Poisson_means,
       aes(x = sample_size,
           y = largest_moment)) +
  geom_count() +
  geom_smooth(se = FALSE) +
  geom_abline(intercept = 0, slope = 1  / 2) +
  scale_x_continuous(breaks = c(1, seq(from = 20,
                                  to = max_sample_size,
                                  by = 20)),
                     minor_breaks = NULL) + 
  scale_y_continuous(breaks = c(1, seq(from = 10,
                                       to = max(results_all_five_Poisson_means$largest_moment) + 10,
                                       by = 10)),
                     minor_breaks = NULL) +
  scale_size_area("Number of simulated data sets out of 250",
                  breaks = c(1, 10, 50, 100, 150, 200, 250)) +
  facet_grid(. ~ Poisson_mean,
             labeller = label_parsed) +
  labs(x = "Sample size n",
       # y = "Number of moments required for complete sample recovery",
       y = "Smallest p for complete sample recovery",
       # caption = "Diagonal line: number of moments = sample size"
       ) +
    theme_bw() +
  theme(axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 16),
        strip.text.x = element_text(size = 20),
        legend.position = "bottom") +
  guides(size = guide_legend(nrow = 1))
#
# ----------------------------------
#
# Save the graph as an eps
#
ggsave("Figure_3.eps",
        device = cairo_ps, 
        dpi = 1200, 
        width = 14,
        height = 11, 
        units = "in")
#
##############################################################
