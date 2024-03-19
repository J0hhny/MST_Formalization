#### Setting WD and loading packages ----

## Setting working directory

setwd("")

## Install necessary packages

install.packages("extrafont")
install.packages("ggplot2")
install.packages("dplyr")

## Load necessary packages

library(extrafont)
library(ggplot2)
library(dplyr)

## Loading font for use of Times New Roman in graphs

loadfonts(device="win")
loadfonts("all")

#### Testing the Law of Practice function ----

### Here we first wanted to test how the different parameters of our 
### adapted Law of power function would influence the development of the 
### function and the resulting performance.

## Function describing the general performance increase of real action modeled after the Law of Practice

real_action <- function (baseline, n, growth, A){
performance <- baseline + ((1-baseline) * n^growth)/(A + n^growth)
return(performance)
}

## Creating different parameters to test how they influence the shape of the function

baselines_ra <- c(0.25, 0.5, 0.75)
growths <- c(0.2, 0.5, 0.8)
As <- c(50, 150)  
n_values <- seq(1, 10000, by = 1)

parameters <- expand.grid(baseline = baselines_ra, growth = growths, A = As, n = n_values)

parameters$performance <- with(parameters, real_action(baseline, n, growth, A))

## Add a unique identifier to each parameter combination to be displayed as different lines in plot

parameters$param_id <- with(parameters, paste("Growth:", growth, "A:", A))

## see "creating figure 4" for use of this function in plotting

#### iterinary adaptions of functions for real and mental practice ----

### Here we adapted the function of law of practice into an iterinary approach
### this is necessary to model the expertise level adapted function 

## itinerary adaption of the function of real action to implement
## with the mental practice function

real_practice <- function (baseline, growth, A, n_max){
  n_values <- 1:n_max
  performance_values <- numeric(length = n_max)
  performance = baseline
  
  for (n in n_values) {
    performance <- baseline + ((1-baseline) * n^growth)/(A + n^growth)
    performance_values[n] <- performance
    n <- n+1
  }
  
  return(data.frame(n = n_values, performance = performance_values))
}

## defining a test dataframe for modelling of

data_real_practice <- real_practice(baseline = 0.5, growth = 0.5, A = 50, n_max = 10000)


## function for the simulation of mental practice effect on performance

mental_practice <- function (baseline, growth, A, n_max, F){
  n_values <- 1:n_max
  performance_values <- numeric(length = n_max)
  performance = baseline
  for (n in n_values) {
  functional_equivalence <- performance
  performance <- baseline + (((1-baseline) * n^growth)/(A + n^growth)) * (functional_equivalence-F)
  performance_values[n] <- performance
  n <- n+1
  }
  
  return(data.frame(n = n_values, performance = performance_values))
}

data_mental_practice <- mental_practice(baseline = 0.5, growth = 0.5, A = 50, n_max = 10000, F = 0.2)

#### simulating participants of mental practice ----

### Here we define a function that is able to simulate many participants with different
### growth and n_max values and set them into a dataframe. We then use this dataframe to
### create a scatterplot and run t-tests on the effect strenght of mental practice on a
### basic baseline of 0.5

set.seed(42) # setting seed for reproducibility

## defining function to output a single performance value for a single simulated
## participant

mental_practice_single <- function (baseline, growth, A, n_max, F_){
  n_values <- 1:n_max
  performance = baseline
  for (n in 1:n_max) {
    functional_equivalence <- performance
    performance <- baseline + (((1-baseline) * n^growth)/(A + n^growth)) * (functional_equivalence-F_)
    n <- n+1
    }
    return(performance)
  }

num_participants <- 100  # Number of participants

# Sample growth and n_max for each participant from a normal distribution

simulation_mental_growth_values <- rnorm(num_participants, mean = 0.5, sd = 0.05)
simulation_mental_n_max_values <- round(rnorm(num_participants, mean = 5000, sd = 1000))

# Ensure n_max_values are positive and at least 1

simulation_mental_n_max_values <- pmax(simulation_mental_n_max_values, 1)

# Generate a performance value for each participant

simulation_mental_performance_values <- mapply(mental_practice_single, 
                                               baseline = rep(0.5, num_participants), 
                                               growth = simulation_mental_growth_values, 
                                               A = rep(50, num_participants), 
                                               n_max = simulation_mental_n_max_values,
                                               F_ = rep(0.2, num_participants))


# Create a dataframe for further analysis

simulation_mental_data <- data.frame(growth = simulation_mental_growth_values, n_max = simulation_mental_n_max_values,
                                     performance = simulation_mental_performance_values)

# View the first few rows of the dataframe

head(simulation_mental_data)

## Categorize growth values to show the effect of different sizes of the growth factor
## in the scatterplot

simulation_mental_data$growth_category <- cut(simulation_mental_data$growth, 
                                              breaks = c(-Inf, 0.4,0.5, 0.6, Inf),
                                              labels = c("0-0.4", "0.4-0.5","0.5-0.6", ">0.6"))

## Create a scatterplot to see how different participants increase their performance 
## over the mental practice

ggplot(simulation_mental_data, aes(x = n_max, y = performance, color = growth_category)) +
  geom_point() +
  labs(title = "Performance vs. N_max with Growth Categories",
       x = "N_max", y = "Performance", color = "Growth Category") +
  theme_minimal()


#### creating figure for the functional equivalence function ----

### Here we create the function that represents how the functional equivalence 
### is dependent on the baseline performance - this figure is used in table 3

## Define the functional equivalence function

fun_equ <- function(F, baseline_performance) {
  F <- 0.2
  functional_equivalence <- baseline_performance - F
  return(functional_equivalence)
}


# Generate a sequence of baseline_performance values

baseline_performance <- seq(0, 1, by = 0.01)

# Compute functional equivalence for each baseline_performance value

fun_equ_values <- fun_equ(F, pre_task_performance)

# Plot the graph

df_fun_equ <- data.frame(baseline_performance, fun_equ_values)

fun_equ_plot <- ggplot(df_fun_equ, aes(x = baseline_performance, y = fun_equ_values)) +
  geom_line(color = "blue", linewidth = 2) +
  scale_x_continuous(limits = c(0.2, 1)) +
  ggtitle("Functional Equivalence as a Function of Baseline Performance") +
  xlab("Baseline Performance") +
  ylab("Functional Equivalence") +
  theme_classic()

# save the graph

print(fun_equ_plot)
ggsave("fun_equ.png", plot = fun_equ_plot)

#### creating figure 4 ----

### here we create figure 4 showing the performance increase of practice in general
### modeled after the law of practice

figure4 <- ggplot(data_real_practice, aes(x = n, y = performance)) +
  geom_line(linewidth = 1.5) +
  labs(title = "Performance increase after n real practice sessions",
       x = "n", y = "Performance", caption = "Graph showing the development of post-taks 
      performance after n iterations of real practice") +  
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12)) 


print(figure4)

ggsave("figure4.png", width = 10, height = 4, dpi = 900)


#### creating figure 5 ----

### here we create figure 5 comparing real and mental practice and their effect
### on performance based on different baseline values

## Generate data for different baseline values, set F = 0.2 and label it

baselines <- c(0.25, 0.5, 0.75)
data_list <- lapply(baselines, function(b) {
  df1 <- mental_practice(baseline = b, growth = 0.5, A = 50, n_max = 10000, F = 0.2)
  df1$baseline <- as.factor(b)
  df1$type <- "mental_practice"
  
  df2 <- real_practice(baseline = b, growth = 0.5, A = 50, n_max = 10000)
  df2$baseline <- as.factor(b)
  df2$type <- "real_practice"
  
  return(rbind(df1, df2))
})

## Combine into a single data frame

combined_data <- bind_rows(data_list)

# Plot

ggplot(combined_data, aes(x = n, y = performance, colour = baseline, linetype = type)) +
  geom_line() +
  labs(title = "Comparison of performance improvement",
       subtitle = "Comparing Mental practice and real action across baselines",
       x = "n",
       y = "Performance") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12)) +  # Set all plot text to Times New Roman
  scale_linetype_manual(values = c("mental_practice" = "solid", "real_practice" = "dashed"))

# Save the plot

ggsave("figure5.png", plot = last_plot(), width = 10, height = 4, dpi = 900)




#### Simulation and testing ----

### Here we conduct our t-testing and simulate the datasets of low and high expertise 
### groups. 

set.seed(42) # setting seed for reproducibility

## t test of mental practice data against baseline and therefore its general effect
## on performance

t.test(simulation_mental_data$performance, mu = 0.5, alternative = "greater")

### Testing for high and low expertise groups 

# Generate performance values with low baseline

le_simulation_mental_performance_values <- mapply(mental_practice_single, 
                                               baseline = rep(0.25, num_participants), 
                                               growth = simulation_mental_growth_values, 
                                               A = rep(50, num_participants), 
                                               n_max = simulation_mental_n_max_values,
                                               F_ = rep(0.2, num_participants))

# Create a dataframe for further analysis with low expertise (le) participants

le_simulation_mental_data <- data.frame(growth = simulation_mental_growth_values, n_max = simulation_mental_n_max_values,
                                     performance = le_simulation_mental_performance_values)

# add column showing performance increase for comparison

le_simulation_mental_data <- le_simulation_mental_data %>%
  mutate(performance_increase = performance - 0.25)

# Generate performance values with high baseline

he_simulation_mental_performance_values <- mapply(mental_practice_single, 
                                                  baseline = rep(0.75, num_participants), 
                                                  growth = simulation_mental_growth_values, 
                                                  A = rep(50, num_participants), 
                                                  n_max = simulation_mental_n_max_values,
                                                  F_ = rep(0.2, num_participants))

# Create a dataframe for further analysis with high expertise (he) participants

he_simulation_mental_data <- data.frame(growth = simulation_mental_growth_values, n_max = simulation_mental_n_max_values,
                                        performance = he_simulation_mental_performance_values)

# add column showing performance increase for comparison

he_simulation_mental_data <- he_simulation_mental_data %>%
  mutate(performance_increase = performance - 0.75)

## t-testing whether low expertise and high expertise individuals profit to differing
## degress from mental practice

t.test(x = le_simulation_mental_data$performance_increase,
       y = he_simulation_mental_data$performance_increase)


## testing for general effect of mental practice in low expertise group

t.test(x = le_simulation_mental_data$performance_increase, mu = 0, alternative = "greater")


#### creating figure 6 ----

### here we create the scatterplot showing our simulated participants from the 
### low and high expertise groups and giving their performance increase a visual 
### depiction.

# Add a 'source' column to distinguish between groups

le_simulation_mental_data$source <- 'Low Expertise'
he_simulation_mental_data$source <- 'High Expertise'

# Combine the two dataframes

combined_data <- rbind(le_simulation_mental_data, he_simulation_mental_data)

## creating the scatterplot to compare performance increase between expertise groups

ggplot(combined_data, aes(x = n_max, y = performance_increase, color = source)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Comparison of performance increase",
       subtitle = "Low vs high expertise participants",
       x = "Practice iterations",
       y = "Performance increase",
       color = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman", size = 12)) +  # Set all plot text to Times New Roman
  scale_color_manual(values = c("Low Expertise" = "blue", "High Expertise" = "red"))

## save the plot

ggsave("figure6.png", width = 10, height = 4, dpi = 900)

