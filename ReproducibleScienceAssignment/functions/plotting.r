## Script name: plotting.r
##
## Purpose of script: Containing functions to plot an explanatory and results figure for the analysis of Body mass of Adelie penguins on two islands. 
##
## Author: Candidate 1065058
##
## Date Created: 2023-10-03
##
##
## ---------------------------
##
## Notes: Functions to be used in the ReproducibleScienceAssignment





#function to produce an explanatory histogram
plot_body_mass_figure <- function(body_mass_data){
  #creating a function called 'plot_body_mass_figure' which uses the 'body_mass_data'
  body_mass_data <- as.data.frame(body_mass_data)
  #converts 'body_mass_data' into a data frame
  body_mass_data %>% 
    #using a pipe
    ggplot(aes(x = body_mass_g, fill = island))+
    geom_histogram(bins = 12)+
    #creating a histogram with 12 bins
    facet_wrap( ~ island, nrow = 2)+
    #creates seperate panels for each level of the 'island' variable 
    labs(title = "Histogram of Adelie Penguin Body Mass on Biscoe and Torgersen Island", x = "Body Mass (g)", y = "Frequency")+
    theme_bw()+
    guides(fill = guide_legend(title = "Island"))
  #title the legend as 'Island'
  
}


#function to produce the results boxplot 
plot_body_mass_results_figure <- function(body_mass_data){
  #creating a function called 'plot_body_mass_results_figure' which uses the 'body_mass_data'
  body_mass_data <- as.data.frame(body_mass_data)
  #converts 'body_mass_data' into a data frame
ggplot(body_mass_data, aes(x = island, y = body_mass_g)) +
  geom_boxplot(aes(colour = island), width = 0.3, show.legend = FALSE) +
  #adding a boxplot 
  geom_jitter(aes(color = island), alpha = 0.3, position = position_jitter(width = 0.2, seed = 0)) +
  #adding jittered points so they dont overlay each other
  geom_errorbar(
    aes(x = island, ymin = ifelse(island == "Biscoe", 3558, 3926.7),
        ymax = ifelse(island == "Torgersen", 2958, 4562.7)),
    #these are the values from the bootstrapped confidence interval
    position = position_dodge(0.5),
    width = 0.2,
    color = "black",
    stat = "identity"
  ) +
  #here I used manually plotted the error bars produced via bootstrapping for the Mann-Whitney U test in black
  scale_color_manual(values = c("purple", "cyan4")) +
  scale_x_discrete(labels = c("Biscoe", "Torgersen")) +
  labs(
    x = "Island Location",
    y = "Body Mass (g)",
    title = "Body Mass of Adelie Penguins on Biscoe and Torgersen Island"
  ) +
  #adding the plot labels and title 
  theme_bw()+
  #adding a black and white theme
  guides(fill = guide_legend(title = "Island"))
}

