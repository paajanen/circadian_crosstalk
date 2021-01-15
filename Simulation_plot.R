library(tidyverse)
library(gridExtra)

#Set the basic variables
n_simu <- 1000 # total number of simulations
n_obs <- 168 # total number of timepoints, 168 hrs = 7*24 hrs
n<- n_simu*n_obs # the size of the grid
amp <- 2 # Set amplitude to 2, to get the range 1 to 3.

# Here we set the seed to be able to produce exactly the same pictures. Pseudorandomness.
set.seed(2020)


# Start creating the tibble from which to plot
simulation <- data.frame(obs_id = 1:n_obs) %>% 
  dplyr::slice(rep(obs_id, each = n_simu)) %>% # each n_simu times
  mutate(simu_id = as.factor(rep(1:n_simu, n_obs)), # create simu_id as factor
         angles=2*pi*(obs_id-1)/24, # Create the angles for a 24 hr clock
         sin_var = sin(angles)+amp) %>% # Create normal sine wave
  select(simu_id, obs_id, angles, sin_var)

#Create a random number for each of the simulations, these can be changed for testing
random_n_simu <- rnorm(n_simu,mean = 1,sd=0.1 ) # this is corresponding to between  2 to 3 hr standard deviation

# Make a tibble encoding the random numbers and add it into the simulation-tibble
random <- rep(random_n_simu, each=n_obs) #expand the random_n_simu by n_obs times
simu_id <- as.factor(rep(1:n_simu, each=n_obs)) #make a column for simu_id
random_tibble <- tibble(simu_id, random) %>% group_by(simu_id) %>% mutate(obs_id=row_number()) #create a tibble
# join this tibble with the simulation-tibble
simulation<- left_join(random_tibble, 
                      simulation, 
                      by=c("simu_id","obs_id"))

#Create new variable, called sin_var_period that takes into account the varying periods (multiplies angles with random)
# We call this tibble freerunning_simulation, as this finishes creating the freerunning simulation.
freerunning_simulation <- simulation  %>% mutate(sin_var_period=sin(angles*random)+amp)

# Now we create the tibble that has the phase resetting, but otherwise follows the free-running simulation
phase <- freerunning_simulation %>% filter(obs_id %in% rep(1:24)) %>% #We only take the first 24 hrs from each simulation
  mutate(count=7) %>% 
  select(simu_id,obs_id,angles,sin_var,sin_var_period,count) 

#%>% 
#  mutate(resetting=sin_var_period-sin_var, count=7) %>% 
#  select(simu_id,sin_var_period,count)


# We create a vector to cover the 7 days for each simulation.
new_obs <- rep(1:7,n_simu) 

# We add a new column for obs_id to cover the 168 hrs. This is now the tibble for entrainment conditions.
phase_simulation <- phase %>% uncount(count) %>% group_by(obs_id) %>% mutate(new_obs_id=new_obs) %>%  mutate(obs_id=(new_obs_id-1)*24+obs_id)

# In order to get random subsets from the simulation of, say 1000, possibilities, we use a function.
# Function from (https://github.com/tidyverse/dplyr/issues/361)
sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

# The plotting function, which also allows for the choice of subset. 
Choose_subset_and_plot <- function(df,k,...) { # df, datafame, k a number, the size of subset
  arguments <- list(...)
  sampling_k <- df %>% group_by(simu_id) %>% sample_n_groups(k) # choose the subsets
  sampling_k <- sampling_k %>% group_by(obs_id) %>% # For each calculate the mean over all timepoints
    mutate(mean_k= mean(sin_var_period))
  plot <- ggplot()+aes(x=obs_id, y=sin_var_period, colour=simu_id)+ # basic plot
  geom_line(data=sampling_k,alpha=0.5) + # makes the different colour lines of each simulation, with opaquety 0.5
  geom_line(data=sampling_k, aes(x=obs_id, y=mean_k), colour="black")+ # plots the mean line
  scale_x_continuous(breaks = seq(0, 168, by = 24))+ # PUTS THE LITTLE TICKS ON THE X-AXIS
  coord_fixed(ratio=10, xlim=c(0,168), ylim=c(1,3))+ # SCALING THE PLOTS RATIOS
  labs(x="Time since start of experiment (h)")+ # THE X-LABEL
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ # THE BACKGROUND OF THE GRAPH, NO GRID
  theme(axis.line = element_line(colour = "black"))  + # DRAWING THE BLACK LINES AROUND THE PLOT
  theme(legend.position="none")+ # does not add the colour coding for each simulation
  scale_y_continuous("R.Expression",breaks = seq(1, 3, by = 1))+ # y-label, and numbers on y-axis
  theme(panel.background = element_rect(fill='#FFFFAC')) # background colour to yellow.
  return(plot)
}

#Here we create the individual figures for the panels with 1,10,50,100,1000 simulations
freerunning1 <- Choose_subset_and_plot(freerunning_simulation,1)
entrained1 <- Choose_subset_and_plot(phase_simulation,1)
freerunning10 <- Choose_subset_and_plot(freerunning_simulation,10)
entrained10 <- Choose_subset_and_plot(phase_simulation,10)
freerunning50 <- Choose_subset_and_plot(freerunning_simulation,50)
entrained50 <- Choose_subset_and_plot(phase_simulation,50)
freerunning100 <- Choose_subset_and_plot(freerunning_simulation,100)
entrained100 <- Choose_subset_and_plot(phase_simulation,100)
freerunning1000 <- Choose_subset_and_plot(freerunning_simulation,1000)
entrained1000 <- Choose_subset_and_plot(phase_simulation,1000)


# These are combined for a grid and saved
free_vs_entrained <- grid.arrange(freerunning1,entrained1,freerunning10,entrained10,freerunning50,entrained50,freerunning100,
             entrained100,freerunning1000,entrained1000,nrow=5)
ggsave(free_vs_entrained, file=paste0("Figure4.pdf"), width = 20, height = 20, units = "cm")
