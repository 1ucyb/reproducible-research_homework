# Loads in packages

library(ggplot2)
library(gridExtra)

# Sets seed to ensure reproducible results

set.seed(7)

# Function to define a random walk

random_walk = function(n_steps){ #n_steps is number of steps
  
  # Creates a dataframe to store the random walk:
  # x and y will store the co-ordinates of each step
  # while t contains the time, or step number.
  
  df = data.frame(x = rep(NA, n_steps), y = rep(NA, n_steps), time = 1:n_steps)
  
  # Sets the first point of the walk to the origin.
  
  df[1, ] = c(0, 0, 1)
  
  # Loop that creates the walk, starting at step 2 as step 1 is the origin.
  
  for (i in 2:n_steps) {
    h = 0.25 # Defines the length of a run
    angle = runif(1, min = 0, max = 2 * pi) # Generates a random angle in radians
    df[i, 1] = df[i - 1, 1] + cos(angle) * h # Works out new x value
    df[i, 2] = df[i - 1, 2] + sin(angle) * h # Works out new y value
  }
  
  return(df)
}

# Plots two identical random walks

data1 = random_walk(500)

plot1 = ggplot(aes(x = x, y = y), data = data1) +
  geom_path(aes(colour = time)) +
  theme_bw() +
  xlab("x-coordinate") +
  ylab("y-coordinate")

data2 = random_walk(500)

plot2 = ggplot(aes(x = x, y = y), data = data2) +
  geom_path(aes(colour = time)) +
  theme_bw() +
  xlab("x-coordinate") +
  ylab("y-coordinate")

grid.arrange(plot1, plot2, ncol = 2)