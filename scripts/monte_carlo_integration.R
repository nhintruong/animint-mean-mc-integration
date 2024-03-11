library(ggplot2)
library(animint2)

f <- function(x) {
  return(x-x^2)
}

generate_sample_mean <- function(n) {
  x_values <- runif(n, min = 0, max = 1)
  f_values <- f(x_values)
  sample_mean <- mean(f_values)
  return(list(sample_mean = sample_mean, x_values = x_values, f_values = f_values))
}

# Example
result <- generate_sample_mean(100)
print(result$sample_mean)
print(result$f_values)


prepare_data <- function(n) {
  result <- generate_sample_mean(n)
  df <- data.frame(
    step = 1:n,
    x_values = result$x_values,
    y_values = result$f_values,
    sample_mean = rep(result$sample_mean, n)
  )
  return(df)
}


# Example 
result <- prepare_data_step(20)
print(result)


# Create Animation
create_animation <- function(n) {
  # Prepare the data
  df <- prepare_data(n)
  
  static_plot <- ggplot(df, aes(x = x_values, y = y_values)) +
    geom_rect(aes(xmin = x_values, xmax = x_values + 1/n, ymin = 0, ymax = y_values), fill = "blue", alpha = 0.1) +
    stat_function(fun = f, color = "green", size = 1) +
    labs(title = "Sample Mean Monte Carlo Integration", x = "x values", y = "f(x) = x^2") +
    theme_bw() 
  
  # Create the animation
  ggplot_anim <- ggplot(df, aes(x = x_values, y = y_values, key = step)) +
    geom_rect(aes(xmin = x_values, xmax = x_values + 1/n, ymin = 0, ymax = y_values), fill = "blue", alpha = 0.5 , showSelected = "step") +
    geom_line(aes(x = x_values, y = sample_mean), color = "red", size = 2,showSelected = "step") +
    stat_function(fun = f, color = "green", size = 1) +
    labs(title = "Sample Mean Monte Carlo Integration", x = "x values", y = "f(x) = x - x^2") +
    theme_bw() 
    #+ coord_flip() 

  # Animate the plot
  viz.duration <- animint(ggplot_anim, duration = list(step = 500), source="", title="MonteCarloIntegration")
  
  viz.duration.time <- viz.duration
  viz.duration.time$time <- list(variable = "step", ms = 1000)
  # Return the animated plot
  return(viz.duration.time)
}

# Example usage
n <- 100
anim_result <- create_animation(n)
print(anim_result)
