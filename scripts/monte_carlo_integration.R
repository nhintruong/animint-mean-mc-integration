library(ggplot2)
library(animint2)
library(lapply)

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
    sample_mean = rep(result$sample_mean, n),
    area_integrated = rep(0, n)
  )
  
  for (i in 0:(n-1)) {
    x1 <- df$x_values[i]
    x2 <- df$x_values[i+1]
    y1 <- df$y_values[i]
    y2 <- df$y_values[i+1]
    area <- 0.5 * (x2 - x1) * (y1 + y2)
    df$area_integrated[i] <- area
  }
  
  df$cumulative_area <- cumsum(df$area_integrated)
  
  return(df)
}

# Example 
result <- prepare_data(20)
print(result)


# Create Animation
create_animation <- function(n) {
  # Prepare the data
  df <- prepare_data(n)
  
  # Create the animation
  viz.one <- ggplot(df, aes(x = x_values, y = y_values, key = step)) +
    geom_rect(aes(xmin = x_values, xmax = x_values + 1/n, ymin = 0, ymax = y_values), 
              fill = "blue", 
              alpha = 0.6,
              showSelected="step") +
    stat_function(fun = f, color = "green", size = 1) +
    labs(title = "Sample Mean Monte Carlo Integration", x = "x values", y = "f(x) = x - x^2") +
    theme_bw() 

  # Animate the plot
  viz.two <- ggplot(df, aes(x = x_values, y = y_values, key = step)) +
    ggtitle("Monte Carlo Estimation") +
    geom_rect(aes(xmin = x_values, xmax = x_values + 1/n, ymin = 0, ymax = y_values),
                  clickSelects="step",
                  alpha=0.6,
                  fill="black") +
    stat_function(fun = f, color = "green", size = 1) + 
    labs(x="x values", y="Area Integrated") +
    theme_bw() 
  # theme_animint(width=600, height=600)
  
  viz.three <- ggplot(df, aes(x = step, y = cumulative_area, key = step)) +
    ggtitle("Monte Carlo Estimation") +
    geom_line(color="red") +
    geom_tallrect(aes(xmin = step-0.5, xmax = step + 0.5, ymin = 0, ymax = y_values),
              clickSelects="step",
              showSelected = "step") +
    labs(x="steps", y="cumulative Area Integrated") +
    theme_bw() 
    # theme_animint(width=600, height=600)

  viz.publish <- animint(viz.one,
                         viz.two,
                         viz.three,
                         title="Hit or Miss Monte Carlo Integration",
                         time=list(variable="step", ms=500),
                         first=list(step=1),
                         out.dir='./output'
                         )
  
  
  return(viz.publish)
}

# Example usage
n <- 200
anim_result <- create_animation(n)
print(anim_result)


