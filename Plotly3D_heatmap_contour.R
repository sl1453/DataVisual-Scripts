# Installing and loading required packages
install.packages("plotly")
library(plotly)

distance <- seq(0, 10, by=1)
time <- seq(10, 1, by=-1)

signal_sample1 <- outer(time, distance, FUN = function(t, d) {
  a = 6 + sin((t*pi/10)-pi) * (5) # Amplitude varying with time
  b = 5 + sin(t*pi/6) * (4)  # Adjusting center of the peak with time. The value 4 adjusts the amplitude of movement.
  return(gaussian(d, a, b, c))
})

plot_ly() %>%
  add_surface(x = time, y = distance, z = t(signal_sample1), colorscale = list(c(0, 1), c("white", "blue"))) %>%
  layout(title = "Sample 1", scene = list(xaxis = list(title = "Time"),
                                          yaxis = list(title = "Distance"),
                                          zaxis = list(title = "Signal")))
plot_ly() %>%
  add_surface(x = time, y = distance, z = t(signal_sample2), colorscale = list(c(0, 1), c("white", "red"))) %>%
  layout(title = "Sample 2", scene = list(xaxis = list(title = "Time"),
                                          yaxis = list(title = "Distance"),
                                          zaxis = list(title = "Signal")))
####################################################################################################################

# Common time and distance scales for both datasets
time <- seq(1, 10, by=1)
distance <- seq(0, 10, by=1)

# Dataset 1

## Signal intensity due to time (linear relationship)
signal_time <- time

## Signal intensity due to distance (bell-shaped curve)
signal_distance <- exp(-((distance - mean(distance))^2) / (2 * var(distance)))

## Adjusting signal for the time-distance relationship
# We'll use a Gaussian curve for the time based on the distance
time_function <- function(d) {
  amplitude = max(time)
  center = mean(distance)
  width = var(distance)
  
  return(amplitude * exp(-((d - center)^2) / (2 * width)))
}

# Vectorize the function
v_time_function <- Vectorize(time_function)

signal_time_adjusted <- outer(distance, time, FUN = function(d, t) {
  return(t * v_time_function(d))
})

## Combined signal using the combination method
signal_combined_1 <- (signal_time_adjusted + signal_distance) / 2

# Dataset 2
## Signal intensity due to time (linear relationship)
# Dataset 2
## Signal intensity due to time (linear relationship)
signal_time_2 <- time

## Adjusting signal for the time-distance relationship
# We'll use random time based on the distance
random_time_values <- runif(length(distance), min = min(time), max = max(time))

signal_time_adjusted_2 <- outer(distance, time, FUN = function(d, t) {
  return(t * random_time_values)
})

## Signal intensity due to distance (random)
signal_distance_2 <- runif(length(distance), 
                           min = min(signal_time_adjusted_2),
                           max = max(signal_time_adjusted_2))

## Combined signal using the combination method
signal_combined_2 <- (signal_time_adjusted_2 + signal_distance_2) / 2


# 3-D Plots
plot1 <- 
  plot_ly(z=~signal_combined_1, x=~time, y=~distance, type="surface", colorscale="Viridis") %>%
  layout(title = "hypothetical_wt",scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance'),
                      zaxis = list(title = 'Signal Intensity')))  # Dataset 1
Plot2 <- 
  plot_ly(z=~signal_combined_2, x=~time, y=~distance, type="surface", colorscale="Viridis") %>%
  layout(title = "hypothetical_mutant", scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance'),
                      zaxis = list(title = 'Signal Intensity')))  # Dataset 2
# Heatmaps
plot3 <- 
  plot_ly(z=~signal_combined_1, x=~time, y=~distance, type="heatmap", colorscale="Viridis",
          colorbar = list(title = "hypothetical\nwt_signal", y = 1.05))%>%
  layout(scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance')))
plot4 <- 
  plot_ly(z=~signal_combined_2, x=~time, y=~distance, type="heatmap", colorscale="Viridis",
          colorbar = list(title = "hypothetical\nmutant_signal",y=5.15))%>%
  layout(scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance')))
#contour
plot5 <- 
  plot_ly(x = ~time, 
         y = ~distance, 
         z = ~signal_combined_1,
         type = "contour",
         colorscale="Viridis",
         colorbar = list(title = "hypothetical\nwt_signal", y = 1.05))%>%
  layout(scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance')))

plot6 <- 
  plot_ly(x = ~time, 
        y = ~distance, 
        z = ~signal_combined_2,
        type = "contour",
        colorscale="Viridis",
        colorbar = list(title = "hypothetical\nmutant_signal",y = 5.15))%>%
  layout(scene = list(xaxis = list(title = 'Time'),
                      yaxis = list(title = 'Distance')))
plot1
Plot2
subplot(plot3, plot4, margin = 0.1)
subplot(plot5, plot6, margin = 0.1)
  