# Initialisation ----------------------------------------------------------

rm(list = ls())

# Functions ---------------------------------------------------------------

plot_curve <- function(df, BL_bounds = c(), ATP_bounds = c(), ATP_peak = NA, Iono_peak = NA){
  # Plot curve
  #
  # Args:
  # df: dataframe for one experiment
  # BL_bounds: Basal level time bounds
  # ATP bounds: ATP response time bounds
  # ATP_peak: Time of ATP peak
  # Iono_peak: Time of Ionomycin peak
  #
  # Returns:
  # Ggplot
  
  library(ggplot2)

  ggplot() +
    geom_line(data = df, aes(x = t, y = y),
              size = 2) +
    geom_area(data = subset(df, t >= min(BL_bounds) &  t <= max(BL_bounds)),
              aes(x = t, y = y),
              fill = "#009E73", alpha = .5) + # green (basal level)
    geom_area(data = subset(df, t >= min(ATP_bounds) &  t <= max(ATP_bounds)),
              aes(x = t, y = y),
              fill = "#56B4E9", alpha = .5) + # blue (ATP response)
    geom_point(data = df[which.min(abs(df$t - ATP_peak)), ],
               aes(x = t, y = y),
               size = 4, colour = "#F0E442") + # yellow (ATP peak)
    geom_point(data = df[which.min(abs(df$t - Iono_peak)), ],
               aes(x = t, y = y),
               size = 4, colour = "#E69F00") + # orange (Iono peak)
    labs(x = "Time", y = "Ratio") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_cartesian(ylim = c(0, round(max(df$y) * 1.05, 1))) +
    theme_classic(base_size = 20)
}

compute_stats <- function(df, BL_bounds = c(), ATP_bounds = c(), ATP_peak = NA, Iono_peak = NA){
  #
  #
  # Args:
  # df: dataframe for one experiment
  # BL_bounds: Basal level time bounds
  # ATP bounds: ATP response time bounds
  # ATP_peak: Time of ATP peak
  # Iono_peak: Time of Ionomycin peak
  #
  # Returns:
  #
  
  out <- data.frame(Experiment = unique(df$Experiment)[1])
  
  if(length(unique(BL_bounds)) != 2){
    tmp <- NA
  }else{
    tmp <- mean(subset(df, t >= min(BL_bounds) &  t <= max(BL_bounds))$y)
  }
  out[, "Basal level"] <- tmp
  
  if(length(unique(ATP_bounds)) != 2){
    tmp <- NA
  }else{
    tmp <- abs(diff(ATP_bounds))
  }
  out[, "ATP response duration"] <- tmp
  
  if (is.na(ATP_peak)){
    tmp <- NA
  }else{
    tmp <- df$y[which.min(abs(df$t - ATP_peak))]
  }
  out[, "ATP peak"] <- tmp

  if (is.na(Iono_peak)){
    tmp <- NA
  }else{
    tmp <- df$y[which.min(abs(df$t - Iono_peak))]
  }
  out[, "Ionomycin peak"] <- tmp

  return(out)
}

# Data --------------------------------------------------------------------

file <- "test.csv"

# df <-  readxl::read_excel(file)
df <- read.csv(file, header = TRUE, sep = ";")
colnames(df) <- c("t", 1:(ncol(df) - 1))
df <- reshape2::melt(df, id.vars = "t", variable.name = "Experiment", value.name = "y")

# Example -----------------------------------------------------------------

plot_curve(subset(df, Experiment == 2),
           BL_bounds = c(0, 5),
           ATP_bounds = c(7, 10),
           ATP_peak = 8,
           Iono_peak = 12)

compute_stats(subset(df, Experiment == 2),
              BL_bounds = c(0, 5),
              ATP_bounds = c(7, 10),
              ATP_peak = 8,
              Iono_peak = NA)

