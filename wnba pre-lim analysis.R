library(ggplot2)
library(tidyr)
library(plotly)
source("wnba clean v4.R")

# Get unique seasons
seasons <- unique(df$Season)

# Initialize a list to store plots
plots <- list()

# Loop through each season to create and store the plots
for (season in seasons) {
  plot_name <- paste0(season, "_PTS_Plot")
  
  # Filter data for the current season
  df_season <- df %>% filter(Season == season)
  
  # Create the plot
  plot <- ggplot(df_season, aes(x = games_into_season)) +
    geom_point(aes(y = PTS, color = "PTS")) +
    geom_line(aes(y = MAV3_lag_PTS, color = "MAV3_lag_PTS")) +
    geom_line(aes(y = bl_PTS, color = "bl_PTS")) +
    facet_wrap(~ Team, ncol = 4) +  # Adjust ncol to arrange panes differently if needed
    labs(title = paste("Season:", season), y = "Value", color = "Metric") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Store the plot in a named variable
  assign(plot_name, plot)
  
  # Optionally, store the plot in a list
  plots[[plot_name]] <- plot
}

# Access the plots
print(plots)



#general spread of PTS per team
var_sum <- df %>%
  group_by(Team,Season) %>%
  summarise(Avg_PTS = mean(PTS),
            sd_PTS = sd(PTS))

ggplot(var_sum, aes(x = Season, y = Avg_PTS, fill = Team)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Avg_PTS - sd_PTS, ymax = Avg_PTS + sd_PTS), 
                width = 0.2, 
                position = position_dodge(0.9)) +
  theme_minimal() +
  labs(title = "Average PTS by Team and Season",
       x = "Season",
       y = "Average PTS") +
  facet_wrap(~ Team, ncol = 4) +
  theme(legend.position = "none")  # Hides the legend, as facets make it redundant

#indicators for better/worse performances


