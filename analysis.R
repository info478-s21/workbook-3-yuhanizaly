# Analysis

# Set up - make sure to set your working directory using RStudio
library(tidyr)
library(dplyr)
library(ggplot2)

# Create the `charts/` directory (you can do this from R!)
dir.create("charts", showWarnings = FALSE)

# Load prepped data
health_data <- read.csv("./data/prepped/all_data.csv")

# Are HALE and life expectancy correlated?
# - Plot 2016 life expectancy against 2016 HALE. Save the graph to `charts/`
# - Compute the correlation between 2016 life expectancy against 2016 HALE

data_2016 <- health_data %>%
  filter(year == 2016)

ggplot(data_2016) +
  geom_point(mapping = aes(x = le, y = hale)) +
  labs(title = "Life Expextancy vs HALE",
       x = "Life Expectancy",
       y = "HALE"
       )
ggsave("charts/le_hale_graph.png")
cor(data_2016$hale, data_2016$le)

# Yes, HALE and life expectancy are positively correlated because the data points
# on the graph shows a strong positive correlation of 0.999.

# Are HALE and DALYs correlated?
# - Plot 2016 HALE against 2016 DALYs. Save the graph to `charts/`
# - Compute the correlation between 2016 HALE and DALYs

ggplot(data_2016) +
  geom_point(mapping = aes(x = dalys, y = hale)) +
  labs(title = "DALYs vs HALE",
       x = "DALY",
       y = "HALE"
  )
ggsave("charts/dalys_hale_graph.png")
cor(data_2016$daly, data_2016$hale)

# Yes, HALE and DALYs are negatively correlated because the graph shows a
# negative slope with a strong correlation of -0.989

# As people live longer, do they live healthier lives
# (i.e., is a smaller fraction of life spent in poor health)?
# Follow the steps below to attempt to answer this question.


# First, you will need to reshape the data to create columns *by metric-year*
# This will create `hale_2016`, `hale_1990`, `le_2016`, etc.
# To do this, I suggest that you use the `pivot` function in the new
# tidyverse release:https://tidyr.tidyverse.org/articles/pivot.html#wider

data_wide <- health_data %>%
  pivot_wider(names_from = year,
              values_from = c(hale, le, dalys))

# Create columns to store the change in life expectancy, and change in hale
data_wide <- data_wide %>%
  mutate(hale_diff = hale_2016 - hale_1990,
  le_diff = le_2016 - le_1990)

# Plot the *change in hale* against the *change in life expectancy*
# Add a 45 degree line (i.e., where x = y), and save the graph to `charts/`
# What does this mean?!?! Put your interpretation below

change_hale_vs_le <- ggplot(data_wide) +
  geom_point(mapping = aes(x = le_diff, y = hale_diff)) +
  labs(title = "Life Expectancy difference vs HALE difference",
       x = "Change in Life Expectancy",
       y = "Change in HALE") +
  geom_abline(intercept = 0, slope = 1) +
  xlim(-15, 20) +
  ylim(-15, 20)


ggsave("charts/change_plot.png")


## The LE difference VS HALE difference graph shows that the difference of LE
## between 2016 and 1990 and the difference of HALE between 2016 and 1990 have a
## strong positive correlation because the data points are very close together 
## and close to the linear line. This means that majority of the countries show 
## almost no difference after LE gets adjusted to account for the 
## disease/disability to become HALE. In other words, change in life expectancy 
## is closely equal to the change in HALE. Something interesting is that while 
## people expect to live longer lives, after LE is adjusted, the years of the 
## healthy lives they have is lower than the expectations. 



