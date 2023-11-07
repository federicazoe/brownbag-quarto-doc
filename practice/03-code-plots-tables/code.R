
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(kableExtra)


# Load data ---------------------------------------------------------------

experiment <- read_csv("data/data.csv")
subjects <- experiment %>% 
  distinct(subject, italian)

experiment <- experiment %>%
  mutate(veggie_label = factor(
    veggie_label,
    levels = c("standard",
               "organic",
               "sustainable",
               "organic-sustainable")
  ))


# Compute number of subjects and italian percentage -----------------------
nrow(subjects)
round(mean(subjects$italian)*100)

# Median and mode of canteen attendance  ----------------------------------

median_attendance <- experiment %>%
  group_by(subject) %>%
  summarize(N = n()) %>% 
  pull(N) %>% 
  median()

max_attendance <- experiment %>%
  group_by(subject) %>%
  summarize(N = n()) %>% 
  pull(N) %>% 
  max()


# Histogram of number of visits -------------------------------------------

experiment %>%
  group_by(subject) %>%
  mutate(N = n()) %>%
  distinct(subject, N) %>%
  ggplot() +
  aes(x = N) +
  geom_histogram(binwidth = 1,
                 color = "white",
                 fill = "#8174a1") +
  theme_bw() +
  labs(x = "Number of days a student attended the canteen",
       y = "Count") +
  scale_x_continuous(limits = c(0, 31),
                     breaks = seq(1, 31, by = 2))


# Table of veggie orders percentage by veggie label -----------------------

veg_orders_summary <- experiment %>% 
  group_by(veggie_label) %>% 
  summarize(
    proportion_veg = mean(response)*100,
    n_orders = length(response)
  )

knitr::kable(
  veg_orders_summary,
  format = "latex",
  escape = FALSE,
  booktabs = TRUE,
  col.names = c(
    "Label",
    "Vegetable plate (\\%)",
    "Total orders"
  ),
  align =  c("c", "c"),
  digits = 2
) %>%
  kable_styling(latex_options = "HOLD_position")

