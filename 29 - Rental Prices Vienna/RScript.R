# Libraries
library(tidyverse)
library(tidytext)
library(lubridate)
library(broom)
library(scales)
library(ggrepel)

# Working Directory To Source File Location
setwd(dirname(rstudioapi::getActiveDocumentContext()[[2]]))

# Chart Theme
theme_set(theme_bw() +
            theme(plot.title = element_text(size = 14, face = "bold"),
                  plot.subtitle = element_text(size = 10, face = "italic",
                                               colour = "grey50")))

# Loading Data
data <- read_csv("Willhaben_everything_clean.csv")

data

# General EDA
data %>% 
  select(price, sqm) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~ name, scales = "free") +
  scale_x_log10()

data %>% 
  ggplot(aes(sqm)) +
  stat_ecdf() +
  geom_vline(xintercept = 55) +
  scale_x_log10()

# Price distribution by postcode
data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  group_by(postcode) %>% 
  mutate(postcode = paste0(postcode, " (n=", n(), ")")) %>% 
  ggplot(aes(x = price,
             y = postcode %>% reorder(price))) +
  geom_boxplot(outlier.color = NA) +
  coord_cartesian(xlim = c(0, 6000)) +
  labs(title = "Monthly Rent in Vienna by Postcode",
       subtitle = "Number of appartments shown in brackets.",
       y = NULL, x = "Monthly Rent") +
  scale_x_continuous(labels = comma_format(suffix = "€"))

# Price distribution by landlord
data %>% 
  filter(landlord %in% (data %>% 
                          count(landlord, sort = T) %>% 
                          head(20) %>% 
                          pull(landlord))) %>%
  group_by(landlord) %>% 
  mutate(landlord = paste0(landlord, " (n=", n(), ")")) %>% 
  ungroup() %>% 
  ggplot(aes(x = price,
             y = landlord %>% reorder(price))) +
  geom_boxplot(outlier.color = NA) +
  coord_cartesian(xlim = c(0, 2500)) +
  labs(title = "Monthly Rent in Vienna by Landlord",
       subtitle = "Number of appartments shown in brackets.\nOnly showing the 20 most common landlords.",
       y = NULL, x = "Monthly Rent") +
  scale_x_continuous(labels = comma_format(suffix = "€"))
  
# Number of appartments by postcode
data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  count(postcode) %>% 
  ggplot(aes(x = postcode,
             y = n)) +
  geom_col() +
  labs(title = "Vienna Rented Appartments",
       y = "Number of Appartments", x = "Postcode") +
  scale_x_continuous(breaks = seq(1010, 1230, 10)) +
  theme(panel.grid.minor.x = element_blank())

# Average rent by postcode
data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  group_by(postcode) %>% 
  summarise(price = mean(price, na.rm = T),
            sqm = mean(sqm, na.rm = T)) %>% 
  ggplot(aes(x = postcode)) +
  geom_col(aes(y = price)) +
  geom_text(aes(y = price, label = paste0(round(sqm, 0), "m2")),
            nudge_y = 100, size = 3) +
  labs(title = "Vienna Average Rent Prices By Postcode",
       subtitle = "Average appartment size shown above bars",
       y = "Average Rent", x = "Postcode") +
  scale_x_continuous(breaks = seq(1010, 1230, 10)) +
  theme(panel.grid.minor.x = element_blank())

# Price per square metre by postcode, size of flats by postcode
data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  mutate(price_sqm = price/sqm) %>% 
  ggplot(aes(x = price_sqm,
             y = postcode %>% reorder(price_sqm))) +
  geom_boxplot(outlier.color = NA) +
  coord_cartesian(xlim = c(5, 40)) +
  labs(title = "Distribution of Monthly Rent per Square Metre in Vienna",
       subtitle = "Postcodes are ordered by highest to lower price",
       y = NULL, x = NULL) +
  scale_x_continuous(labels = comma_format(suffix = "€/m2"))

data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  mutate(price_sqm = price/sqm) %>% 
  ggplot(aes(y = price_sqm,
             x = postcode,
             group = postcode)) +
  geom_boxplot(outlier.color = NA) +
  coord_cartesian(ylim = c(0, 40)) +
  labs(title = "Distribution of Monthly Rent per Square Metre in Vienna",
       subtitle = "Postcodes are ordered ascendingly",
       y = NULL, x = "Postcode") +
  scale_x_continuous(breaks = seq(1010, 1230, 10)) +
  scale_y_continuous(labels = comma_format(suffix = "€/m2")) +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())


# Average price and size correlation
data %>% 
  filter(substr(postcode, 1, 1) == 1) %>% 
  group_by(postcode) %>% 
  summarise(price = median(price, na.rm = T),
            sqm = median(sqm, na.rm = T)) %>% 
  ggplot(aes(x = sqm)) + 
  geom_point(aes(y = price), colour = "dodgerblue", alpha = 0.5,
             size = 3) +
  # geom_text(
  #   # data = data %>% 
  #   #           filter(postcode == 1080) %>% 
  #   #           group_by(postcode) %>% 
  #   #           summarise(price = mean(price, na.rm = T),
  #   #                     sqm = mean(sqm, na.rm = T)), 
  #           aes(y = price, label = postcode), size = 3) +
  geom_text_repel(aes(y = price, label = postcode), size = 3,
                  force = 20, alpha = 0.8) +
  geom_smooth(aes(y = price), method = "lm", se = F, size = 0.75,
              colour = "grey50") +
  labs(title = "Vienna: Relationship of Median Price and Size",
       x = "Size", y = "Price") +
  scale_y_continuous(labels = comma_format(suffix = "€")) +
  scale_x_continuous(labels = comma_format(suffix = "m2"),
                     breaks = seq(50, 100, 10))

# Correlation of sqm/rooms and price
data %>% 
  filter(substr(postcode, 1, 1) == 1,
         sqm < 250) %>% 
  filter(postcode %in% seq(1010, 1090, 10)) %>% 
  ggplot(aes(x = sqm,
             y = price)) +
  geom_point(alpha = 0.5, size = 1, colour = "dodgerblue") +
  geom_smooth(method = "loess", se = F, size = 0.5) +
  geom_hline(yintercept = 915, lty = "dashed", size = 0.25) +
  geom_vline(xintercept = 52, lty = "dashed", size = 0.25) +
  labs(title = "Januar 2023: Mietpreise in Wien nach Postleitzahl",
       subtitle = "Quelle: Gescraped von willhaben.at | Vergleich mit 52m2 Wohnung für 915€",
       y = "Mietpreis", x = "Wohnungsgrösse") +
  facet_wrap(~ postcode, scales = "free") +
  scale_x_continuous(labels = comma_format(suffix = "m2")) +
  scale_y_continuous(labels = comma_format(suffix = "€")) +
  theme(legend.position = "none",
        panel.grid.minor.y = element_blank())