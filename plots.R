# Load needed libraries
library(tidyverse)
options(scipen = 999)

# Read in the dataset create in the "create_dataset.R" script.
dataset <- read_csv("dataset.csv")

# Change the geo volume to factors for better labelling in the plots.
dataset$geo <- factor(dataset$geo)
levels(dataset$geo)[levels(dataset$geo)=="dk"] <- "Danmark"
levels(dataset$geo)[levels(dataset$geo)=="west"] <- "Vestlige lande"
levels(dataset$geo)[levels(dataset$geo)=="nonwest"] <- "Ikke-Vestlige lande"

# Plot overall count ----
(gg_count <- ggplot(dataset) + 
  geom_line(aes(year, conv_count, color = geo)) + 
  theme_minimal() + 
  theme(legend.position = "top") +
  scale_color_discrete(name = "Oprindelsesland") +
  scale_y_continuous(
    breaks = seq(0, 150000, by = 25000),
    labels = seq(0, 150000, by = 25000) %>% prettyNum(big.mark = ".", decimal.mark = ",")) +
  labs(y = "Antal", x = "", 
       title = "Antal af dømte i befolkningen efter oprindelsesland",
       subtitle = "Inddelt i Danmark, Vestlige lande og Ikke-Vestlige lande som defineret af DST.",
       caption = "Kilde: Danmarks Statistik (DST), FOLK2 + STRAFNA3"))

# Plot overall share ----

# Do a little calculation to get each segments share of total
# population
dataset2 <- dataset %>%
  select(year, geo, conv_count, pop_count) %>%
  group_by(year) %>%
  mutate(all_pop = sum(pop_count)) %>%
  ungroup() %>%
  select(-pop_count) %>%
  mutate(pct = conv_count / all_pop * 100)
  
(gg_share <- ggplot(dataset2) + 
    geom_line(aes(year, pct, color = geo)) + 
    theme_minimal() + 
    theme(legend.position = "top") +
    scale_color_discrete(name = "Oprindelsesland") +
    scale_y_continuous(
      breaks = seq(0, 2.75, by = .25),
      labels = seq(0, 2.75, by = .25) %>% prettyNum(big.mark = ".", decimal.mark = ",")) +
    labs(y = "Andel", x = "", 
         title = "Andel af dømte i befolkningen efter oprindelsesland",
         subtitle = "Inddelt i Danmark, Vestlige lande og Ikke-Vestlige lande som defineret af DST.",
         caption = "Kilde: Danmarks Statistik (DST), FOLK2 + STRAFNA3"))

# Plot within share ----
(gg_share_within <- ggplot(dataset) + 
   geom_line(aes(year, pct, color = geo)) + 
   theme_minimal() + 
   theme(legend.position = "top") +
   scale_color_discrete(name = "Oprindelsesland") +
   scale_y_continuous(
     breaks = seq(0, 5.75, by = .5),
     labels = seq(0, 5.75, by = .5) %>% prettyNum(big.mark = ".", decimal.mark = ",")) +
   labs(y = "Andel", x = "", 
        title = "Andel af dømte i egen befolkningsgruppe efter oprindelsesland",
        subtitle = "Inddelt i Danmark, Vestlige lande og Ikke-Vestlige lande som defineret af DST.",
        caption = "Kilde: Danmarks Statistik (DST), FOLK2 + STRAFNA3"))

# Plot within latest trend ----
(gg_share_within_index <- 
   ggplot(dataset) + 
   geom_line(aes(year, index, color = geo)) + 
   theme_minimal() + 
   theme(legend.position = "top") +
   scale_color_discrete(name = "Oprindelsesland") +
   scale_y_continuous(
     breaks = seq(80, 140, by = 10),
     labels = seq(80, 140, by = 10) %>% prettyNum(big.mark = ".", decimal.mark = ",")) +
   labs(y = "Indeks (år 2000 = 100)", x = "", 
        title = "Indeks af andel af dømte i egen befolkningsgruppe efter oprindelsesland",
        subtitle = "Inddelt i Danmark, Vestlige lande og Ikke-Vestlige lande som defineret af DST.",
        caption = "Kilde: Danmarks Statistik (DST), FOLK2 + STRAFNA3"))

