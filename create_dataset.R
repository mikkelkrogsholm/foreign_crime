# Load neeeded libraries
library(tidyverse)

# The tables are manually downloaded from statbank.dk and not
# via the api, because the api doesn't segment into west and 
# nonwest

# Read in the csv files fron Statistics Denmark
strafna3 <- read_delim("strafna3.csv", ";", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
names(strafna3) <- c("year", "dk", "west", "nonwest")

folk2 <- read_delim("folk2.csv", ";", escape_double = FALSE,col_names = FALSE, trim_ws = TRUE)
names(folk2) <- c("year", "dk", "west", "nonwest")

# Convert the data to long format
strafna3_long <- strafna3 %>% gather(geo, conv_count, -year)
folk2_long <- folk2 %>% gather(geo, pop_count, -year)

# Join the datasets together
joined_data <- inner_join(strafna3_long, folk2_long)

# Perform calculations
# 1) calculate share of convicted within each geo-segment
# 2) index this share to compare development within each
joined_data <- joined_data %>% 
  mutate(pct = conv_count / pop_count * 100) %>%
  group_by(geo) %>%
  mutate(index = pct / pct[1] * 100) %>%
  ungroup()

# Write the data to disk for others to use
write_csv(joined_data, "dataset.csv")