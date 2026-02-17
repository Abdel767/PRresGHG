library(tidyverse)
library(janitor)
library(broom)

dat <- readr::read_csv("IncubationData/IncRates_vs_DistanceFromTributaries.csv") %>%
  janitor::clean_names() %>%   # simplify names
  select(-bottle_label, -zone) %>% # omit unneeded variables
  pivot_longer(!distance_from_tributary_m) %>% # pivot to long
  mutate(
    # simplify names
    name = case_when(
      name == "ch4_umol_g_dw_d" ~ "ch4_dw",
      name == "ch4_umol_g_afdm_d" ~ "ch4_afdm",
      name == "co2_umol_g_dw_d" ~ "co2_dw",
      name == "co2_umol_g_afdm_d" ~ "co2_afdm",
      TRUE ~ "FLY YOU FOOLS"
    ),
    # create a new column that will be used for faceting
    gas = case_when(
      grepl("ch4", name) ~ "ch4",
      grepl("co2", name) ~ "co2"
    )
  ) %>%
  # normalize data to range from 0 -1 for each rate. That will allow
  # direct comparison between DW and AFDM normalised rates
  group_by(name) %>%
  mutate(
    value_normalized = (value - min(value)) / (max(value) - min(value))
  ) %>%
  ungroup() 

# plot data for inspection
dat %>%
  ggplot(aes(distance_from_tributary_m, value_normalized, color = name)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~gas)


# hypothesis: slope of AFDM normalized rates exceed that of DM normalized
# due to increased C lability with distance from tributary. This would be
# identified as an interaction between 

# 1. ch4 data first
ch4_dat <- dat %>%
  filter(gas == "ch4") 

ch4_model <- lm(value_normalized ~ distance_from_tributary_m * name, 
                data = ch4_dat)


# distance is significant
# interaction is not, thus DM and AFDM respond the same way, contrary to our
# hypothesis.
anova(ch4_model)

# Nest we delete the interaction term and rerun the model
ch4_model_drop1 <- update(ch4_model, ~ . - distance_from_tributary_m:name)

# test again for significance of interaction
anova(ch4_model, ch4_model_drop1) # p = 0.88, interaction not significant

anova(ch4_model_drop1) # distance is significant, p = 0.002



# 1. co2 data first
co2_dat <- dat %>%
  filter(gas == "co2") 

co2_model <- lm(value_normalized ~ distance_from_tributary_m * name, 
                data = co2_dat)


# distance is significant
# interaction is not, thus DM and AFDM respond the same way, contrary to our
# hypothesis.
anova(co2_model)

# Nest we delete the interaction term and rerun the model
co2_model_drop1 <- update(co2_model, ~ . - distance_from_tributary_m:name)

# test again for significance of interaction
anova(co2_model, co2_model_drop1) # p = 0.75, interaction not significant

anova(co2_model_drop1) # distance is significant, p = 0.012

