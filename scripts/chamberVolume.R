# CALCULATE CHAMBER VOLUMES

# Calculate chamber volume based on relationship between water level
# and volume.  See chamberDesign.xlsx in projectDocuments/equipment.

# chmVol.L = (42.057 + (-0.2189 * mean(chamb_grad_a, chamb_grad_b, na.rm = TRUE))))

# JC 3/13/2023: I think this produces the desired outcome:
fld_sheet  <- fld_sheet %>% 
  mutate(chmVol.L = (42.057 + (-0.2189 * rowMeans(
    select(., chamb_grad_a, chamb_grad_b), na.rm = TRUE)))) %>%
  # NaN may cause errors? 
  mutate(chmVol.L = if_else(is.na(chmVol.L), NA_real_, chmVol.L))