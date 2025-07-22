# READ ANALYTICAL DATA FROM TTEB LABORATORY


# 1. READ CHEMISTRY DATA--------------
# Create dataframe of analyte detection and reporting limits
limits <- tibble(
  # Analyte vector
  analyte = c("al", "as", "ba", "be", "ca", "cd", "cr", "cu", "fe", "k", 
              "li", "mg", "mn", "na", "ni", "pb", "p", "sb", "si", "sn", 
              "sr", "s", "v", "zn", 
              "no2", "no3", "f", "br",  
              "cl", "so4", "toc", "doc"),
  
  # Detection limit vector
  detection_limit = c(0.004, 0.004, 0.001, 0.005, 0.010, 0.0003, 0.001, 0.001, 
                      0.001, 0.3, 0.005, 0.005, 0.001, 0.03, 0.001, 0.002, 
                      0.005, 0.003, 0.020, 0.001, 0.001, 0.003, 0.001, 0.0005,
                      0.005, 0.006, 0.007, 0.008, 0.03, 0.05, 0.05, 0.05),
  
  # Reporting limit vector
  reporting_limit = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 
                      0.5, 0.5, 0.5, 0.5, 0.5, 20.0, 0.5, 4.0, 2.0, 0.5, 20, 
                      0.5, 0.5, 0.05, 0.05, 0.05, 0.05, 0.5, 0.5, 1, 1),
)


tteb <- readr::read_csv("inputData/2024_campaign/puerto_rico_2024_chemistry_2025-07-17.csv") %>%
  janitor::clean_names() %>%
  # Remove PO4 (not used)
  #select(-po4i_cas_p) %>%
  rename_with(~ if_else( # rename any column names containing an underscore _
    str_detect(., "_"), str_extract(., "^[^_]*"), .)) %>% # keep chars before _
  rename_with(~ if_else( # rename any column names containing a number
    str_detect(., "[0-9]"), str_sub(., 1, 3), .)) %>% # keep first 3 chars
  select(-studyid, lab_id = labid) %>% # remove unneeded columns
  
  # a value of 9999999999999990.000 indicates no data for that sample/analyte.
  # This often occurs if a summary file contains samples with different
  # requested analytes.  For example, the Beaulieu file contains samples that did
  # not request metals (e.g. Falls Lake (FL), 2018 SuRGE samples (LVR, PLR))
  # and samples that did (e.g. 2020 SuRGE samples).  Samples that did not request
  # metals have values of 9999999999999990.000 for all metals analytes.
  # However, a value of 9999999999999990.000 may also indicate that the analyte
  # was outside of the standard curve and was rerun, but the summary file wasn't
  # updated with re-run value.  This is the case for labid 203173.
  mutate(across(where(is.numeric), # replace lab's placeholder numbers with 'NA'
                ~ na_if(., 9999999999999990.000))) %>%
  
  # pivot longer to apply flags 
  pivot_longer(cols = where(is.numeric) & !lab_id, names_to = "analyte") %>% 
  left_join(limits, by = "analyte") %>% 
  mutate(
    qual = case_when(
      str_detect(flag, "H") ~ "H",
      .default = ""), 
    flag = case_when(
      value < detection_limit ~ "ND", 
      .default = ""), 
    bql = case_when(
      value >= detection_limit & value < reporting_limit ~ "L",
      .default = ""), 
    value = abs(value)) %>% 
  unite(flags, flag, bql, qual, sep = " ") %>%
  mutate(flags = str_squish(flags)) %>%
  # remove _limit columns 
  select(-detection_limit, -reporting_limit) %>%
  # Pivot wider with analytes and _flags as columns 
  pivot_wider(
    names_from = analyte,
    values_from = c(value, flags),
    names_glue = "{analyte}_{.value}") %>%
  # Get rid of "_value" in column names
  rename_with(~ str_remove(., "_value"))



# 2. READ CHAIN OF CUSTODY----------------
# Read in chain on custody data for 2024 Puerto Rioc samples submitted to TTEB
# See inputData/2024_campaign/tteb.....xlsx