# NUTRIENT ANALYSIS ON LACHAT CONDUCTED IN AWBERC

# Original data can be found at: L:\Priv\Cin\ORD\Pegasus-ESF\Lachat Data\Nutrients

# Need to create a "nutrients_qual" column to indicate holding time violations.
# Value of "HOLD" if holding time violated, else blank.  Holding time should be
# calculated as difference between "analyte_detection_date" and "collection_date".
# use flag columns (i.e. srp_flag, no2_3_flag) to indicate censored values.  

# The lab quantified inorganics (no2, no2.3, oP (named RP in awberc data file),
# and nh4) in both filtered and unfiltered samples.  We only want inorganic data
# for filtered samples.  Filtered samples can be identified by the "D" proceeding
# the sample collection depth in the 'pos $char4.' column (e.g., D-Sh).

# All analyte names begin with a "T" to indicate "total" (e.g. TNH4).  Ignore this
# and use the analyte names defined in github Wiki page ("Chemistry units,
# names, and data sources).



# SCRIPT TO READ IN WATER CHEM------------------

# function reads in data, filters data, and creates all new columns
get_awberc_data <- function(path, data, sheet) { 
  
  d <- read_excel(paste0(path, data), #
                  sheet = sheet, skip = 1, guess_max = 5000) %>%  # guess_max = 10000
    janitor::clean_names() %>% # clean up names for rename and select, below
    janitor::remove_empty(which = c("rows", "cols")) %>% # remove empty rows
    rename(collection_date = collection_date_cdate, #rename fields
           analyte_detection_date = analyte_detection_date_ddate,
           finalConc = peak_concentration_corrected_for_dilution_factor,
           analyte = analyte_name_analy,
           tp_tn = tp_tn_adjusted_concentration_full_series_ug_p_l,
           lake_id = site_id_id,
           crossid = c_ross_id_pos, 
           site_id = long_id_subid,
           sample_type = type, 
           rep = rep_number) %>%
    # dates and holding times
    mutate(
      # convert to date
      collection_date = as.Date(collection_date, format = "%m/%d/%Y"), 
      analyte_detection_date = as.Date(analyte_detection_date, format = "%Y%m%d"),
      # determine if holding time exceeded
      nutrients_qual = if_else( 
      (analyte_detection_date - collection_date) > 28,
      "H", NA_character_)) %>% # TRUE = hold time violation
    mutate(finalConc = ifelse( # correct TP and TN are in tp_tn
      analyte %in% c("TP", "TN"),
      tp_tn,
      finalConc)) %>%
    filter(sample_type != "SPK", # exclude matrix spike
           sample_type != "CHK") %>% # exclude standard check
    select(lake_id, site_id, collection_date, crossid, sample_type, analyte,
           finalConc, nutrients_qual, rep) %>% # keep only needed fields
    mutate(analyte = str_to_lower(analyte)) %>% # make analyte names lowercase
    mutate(analyte = case_when( # change analyte names where necessary
      analyte == "trp" ~ "op",
      analyte == "tnh4" ~ "nh4",
      analyte == "tno2" ~ "no2",
      analyte == "tno2-3" ~ "no2_3",
      TRUE   ~ analyte)) %>%
    mutate(sample_type = case_when( # recode sample type identifiers
      grepl("dup", sample_type, ignore.case = TRUE) ~ "duplicate", # laboratory duplicate
      grepl("ukn", sample_type, ignore.case = TRUE) ~ "unknown",
      grepl("blk", sample_type, ignore.case = TRUE) ~ "blank", # field blank
      TRUE ~ sample_type)) %>%
    # define sample depth
    mutate(sample_depth = case_when(
             crossid == "Dp" ~ "deep",
             crossid == "Sh" ~ "shallow",
             TRUE ~ "FLY YOU FOOLS")) %>% # error code
    mutate(analyte_flag = case_when( # create the analyte_flag column
      analyte == "nh4" & finalConc < 6 ~ "ND",
      analyte == "no2" & finalConc < 6 ~ "ND",
      analyte == "no2_3" & finalConc < 6 ~ "ND",
      analyte == "op" & finalConc < 3 ~ "ND",
      analyte == "tn" & finalConc < 25 ~ "ND",
      analyte == "tp" & finalConc < 5 ~ "ND",
      TRUE ~ "")) %>%
    mutate(finalConc = case_when( # create the finalConc column
      analyte == "nh4" & finalConc < 6 ~ 6,
      analyte == "no2" & finalConc < 6 ~ 6,
      analyte == "no2_3" & finalConc < 6 ~ 6,
      analyte == "op" & finalConc < 3 ~ 3,
      analyte == "tn" & finalConc < 25 ~ 25,
      analyte == "tp" & finalConc < 5 ~ 5,
      TRUE ~ finalConc)) %>%
    # observations above the MDL, but below the lowest non-zero standard are flagged "L"
    mutate(analyte_flag = case_when( # create L values in the analyte_flag column
      analyte == "nh4" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "no2" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "no2_3" & finalConc > 6 & finalConc < 20 ~ "L",
      analyte == "op" & finalConc > 3 & finalConc < 5 ~ "L",
      analyte == "tn" & finalConc > 25 & finalConc < 30 ~ "L",
      analyte == "tp" & finalConc > 5 & finalConc < 5 ~ "L", # the MDL and lowest standard is 5
      TRUE ~ analyte_flag)) %>%
    mutate(units = case_when(
      analyte == "nh4"  ~ "ug_n_l",
      analyte == "no2"  ~ "ug_n_l",
      analyte == "no2_3"  ~ "ug_n_l",
      analyte == "op"  ~ "ug_p_l",
      analyte == "tn"  ~ "ug_n_l",
      analyte == "tp"  ~ "ug_p_l",
      TRUE ~ "")) %>%
    mutate(sample_type = case_when(
      sample_type == "duplicate" ~ "unknown",
      TRUE ~ sample_type)) %>%
    mutate(sample_depth = case_when(
      sample_type == "blank" ~ "blank", # see Wiki lake_id, site_id, and sample_depth formats
      TRUE ~ sample_depth)) %>%
    mutate(rep = as.character(rep), # 2022 data should all be numeric, but convert to chr for consistency with 2021 and code below
           rep = case_when(
             rep == "A" ~ "1", # convert to number for dup_agg function
             rep == "B" ~ "2",
             TRUE ~ rep)) %>%
    select(-crossid)  %>% # no longer need crossid
    mutate(site_id = as.numeric(site_id)) # convert to numeric to match other chem data
  
  
  return(d)
  
  
}


# READ DATA-----------
nutrients24 <- get_awberc_data("inputData/2024_campaign/", 
                          "puerto_rico_2024_nutrients_2025-03-06.xlsx", 
                          "2025 Data") 

# rep == 1 is a typical unknown sample.
# rep = 2 is a field duplicate
# nutrients_qual == H indicated a holding time violation
# analyte_flag == "ND" indicates a non-detect value
# analyte_flag == "L" indicates a value above the MDL, but below the lowest non-zero standard
# analyte_flag == "" indicates a value above the lowest non-zero standard

