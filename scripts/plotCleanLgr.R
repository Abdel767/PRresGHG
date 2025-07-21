# THIS SCRIPT WILL BE USED TO PLOT AND CLEAN LGR DATA IN PREPARATION
# FOR CALCULATION OF DIFFUSIVE EMISSION RATE

#####################################################
## TO RUN THE CODE BELOW, YOU MUST FIRST GENERATE THE gga AND fld_sheet DATA 
## OBJECTS.  THIS CAN BE DONE BY RUNNING ALL SCRIPTS IN THE ORDER
## DEFINED IN masterScript.R, OR YOU CAN JUST RUN THESE 4 LINES:
# source("scripts/masterLibrary.R") # Read in renv controlled library
# source("scripts/readFieldSheets.R") # read prData...xlsx.  fld_sheet, dg_sheet
# source("scripts/readLgr.R") # read raw LGR data

#1. INSPECT INSTANCES OF NA IN GGA------------
# Time/date stamp first
filter(gga, is.na(RDateTime))
# no NAs for this field [9/21/2023] 
gga <- filter(gga, !is.na(RDateTime)) # strip out missing RDateTime which complicate functions below.


#2.  ASSIGN site_id, AND chamb_deply_date_time TO LGR OBSERVATIONS.------------------------
# Many rows in fld_sheet have NA for chamb_deply_date_time.  For example, at all oversample
# sites where chambers were not deployed.  We want to remove these rows, or the missing
# values complicate the loop.

missing_chamb_deply_date_time <- is.na(fld_sheet$chamb_deply_date_time) # logical for missing chamber deployment times

# Join with fld_sheet to get site_id and chamb_deply_date_time
# This join duplicates the time series for each station within
# each lake
#####################################################################################

gga$RDateTime <- as.POSIXct(gga$RDateTime, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
fld_sheet$chamb_deply_date_time <- as.POSIXct(fld_sheet$chamb_deply_date_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Filter only sites that were sampled (eval_status == "TS")
fld_ts <- fld_sheet %>%
  filter(eval_status == "TS") %>%
  select(lake_id, site_id, chamb_deply_date_time)

# Perform fuzzy join using a 24-hour tolerance window
gga_2 <- difference_inner_join(
  gga, fld_ts,
  by = c("RDateTime" = "chamb_deply_date_time"),
  max_dist = as.difftime(24, units = "hours"),
  distance_col = "time_diff"
) %>%
  arrange(RDateTime, time_diff) %>%
  distinct(RDateTime, .keep_all = TRUE)

# Optional: remove time_diff if not needed
gga_2 <- gga_2 %>% select(-time_diff)

#####################################################################################

#3. ADD CO2 AND CH4 RETRIEVAL AND DEPLOYMENT TIMES
# We may want to model different portions of the time series for CO2 and CH4.
# Here we create fields to hold retrieval and deployment times for each gas.
gga_2 <- gga_2 %>% 
  # mutate ensures that all records have deployment and retrieval times for CO2 and CH4
  # assume deployment time recorded in field is correct, will inspect/modify below.
  mutate(co2DeplyDtTm = chamb_deply_date_time , 
         co2RetDtTm =  chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
         ch4DeplyDtTm = chamb_deply_date_time,
         ch4RetDtTm = chamb_deply_date_time + (60*5))


#3. RECORD ADJUSTMENTS TO TIME SERIES PLOTS-------------
# COMPLETE 3.1, 3.2, AND 3.3.  REPEAT UNTIL ALL PLOTS HAVE BEEN REVIEWED.
#3.1  Manually inspect each plot and record best deployment and retrieval times
# in lab specific Excel file.  

# specify which lake, site, and campaign date to inspect
lake_id.i <- "1000"  # numeric component of lake_id without leading zero(s), formatted as character
# campaign_date.i <- as.Date("2023-09-19") # campaign date formatted as Date
# campaign_date.i <- as.Date("2023-10-24")
# campaign_date.i <- as.Date("2023-10-27")
RDate.i <- as.Date("2024-08-04")
site_id.i <- 24 # numeric component of site_id, formatted as numeric

plotCh4 <- gga_2 %>% 
  filter(lake_id == lake_id.i, 
         site_id == site_id.i, 
         as.Date(RDateTime) == RDate.i, # filter by campaign date
         RDateTime > ch4DeplyDtTm - 60, # start plot 1 minute prior to deployment
         RDateTime < ch4RetDtTm + 60, # extend plot 1 minute post deployment
         CH4._ppm > 0) %>%
  ggplot(aes(RDateTime, CH4._ppm)) + 
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(ch4DeplyDtTm))) +
  geom_vline(aes(xintercept = as.numeric(ch4RetDtTm))) +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle(paste("site_id =", site_id.i, "campaign_date =", RDate.i))
ggplotly(plotCh4)  

plotCo2 <- gga_2 %>% 
  filter(lake_id == lake_id.i, 
         site_id == site_id.i, 
         as.Date(RDateTime) == RDate.i, # filter by campaign date
         RDateTime > co2DeplyDtTm - 60, # start plot 1 minute prior to deployment
         RDateTime < co2RetDtTm + 60, # extend plot 1 minute post deployment
         CO2._ppm > 0) %>%
  ggplot(aes(RDateTime, CO2._ppm)) + 
  geom_point() +
  geom_vline(aes(xintercept = as.numeric(co2DeplyDtTm))) +
  geom_vline(aes(xintercept = as.numeric(co2RetDtTm))) +
  scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
  ggtitle(paste("site_id =", site_id.i, "campaign_date =", RDate.i))
ggplotly(plotCo2)


#3.2  Read in refined deployment and retrieval data from Excel files.
# use .xls.  Can read file into R while file is open in Excel, which is convenient.

# Read data
# adjData <- readxl::read_xls(path = "inputData/lgr/chamberAdjustments1000.xls",
#                             range =cell_cols("DATA!A:J"), # columns A:J
#                             col_types = c("text", "numeric", 
#                                           rep("date", 4), 
#                                           rep("text", 4))) %>% #lake_id is character
#   mutate(campaign_date = case_when(abs(as.Date(co2DeplyDtTm) - as.Date("2023-09-20")) <= 3 ~ as.Date("2023-09-20"),
#                                    abs(as.Date(co2DeplyDtTm) - as.Date("2023-10-24")) <= 3 ~ as.Date("2023-10-24"),
#                                    TRUE ~ as.Date("1787-12-07"))) %>% # Delaware is first state to ratify constitution)
#   janitor::remove_empty("rows") # remove rows that contain only NA
# 
# str(adjData)
####################################################################################################################################
#3.2 Load chamberAdjustment data
adjData <- readxl::read_xls(path = "inputData/lgr/ChamberAdjustmentsThesisAbdel.xls",
                             range = cell_cols("A:J"), # columns A:J
                             col_types = c("text", "numeric", 
                                           rep("date", 4), 
                                           rep("text", 4))) %>% #lake_id is character
  # Ensure dates are properly parsed
  mutate(co2DeplyDtTm = as.POSIXct(co2DeplyDtTm),
         co2RetDtTm = as.POSIXct(co2RetDtTm),
         ch4DeplyDtTm = as.POSIXct(ch4DeplyDtTm),
         ch4RetDtTm = as.POSIXct(ch4RetDtTm)) %>%
  # Create campaign_date containing only the date part
  mutate(campaign_date = as.Date(co2DeplyDtTm)) %>%
  # Remove rows that contain only NA
  janitor::remove_empty("rows")
#####################################################################################################################################
#3.3. update deployment and retrieval times based on fixes above (see 3.1 and 3.2)
# gga_2 <- gga_2 %>% 
#   # remove co2DeplyDtTm, co2RetDtTm, ch4DeplyDtTm, and ch4RetDtTm.  They will be replaced with
#   # data from adjData or derived from chamb_deply_date_time
#   select(-contains("DtTm")) %>%
#   # Remove these columns if present.  Won't be present first time through, but
#   # will in subsequent iterations.  Will be replaced with data from adjData.
#   # This won't throw error if specified columns are absent.
#   select_if(!names(.) %in% c("co2Notes", "ch4Notes", "co2Status", "ch4Status")) %>%
#   # Join with adjDataDf.
#   left_join(., adjData) %>%
#   # mutate ensures that all records have deployment and retrieval times for CO2 and CH4
#   mutate(co2DeplyDtTm = case_when(is.na(co2DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
#                             TRUE ~ co2DeplyDtTm), # if not na, then use data supplied from adjDataDf
#          co2RetDtTm = case_when(is.na(co2RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
#                             TRUE ~ co2RetDtTm), # if not na, then use data supplied from adjDataDf
#          ch4DeplyDtTm = case_when(is.na(ch4DeplyDtTm) ~ chamb_deply_date_time, # if na, then use field sheet data
#                                   TRUE ~ ch4DeplyDtTm), # if not na, then use data supplied from adjDataDf
#          ch4RetDtTm = case_when(is.na(ch4RetDtTm) ~ chamb_deply_date_time + (60*5), # assume retrieval 5 minutes after deployment
#                                 TRUE ~ ch4RetDtTm))  # if not na, then use data supplied from adjDataDf
gga_2 <- gga_2 %>%
  select(-any_of(c("co2DeplyDtTm", "co2RetDtTm", "ch4DeplyDtTm", "ch4RetDtTm",
                   "co2Notes", "ch4Notes", "co2Status", "ch4Status"))) %>%
  left_join(adjData, by = c("lake_id", "site_id")) %>%
  mutate(
    co2DeplyDtTm = coalesce(co2DeplyDtTm, chamb_deply_date_time),
    co2RetDtTm   = coalesce(co2RetDtTm, chamb_deply_date_time + minutes(5)),
    ch4DeplyDtTm = coalesce(ch4DeplyDtTm, chamb_deply_date_time),
    ch4RetDtTm   = coalesce(ch4RetDtTm, chamb_deply_date_time + minutes(5))
  )

# GO BACK TO STEP 3.1 TO REVIEW TIME SERIES AFTER INCORPORATING NEW DEPLOYMENT AND RETRIEVAL TIMES
# IF SATISFIED WITH PROFILES, MOVE ON TO STEP 4.


##########################################
###########  CODE BELOW MUST BE UPDATED TO ACCOMODATE UNIQUE COMBINATIONS
###########  OF SITE AND SAMPLING DATE.  THIS MIGHT NECESITATE CHANGES TO UPSTREAM
###########  TO CARRY UNIQUE SAMPLING DATE THROUGH
##########################################


#4. PREPARE DATA TO PLOT ALL TIME SERIES----------------
# Trim data to only those we plan to model, plus 60 second buffer on either side
# of modeling window.
gga_3 <- gga_2 %>%
  group_by(site_id) %>% # for each lake and site....
  filter(RDateTime > (min(c(co2DeplyDtTm, ch4DeplyDtTm)) - 60) & 
           RDateTime < (max(c(co2RetDtTm, ch4RetDtTm)) + 60)) %>%
  ungroup()



#5.  PLOT CO2 AND CH4 PROFILES FOR INSPECTION---------------
# Plot all profiles on a single .pdf
# pdf("output/figures/ggaProfile.pdf", paper = "a4r") # landscape orientation
# tic()
# for (i in 1:with(gga_3[!is.na(gga_3$lake_id), ], # this eliminates observations without a Lake_Name (LGR data when chamber not deployed)
#                  length(unique(paste(site_id, lake_id))))) {  # each combination of site and lake
#   print(i)
#   site.lake.i <- with(gga_3[!is.na(gga_3$lake_id), ],  # extract unique lake x site combination
#                       unique(paste(site_id, lake_id)))[i]
#   site.i <- gsub(" .*$", "", site.lake.i)  # extract site.  regex allows for siteIDs of different lengths (i.e. S-01, SU-01)
#   lake.i <- substr(site.lake.i, start = nchar(site.i) + 2, stop = nchar(site.lake.i)) # extract lake name
#   data.i <- filter(gga_3, lake_id == lake.i, site_id == site.i) %>%  # Pull out GGA data chunk
#     select(-GasT_C) # No need to plot gas temperature
#   RDate.i <- unique(data.i$RDate)  # for panel title
# 
#   plot.i <- ggplot(data.i,  aes(x = RDateTime, y = CH4._ppm)) + 
#           geom_point() +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
#           scale_x_datetime(labels=date_format("%H:%M")) +
#           ggtitle(paste(lake.i, site.i, RDate.i)) +
#           theme(axis.text.x = element_text(size = 7),
#                 plot.title = element_text(size = 11))
#   
#   plot.ii <- ggplot(data.i,  aes(x = RDateTime, y = CO2._ppm)) + 
#           geom_point() +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
#           geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
#           scale_x_datetime(labels=date_format("%H:%M")) +
#           ggtitle(paste(lake.i, site.i)) +
#           theme(axis.text.x = element_text(size = 7))
#   
#   
#   grid.arrange(plot.i, plot.ii, ncol = 2) # use to put two plots per page
# }
# 
# 
# dev.off() #15 min, 911 pages, 1/6/23
# 
# toc()



###########

# for (i in 1:length(unique(gga_3$site_id))) {  # iterate over unique site IDs
#   print(i)
#   site.i <- unique(gga_3$site_id)[i]  # extract the current site ID
#   
#   data.i <- filter(gga_3, site_id == site.i) %>%  # Pull out GGA data for the site
#     select(-GasT_C) # Exclude gas temperature from the plot
#   
#   RDate.i <- unique(data.i$RDate)  # for panel title
#   
#   plot.i <- ggplot(data.i, aes(x = RDateTime, y = CH4._ppm)) + 
#     geom_point() +
#     geom_vline(data = data.i, aes(xintercept = as.numeric(ch4DeplyDtTm))) +
#     geom_vline(data = data.i, aes(xintercept = as.numeric(ch4RetDtTm))) +
#     scale_x_datetime(labels=date_format("%H:%M")) +
#     ggtitle(paste("Site:", site.i, "Date:", RDate.i)) +
#     theme(axis.text.x = element_text(size = 7),
#           plot.title = element_text(size = 11))
#   
#   plot.ii <- ggplot(data.i, aes(x = RDateTime, y = CO2._ppm)) + 
#     geom_point() +
#     geom_vline(data = data.i, aes(xintercept = as.numeric(co2DeplyDtTm))) +
#     geom_vline(data = data.i, aes(xintercept = as.numeric(co2RetDtTm))) +
#     scale_x_datetime(labels=date_format("%H:%M")) +
#     ggtitle(paste("Site:", site.i)) +
#     theme(axis.text.x = element_text(size = 7))
  
  # Display or save the plots as per your requirement
  # For example, to display:
#   grid.arrange(plot.i, plot.ii, ncol = 2) # arrange two plots per page
# }
####################################################################################################


# # Convert campaign_date to Date format
# adjData$campaign_date <- as.Date(adjData$campaign_date, format="%Y-%m-%d")
# 
# # Function to plot CH4 data
# plot_ch4_data <- function(lake_id.i, site_id.i, campaign_date.i, gga_2, adjData) {
#   # Filter the time windows for the specific site and campaign date
#   adjData <- filter(adjData, lake_id == lake_id.i & site_id == site_id.i & campaign_date == campaign_date.i)
#   
#   if (nrow(adjData) > 0) {
#     ch4_deploy <- as.POSIXct(adjData$ch4DeplyDtTm, format="%Y-%m-%d %H:%M:%S")
#     ch4_retrieve <- as.POSIXct(adjData$ch4RetDtTm, format="%Y-%m-%d %H:%M:%S")
#     
#     plotCh4 <- gga_2 %>% 
#       filter(lake_id == lake_id.i, 
#              site_id == site_id.i, 
#              as.Date(RDateTime) == campaign_date.i, # filter by campaign date
#              RDateTime > ch4_deploy - 60, # start plot 1 minute prior to deployment
#              RDateTime < ch4_retrieve + 60, # extend plot 1 minute post deployment
#              CH4._ppm > 0) %>%
#       ggplot(aes(RDateTime, CH4._ppm)) + 
#       geom_point() +
#       geom_vline(xintercept = as.numeric(ch4_deploy)) +
#       geom_vline(xintercept = as.numeric(ch4_retrieve)) +
#       scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
#       ggtitle(paste("site_id =", site_id.i, "campaign_date =", campaign_date.i))
#     
#     ggplotly(plotCh4)
#   } else {
#     print(paste("No time window data available for site", site_id.i, "on", campaign_date.i))
#   }
# }
# 
# # Function to plot CO2 data
# plot_co2_data <- function(lake_id.i, site_id.i, campaign_date.i, gga_2, adjData) {
#   # Filter the time windows for the specific site and campaign date
#   adjData <- filter(adjData, lake_id == lake_id.i & site_id == site_id.i & campaign_date == campaign_date.i)
#   
#   if (nrow(adjData) > 0) {
#     co2_deploy <- as.POSIXct(adjData$co2DeplyDtTm, format="%Y-%m-%d %H:%M:%S")
#     co2_retrieve <- as.POSIXct(adjData$co2RetDtTm, format="%Y-%m-%d %H:%M:%S")
#     
#     plotCo2 <- gga_2 %>% 
#       filter(lake_id == lake_id.i, 
#              site_id == site_id.i, 
#              as.Date(RDateTime) == campaign_date.i, # filter by campaign date
#              RDateTime > co2_deploy - 60, # start plot 1 minute prior to deployment
#              RDateTime < co2_retrieve + 60, # extend plot 1 minute post deployment
#              CO2._ppm > 0) %>%
#       ggplot(aes(RDateTime, CO2._ppm)) + 
#       geom_point() +
#       geom_vline(xintercept = as.numeric(co2_deploy)) +
#       geom_vline(xintercept = as.numeric(co2_retrieve)) +
#       scale_x_datetime(date_labels = ("%m/%d %H:%M")) +
#       ggtitle(paste("site_id =", site_id.i, "campaign_date =", campaign_date.i))
#     
#     ggplotly(plotCo2)
#   } else {
#     print(paste("No time window data available for site", site_id.i, "on", campaign_date.i))
#   }
# }
# 
# # Example calls for site 3 on the campaign dates "2023-10-27"
# plot_ch4_data("1000", 3, as.Date("2023-10-27"), gga_2, adjData)
# plot_co2_data("1000", 3, as.Date("2023-10-27"), gga_2, adjData)
# 
# # Repeat for other sites and dates as needed
# plot_ch4_data("1000", 7, as.Date("2023-10-27"), gga_2, adjData)
# plot_co2_data("1000", 7, as.Date("2023-10-27"), gga_2, adjData)
# 
# plot_ch4_data("1000", 2, as.Date("2023-10-27"), gga_2, adjData)
# plot_co2_data("1000", 2, as.Date("2023-10-27"), gga_2, adjData)

# Get all unique site-lake combinations
site_lake_combos <- unique(paste(gga_3$lake_id, gga_3$site_id))

# Loop through each site-lake combo
for (i in seq_along(site_lake_combos)) {
  lake.i <- strsplit(site_lake_combos[i], " ")[[1]][1]
  site.i <- strsplit(site_lake_combos[i], " ")[[1]][2]
  
  data.i <- gga_3 %>%
    filter(lake_id == lake.i, site_id == site.i)
  
  if (nrow(data.i) == 0) next
  
  plot.ch4 <- ggplot(data.i, aes(x = RDateTime, y = CH4._ppm)) +
    geom_point() +
    geom_vline(aes(xintercept = as.numeric(ch4DeplyDtTm))) +
    geom_vline(aes(xintercept = as.numeric(ch4RetDtTm))) +
    scale_x_datetime(labels = date_format("%m/%d %H:%M")) +
    ggtitle(paste("Lake:", lake.i, "Site:", site.i, "CH4")) +
    theme(axis.text.x = element_text(size = 7),
          plot.title = element_text(size = 11))
  
  plot.co2 <- ggplot(data.i, aes(x = RDateTime, y = CO2._ppm)) +
    geom_point() +
    geom_vline(aes(xintercept = as.numeric(co2DeplyDtTm))) +
    geom_vline(aes(xintercept = as.numeric(co2RetDtTm))) +
    scale_x_datetime(labels = date_format("%m/%d %H:%M")) +
    ggtitle(paste("Lake:", lake.i, "Site:", site.i, "CO2")) +
    theme(axis.text.x = element_text(size = 7))
  
  # Display plots side by side
  grid.arrange(plot.ch4, plot.co2, ncol = 2)
  
  # Prompt user to press Enter to continue
  readline(prompt = paste0("(", i, "/", length(site_lake_combos), ") Press [Enter] to continue to the next site..."))
}



