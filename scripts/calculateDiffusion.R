


# EMISSION RATE CALCULATIONS--------------------
# STEP 1:  CALCULATE EMISSION RATE VIA LINEAR AND NONLINEAR REGRESSION
#          FOR SITES WHERE PERIODS OF LINEAR ACCUMULATION ARE INDICATED 
# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OF NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# STEP 3: MERGE WITH OTHER DATA


# STEP 1: LINEAR AND NONLINEAR REGRESSION
# for practice, only consider sites with co2Status or ch4Status == done
# good.data <- adjData %>% filter(co2Notes == "unstable start" | co2Status == "done" | ch4Status == "done") %>%
#   select(lake_id, site_id, contains("status"),co2Notes)

# filter down to lake and sites with good data
# gga_4 <- gga_3 %>% filter(paste0(lake_id, site_id) %in% paste0(good.data$lake_id, good.data$site_id)) %>%
#   left_join(., good.data %>% select(lake_id, site_id, ch4Status, co2Status,co2Notes))

# substitute NA for profiles that are "in progress"
# gga_4 <- gga_4 %>% mutate(CO2.case = ifelse(co2Status == "done", "a",
#                                             ifelse(co2Notes == "unstable start", "a","b")),
#                           CO2._ppm = case_when(CO2.case == "a" ~ CO2._ppm,
#                                                TRUE ~ NA_real_),
#                           CH4._ppm = case_when(ch4Status == "done" ~ CH4._ppm,
#                                                TRUE ~ NA_real_))
# 
# 
# n <- length(unique(paste(gga_3$lake_id, gga_3$site_id)))
# temp <- rep(NA, n)

#Separate the input formatting from the flux calculations to deal with long run time
#Format list of input gga files for methane and for CO2
data.gga.ch4.list  <- list()
data.gga.co2.list <- list()

start.time <- Sys.time()  # start timer

for (i in 1:n) {  # For each unique site
  site.lake.visit.i <- unique(paste(gga_3$site_id, gga_3$lake_id))[i]
  site.i <- stringr::word(site.lake.visit.i, 1) %>% as.numeric() # extract characters before space.  site_id is numeric.
  lake.i <- stringr::word(site.lake.visit.i, 2) # extract characters after space. lake_id is character  
  
  # Need chamber volume.  SEE chamberVolume.R 
  chmVol.L.i <- fld_sheet %>% filter(site_id == site.i, 
                                     lake_id == lake.i) %>% 
    select(chmVol.L) %>% pull()   
  
  data.i.ch4 <- filter(gga_3,  # extract data
                       RDateTime >= ch4DeplyDtTm, # based on diff start time
                       RDateTime <= ch4RetDtTm, # based on diff end time
                       site_id == site.i,
                       lake_id == lake.i) %>%
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds).
           chmVol.L = chmVol.L.i[1]) %>%
    select(lake_id, site_id, CH4._ppm, elapTime, GasT_C, chmVol.L)  # Pull out data of interest
  
  data.gga.ch4.list[[i]]<-data.i.ch4 
  
  data.i.co2 <- filter(gga_3,  # extract data
                       RDateTime >= co2DeplyDtTm, # based on diff start time
                       RDateTime <= co2RetDtTm, # based on diff end time
                       site_id == site.i,
                       lake_id == lake.i)  %>%
    # Calculate elapsed time (seconds).  lm behaves strangely when used with POSIXct data.
    mutate(elapTime = RDateTime - RDateTime[1], # Calculate elapsed time (seconds).
           chmVol.L = chmVol.L.i[1]) %>%  # subscripting needed to remove name
    select(lake_id, site_id, CO2._ppm, elapTime, GasT_C, chmVol.L,co2Notes)  # Pull out data of interest
  
  data.gga.co2.list[[i]]<-data.i.co2
  
  print(i)
}

end.time = Sys.time()

time.taken = end.time - start.time

print(round(time.taken,2))

#Takes ~3hr to compile data 
#save(data.gga.ch4.list,file="inputData/input.ch4.list.Rdata")
#save(data.gga.co2.list,file="inputData/input.co2.list.Rdata")

#load(file="inputData/input.co2.list.Rdata")

start.time <- Sys.time()

out=NULL
# Run the model
OUT = {   
  
  site_id <- data.gga.ch4.list[[i]]$site_id[1]
  lake_id <- data.gga.ch4.list[[i]]$lake_id[1]
  
  # Are there data available to run the model?
  co2.indicator <- length(data.gga.co2.list[[i]]$CO2._ppm) == 0 | all(is.na(data.gga.co2.list[[i]]$CO2._ppm))
  ch4.indicator <- length(data.gga.ch4.list[[i]]$CH4._ppm) == 0 | all(is.na(data.gga.ch4.list[[i]]$CH4._ppm))
  
  
  # Data needed for emission rate calcs.  Same #'s for CO2 and CH4.  Arbitrarily pulled from CO2.
  temp.i <- if (co2.indicator) mean(data.gga.ch4.list[[i]]$GasT_C, na.rm = TRUE) else (mean(data.gga.co2.list[[i]]$GasT_C, na.rm = TRUE))  # GGA measured temp
  volume.i <- if (co2.indicator) unique(data.gga.ch4.list[[i]][!is.na(data.gga.ch4.list[[i]]$chmVol.L), "chmVol.L"]) else
    unique(data.gga.co2.list[[i]][!is.na(data.gga.co2.list[[i]]$chmVol.L), "chmVol.L"])# Dome volume
  
  # lm
  lm.ch4.i <- try(lm(data.gga.ch4.list[[i]]$CH4._ppm ~ data.gga.ch4.list[[i]]$elapTime), silent = TRUE)  # suppress warning if fails
  lm.co2.i <- try(lm(data.gga.co2.list[[i]]$CO2._ppm ~ data.gga.co2.list[[i]]$elapTime), silent = TRUE)  # linear regression
  
  # lm slopes
  slope.ch4.i <- if(ch4.indicator) NA else (as.numeric(coef(lm.ch4.i)[2]))  # lm slope: ppm s-1
  slope.co2.i <- if(co2.indicator) NA else (as.numeric(coef(lm.co2.i)[2]))   # lm slope: ppm s-1
  ch4.lm.slope <- slope.ch4.i
  co2.lm.slope<- slope.co2.i
  
  # lm p-values
  fstat.ch4 <- if(ch4.indicator) rep(NA,3) else summary(lm.ch4.i)$fstatistic
  fstat.co2 <- if(co2.indicator) rep(NA,3) else summary(lm.co2.i)$fstatistic
  ch4.lm.pval  <- pf(fstat.ch4[1], fstat.ch4[2], fstat.ch4[3], lower.tail = FALSE)
  co2.lm.pval  <- pf(fstat.co2[1], fstat.co2[2], fstat.co2[3], lower.tail = FALSE)
  
  # lm r2 values
  ch4.lm.r2  <- if(ch4.indicator) NA else summary(lm.ch4.i)["r.squared"]
  co2.lm.r2  <- if(co2.indicator) NA else summary(lm.co2.i)["r.squared"]
  
  # lm AIC values
  ch4.lm.aic <- if(ch4.indicator) NA else AIC(lm.ch4.i)
  co2.lm.aic <- if(co2.indicator) NA else AIC(lm.co2.i)
  
  #lm Standard Error of slope
  ch4.lm.se <- if(ch4.indicator) NA else sqrt(diag(vcov(lm.ch4.i)))[2]
  co2.lm.se <- if(co2.indicator) NA else sqrt(diag(vcov(lm.co2.i)))[2]
  
  # Exponential Model
  cmax.ch4 <- data.gga.ch4.list[[i]]$CH4._ppm[max(which(!is.na(data.gga.ch4.list[[i]]$CH4._ppm)))]  # cmax = final CH4
  c.initial.ch4 <- data.gga.ch4.list[[i]]$CH4._ppm[min(which(!is.na(data.gga.ch4.list[[i]]$CH4._ppm)))]  # initial CH4
  exp.ch4.i <-try(nlsLM(CH4._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.gga.ch4.list[[i]], start=list(cmax=cmax.ch4, b=cmax.ch4-c.initial.ch4, k=.03)),
                  silent = TRUE)
  
  cmax.co2 <- data.gga.co2.list[[i]]$CO2._ppm[max(which(!is.na(data.gga.co2.list[[i]]$CO2._ppm)))]  # cmax = final CO2
  c.initial.co2 <- data.gga.co2.list[[i]]$CO2._ppm[min(which(!is.na(data.gga.co2.list[[i]]$CO2._ppm)))]  # initial CO2
  exp.co2.i <-try(nlsLM(CO2._ppm~cmax-(cmax-b)*exp(-k*as.numeric(elapTime)),
                        data = data.gga.co2.list[[i]], start=list(cmax=cmax.co2, b=cmax.co2-c.initial.co2, k=0.004)),
                  silent=TRUE)
  # Ex r2
  rss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else sum(residuals(exp.ch4.i)^2)
  tss.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else
    sum((data.gga.ch4.list[[i]]$CH4._ppm - mean(data.gga.ch4.list[[i]]$CH4._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  ch4.ex.r2 = 1 - rss.ch4.i/tss.ch4.i
  
  rss.co2.i <- if(class(exp.co2.i) == "try-error") NA else sum(residuals(exp.co2.i)^2)
  tss.co2.i <- if(class(exp.co2.i) == "try-error") NA else
    sum((data.gga.co2.list[[i]]$CO2._ppm - mean(data.gga.co2.list[[i]]$CO2._ppm, na.rm=TRUE))^2, na.rm=TRUE)
  co2.ex.r2 = 1 - rss.co2.i/tss.co2.i
  
  # Ex AIC
  ch4.ex.aic = if(class(exp.ch4.i) == "try-error") NA else AIC(exp.ch4.i)
  co2.ex.aic = if(class(exp.co2.i) == "try-error") NA else AIC(exp.co2.i)
  
  #Ex standard error of k
  ch4.ex.se = if(class(exp.ch4.i) == "try-error") NA else sqrt(diag(vcov(exp.ch4.i)))[3]
  co2.ex.se = if(class(exp.co2.i) == "try-error") NA else sqrt(diag(vcov(exp.co2.i)))[3]
  
  # Ex slope
  coef.exp.ch4.i <- if(class(exp.ch4.i) == "try-error") NA else coef(exp.ch4.i)
  ch4.ex.slope = if(class(exp.ch4.i) == "try-error") NA else
    coef.exp.ch4.i["k"]*(coef.exp.ch4.i["cmax"]-coef.exp.ch4.i["b"])  # ppm s-1
  
  coef.exp.co2.i <- if(class(exp.co2.i) == "try-error") NA else coef(exp.co2.i)
  co2.ex.slope = if(class(exp.co2.i) == "try-error") NA else
    coef.exp.co2.i["k"]*(coef.exp.co2.i["cmax"]-coef.exp.co2.i["b"])  # ppm s-1
  
  #Ex k
  ch4.ex.k = if(class(exp.ch4.i) == "try-error") NA else
    coef.exp.ch4.i["k"]
  co2.ex.k = if(class(exp.co2.i) == "try-error") NA else
    coef.exp.co2.i["k"]
  
  # Emission rate.  Assumes atmospheric pressure of 1 atm.
  # Converting from parts per million to umole cross out.  No conversion factor necessary. Dome area = 0.2 m2
  ch4.lm.drate.i.umol.s <- ((volume.i * 1 * slope.ch4.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  ch4.lm.drate.mg.h = if (is.na(ch4.lm.drate.i.umol.s[1,]))  # throws error if no data
    NA else
      ch4.lm.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.lm.drate.i.umol.s <- ((volume.i * 1 * slope.co2.i) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  co2.lm.drate.mg.h =  if  (is.na(co2.lm.drate.i.umol.s[1,])) # throws error if no data
    NA else
      co2.lm.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  ch4.ex.drate.i.umol.s <- ((volume.i * 1 * ch4.ex.slope) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CH4 s-1
  ch4.ex.drate.mg.h = if (is.na(ch4.lm.drate.i.umol.s[1,])) # throws error if no data
    NA else
      ch4.ex.drate.i.umol.s * (16/1000) * (60*60)  # mg CH4 m-2 h-1
  
  co2.ex.drate.i.umol.s <- ((volume.i * 1 * co2.ex.slope) / (0.082057 * (temp.i + 273.15))) / 0.2 #umol CO2 s-1
  co2.ex.drate.mg.h =  if (is.na(co2.lm.drate.i.umol.s[1,])) # throws error if no data
    NA else
      co2.ex.drate.i.umol.s * (44/1000) * (60*60)  #mg CO2 m-2 h-1
  
  co2note<-data.gga.co2.list[[i]]$co2Notes[1]
  nco2<-length(data.gga.co2.list[[i]]$CO2._ppm)
  nch4<-length(data.gga.ch4.list[[i]]$CH4._ppm)
  
  out<-data.frame(site_id, lake_id, 
                  ch4.lm.slope, ch4.lm.drate.mg.h, 
                  ch4.lm.aic, ch4.lm.r2, ch4.lm.se, ch4.lm.pval,
                  ch4.ex.aic, ch4.ex.se, ch4.ex.r2, ch4.ex.slope, 
                  ch4.ex.drate.mg.h, ch4.ex.k, 
                  co2.lm.slope, co2.lm.drate.mg.h, 
                  co2.lm.aic, co2.lm.r2, co2.lm.se, co2.lm.pval,
                  co2.ex.aic, co2.ex.se, co2.ex.r2, co2.ex.slope, 
                  co2.ex.k, co2.ex.drate.mg.h,co2note,nco2,nch4, row.names = i)
  colnames(out)<-c("site_id", "lake_id", 
                   "ch4.lm.slope", "ch4.lm.drate.mg.h", 
                   "ch4.lm.aic", "ch4.lm.r2", "ch4.lm.se", "ch4.lm.pval",
                   "ch4.ex.aic", "ch4.ex.se", "ch4.ex.r2", "ch4.ex.slope", 
                   "ch4.ex.drate.mg.h", "ch4.ex.k", 
                   "co2.lm.slope", "co2.lm.drate.mg.h", 
                   "co2.lm.aic", "co2.lm.r2", "co2.lm.se", "co2.lm.pval",
                   "co2.ex.aic", "co2.ex.se", "co2.ex.r2", "co2.ex.slope", 
                   "co2.ex.k", "co2.ex.drate.mg.h","co2note","nco2","nch4")
  out
  # Plots
  # CH4 first
  # ch4.ex.pred <- try(
  #   data.frame(
  #     ch4.pred = predict(
  #       exp.ch4.i,newdata = data.i.ch4), # pred values from exponential model
  #     elapTime = data.i.ch4$elapTime),
  #   silent = TRUE)
  # 
  # ch4.title <- paste(OUT[i, "site"], # plot title
  #                    OUT[i, "lake_id"],
  #                    OUT[i, "visit"],
  #                    "ex.r2=",
  #                    round(OUT[i, "ch4.ex.r2"], 2),
  #                    "ex.AIC=",
  #                    round(OUT[i, "ch4.ex.aic"],2),
  #                    "ex.rate=",
  #                    round(OUT[i, "ch4.ex.drate.mg.h"], 2),
  #                    "\n lm.r2=",
  #                    round(OUT[i, "ch4.lm.r2"],2),
  #                    "lm.AIC=",
  #                    round(OUT[i, "ch4.lm.aic"],2),
  #                    "lm.rate=",
  #                    round(OUT[i, "ch4.lm.drate.mg.h"], 2),
  #                    sep=" ")
  # 
  # p.ch4 <- ggplot(data.i.ch4, aes(as.numeric(elapTime), CH4._ppm)) +
  #   geom_point() +
  #   xlab("Seconds") +
  #   ggtitle(ch4.title) +
  #   stat_smooth(method = "lm", se=FALSE)
  # if (class(exp.ch4.i) == "try-error") p.ch4 else  # if exp model worked, add exp line
  #   p.ch4 <- p.ch4 + geom_line(data=ch4.ex.pred, aes(as.numeric(elapTime), ch4.pred), color = "red")
  # print(p.ch4)
  # 
  # 
  # # CO2 models
  # co2.ex.pred <- try(
  #   data.frame(co2.pred = predict(
  #     exp.co2.i, newdata = data.i.co2),  # pred data from exp model
  #     elapTime = data.i.co2$elapTime),
  #   silent = TRUE)
  # 
  # co2.title <- paste(OUT[i, "site"], # plot title
  #                    OUT[i, "lake_id"],
  #                    OUT[i, "visit"],
  #                    "ex.r2=",
  #                    round(OUT[i, "co2.ex.r2"], 2),
  #                    "ex.AIC=",
  #                    round(OUT[i, "co2.ex.aic"],2),
  #                    "ex.rate=",
  #                    round(OUT[i, "co2.ex.drate.mg.h"], 2),
  #                    "\n lm.r2=",
  #                    round(OUT[i, "co2.lm.r2"],2),
  #                    "lm.AIC=",
  #                    round(OUT[i, "co2.lm.aic"],2),
  #                    "lm.rate=",
  #                    round(OUT[i, "co2.lm.drate.mg.h"], 2),
  #                    sep=" ")
  # 
  # p.co2 <- ggplot(data.i.co2, aes(as.numeric(elapTime), CO2._ppm)) +
  #   geom_point() +
  #   xlab("Seconds") +
  #   ggtitle(co2.title) +
  #   stat_smooth(method = "lm", se=FALSE)
  # if (class(exp.co2.i) == "try-error") p.co2 else  # if exp model worked, add exp line
  #   p.co2 <- p.co2 + geom_line(data=co2.ex.pred,
  #                              aes(as.numeric(elapTime), co2.pred),
  #                              color = "red")
  # print(p.co2)
  # }
  
}

#--------------------------------------
end.time = Sys.time()

time.taken = end.time - start.time

print(round(time.taken,2))

# stopCluster(cl1)  # close the clusters

dev.off()
start.time;Sys.time() 

OUTb<-do.call(bind_rows, OUT)

#A lot faster to run now
#save(OUT, file="output/diffusiveOUT.RData")
#load("output/diffusiveOUT.RData") # load if not run above


# STEP 2: USE AIC TO DETERMINE WHETHER LINEAR OR NON-LINEAR FIT IS BEST.
#         CONFIRM CHOICE BY INSPECTING RAW DATA
# Choose best rate.  Just use AIC
# Cowan lake manual syringe sample data wouldn't support ex model.
# Include is.na(ex.aic) to accommodate this.
OUT2 <- mutate(OUTb,
              co2.best.model = ifelse(co2note=="unstable start", "linear", ifelse(is.na(co2.ex.k),"linear",
                                      ifelse(co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic),
                                           "linear", "exponential"))),
              co2.drate.mg.h.best = ifelse(co2.best.model == "linear",
                                           co2.lm.drate.mg.h, co2.ex.drate.mg.h),
              ch4.best.model = ifelse(ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic),
                                      "linear", "exponential"),
              ch4.drate.mg.h.best = ifelse(ch4.best.model == "linear",
                                           ch4.lm.drate.mg.h, ch4.ex.drate.mg.h),
              ch4.se.overlap = ifelse(ch4.best.model == "linear",
                                      ch4.lm.slope-ch4.lm.se,ch4.ex.k-ch4.ex.se),
              co2.se.overlap =ifelse(co2.best.model == "linear",
                                     co2.lm.slope-co2.lm.se,co2.ex.k-co2.ex.se))

#Maximum methane diffusion rate whose standard error overlaps zero
#66 mg CH4 m-2 d-1
a<-filter(OUT2,ch4.se.overlap<0)
max(a$ch4.drate.mg.h.best)*24
summary(a)

#Maximum and minimum carbon dioxide rates whose standard errors overlap zero
#193 and -7318 mg CO2 m-2 d-1
b<-filter(OUT2,co2.se.overlap<0)
max(b$co2.drate.mg.h.best)*24
min(b$co2.drate.mg.h.best)*24
# Inspect r2.
plot(with(OUT2,ifelse(co2.best.model == "linear", 
                     co2.lm.r2, co2.ex.r2)))  # CO2: some low ones to investigate
plot(with(OUT2,ifelse(ch4.best.model == "linear", 
                     ch4.lm.r2, ch4.ex.r2)))  # CH4:  some low ones to investigate

# If r2 of best model < 0.9, then set to NA
OUT2 <- mutate(OUT2, 
              co2.drate.mg.h.best = case_when(
                # (co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & co2.lm.r2 < 0.9 ~ NA_real_,
                # (co2.ex.aic < co2.lm.aic) & co2.ex.r2 < 0.9 ~ NA_real_,
                (co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & co2.lm.r2 < 0.9 ~ 0,
                (co2.ex.aic < co2.lm.aic) & co2.ex.r2 < 0.9 ~ 0,
                TRUE ~ co2.drate.mg.h.best),
              
              # 
              #               co2.drate.mg.h.best = ifelse((co2.lm.aic < co2.ex.aic | is.na(co2.ex.aic)) & 
              #                                              co2.lm.r2 < 0.9, # if ex is best, but r2<0.9
              #                                                 NA, # then NA
              #                                            ifelse((co2.ex.aic < co2.lm.aic) & 
              #                                                     co2.ex.r2 < 0.9, # if lm is best, but r2<0.9
              #                                                   NA, # the NA
              #                                                   co2.drate.mg.h.best)), # otherwise assume value defined above
              #         
              ch4.drate.mg.h.best = case_when(
                # (ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9 ~ NA_real_,
                # (ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9 ~ NA_real_,
                (ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9 ~ 0,
                (ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9 ~ 0,
                TRUE ~ ch4.drate.mg.h.best))
              
              #                                    
              # ch4.drate.mg.h.best = ifelse((ch4.lm.aic < ch4.ex.aic | is.na(ch4.ex.aic)) & ch4.lm.r2 < 0.9, # if ex is best, but r2<0.9
              #                              NA, # then NA
              #                              ifelse((ch4.ex.aic < ch4.lm.aic) & ch4.ex.r2 < 0.9, # if lm is best, but r2<0.9
              #                                     NA, # the NA
              #                                     ch4.drate.mg.h.best))) # otherwise assume value defined above
              # 

# Run through janitor to enforce SuRGE name conventions
OUT2 <- janitor::clean_names(OUT2) %>%
  mutate(visit = as.numeric(visit))

# Inspect r2 after scrubbing r2<0.9
plot(with(OUT2[!is.na(OUT2$co2_drate_mg_h_best),], 
          ifelse(co2_best_model == "linear", co2_lm_r2, co2_ex_r2)))  # CO2: all > 0.9

plot(with(OUT2[!is.na(OUT2$ch4_drate_mg_h_best),], 
          ifelse(ch4_best_model == "linear", ch4_lm_r2, ch4_ex_r2)))  # CH4: all > 0.9

#Look at averages by site/visit combo
#You get an average of 4.06, median of 1.35 and 3rd quartile of 2.7 mg m_2 h-1 when you make all the r2<0.9 values 0
#You get an average of 4.52, median of 1.65 and 3rd quartile of 2.77 mg m_2 h-1 when you make all the r2<0.9 values NA
bysch4<-OUT2 %>%
  filter(!is.na(ch4_drate_mg_h_best))%>%
  mutate(vID=paste(lake_id,visit))%>%
  group_by(vID)%>%
  summarise(lake_id[1],visit[1],ch4_drate_mg_h=mean(ch4_drate_mg_h_best),
            length=length(ch4_drate_mg_h_best))

#You get an average of 113.8, median of 44.1, 3rd quartile of 180  CO2 m-2 h-1 when r2<0.9 is NA
#You get an average of  121.2, median of 80.3, 3rd quartile of 195  CO2 m-2 h-1 when r2<0.9 is NA
bysco2<-OUT2 %>%
  filter(!is.na(co2_drate_mg_h_best))%>%
  mutate(vID=paste(lake_id,visit))%>%
  group_by(vID)%>%
  summarise(lake_id[1],visit[1],co2_drate_mg_h=mean(co2_drate_mg_h_best))



# STEP 3: MERGE DIFFUSION RATES WITH eqAreaData
# First, strip NA from OUT

# 
# #OUT2 <- filter(OUT2, !is.na(Lake_Name)) # Just one NA slipped in
# eqAreaData <- merge(eqAreaData, OUT2, by.x = c("Lake_Name", "siteID"), 
#       by.y = c("Lake_Name", "site"), all=TRUE)
# 
# str(eqAreaData) # 1531 observations
# 
# # Any sites not have a diffusive rate?
# # Only a subset of Cowan Lake sites were sampled due to water in LGR.
# # Other sites had strong ebullition in LGR profile.
# filter(eqAreaData, EvalStatus == "sampled", is.na(ch4.drate.mg.h.best)) %>%
#   select(Lake_Name, siteID, 
#          ch4.lm.drate.mg.h, ch4.ex.drate.mg.h, ch4.drate.mg.h.best,
#          co2.lm.drate.mg.h, co2.ex.drate.mg.h, co2.drate.mg.h.best)
# 
# # Calculate diffusive CO2 emissions for Cowan Lake site where we 
# # had diffusive CH4 and dissolved gases, but not diffusive CO2.  This
# # is only site 04, but combined with the estimate from site 09, will provide
# # sufficient observations (n=2) for grts function to work. Recall LGR got
# # wet at Cowan and samples were manually collected
# 
# logInd <- with(eqAreaData, grepl(pattern = "Cowan", x = Lake_Name) & # cowan lake
#                  !is.na(co2.sat.ratio) & # has dissolved co2
#                  !is.na(ch4.sat.ratio) & # has dissolved ch4
#                  !is.na(ch4.drate.mg.h.best) & # has diffusive CH4
#                  is.na(co2.drate.mg.h.best)) # does not have diffusive CO2
# 
# dco2Tmp <- filter(eqAreaData, logInd) %>%
#   # 10,000 L->m3; 1mol CH4 = 16 g CH4; 1000mgCH4 = 1g CH4
#   mutate(excessCo2 = (dissolved.co2 - (dissolved.co2/co2.sat.ratio)) * 10000*44*1000, # mg/m3
#          excessCh4 = (dissolved.ch4 - (dissolved.ch4/ch4.sat.ratio)) * 10000*16*1000, # mg/m3
#          kCh4 = ch4.drate.mg.h.best / excessCh4, # m/h
#          scCh4 = 1897.8 - (114.28*Tmp_C_S) + (3.2902*(Tmp_C_S^2)) - (0.039061*(Tmp_C_S^3)), # CH4 schmidt number
#          scCo2 = 1911.1 - (118.11*Tmp_C_S) + (3.4527*(Tmp_C_S^2)) - (0.04132*(Tmp_C_S^3)), # CO2 schmidt number
#          kCo2 = kCh4 * ((scCo2/scCh4)^-0.5), # k for CH4 converted to CO2 (m/h)
#          co2.drate.mg.h.best = kCo2 * excessCo2) %>%
#   select(co2.drate.mg.h.best) %>% as.numeric
# 
# eqAreaData[logInd, "co2.drate.mg.h.best"] = dco2Tmp


# # STEP 3: MERGE DIFFUSION RATES WITH eqAreaData
# # First, strip NA from OUT
# 
# 
# #OUT2 <- filter(OUT2, !is.na(Lake_Name)) # Just one NA slipped in
# eqAreaData <- merge(eqAreaData, OUT2, by.x = c("Lake_Name", "siteID"), 
#       by.y = c("Lake_Name", "site"), all=TRUE)
# 
# str(eqAreaData) # 1531 observations
# 
# # Any sites not have a diffusive rate?
# # Only a subset of Cowan Lake sites were sampled due to water in LGR.
# # Other sites had strong ebullition in LGR profile.
# filter(eqAreaData, EvalStatus == "sampled", is.na(ch4.drate.mg.h.best)) %>%
#   select(Lake_Name, siteID, 
#          ch4.lm.drate.mg.h, ch4.ex.drate.mg.h, ch4.drate.mg.h.best,
#          co2.lm.drate.mg.h, co2.ex.drate.mg.h, co2.drate.mg.h.best)
# 
# # Calculate diffusive CO2 emissions for Cowan Lake site where we 
# # had diffusive CH4 and dissolved gases, but not diffusive CO2.  This
# # is only site 04, but combined with the estimate from site 09, will provide
# # sufficient observations (n=2) for grts function to work. Recall LGR got
# # wet at Cowan and samples were manually collected
# 
# logInd <- with(eqAreaData, grepl(pattern = "Cowan", x = Lake_Name) & # cowan lake
#                  !is.na(co2.sat.ratio) & # has dissolved co2
#                  !is.na(ch4.sat.ratio) & # has dissolved ch4
#                  !is.na(ch4.drate.mg.h.best) & # has diffusive CH4
#                  is.na(co2.drate.mg.h.best)) # does not have diffusive CO2
# 
# dco2Tmp <- filter(eqAreaData, logInd) %>%
#   # 10,000 L->m3; 1mol CH4 = 16 g CH4; 1000mgCH4 = 1g CH4
#   mutate(excessCo2 = (dissolved.co2 - (dissolved.co2/co2.sat.ratio)) * 10000*44*1000, # mg/m3
#          excessCh4 = (dissolved.ch4 - (dissolved.ch4/ch4.sat.ratio)) * 10000*16*1000, # mg/m3
#          kCh4 = ch4.drate.mg.h.best / excessCh4, # m/h
#          scCh4 = 1897.8 - (114.28*Tmp_C_S) + (3.2902*(Tmp_C_S^2)) - (0.039061*(Tmp_C_S^3)), # CH4 schmidt number
#          scCo2 = 1911.1 - (118.11*Tmp_C_S) + (3.4527*(Tmp_C_S^2)) - (0.04132*(Tmp_C_S^3)), # CO2 schmidt number
#          kCo2 = kCh4 * ((scCo2/scCh4)^-0.5), # k for CH4 converted to CO2 (m/h)
#          co2.drate.mg.h.best = kCo2 * excessCo2) %>%
#   select(co2.drate.mg.h.best) %>% as.numeric
# 
# eqAreaData[logInd, "co2.drate.mg.h.best"] = dco2Tmp
# 
