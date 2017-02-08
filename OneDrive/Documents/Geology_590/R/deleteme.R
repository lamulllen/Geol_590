rm(list=ls())

#Load required packages
library(plyr) # 'split-apply-combine'
library(lubridate) # Deals with dates
library(ggplot2) # plotting

# Use a function I wrote
source("R/lm_stats.R") # start from working directory

# Load the data processing function
source("R/process_LM_PEEC_data.R")

browser()

# Run it once
ThirdCreek1 <- process_LM_PEEC_data(data.fn="data/2015_06_08_time_trial_third_creek.csv", calib.fn="data/2015_05_20_third_creek_calib.csv",
                            site.code="ThirdCreek1", print.plots=TRUE, save.plots=FALSE)

ThirdCreek2 <- process_LM_PEEC_data(data.fn="data/2015_06_12_time_trial_third_creek.csv", calib.fn="data/2015_05_20_third_creek_calib.csv",
                              site.code="ThirdCreek2", print.plots=TRUE, save.plots=FALSE)

ThirdCreek3 <- process_LM_PEEC_data(data.fn="data/2015_06_15_time_trial_third_creek.csv", calib.fn="data/2015_05_20_third_creek_calib.csv",
                                    site.code="ThirdCreek3", print.plots=TRUE, save.plots=FALSE)

# Put all the data frames into a list, and then bind them into one big data frame
all_data_list <- list(ThirdCreek1=ThirdCreek1, ThirdCreek2=ThirdCreek2, ThirdCreek3=ThirdCreek3)
all_sites_df <- do.call(rbind, all_data_list)

# Plot the data. Or whatever.
p_TN_processed <- ggplot(all_sites_df, aes(x=substrate, y=calib.slope, colour=treatment, shape=treatment)) +
  geom_pointrange(aes(ymin=calib.slope-calib.se, ymax=calib.slope+calib.se), position=position_jitter(width=0.05)) + 
  scale_shape_manual(values=c(1, 16)) + 
  scale_colour_manual(values=c("red", "black")) + 
  ylab(expression(paste(v[0], ", nM ", hr^{-1}))) + # see demo(plotmath)
  facet_wrap(~site.code, scales="free_y") +
  theme(axis.text.x=element_text(angle=-60, hjust=0)) # see ?theme
print(p_TN_processed)
# ggsave("2015_06_17_time_trial_calib_slopes.png", height=5, width=5, units="in", dpi=500)

timeseries_data <- ggplot(all_sites_df, aes(x=date, y=calib.slope, colour=substrate)) +
  geom_pointrange(aes(ymin = calib.slope - calib.se, ymax = calib.slope + calib.se)) +
  do.call(rbind, by(calib.slope$substrate), function(df) {
  nums <- tapply(df$length, df$day, length)
  data.frame(substrate, date = as.numeric(names(nums)), number=as.vector(nums))
})
p <- ggplot(timeseries_data, aes(x=date, y=calib.slope, group=substrate))
p + geom_line()

#Plot timeseries data v0 vs. date
timeseries_data <- ggplot(all_sites_df, aes(x=date, y=v0, colour=substrate)) +
  geom_pointrange(aes(ymin=slope-slope.se, ymax=slope+slope.se)) +
  geom_line() +
  facet_wrap(~site)
print(timeseries_data)

