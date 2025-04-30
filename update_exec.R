library(cronR)
cmd <- cron_rscript("daily_etl.R")  # Your script name
cron_add(cmd, frequency = "daily", at = "14:48", id = "update_lmb_stats_48")