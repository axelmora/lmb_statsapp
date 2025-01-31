library(cronR)

# Define command to run the refresh script
cmd <- cron_rscript("refresh_data.R")

# Schedule the script to run daily at midnight (00:00)
cron_add(cmd, frequency = "daily", at = "00:00", id = "data_refresh")