# Load necessary library
library(jsonlite)

fun_test <- function(startDate,endDate){
  url <- paste0("https://statsapi.mlb.com/api/v1/transactions?startDate=",startDate,"&endDate=",endDate)
  
print(url)
}



lmb_trans <- function(startDate,endDate){
  # Define the URL
  url <- paste0("https://statsapi.mlb.com/api/v1/transactions?startDate=",startDate,"&endDate=",endDate)
  
  # Fetch and parse the JSON response
  response <- fromJSON(url)
  
  lmb_trans <- response$transactions
  
  lmb_trans <- lmb_trans %>%
    unnest_wider(person, names_sep = "_") %>%
    unnest_wider(toTeam, names_sep = "_") %>%
    unnest_wider(fromTeam, names_sep = "_") %>%
    filter(toTeam_id %in% teams$team_id | fromTeam_id %in% teams$team_id) %>%
    select(effectiveDate, typeDesc, toTeam_name, person_fullName, description) %>%
    mutate(effectiveDate = as.Date(effectiveDate)) %>%
    rename("Date" = effectiveDate,
           "Type" = typeDesc,
           "Team" = toTeam_name,
           "Player Name" = person_fullName,
           "Description" = description) %>%
    arrange(desc(Date))
    lmb_trans
}

#trans <- gs4_create("trans", sheets = lmb_trans)
write_sheet(lmb_trans, trans, sheet = "lmb_trans")



# Define the URL
url_pace <- "https://statsapi.mlb.com/api/v1/gamePace?leagueId=125&season=2025"

# Fetch and parse the JSON response
response <- fromJSON(url_pace)