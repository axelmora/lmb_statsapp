extract_schedule <- function(start_date, end_date, level = c("mlb", "aaa", "lmb"), game_type = "R") {
  
  if (lubridate::year(start_date) != lubridate::year(end_date)) {
    stop("Please choose `start_date` and `end_date` within the same calendar year")
  }
  level <- match.arg(level)
  game_type <- sanitize_game_type(game_type)
  
  start <- format(as.Date(start_date), "%m/%d/%Y")
  end <- format(as.Date(end_date), "%m/%d/%Y")
  sport_id <- switch(level, mlb = 1, aaa = 11, lmb = 23)
  schedule_filter <- glue::glue(
    "sportId={sport_id}&gameType={paste(game_type, collapse = ',')}&startDate={start}&endDate={end}"
  )
  endpoint <- glue::glue("http://statsapi.mlb.com:80/api/v1/schedule?{schedule_filter}")
  
  schedule_json <- jsonlite::fromJSON(endpoint, flatten = TRUE)
  if(schedule_json$totalGames == 0) {
    stop(
      glue::glue(
        "No games found between {start} and {end} of type {paste(game_type, collapse = ', ')}"
      )
    )
  }
  
  schedule <- do.call(dplyr::bind_rows, args = schedule_json$dates$games)
  
  # We rely on the resumeDate column to avoid duplicating resumed games, but that column will
  # not be included in `schedule` if there were no resumed games in our timeframe.
  if (is.null(schedule$resumeDate)) {
    schedule$resumeDate <- NA
  }
  
  game <- schedule |>
    # Filter out non-NA resumeDate to get down to one row per game ID
    dplyr::filter(status.detailedState %in% c("Final", "Completed Early"), is.na(resumeDate)) |>
    dplyr::arrange(officialDate) |>
    dplyr::select(
      game_id = gamePk,
      game_type = gameType,
      year = season,
      date = officialDate,
      venue_id = venue.id,
      team_id_away = teams.away.team.id,
      team_name_away = teams.away.team.name,
      score_away = teams.away.score,
      team_id_home = teams.home.team.id,
      team_name_home = teams.home.team.name,
      score_home = teams.home.score
    )
  
  return(game)
}

download_statsapi <- function(start_date,
                              end_date,
                              level = c("mlb", "aaa"),
                              game_type = "R",
                              cl = NULL) {
  
  game <- extract_schedule(start_date, end_date, level)
  year <- lubridate::year(start_date)
  
  data_list <- pbapply::pblapply(
    X = game$game_id,
    # If we encounter an error, try a total of three times before returning NULL and moving on
    FUN = function(game_id) {
      is_success <- FALSE
      num_attempts <- 0
      while (!is_success & num_attempts < 3) {
        Sys.sleep(0.1)  # Avoid being rate-limited by statsapi
        data <- try(extract_game(game_id))
        if ("try-error" %in% class(data)) {
          num_attempts <- num_attempts + 1
          data <- NULL
          Sys.sleep(5)  # Take a long pause in case it helps avoid network error
        } else {
          is_success <- TRUE
        }
      }
      return(data)
    },
    cl = cl
  )
  
  event <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$event)) |>
    tibble::add_column(year = year, .after = "game_id")
  pitch <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$pitch)) |>
    tibble::add_column(year = year, .after = "game_id")
  play <- do.call(dplyr::bind_rows, args = lapply(data_list, function(x) x$play)) |>
    tibble::add_column(year = year, .after = "game_id")
  
  data <- list(
    event = event,
    pitch = pitch,
    play = play,
    game = game
  )
  
  return(data)
}