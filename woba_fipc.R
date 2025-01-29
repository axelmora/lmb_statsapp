extract_game <- function(game_id) {
  
  event_endpoint <- glue::glue("https://statsapi.mlb.com/api/v1.1/game/{game_id}/feed/live")
  event_json <- jsonlite::fromJSON(event_endpoint)
  
  lineup_endpoint <- glue::glue("https://statsapi.mlb.com/api/v1/game/{game_id}/boxscore")
  lineup_json <- jsonlite::fromJSON(lineup_endpoint)
  
  
  # Extract event data ----
  
  event_data <- event_json$liveData$plays$allPlays
  
  event_base_out_state <- track_base_out_by_event(event_data)
  
  event_without_fielder_id <- tibble::tibble(
    game_id = game_id,
    event_index = event_data$about$atBatIndex,
    inning = event_data$about$inning,
    half_inning = event_data$about$halfInning,
    batter_id = event_data$matchup$batter$id,
    bat_side = event_data$matchup$batSide$code,
    pitcher_id = event_data$matchup$pitcher$id,
    pitch_hand = event_data$matchup$pitchHand$code,
    event = event_data$result$event,
    is_out = event_data$result$isOut,
    score_away = event_data$result$awayScore,
    score_home = event_data$result$homeScore,
    runs_on_event = sapply(event_data$runners,
                           FUN = function(x) sum(dplyr::coalesce(x$movement$end, "") == "score")
    )
  ) |>
    dplyr::left_join(event_base_out_state, by = "event_index")
  
  
  # Extract play data ----
  
  play_data <- do.call(dplyr::bind_rows, args = event_data$playEvents)
  
  play_all <- tibble::tibble(
    play_id = play_data$playId,
    action_play_id = replace_null(play_data$actionPlayId),
    game_id = game_id,
    event_index = rep(event_data$about$atBatIndex, times = sapply(event_data$playEvents, nrow)),
    play_index = play_data$index,
    pitch_number = play_data$pitchNumber,
    type = play_data$type,
    is_substitution = replace_null(play_data$isSubstitution),
    player_id = play_data$player$id,
    position = replace_null(play_data$position$code),
    outs = play_data$count$outs,
    post_balls = play_data$count$balls,
    post_strikes = play_data$count$strikes,
    post_disengagements = replace_null(play_data$details$disengagementNum, replacement = 0),
    description = play_data$details$description,
    event = play_data$details$event,
    from_catcher = replace_null(play_data$details$fromCatcher),
    runner_going = replace_null(play_data$details$runnerGoing),
    is_out = play_data$details$isOut,
    extension = replace_null(play_data$pitchData$extension),
    strike_zone_top = play_data$pitchData$strikeZoneTop,
    strike_zone_bottom = play_data$pitchData$strikeZoneBottom,
    hit_coord_x = play_data$hitData$coordinates$coordX,
    hit_coord_y = play_data$hitData$coordinates$coordY,
  ) |>
    # Get pre-pitch count and disengagements
    dplyr::group_by(game_id, event_index) |>
    # We have to track the number of disengagements throughout the end of each plate appearance.
    # For some reason, disengagementNum reverts to NA for the final pitch of each plate appearance.
    tidyr::fill(post_disengagements, .direction = "down") |>
    tidyr::replace_na(list(post_disengagements = 0)) |>
    dplyr::mutate(
      pre_balls = dplyr::coalesce(dplyr::lag(post_balls, 1), 0),
      pre_strikes = dplyr::coalesce(dplyr::lag(post_strikes, 1), 0),
      pre_disengagements = dplyr::coalesce(dplyr::lag(post_disengagements, 1), 0),
    ) |>
    dplyr::ungroup()
  
  pitch <- play_all |>
    dplyr::filter(type == "pitch") |>
    dplyr::select(play_id, game_id, event_index, play_index, pitch_number,
                  outs, balls = pre_balls, strikes = pre_strikes,
                  description, extension,
                  strike_zone_top, strike_zone_bottom, hit_coord_x, hit_coord_y
    )
  
  
  # Extract fielder credits, fielder lineups and substitutions ----
  
  fielder_credit <- lapply(
    X = event_data$runners,
    FUN = function(x) {
      do.call(dplyr::bind_rows, args = x$credit)
    }
  )
  first_fielder <- do.call(dplyr::bind_rows, args = fielder_credit) |>
    tibble::add_column(
      event_index = rep(event_data$about$atBatIndex, times = sapply(fielder_credit, nrow)),
      .before = 1
    ) |>
    dplyr::group_by(event_index) |>
    dplyr::slice(1) |>
    # We're doing this weird thing instead of dplyr::select because `position` is itself a dataframe
    # within the dataframe. I don't entirely understand this data structure.
    with(tibble::tibble(event_index, first_fielder = position$code))
  
  starting_lineup_home <- extract_fielding_lineup(players = lineup_json$teams$home$players) |>
    tibble::add_column(half_inning = "top", .before = 1)
  starting_lineup_away <- extract_fielding_lineup(players = lineup_json$teams$away$players) |>
    tibble::add_column(half_inning = "bottom", .before = 1)
  starting_lineup <- dplyr::bind_rows(starting_lineup_home, starting_lineup_away)
  
  lineup_by_event <- event_without_fielder_id |>
    dplyr::select(event_index, half_inning) |>
    dplyr::left_join(starting_lineup, by = "half_inning", relationship = "many-to-many") |>
    dplyr::group_by(half_inning) |>
    dplyr::mutate(player_id = ifelse(event_index == min(event_index), player_id, NA)) |>
    dplyr::ungroup()
  
  substitution <- play_all |>
    dplyr::filter(is_substitution, position %in% 2:10) |>   # keep only players who occupy position
    # Keep only the first substitution for each position in each event
    dplyr::group_by(event_index, position) |>
    dplyr::arrange(play_index) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(event_index, position = as.integer(position), player_id)
  
  lineup_by_event_wide <- lineup_by_event |>
    dplyr::left_join(substitution,
                     by = c("event_index", "position"),
                     suffix = c("_before", "_after")
    ) |>
    dplyr::mutate(player_id = dplyr::coalesce(player_id_before, player_id_after)) |>
    dplyr::group_by(half_inning, position) |>
    tidyr::fill(player_id, .direction = "down") |>
    dplyr::ungroup() |>
    dplyr::transmute(event_index, name = glue::glue("fielder_{position}_id"), player_id) |>
    tidyr::pivot_wider(names_from = name, values_from = player_id)
  
  event <- event_without_fielder_id |>
    dplyr::left_join(first_fielder, by = "event_index") |>
    dplyr::left_join(lineup_by_event_wide, by = "event_index")
  
  
  # ----
  
  play_base_out_state <- track_base_out_by_play(event_data)
  
  # This table includes all pitches, pickoff attempts, stepoffs and automatic balls/strikes
  play <- play_all |>
    dplyr::filter(!is.na(play_id)) |>   # remove non-play "actions" like stolen base attempts
    dplyr::left_join(play_base_out_state, by = "play_id") |>
    tidyr::replace_na(
      list(is_stolen_base = FALSE, is_caught_stealing = FALSE, is_defensive_indiff = FALSE)
    ) |>
    dplyr::select(play_id, game_id, event_index, play_index, pitch_number,
                  pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_outs, pre_balls, pre_strikes,
                  pre_disengagements,
                  runs_on_play,
                  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_outs, post_balls, post_strikes,
                  post_disengagements, type, runner_going, from_catcher,
                  is_pickoff, is_pickoff_error, is_stolen_base, is_caught_stealing, is_defensive_indiff
    )
  
  
  return(
    list(
      event = event,
      pitch = pitch,
      play = play
    )
  )
}

track_base_out_by_event <- function(event_data) {
  
  post_state <- tibble::tibble(
    event_index = event_data$about$atBatIndex,
    # We coalesce with NA to handle the case where the value is NULL (e.g. on one reached 3B)
    post_runner_1b_id = dplyr::coalesce(event_data$matchup$postOnFirst$id, NA),
    post_runner_2b_id = dplyr::coalesce(event_data$matchup$postOnSecond$id, NA),
    post_runner_3b_id = dplyr::coalesce(event_data$matchup$postOnThird$id, NA),
    post_outs = event_data$count$outs
  )
  
  pre_state <- post_state |>
    dplyr::transmute(
      event_index,
      pre_runner_1b_id = dplyr::lag(post_runner_1b_id, 1),
      pre_runner_2b_id = dplyr::lag(post_runner_2b_id, 1),
      pre_runner_3b_id = dplyr::lag(post_runner_3b_id, 1),
      pre_outs = dplyr::lag(post_outs, 1, default = 0) %% 3
    )
  
  zombie_runner <- dplyr::coalesce(
    sapply(event_data$playEvents, function(x) any(x$details$event == "Runner Placed On Base")),
    FALSE
  )
  
  zombie_runner_id <- do.call(dplyr::bind_rows, args = event_data$playEvents) |>
    dplyr::filter(details$event == "Runner Placed On Base") |>
    with(player$id)
  
  pre_state$pre_runner_2b_id[zombie_runner] <- zombie_runner_id
  
  base_out_state <- pre_state |>
    dplyr::left_join(post_state, by = "event_index") |>
    dplyr::select(
      event_index,
      dplyr::starts_with("pre_"),
      dplyr::starts_with("post_")
    )
  
  return(base_out_state)
}

track_base_out_by_play <- function(event_data) {
  
  # Step 0. Extract runner data ----
  
  runner_detail_list <- lapply(event_data$runners, function(x) x$detail)
  runner_movement_list <- lapply(event_data$runners, function(x) x$movement)
  runner_length <- sapply(runner_detail_list, function(x) if(is.null(x)) 0 else nrow(x))
  runner_movement <- dplyr::bind_cols(
    do.call(dplyr::bind_rows, args = runner_detail_list),
    do.call(dplyr::bind_rows, args = runner_movement_list)
  ) |>
    tibble::add_column(event_index = rep(event_data$about$atBatIndex, times = runner_length)) |>
    # Limit ourselves to one movement per event/play/runner
    dplyr::group_by(event_index, play_index = playIndex, runner$id) |>
    # Take the last movement to get the end base (start base is provided by `originBase`)
    dplyr::slice(dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      event_index,
      play_index,
      event_runner = event,  # sometimes the event (e.g. stolen base) is in runners data
      runner_id = runner$id,
      start_base = dplyr::coalesce(originBase, "batter"),
      end_base = dplyr::coalesce(end, "out"),
      is_out = isOut,
      is_scoring_event = isScoringEvent
    )
  
  
  # Step 1. Consolidate runner movement within play ID ----
  
  # We have to do this because, for example, generally a stolen base attempt on a pitch will have
  # two separate rows in the play data: one for the pitch and one for the stolen base attempt.
  # We want to conlidate these into one row of data.
  
  play_data <- do.call(dplyr::bind_rows, args = event_data$playEvents)
  
  play_keys <- tibble::tibble(
    play_id = play_data$playId,
    action_play_id = replace_null(play_data$actionPlayId),
    event_index = rep(event_data$about$atBatIndex, times = sapply(event_data$playEvents, nrow)),
    play_index = play_data$index,
    event_play = play_data$details$event  # sometimes the event (e.g. stolen base) is in play data
  )
  
  runner_movement_consolidated <- play_keys |>
    dplyr::transmute(
      play_id = dplyr::coalesce(play_id, action_play_id),
      event_index,
      play_index,
      event_play
    ) |>
    dplyr::inner_join(runner_movement, by = c("event_index", "play_index")) |>
    dplyr::group_by(play_id, runner_id) |>
    dplyr::arrange(event_index, play_index) |>
    dplyr::summarize(
      start_base = start_base[1],
      end_base = end_base[dplyr::n()],
      outs = sum(end_base == "out"),
      runs = sum(end_base == "score"),
      # Check both `event_play` and `event_runners` for stolen base, etc.
      # Usually it's in both, but sometimes only one or the other will have it.
      is_pickoff = any(
        grepl(
          "Pickoff", c(event_play, event_runner)) &
          !grepl("Pickoff Error", c(event_play, event_runner)
          )
      ),
      is_pickoff_error = any(grepl("Pickoff Error", c(event_play, event_runner))),
      is_stolen_base = any(grepl("Stolen Base", c(event_play, event_runner))),
      is_caught_stealing = any(grepl("Caught Stealing", c(event_play, event_runner))),
      is_defensive_indiff = any(grepl("Defensive Indiff", c(event_play, event_runner))),
      .groups = "drop"
    )
  
  
  # Step 2. Initiate play base-out state from pre-event base-out state ----
  
  event_base_out_state <- track_base_out_by_event(event_data)
  
  pre_state <- event_base_out_state |>
    dplyr::select(
      event_index, dplyr::starts_with("pre_"), dplyr::starts_with("post_")
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "pre_", "pre_event_"),
      .cols = dplyr::starts_with("pre_")
    ) |>
    dplyr::rename_with(
      .fn = ~ stringr::str_replace(.x, "post_", "post_event_"),
      .cols = dplyr::starts_with("post_")
    ) |>
    dplyr::mutate(
      pre_runner_1b_id = pre_event_runner_1b_id,
      pre_runner_2b_id = pre_event_runner_2b_id,
      pre_runner_3b_id = pre_event_runner_3b_id,
      pre_outs = pre_event_outs
    )
  
  base_out_state <- play_keys |>
    dplyr::filter(!is.na(play_id)) |>   # remove non-play "actions" like stolen base attempts
    dplyr::select(play_id, event_index, play_index) |>
    dplyr::left_join(pre_state, by = "event_index") |>
    # Initialize post-play outs to match pre-play outs (before accounting for runner movement)
    dplyr::mutate(
      post_runner_1b_id = pre_runner_1b_id,
      post_runner_2b_id = pre_runner_2b_id,
      post_runner_3b_id = pre_runner_3b_id,
      post_outs = pre_outs
    )
  
  
  # Step 3. Set up and join helper tables with runner movement ----
  
  runner_movement_from <- list()
  runner_movement_to <- list()
  
  for (base in c("1B", "2B", "3B")) {
    
    runner_id_from_string <- glue::glue("runner_id_from_{tolower(base)}")
    runner_id_to_string <- glue::glue("runner_id_to_{tolower(base)}")
    
    runner_movement_from[[base]] <- runner_movement_consolidated |>
      dplyr::filter(start_base == base) |>
      dplyr::mutate(!!runner_id_from_string := runner_id) |>
      dplyr::select(play_id, dplyr::all_of(runner_id_from_string))
    
    runner_movement_to[[base]] <- runner_movement_consolidated |>
      dplyr::filter(end_base == base) |>
      dplyr::mutate(!!runner_id_to_string := runner_id) |>
      dplyr::select(play_id, dplyr::all_of(runner_id_to_string))
    
    base_out_state <- base_out_state |>
      dplyr::left_join(runner_movement_from[[base]], by = "play_id") |>
      dplyr::left_join(runner_movement_to[[base]], by = "play_id")
  }
  
  play_summary <- runner_movement_consolidated |>
    dplyr::group_by(play_id) |>
    dplyr::summarize(
      outs = sum(outs),
      runs = sum(runs),
      is_pickoff = any(is_pickoff),
      is_pickoff_error = any(is_pickoff_error),
      is_stolen_base = any(is_stolen_base),
      is_caught_stealing = any(is_caught_stealing),
      is_defensive_indiff = any(is_defensive_indiff),
      .groups = "drop"
    )
  
  base_out_state <- base_out_state |>
    dplyr::left_join(play_summary, by = "play_id") |>
    dplyr::mutate(
      outs = dplyr::coalesce(outs, 0),
      runs = dplyr::coalesce(runs, 0),
      is_pickoff = dplyr::coalesce(is_pickoff, FALSE),
      is_pickoff_error = dplyr::coalesce(is_pickoff_error, FALSE),
      is_stolen_base = dplyr::coalesce(is_stolen_base, FALSE),
      is_caught_stealing = dplyr::coalesce(is_caught_stealing, FALSE),
      is_defensive_indiff = dplyr::coalesce(is_defensive_indiff, FALSE)
    )
  
  
  # Step 4. Update play base-out states using runner movement ----
  
  # Iteratively update base-out state play-by-play until the updates propagate through all events
  
  max_plays_per_event <- base_out_state |>
    dplyr::count(event_index) |>
    with(max(n))
  
  for (i in 1:max_plays_per_event) {
    
    # Step 1: Update post state based on runner movement
    base_out_state <- base_out_state |>
      dplyr::mutate(
        post_runner_1b_id = dplyr::case_when(
          !is.na(runner_id_to_1b) ~ runner_id_to_1b,
          !is.na(runner_id_from_1b) ~ NA,
          TRUE ~ pre_runner_1b_id
        ),
        post_runner_2b_id = dplyr::case_when(
          !is.na(runner_id_to_2b) ~ runner_id_to_2b,
          !is.na(runner_id_from_2b) ~ NA,
          TRUE ~ pre_runner_2b_id
        ),
        post_runner_3b_id = dplyr::case_when(
          !is.na(runner_id_to_3b) ~ runner_id_to_3b,
          !is.na(runner_id_from_3b) ~ NA,
          TRUE ~ pre_runner_3b_id
        ),
        post_outs = pre_outs + dplyr::coalesce(outs, 0)
      )
    
    # Step 2: Update pre state based on post state of prior play
    base_out_state <- base_out_state |>
      dplyr::group_by(event_index) |>
      dplyr::mutate(
        pre_runner_1b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_1b_id,
          no = dplyr::lag(post_runner_1b_id, 1)
        ),
        pre_runner_2b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_2b_id,
          no = dplyr::lag(post_runner_2b_id, 1)
        ),
        pre_runner_3b_id = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_runner_3b_id,
          no = dplyr::lag(post_runner_3b_id, 1)
        ),
        pre_outs = ifelse(
          test = play_index == min(play_index),
          yes = pre_event_outs,
          no = dplyr::lag(post_outs, 1)
        )
      ) |>
      dplyr::ungroup()
  }
  
  base_out_state <- base_out_state |>
    dplyr::select(play_id,
                  pre_runner_1b_id, pre_runner_2b_id, pre_runner_3b_id, pre_outs,
                  post_runner_1b_id, post_runner_2b_id, post_runner_3b_id, post_outs, runs_on_play = runs,
                  is_pickoff, is_pickoff_error, is_stolen_base, is_caught_stealing, is_defensive_indiff
    )
  
  return(base_out_state)
}

replace_null <- function(x, replacement = NA) {
  if (is.null(x)) {
    return(replacement)
  } else {
    return(x)
  }
}

extract_fielding_lineup <- function(players) {
  
  first_position <- do.call(
    what = c,
    args = lapply(players, function(x) x$allPositions$code[1])
  ) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::transmute(
      position = as.integer(V1),
      player_id = as.integer(substring(rowname, 3))
    )
  
  is_substitute <- sapply(players, function(x) x$gameStatus)['isSubstitute', ] |>
    sapply(identity) |>
    as.matrix() |>
    as.data.frame() |>
    tibble::rownames_to_column() |>
    dplyr::transmute(
      player_id = as.integer(substring(rowname, 3)),
      is_substitute = V1
    )
  
  fielding_lineup <- first_position |>
    dplyr::left_join(is_substitute, by = "player_id") |>
    dplyr::filter(position != 1, !is_substitute) |>
    dplyr::select(position, player_id) |>
    dplyr::arrange(position)
  
  return(fielding_lineup)
}

download_statsapi <- function(start_date,
                              end_date,
                              level = c("lmb","aaa"),
                              game_type = "R",
                              cl = NULL) {
  
  game <- extract_schedule(start_date, end_date, level)
  game <- game %>%
          filter(game_id %in% games$game_pk)
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

extract_schedule <- function(start_date, end_date, level = c("lmb","aaa"), game_type = "R") {
  
  if (lubridate::year(start_date) != lubridate::year(end_date)) {
    stop("Please choose `start_date` and `end_date` within the same calendar year")
  }
  level <- match.arg(level)
  game_type <- sanitize_game_type(game_type)
  
  start <- format(as.Date(start_date), "%m/%d/%Y")
  end <- format(as.Date(end_date), "%m/%d/%Y")
  sport_id <- switch(level, lmb = 11, aaa = 11)
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

sanitize_game_type <- function(game_type) {
  game_type <- match.arg(
    arg = game_type,
    choices = c(
      "R",  # regular season
      "F",  # first-round playoff series, aka wild card
      "D",  # division series
      "L",  # league championship series
      "W",  # world series
      "S",  # spring training
      "A",  # all-star game
      "E"   # exhibition
    ),
    several.ok = TRUE
  )
  return(game_type)
}

estimate_base_out_run_exp <- function(event) {
  
  base_out_transition <- event |>
    dplyr::filter(event != "Game Advisory") |>
    dplyr::mutate(
      pre_base_out_state = paste0(
        1 * !is.na(pre_runner_1b_id),
        1 * !is.na(pre_runner_2b_id),
        1 * !is.na(pre_runner_3b_id),
        pre_outs
      ),
      post_base_out_state = paste0(
        1 * !is.na(post_runner_1b_id),
        1 * !is.na(post_runner_2b_id),
        1 * !is.na(post_runner_3b_id),
        post_outs
      ),
      runs = runs_on_event
    ) |>
    # Count the number of each base-out transition
    dplyr::count(pre_base_out_state, post_base_out_state, runs) |>
    # Compute the probability of each base-out transition
    dplyr::group_by(pre_base_out_state) |>
    dplyr::mutate(pre_runs = 0, post_runs = runs, prob = n / sum(n)) |>
    dplyr::ungroup() |>
    dplyr::bind_rows(
      tibble::tibble(
        pre_base_out_state = c("0000", "0003"),
        pre_runs = c(0, 0),
        post_base_out_state = c("0000", "0003"),
        post_runs = c(0, 0),
        prob = c(0, 1)
      )
    ) |>
    dplyr::select(dplyr::starts_with("pre_"), dplyr::starts_with("post_"), prob)
  
  # Add an additional dimension (runs already scored) to the state of an inning so that our
  # Markov chain can track the cumulative runs scored in the terminal states.
  base_out_transition_augmented <- tibble::tibble()
  for (extra_runs in 0:9) {   # cap runs already scored at 9 because we have to cap it somewhere
    base_out_transition_augmented <- base_out_transition_augmented |>
      dplyr::bind_rows(
        base_out_transition |>
          dplyr::mutate(
            pre_runs = pre_runs + extra_runs,
            post_runs = pmin(post_runs + extra_runs, 9),
            pre_state = glue::glue("{pre_base_out_state}{pre_runs}"),
            post_state = glue::glue("{post_base_out_state}{post_runs}")
          ) |>
          dplyr::select(pre_state, post_state, prob)
      )
  }
  
  # Convert tibble to matrix for multiplication
  transition_matrix <- base_out_transition_augmented |>
    dplyr::group_by(pre_state, post_state) |>
    dplyr::summarize(prob = sum(prob), .groups = "drop") |>
    dplyr::arrange(post_state) |>
    tidyr::pivot_wider(names_from = post_state, values_from = prob, values_fill = 0) |>
    dplyr::arrange(pre_state) |>
    dplyr::select(-pre_state) |>
    as.matrix()
  
  # Raise transition matrix to 20th power to get terminal state probabilties from each start state.
  # We substitute 20 for infinity because the probability of 20 PA in an inning is very very low.
  transition_matrix_20_steps <- transition_matrix
  for (step in 1:19) {
    transition_matrix_20_steps <- transition_matrix_20_steps %*% transition_matrix
  }
  
  # Extract run expectancy for each start state from the terminal state probabilties
  base_out_run_exp <- transition_matrix_20_steps |>
    tibble::as_tibble() |>
    tibble::add_column(
      pre_state = sort(unique(base_out_transition_augmented$pre_state)), .before = 1
    ) |>
    tidyr::pivot_longer(cols = -pre_state, names_to = "post_state", values_to = "prob") |>
    dplyr::mutate(
      outs = as.integer(substring(post_state, 4, 4)),
      runs = as.integer(substring(post_state, 5, 5))
    ) |>
    dplyr::group_by(pre_state) |>
    dplyr::summarize(exp_runs = weighted.mean(runs, w = prob), .groups = "drop") |>
    dplyr::filter(substring(pre_state, 5, 5) == "0") |>
    dplyr::transmute(
      runner_1b = substring(pre_state, 1, 1) == 1,
      runner_2b = substring(pre_state, 2, 2) == 1,
      runner_3b = substring(pre_state, 3, 3) == 1,
      outs = as.integer(substring(pre_state, 4, 4)),
      exp_runs
    )
  
  return(base_out_run_exp)
}

dw19 <- download_statsapi(start_date = "2019-04-04", end_date = "2019-08-30", level = "lmb")

re24_plays <- data.frame()
RUNS_1 <- data.frame()
re24_plays <- dw19$event

re24_plays <-
re24_plays %>% 
  group_by(game_id) %>% 
  mutate(RUNS = lag(cumsum(runs_on_event)))

re24_plays <-
re24_plays %>% 
  mutate(
    HALF.INNING = paste(game_id,inning,half_inning),
    OUTS.ON.PLAY = as.integer(post_outs) - as.integer(pre_outs),
    RUNS.SCORED = runs_on_event)


re24_plays %>%
  group_by(HALF.INNING) %>%
  summarize(Outs.Inning = sum(OUTS.ON.PLAY), 
            Runs.Inning = sum(RUNS.SCORED),
            Runs.Start = first(RUNS),
            MAX.RUNS = Runs.Inning + Runs.Start) -> half_innings

re24_plays %>%
  inner_join(half_innings, by = "HALF.INNING") %>%
  mutate(RUNS.ROI = abs(MAX.RUNS - RUNS)) -> re24_plays

re24_plays[,c(15:17,19:21)] <- sapply((re24_plays[,c(15:17,19:21)]), as.character)
#re24_plays[is.na(re24_plays[,c(15:17,19:21)])] <- ""

re24_plays <- re24_plays %>%
  mutate_if(is.character, ~replace_na(., ""))
re24_plays <- re24_plays %>%
  mutate_if(is.integer, ~replace_na(., 0))

re24_plays <- as.data.frame(re24_plays)

re24_plays %>% 
  
  mutate(BASES = 
           paste0(ifelse(pre_runner_1b_id == "", 0, 1),
                 ifelse(pre_runner_2b_id == "", 0, 1),
                 ifelse(pre_runner_3b_id == "", 0, 1)
           ),
         STATE = paste(BASES, pre_outs)) -> re24_plays

re24_plays %>%
  mutate(NEW.BASES = 
           paste0(ifelse(post_runner_1b_id == "", 0, 1),
                 ifelse(post_runner_2b_id == "", 0, 1),
                 ifelse(post_runner_3b_id == "", 0, 1)
          ),
         NOUTS = as.integer(pre_outs) + as.integer(OUTS.ON.PLAY),
         NEW.STATE = paste(NEW.BASES, NOUTS)) -> re24_plays


re24_plays %>% 
  filter((STATE != NEW.STATE) | (as.integer(RUNS.SCORED) > 0)) -> re24_plays


re24_plays %>%
  filter(as.integer(Outs.Inning) == 3) -> re24_plays_final

re24_plays_final %>% 
  group_by(STATE) %>%
  summarize(Mean = mean(as.integer(RUNS.ROI))) %>%
  mutate(Outs = substr(STATE, 5, 5)) %>%
  arrange(Outs) -> RUNS_1 


re24n <- matrix(round(RUNS_1$Mean, 2), 8, 3)



dimnames(re24n)[[2]] <- c("0 outs", "1 out", "2 outs")
dimnames(re24n)[[1]] <- c("___", "__3", "_2_", "_23", 
                         "1__", "1_3", "12_", "123")

re_24_1 <- t(re24n)
col.order <- c("___", "1__", "_2_", "__3", "12_", "1_3", "_23", "123")
run_expectancy_matrix <- (t(re_24_1[, col.order]))

print(run_expectancy_matrix)

run_expectancy_matrix_19 <- run_expectancy_matrix

#cp <- matrix(c(0.55,0.91,1.15,1.29,1.35,1.54,1.79,2.15,0.29,0.53,0.63,0.83,0.83,1.02,
#               1.34,1.41,0.14,0.28,0.35,0.37,0.47,0.45,0.65,0.72),8,3)


# Preparation work for finding run values
re24_plays %>%
  
  # Concentrate run values by state
  left_join(select(RUNS_1, -Outs), by = "STATE") %>%
  rename(Runs.State = Mean) %>%
  left_join(select(RUNS_1, -Outs), 
            by = c("NEW.STATE" = "STATE")) %>%
  rename(Runs.New.State = Mean) %>%
  replace_na(list(Runs.New.State = 0)) %>%
  
  # Find run value of each play
  mutate(run_value = Runs.New.State - Runs.State +
           RUNS.SCORED) -> re24_plays

# Create necessary variable
outs <- rep(0, nrow(re24_plays))

# Find plays that resulted in outs
re24_plays %>% 
  filter(OUTS.ON.PLAY > 0) -> outs

# Find run value of outs
outs %>%
  summarize(mean_run_value = mean(run_value)) -> mean_outs

print(mean_outs)

############
# Create necessary variable
hbp <- rep(0, nrow(re24_plays))

# Find all instances of a single
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("Hit By Pitch", re24_plays$event[i]))) {
    hbp[i] <- 1
  }
}

# Select plays with a single
re24_plays %>% 
  mutate(hbps = hbp) %>% 
  filter(hbps == 1) -> hbps

# Find run value of singles by finding the mean run values of all plays with
# a single
hbps %>%
  summarize(mean_run_value = mean(run_value)) -> mean_hbps

# Find the run value of a single to that of an out
HBP <- mean_hbps - mean_outs

print(HBP)
##############

# Create necessary variable
sf <- rep(0, nrow(re24_plays))
ibb <- rep(0, nrow(re24_plays))

# Find all instances of a sacrifice fly
for (i in 1:nrow(re24_plays)){
  if (isTRUE(grepl("Sac Fly", re24_plays$result.description[i]))) {
    sf[i] <- 1
  }
  if (isTRUE(grepl("Intent Walk", re24_plays$result.description[i]))) {
    ibb[i] <- 1
  }
}

# Select plays with a sacrifice fly
re24_plays %>% 
  mutate(sf = sf, ibb = ibb) -> re24_plays

# Create "wOBA Multiplier" by calculating the league wOBA figure
woba_multiplier <- 
  (HBP*nrow(hbps) + BB*nrow(walks) + SINGLE*nrow(singles) + DOUBLE*nrow(doubles) 
   + TRIPLE*nrow(triples) + HOME_RUN*nrow(home_runs))/(nrow(re24_plays) - sum(ibb) 
                                                 - sum(sf))
# Calculate the league OBP figure
league_obp <- 
  (nrow(hbps) + nrow(walks) + nrow(singles) + nrow(doubles) + nrow(triples) 
   + nrow(home_runs))/(nrow(re24_plays) - sum(ibb))

woba_scale <- league_obp / woba_multiplier

# Multiply the play run values by the wOBA scale to obtain the weights used in
# the wOBA calculation
woba_weights <- c(woba_scale*HBP, woba_scale*BB, woba_scale*SINGLE, 
                  woba_scale*DOUBLE, woba_scale*TRIPLE, woba_scale*HOME_RUN)

# Create an easily readable table containing the wOBA weights
linear_weights_table <- as.data.frame(woba_weights)

# Ensure the table is named correctly
colnames(linear_weights_table) <- c("HBP", "BB", "1B", "2B", "3B", "HR")
#rownames(linear_weights_table) <- c("Weights")

# View the final product
print(linear_weights_table)

woba_fipc_19 <- linear_weights_table %>%
  mutate(Season = 2019,
          wOBA = woba_multiplier$mean_run_value,
         "wOBAScale" = woba_scale$mean_run_value,
         `R/PA` = 0.154,
         cFIP = 5.79 - (((13*2391)+(3*(997+6657))-(2*13037))/16461)) %>%
  relocate(Season, wOBA, "wOBAScale") %>%
  rename(
        ,`wHBP` = `HBP`
        ,`wBB` = `BB`
        ,`w1B` = `1B`
        ,`w2B` = `2B`
        ,`w3B` = `3B`
        ,`wHR` = `HR`
  )



#woba_fipc_24 <- read_csv("Documents/lmb_statsapp/lmb_stats/woba_fipc.csv")
#woba_fipc_24 <- woba_fipc_24[-1]


woba_fipc <- rbind(woba_fipc_24, woba_fipc_23, woba_fipc_22, woba_fipc_21, woba_fipc_19)

write.csv(woba_fipc,"/Users/axel.mora/Documents/lmb_statsapp/lmb_stats/woba_fipc.csv")
#, woba_fipc_22, woba_fipc_21)