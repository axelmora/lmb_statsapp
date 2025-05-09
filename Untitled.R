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
    pitch_type = play_data$details$type$code,
    ax = play_data$pitchData$coordinates$aX,
    ay = play_data$pitchData$coordinates$aY,
    az = play_data$pitchData$coordinates$aZ,
    vx0 = play_data$pitchData$coordinates$vX0,
    vy0 = play_data$pitchData$coordinates$vY0,
    vz0 = play_data$pitchData$coordinates$vZ0,
    x0 = play_data$pitchData$coordinates$x0,
    z0 = play_data$pitchData$coordinates$z0,
    extension = replace_null(play_data$pitchData$extension),
    strike_zone_top = play_data$pitchData$strikeZoneTop,
    strike_zone_bottom = play_data$pitchData$strikeZoneBottom,
    launch_speed = play_data$hitData$launchSpeed,
    launch_angle = play_data$hitData$launchAngle,
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
                  description, pitch_type, ax, ay, az, vx0, vy0, vz0, x0, z0, extension,
                  strike_zone_top, strike_zone_bottom, launch_speed, launch_angle, hit_coord_x, hit_coord_y
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