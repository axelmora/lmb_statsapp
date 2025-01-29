event_endpoint <- "https://statsapi.mlb.com/api/v1.1/game/770483/feed/live"
event_json <- jsonlite::fromJSON(event_endpoint)

event_data <- event_json$liveData$plays$allPlays

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


event_base_out_state <- track_base_out_by_event(event_data)

event_without_fielder_id <- tibble::tibble(
  game_id = 770483,
  event_index = event_data$about$atBatIndex,
  inning = event_data$about$inning,
  half_inning = event_data$about$halfInning,
  batter_id = event_data$matchup$batter$id,
  bat_side = event_data$matchup$batSide$code,
  pitcher_id = event_data$matchup$pitcher$id,
  pitch_hand = event_data$matchup$pitchHand$code,
  event = event_data$result$event,
  is_out = event_data$result$isOut,
  runs = event_data$result$awayScore + event_data$result$homeScore,
  runs_on_event = sapply(event_data$runners,
                         FUN = function(x) sum(dplyr::coalesce(x$movement$end, "") == "score")
  )
) |>
  dplyr::left_join(event_base_out_state, by = "event_index")
