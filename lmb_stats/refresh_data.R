library(googlesheets4)

cache_dir = "cache"
gs_ids <- list(
  woba_fipc = "1H8xuzPAjuNJxlWaBAk1fmo-lGH9Jfm0Zeyrcv0R5OPY",
  park_factors = "1VkbsNGuHrhZHYoMdxiWVjeUyc3kSMLafYpg0nLHHdcY",
  fielding_std = "1sbVBgIjyYUqIRCAIwDZpWQz25u5nTayRfUcNt_zCSlo",
  fielding_adv = "17_HvfhVProIHQXWVNKk02HhS65ujryTJBrk54oOxCTc",
  hitting_std = "1YdS-ADBbfZS1Z9YOuWUbGIj2mOZ4tmS1rQBmunhY414",
  hitting_adv = "1toeJeYcCvlauqXNPlG3WLv13uPH6ZQVBg2qV93raC-k",
  pitching_std = "1rJGvqEWKeQTRRCaMd6AYujMOCU4Ibe0-uDPEMcg67ag",
  pitching_adv = "1V9mHwpPeY-Pq8yfZtmAyYT94v5M2QYx25aSVYygX2Dc",
  team_fielding_std = "1O7qE1BRoiMMKCU77FbpBO_ZVKjOjXdoHCDCMLuK7KM8",
  team_fielding_adv = "1cf9H4oF88Nf_Br69TPTMm6A2OnAtr4KLnD8RF0CW-RA",
  team_hitting_std = "1OZAMvh_0OyVoA3RRnDmVr8ljhpUzFaj2NgUcbFbXZV0",
  team_hitting_adv = "1p1J3aslmnA5fjMHiCuQu16Fn6nF4b7-tOUmGntLlOMI",
  team_pitching_std = "1sqaj3q3QoPOMO0GPUf1ydr96_x7VornWwF3U9lEn05k",
  team_pitching_adv = "1BtBwDEU2NyiNgn-isEc5ObM-Nu6VrAotldexFauVofs",
  lmb_att_24 = "1uer8QQuM-x8VyxCDJBlCtqXofPHE-Dlkzzk64xzLFBQ",
  lmb_pace_24 = "1sJ_KjQgmKDUtLRh1MW0yHyQWTzllarKHXGzSGrLNVBk",
  lmb_pace_venue_24 = "1_zF8o6iYKrcpE0Cwgky4dpm7gXMt4nU1At7Z4j4qFJI",
  game_logs = "11gdMD1brR01ZuW31b9JpdEj0Jx3qM0CAxhZdNARVLtk",
  hitting_cp = "1K-wOBfh9QW4ucEShjypBXQkti962Yajyf-Da5QEKbg0"
)

refresh_data <- function(gs_ids) {
  datasets <- list()
  for (name in names(gs_ids)) {
    file_path <- file.path(cache_dir, paste0(name, ".rds"))
    print(gs_ids[[name]])
    data <- read_sheet(gs_ids[[name]])
    saveRDS(data, file_path)
    print(Sys.time())
  }
}

refresh_data(gs_ids)  

