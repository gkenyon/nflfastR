nflpackages <- c('devtools', 'nflfastR', 'XML', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

pbp <-  read_rds("data/play_by_play_2020.rds")


pbp_df <- pbp %>%
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    playck = paste0(game_id,play_id),
    year=2019,
    qb_scramble = ifelse(qb_scramble==1&play_type=="pass",0,qb_scramble),
    play_type2 = as.factor(ifelse(qb_scramble==1|play_type=="pass", "dropback",play_type)),
    skill_ID = ifelse(play_type=="run",rusher_player_id,ifelse(
      play_type=="dropback",receiver_player_id,NA)),
    opp_yds = ifelse(air_yards < 0|is.na(air_yards), 0, air_yards),
    adj_opp_yds = ifelse(opp_yds == 0, 1, opp_yds),
    dropback_ID = ifelse(qb_dropback==1, ifelse(
      qb_scramble==1,skill_ID,passer_player_id),"None"),
    skill_ID = ifelse(play_type2=="run",rusher_player_id,ifelse(
      play_type2=="dropback",receiver_player_id,NA)),
    pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0))

#Team totals for skill player analysis #####
team_agg_df <- pbp_df %>%
  filter(
  !is.na(home_wp),
  !is.na(away_wp),
  timeout == 0) %>%
  group_by(posteam) %>%
    summarize(
  team_dropbacks = sum(qb_dropback, na.rm = T),
  team_pass_attempts = sum(pass_attempt, na.rm = T),
  team_rush_attempts = sum(rush_attempt, na.rm = T),
  team_opportunities = team_pass_attempts+team_rush_attempts,
  team_air_yards = sum(air_yards, na.rm = T),
  team_total_yards = sum(yards_gained, na.rm = T)
)
