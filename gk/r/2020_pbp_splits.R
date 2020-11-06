nflpackages <- c('devtools', 'nflfastR', 'bitops', 'RCurl', 'ggplot2', 'nnet', 'magrittr', 'bindrcpp', 'tidyverse', 'tibble', 'tidyr', 'readr', 'purrr', 'dplyr', 'ggjoy', 'na.tools')
lapply(nflpackages, require, character.only = TRUE)

sched <- readRDS(url(
  "http://www.habitatring.com/games.rds"
)) %>% filter(season == 2020 & week < 10)

pbp <- sched %>% pull(game_id) %>%
  fast_scraper(pp = TRUE) %>%
  clean_pbp() %>%
  add_qb_epa() %>%

  add_xyac()

saveRDS(pbp, "~/GitHub/nflfastR/data-raw/pbp_data/play_by_play_2020.rds")

pbp <- readRDS("~/GitHub/nflfastR/data-raw/pbp_data/play_by_play_2020.rds")
# pbp <- scrape_season_play_by_play(2019, type = "reg") if fresh is needed, able to ensure unique plays by using pbp %>% mutate(playck = paste0(game_id,play_id)) %>% distinct(playck, .keep_all = TRUE)


pbp_df <- pbp %>%
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run") %>%
  mutate(
    playck = paste0(game_id,play_id),
    year=2020,
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
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)) %>% distinct(playck, .keep_all = TRUE)

#Team totals for skill player analysis #####
team_agg_df = pbp_df %>%
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

#Make QB DF######
qb_df_adj = pbp_df %>% filter(play_type=='pass'&pass_attempt==1) %>% group_by(passer_player_id, passer_player_name, Tm=posteam) %>% summarise(
  Att=sum(pass_attempt),
  Comp=sum(complete_pass),
  Yds=sum(yards_gained, na.rm=TRUE),
  AY=sum(air_yards, na.rm=TRUE),
  aDOt=AY/Att,
  TDs=sum(touchdown),
  Ints=sum(interception),
  Fmb=sum(fumble),
  cpoe=
  Sk=sum(sack),
  Sk_yds=sum(ifelse(sack==1, yards_gained, 0)),
  apacr = (Yds + TDs * 20 - 45 * (Ints + Fmb))/
    AY,
  aya = (Yds + (20*TDs)-(45*Ints))/Att,
  anya = (Yds + (TDs * 20) - (45 * Ints)-Sk_yds)/(Att+Sk),
  pacr = Yds/AY,
  epa = sum(epa,na.rm=TRUE),
  wpa = sum(wpa,na.rm=TRUE)
) %>% filter(Att>46) %>%  arrange(-epa)

# Skill Player Data -------------------------------------------------------
wr_df_adj <- pbp_df %>% filter(!is.na(receiver_player_id), play_type=="pass"&qb_dropback==1) %>% group_by(receiver_player_id, receiver_player_name, posteam)%>%
  transmute(
    target = pass_attempt,
    rec = complete_pass,
    yards_gained = yards_gained,
    air_yards = air_yards,
    yac = yards_after_catch,
    td = touchdown,
    epa,
    wpa) %>%
  summarize_all(sum, na.rm = TRUE) %>%
  left_join(team_agg_df, by = "posteam") %>%
  mutate(
    ms_team_targets = target/team_pass_attempts,
    ms_team_air_yards = air_yards/team_air_yards,
    wopr = 1.5 * (ms_team_targets) + 0.7 * (ms_team_air_yards),
    racr = yards_gained/air_yards,
    aDOT = air_yards/target) %>%
  select(-c(team_dropbacks, team_pass_attempts, team_rush_attempts, team_opportunities, team_air_yards, team_total_yards)) %>% ungroup() %>% filter(target>10) %>% arrange(-target)

# select(play_id, game_id, posteam, game_date, desc, play_type, yards_gained, qb_dropback, qb_scramble, air_yards, yards_after_catch, epa, wpa, rush_attempt, pass_attempt, sack, touchdown, pass_touchdown, rush_touchdown, complete_pass, fumble, passer_player_id, passer_player_name, receiver_player_id, receiver_player_name, rusher_player_id, rusher_player_name, pass, rush, success, play_type2, skill_ID, dropback_ID, year)


# Misc --------------------------------------------------------------------
#
# #Keep only Runs and Passes for 2019
# pbp <- read_csv("reg_pbp_2019.csv")
# pbp_rp <- pbp %>%
#   filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")
#
# pbp_rp <- pbp_rp %>%
#   mutate(
#     pass = if_else(str_detect(desc, "(pass)|(sacked)|(scramble)"), 1, 0),
#     rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
#     success = ifelse(epa>0, 1 , 0)
#   )
# pbp_rp_19 <- pbp_rp %>% filter(pass==1 | rush==1)
# write_csv(pbp_rp, "~/Github/nflscrapR/data-scrapR/raw/2019_pbp_rp.csv")
#
# #Read in old data, join and save.
# pbp_all_rp <- readRDS("~/GitHub/nflscrapR/data-scrapR/reg_pbp_all_rp.rds")
# reg_pbp_all_rp <- dplyr::bind_rows(pbp_all_rp, pbp_rp_19)
# saveRDS(reg_pbp_all_rp, "~/Github/nflscrapR/data-scrapR/reg_pbp_all_rp.rds")
#
# # aDOT --------------------------------------------------------------------
# pbp_aDOT <- pbp_data %>%
#   filter(PlayType == "Pass") %>%
#   group_by("Receiver") %>%
#   summarize(adot = mean(air_yards), targets = n(), catch_rate = sum(complete_pass)/targets)
