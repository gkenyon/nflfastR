#update_current_season_pbp.R

library(nflfastR)
library(tidyverse)

## SCRAPE ONGOING SEASON

y <- dplyr::if_else(
  lubridate::month(lubridate::today("America/New_York")) >= 9,
  lubridate::year(lubridate::today("America/New_York")) ,
  lubridate::year(lubridate::today("America/New_York")) - 1
)


# # get existing pbp
# existing_pbp <- readRDS('data/play_by_play_2020.rds')
# # get IDs of scraped games
# already_scraped <- existing_pbp %>%
#   dplyr::pull(game_id) %>%
#   unique()

#get completed games
sched <- readRDS(url(
  "http://www.habitatring.com/games.rds"
)) %>%
  filter(season == y, !is.na(result)) %>%
  pull(game_id)

# figure out which games we need
need_scrape <- sched[!sched %in% already_scraped]

# grab the games we need
new_pbp <- fast_scraper(need_scrape, pp = FALSE) %>%
  clean_pbp() %>%
  add_qb_epa() %>%
  add_xyac()

pbp <- bind_rows(
  existing_pbp,
  new_pbp
)


# rds
saveRDS(pbp, glue::glue('data/play_by_play_{y}.rds'))
