library(tidyverse)
library(nflreadr)
library(lubridate)
library(caret)
library(furrr)

fix_schedule_names = function(df){
  df = df %>% 
  mutate(home_team = if_else(home_team == 'ARI', 'ARZ', home_team)) %>%
    mutate(home_team = if_else(home_team == 'SD', 'LAC', home_team)) %>% 
    mutate(home_team = if_else(home_team == 'STL', 'LA', home_team)) %>% 
    mutate(home_team = if_else(home_team == 'OAK', 'LV', home_team)) %>% 
    mutate(away_team = if_else(away_team == 'ARI', 'ARZ', away_team)) %>%
    mutate(away_team = if_else(away_team == 'SD', 'LAC', away_team)) %>% 
    mutate(away_team = if_else(away_team == 'STL', 'LA', away_team)) %>% 
    mutate(away_team = if_else(away_team == 'OAK', 'LV', away_team)) %>% 
    mutate(home_team = if_else(home_team == 'BAL', 'BLT', home_team)) %>% 
    mutate(away_team = if_else(away_team == 'BAL', 'BLT', away_team)) %>% 
    mutate(home_team = if_else(home_team == 'CLE', 'CLV', home_team)) %>% 
    mutate(away_team = if_else(away_team == 'CLE', 'CLV', away_team)) %>% 
    mutate(home_team = if_else(home_team == 'HOU', 'HST', home_team)) %>% 
    mutate(away_team = if_else(away_team == 'HOU', 'HST', away_team)) %>% 
    return()
}

plan(multisession, workers = 12)

filename_addons =c('', paste0(' (', seq(1:10), ')'))

rushing_summary = tibble()
offense_run_blocking = tibble()
passing_summary = tibble()
run_def_summary = tibble()

for(i in 1:length(filename_addons)){
    rushing_summary = rushing_summary %>% 
      bind_rows(read_csv(paste0('data/rushing_summary', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
    
    offense_run_blocking = offense_run_blocking %>% 
      bind_rows(read_csv(paste0('data/offense_run_blockng', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
    
    passing_summary = passing_summary %>% 
      bind_rows(read_csv(paste0('data/passing_summary', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
    
    run_def_summary = run_def_summary %>% 
      bind_rows(read_csv(paste0('data/run_defense_summary', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
}

run_def_summary = run_def_summary %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(team_name = if_else(team_name == 'SD', 'LAC', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'STL', 'LA', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'OAK', 'LV', team_name)) %>% 
  group_by(team_name, year) %>% 
  summarize(run_def_grade = weighted.mean(grades_run_defense, snap_counts_run, na.rm=TRUE)) 

schedules = nflreadr::load_schedules(2013:2023) %>% 
  fix_schedule_names()

opponents = expand_grid(year = 2013:2023, team_name = unique(run_def_summary$team_name)) %>% 
  mutate(home_opponents = future_map2(year, team_name, ~schedules %>% filter(home_team == .y & season == .x)),
         away_opponents = future_map2(year, team_name, ~schedules %>% filter(away_team == .y & season == .x)))

run_def_summary = opponents %>% 
  mutate(home_run_grade = map_dbl(home_opponents, ~.x %>% 
                               mutate(year = season) %>% 
                               select(year, away_team) %>% 
                               rename(team_name = away_team) %>% 
                               left_join(run_def_summary, by=c('team_name', 'year')) %>% 
                               summarize(run_def_grade = mean(run_def_grade, na.rm=TRUE)) %>% 
                               pull(run_def_grade))) %>% 
  mutate(away_run_grade = map_dbl(away_opponents, ~.x %>% 
                                    mutate(year = season) %>% 
                                    select(year, home_team) %>% 
                                    rename(team_name = home_team) %>% 
                                    left_join(run_def_summary, by=c('team_name', 'year')) %>% 
                                    summarize(run_def_grade = mean(run_def_grade, na.rm=TRUE)) %>% 
                                    pull(run_def_grade))) %>% 
  mutate(home_games = map_int(home_opponents, nrow),
         away_games = map_int(away_opponents, nrow)) %>% 
  select(-home_opponents, -away_opponents) %>% 
  mutate(run_def_score = (home_run_grade * home_games + away_run_grade * away_games)/(home_games+away_games)) %>% 
  select(year, team_name, run_def_score)

offense_run_blocking = offense_run_blocking %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(team_name = if_else(team_name == 'SD', 'LAC', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'SL', 'LA', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'OAK', 'LV', team_name)) %>% 
  group_by(team_name, year) %>% 
  summarize(run_block_grade = weighted.mean(grades_run_block, player_game_count, na.rm=TRUE)) 

offense_run_blocking %>% 
  ggplot(aes(x=year, y=run_block_grade, color=team_name)) + geom_line() + facet_wrap(~team_name) +
  scale_x_continuous(breaks=2012:2023, minor_breaks = NULL) +
  theme_bw() + theme(axis.text.x = element_text(size=8))

passing_summary = passing_summary %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(team_name = if_else(team_name == 'SD', 'LAC', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'SL', 'LA', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'OAK', 'LV', team_name)) %>% 
  group_by(team_name, year) %>% 
  slice_max(aimed_passes) %>% 
  summarize(pass_grade = weighted.mean(grades_pass, player_game_count, na.rm=TRUE))

passing_summary %>% 
  ggplot(aes(x=year, y=pass_grade, color=team_name)) + geom_line() + facet_wrap(~team_name) +
  scale_x_continuous(breaks=2012:2023, minor_breaks = NULL) +
  theme_bw() + theme(axis.text.x = element_text(size=8))

rb_data = rushing_summary %>% 
  filter(position == 'HB') %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(team_name = if_else(team_name == 'SD', 'LAC', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'SL', 'LA', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'OAK', 'LV', team_name)) %>% 
  left_join(passing_summary) %>% 
  left_join(offense_run_blocking) %>% 
  left_join(run_def_summary)


nested_rb_data = rb_data %>% 
  select(-position, -team_name, -franchise_id) %>% 
  filter(attempts > 17) %>% 
  group_by(player) %>% 
  nest()

players = nested_rb_data$player %>% unique()

contracts = load_contracts()

top_rbs = contracts %>% filter(position == 'RB') %>% 
  filter(apy_cap_pct > quantile(apy_cap_pct, 0.5)) %>% pull(player)

rb_data = contracts %>% 
  filter(player %in% players) %>% 
  filter(player %in% top_rbs) %>% 
  left_join(nested_rb_data)

rb_data = rb_data %>% 
  mutate(data = map2(data, year_signed, ~filter(.x, year < .y))) %>% 
  mutate(nrow_data = map(data, nrow)) %>% 
  filter(nrow_data >= 1) %>% 
  select(-nrow_data)

all_years = rb_data %>%
  mutate(years = map(data, ~unique(.x$year))) %>%
  pull(years) %>%
  unlist() %>%
  unique() %>%
  sort()

process_player_data = function(df, year_signed){

  df = df %>% 
    mutate(years_before_contract = year-year_signed) %>% 
    select(-year)
  
  transposed_data = as_tibble(cbind(nms = names(df), t(df))) %>% 
    mutate(across(-nms, as.numeric)) %>% 
    filter(nms != 'player_id')
  
  names(transposed_data) = c('nms', 
    transposed_data %>% 
      filter(nms == 'years_before_contract') %>% 
      select(-nms) %>% t() %>% as.character())
  
  transposed_data %>% 
    filter(nms != 'years_before_contract') %>% 
    pivot_wider(names_from=nms, values_from=-c(nms)) %>% 
    return()
}

rb_data = rb_data %>% 
  mutate(data = map2(data, year_signed, ~process_player_data(.x, .y))) %>% 
  group_by(player, year_signed) %>% 
  slice_max(apy_cap_pct) %>% 
  slice_head(n=1) %>% 
  ungroup()

output_data = rb_data %>% 
  filter(player != 'Saquon Barkley') %>%
  select(-player, -position, -team, -is_active, -player_page, -years, -value, -apy, 
         -guaranteed, -inflated_value, -inflated_apy, -inflated_guaranteed, -player_page, 
         -otc_id, -college, -draft_round, -draft_team, -cols, -date_of_birth, -height, -weight) %>% 
  select(-starts_with('grade')) %>%
  unnest(data)

output_data %>% write_csv('rb_data.csv')

saquon_data = rb_data %>% 
  filter(player == 'Saquon Barkley') %>%
  select(-player, -position, -team, -is_active, -player_page, -years, -value, -apy, 
         -guaranteed, -inflated_value, -inflated_apy, -inflated_guaranteed, -player_page, 
         -otc_id, -college, -draft_round, -draft_team, -cols, -date_of_birth, -height, -weight) %>% 
  select(-starts_with('grade')) %>%
  unnest(data)

# Add missing columns to saquon_data
missing_cols = setdiff(names(output_data), names(saquon_data))

# Initialize new columns with NA values
for (col in missing_cols) {
  saquon_data[[col]] = NA
}

names(saquon_data)

saquon_data %>% write_csv('saquon_data.csv')
