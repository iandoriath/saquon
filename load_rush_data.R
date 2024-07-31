library(tidyverse)
library(nflreadr)
library(lubridate)
library(caret)
library(furrr)

plan(multisession, workers = 12)

filename_addons =c('', paste0(' (', seq(1:10), ')'))

rushing_summary = tibble()
offense_run_blocking = tibble()
passing_summary = tibble()

for(i in 1:length(filename_addons)){
    rushing_summary = rushing_summary %>% 
      bind_rows(read_csv(paste0('rushing_summary', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
    
    offense_run_blocking = offense_run_blocking %>% 
      bind_rows(read_csv(paste0('offense_run_blockng', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
    
    passing_summary = passing_summary %>% 
      bind_rows(read_csv(paste0('passing_summary', filename_addons[i], '.csv')) %>% mutate(year=2024-i))
}

offense_run_blocking = offense_run_blocking %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(team_name = if_else(team_name == 'SD', 'LAC', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'SL', 'LA', team_name)) %>% 
  mutate(team_name = if_else(team_name == 'OAK', 'LV', team_name)) %>% 
  group_by(team_name, year) %>% 
  summarize(run_block_grade = weighted.mean(grades_run_block, player_game_count)) 

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
  summarize(pass_grade = weighted.mean(grades_pass, player_game_count))

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
  left_join(offense_run_blocking)

nested_rb_data = rb_data %>% 
  select(-position, -team_name, -franchise_id) %>% 
  filter(attempts > 17) %>% 
  group_by(player) %>% 
  nest()

players = nested_rb_data$player %>% unique()

contracts = load_contracts()

rb_data = contracts %>% 
  filter(player %in% players) %>% 
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
