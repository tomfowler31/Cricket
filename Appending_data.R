#Cricket Analysis
#Oct 2022
#Combine 10k volume of cricsheet.org https://cricsheet.org/downloads/#experimental

library("plyr")
library("dplyr")
library("readr")
library("stringr")
library("tidyverse")
library("fs")
library("tidyr")

#########################################
###### ====== MATCH FIES =======#########
#########################################

data_all <- list.files(path = "C:/Users/Tom/Documents/Data/Cricket/Data/Tests",  # Identify all CSV files
                       pattern = "\\d.csv", full.names = TRUE) %>% 
  
  #lapply(
  read_csv (col_types = cols(season = 'c', penalty = 'i', other_wicket_type = 'c', other_player_dismissed = 'c' , .default = col_guess())) %>% # Store all files in list
  bind_rows # Combine data sets into one data set 

#simple line count, and note created for runouts later on
match <- data_all %>%
  mutate(balls_faced = 1) %>%
  mutate(note = 'none')

#over as the actual over it is classed as (i.e. no over zero)
detailsA <- match %>%
  mutate(over = floor(ball)+1)

#ball number in over
detailsB <- detailsA %>%
  group_by(match_id, batting_team,innings, over) %>%
  mutate(ball_in_over = cumsum(balls_faced))

#bowlers cumulative over
detailsC <- detailsB %>%
  group_by(match_id, batting_team, innings, bowler) %>%
  mutate(bowler_over = cumsum(c(1, diff(over) != 0)))

#wides and penalty come through as logical
detailsD <- detailsC %>%
  mutate(wides = as.numeric(wides))%>%
  mutate(penalty = as.numeric(penalty))

#wides / noballs come through as NA
detailsE <-detailsD %>%
  mutate_if(is.numeric, ~replace_na(., 0))

#partnership / wicket number (cumulative count of wicket / partnerships (to get batting partnerships scaffolded). Thanks ChatGPT. 
detailsF <- detailsE %>%
  group_by(match_id, batting_team, innings) %>%
  mutate(batting_part = cumsum(!is.na(wicket_type))+1) %>% #+1 means starts at 1.
  mutate(batting_partnership = lag(batting_part)) #need to have the partnership lag a row (i.e. so the new partnership starts the ball after the wicket)
#not quite there yet - first row appears as "NA"

# runs off ball and cumulative
detailsG <- detailsF %>%
  mutate(runs_score = runs_off_bat + extras) %>%
  group_by(match_id, batting_team,innings) %>%
  mutate(running_score = cumsum(runs_score)) 

#cumulative balls faced
detailsH <- detailsG %>%
  group_by (match_id,batting_team, innings, striker) %>%
  mutate(cum_balls_faced = cumsum(balls_faced))

#partnerships naming
detailsI <- detailsH %>%
  group_by (match_id,batting_team, innings, batting_partnership) %>%
  mutate(cum_partnership_runs_faced = cumsum(balls_faced)) %>%
  mutate(cum_partnership_runs_scored = cumsum(runs_score)) %>%
  mutate(cum_partnership_runs_scored_off_bat = cumsum(runs_off_bat))

#running score on batsmen
detailsJ <- detailsI %>%
  group_by (match_id,batting_team, innings, striker) %>%
  mutate(cum_runs_off_bat = cumsum(runs_off_bat))

## accounting for non striker being runout 

match_runout <- detailsJ %>%
  filter(striker != player_dismissed)

match_runout <- match_runout %>%
  mutate(non_striker = striker) %>%
  mutate(striker = player_dismissed) %>%
  mutate(balls_faced = 0) %>%
  mutate(note = 'non facing out')


#testing - iterative process for updates
details <- bind_rows(detailsJ, match_runout)

bowler_wickets <- c("bowled", "caught", "lbw", "stumped","caught and bowled", "hit wicket")
team_wickets <- c("run out", "hit wicket", "obstructing the field", "retired hurt")
#retired not out doesn't count as a wicket

details <- details %>%
  mutate(bowl_wickets = ifelse(wicket_type %in% bowler_wickets,1,0)) %>%
  mutate(team_wickets = ifelse(wicket_type %in% team_wickets,1,0))

#### chunk start 
##code to create columns of runs scored
##duplicate runs_off_bat so pivot_wider doesnt drop it
details <- details %>%
  mutate(runs_off_bat2 = runs_off_bat)

append_integer_column <- function(df, int_column_name) {
  # Convert integer values to binary
  df <- df %>%
    pivot_wider(names_from = {{int_column_name}}, values_from = runs_off_bat2, values_fill = list(runs_off_bat2 = 0))
  
  return(df)
}

# Append integer column as additional columns within the dataframe
details <- append_integer_column(details, int_column_name = "runs_off_bat2")
### chunk complete

#to prep ready to  export
as.data.frame(details)


rm(match,data_all,detailsA,detailsB, detailsC, detailsD, detailsE, detailsF, detailsG, detailsH, detailsI, detailsJ, match_runout)

# full match details export
write.csv(details, "C:/Users/Tom/Documents/Data/Cricket/Output/ball_by_ball.csv", row.names = TRUE )
#########################################
###### ====== INFO FILES =======#########
#########################################


##create match _info.csv files###
#used later on for extracting path

setwd("C:/Users/Tom/Documents/Data/Cricket/Tests Data")

info_files <- list.files(pattern = "\\d{5,6}_info.csv")

#reading in files, skipping first line, specify column names, as default variability [runtime is increased], extracting match id from the file path
dat <- read_csv(info_files, skip = 1, col_names = c("info", "measure", "detail"), id = "path")


#https://stackoverflow.com/questions/49500569/using-regular-expressions-in-r-to-grab-numbers-from-a-string
#extract file path from path
dat <- dat %>%
  mutate(path = basename(path)) %>% #basename for file name
  mutate(path = str_extract(path, "(\\d+)")) %>% #matchID 
  mutate(match_id = path)

  #drop info column 
game_details <- dat  %>%
  select(-info)

#drop dat
rm(dat)

#create copy of game_details for player lookup and match id's for player specific work

game_details_players <- game_details %>%
  filter(measure %in% c("players", "player"))%>%
  mutate(match_id = as.numeric(path))

# check all games included (this is rubbish)
#count_games <- game_details_players %>%
# mutate(games = n_distinct(match_id))

#manual dropping of columns
game_details <- base::subset(game_details, !measure == "registry")

game_details <- base::subset(game_details, !measure == "player")

game_details <- base::subset(game_details, !detail == "")

#cross tab for use in visual mapping
game_details_crosstab <-game_details %>%
  pivot_wider(names_from = "measure", values_from = "detail", values_fn = toString)

#ODI GAMES 2023/24 WORLD CUP
#game_details_crosstab_ODI <- game_details_crosstab %>%
#filter(event == "ICC Cricket World Cup") %>%
#filter(season == "2023/24")

#odi_wc_games <- game_details_crosstab_ODI %>%
# distinct(match_id) %>%
#  mutate(match_id = as.double(match_id))

#selected_ball_by_ball <- 
 # plyr::join(details, odi_wc_games, by = "match_id", type = "inner", match = "all") 

#selected_ball_by_ball <- inner_join(details, odi_wc_games, by = "match_id")


#export
write.csv(game_details_crosstab, "C:/Users/Tom/Documents/Data/Cricket/Output/Anderson/info_data.csv", row.names = TRUE )

################### Match Info List #######
##includes format, gender, international
### formatting the README FILE into the following headers:
## rework required as '-' tab has some issues with team names "MID-WEST" e.g.
###date,type , format , gender , matchid , teams 
# 2023-11-08 - international - ODI - male - 1384431 - England vs Netherlands

#import
match_info <- read.csv("C:/Users/Tom/Documents/Data/Cricket/Data/Tests/readme_match_info.csv")

#clean up whitespace
match_info <- match_info %>%
  mutate(type = str_trim(type)) %>%
  mutate(format = str_trim(format)) %>%
  mutate(gender = str_trim(gender)) %>%
  mutate(teams = str_trim(teams,side = "left"))

write.csv(match_info, "C:/Users/Tom/Documents/Data/Cricket/Output/Anderson/match_info.csv", row.names = TRUE )


### searching for certain players  ################
## export all the games they were involved in
## needs to refine the 

#
#how to split out a column by the colum
game_details_players <- game_details_players %>%
  select(-path, -measure) %>%
  mutate(player = str_split_fixed(as.character(detail), ",", 2)) %>%
  mutate(player_3 =  data.frame(do.call('rbind', strsplit(as.character(game_details_players$detail), ',', fixed = FALSE))))

#create a game register of all games and players
game_register <- distinct(game_details_players, match_id, player_3$X2)

#specify players of interest 
players <- c("JM Anderson")

#brings back the games they were registered in
specific_matches_inc_players <- game_register %>%
  filter(`player_3$X2`%in% players)

#distinct list of games
specific_matches <- specific_matches_inc_players %>%
  distinct(match_id) %>%
  mutate(match_id = as.character(match_id))


#filter all games to just bring back their scorecard details
selected_ball_by_ball <- 
  plyr::join(details, specific_matches, by = "match_id", type = "inner", match = "all")

#export
#write.csv(selected_ball_by_ball, "C:/Users/Tom/Documents/Data/Cricket/Output/Anderson/selected_ball_by_ball.csv", row.names = TRUE )

##### summary by player and game ########


selected_batting_columns <- c("runs_off_bat", "balls_faced", "bowl_wickets", "team_wickets","0","1","2","3","4","5","6","7","8")

batting_stats <- selected_ball_by_ball %>%
  group_by(match_id, start_date, striker, batting_team, bowling_team, innings) %>%
  filter(striker == players) %>%
  # need to refine this so that 0-8 are a count (or divisable?) 
  summarise(across(all_of(selected_batting_columns),sum))
   
write.csv(batting_stats, "C:/Users/Tom/Documents/Data/Cricket/Output/batting_stats.csv", row.names = TRUE )


selected_bowling_columns <- c("runs_off_bat", "balls_faced", "extras", "wides", "noballs", "bowl_wickets", "team_wickets","0","1","2","3","4","5","6","7","8")

#create a wicket count (plus a non bowler attributed wicket count?)
bowling_stats <- selected_ball_by_ball %>%
  group_by(match_id, start_date, bowler, batting_team, bowling_team, innings) %>%
  filter(bowler == players) %>%
  # need to refine this so that 0-8 are a count (or divisable?) 
  summarise(across(all_of(selected_bowling_columns),sum))

write.csv(bowling_stats, "C:/Users/Tom/Documents/Data/Cricket/Output/bowling_stats.csv", row.names = TRUE )
