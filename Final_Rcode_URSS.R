# Imports+Libraries
install.packages("hoopR")
install.packages("tidyverse")
install.packages("reshape2")
install.packages("ggplot2")
install.packages("viridis")
library("hoopR")
library(dplyr)
library(reshape2)
library(tidyverse)
library(ggplot2)
library(viridis)
#-----------------------------------------
# Functions
#-----------------------------------------
data_loading <- function(year) {
  # Function to load our dataset, we only consider nba_pbp. 
  library("hoopR")
  nba_player_box <- load_nba_player_box(year:year+1)
  nba_pbp <- load_nba_pbp(seasons=year:year+1)
}

data_converter_csv <- function(file){
  # Function to convert the file to csv format
  write.csv(nba_player_box, "file", row.names = FALSE)
}

df_info <- function(df) {
  # Function to return general information about the dataframe
  df_type<-class(file)
  df_dim<-dim(df)
  df_cols<-colnames(df)
  sprintf("This Dataframe is of type:  %s",df_type)
  sprintf("This Dataframe has dimensions: %s",df_dim)
  sprintf("This Dataframe has column names: %s",df_cols)
}

df_edits_cols <- function(df, columns_to_remove) {
  # Function to remove all columns which are passed as a parameter to the function. 
  # Check if columns_to_remove is a character vector
  if (!is.character(columns_to_remove)) {
    stop("columns_to_remove must be a character vector of column names.")
  }
  
  # Remove specified columns
  df <- df %>% select(-all_of(columns_to_remove))
  
  return(df)
}

remove_rows_with_keywords <- function(df, keywords) {
  # Function to remove all rows where a certain keyword is present in the type_text column 
  # Convert keywords to a single regex pattern
  keywords_pattern <- str_c(keywords, collapse = "|")
  
  # Filter the dataframe
  df_filtered <- df %>%
    filter(!str_detect(type_text, keywords_pattern))
  
  # Return the filtered dataframe
  return(df_filtered)
}

time_converter <- function(df){
  # function to convert the time column of the dataframe for each game to seconds. 
  df<-df %>% mutate(time_s=
                      ifelse(period<5,
                             (period-1)*720 + 720-start_quarter_seconds_remaining,
                             2880+(period-5)*300+300-start_quarter_seconds_remaining))
  return(df)
}

time_interval_add<-function(df,time_col){
  # Function which adds the column of time_intervals.
  df <- df %>%
    mutate(
      time_int = case_when(
        row_number() == 1 ~ !!sym(time_col),  # For the first row, just use the existing value
        (!!sym(time_col) - lag(!!sym(time_col))) >= 0 ~ !!sym(time_col)-lag(!!sym(time_col)),
        (!!sym(time_col) - lag(!!sym(time_col))) < 0 ~ !!sym(time_col)
      )
    )
}

add_col_of_ones<-function(df){
  # Function to add a column of ones, later will be edited to add which game each event belonged to
  df <- df %>%
    mutate(seq_no = 1)
}

sequence_classifier <- function(df, time_col) {
  # Function to classify which game each event was part of
  # Convert column name to symbol for evaluation
  time_col <- sym(time_col)
  
  # Initialize seq_no column
  df <- df %>%
    mutate(seq_no = 1)
  
  # Calculate seq_no by iterating through the rows
  df <- df %>%
    mutate(seq_no = {
      # Create a vector to hold the sequence numbers
      seq_vec <- rep(1, n())
      
      # Loop through the rows starting from the second row
      for (i in 2:n()) {
        if ((df[[as.character(time_col)]][i] - df[[as.character(time_col)]][i - 1]) >= 0) {
          seq_vec[i] <- seq_vec[i - 1]
        } else {
          seq_vec[i] <- seq_vec[i - 1] + 1
        }
      }
      
      # Return the vector as the new column
      seq_vec
    })
  
  return(df)
}

shot_classifier <- function(df){
  # Function which classifies each shot attempt as either 2 point or 3 point attempt. 
  df <- df %>%
    mutate(shot_attempt_val = case_when(
      # Check if its a shooting Play. If not set NA
      str_detect(shooting_play,"FALSE")~NA,
      
      # If free throw set equal to 1. 
      str_detect(type_text, "Free Throw") ~ 1,
      
      # Condition 1: Check if 'two point' is in type_text
      str_detect(text, "two point") ~ 2,
      
      # Condition 2: Check if there is an integer less than 22 in text
      str_detect(text, "\\b([0-9]|1[0-9]|2[0-1])\\b") ~ 2,
      
      # Condition 3: Check if (x_coordinate-47)^2 + y_coordinate^2 <= 22 or (x_coordinate+47)^2 + y_coordinate^2 <= 22
      (coordinate_x > 0 & ((coordinate_x - 47)^2 + coordinate_y^2 <= 22^2)) |
        (coordinate_y < 0 & ((coordinate_x + 47)^2 + coordinate_y^2) <= 22^2) ~ 2,
      # Condition 4: Check if Tip Shot in type_text
      str_detect(type_text,"Tip Shot")~2,
      
      # Condition 5: Check if Tip Dunk Shot in type_text
      str_detect(type_text,"Dunk")~2,
      
      # Default case
      TRUE ~ 3
    ))
}

remove_event_no_loc <- function(df, column_name, threshold) {
  # Removes all events with no location
  # Ensure the column name is in the correct format
  column_name <- enquo(column_name)
  
  # Filter the dataframe
  df_filtered <- df %>%
    filter(abs(!!column_name) <= threshold)
  
  # Return the filtered dataframe
  return(df_filtered)
}

mark_classifier <- function(df){
  # This function is specific to the way I have chosen to classify the possible marks. 
  # My classification can be seen at URSS: A Bayesian approach to modelling NBA data
  # Fouls
  # Offensive fouls
  offensive_fouls <- c("Offensive Goaltending Turnover", "Double Dribble Turnover", "Palming Turnover",
                       "Traveling", "Disc Dribble Turnover", "Shot Clock Turnover", 
                       "3-Second Turnover", "5-Second Turnover", "8-Second Turnover", 
                       "Illegal Assist Turnover", "Lane Violation Turnover", 
                       "5-Second Back to the Basket Turnover","Step Turnover")
  
  keywords_off_pattern <- str_c(offensive_fouls, collapse = "|")
  # Defensive fouls
  defensive_fouls <- c("Loose Ball Foul", "Personal Take Foul", "Clear Path Foul", 
                       "Away from Play Foul", "Defensive 3-Seconds Technical", 
                       "Flagrant Foul Type 1", "Transition Take Foul", "Flagrant Foul Type 2")
  keywords_def_pattern<-str_c(defensive_fouls,collapse="|")
  
  # Miscellaneous fouls
  miscellaneous_fouls <- c("Excess Timeout Technical", 
                           "Excess Timeout Turnover", "Delay Technical", 
                           "Jumpball Violation Turnover", "Punched Ball Turnover", 
                           "Basket from Below Turnover","Personal Foul")
  keywords_misc_pattern<-str_c(miscellaneous_fouls,collapse="|")
  
  
  df <- df %>%
    mutate(mark_val = case_when(
      # Check if Free Throw, if successful and home assign 1,etc
      (str_detect(type_text,"Free Throw")& scoring_play)&home_team_id==team_id~1,
      
      (str_detect(type_text,"Free Throw")& scoring_play)&home_team_id!=team_id~15,
      
      (str_detect(type_text,"Free Throw")& !scoring_play)&home_team_id==team_id~2,
      
      (str_detect(type_text,"Free Throw")& !scoring_play)&home_team_id!=team_id~16,
      
      (!str_detect(type_text,"Layup")&shot_attempt_val==2&scoring_play)&home_team_id==team_id~3,
      
      (!str_detect(type_text,"Layup")&shot_attempt_val==2&scoring_play)&home_team_id!=team_id~17,
      
      (!str_detect(type_text,"Layup")&shot_attempt_val==2&!scoring_play)&home_team_id==team_id~4,
      
      (!str_detect(type_text,"Layup")&shot_attempt_val==2&!scoring_play)&home_team_id!=team_id~18,
      
      (str_detect(type_text,"Layup")&scoring_play)&home_team_id==team_id~5,
      
      (str_detect(type_text,"Layup")&scoring_play)&home_team_id!=team_id~19,
      
      (str_detect(type_text,"Layup")&!scoring_play)&home_team_id==team_id~6,
      
      (str_detect(type_text,"Layup")&!scoring_play)&home_team_id!=team_id~20,
      
      (shot_attempt_val==3&scoring_play)&home_team_id==team_id~7,
      
      (shot_attempt_val==3&scoring_play)&home_team_id!=team_id~21,
      
      (shot_attempt_val==3&!scoring_play)&home_team_id==team_id~8,
      
      (shot_attempt_val==3&!scoring_play)&home_team_id!=team_id~22,
      
      str_detect(type_text,"Offensive Rebound")&home_team_id==team_id~9,
      
      str_detect(type_text,"Offensive Rebound")&home_team_id!=team_id~23,
      
      str_detect(type_text,"Defensive Rebound")&home_team_id==team_id~10,
      
      str_detect(type_text,"Defensive Rebound")&home_team_id!=team_id~24,
      
      # Fouls
      str_detect(type_text,keywords_off_pattern)&home_team_id==team_id~11,
      
      str_detect(type_text,keywords_off_pattern)&home_team_id!=team_id~25,
        
      str_detect(type_text,keywords_def_pattern)&home_team_id==team_id~12,
        
      str_detect(type_text,keywords_def_pattern)&home_team_id!=team_id~26,
      
      (str_detect(type_text,"Bad Pass")|str_detect(type_text,"Lost Ball"))&home_team_id==team_id~13,
      
      (str_detect(type_text,"Bad Pass")|str_detect(type_text,"Lost Ball"))&home_team_id!=team_id~27,
      # If there is no mark assigned thus far, set as miscelaneous foul
      (str_detect(type_text,keywords_misc_pattern)|str_detect(type_text," "))&home_team_id==team_id~14,
      # If there is no mark assigned thus far, set as miscelaneous foul
      str_detect(type_text,keywords_misc_pattern)|str_detect(type_text," ")&home_team_id!=team_id~28,
      
      TRUE ~ 0
    ))
}

adjust_zeros_in_df <- function(df, column_name) {
  # Removes all zero time intervals by adding 0.1 to such events. 
  # Ensure the column exists in the dataframe
  if (!column_name %in% colnames(df)) {
    stop(paste("Column", column_name, "does not exist in the dataframe"))
  }
  
  # Ensure the column is numeric
  if (!is.numeric(df[[column_name]])) {
    stop("The column must be numeric")
  }
  
  # Get the column to be adjusted
  column <- df[[column_name]]
  
  # Initialize a counter for consecutive zeros
  zero_count <- 0
  
  # Iterate through the column and adjust zeros
  for (i in 1:length(column)) {
    if (column[i] == 0) {
      zero_count <- zero_count + 1
      column[i] <- column[i] + (zero_count * 0.1)
    } else {
      zero_count <- 0
    }
  }
  df[[column_name]] <- column
  
  return(df)
}

remove_na__zerotimestamps_large_timeint_rows <- function(df) {
  # Function to remove rows wiht NA values in type_text column(7 rows), zero timestamps(5 events) 
  # and rows corresponding to timeintervals greater than 100 seconds. 
  
  # Remove rows with NA values in the specified column
  df <- df[!is.na(df[['team_id']]), ]
  df <- df %>% filter(!!sym('time_s') != 0)
  df <- df %>% filter(a <= 100)
  return(df)
}

  
# ----------------------
# END FUNCTIONS
# ----------------------
# Begin process
#nba_pbp <- data_loading(2022)
#nba_pbp_colremoved <- df_edits(nba_pbp,c("period_number","period_display_value","clock_display_value","athlete_id_1",
#                                 "athlete_id_2","athlete_id_3","wallclock","coordinate_x_raw",
#                                 "coordinate_y_raw","home_team_mascot","home_team_abbrev","home_team_name_alt",
#                                 "away_team_mascot","away_team_abbrev","away_team_name_alt",
#                                 "game_spread","home_favorite","game_spread_available","home_team_spread",
#                                 "qtr","time","clock_minutes","clock_seconds","home_timeout_called",
#                                 "away_timeout_called","half","game_half","lead_qtr","lead_half",
#                                 "start_half_seconds_remaining","start_game_seconds_remaining",
#                                 "end_quarter_seconds_remaining","end_half_seconds_remaining",
#                                 "end_game_seconds_remaining","lag_qtr","lag_half","type_abbreviation"))
# nba_pbp_time<-time_converter(nba_pbp_colremoved)
# nba_pbp_time<-shot_classifier(nba_pbp_time)
# nba_pbp_time_class_marks<-mark_classifier(nba_pbp_time)
# nba_pbp_time_class_marks_noloc<-remove_event_no_loc(nba_pbp_time_class_marks,coordinate_x,100)
# nba_pbp_remove_pairs<-remove_rows_with_keywords(nba_pbp_time_class_marks_noloc,c("Shooting Foul","Offensive Foul","Offensive Charge"))
# nba_time_int_add<-time_interval_add(nba_pbp_remove_pairs,'time_s')
# nba_zero_timeint_removed<-adjust_zeros_df(nba_pbp_sequences_added,'time_s')
# nba_pbp_seq_add<-add_col_of_ones(nba_time_int_add)
# nba_pbp_sequences_added<-sequence_classifier(nba_pbp_seq_add,'time_s')
#nba_zero_timestamps_andNA_removed<-remove_na_and_zerotimestamps_rows(nba_pbp_sequences_added)
#nba_time_int_updated_no_zeros<-adjust_zeros_in_df(nba_zero_timestamps_andNA_removed,'time_int')
#nba_finalv2<-remove_na__zerotimestamps_large_timeint_rows(nba_reclassify)


# ------------------
# Creating transition matrix
# ------------------
mark_vals <- nba_finalv2$mark_val

# Initialize the 28x28 matrix for counts
n <- 28
transition_counts <- matrix(0, nrow = n, ncol = n)

# Fill the transition matrix with counts
for (i in 1:(length(mark_vals) - 1)) {
  current_state <- mark_vals[i]
  next_state <- mark_vals[i + 1]
  if (current_state <= n && next_state <= n) {
    transition_counts[current_state, next_state] <- transition_counts[current_state, next_state] + 1
  }
}

# Convert counts to probabilities
row_sums <- rowSums(transition_counts)
transition_probabilities <- sweep(transition_counts, 1, row_sums, FUN = "/")
transition_probabilities[is.nan(transition_probabilities)] <- 0

# labels 
labels <- c(
  "H_FreeThrow_S", "H_FreeThrow_U", "H_2pointjump_S", "H_2pointjump_U",
  "H_2pointLay_S", "H_2pointLay_U", "H_3point_S", "H_3point_U",
  "H_OffensiveRebound", "H_DefensiveRebound", "H_OffensiveFoul", "H_DefensiveFoul",
  "H_BadPass_Lostball", "H_Misc_Foul", "A_FreeThrow_S", "A_FreeThrow_U",
  "A_2pointjump_S", "A_2pointjump_U", "A_2pointLay_S", "A_2pointLay_U",
  "A_3point_S", "A_3point_U", "A_OffensiveRebound", "A_DefensiveRebound",
  "A_OffensiveFoul", "A_DefensiveFoul", "A_BadPass_Lostball", "A_Misc_Foul"
)

# Set the row and column names of the matrix
rownames(transition_probabilities) <- labels
colnames(transition_probabilities) <- labels

# Convert the matrix to a data frame suitable for ggplot2
transition_df <- melt(transition_probabilities)
colnames(transition_df) <- c("From", "To", "Probability")

# Plot the heatmap using ggplot2
ggplot(transition_df, aes(x = To, y = From, fill = Probability)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Transition Matrix Heatmap",
       x = "Next State",
       y = "Current State",
       fill = "Probability") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))









# Writing the files to CSV
write.csv(nba_finalv2, "nba_finalv3.csv", row.names = FALSE)
rm(nba_player_box)

# END SCRIPT
# ---------------------------------------------------------------------------------
# Documenting time frames and progression Using Narnayan et al section 5.1
# STEP 1 OF CLEANING PROCESS
# nba_pbp: Original dataframe with no edits
# ------------------------------
# nba_pbp_colremoved:
# First iteration of edits. Unneccessary columns removed. Removed 37 Columns,
# insert the 37 columns removed here:
# "period_number","period_display_value","clock_display_value","athlete_id_1",
# "athlete_id_2","athlete_id_3","wallclock","coordinate_x_raw",
# "coordinate_y_raw","home_team_mascot","home_team_abbrev","home_team_name_alt"
# "away_team_mascot","away_team_abbrev","away_team_name_alt",
# "game_spread","home_favorite","game_spread_available","home_team_spread",
# "qtr","time","clock_minutes","clock_seconds","home_timeout_called",
# "away_timeout_called","half","game_half","lead_qtr","lead_half",
# "start_half_seconds_remaining","start_game_seconds_remaining",
# "end_quarter_seconds_remaining","end_half_seconds_remaining",
# "end_game_seconds_remaining","lag_qtr","lag_half","type_abbreviation".
# -----------------------------
# nba_pbp_time: Added three columns. 
# 1. The second at which any event occurred. 
# 2. If a shot was attempted, what shot the player was attempting, either 1(free throw),2 or 3
# ------------------------------
# nba_pbp_time_class_marks:
# Added Mark classification column
# ------------------------------
# nba_pbp_time_class_marks_noloc:
# Removed all events with no location, eg substituion, initiated review,jumppball etc
# ------------------------------
# nba_pbp_remove_pairs:
# Removed Remove (outcome = unsuccessful) version of events that come in pairs, 
# Shooting Foul-Free Throw, Offensive Foul-Offensive Foul Turnover etc.  
# ------------------------------
# nba_pbp_timeinterval add:
# added a timeinterval column
# ------------------------------
# nba_pbp_seq_add:
# added a column of 1s to then edit for the sequences.
# # ------------------------------
# nba_sequences_added:
# added the sequence number column. 
# ------------------------------
# nba_zero_timestamps_andNA_removed:
# remove zero timestamps and NA rows
# ------------------------------
# nba_time_int_updated_no_zeros:
# Removed all zero timeintervals
# nba_finalv2:
# final version used for running code(final tweaks done in python)
# ------------------------------
# STEP 2 OF CLEANING PROCESS.
# Remove all rows where there is a NaN value. 
# Check if there are impossible time_stamps. Eg two consecutive free throws with different timestamps. 
# Note that as the basketball stops each tme the ball goes out of play the terminal event cleaning process as
# in narayanan does not hold
# STEP 3 OF CLEANING PROCESS
# Check for Simultaneous events. 
# For all simultaneous events add a time of 0.1 to prevent any zero time_intervals. 
# 
# FOUL CLASSIFICATION 
# offensive_fouls <- c("Offensive Goaltending Turnover", "Double Dribble Turnover", "Palming Turnover",
#                      "Traveling", "Disc Dribble Turnover", "Shot Clock Turnover", 
#                      "3-Second Turnover", "5-Second Turnover", "8-Second Turnover", 
#                      "Illegal Assist Turnover", "Lane Violation Turnover", 
#                      "5-Second Back to the Basket Turnover")

# Defensive fouls
# defensive_fouls <- c("Loose Ball Foul", "Personal Take Foul", "Clear Path Foul", 
#                      "Away from Play Foul", "Defensive 3-Seconds Technical", 
#                      "Flagrant Foul Type 1", "Transition Take Foul", "Flagrant Foul Type 2")

# Miscellaneous fouls
# miscellaneous_fouls <- c("Excess Timeout Technical", 
#                         "Excess Timeout Turnover", "Delay Technical", 
#                         "Jumpball Violation Turnover", "Punched Ball Turnover", 
#                         "Basket from Below Turnover")



