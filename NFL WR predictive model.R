####  Set the working directory ####

setwd("C:/Users/fuadb/OneDrive/Desktop/R Project")

.libPaths( c( .libPaths(), "C:/Rpackages") )

#### Read in data ####

install.packages("nflfastR")

library("nflfastR")
library("tidyverse")
library("janitor")

#### 01_data_prep.R ####

# Load full PBP for seasons 
seasons <- 2019:2024
pbp <- load_pbp(seasons)


# pass plays with a receiver
pbp_rec <- pbp %>% filter(pass == 1, !is.na(receiver_player_id))

# Build WR season stats
wr_stats <- pbp_rec %>%
  group_by(season, receiver_player_id, receiver_player_name, posteam) %>%
  summarise(
    targets    = n(),
    receptions = sum(complete_pass, na.rm = TRUE),
    rec_yards  = sum(receiving_yards, na.rm = TRUE),
    rec_tds    = sum(pass_touchdown, na.rm = TRUE), 
    .groups    = "drop"
  ) %>%
  clean_names()

# efficiency metrics for each WR:
wr_stats <- wr_stats %>%
  mutate(
    yards_per_target = rec_yards/targets,
    catch_rate = receptions/targets,
    td_rate = rec_tds/targets
  )

# team-level passing stats per season
team_pass <- pbp %>%
  filter(pass==1) %>% # keep only rows where a pass play happened
  group_by(season, posteam) %>%
  summarise(
    team_pass_attempts=n(),
    team_pass_yards=sum(passing_yards, na.rm=TRUE),
    .groups='drop'
  ) %>%
  mutate(team_yards_per_attempt = team_pass_yards/team_pass_attempts)  # efficiency metric

# team passing stats onto WR stats
wr_stats <- wr_stats %>% left_join(team_pass, by=c("season","posteam")) #matches each WR row with the team_pass row for the same season and team.

#next-season yards
# For each player, look at the NEXT season's yards
wr_lag <- wr_stats %>%
  arrange(receiver_player_id, season) %>%
  group_by(receiver_player_id) %>%
  mutate(
    next_season = lead(season),
    next_rec_yards = lead(rec_yards)
  ) %>% ungroup()

#Keep only rows with valid next season
# and filter to WRs with enough volume
wr_model_data <- wr_lag %>%
  filter(!is.na(next_rec_yards), targets >= 30) # only WRs with at least 30 targets (meaningful usage)

# save to file
write_csv(wr_model_data, "data/processed/wr_model_data.csv")

# Viewing the WRs stats
view(wr_stats)

summary(wr_stats)
summary(wr_model_data)

#### EDA (Exploratory Data Analysis) ####

install.packages("gg2plot")
library(ggplot2)

# Visualization on Distribution of all WRs season combined 

ggplot(wr_model_data, aes(targets)) +
  geom_histogram(bins = 35, fill = "steelblue", colour = "black") +
    labs(title = "Distribution of WRs Target", x = "Targets", y = "Counts" )

# Visualization on Targets vs Next season Yards 

ggplot(wr_model_data, aes(targets, next_rec_yards)) +
  geom_point(alpha = 0.6) +
  labs(title = "Targets VS Next season yards", 
       x = "Targets (Current Season)",
       y = "Next Year Receiving Yards")

# Visualization on Receiving Yards vs Next Season Receiving Yards

ggplot(wr_model_data, aes(rec_yards, next_rec_yards)) +
  geom_point(alpha = 0.6, colour = "darkgreen") +
  labs(title = "Receiving Yards vs Next Season Receiving Yards", 
       x = "Receiving Yards (Current Season)", 
       y = "Next season Receiving Yards")
    
  

# Visualization on WR Receiving Yards by Team 

ggplot(wr_model_data, aes(posteam, rec_yards)) +
  geom_boxplot() +
  labs(title = "Receiving yards per team ", 
       x = "NFL Teams",
       y = "Receiving Yards") +
         theme(axis.text.x = element_text(angle = 90))

# Visualization on correlation heat map among features

install.packages("GGally")
library(GGally)

wr_model_data %>%
  select(
    Targets = targets,
    Rec = receptions,
    Yds = rec_yards,
    TDs = rec_tds,
    YPT = yards_per_target,
    TPA = team_pass_attempts,
    NextYds = next_rec_yards
  ) %>%
  ggcorr(label = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 5))

#visualization on top 15 WRs by Targets 

wr_stats %>%
arrange(desc(targets)) %>% 
filter(season == 2024) %>%
slice(1:15) %>%
ggplot(aes(x = reorder(receiver_player_name, targets), y = targets)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top 15 WRs by Targets (2024)",
       x = "Receiver",
       y = "Targets"
  )

# visualization on top 15 WRs by Receiving Yards

wr_stats %>%
  arrange(desc(rec_yards)) %>%
  filter(season == 2024) %>%
  slice(1:15) %>%
    ggplot(aes(x = reorder(receiver_player_name, rec_yards), y = rec_yards)) +
    geom_col(fill = "brown") +
    coord_flip() +
    labs(title = "Top 15 WRs by Receiving Yards (2024)",
         x = "receiving yards",
         y = "receiver" 
         )

# visualization comparison between WRs Target vs Receiving Yards 2024

ggplot(wr_stats %>% filter(season == 2024), aes(
                           x = targets,
                           y = rec_yards)) +
  geom_point() +
  geom_smooth(method = lm) +
  labs(title = "Targets Vs Receving Yards (2024)", 
       x = "Targets", y = "Receiving Yards")

#### Correlation Analysis ####

cor(x = wr_model_data$targets, y = wr_model_data$next_rec_yards, use = "complete.obs", method = "pearson")
# The cor score = 0.643

cor(x = wr_model_data$rec_yards, wr_model_data$next_rec_yards, use = "complete.obs", method = "pearson")
# The cor score = 0.688

wr_stats %>%
filter(season == 2024) %>%
  summarise(
 cor(x = targets, rec_yards, use = "complete.obs", method = "pearson"))

# The cor score = 0.970

####  Linear Regression Model ####

# create train model / test split 

train_data <-  wr_model_data %>%
  filter(next_season <= 2023)

test_data <- wr_model_data %>%
  filter(next_season == 2024 )

cat("Training rows:", nrow(train_data), "\n") #772 rows
cat("test rows:", nrow(test_data), "\n") #202 rows

#### Specify model formula ####

# next_rec_yards is the target #

# Features added to the model formula 

model_formula <- next_rec_yards ~
  targets +
  rec_yards +
  rec_tds +
  yards_per_target +
  team_pass_attempts 
  
# fit linear regression model on the training data
lm_fit <- lm(model_formula, data = train_data)

# summary of linear regression model 

summary(lm_fit)

# evaluate model on unseen data 

test_results <- test_data %>%
  mutate(pred_next_yards = predict(lm_fit, newdata = test_data),
    error = next_rec_yards - pred_next_yards,
    abs_error = abs(error)
    )

# compute evaluation metrics: MAE, RMSE, R^2

mae <- mean(test_results$abs_error, na.rm = TRUE)
rmse <- sqrt(mean(test_results$error^2, na.rm = TRUE))
r2 <- cor(test_results$next_rec_yards,
          test_results$pred_next_yards,
          use = "complete.obs")^2

cat("\n--- Test performance on 2023->2024 season ---\n")
cat("MAE  :", round(mae, 1), "yards\n")
cat("RMSE :", round(rmse, 1), "yards\n")
cat("R^2  :", round(r2, 3), "\n")

#### Plot Actual vs Predicted Actual next season yards ####

ggplot(test_results, aes(x = next_rec_yards, y = pred_next_yards)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(title = "Actual vs Predicted Next - Season Receiving Yards (Test season 2023 ->2024) ",
       x = "Actual Next - Season Yards", 
       y = "Predicted Next - Season Yards" )


#### Prediction WRs receiving Yards for 2025 ####
# using 2024 season statistics 

wr_model_data <- read_csv("data/processed/wr_model_data.csv",
                          show_col_types = FALSE)

model_formula <- next_rec_yards ~
  targets +
  rec_yards +
  rec_tds +
  yards_per_target +
  team_pass_attempts

final_lm_fit <- lm(model_formula, data = wr_model_data)

summary(final_lm_fit)

# Prepare 2024 data for prediction 

wr_2024 <- wr_stats %>%
  filter(season == 2025 | season == 2024) %>%
  mutate(next_rec_yards = NULL)

wr_2024 <- wr_stats %>%
  filter(season == 2024, targets >= 30) %>%   # keep only meaningful 2024 WRs
  select(
    season,
    receiver_player_id,
    receiver_player_name,
    posteam,
    targets,
    rec_yards,
    rec_tds,
    yards_per_target,
    team_pass_attempts
  )

wr_2024 <- wr_2024 %>%
  mutate(pred_2025_rec_yards = predict(final_lm_fit, newdata = wr_2024))

# Top 30 WRs in the 2024 season
wr_2024_top <- wr_2024 %>%
  arrange(desc(pred_2025_rec_yards)) %>%
  slice(1:30)

view(wr_2024_top)

wr_2024_top %>%
  select(receiver_player_name, posteam, targets, rec_yards, pred_2025_rec_yards)

#Predict 2025 receiving yards

wr_2025_predictions <- wr_2024 %>%
  mutate(
    pred_2025_rec_yards = predict(final_lm_fit, newdata = wr_2024)
  )

# Rank WRs by predicted 2025 yards

wr_2025_ranked <- wr_2025_predictions %>%
  arrange(desc(pred_2025_rec_yards)) %>%
  select(
    receiver_player_name,
    posteam,
    targets,
    rec_yards,
    pred_2025_rec_yards
  )

head(wr_2025_ranked, 20)

#### 8. Save predictions to CSV ####
write_csv(wr_2025_ranked,
          "data/processed/wr_2025_predictions.csv")

# Visualization Predicted Top 15 WRs in 2025


ggplot(head(wr_2025_ranked, 20),
       aes(x = reorder(receiver_player_name, pred_2025_rec_yards),
           y = pred_2025_rec_yards)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 15 Predicted WRs by Receiving Yards (2025)",
    x = "Receiver",
    y = "Predicted Receiving Yards"
  )


