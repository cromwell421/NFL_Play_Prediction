library(tidyverse)
library(nflscrapR)
library(ggplot2)
library(stringr)
library(gridExtra)


########################################
#Create baseline metric


#team passing rates by team
league_pass_rate <- play_predictor %>% 
  filter(play_type == "pass" | play_type == "run") %>%
  filter(season >= 2015) %>%
  group_by(down) %>% 
  summarise(league_pass_rate = mean(qb_dropback, na.rm = TRUE))


#team passing rates by team
team_passing_rates <- play_predictor %>% 
  mutate(pass_pred = "pass") %>% 
  mutate(accuracy_score = ifelse(pass_pred==play_type,1,0)) %>% 
  group_by(posteam,season) %>% 
  summarise(baseline = mean(accuracy_score, na.rm = TRUE)) %>% 
unite(team, posteam, season, sep ="_")


#team passing rates - by down for entire leaguye average
baseline_rates <- play_predictor %>% 
  mutate(pass_pred = "pass") %>% 
  mutate(accuracy_score = ifelse(pass_pred==play_type,1,0)) %>% 
  summarise(mean(accuracy_score, na.rm = TRUE)) 


##########################################


###Select data for the model
play_predictor <- pbp_all %>% 
  mutate(prev_play = lag(play_type)) %>% 
  mutate(under_4_min_half = ifelse(half_seconds_remaining <= 240,1,0)) %>% 
  select(play_id, season, week, posteam, yardline_100, shotgun, wp, qtr, under_4_min_half, half_seconds_remaining, down, ydstogo, score_differential,prev_play, play_type,qb_dropback) %>% 
  filter(play_type == "pass" | play_type == "run") %>%
  filter(season >=2015) 



#factor categorical varibales
play_predictor$prev_play <- as.factor(play_predictor$prev_play)
play_predictor$play_type <- as.factor(play_predictor$play_type)
play_predictor$shotgun <- as.factor(play_predictor$shotgun)
play_predictor$under_4_min_half <- as.factor(play_predictor$under_4_min_half)

#############################################################

data_split <- floor(0.75 * nrow(play_predictor))

set.seed(421)
train_ind <- sample(seq_len(nrow(play_predictor)), size = data_split)

play_predictor_train <- play_predictor[train_ind, ]
play_predictor_test <- play_predictor[-train_ind, ]

#############################################################

#check which number 0 or 1 is pass and run
contrasts(play_predictor_train$prev_play)

#############################################################


#logitMod_ATL <- glm(play_type ~  yardline_100 + qtr + half_seconds_remaining + down + ydstogo + score_differential + prev_play,family = binomial, data=play_predictor)
logitMod <- glm(play_type ~ wp+ down +under_4_min_half  + ydstogo + score_differential,family = binomial, data=play_predictor_train)


summary(logitMod)
anova(logitMod, test="Chisq")

#############################################################


#training set
league_prob <- predict(logitMod, play_predictor_train, type="response")
league_pred = rep("pass", dim(play_predictor_train)[1])
league_pred[league_prob > .5] = "run"
table(league_pred, play_predictor_train$play_type)

mean(league_pred == play_predictor_train$play_type)


#test the model on test set
league_prob_test <- predict(logitMod, play_predictor_test, type="response")
league_pred_test = rep("pass", dim(play_predictor_test)[1])
league_pred_test[league_prob_test > .5] = "run"
table(league_pred_test, play_predictor_test$play_type)

#create baseline
test_baseline = mean(league_pred_test == play_predictor_test$play_type)

#########################################


#Predict for each team train
predictor_by_team <- play_predictor_train
predictor_by_team$pred <- predict(logitMod, play_predictor_train, type="response")

predictor_by_team <-predictor_by_team %>% 
  mutate(pred_name = ifelse(pred > .5, "run","pass")) %>% 
  mutate(score = ifelse(pred_name == play_type, 1, 0)) %>% 
  group_by(posteam,season) %>% 
  summarise(predictability =mean(score, na.rm = TRUE)) %>% 
  unite(team, posteam, season, sep ="_")


#Predict for each team test
predictor_by_team_test <- play_predictor_test
predictor_by_team_test$pred <- predict(logitMod, play_predictor_test, type="response")

predictor_by_team_test <-predictor_by_team_test %>% 
  mutate(pred_name = ifelse(pred > .5, "run","pass")) %>% 
  mutate(score = ifelse(pred_name == play_type, 1, 0)) %>% 
  group_by(posteam,season) %>% 
  summarise(predictability = mean(score, na.rm = TRUE)) %>% 
  unite(team, posteam, season, sep ="_")

#############################################################
#add baseline to table
predictor_by_team_test$baseline <- mean(league_pred_test == play_predictor_test$play_type)
predictor_by_team_test$accuracy <- predictor_by_team_test$predictability - predictor_by_team_test$baseline

#############################################################

#Add Winning % by predictability chart
win_percentage <- games_all %>%
  filter(season >= 2015) %>% 
  mutate(winning_team = ifelse(home_score > away_score, home_team,away_team)) %>%
  mutate(losing_team = ifelse(home_score <= away_score, home_team,away_team)) %>%
  
  group_by(losing_team,season) %>%
  summarise(losses = n(), wins = 16 - losses, winning_percentage = 1-(losses/16)) %>%
#  rename(posteam = winning_team) %>% 
  unite(team, losing_team, season, sep ="_")


pred_wins <- merge(x = predictor_by_team_test, y = win_percentage, by = "team", all = TRUE)


#####################################


summary(lm(winning_percentage ~ predictability, data = pred_wins))

ggplot(pred_wins, aes(y = winning_percentage, x = predictability), group = team) + 
  geom_point() + 
  #  geom_text_repel(aes(label = ifelse(total_WPA > 2.1 | total_WPA < 0.2 | total_EPA > 60 | total_EPA < 10,as.character(team_year),""))) +
  xlab("Play Call Predictability") + 
  ylab("Winning Percentage") + 
  theme_minimal() +
  geom_smooth(method='lm',col = "red") +
  labs(title = "Winning % by Predictability") +
  labs(title = "Team Winning % by Predictability (2015 - 2019)",
       subtitle = "Using Team-Agnostic Models",
       caption = "Source: NFLScrapR") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.title = element_text(size = 16), axis.text = element_text(size = 16)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 16))


#############################################################


#Add off EPA by predictability chart
off_epa <- pbp_all %>%
  filter(!is.na(posteam)) %>% 
  filter(play_type == "run" | play_type == "pass") %>% 
  filter(season >= 2015) %>% 
  group_by(posteam,season) %>%
  summarise(off_epa = mean(epa,na.rm=TRUE)) %>% 
  unite(team, posteam, season, sep ="_")

  
pred_wins2 <- merge(x = predictor_by_team_test, y = off_epa, by = "team", all = TRUE)

summary(lm(off_epa ~ predictability, data = pred_wins2))

ggplot(pred_wins2, aes(y = off_epa, x = predictability), group = posteam) + 
  geom_point() + 
  #  geom_text_repel(aes(label = ifelse(total_WPA > 2.1 | total_WPA < 0.2 | total_EPA > 60 | total_EPA < 10,as.character(team_year),""))) +
  xlab("Play Call Predictability") + 
  ylab("Off EPA per Play") + 
  theme_minimal() +
  geom_smooth(method='lm',col = "red") +
  labs(title = "Offensive EPA by Play Call Predictability (2015 - 2019)",
       subtitle = "Using Team-Agnostic Models",
       caption = "Source: NFLScrapR") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), axis.title = element_text(size = 16), axis.text = element_text(size = 16)) +
  theme(plot.subtitle = element_text(hjust = 0.5, size = 16))



#############################################################


#Train model for each team

loop_test <- pbp_all %>% 
  mutate(prev_play = lag(play_type)) %>% 
  mutate(under_4_min_half = ifelse(half_seconds_remaining <= 240,1,0)) %>% 
  select(play_id, season, week, posteam, yardline_100, shotgun, wp, qtr, half_seconds_remaining,under_4_min_half, down, ydstogo, score_differential,prev_play, play_type) %>% 
  filter(play_type == "pass" | play_type == "run") %>%
#  filter(shotgun == 0) %>% 
  filter(season >= 2015)

loop_test$prev_play <- as.factor(loop_test$prev_play)
loop_test$play_type <- as.factor(loop_test$play_type)
loop_test$shotgun <- as.factor(loop_test$shotgun)
loop_test$under_4_min_half <- as.factor(loop_test$under_4_min_half)


dl = list()
for(i in unique(loop_test$posteam)) {
  
  loop_test2 <- loop_test %>% 
    filter(posteam == i)

   data_split2 <- floor(0.75 * nrow(loop_test2))
  
  set.seed(421)
  train_ind2 <- sample(seq_len(nrow(loop_test2)), size = data_split2)
  play_predictor_train2 <- loop_test2[train_ind2, ]
  play_predictor_test2 <- loop_test2[-train_ind2, ]
  
  logitMod2 <- glm(play_type ~   under_4_min_half + wp+ down +shotgun+ ydstogo +score_differential,family = binomial, data=play_predictor_train2)
  print(i)
  print(summary(logitMod2))
  a <- predict(logitMod2, play_predictor_test2, type="response")
  
  df <- data.frame(i,play_predictor_test2$season,play_predictor_test2$week,play_predictor_test2$play_id,play_predictor_test2$play_type, a)
  dl[[i]] <- df 
  
}
df1 <- plyr::rbind.fill(dl)
df1 <- rename(df1, posteam = 1)
df1 <- rename(df1, season = 2)
df1 <- rename(df1, week = 3)
df1 <- rename(df1, play_id = 4)
df1 <- rename(df1, play_type = 5)
df1 <- rename(df1, pred = 6)

#########################################

#overall prediction for the leage as baseline using team based model
team_base <- df1 %>% 
  mutate(prediction = ifelse(pred < .5, "pass","run")) %>% 
  mutate(acc = ifelse(prediction == play_type, 1, 0)) %>% 
  summarise(score = mean(acc, na.rm = TRUE)) 


#########################################
#create model score for each team

df2 <- df1 %>% 
  mutate(prediction = ifelse(pred < .5, "pass","run")) %>% 
  mutate(acc = ifelse(prediction == play_type, 1, 0)) %>% 
  group_by(posteam,season) %>% 
  summarise(score = mean(acc, na.rm = TRUE)) %>% 
  unite(team, posteam, season, sep ="_")


#Add off EPA by predictability chart

pred_wins3 <- merge(x = df2, y = off_epa, by = "team", all = TRUE)
#pred_wins3 <- merge(x = pred_wins3, y = team_passing_rates, by = "team", all = TRUE)

pred_wins3 <- pred_wins3 %>% 
  mutate(baseline = team_base$score) %>% 
  mutate(accuracy = score-baseline)
#  mutate(accuracy2 = ifelse(accuracy <= 0,0,1)) %>% 
# summarise(mean(accuracy))


summary(lm(off_epa ~ score, data = pred_wins3))

ggplot(pred_wins3, aes(y = off_epa, x = score), group = posteam) + 
  geom_point() + 
  #  geom_text_repel(aes(label = ifelse(total_WPA > 2.1 | total_WPA < 0.2 | total_EPA > 60 | total_EPA < 10,as.character(team_year),""))) +
  xlab("Play Call Predictability") + 
  ylab("Off EPA per Play") + 
  theme_minimal() +
  geom_smooth(method='lm',col="red") +
  labs(title = "Offensive EPA by Play Call Predictability (2015 - 2019)",
       subtitle = "Using Team-Specific Models",
       caption = "Source: NFLScrapR")



######################################3


ggplot(league_pass_rate, aes(y = league_pass_rate, x = down, fill = down))+ 
  geom_bar(position="identity", stat="identity") +
  geom_text(aes(label = scales::percent(league_pass_rate),vjust = -0.15)) +
  scale_y_continuous(labels = scales::percent_format(league_pass_rate = 1)) +
  xlab("Down") + 
  theme_minimal() +
  ylab("Drop Back Rate") +
  theme(legend.position = "none") +
  labs(title = "League Average op Back Rate by Down (2015 - 2019)",
       caption = "Source: NFLScrapR")


#############################################333
#bar charts for 2019 predictability - league model

bar_plot_2019 <- pred_wins %>% 
  filter(str_detect(team, "2019")) %>% 
  mutate(team = str_remove_all(team, "_2019")) %>%
  arrange(desc(accuracy))


league1 <-ggplot(bar_plot_2019, aes(y = accuracy, x = reorder(team, accuracy),
                               fill=ifelse(accuracy>=0,"green","red"))) + 
  geom_bar(position="identity", stat="identity") +
  xlab("Team") + 
  ylab("Predictability +/- Baseline") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Team Predictability for 2019 Season",
       subtitle = "Using Team-Agonstic Models\n",
       caption = "\n\n\n")


#############################################333
#bar charts for top/bottom 10 predictability - league model

bar_plot_top <- pred_wins %>% 
  mutate(team = str_remove_all(team, "20")) %>%
  arrange(desc(accuracy)) %>% 
  top_n(n=15,wt=accuracy)

bar_plot_bot <- pred_wins %>% 
  mutate(team = str_remove_all(team, "20")) %>%
  arrange(accuracy) %>% 
  top_n(n=15,wt=-accuracy)

bar_plot_top_bot <- bind_rows(bar_plot_top,bar_plot_bot)


league2 <- ggplot(bar_plot_top_bot, aes(y = accuracy, x = reorder(team, accuracy),
                                  fill=ifelse(accuracy>=0,"green","red"))) + 
  geom_bar(position="identity", stat="identity") +
  coord_flip() +
  xlab("Team_Year") + 
  ylab("Predictability +/- Baseline") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Team Predictability 2015 - 2019",
       subtitle = "Using Team-Agonstic Model\nTop and Bottom 15 Teams",
       caption = "Right = More Predictable\nLeft = Less Predicatble\nSource: NFLScrapR")

grid.arrange(league1, league2, ncol=2)


#############################################333
#bar charts for 2019 predictability - team model

bar_plot_2019_team <- pred_wins3 %>% 
  filter(str_detect(team, "2019")) %>% 
  mutate(team = str_remove_all(team, "_2019")) %>%
  arrange(desc(accuracy))


team1 <- ggplot(bar_plot_2019_team, aes(y = accuracy, x = reorder(team, accuracy),
                          fill=ifelse(accuracy>=0,"green","red"))) + 
  geom_bar(position="identity", stat="identity") +
  xlab("Team") + 
  ylab("Predictability +/- Baseline") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Team Predictability for 2019 Season",
       subtitle = "Using Team-Specific Models\n",
       caption = "\n\n\n")


#############################################333
#bar charts for top/bottom 10 predictability - league model

bar_plot_top_team <- pred_wins3 %>% 
  mutate(team = str_remove_all(team, "20")) %>%
  arrange(desc(accuracy)) %>% 
  top_n(n=15,wt=accuracy)

bar_plot_bot_team <- pred_wins3 %>% 
  mutate(team = str_remove_all(team, "20")) %>%
  arrange(accuracy) %>% 
  top_n(n=15,wt=-accuracy)

bar_plot_top_bot_team <- bind_rows(bar_plot_top_team,bar_plot_bot_team)



team2 <- ggplot(bar_plot_top_bot_team, aes(y = accuracy, x = reorder(team, accuracy),
                             fill=ifelse(accuracy>=0,"green","red"))) + 
  geom_bar(position="identity", stat="identity") +
  coord_flip() +
  xlab("Team_Year") + 
  ylab("Predictability +/- Baseline") + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Team Predictability 2015 - 2019",
       subtitle = "Using Team-Specific Models\nTop and Bottom 15 Teams",
       caption = "Right = More Predictable\nLeft = Less Predicatble\nSource: NFLScrapR")



grid.arrange(team1, team2, ncol=2)

