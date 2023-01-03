library(tidyverse)
library(baseballr)
library(GeomMLBStadiums)
library(Lahman)
library(ggthemes)
library(gt)
library(gtExtras)
library(patchwork)
library(magick)
library(webshot2)


#### DATA GATHER ####



#### Pitch Classification ####

#### James Stein BA ####

test_at_bat_data <- Lahman::Batting

test_batting_2020 <- test_at_bat_data |> 
  filter(yearID == 2020) |> 
  select(playerID, G, AB, H) |> 
  group_by(playerID) |> 
  summarise(across(.cols = (G:H), .fns = sum),.groups = "drop") |> 
  mutate(BA = ifelse(is.na(H / AB), 0, H / AB))

test_batting_2021 <- test_at_bat_data |> 
  filter(yearID == 2021) |> 
  select(playerID, G, AB, H) |> 
  group_by(playerID) |> 
  summarise(across(.cols = (G:H), .fns = sum),.groups = "drop") |> 
  mutate(BA = ifelse(is.na(H / AB), 0, H / AB))


##### Estimator for BA #####

# JAMES STEIN Explaination:

# z = y_hat + c * (y - y_hat)
# y_hat = "populatiion mean"
# y = "observed average for individual player"
# c = "shrinkage factor" = 1 - ((n_obs - degrees_of_freedom) * sigma^2) / sum((y - y_hat)^2)
# degrees_of_freedom = 3 = "the maximum number of logically independent values, which are values that have the freedom to vary"
# 1 - c = "shrinkage towards group mean"
# "Shrinkage Factor is telling us how to regress the players BA towards the population mean

# Parameters:

# BA : numeric vector of all players measured batting averages

# AB : numeric vector of all players number of at bats

# Return: vector of equivalent length of estimated batting average for players

mean(test_batting_2020$AB)
quantile(test_batting_2020$AB)

mean(test_batting_2020$BA)
sd(test_batting_2020$BA)

mean(test_batting_2020$BA[test_batting_2020$AB >= quantile(test_batting_2020$AB, probs = 0.65)])

sd(test_batting_2020$BA[test_batting_2020$AB >= quantile(test_batting_2020$AB, probs = 0.65)])

##### James Stein Function #####

test_james_stein_estimator <- function(BA, AB) {
  
  # confirm both vectors are the same length
  stopifnot(length(BA) == length(AB))
  
  # which players where in at least the 55th percentile of ABs
  prcntl_50 <- AB >= quantile(AB, probs = 0.65)
  
  # calc group mean and sd of BA for players at least in the 55th percentile of ABs
  group_mean <- mean(BA[prcntl_50])
  group_sd <- sd(BA[prcntl_50])
  
  # calc sum squares of players difference from group mean
  sum_sq_diff <- sum((BA - group_mean)^2)
  
  # calc "c" if not passed
  c <- 1 - ((length(AB) - 3) * group_sd^2) / sum_sq_diff
  
  # calc players estimated BA given players measured BA and "c"
  js_est <- group_mean + c * (BA - group_mean) #"z = y_hat + c * (y - y_hat)
  
  return(js_est)
}


# joining with player name
test_batting_2022_js <- test_batting_2020 |> 
  mutate(
    james_stein = test_james_stein_estimator(BA, AB),
    js_sd = ifelse(AB == 0,
                   sqrt((james_stein * (1 - james_stein))) / 1,
                   sqrt((james_stein * (1 - james_stein)) / AB)),
    low = ifelse(james_stein - js_sd < 0, 0, james_stein - js_sd),
    high = james_stein + js_sd
  )

test_batting_2022_js <- test_batting_2022_js |> 
  left_join(
    Lahman::People |> 
      mutate(
        name = paste(nameFirst, nameLast)
      ) |> 
      select(playerID,name),
    by = "playerID"
  )

# 2021

test_batting_2021_js <- test_batting_2021 |> 
  mutate(
    james_stein = test_james_stein_estimator(BA, AB),
    js_sd = ifelse(AB == 0,
                   sqrt((james_stein * (1 - james_stein))) / 1,
                   sqrt((james_stein * (1 - james_stein)) / AB)),
    low = ifelse(james_stein - js_sd < 0, 0, james_stein - js_sd),
    high = james_stein + js_sd
  ) |> 
  inner_join(test_batting_2022_js,
             by = "playerID",
             suffix = c("_2021", "_2020"))

quantile(test_batting_2021_js$AB_2021)

##### PLOT #####

test_batting_2021_js |> 
  filter(AB_2021 > 230 & BA_2021 > 0) |> 
  sample_n(10) |> 
  ggplot(aes(y = name, x = BA_2021)) +
  geom_errorbar(aes(xmin = low_2020, xmax = high_2020),
                width = 0,
                size = 5,
                color = "lightgray",
                alpha = 0.6) +
  geom_point(aes(size = AB_2021),
             color = "red") +
  geom_point(aes(x = james_stein_2020),
             color = "skyblue",
             size = 5,
             shape = 124) +
  geom_point(aes(x = james_stein_2021),
             color = "blue",
             size = 5,
             shape = 124) +
  annotate(geom = "text",
           x = 0.05,
           y = 9.5,
           hjust = 0,
           label = "OBSERVED BA",
           color = "red") +
  annotate(geom = "text",
           x = 0.05,
           y = 9.0,
           hjust = 0,
           label = "ESTIMATED TRUE BA (2021)",
           color = "blue") + 
  annotate(geom = "text",
           x = 0.05,
           y = 8.5,
           hjust = 0,
           label = "ESTIMATED TRUE BA (2020)",
           color = "skyblue") +
  annotate(geom = "text",
           x = 0.05,
           y = 8.0,
           hjust = 0,
           label = "ERRORBAR BASED OFF 2020 BA",
           color = "gray") +
  labs(
    title = "MLB 2021 James Stein Estimates | Batting Average",
    subtitle = "| At Bats > 230 |",
    x = "Batting Average 2021",
    size = "2021 AB"
  ) +
  theme_clean() +
  theme(
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_text(face = "bold")
  )




##### GT #####

test_table_players <- test_batting_2021_js |> 
  filter(AB_2021 > 230 & AB_2020 > 100,
         BA_2021 > 0 & BA_2020 > 0) |> 
  pull(playerID) |> 
  sample(size = 10)

test_vis_data <- test_batting_2021_js |> 
  filter(playerID %in% test_table_players) |> 
  arrange(playerID) |> 
  mutate(
    name = factor(name, levels = name) # to preserve
  )

test_gt_ba <- test_vis_data |> 
  arrange(desc(name)) |> 
  select(
    name,
    G_2020,
    G_2021,
    AB_2020,
    AB_2021,
    BA_2020,
    BA_2021,
    james_stein_2020,
    james_stein_2021
  ) |> 
  gt(
    rowname_col = "name"
  ) |> 
  cols_label(
    G_2020 = "2020",
    G_2021 = "2021",
    AB_2020 = "2020",
    AB_2021 = "2021",
    BA_2020 = "2020",
    BA_2021 = "2021",
    james_stein_2020 = "2020",
    james_stein_2021 = "2021",
  ) |> 
  tab_spanner(
    label = html("<br>Games"),
    columns = starts_with("G")
  ) |> 
  tab_spanner(
    label = html("<br>At Bats"),
    columns = starts_with("AB")
  ) |> 
  tab_spanner(
    label = html("<br>Batting Average"),
    columns = starts_with("BA")
  ) |> 
  tab_spanner(
    label = html("<br>JS Estimate"),
    columns = starts_with("james_stein")
  ) |> 
  fmt_number(
    c(starts_with("BA"), starts_with("james_stein")),
    decimals = 3
  ) |> 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = starts_with("BA")
    )) |> 
  tab_style(
    style = cell_text(color = "blue"),
    locations = cells_body(
      columns = starts_with("james_stein")
    ))
test_gt_ba


##### Patchwork #####

# plot objects for patchwork

test_plot_2020 <- test_vis_data |> 
  ggplot(aes(y = name, x = BA_2020)) +
  geom_errorbar(aes(xmin = low_2020, xmax = high_2020),
                width = 0,
                size = 5,
                color = "lightgray",
                alpha = 0.6) +
  geom_point(aes(size = AB_2020),
             color = "red") +
  geom_point(aes(x = james_stein_2020),
             color = "blue",
             size = 5,
             shape = 124) +
  xlim(0,.45) +
  labs(
    title = "2020 Batting Averages | MLB"
  ) +
  theme_clean() +
  theme(
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

test_plot_2020


test_plot_2021 <- test_vis_data |> 
  ggplot(aes(y = name, x = BA_2021)) +
  geom_errorbar(aes(xmin = low_2021, xmax = high_2021),
                width = 0,
                size = 5,
                color = "lightgray",
                alpha = 0.6) +
  geom_point(aes(size = AB_2021),
             color = "red") +
  geom_point(aes(x = james_stein_2021),
             color = "blue",
             size = 5,
             shape = 124) +
  xlim(0,.45) +
  labs(
    title = "2021 Batting Averages | MLB"
  ) +
  theme_clean() +
  theme(
    plot.title.position = "plot",
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "none"
  )

test_plot_2021


# combine plots and gt into single plot

# convert to temp image

