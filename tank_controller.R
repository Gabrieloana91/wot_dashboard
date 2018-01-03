
# Wot Controller ----------------------------------------------------------

library(plotly)
library(highcharter)
library(lubridate)
library(echarts)
library(RColorBrewer)
library(rjson)
library(stats)
library(spatstat)
library(quantmod)
library(tidyr)
library(plyr)
library(dtplyr)
library(data.table)
library(Rcpp)
library(zoo)
library(grid)
library(boot)
library(ISOcodes)
library(ggplot2)
library(xlsx)
library(stringr)
library(scales)
library(dplyr)

# setwd("C:/Users/GabzPC/Documents/wot_dashboard")
# source("drilldown_plot.R")


df <- read.csv("Replay Data.csv") %>%
  filter(Result != "Unknown") %>% 
  rename(tank = Tank,
         nation = Nation,
         tier = Tier,
         class = Class,
         result = Result,
         duration = Duration,
         survived = Survived,
         destroyed = Destroyed,
         spotted = Spotted,
         dmg_dealt = DMG.dealt,
         dmg_assist = Assisted.DMG,
         profit = Profit,
         wn8 = WN8,
         exp = Total.XP,
         free_exp = Total.free.XP) 

names(df)[1] <- "ts"

df <- df %>% 
  mutate(ts = as.POSIXct(ts, format = "%d/%m/%Y %H:%M:%S"),
         duration = hms(duration),
         tank = gsub(grep("Rhm.-Borsig", df$tank, value = T), "Rhm.-Borsig Waffentrager", df$tank),
         tier = as.numeric(tier),
         profit = as.numeric(profit),
         wn8 = as.numeric(wn8),
         dmg_dealt = as.numeric(dmg_dealt),
         dmg_assist = as.numeric(dmg_assist),
         destroyed = as.numeric(destroyed),
         spotted = as.numeric(spotted),
         exp = as.numeric(exp),
         free_exp = as.numeric(free_exp),
         dt = as.Date(ts),
         premium = case_when(
           Premium.vehicle.bonus.XP > 0 ~ "Premium"
         )) %>% 
  select(-Premium.vehicle.bonus.XP) %>% 
  arrange(ts)

df <- df %>% 
  group_by(ts) %>% 
  mutate(game = length(tank)) %>% 
  group_by(dt) %>% 
  mutate(game = cumsum(game)) %>% 
  left_join(df %>% 
              mutate(player = "me") %>% 
              select(dt, player) %>% 
              group_by(dt, player) %>% 
              mutate(session = length(unique(dt))) %>% 
              distinct(.) %>% 
              group_by(player) %>%
              mutate(session = cumsum(session)), by = "dt") %>% 
  select(-player) 

df$tank[df$tank == "M4A1 Revalorisé"] <- "M4A1 Revalorise"

# 1. Data Structure -------------------------------------------------------

# Session Information 
session <- df %>% 
  filter(session %in% tail(unique(df$session),10)) %>% 
  group_by(session) %>% 
  summarise(games_played = max(game),
            win_rate = round(length(game[result == "Victory"]) / max(game),3)*100,
            survive_rate = round(length(game[survived == "True"]) / max(game),3)*100,
            total_experience = round(sum(exp)),
            total_free_exp = round(sum(free_exp)),
            median_tier = round(mean(tier)),
            median_damage = round(mean(dmg_dealt)),
            median_experience = round(mean(exp)),
            kills = round(sum(destroyed))
  ) %>% 
  melt(., id.vars = 1)

# Cummulative Information
cumm <- df %>% 
  group_by(session) %>% 
  filter(session %in% tail(unique(session),2)) %>% 
  group_by(game) %>% 
  select(session,game, result, wn8) %>%
  mutate(victories = ifelse(result == "Victory", 1, 0)) %>% 
  group_by(session) %>% 
  mutate(victories = cumsum(victories),
         wn8 = cumsum(wn8)) %>% 
  group_by(game, session) %>% 
  mutate(pass_rate = round(victories / game * 100,1)) %>% 
  ungroup() %>% 
  mutate(wn8_rate = round(wn8 / game)) %>% 
  select(session, pass_rate, game, wn8_rate) %>% 
  mutate(session_temp = paste0("Session ",session)) %>% 
  select(session_temp, game, wn8_rate, pass_rate)

# Wn8 and Win Rate by Date
date_info <- df %>% 
  group_by(ts) %>% 
  select(wn8, result, free_exp, profit, survived, destroyed) %>% 
  mutate(victories = ifelse(result == "Victory", 1, 0),
         survived = ifelse(survived == "True", 1, 0)) %>% 
  ungroup() %>% 
  mutate(game = 1,
         wn8 = cumsum(wn8),
         victories = cumsum(victories),
         free_exp = cumsum(free_exp),
         profit = cumsum(profit),
         survived = cumsum(survived),
         destroyed = cumsum(destroyed)) %>% 
  mutate(game = cumsum(game),
         pass_rate = round(victories / game * 100,1),
         survival_rate = round(survived / game * 100,1)) %>% 
  group_by(ts) %>% 
  mutate(WN8 = round(wn8 / game),
         free_exp = round(free_exp / game),
         profit = round(profit / game),
         kill_ratio = round(destroyed / survived,2)) %>% 
  mutate(kill_ratio = ifelse(kill_ratio == "Inf", 3, kill_ratio))

# Premium Tanks
premium <- df %>% 
  filter(premium == "Premium") %>% 
  group_by(tank, class, game, session) %>% 
  mutate(game_no = 1) %>% 
  group_by(tank) %>% 
  mutate(game_no = cumsum(game_no)) %>% 
  group_by(tank, class, game_no) %>% 
  summarise(dmg = sum(dmg_dealt) + sum(dmg_assist),
            profit = sum(profit)) %>% 
  group_by(tank) %>% 
  mutate(cum_profit = cumsum(profit),
         low_average_dmg = round(mean(dmg) - 0.1 * mean(dmg)),
         high_average_dmg = round(mean(dmg) + 0.1 * mean(dmg)),
         low_average_profit = round(mean(profit) - 0.1 * mean(profit)),
         high_average_profit = round(mean(profit) + 0.1 * mean(profit)))


# 1.1Box Outputs -------------------------------------------------------------

games <- length(unique(df$ts))
matches_today <- length(unique(df$ts[as.Date(df$ts) == Sys.Date()]))
wn8_today <- round(mean(df$wn8[as.Date(df$ts) == Sys.Date()]))
credits_today <- round(sum(df$profit[as.Date(df$ts) == Sys.Date()]))
free_exp_today <- round(sum(df$free_exp[as.Date(df$ts) == Sys.Date()]))
win_rate_today <- df %>% 
  filter(as.Date(ts) == Sys.Date()) %>% 
  summarise(wr = round(length(result[result == "Victory"]) / length(result) * 100,2))

win_rates_prem <- df %>% 
  filter(premium == "Premium") %>% 
  group_by(tank, class) %>% 
  summarise(matches = length(tank),
            win_rate = round(length(tank[result == "Victory"]) / length(tank) * 100),
            wn8 = mean(wn8))

# 2. Plots ----------------------------------------------------------------


# 2.1 WN8 Per Game --------------------------------------------------------


X1 <- hchart(cumm[cumm$session_temp %in% tail(unique(cumm$session_temp),2),], "line", hcaes(x = game, y = wn8_rate, group = session_temp)) %>% 
  hc_tooltip(crosshairs = TRUE,
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 1426, to = 1750, color = "rgba(100, 0, 0, 0.1)",
                  label = list(text = "WN8 Average Interval")))) %>% 
  hc_title(text = "Cummulative WN8 Rating Per Game", align = "center") %>% 
  hc_legend(align = "left", verticalAlign = "middle",
            layout = "vertical", x = 0, y = 0) 


# 2.2 Win Rate Per Game ---------------------------------------------------


X2 <- hchart(cumm[cumm$session_temp %in% tail(unique(cumm$session_temp),2),], "line", hcaes(x = game, y = pass_rate, group = session_temp)) %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 45, to = 55, color = "rgba(100, 0, 0, 0.2)",
                  label = list(text = "Win Rate Average Interval")))) %>% 
  hc_title(text = "Cummulative Pass Rate", align = "center") %>% 
  hc_legend(align = "left", verticalAlign = "middle",
            layout = "vertical", x = 0, y = 0) 


# 2.3 Date Info -----------------------------------------------------------
# WN8

P1 <- highchart(type = "stock") %>% 
  hc_add_series_times_values(date_info$ts,
                             date_info$WN8,
                             name = "WN8") %>%
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538())%>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 1426, to = 1750, color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "WN8 Average Interval")))) %>% 
  hc_title(text = "<b>WN8 Rating</b>", align = "center")

#WinRate
P2 <- highchart(type = "stock") %>% 
  hc_add_series_times_values(date_info$ts,
                             date_info$pass_rate,
                             name = "Win Rate") %>%
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(max = 100,
           title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 45, to = 55, color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Average Win Rate Interval")))) %>% 
  hc_title(text = "Win Rate Rating", align = "center")

#Profit
P3 <- highchart(type = "stock") %>% 
  hc_add_series_times_values(date_info$ts,
                             date_info$profit,
                             name = "Profit") %>%
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 30000, to = 37500, color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Average Profit Interval")))) %>% 
  hc_title(text = "Profit Rating", align = "center")  
  
#Survival Rate 
P4 <- highchart(type = "stock") %>% 
  hc_add_series_times_values(date_info$ts,
                             date_info$survival_rate,
                             name = "Survival Rate") %>%
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = 45, to = 55, color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Average Survival Rate Interval")))) %>% 
  hc_title(text = "Survival Rate Rating", align = "center")  

#Kill Ratio
P5 <- highchart(type = "stock") %>% 
  hc_add_series_times_values(date_info$ts,
                             date_info$kill_ratio,
                             name = "Kill Ratio") %>%
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_title(text = "Kill Ratio Rating", align = "center")  
  

# 2.4 Premium Tanks -------------------------------------------------------


prem_P <- 
  highchart() %>% 
  hc_chart(type = "area") %>% 
  hc_title(text = "Cummulative Profit") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "Lorraine 40 t"],
                name = "Lorraine 40t") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "M4A1 Revalorise"],
                name = "M4A1 Revalorise") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "FV4202"],
                name = "FV4202") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "Strv S1"],
                name = "Strv S1",
                color = "green") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "T-29"],
                name = "T-29") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "T-54 first prototype"],
                name = "T-54 first prototype") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "T26E5 Patriot"],
                name = "T26E5 Patriot") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "T34"],
                name = "T34") %>% 
  hc_add_series(premium$cum_profit[premium$tank == "WZ-120-1G FT"],
                name = "WZ-120-1G FT",
                color = "yellow") %>% 
  hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>:
             <b>{point.percentage:.1f}%</b> ({point.y:,.0f} credits)<br/>",
             shared = TRUE) %>% 
  hc_plotOptions(area = list(
    stacking = "line",
    lineColor = "#ffffff",
    lineWidth = 1,
    marker = list(
      lineWidth = 1,
      lineColor = "#ffffff"
    ))
  ) %>% 
  hc_add_theme(hc_theme_538())

prem <- premium %>% 
  group_by(tank) %>% 
  summarise(max_profit = max(cum_profit))

prem_P2 <- hchart(prem , "treemap", hcaes(x = tank, value = max_profit, color = max_profit)) %>% 
  hc_title(text = "Total Profit") %>%
  hc_add_theme(hc_theme_538()) %>% 
  hc_tooltip(pointFormat = "{point.max_profit:,.0f} credits<br/>",
             shared = TRUE)

prem2 <- premium %>% 
  group_by(tank) %>% 
  summarise(profit_per_game = round(max(cum_profit) / max(game_no)))
  

prem_P3 <- highchart() %>% 
  hc_add_series(prem2, "pie", hcaes(name = tank, y = profit_per_game), name = "credits") %>% 
  hc_tooltip(formatter = JS("function(){
                                return  '<b>' + this.point.label + ': </b>( Credits:' +this.y+', Percentage: '+Highcharts.numberFormat(this.percentage)+'%)'
  }"),useHTML = FALSE) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_title(text = "Profit Per Game") 


  
# 2.4.1 Lorraine ----------------------------------------------------------

lorraine_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "Lorraine 40 t"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "Lorraine 40 t"]), to = mean(premium$high_average_dmg[premium$tank == "Lorraine 40 t"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "Lorraine 40t - Damage", align = "center")  

lorraine_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "Lorraine 40 t"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "Lorraine 40 t"]), to = mean(premium$high_average_profit[premium$tank == "Lorraine 40 t"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "Lorraine 40t - Profit", align = "center")



# 2.4.2 Revalorise -------------------------------------------------------------------

rev_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "M4A1 Revalorise"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "M4A1 Revalorise"]), to = mean(premium$high_average_dmg[premium$tank == "M4A1 Revalorise"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "M4A1 Revalorise - Damage", align = "center")  

rev_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "M4A1 Revalorise"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "M4A1 Revalorise"]), to = mean(premium$high_average_profit[premium$tank == "M4A1 Revalorise"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "M4A1 Revalorise - Profit", align = "center")




# 2.4.3 FV4202 ------------------------------------------------------------

fv_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "FV4202"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "FV4202"]), to = mean(premium$high_average_dmg[premium$tank == "FV4202"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "FV4202 - Damage", align = "center")  

fv_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "FV4202"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "FV4202"]), to = mean(premium$high_average_profit[premium$tank == "FV4202"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "FV4202 - Profit", align = "center")



# 2.4.4 Strv S1 -----------------------------------------------------------
s1_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "Strv S1"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "Strv S1"]), to = mean(premium$high_average_dmg[premium$tank == "Strv S1"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "Strv S1 - Damage", align = "center")  

s1_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "Strv S1"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "Strv S1"]), to = mean(premium$high_average_profit[premium$tank == "Strv S1"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "Strv S1 - Profit", align = "center")

# 2.4.5 T-54 First Prototype-------------------------------------------------------------------
t54_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "T-54 first prototype"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "T-54 first prototype"]), to = mean(premium$high_average_dmg[premium$tank == "T-54 first prototype"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "T-54 first prototype - Damage", align = "center")  

t54_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "T-54 first prototype"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "T-54 first prototype"]), to = mean(premium$high_average_profit[premium$tank == "T-54 first prototype"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "T-54 first prototype - Profit", align = "center")

# 2.4.6 T26E5 Patriot -----------------------------------------------------
pat_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "T26E5 Patriot"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "T26E5 Patriot"]), to = mean(premium$high_average_dmg[premium$tank == "T26E5 Patriot"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "T26E5 Patriot - Damage", align = "center")  

pat_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "T26E5 Patriot"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "T26E5 Patriot"]), to = mean(premium$high_average_profit[premium$tank == "T26E5 Patriot"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "T26E5 Patriot - Profit", align = "center")


# 2.4.7 WZ-120-1G FT ------------------------------------------------------
wz_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "WZ-120-1G FT"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "WZ-120-1G FT"]), to = mean(premium$high_average_dmg[premium$tank == "WZ-120-1G FT"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "WZ-120-1G FT - Damage", align = "center")  

wz_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "WZ-120-1G FT"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "WZ-120-1G FT"]), to = mean(premium$high_average_profit[premium$tank == "WZ-120-1G FT"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "WZ-120-1G FT - Profit", align = "center")


# 2.4.8 T34 ---------------------------------------------------------------
t34_P1 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$dmg[premium$tank == "T34"], 
                name = "Damage") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_dmg[premium$tank == "T34"]), to = mean(premium$high_average_dmg[premium$tank == "T34"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Damage Interval")))) %>% 
  hc_title(text = "T34 - Damage", align = "center")  

t34_P2 <- 
  highchart(type = "chart") %>% 
  hc_add_series(premium$profit[premium$tank == "T34"], 
                name = "Profit") %>% 
  hc_tooltip(crosshairs = TRUE, 
             shared = TRUE, borderWidth = 2) %>% 
  hc_add_theme(hc_theme_538()) %>% 
  hc_yAxis(title = list(text = ""),
           opposite = TRUE,
           showFirstLabel = FALSE,
           showLastLabel = FALSE,
           plotBands = list(
             list(from = mean(premium$low_average_profit[premium$tank == "T34"]), to = mean(premium$high_average_profit[premium$tank == "T34"]), color = "rgba(100, 1, 0, 0.2)",
                  label = list(text = "Profit Interval")))) %>% 
  hc_title(text = "T34 - Profit", align = "center")


