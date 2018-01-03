prem_P4 <- highchart() %>% 
  hc_plotOptions(
    series = list(
      boderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>% 
  hc_chart(type = "column") %>% 
  hc_xAxis(type = "category") %>% 
  hc_add_series(
    pointWidth=80,
    name = "Matches",
    data = list(
      list(name = "Mediums", y = sum(win_rates_prem$matches[win_rates_prem$class == "Medium Tank"]), drilldown = "medium")
    )
  ) %>% 
  hc_add_series(
    pointWidth=80,
    name = "Matches ",
    data = list(
      list(name = "Destroyer", y = sum(win_rates_prem$matches[win_rates_prem$class == "Tank Destroyer"]), drilldown = "td")
    )
  )  %>% 
  hc_add_series(
    pointWidth=80,
    name = "Matches ",
    data = list(
      list(name = "Heavy", y = sum(win_rates_prem$matches[win_rates_prem$class == "Heavy Tank"]), drilldown = "heavy")
    )
  )  %>% 
  hc_drilldown(
    series = list(
      list(
        id = "medium",
        name = "Stats",
        data = list(
          list(name = "Win Rate", 
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank"]),
               drilldown = "m_win_rate"),
          list(name = "WN8",
               y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank"])),
               drilldown = "m_wn8"),
          list(name = "Matches",
               y = sum(win_rates_prem$matches[win_rates_prem$class == "Medium Tank"]),
               drilldown = "m_matches")
        )
      ),
      list(
        id  = "m_win_rate",
        name = "Medium Tanks Win Rates (in Percent)",
        data = list(
          list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1], 
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1]])),
          list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2],
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2]])),
          list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3],
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3]])),
          list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4],
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4]])),
          list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5],
               y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5]]))
        )
      ),
    list(
      id  = "m_wn8",
      name = "Medium Tanks WN8",
      data = list(
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1], 
             y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1]]))),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2],
             y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2]]))),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3],
             y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3]]))),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4],
             y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4]]))),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5],
             y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5]])))
      )
    ),
    list(
      id  = "m_matches",
      name = "Medium Tanks Matches",
      data = list(
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1], 
             y = win_rates_prem$matches[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[1]]),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2],
             y = win_rates_prem$matches[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[2]]),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3],
             y = win_rates_prem$matches[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[3]]),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4],
             y = win_rates_prem$matches[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[4]]),
        list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5],
             y = win_rates_prem$matches[win_rates_prem$class == "Medium Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Medium Tank"])[5]])
      )
    ),
  list(
    id = "td",
    name = "Stats",
    data = list(
      list(name = "Win Rate", 
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Tank Destroyer"]),
           drilldown = "td_win_rate"),
      list(name = "WN8",
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Tank Destroyer"])),
           drilldown = "td_wn8"),
      list(name = "Matches",
           y = sum(win_rates_prem$matches[win_rates_prem$class == "Tank Destroyer"]),
           drilldown = "td_matches")
    )
  ),
  list(
    id  = "td_win_rate",
    name = "Tank Destroyer Win Rates (in Percent)",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1], 
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1]])),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2],
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2]]))
    )
  ),
  list(
    id  = "td_wn8",
    name = "Tank Destroyer WN8",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1], 
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1]]))),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2],
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2]])))
    )
  ),
  list(
    id  = "td_matches",
    name = "Tank Destroyer Matches",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1], 
           y = win_rates_prem$matches[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[1]]),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2],
           y = win_rates_prem$matches[win_rates_prem$class == "Tank Destroyer" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Tank Destroyer"])[2]])
      
    )),
  list(
    id = "heavy",
    name = "Stats",
    data = list(
      list(name = "Win Rate", 
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Heavy Tank"]),
           drilldown = "h_win_rate"),
      list(name = "WN8",
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Heavy Tank"])),
           drilldown = "h_wn8"),
      list(name = "Matches",
           y = sum(win_rates_prem$matches[win_rates_prem$class == "Heavy Tank"]),
           drilldown = "h_matches")
    )
  ),
  list(
    id  = "h_win_rate",
    name = "Heavy Tank Win Rates (in Percent)",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1], 
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1]])),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2],
           y = mean(win_rates_prem$win_rate[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2]]))
    )
  ),
  list(
    id  = "h_wn8",
    name = "Heavy Tank WN8",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1], 
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1]]))),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2],
           y = round(mean(win_rates_prem$wn8[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2]])))
    )
  ),
  list(
    id  = "h_matches",
    name = "Heavy Tank Matches",
    data = list(
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1], 
           y = win_rates_prem$matches[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[1]]),
      list(name = sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2],
           y = win_rates_prem$matches[win_rates_prem$class == "Heavy Tank" & win_rates_prem$tank == sort(win_rates_prem$tank[win_rates_prem$class == "Heavy Tank"])[2]])
      
    ))
  )
) %>% 
  hc_add_theme(hc_theme_flat()) %>% 
  hc_title(text = "Interactive Premium Tank Statistics")


