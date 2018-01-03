library(shiny)
library(shinydashboard)
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


# 1. UI.R -----------------------------------------------------------------
# source("C:\\Users\\GabzPC\\Documents\\wot_dashboard\\tank_controller.R")
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "World of Tanks Dashboard"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Overview", tabName = "dashboard", icon = icon("dashboard")),
                        menuItem("Premium", tabName = "premium", icon = icon("cc-visa"),
                                 menuSubItem("All Premium Tanks", tabName = "overall", icon = icon("taxi")),
                                 menuSubItem("Patriot", tabName = "patriot", icon = icon("taxi")),
                                 menuSubItem("Revalorise", tabName = "rev", icon = icon("taxi")),
                                 menuSubItem("Lorraine", tabName = "lorraine", icon = icon("taxi")),
                                 menuSubItem("FV4202", tabName = "fv4202", icon = icon("taxi")),
                                 menuSubItem("Strv S1", tabName = "strv_s1", icon = icon("taxi")),
                                 menuSubItem("T54 Prototype", tabName = "t54-1", icon = icon("taxi")),
                                 menuSubItem("WZ-120-1G", tabName = "wz", icon = icon("taxi")),
                                 menuSubItem("T34", tabName = "t34", icon = icon("taxi"))
                                 ),
                        menuItem("Settings", tabName = "refresh", icon = icon("cog"),
                                 actionButton("action","Load Data", icon = icon("refresh")))
                        )
                    ),
                    dashboardBody(
                      tabItems(
                        # First Tab
                        tabItem(tabName = "dashboard",
                                fluidRow(
                                  valueBoxOutput("wn8_box"),
                                  valueBoxOutput("matches_t"),
                                  valueBoxOutput("games_t"),
                                  valueBoxOutput("profit_t"),
                                  valueBoxOutput("free_exp_t"),
                                  valueBoxOutput("win_rate_t"),
                                  box(highchartOutput("WN8")),
                                  box(highchartOutput("pass_rate")),
                                  box(highchartOutput("profit")),
                                  box(highchartOutput("survival")),
                                  box(highchartOutput("kill_ratio"))
                                )
                                
                        ),
                        
                        # Second Tab
                        tabItem(tabName = "overall",
                                fluidRow(
                                  box(highchartOutput("premium_tank"), width = 12),
                                  box(highchartOutput("prem_P2")),
                                  box(highchartOutput("prem_P3")),
                                  box(highchartOutput("prem_P4")))),
                        
                        # Subtabs
                        tabItem(tabName = "patriot",
                                fluidRow(valueBoxOutput("patriot_wn8"),
                                         valueBoxOutput("patriot_match"),
                                         valueBoxOutput("patriot_wr"),
                                         box(highchartOutput("pat_P1")),
                                         box(highchartOutput("pat_P2")))),
                        tabItem(tabName = "lorraine",
                                fluidRow(valueBoxOutput("lorr_wn8"),
                                         valueBoxOutput("lorr_match"),
                                         valueBoxOutput("lorr_wr"),
                                         box(highchartOutput("lorraine_P1")),
                                         box(highchartOutput("lorraine_P2")))),
                        tabItem(tabName = "fv4202",
                                fluidRow(valueBoxOutput("FV4202_wn8"),
                                         valueBoxOutput("FV4202_match"),
                                         valueBoxOutput("FV4202_wr"),
                                         box(highchartOutput("fv_P1")),
                                         box(highchartOutput("fv_P2")))),
                        tabItem(tabName = "strv_s1",
                                fluidRow(valueBoxOutput("strv_wn8"),
                                         valueBoxOutput("strv_match"),
                                         valueBoxOutput("strv_wr"),
                                         box(highchartOutput("s1_P1")),
                                         box(highchartOutput("s1_P2")))),
                        tabItem(tabName = "rev",
                                fluidRow(valueBoxOutput("rev_wn8"),
                                         valueBoxOutput("rev_match"),
                                         valueBoxOutput("rev_wr"),
                                         box(highchartOutput("rev_P1")),
                                         box(highchartOutput("rev_P2")))),
                        tabItem(tabName = "t54-1",
                                fluidRow(valueBoxOutput("t54_wn8"),
                                         valueBoxOutput("t54_match"),
                                         valueBoxOutput("t54_wr"),
                                         box(highchartOutput("t54_P1")),
                                         box(highchartOutput("t54_P2")))),
                        tabItem(tabName = "wz",
                                fluidRow(valueBoxOutput("wz_wn8"),
                                         valueBoxOutput("wz_match"),
                                         valueBoxOutput("wz_wr"),
                                         box(highchartOutput("wz_P1")),
                                         box(highchartOutput("wz_P2")))),
                        tabItem(tabName = "t34",
                                fluidRow(valueBoxOutput("t34_wn8"),
                                         valueBoxOutput("t34_match"),
                                         valueBoxOutput("t34_wr"),
                                         box(highchartOutput("t34_P1")),
                                         box(highchartOutput("t34_P2")))),
                        # Third tab
                        
                        tabItem(tabName = "tank_selector",
                                fluidRow(verbatimTextOutput("non_premium_plot1")))
                        

                      )
                      
                      
                    )
)





# 2. APP.R ----------------------------------------------------------------

server <- function(input, output) { 
  
  observeEvent(input$action, {
    # source("C:\\Users\\GabzPC\\Documents\\wot_dashboard\\tank_controller.R")
    source("tank_controller.R", local = TRUE)
    source("drilldown_plot.R", local = TRUE)
    

    
    #Overall Plots
    output$WN8 <- renderHighchart({P1})
    output$pass_rate <- renderHighchart({P2})
    output$profit <- renderHighchart({P3})
    output$survival <- renderHighchart({P4})
    output$kill_ratio <- renderHighchart({P5})
    # Premium Tank 
    output$premium_tank <- renderHighchart({prem_P})
    output$prem_P2 <- renderHighchart({prem_P2})
    output$prem_P3 <- renderHighchart({prem_P3})
    output$prem_P4 <- renderHighchart({prem_P4})
    output$lorraine_P1 <-  renderHighchart({lorraine_P1})
    output$lorraine_P2 <-  renderHighchart({lorraine_P2})
    output$rev_P1 <-  renderHighchart({rev_P1})
    output$rev_P2 <-  renderHighchart({rev_P2})
    output$fv_P1 <-  renderHighchart({fv_P1})
    output$fv_P2 <-  renderHighchart({fv_P2})
    output$s1_P1 <-  renderHighchart({s1_P1})
    output$s1_P2 <-  renderHighchart({s1_P2})
    output$t54_P1 <-  renderHighchart({t54_P1})
    output$t54_P2 <-  renderHighchart({t54_P2})
    output$pat_P1 <-  renderHighchart({pat_P1})
    output$pat_P2 <-  renderHighchart({pat_P2})
    output$wz_P1 <-  renderHighchart({wz_P1})
    output$wz_P2 <-  renderHighchart({wz_P2})
    output$t34_P1 <-  renderHighchart({t34_P1})
    output$t34_P2 <-  renderHighchart({t34_P2})
    
    
    
    output$wn8_box <- renderValueBox({
      valueBox(wn8_today, "WN8 Today", icon = icon("tachometer"), color = "red")
    })
    output$matches_t <- renderValueBox({
      valueBox(matches_today, "Matches Today", icon = icon("tachometer"), color = "yellow")
    })
    output$games_t <- renderValueBox({
      valueBox(games, "Matches", icon = icon("tachometer"), color = "purple")
    })
    output$profit_t <- renderValueBox({
      valueBox(credits_today, "Profit Today", icon = icon("tachometer"), color = "red")
    })
    output$free_exp_t <- renderValueBox({
      valueBox(free_exp_today, "Free XP Today", icon = icon("tachometer"), color = "yellow")
    })
    output$win_rate_t <- renderValueBox({
      valueBox(paste0(win_rate_today$wr,"%"), "Win Rate Today", icon = icon("tachometer"), color = "purple")
    })
    
    ### Premium tanks render value boxes
    # FV4202
    output$FV4202_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "FV4202"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$FV4202_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "FV4202"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$FV4202_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "FV4202"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # Patriot
    output$patriot_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "T26E5 Patriot"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$patriot_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "T26E5 Patriot"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$patriot_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "T26E5 Patriot"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # Lorraine 40t
    output$lorr_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "Lorraine 40 t"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$lorr_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "Lorraine 40 t"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$lorr_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "Lorraine 40 t"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # Revalorise
    output$rev_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "M4A1 Revalorise"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$rev_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "M4A1 Revalorise"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$rev_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "M4A1 Revalorise"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # Strv S1
    output$strv_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "Strv S1"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$strv_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "Strv S1"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$strv_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "Strv S1"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # T-29
    output$t29_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "T-29"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$t29_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "T-29"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$t29_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "T-29"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # T-54 first prototype
    output$t54_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "T-54 first prototype"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$t54_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "T-54 first prototype"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$t54_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "T-54 first prototype"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # T34
    output$t34_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "T34"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$t34_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "T34"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$t34_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "T34"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    
    # WZ-120-1G FT
    output$wz_match <- renderValueBox({
      valueBox(win_rates_prem$matches[win_rates_prem$tank == "WZ-120-1G FT"], "Matches", icon = icon("tachometer"), color = "red")
    })
    output$wz_wr <- renderValueBox({
      valueBox(paste0(win_rates_prem$win_rate[win_rates_prem$tank == "WZ-120-1G FT"],"%"), "Win Rate", icon = icon("tachometer"), color = "yellow")
    })
    output$wz_wn8 <- renderValueBox({
      valueBox(round(win_rates_prem$wn8[win_rates_prem$tank == "WZ-120-1G FT"]), "WN8", icon = icon("tachometer"), color = "aqua")
    })
    

  })
  
}

# 3. RUN APPLICATION ------------------------------------------------------

shinyApp(ui = ui, server = server)
