# app.R

# This is the UI part of the Shiny app for predicting breath ethanol levels
# Author: Muenchow


library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyBS)


dashboardPage(
  
  # test
  dashboardHeader(
    title = "AlcoholTracker Sommer 25 edition",
    tags$li(
      class = "dropdown", 
      style = "position: absolute; right: 20px; top: 13px",
      div(
        style = "display: flex; gap: 10px; align-items: center;",
        p("Redraw GIFs: ", 
          style = "margin-bottom: 0em; color: white"),
        actionButton(
          "redraw_btn", 
          icon("arrows-rotate"),  # Corrected icon name
          title = "Redraw GIFs", 
          class = "btn btn-xs btn-default",
          width = 40
        ),
        p("Reset drinks: ", 
          style = "margin-bottom: 0em; color: white"),
        actionButton(
          "reset_drinks_btn", 
          icon("undo"), 
          title = "Reset Drinks", 
          class = "btn btn-xs btn-default",
          width = 40
        )
      )
    )
  ),
  dashboardSidebar(
    # Main sidebar content (menu)
    sidebarMenu(
      menuItem("First step", tabName = "apriori", icon = icon("user-plus")),
      menuItem("Scenarios", tabName = "aposthoc", icon = icon("user-check"))
    )
  ),
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom_styles.css")
    ),
      
    tabItems(
      # A Priori Tab
      tabItem(
        tabName = "apriori",
        fluidRow(
          column(
            width = 4,
            box(
              title = "Participant Info",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow(
                column(6,
                       numericInput("ap_age", "Age (years)", value = 40, min = 0)
                ),
                column(6,
                       numericInput("ap_weight", "Weight (kg)", value = 80, min = 0)
                )
              ),
              fluidRow(
                column(6,
                       numericInput("measured_time", "Last drink (min)", value = 15.0, min = 0)
                ),
                column(6,
                       numericInput("measured_conc", "Promille (‰)", value = 0.1, min = 0.01)
                )
              )
              
            ),
            box(
              title = "Drinks",
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              fluidRow( 
                column(
                  4,
                  tags$p("", class = "drink-label"),
                  br(), br(),
                  div(class = "icon-placement1",
                      actionButton(
                        inputId = "add_beer",
                        label = tags$img(src = "bottle.png", height = "50px"),
                        class = "icon-button",
                        title = "Standardbier in Deutschland ist meist ein Pils, Export oder Helles (0,5 L, 5 % Vol.). \n Fun Fact: Deutschland hat über 1.500 Brauereien – mehr als jedes andere Land der Welt!",
                        `data-toggle` = "tooltip"
                      ),
                  ),
                  div(class = "icon-placement2",
                      actionButton(
                        inputId = "add_white_wine",
                        label = tags$img(src = "wine-glass-white.png", height = "50px"),
                        class = "icon-button",
                        title = "Weißwein-Standardglas: 0,2 L, etwa 12 % Vol. \n Riesling ist der beliebteste deutsche Weißwein. Fun Fact: Deutschland ist weltweit der drittgrößte Weinexporteur von Riesling.",
                        `data-toggle` = "tooltip"
                      ),
                  ),
                  div(class = "icon-placement2",
                      actionButton(
                        inputId = "add_shot",
                        label = tags$img(src = "shot.png", height = "50px"),
                        class = "icon-button",
                        title = "Shots werden meist als Schnaps oder Likör (2 cl, 35–40 % Vol.) serviert. \n Fun Fact: Der Begriff ‚Kurzer‘ ist typisch norddeutsch für einen Shot.",
                        `data-toggle` = "tooltip"
                      )
                  )
                )
                
                ,  
                
                column(8,
                       tags$p("", class = "quantity-label"),
                       fluidRow(
                         div(class = "slider-input",
                             sliderInput(
                               inputId = "beer_slider",
                               label = " ", min = 0, max = 10, value = 2, step = 1)),
                         column(4,
                                textInput(inputId = "beer_vol", " ", value = " ", width = "100%")),
                         column(1,
                                tags$p("L", class = "unit-label")),
                         column(4,
                                textInput("beer_conc", " ", value = "5,0")),
                         column(1,
                                tags$p("%", class = "unit-label")),
                       ),
                       
                       fluidRow(
                         div(class = "slider-input",
                             sliderInput(
                               inputId = "white_wine_slider",
                               label = " ", min = 0, max = 10, value = 0, step = 1)),
                         column(4,
                                textInput(inputId = "white_wine_vol", " ", value = " ")),
                         column(1,
                                tags$p("L", class = "unit-label")),
                         column(4,
                                textInput("white_wine_conc", " ", value = "12,0")),
                         column(1,
                                tags$p("%", class = "unit-label")),
                       ),
                       fluidRow(
                         div(class = "slider-input",
                             sliderInput(
                               inputId = "shot_slider",
                               label = " ", min = 0, max = 10, value = 0, step = 1)),
                         column(4,
                                textInput(inputId = "shot_vol", " ", value = " ")),
                         column(1,
                                tags$p("mL", class = "unit-label")),
                         column(4,
                                textInput("shot_conc", " ", value = "40,0")),
                         column(1,
                                tags$p("%", class = "unit-label")),
                       ),
                ) 
              )
            )
          ),
          column(
            width = 8,
            box(
              title = "Model prediction vs. measured concentration",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              div(
                style = "position: relative;",
                plotOutput("aprioriPlot", height = "600px"),
                # Slider anchored to the bottom right inside the box
                div(
                  style = "
                    position: absolute;
                    top: 10px; 
                    right: 20px; 
                    width: 320px;
                    background: rgba(255,255,255,0.85);
                    padding: 6px 12px 6px 12px;
                    border-radius: 8px;
                    box-shadow: 0 2px 8px rgba(0,0,0,0.07);
                    display: flex; 
                    align-items: center;
                    z-index: 10;
                  ",
                  sliderInput(
                    inputId = "ap_time",
                    label = NULL,
                    min = 1,
                    max = 24,
                    value = 10,
                    step = 1,
                    width = "180px"
                  ),
                  tags$span("Timeframe (hours)", style = "margin-left: 8px; font-size: 14px; color: #444;")
                )
              )
            )
          ),
          fluidRow(
            column(
              width = 12,
              uiOutput("gif_panel")
            ))
        )
      ),
      
      
      
      tabItem(
        tabName = "aposthoc",
        uiOutput("aposthoc_body")
      )
      
    )
  )
)
