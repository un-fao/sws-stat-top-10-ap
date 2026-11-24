#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)

navbarPage('Main Commodities Production Data Analysis',
  tabPanel('Dynamic',
           # Input: Select the random distribution type ----
           # selectInput("staorfl", "Commodity type:", c("Static", "Dynamic")),
           # Input: Select the commodity type ----
           sidebarPanel(selectInput("comm","Commodity:",c("All", unique(as.character(unique(world_crop3$Item))))),
                        selectInput("ani","Live Animal:",c("All", unique(as.character(unique(world_liveanimal3$Item))))),
                        # Input: Slider for the percentage to control ----
                        numericInput("percentage","Percentage(%) to control:",value = 0.1,min = 0,max = 1, step = 0.1)),
           mainPanel(# Output: Tabset for primary commodities and live stocks ----
                     tabsetPanel(type = "tabs",
                                 tabPanel("Primary commodities dynamic", dataTableOutput("dynamicPrimary")),
                                 tabPanel("Live animal dynamic", dataTableOutput("dynamicLiveanimal"))))
        ),
  
  tabPanel('Static',
           # Input: Select the random distribution type ----
           # selectInput("staorfl", "Commodity type:", c("Static", "Dynamic")),
           # Input: Select the commodity type ----
           sidebarPanel(selectInput("comm2","Commodity:",c("All", unique(as.character(unique(static_list$Item))))),
                        selectInput("ani2","Live Animal:",c("All", unique(as.character(unique(static_list_liveanimal$Item))))),
                        #uiOutput('check'),
                        # Input: Slider for the percentage to control ----
                        numericInput("percentage2","Percentage(%) to control:",value = 0.1,min = 0,max = 1, step = 0.1)),
           mainPanel(# Output: Tabset for primary commodities and live stocks ----
                     tabsetPanel(type = "tabs",
                                 tabPanel("Primary commodities static", dataTableOutput("staticPrimary")),
                                 tabPanel("Live animal static", dataTableOutput("staticLiveanimal"))))
           )

  )



# # Define UI for random distribution app ----
# navbarPage(
#     tabPanel('Dynamic',
#         #titlePanel("Main Commodities Production Data Analysis"),
#         
#         # Sidebar layout with input and output definitions ----
#         sidebarLayout(
#             # Sidebar panel for inputs ----
#             sidebarPanel(
#                 # Input: Select the random distribution type ----
#                 selectInput("staorfl", "Commodity type:",
#                             c("Static", "Dynamic")),
#                 
#                 
#                 # Input: Select the commodity type ----
#                 selectInput("comm",
#                             "Commodity:",
#                             c("All",
#                               unique(as.character(unique(world_crop3$Item))))),
#                 
#                 uiOutput('check'),
#                 # Input: Slider for the percentage to control ----
#                 sliderInput("percentage",
#                             "Percentage",
#                             value = 0.02,
#                             min = 0,
#                             max = 10)
#     )),
#         
#         # Main panel for displaying outputs ----
#         mainPanel(
#             
#             # Output: Tabset for primary commodities and live stocks ----
#             tabsetPanel(type = "tabs",
#                         tabPanel("Primary commodities", dataTableOutput("worldCrops")),
#                         tabPanel("Live animal", dataTableOutput("worldCrops"))
#             )
#             
#         )
#     )
# )
