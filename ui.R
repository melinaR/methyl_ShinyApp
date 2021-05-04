#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(latex2exp)
library(DiceEval)
library(raster)
library(Jmisc)
library(plotly)
library(DT)

load(file = "../../data/small_data.Rdata")
Index_seq = small_data$index
Y = small_data$Y
N = nrow(Y)
K = ncol(Y)
sourceAll(path="../../code/R_functions/")

dashboardPage(
    dashboardHeader(title = "FM méthyl"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Paramètres et résultats globaux", tabName = "all", icon = icon("th")),
            menuItem("y_true vs y_pred", tabName = "ychart", icon = icon("chart-bar")),
            menuItem("Visualisations des processus", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("FMP : évolution RMSE", tabName = "fmp", icon = icon("th")),
            menuItem("FMPB : évolution RMSE", tabName = "fmpb", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
        
        #### subData ####
            # Second tab content
            tabItem(tabName = "all",
                    fluidPage(
                        shinyjs::useShinyjs(),
                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                            sidebarPanel(
                                selectInput("kstar", label = "Nombre d'échantillons avec NA",
                                            choices = c(1:(K-1)), selected = 1),
                                sliderInput("nstar", label = "Nombre de site NA",
                                            min = N/10, max = N-(N/10), value = N/10, step = N/10),
                                sliderInput("rank", label = "Rang de la factorisation",
                                            min = 2, max = K, value = 2, step = 1),
                                textInput("par_fmn", label = withMathJax("paramètre pour fmn : 
                                                                       $$\\alpha_1,\\alpha_2,\\alpha_3,
                                                                       \\beta_1,\\beta_2,\\beta_3$$"),
                                          value="0,0,0,0,0,0"),
                                textInput("par_fmp", label = withMathJax("paramètre pour fmp : 
                                                                       $$\\lambda,\\mbox{max_iter}$$"),
                                          value="0,80"),
                                textInput("par_fmpb", label = withMathJax("paramètre pour fmpb : 
                                                                       $$\\mbox{ngibbs},\\mbox{burnin},
                                                                          \\alpha,\\beta_{u,v}$$"),
                                          value="100,1,10000,10000"),
                                actionButton("run_plot",
                                             "mise a jour données"),
                                actionButton("run_rank",
                                             "mise a jour de rang"),
                                actionButton("make_matlab","Création data pour fmp et fmpb"),
                                actionButton("run_fmn","Run FMN"),
                                actionButton("run_fmp","Run FMP"),
                                actionButton("run_fmpb","Run FMPB"),
                                actionButton("run_fgasp","Run fGasp")
                            ),

                            # Show a plot of the generated distribution
                            mainPanel(
                                box(title = "résultats des factorisations",
                                    tableOutput("out_all")),
                                box(plotOutput("visuMat"))
                            )
                        )
                    )
            ) ,

            #### ychart ####
            tabItem(tabName = "ychart",
                    fluidPage(
                        shinyjs::useShinyjs(),

                        # Application title

                        # Sidebar with a slider input for number of bins
                        sidebarLayout(
                            sidebarPanel(
                                uiOutput("secondSelection"),
                                textOutput("param_all")
                            ),

                            # Show a plot of the generated distribution
                            mainPanel(
                                box(plotOutput("plotres_fmn")),
                                box(plotOutput("plotres_fmp")),
                                box(plotOutput("plotres_fmpb")),
                                box(plotOutput("plotres_fgasp"))
                            )
                        )
                    )
            ),

            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                                plotlyOutput("visu")
                            )
            ),

            # Second tab content
            tabItem(tabName = "fmp",
                    fluidRow(
                        box(plotlyOutput("out_fmp"))
                    )
            ),

            # thirdtab content
            tabItem(tabName = "fmpb",
                    fluidRow(
                        box(plotlyOutput("out_fmpb"))
                    )
            )

         )
    )
)

# Define UI for application that draws a histogram
# shinyUI(fluidPage(
# 
#     # Application title
#     titlePanel("Visualisation des données"),
# 
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#         sidebarPanel(
#             selectInput("kstar", label = "Nombre d'échantillons avec NA",
#                         choices = c(1,2), selected = 1),
#             sliderInput("nstar", label = "Nombre de site NA",
#                         min = 100, max = 900, value = 100, step = 100),
#             actionButton("run_plot",
#                          "Run figure"),
#             sliderInput("rank", label = "Rang de la factorisation",
#                         min = 2, max = K, value = 2, step = 1),
#             actionButton("run_fmn","Run FMN")
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#             plotlyOutput("visu"),
#             textOutput("out_fmn"),
#             tableOutput("out_all")
#         )
#     )
# ))
