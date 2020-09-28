#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("smoother",
               selected = "Adds trend line to a fluorescence profile",
               theme = shinythemes::shinytheme("united"),
               tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #000000;}"))
               ),
               tabPanel(
                 # Panel title
                 "Adds trend line to a fluorescence profile",
                 
                 sidebarLayout(
                   sidebarPanel(
                     checkboxInput('example', 'Load example dataset?'),
                     fileInput("dane_1", 'Choose first txt file',
                               accept=c('.txt')),
                     fileInput("dane_2", 'Choose second txt file',
                               accept=c('.txt')),
                     radioButtons('header', 'Do datasets contain headers?', choices = list("Yes" = TRUE, "No" = FALSE), 
                                  inline = TRUE),
                     radioButtons('sep', 'Separator?',
                                  c(Comma=',',
                                    Semicolon=';',
                                    Tabulator='\t', 
                                    Space = " "),
                                  "\t", inline = TRUE),
                     radioButtons("procent_x", "Change cell length into percent?", 
                                  choices = list("Yes" = TRUE, "No" = FALSE), 
                                  selected = TRUE, inline = TRUE),
                     radioButtons("procent_y", "Change fluorescence intensity into percent?", 
                                  choices = list("Yes" = TRUE, "No" = FALSE), 
                                  selected = TRUE, inline = TRUE),
                     radioButtons('jaki_wykres', "Choose plot type:", 
                                  choices = list('First 10 cells' = 'example', 
                                                 'All profiles as lines with a trend line' = 'linie',
                                                 'All profiles as points with a trend line' = 'punkty',
                                                 'Only trend lines' = 'trend')),
                     radioButtons('jaki_trend', "How should trend line be calculated?", 
                                  choices = list('Loess (from stat_smooth)' = 'loess',
                                                 'Mean' = 'mean',
                                                 'Rolling mean' = 'rollmean'),
                                  selected = 'loess', inline = TRUE),
                     numericInput("alpha", "Choose alpha value for points or lines", 0.3, 
                                  min = 0, max = 1, step = 0.1),
                     numericInput("trend_size", "Choose size of trend line", 1, 
                                  min = 0, max = 10, step = 0.1),
                     textInput('wlasne_kolory', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format',  ''),
                     radioButtons("czy_legenda", "Add legend?", 
                                  choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                  selected = "TRUE", inline = TRUE),
                     textInput('legenda_nazwa', 'Legend title', 'Protein'),
                     textInput('legenda_grupy', 'Please provide names for legend groups separated by commas', 'x, y'),
                     textInput('os_x_nazwa', 'X axis name', '% of cell length'),
                     textInput('os_y_nazwa', 'Y axis name', '% of fluorescence intensity'),
                     downloadButton('download_wykres', 'Download plot'),
                     numericInput('width', 'Plot width [cm]', 20, min = 5, max = 25),
                     numericInput('height', 'Plot height [cm]', 14, min = 5, max = 25),
                     numericInput('res', 'Resolution', 200, min = 100, max = 500)
                     
                     
                   ),
                   
                   # Show a plot 
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Plot",
                                          plotOutput("wykres", height = "700px")
                                 ),
                                 #br(),
                                 #hr(),
                                 #br(),
                                 tabPanel("Data",
                                          h4("First six lines of uploaded dataset, group column and column names are added automatically"),
                                          tableOutput("dane")
                                 )
                     )
                   )
                 )
               ),
               tabPanel("kmeans",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons('kmeans', 'Choose algorithm?', 
                                         choices = list('kmeans' = 'kmeans', 'fuzzy kmeans' = 'fuzzy'), 
                                         inline = TRUE),
                            numericInput('n_clusters', "Number of groups", min = 2, step = 1, value = 2),
                            downloadButton('download_wykres_kmeans', 'Download plot'),
                            numericInput('width_kmeans', 'Plot width [cm]', 20, min = 5, max = 25),
                            numericInput('height_kmeans', 'Plot height [cm]', 14, min = 5, max = 25),
                            numericInput('res_kmeans', 'Resolution', 200, min = 100, max = 500)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Plot - trend line',
                                       plotOutput("wykres_kmeans", height = "700px")
                              ),
                              tabPanel('Plot - groups',
                                       plotOutput("wykres_cluster", height = "500px"),
                                       plotOutput("wykres_grupy", height = '300px')
                              ),
                              tabPanel('Summary',
                                       verbatimTextOutput('podsumowanie_cluster')
                                       ),
                              tabPanel('Data',
                                       tableOutput("dane_kmeans")
                              )
                            )
                          )
                        )
               )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'smoother'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

