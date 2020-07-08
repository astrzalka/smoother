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
               selected = "Dodaje linię trendu do wykresu fluorescencji",
               theme = shinythemes::shinytheme("united"),
               tags$head(
                 tags$style(HTML("hr {border-top: 1px solid #000000;}"))
               ),
               tabPanel(
                 # Panel title
                 "Dodaje linię trendu do wykresu fluorescencji",
                 
                 sidebarLayout(
                   sidebarPanel(
                     checkboxInput('example', 'Czy chcesz załadować przykładowe dane?'),
                     fileInput("dane_1", 'Wybierz pierwszy plik .txt',
                               accept=c('.txt')),
                     fileInput("dane_2", 'Wybierz drugi plik .txt',
                               accept=c('.txt')),
                     radioButtons('header', 'Czy dane mają nagłówki?', choices = list("Tak" = TRUE, "Nie" = FALSE), 
                                  inline = TRUE),
                     radioButtons('sep', 'Separator?',
                                  c(Przecinek=',',
                                    Średnik=';',
                                    Tabulator='\t', 
                                    Spacja = " "),
                                  "\t", inline = TRUE),
                     radioButtons("procent_x", "Czy zmienić dlugość komórki na procenty", 
                                  choices = list("Tak" = TRUE, "Nie" = FALSE), 
                                  selected = TRUE, inline = TRUE),
                     radioButtons("procent_y", "Czy zmienić intensywność fluorescencji na procenty", 
                                  choices = list("Tak" = TRUE, "Nie" = FALSE), 
                                  selected = TRUE, inline = TRUE),
                     radioButtons('jaki_wykres', "Jaki wykres narysować?", 
                                  choices = list('Pierwsze 10 komórek' = 'example', 
                                                 'Wszystkie nałożone jako linie + linia trendu' = 'linie',
                                                 'Wszystkie nałożone jako punkty + linia trendu' = 'punkty',
                                                 'Tylko linie trendu' = 'trend')),
                     radioButtons('jaki_trend', "Jak wyliczyć linię trendu?", 
                                  choices = list('Loess (ze stat_smooth)' = 'loess',
                                                 'Średnia' = 'mean',
                                                 'Średnia ruchoma' = 'rollmean'),
                                  selected = 'loess', inline = TRUE),
                     numericInput("alpha", "Podaj wartość alpha dla linii/punktów na wykresie", 0.3, 
                                  min = 0, max = 1, step = 0.1),
                     numericInput("trend_size", "Podaj grubość linii trendu", 1, 
                                  min = 0, max = 10, step = 0.1),
                     textInput('wlasne_kolory', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000',  ''),
                     radioButtons("czy_legenda", "Czy dodać legendę do wykresu", 
                                  choices = list("Tak" = "TRUE", "Nie" = "FALSE"), 
                                  selected = "TRUE", inline = TRUE),
                     textInput('legenda_nazwa', 'Tytuł legendy', 'Białko'),
                     textInput('legenda_grupy', 'Podaj nazwy grup do legendy oddzielone przecinkiem', 'x, y'),
                     textInput('os_x_nazwa', 'Nazwa osi X', '% długości komórki'),
                     textInput('os_y_nazwa', 'Nazwa osi Y', '% intensywności fluorescencji'),
                     downloadButton('download_wykres', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                     numericInput('width', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                     numericInput('height', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                     numericInput('res', 'Rozdzielczość', 200, min = 100, max = 500)
                     
                     
                   ),
                   
                   # Show a plot 
                   mainPanel(
                     tabsetPanel(type = "tabs",
                                 tabPanel("Wykres",
                                          plotOutput("wykres", height = "700px")
                                 ),
                                 #br(),
                                 #hr(),
                                 #br(),
                                 tabPanel("Dane",
                                          h4("Pierwsze 6 wierszy wczytanego pliku, kolumna grupa i nazwy kolumn są dodawane automatycznie"),
                                          tableOutput("dane")
                                 )
                     )
                   )
                 )
               ),
               tabPanel("kmeans",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons('kmeans', 'Jaki algorytm zastosować?', 
                                         choices = list('kmeans' = 'kmeans', 'fuzzy kmeans' = 'fuzzy'), 
                                         inline = TRUE),
                            numericInput('n_clusters', "Podaj liczbę grup", min = 2, step = 1, value = 2),
                            downloadButton('download_wykres_kmeans', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                            numericInput('width_kmeans', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                            numericInput('height_kmeans', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                            numericInput('res_kmeans', 'Rozdzielczość', 200, min = 100, max = 500)
                          ),
                          mainPanel(
                            tabsetPanel(
                              tabPanel('Wykres - linia trendu',
                                       plotOutput("wykres_kmeans", height = "700px")
                              ),
                              tabPanel('Wykres - grupy',
                                       plotOutput("wykres_cluster", height = "500px"),
                                       plotOutput("wykres_grupy", height = '300px')
                              ),
                              tabPanel('Podsumowanie',
                                       verbatimTextOutput('podsumowanie_cluster')
                                       ),
                              tabPanel('Dane',
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

