#' plot_smoothed_fluorescence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_plot_smoothed_fluorescence_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns('jaki_wykres'), "Jaki wykres narysować?", 
                 choices = list('Pierwsze 10 komórek' = 'example', 
                                'Wszystkie nałożone jako linie + linia trendu' = 'linie',
                                'Wszystkie nałożone jako punkty + linia trendu' = 'punkty',
                                'Tylko linie trendu' = 'trend')),
    numericInput(ns("alpha"), "Podaj wartość alpha dla linii/punktów na wykresie", 0.3, 
                 min = 0, max = 1, step = 0.1),
    numericInput(ns("trend_size"), "Podaj grubość linii trendu", 1, 
                 min = 0, max = 10, step = 0.1),
    textInput(ns('wlasne_kolory'), 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000',  ''),
    radioButtons(ns("czy_legenda"), "Czy dodać legendę do wykresu", 
                 choices = list("Tak" = "TRUE", "Nie" = "FALSE"), 
                 selected = "TRUE", inline = TRUE),
    textInput(ns('legenda_nazwa'), 'Tytuł legendy', 'Białko'),
    textInput(ns('legenda_grupy'), 'Podaj nazwy grup do legendy oddzielone przecinkiem', 'x, y'),
    textInput(ns('os_x_nazwa'), 'Nazwa osi X', '% długości komórki'),
    textInput(ns('os_y_nazwa'), 'Nazwa osi Y', '% intensywności fluorescencji'),
    plotOutput(ns("wykres"), height = "600px")
 
  )
}
    
#' plot_smoothed_fluorescence Server Function
#'
#' @noRd 
mod_plot_smoothed_fluorescence_server <- function(input, output, session, dane_final){
  ns <- session$ns
  # rysowanie wykresu
  wykresInput <- reactive({
    
    #envir <- environment()
    
     data <- dane_final()
    # 
    #data <- data.frame(dlugosc = 1:10)
    
    #p <- ggplot2::ggplot(data, ggplot2::aes(x = dlugosc))
    #p <- p + ggplot2::geom_histogram()
    
     p <- ggplot2::ggplot(data, ggplot2::aes(x = dlugosc, y = int, color = grupa))
    # 
     # wykres 10 pierwszych komórek
     if(input$jaki_wykres == 'example'){
       
       p <- ggplot2::ggplot(data %>% filter(ind <= 10)
                            , ggplot2::aes(x = dlugosc, y = int, color = grupa))    
       p <- p + ggplot2::geom_line()#+
         #ggplot2::facet_wrap(~ind)
       
     }
    # 
    # # linia trendu + linie
    # if(input$jaki_wykres == 'linie'){
    #   p <- p + ggplot2::geom_line(aes(group = ind2), alpha = input$alpha)
    # }
    # 
    # # linia trendu + punkty
    # if(input$jaki_wykres == 'punkty'){
    #   p <- p + ggplot2::geom_point(alpha = input$alpha)
    # }
    # # sama linia trendu
    # if(input$jaki_wykres %in% c('linie', 'punkty', 'trend')){
    #   p <- p + ggplot2::stat_smooth(size = input$trend_size)
    # }
    # # białe tło
    # p <- p + ggplot2::theme_bw()
    # # nazwy osi
    # p <- p + ggplot2::xlab(input$os_x_nazwa)+
    #   ggplot2::ylab(input$os_y_nazwa)
    # 
    # # zamiana podanych nazw grup na wektor nazw
    # moje_grupy <- sub(' ', '', unlist(stringr::str_split(input$legenda_grupy, ',')))
    # 
    # # jak nie ma podanych kolorów to używa standardowych dla ggplot
    # if(input$wlasne_kolory == ''){
    #   p <- p + ggplot2::scale_color_discrete(name = input$legenda_nazwa,
    #                                 labels = moje_grupy) 
    #   
    #   # podane nazwy kolorów zmieniane na wektor i używane w scale_color_manual
    # } else {
    #   my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory, ',')))
    #   p <- p + ggplot2::scale_color_manual(name = input$legenda_nazwa, values = my_colors,
    #                               labels = moje_grupy)
    # }
    # 
    # 
    # # czy dodać legendę?
    # if(input$czy_legenda == FALSE){
    #   p <- p + ggplot2::theme(legend.position = 'none')
    # }
    
    # wyświetla wykres
     return(p)
  })
  
  # funckja pokazujące wykres w aplikacji
  output$wykres <- renderPlot({
    #if (is.null(input$dane_1))
    #  return(NULL)
    #print(wykresInput())
    wykresInput()

  })
  
}
    
## To be copied in the UI
# mod_plot_smoothed_fluorescence_ui("plot_smoothed_fluorescence_ui_1")
    
## To be copied in the server
# callModule(mod_plot_smoothed_fluorescence_server, "plot_smoothed_fluorescence_ui_1")
 
