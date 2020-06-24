#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  library(dplyr)
  # List the first level callModules here
  # wczytanie danych z  jednego lub dwóch plików, dodanie im grupy i nazw kolumn
  dane <- reactive({
    
    if(input$example == TRUE){
      
      d <- przyklad
      
      return(d)
      
    } else {
      
      inFile_1 <- input$dane_1
      inFile_2 <- input$dane_2
      # jak plik pierwszy pusty zwraca null
      if (is.null(inFile_1)){
        return(NULL)
      } else {
        
        d <- read.table(inFile_1$datapath, header=as.logical(input$header), sep = input$sep, quote = "\"")
        d$grupa <- 'x'
        if(ncol(d) == 4){
          d <- d[,-1]
        }
        
      }
      
      if (is.null(inFile_2)){
        # gdy nie ma pliku dwa to zwraca zawartość pierwszego
        colnames(d) <- c('dlugosc', 'int', 'grupa')
        #d$dlugosc <- as.numeric(d$dlugosc)
        return(d)
        
      } else {
        # jak jest plik dwa to łączy je razem, grupy x , y
        d_2 <- read.table(inFile_2$datapath, header=as.logical(input$header), fill = TRUE, sep = input$sep, quote = "\"", stringsAsFactors = FALSE)
        d_2$grupa <- 'y'
        if(ncol(d_2) == 4){
          d_2 <- d_2[,-1]
        }
        d <- rbind(d, d_2)
        colnames(d) <- c('dlugosc', 'int', 'grupa')
        return(d)
      }
    }
  })
  
  # obróbka danych
  dane_final <- reactive({
    
    data <- dane()
    # dodanie indeksów koniecznych do normalizacji i rysowania wykresów
    data <- data %>% dplyr::group_by(grupa) %>% dplyr::mutate(ind = dodaj_ind(dlugosc)) %>%
      dplyr::ungroup() %>% dplyr::mutate(ind2 = dodaj_ind(dlugosc))
    
    # normalizacja dlugości i intensywności - odjęcie minimum i dzielenie przez maksimum dla każdej komórki
    if(input$procent_x == TRUE){
      data <- data %>% dplyr::group_by(grupa, ind) %>% dplyr::mutate(dlugosc = dlugosc - min(dlugosc),
                                                                     dlugosc = dlugosc/max(dlugosc))
    }
    if(input$procent_y == TRUE){
      data <- data %>% dplyr::group_by(grupa, ind) %>% dplyr::mutate(int = int - min(int),
                                                                     int = int/max(int))
    }
    
    return(data)
    
  })
  
  # rysowanie wykresu
  wykresInput <- reactive({
    
    #envir <- environment()
    
    data <- dane_final()
    
    p <- ggplot2::ggplot(data, ggplot2::aes(x = dlugosc, y = int, color = grupa))
    
    # wykres 10 pierwszych komórek
    if(input$jaki_wykres == 'example'){
      
      p <- ggplot2::ggplot(data %>% filter(ind <= 10), ggplot2::aes(x = dlugosc, y = int, color = grupa))    
      p <- p + ggplot2::geom_line()+
        ggplot2::facet_wrap(~ind)
      
    }
    
    # linia trendu + linie
    if(input$jaki_wykres == 'linie'){
      p <- p + ggplot2::geom_line(ggplot2::aes(group = ind2), alpha = input$alpha)
    }
    
    # linia trendu + punkty
    if(input$jaki_wykres == 'punkty'){
      p <- p + ggplot2::geom_point(alpha = input$alpha)
    }
    # sama linia trendu
    if(input$jaki_wykres %in% c('linie', 'punkty', 'trend')){
      
      if(input$jaki_trend == 'loess'){
        p <- p + ggplot2::stat_smooth(size = input$trend_size)
      } else {
        
        data %>% dplyr::group_by(grupa, dlug_cut = cut(dlugosc, breaks = seq(0, 1, by = 0.02))) %>%
          dplyr::summarise(mean_int = mean(int)) %>%
          dplyr::mutate(dlug_cut = seq(0,1, by=0.02)) %>%
          dplyr::mutate(roll_int = zoo::rollmean(x=mean_int, k = 5, fill = NA)) -> data_sum
        
        if(input$jaki_trend == 'mean'){
          
          p <- p + ggplot2::geom_line(data = data_sum, ggplot2::aes(y = mean_int,
                                                                    x = dlug_cut),
                                      size = input$trend_size)
          
        }
        
        if(input$jaki_trend == 'rollmean'){
          
          p <- p + ggplot2::geom_line(data = data_sum, ggplot2::aes(y = roll_int,
                                                                    x = dlug_cut),
                                      size = input$trend_size)
          
        }
        
      }
      
      
    }
    # białe tło
    p <- p + ggplot2::theme_bw()
    # nazwy osi
    p <- p + ggplot2::xlab(input$os_x_nazwa)+
      ggplot2::ylab(input$os_y_nazwa)
    
    # zamiana podanych nazw grup na wektor nazw
    moje_grupy <- sub(' ', '', unlist(stringr::str_split(input$legenda_grupy, ',')))
    
    # jak nie ma podanych kolorów to używa standardowych dla ggplot
    if(input$wlasne_kolory == ''){
      p <- p + ggplot2::scale_color_discrete(name = input$legenda_nazwa,
                                             labels = moje_grupy) 
      
      # podane nazwy kolorów zmieniane na wektor i używane w scale_color_manual
    } else {
      my_colors <- sub(' ', '', unlist(stringr::str_split(input$wlasne_kolory, ',')))
      p <- p + ggplot2::scale_color_manual(name = input$legenda_nazwa, values = my_colors,
                                           labels = moje_grupy)
    }
    
    
    # czy dodać legendę?
    if(input$czy_legenda == FALSE){
      p <- p + ggplot2::theme(legend.position = 'none')
    }
    
    # wyświetla wykres
    print(p)
  })
  
  # funckja pokazujące wykres w aplikacji
  output$wykres <- renderPlot({
    if (is.null(input$dane_1) & input$example == FALSE)
      return(NULL)
    print(wykresInput())
  })
  
  # funkcja do zapisania wyrkesu jako .png
  output$download_wykres <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res, width = input$width, input$height, unit = 'cm')
      print(wykresInput())
      dev.off()
    })
  
}
