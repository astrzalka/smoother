#' normalize_data_fluorescence UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_normalize_data_fluorescence_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(ns("procent_x"), "Czy zmienić dlugość komórki na procenty", 
                 choices = list("Tak" = TRUE, "Nie" = FALSE), 
                 selected = TRUE, inline = TRUE),
    radioButtons(ns("procent_y"), "Czy zmienić intensywność fluorescencji na procenty", 
                 choices = list("Tak" = TRUE, "Nie" = FALSE), 
                 selected = TRUE, inline = TRUE)
 
  )
}
    
#' normalize_data_fluorescence Server Function
#'
#' @noRd 
mod_normalize_data_fluorescence_server <- function(input, output, session, data){
  #ns <- session$ns
  # obróbka danych
  # Uzupełnia indeksy w danych
  # Zakłada, że pojawienie się zera w wejściowym wektorze oznacza kolejną analizowaną klatkę
  dodaj_ind <- function(wektor){
    # liczba elementów w danych
    n <- length(wektor)
    # j - będą kolejne numery klatek
    j <- 0
    # wektor na indeksy
    nowy_wektor <- numeric(length = n)
    for (i in 1:n){
      # zwiększamy j jak pojawi się zero
      if (wektor[i] == 0) {j <- j+1}
      # zamieniamy wartość w wektorze indeksów
      nowy_wektor[i] <- j
    }
    return(nowy_wektor)
  }
  
  dane_final <- reactive({
    
    data <- data()
    # dodanie indeksów koniecznych do normalizacji i rysowania wykresów
    data <- dplyr::group_by(data, grupa)
    data <- dplyr::mutate(data, ind = dodaj_ind(dlugosc)) 
       #dplyr::ungroup() %>% 
       #dplyr::mutate(ind2 = dodaj_ind(dlugosc))
    
    # # normalizacja dlugości i intensywności - odjęcie minimum i dzielenie przez maksimum dla każdej komórki
    # if(input$procent_x == TRUE){
    #   data <- data %>% dplyr::group_by(grupa, ind) %>% dplyr::mutate(dlugosc = dlugosc - min(dlugosc),
    #                                                    dlugosc = dlugosc/max(dlugosc))
    # }
    # if(input$procent_y == TRUE){
    #   data <- data %>% dplyr::group_by(grupa, ind) %>% dplyr::mutate(int = int - min(int),
    #                                                    int = int/max(int))
    # }
    # 
    return(data)
    
  })
  
  return(dane_final)
}
    
## To be copied in the UI
# mod_normalize_data_fluorescence_ui("normalize_data_fluorescence_ui_1")
    
## To be copied in the server
# callModule(mod_normalize_data_fluorescence_server, "normalize_data_fluorescence_ui_1")
 
