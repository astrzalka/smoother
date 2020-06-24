#' varselect UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_varselect_ui <- function(id){
  ns <- NS(id)
  tagList(
    fileInput(ns("dane_1"), 'Wybierz pierwszy plik .txt',
              accept=c('.txt')),
    fileInput(ns("dane_2"), 'Wybierz drugi plik .txt',
              accept=c('.txt')),
    radioButtons(ns('header'), 'Czy dane mają nagłówki?', choices = list("Tak" = TRUE, "Nie" = FALSE), 
                 inline = TRUE),
    radioButtons(ns('sep'), 'Separator?',
                 c(Przecinek=',',
                   Średnik=';',
                   Tabulator='\t', 
                   Spacja = " "),
                 "\t", inline = TRUE)
    
  )
}
    
#' varselect Server Function
#'
#' @noRd 
mod_varselect_server <- function(input, output, session){
  ns <- session$ns
  #stopifnot(is.reactive(dane_1))
 return(
   list(
     dane_1 = reactive(input$dane_1),
     dane_2 = reactive(input$dane_2),
     header = reactive(input$header),
     sep = reactive(input$sep)
   )
 )
}
    
## To be copied in the UI
# mod_varselect_ui("varselect_ui_1")
    
## To be copied in the server
# callModule(mod_varselect_server, "varselect_ui_1")
 
