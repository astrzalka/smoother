#' read_in_data_txt UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_read_in_data_txt_ui <- function(id){
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
                 "\t", inline = TRUE),
    tableOutput("contents")
  )
}

#' read_in_data_txt Server Function
#'
#' @noRd 
mod_read_in_data_txt_server <- function(input, output, session, vars){
  #ns <- session$ns
  
  userFile_1 <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$dane_1, message = FALSE))
    input$dane_1
  })
  
  # wczytanie danych z  jednego lub dwóch plików, dodanie im grupy i nazw kolumn
  dane <- reactive({
    
    # inFile_1 <- vars$dane_1()
    # inFile_2 <- vars$dane_2()
    # jak plik pierwszy pusty zwraca null
    d <- read.table(userFile_1()$datapath, header=as.logical(input$header), sep = input$sep, quote = "\"")
    #d <- data.frame(x = 1:5)
    #return(d)
    # if (is.null(inFile_1)){
    #   return(NULL)
    # } else {
    #   
    #d <- read.table(inFile_1$datapath, header=as.logical(vars$header()), sep = vars$sep(), quote = "\"")
    
    d$grupa <- "x"
    # if(ncol(d) == 4){
    #   d <- d[,-1]
    # }
    colnames(d) <- c('dlugosc', 'int', 'grupa')
    return(d)
    #   
    # }
    # 
    # if (is.null(inFile_2)){
    #   # gdy nie ma pliku dwa to zwraca zawartość pierwszego
    #   colnames(d) <- c('dlugosc', 'int', 'grupa')
    #   #d$dlugosc <- as.numeric(d$dlugosc)
    #   return(d)
    #   
    # } else {
    #   # jak jest plik dwa to łączy je razem, grupy x , y
    #   d_2 <- read.table(inFile_2$datapath, header=as.logical(vars$header()), sep = vars$sep(), quote = "\"", stringsAsFactors = FALSE)
    #   d_2$grupa <- 'y'
    #   if(ncol(d_2) == 4){
    #     d_2 <- d_2[,-1]
    #   }
    #   d <- rbind(d, d_2)
    #   colnames(d) <- c('dlugosc', 'int', 'grupa')
    #   return(d)
    # }
  })
  # return(dane = list(reactive(dane())))
  
  output$contents <- renderTable({
    head(dane())
  })
  
  return(dane)
}

## To be copied in the UI
# mod_read_in_data_txt_ui("read_in_data_txt_ui_1")

## To be copied in the server
# callModule(mod_read_in_data_txt_server, "read_in_data_txt_ui_1")

