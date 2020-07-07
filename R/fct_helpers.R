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

# inputem będą dane_final z shiny, które mają 5 kolumn: dlugosc, int, grupa, ind, ind2
fluorescence_kmeans <- function(data_input, n_clusters, method = 'kmeans'){
  
  data_input %>% dplyr::group_by(grupa, ind, dlug_cut = cut(dlugosc, breaks = seq(0, 1.01, by = 0.05))) %>%
    dplyr::summarise(mean_int = mean(int)) %>%
    dplyr::filter(!is.na(dlug_cut)) %>%
    dplyr::mutate(dlug_proc = seq(0,0.99, by=0.05)) %>%
    dplyr::select(-dlug_cut) -> data
  
  grupy <- unique(data$grupa)
  
  wyniki <- list()
  
  # for(i in 1:length(grupy)){
  #   
  #   data_temp <- data %>% dplyr::filter(grupa == grupy[i]) 
  #   
  #   data_temp_input <- data_input %>% dplyr::filter(grupa == grupy[i]) 
  #   
  #   data_temp %>% 
  #     ungroup() %>%
  #     select(-grupa, -dlug_proc) %>%
  #     tidyr::pivot_wider(names_from = ind, values_from = mean_int) %>%
  #     tidyr::unnest() %>%
  #     as.matrix() %>%
  #     t() -> 
  #     data_temp_wider
  #   
  #   model <- kmeans(data_temp_wider, centers = n_clusters)
  #   
  #   clusters <- model$cluster
  #   
  #   data_temp_input$cluster <- clusters[data_temp_input$ind]
  #   
  #   wyniki[[i]] <- data_temp_input
  # }
  wyniki_wider <- list()
  for(i in 1:length(grupy)){
    
    data_temp <- data %>% dplyr::filter(grupa == grupy[i])
    
    #data_temp_input <- data_input %>% dplyr::filter(grupa == grupy[i])
    
    data_temp %>%
      ungroup() %>%
      select(-grupa, -dlug_proc) %>%
      tidyr::pivot_wider(names_from = ind, values_from = mean_int) %>%
      tidyr::unnest() %>%
      as.matrix() %>%
      t() ->
      data_temp_wider
    
    wyniki_wider[[i]] <- data_temp_wider
    
  }
  
  wyniki_wider <- do.call(cbind, wyniki_wider)
  
  if(method == 'kmeans'){
    
    model <- kmeans(wyniki_wider, centers = n_clusters)
    clusters <- model$cluster
  }
  
  if(method == 'fuzzy'){
    
    model <- ppclust::fcm(wyniki_wider, centers = n_clusters)
    clusters <- model$cluster
  }
  
  
  
  data_input$cluster <- clusters[data_input$ind]
  
  wyniki_tabela <- data_input
  
  #wyniki_tabela <- do.call(rbind, wyniki)
  
  return(list(wyniki_tabela, model, wyniki_wider))
  
}
