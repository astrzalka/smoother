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
