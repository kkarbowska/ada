# CZĘŚĆ TRZECIA
# wczytanie danych - odrazu zmiana nazw kolumn ponieważ R nie odczytał polskich znaków
data <- read.csv("ankieta.csv", sep = ';', col.names = c('DZIAL', 'STAZ', 'CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PLEC', 'WIEK'))

data <- data %>%
  mutate_at(vars(DZIAL, STAZ, CZY_KIER, PYT_1, PYT_2, PYT_3, PLEC), as.factor)

przedzialy_wiekowe <- c(0, 35, 45, 55, Inf)
nazwy_kategori <- c("0-35", "36-45", "46-55", "55+")
data$WIEK_KAT <- cut(data$WIEK, przedzialy_wiekowe, labels = nazwy_kategori, include.lowest = TRUE)

### Zmienna CZY_ZADOW przyjmuje następujące wartości:
### 0 - osoba jest zadowolona
### 1 - osoba jest niezadowolona

data <- mutate(data, CZY_ZADOW = ifelse(as.numeric(as.character(PYT_2)) == -2, 0,
                                        ifelse(as.numeric(as.character(PYT_2)) == -1, 0,
                                              ifelse(as.numeric(as.character(PYT_2)) == 1, 1,
                                                  ifelse(as.numeric(as.character(PYT_2)) == 2, 1, "_")))))
view(data)
data$CZY_ZADOW <- as.factor(as.integer(data$CZY_ZADOW))

str(data)
###################################################
### zadanie 6. 
### Napisz funkcje do wyznaczania realizacji przedziału ufnosci Cloppera-Pearsona. ´
### Niech argumentem wejsciowym będzie poziom ufności, liczba sukcesów i liczba prób lub ´
### poziom ufnosci i wektor danych (funkcja powinna obsługiwać oba przypadki). 

clopper_pearson_confidence_interval <- function(confidence_level, successes=NULL, trials=NULL, vector_data=NULL) {
  
  if (!is.null(vector_data)) {
    successes <- sum(vector_data)
    trials <- length(vector_data)
  }
  alpha <- 1 - confidence_level
  lower_bound <- qbeta(alpha/2, successes, trials - successes + 1)
  upper_bound <- qbeta(1 - (alpha/2), successes + 1, trials - successes)
  
  return(c(lower_bound, upper_bound))
}

### zadanie 7. 
### Korzystając z funkcji napisanej w zadaniu 6. wyznacz realizacje przedziałów
### ufnosci dla prawdopodobienstwa, że pracownik jest zadowolony z wynagrodzenia w pierwszym ˙
### badanym okresie oraz w drugim badanym okresie. Skorzystaj ze zmiennych CZY_ZADW oraz
### CZY_ZADW_2 (utwórz zmienną analogicznie jak w zadaniu 1.7). Przyjmij 1−α = 0.95.

# Na wstępie utworzymy zmienną CZY_ZADOW_2 analogicznie jak tworzyliśmy zmienną CZY_ZADOW
data <- mutate(data, CZY_ZADOW_2 = ifelse(as.numeric(as.character(PYT_3)) == -2, 0,
                                        ifelse(as.numeric(as.character(PYT_3)) == -1, 0,
                                               ifelse(as.numeric(as.character(PYT_3)) == 1, 1,
                                                      ifelse(as.numeric(as.character(PYT_3)) == 2, 1, "_")))))

data$CZY_ZADOW_2 <- as.factor(as.integer(data$CZY_ZADOW_2))


sukces <- count(subset(data, as.integer(as.factor(data$CZY_ZADOW)) == 1 &  as.integer(as.factor(data$CZY_ZADOW_2)) == 1)) %>% as.integer()
print(sukces)

# Jest 74 pracowników spośród 200, którzy byli zadowoleni z wynagrodzenia w pierwszym oraz w drugim badanym okresie. Zatem przedział ufności 
# Clopera - Pearsona na poziomie ufności 0.95 ma się następująco:
clopper_pearson_confidence_interval(confidence_level = 0.95, successes = sukces, trials = nrow(data))
 