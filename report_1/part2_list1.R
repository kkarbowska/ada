library(tidyverse)
library(likert)
library(stats)
library(dbplyr)

katalog = dirname(normalizePath(file.choose())) # wybiera sie plik na którym chcemy pracować, i najlepiej ten w którym są dane
setwd(katalog)

data <- read.csv("ankieta.csv", sep = ';', col.names = c('DZIAL', 'STAZ', 'CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PLEC', 'WIEK'))
view(data)

data <- data %>%
  mutate_at(vars(DZIAL, STAZ, CZY_KIER, PYT_1, PYT_2, PYT_3, PLEC), as.factor)

### Część II
################# zadanie 2. ############################################################
# Zapoznaj sie z biblioteką likert i dostępnymi tam funkcjami summary oraz plot
# (wykresy typu "bar", "heat" oraz "density"), a nast˛epnie zilustruj odpowiedzi na pytanie "Jak
# bardzo zgadzasz si˛e ze stwierdzeniem, ˙ze firma pozwala na (...)?" (zmienna PYT_1) w całej
# badanej grupie oraz w podgrupach ze względu na zmienną CZY_KIER.
kolory <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854")

likert_df = likert(data[,"PYT_1", drop=FALSE])
summary(likert_df)
### Pakiet likert służy do analizy danych przedstawionych skalą Likerta.
### Funkcja summary daje krótkie podsumowanie zbioru danych. 
###  Zawiera nazwę danych których dotyczy analiza ("Item") 
### a także kolumnę "low" odpowiadającą za sumę odpowiedzi poniżej wartości neutralnej, 
### kolumnę "high" odpowiadająca za sumę odpowiedzi poniżej wartości powyżej wartości neutralnej,
## oraz kolumny "średnia" i "sd" odpowiadające odpowiednio średniej i odchyleniu standardowemu


# wykres typu bar - cała badana grupa
plot(likert_df, type = 'bar', legend.position = 'right') +
  ylab('Wartość procentowa') +
  xlab(' ') +
  ggtitle('Wykres typu bar dla zmiennej PYT_1 (cała badana grupa)',
          subtitle = 'Jak bardzo zgadzasz się ze stwierdzeniem, \n że firma pozwala na elastyczne godziny pracy \n tym samym umożliwiając zachowanie równowagi między pracą a życiem prywatnym?') +
  scale_fill_manual(values = kolory,
                    name = 'Odpowiedzi',
                    labels = c("zdecydowanie się nie zgadzam", 
                               "nie zgadzam się", "nie mam zdania", 
                               "zgadzam się", 
                               "zdecydowanie się zgadzam")) 

### Na powyższym wykresie widzimy, że większość respontentów zgadza się ze stwierdzeniem 
### że firma pozwala na elastyczne godziny pracy, tym samym umożliwiając zachowanie równowagi
### między pracą a życiem prywatnym. Zdecydowanie mniej bo aż tylko 16% nie opowiada się za tym 
### zdaniem. Można przypuszczać, że pracownicy są zadowoleni z takiego trybu pracy. 

# wykres typu heat - cała badana grupa
plot(likert_df, type = 'heat', low.color = "#F3FFFB", high.color = "#66c2a5") + 
  ggtitle('Wykres typu heat dla zmiennej PYT_1 (cała badana grupa)',
          subtitle = '"Jak bardzo zgadzasz się ze stwierdzeniem,  że firma pozwala na elastyczne godziny pracy \n tym samym umożliwiając zachowanie równowagi między pracą a życiem prywatnym?"') +
  scale_y_discrete(labels = c("zdecydowanie się \n nie zgadzam", 
                              "nie zgadzam się", 
                              "nie mam zdania", 
                              "zgadzam się", 
                              "zdecydowanie \n się zgadzam", 
                              "Mean(SD)")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
### Na powyższym rynku mamy wykres typu heat. Możemy poznać procentowy rozkład odpowiedzi na pytanie
### " Jak bardzo zgadzasz się ze stwierdzeniem, że firma pozwala na elastyczne godziny pracy, 
### tym samym umożliwiając zachowanie równowagi między pracą a życiem prywatnym?". Możemy zauważyć 
### tak na poprzednim wykresie, że pracownicy są zgodni z tym twierdzeniem.
### Dodatkowo mamy pokazaną średnią odpowiedzi, która wynosi 3,56.

# wykres typu density - cała badana grupa
plot(likert_df, type = 'density', facet=TRUE) +
  scale_x_continuous(breaks=c(1,2,3, 4, 5),
                     labels=c("zdecydowanie się \n nie zgadzam", 
                              "nie zgadzam się", 
                              "nie mam zdania", 
                              "zgadzam się", 
                              "zdecydowanie \n się zgadzam")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
### Wykres przedstawia rozkład zmiennej PYT_1. Podobnie jak w poprzednich wykresach może
### wywnioskować, że jest więcej osób, które się zgadzają z stwierdzeniem.


## podział na podgrupy czy_kier
subgroup_likert <- likert(data[,"PYT_1", drop=FALSE], grouping = data$CZY_KIER)

# wykres typu bar - podział na podgrupy -  tylko to potrzeba 
plot(subgroup_likert, type = "bar") +
  ylab('Wartość procentowa') +
  xlab('Podział na zmienną CZY_KIER') +
  ggtitle('Wykres typu heat dla zmiennej PYT_1 (podział na podgrupy)',
          subtitle = '"Jak bardzo zgadzasz się ze stwierdzeniem,  że firma pozwala na elastyczne godziny pracy \n tym samym umożliwiając zachowanie równowagi między pracą a życiem prywatnym?"') +
  scale_fill_manual(values = kolory,
                    name = 'Odpowiedzi',
                    labels = c("zdecydowanie się nie zgadzam", 
                               "nie zgadzam się", 
                               "nie mam zdania", 
                               "zgadzam się", 
                               "zdecydowanie się zgadzam")) + 
  theme(legend.position = 'right')

### Na powyższym wykresie mamy podział na osoby które są menadżerem ('TAK')
### oraz nie piastują tego stanowiska ('NIE'). Możemy zauważyć, że 
### więcej pracowników firmy, którzy nie mają stanowiska menadżerskiego niż tych które je mają
### jest zadowolona z elastycznych godzin pracy. Może być to związane z tym, że menadżer posiada
### z pewnością obowiązków i granica pomiędzy życiem prywatnym a zawodowym może się zacierać.

################# zadanie 3. ##################################
# Zapoznaj sie z funkcją sample z biblioteki stats, a następnie wylosuj próbkę o
# liczności 10% wszystkich rekordów z pliku "ankieta.csv" w dwóch wersjach: ze zwracaniem
# oraz bez zwracania.

n <- nrow(data) * 0.1 # 10% z 200 (liczność wszystkich rekordów)
# musimy wylosować wiersze 

# losowanie bez zwracania
which_row_false <- sample(c(1:nrow(data)), size = n, replace = FALSE)
view(data[which_row_false,])

# losowanie ze zwracaniem 
which_row_true <- sample(c(1:nrow(data)), size = n, replace = TRUE)
view(data[which_row_true,])


################ zadanie 4. #####################################
# Zaproponuj metode symulowania zmiennych losowych z rozkładu dwumianowego.
# Napisz funkcje do generowania realizacji, a nastepnie zaprezentuj jej działanie porównujac
# wybrane teoretyczne i empiryczne charakterystyki dla przykładowych wartosci paramertów
# rozkładu: n i p.

### Będziemy symulować zmienne losowe z rozkładu dwumianowego metodą odwrotnej 
### dystrybuanty

set.seed(123)

generate_binom <- function(n, p, N){
  U <- runif(N)
  p0 <- (1 - p)^n 
  Cprob <- p0
  
  for (i in 0:n) {
    if (i == 0) {
      U[U <= Cprob] <- n + 1
    } else if (i == 1) {
      U[U <= Cprob] <- n + 2
    } else {
      U[U <= Cprob] <- i
    }
    
    p0 <- ((n - i) / (i + 1)) * (p / (1 - p)) * p0
    Cprob <- Cprob + p0
  }
  
  U[U == n + 1] <- 0
  U[U == n + 2] <- 1
  
  return(U)}

# p = {0.1, 0.5, 0.9}

comparison_cdfs_binom <- function(n=20, p, size=1){
  
  sample <- generate_binom(n, p, size)
  empirical_cdf <- ecdf(sample)
  x_values <- 0:n
  
  plot(empirical_cdf, main = "Porównanie dystrybuant empirycznej z teoretyczną", 
       col = "blue", lwd = 2)
  curve(pbinom(x, n, p), add = TRUE, col = "red", lwd = 2, lty = 2, n = 1000)
  legend("bottomright", legend = c("Empiryczna", "Teoretyczna"), 
         col = c("blue", "red"), lty = 1:2, lwd = 2)    
  }

# przykładowo dla p=0.1
comparison_cdfs_binom(n=20, 0.1, size=1000)

comparison_pdfs_binom <- function(n, p, size=1){
  sample <- generate_binom(n, p, size)
  x_values <- 0:n
  theoretical_prob <- dbinom(x_values, n, p)
  
  values <- unique(sample)
  counts <- table(sample)
  empirical_prob <- counts/size
  
  plot(empirical_prob, type = "p", col = "blue", pch = 16, ylim = c(0, max(max(empirical_prob), max(theoretical_prob)) + 0.05),
       xlab = "x", ylab = "Probability", main = "Porównanie rozkładów prawdopodobieństwa \n rozkład dwumianowy")
  points(x_values, theoretical_prob, col = "red",  pch = 16)
  legend("topright", legend = c("Empiryczny", "Teoretyczny"), col = c("blue", "red"), pch = 16)
}

# przykładowo dla p=0.1
comparison_pdfs_binom(20, 0.1, size=1000)


comparison_mean_binom <- function(n=20, p, size=1){
  sample <- generate_binom(n, p, size)
 
  mean_empirical <- mean(sample)
  mean_theoretical <- n * p
  
  cat("\n\nŚrednia empiryczna:", mean_empirical, "\n")
  cat("Średnia teoretyczna:", mean_theoretical, "\n\n")
}

comparison_mean_binom(20, 0.1, size=1000)

comparison_var_binom <- function(n=20, p, size=1){
  sample <- generate_binom(n, p, size)
  
  var_empirical <- var(sample)
  var_theoretical <- n * p * (1 - p)
  
  cat("Wariancja empiryczna:", var_empirical, "\n")
  cat("Wariancja teoretyczna:", var_theoretical, "\n")
}

comparison_var_binom(20, 0.1, size=1000)



### Porównania dystrybuant, gęstości oraz średniej i wariancji wskazuja na poprawność
### generatora zmiennych losowych




simulateMultinomial <- function(n, probs, size) {
  res <- matrix(0, nrow = size, ncol = length(probs))
  for (k in 1:size) {
    probs <- probs / sum(probs)
    
    cumulativeProbs <- cumsum(probs)
    
    counts <- rep(0, length(probs))
    
    for (i in 1:n) {
      rand <- runif(1)
      
      for (j in 1:length(cumulativeProbs)) {
        if (rand <= cumulativeProbs[j]) {
          counts[j] <- counts[j] + 1
          break
        }
      }
    }
    res[k,] <- counts
  }
  res
}
n = 100
probs_1 <- c(0.1, 0.3, 0.4, 0.2) 
probs_2 <- c(0.5, 0.3, 0.2)
probs_3 <- c(0.1, 0.1, 0.3, 0.2, 0.3)

sample_1 <- simulateMultinomial(n, probs_1, 1000)
sample_2 <- simulateMultinomial(n, probs_1, 1000)
sample_3 <- simulateMultinomial(n, probs_1, 1000)

estimated_probs_1 = colMeans(sample_1/n)
estimated_probs_2 = colMeans(sample_2/n)
estimated_probs_3 = colMeans(sample_3/n)

sprintf("Estimated probabilities vector (0.1, 0.3, 0.4, 0.2): (%s).", toString(estimated_probs_1))
sprintf("Estimated probabilities vector (0.5, 0.3, 0.2): (%s).", toString(estimated_probs_1))
sprintf("Estimated probabilities vector (0.1, 0.1, 0.3, 0.2, 0.3): (%s).", toString(estimated_probs_1))
estimated_probs_1












