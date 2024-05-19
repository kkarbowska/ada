setwd("/Users/maciejostapiuk/projects/ada/report_2/")
## Lista 2. Część 2.###
### operacje na danych jak wcześniej 
library(ggplot2)
library(tidyverse)
library(stats)
library(dplyr)
library(binom)
library(binomCI)
library(MultinomialCI)
library(matlib)
library(ca)
library(FactoMineR)
library(factoextra)
library(DescTools)
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

data$CZY_ZADOW <- as.factor(as.integer(data$CZY_ZADOW))

data <- mutate(data, CZY_ZADOW_2 = ifelse(as.numeric(as.character(PYT_3)) == -2, 0,
                                          ifelse(as.numeric(as.character(PYT_3)) == -1, 0,
                                                 ifelse(as.numeric(as.character(PYT_3)) == 1, 1,
                                                        ifelse(as.numeric(as.character(PYT_3)) == 2, 1, "_")))))

data$CZY_ZADOW_2 <- as.factor(as.integer(data$CZY_ZADOW_2))


view(data)



##  zadanie 1. W ankiecie przedstawionej na poprzedniej liście pracownicy zostali poproszeni
##  o wyrażenie opinii na temat podejścia firmy do utrzymania równowagi między życiem
##  zawodowym a prywatnym. Wsród próbki 200 pracowników (losowanie proste ze zwracaniem)
##  uzyskano wyniki:
##    • 14 pracowników - bardzo niezadowolonych,
##    • 17 pracowników - niezadowolonych,
##    • 40 pracowników - nie ma zdania,
##    • 100 pracowników - zadowolonych,
##    • 29 pracowników - bardzo zadowolonych,
##  Na podstawie danych wyznacz przedział ufności dla wektora prawodobieństw opisującego
##  stopień zadowolenia z podejścia firmy. Przyjmij poziom ufności 0.95.


responses <-c("very dissatisfied"=14, "dissatisfied"=17, "neither sattisfied nor dissatisfied"=40, "satisfied" = 100, "highly satisfied" = 29)
res_binom_ci <- binom.confint(responses, c(200, 200, 200, 200, 200), conf.level = 0.95, methods = c("exact", "asymptotic"))
res_multinom_ci <- multinomialCI(responses, alpha = 0.05/5)




##  zadanie 2. Napisz funkcj˛e, która wyznacza warto´s´c poziomu krytycznego w nast˛epuj ˛acych
##  testach:
##    • chi-kwadrat Pearsona
##    • chi-kwadrat najwi˛ekszej wiarogodno´sci
##  słu˙z ˛acych do weryfikacji hipotezy H0 : p = p0 przy hipotezie alternatywnej H0 : p̸ = p0 na
##  podstawie obserwacji x wektora losowego X z rozkładu wielomianowego z parametrami n i p


get_p_value <-function(x,n,h_0){
  #' Calculates p_values of user's choice test: either chi-square or maximum-likelihood chi-square
  sample <-  rmultinom(1, sum(x), h_0)
  point_probs_estimator = sample/n
  
  chi_square_test_statistic <- sum((x - n*h_0)^2/(n*h_0))
  chi_square_maximum_likelihood_test_statistic <- sum((x - n*point_probs_estimator)^2/(n*point_probs_estimator))
  k <-  length(x)
  
  p_value_chi_sq <- 1-pchisq(chi_square_test_statistic, df = k-1)
  
  p_value_chi_sq_max_likelihood <- 1-pchisq(chi_square_maximum_likelihood_test_statistic, df = k-1)
  
  return(c("p_value of chi-square test" = p_value_chi_sq, "p_value of maximum-likelihood chi-square test" = p_value_chi_sq_max_likelihood ))
}

sample_multinom <-  rmultinom(1, 100, c(0.1, 0.2, 0.4, 0.2, 0.1))

null_hypothesis <-get_p_value(x = sample_multinom, n = 100, h_0 = c(0.1, 0.2, 0.4, 0.2, 0.1))

alternative_hypothesis <-  get_p_value(x = sample_multinom, n = 100, h_0 = c(0.1, 0.2, 0.2, 0.4, 0.1))

null_hypothesis

alternative_hypothesis

##  zadanie 3. Na podstawie danych z ankiety z poprzedniej listy zweryfikuj hipoteze, ze w grupie
##  pracowników zatrudnionwych w Dziale Kreatywnym rozkład odpowiedzi na pytanie dotycz ˛ace
##  podej´scia firmy do utrzymania równowagi mi˛edzy ˙zyciem zawodowym a prywatnym jest
##  równomierny, tzn. jest jednakowe prawdopodobie´nstwo, ˙ze pracownik zatrudniony w Dziale
##  Kreatywnym jest udzielił odpowiedzi "zdecydowanie si˛e nie zgadzam", "nie zgadzam si˛e", "nie
##  mam zdania", "zgadzam si˛e", "zdecydowanie si˛e zgadzam"na pytanie PYT_1. Przyjmij poziom
##  istotno´sci 0.05. Skorzystaj z funkcji napisanej w zadaniu 2.

counts <- data %>%
  filter(DZIAL == "DK") %>%
  group_by(PYT_1) %>%
  summarise(count = n())

communications_emps_responses <-  counts$count
print(communications_emps_responses)


get_p_value(communications_emps_responses, 98, c(0.2, 0.2, 0.2, 0.2, 0.2))

## Zadanie 5
## Korzystajac z testu Fishera, na poziomie istotnosci 0.05, zweryfikuj hipotez˛e, ze˙
## zmienna PŁEC´ i zmienna CZY_KIER s ˛a niezalezne. Czy na poziomie istotno ˙ sci 0 ´ .05 mozemy ˙
## wnioskowac,´ ze prawdopodobie ˙ nstwo tego, ´ ze na stanowisku kierowniczym pracuje kobieta jest ˙
## równe prawdopodobienstwu tego, ´ ze na stanowisku kierowniczym pracuje m˛e ˙ zczyzna?

ftable_PLEC_CZY_KIER <- ftable('Płeć' = data$PLEC, 'Stanowisko kierownicze' = data$CZY_KIER)
ftable_PLEC_CZY_KIER

fisher_test <- fisher.test(ftable_PLEC_CZY_KIER, conf.int = TRUE)
cat('Przedziały ufności:',fisher_test$conf.int)
cat('P-wartość:',fisher_test$p.value)

# Dla tabeli dwudzielczej test Fishera bada nam hipoteze zerową o niezależności zmiennych oraz 
# dodatkowo sprawdza nam czy proporcje pomiędzy grupami są takie same. W zadaniu
# mamy sprawdzić hipotezę zerową czy zmienne PŁEĆ i CZY_KIER są niezależne, oraz czy prawdopodobieństwo, 
# że na stanowisku pracowniczym pracuje kobieta jest równe że na stanowisku kierowniczym pracuje mężczyzna.
# Wykonany test na poziomie istotności 0.05 wskazuje p-wartość 0.6659029, zatem nie ma podstaw do odrzucenia
# hipotezy zerowej. (Co więcej przedziały ufności to [0.5299411, 3.802304].) Mianowicie nie możemy powiedzieć
# że bycie na stanowisku kierownicznym zależy od płci oraz że proporcje pomiędzy płcią na stanowisku
# kierowniczym są nie równe.

## zadanie 6. 
## Korzystajac z testu Freemana-Haltona na poziomie istotnosci 0.05 zweryfikuj
## nast˛epuj ˛ace hipotezy:
## 1. zajmowanie stanowiska kierowniczego nie zalezy od wieku (CZY_KIER oraz WIEK_KAT),
ftable_CZY_KIER_WIEK_KAT <- ftable('Stanowisko kierownicze' = data$CZY_KIER, 'Kategoria wiekowa' = data$WIEK_KAT)
ftable_CZY_KIER_WIEK_KAT

fisher_test_1 <- fisher.test(ftable_CZY_KIER_WIEK_KAT, conf.int = TRUE)
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_1$p.value)

# Rozważaliśmy tezę, że zajmowanie stanowiska kierowniczego nie jest związane z wiekiem. 
# Aby to zbadać, przeprowadziliśmy test Freedmana-Haltona, który jest równoważny funkcji fisher_test w R. 
# Warto zauważyć, że test przy poziomie istotności 0.05 nie dostarczył wystarczających dowodów 
# na odrzucenie hipotezy zerowej. P-wartość wyniosła 0.7823, co oznacza, że przekracza ona poziom istotności, 
# sugerując brak istotności statystycznej.

## 2. zajmowanie stanowiska kierowniczego nie zalezy od stazu pracy (CZY_KIER oraz STAZ),
ftable_CZY_KIER_STAZ <- ftable('Stanowisko kierownicze' = data$CZY_KIER, 'Długość stażu pracy' = data$STAZ)
ftable_CZY_KIER_STAZ

fisher_test_2 <- fisher.test(ftable_CZY_KIER_STAZ, conf.int = TRUE)
fisher_test_2
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_2$p.value)

# Hipoza zerowa zakłada że zajmowanie stanowiska kierowniczego nie zależy od stażu pracy.
# Zweryfikowaliśmy tą hipotezę, podobnie tak jak wcześniej testem Freedmana-Haltona dla tablic
# o większych wymiarach niż 2x2. P-wartość w wykonanym teście jest równa 6.538e-05 co zaprzecza hipotezie 
# zerowej, co jest dość intuicyjne, że zajmowanie stanowiska kierowniczego zależy o długości stażu pracy.


## 3. zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od zajmowanego 
## stanowiska (PYT_2 oraz CZY_KIER),
ftable_PYT2_CZY_KIER <- ftable('Poziom zadowolenia w 1. badanym okresie' = data$PYT_2, 'Stanowisko kierownicze' = data$CZY_KIER)
ftable_PYT2_CZY_KIER

fisher_test_3 <- fisher.test(ftable_PYT2_CZY_KIER, conf.int = TRUE)
fisher_test_3
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_3$p.value)

# Badaliśmy hipotezę zerową sugerującą, że zadowolenie z wynagrodzenia w pierwszym okresie badawczym 
# nie jest związane z zajmowanym stanowiskiem. Wynik testu Fishera wykazał p-wartość wynoszącą 0.0443, 
# co jest niższe od ustalonego poziomu istotności 0.05. W związku z tym istnieją podstawy do odrzucenia 
# hipotezy zerowej. Warto zauważyć, że zwykle osoby na stanowiskach kierowniczych otrzymują wyższe 
# wynagrodzenie, co może przyczynić się do większego zadowolenia z poziomu wynagrodzenia.

## 4. zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od stazu (PYT_2 oraz STAZ),
ftable_PYT2_STAZ <- ftable('Poziom zadowolenia w 1. badanym okresie' = data$PYT_2, 'Długość stażu pracy' = data$STAZ)
ftable_PYT2_STAZ


fisher_test_4 <- fisher.test(ftable_PYT2_STAZ, conf.int = TRUE)
fisher_test_4
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_4$p.value)

# Testowaliśmy hipotezę zerową która mówi, że zadowolenie w pierwszym badanym okresie nie zalezy od stazu pracy
# Wykonany test Fishera wykazał p-wartość 0.01069. co jest mniejsze od hipotezy zerowej, więc istenieją
# podstawy na odrzucenie tezy że zadowolenie w pierwszym badanym okresie nie zalezy od stazu pracy. 
# Osoby z dłuższym stażem mogą mieć większe doświadczenie i umiejętności, 
# co często przekłada się na wyższe zarobki i potencjalnie większe zadowolenie z wynagrodzenia. 
# Ponadto, wraz z upływem czasu pracownik może zdobywać awanse lub dodatkowe kwalifikacje, 
# co również może wpłynąć na jego poziom zadowolenia z wynagrodzenia. 
# Jednakże zależność ta może być różna w zależności od wielu czynników, 
# takich jak branża, lokalizacja, polityka płacowa firmy, czy nawet osobiste preferencje i wartości pracownika.




## 5. zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od płci (PYT_2 oraz PŁEC),
ftable_PYT2_PLEC <- ftable('Poziom zadowolenia w 1. badanym okresie' = data$PYT_2, 'Płeć' = data$PLE)
ftable_PYT2_PLEC

fisher_test_5 <- fisher.test(ftable_PYT2_PLEC, conf.int = TRUE)
fisher_test_5
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_5$p.value)

# Wynik testu Freedmana-Holdana wskazuje, że p-wartość wynosi 0.4758, 
# co jest znacznie wyższe od przyjętego poziomu istotności alpha = 0.05. 
# Oznacza to, że nie ma wystarczających dowodów na odrzucenie hipotezy zerowej, która zakłada, że 
# zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zależy od płci. 
# Innymi słowy, na podstawie tego testu nie możemy stwierdzić istotnego związku między płcią 
# a zadowoleniem z wynagrodzenia w pierwszym badanym okresie.

## 6. zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zalezy od wieku (PYT_2 oraz WIEK_KAT),
ftable_PYT2_WIEK_KAT <- table('Poziom zadowolenia w 1. badanym okresie' = data$PYT_2, 'Kategoria wiekowa' = data$WIEK_KAT)
ftable_PYT2_WIEK_KAT

fisher_test_6 <- fisher.test(ftable_PYT2_WIEK_KAT, conf.int = TRUE, workspace = 500000)
fisher_test_6
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_6$p.value)

# Warto zauważyć, że wynik testu Freedmana-Holdana wykazał p-wartość równą 0.0443, 
# co jest niższe od ustalonego poziomu istotności alpha = 0.05. Oznacza to, 
# że istnieją statystycznie istotne dowody na odrzucenie hipotezy zerowej, 
# która zakłada, że zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zależy od wieku.
# Innymi słowy, istnieje prawdopodobieństwo, że wiek ma istotny wpływ na poziom zadowolenia z wynagrodzenia 
# w badanym okresie.

### Lista 2. część 3 ###

## Zadanie 8
## Korzystaj ˛ac z chisq.test zweryfikuj hipotez˛e, ze zadowolenie z wynagrodzenia ˙
## w pierwszym badanym okresie nie zalezy od zajmowanego stanowiska. Przyjmij poziom ˙
## istotnosci 0.01. Stwórz wykres przy pomocy funkcji assocplot i dokonaj jego interpretacji.
## Wynik testu porównaj z wynikiem uzyskanym w zadaniu 6.

## W zadaniu 6 test 3 sprawdza hipotezę że zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zależy 
## od zajmowanego stanowiska (ftable_PYT2_CZY_KIER)

# Test Chi-kwadrat testuje hipotezę zerową o niezależności zmiennych.
chisq_test_PYT2_CZY_KIER <- chisq.test(ftable_PYT2_CZY_KIER)
chisq_test_PYT2_CZY_KIER

cat('P-wartość:',chisq_test_PYT2_CZY_KIER$p.value)

# W tym zadaniu ponownie przetestowaliśmy hipotezę zerową, sugerującą brak zależności między 
# zadowoleniem z wynagrodzenia a zajmowanym stanowiskiem w pierwszym badanym okresie. 
# Tym razem zdecydowaliśmy się wykorzystać test chi-kwadrat do weryfikacji tej hipotezy. 
# Wynik testu w postaci p-wartości wynoszącej 0.004397 wykazał, że przy założonym poziomie istotności 0.01 
# istnieją wystarczające podstawy do odrzucenia hipotezy zerowej. Porównując test chi-kwadrat z testem Fishera, 
# oba wykazują odrzucenie hipotezy zerowej na poziomie istotności 0.05. 
# Jednakże tylko test chi-kwadrat utrzymuje to odrzucenie na poziomie istotności 0.01.

assocplot(ftable_PYT2_CZY_KIER, col = c("lightblue", "pink"), main = 'Zależność pomiędzy zmiennymi',xlab = "Poziom zadowolenia w 1. badanym okresie", ylab = "Stanowisko kierownicze")


# Na wykresie asocjacji zauważamy, że prostokąty nie są równomiernie rozłożone. 
# Ich różne wielkości sugerują, że istnieje związek między zmiennymi. 
# Wartości prostokątów są znacząco różne, co wskazuje na nielosowy charakter występowania danych kategorii zmiennych. 
# Jednakże, na podstawie samej różnorodności wielkości prostokątów nie możemy jednoznacznie stwierdzić, czy zmienne są niezależne. 
# Konieczne jest przeprowadzenie dodatkowych analiz, takich jak test statystyczny,
# co wykonaliśmy wcześniej i potwierdziło to że zmienne są najprawdopodobniej zależne.


## Zadanie 10 Napisz funkcj˛e, która dla danych z tablicy dwudzielczej oblicza wartos´c poziomu ´
## krytycznego w tescie niezale ´ znosci opartym na ilorazie wiarogodnosci. Korzystaj ˛ac z napisanej ˙
## funkcji, wykonaj test dla danych z zadania 8.

test_IW <- function(table){
  # liczba obserwacji
  n <- sum(table)
  C <- ncol(table)
  R <- nrow(table)
  
  # rozkłady brzegowe
  R_totals <- rowSums(table)
  C_totals <- colSums(table)
  
  outer_table <- outer(R_totals, C_totals)
  
  expected <- (outer_table/(table*n))^table
  
  lambda <- prod(expected)
  G2 <- -2*log(lambda)
  
  df <- (R-1)*(C-1)
  p_value <- 1-pchisq(G2, df)
  
  return (p_value)
}

cat('P-wartość: ',test_IW(ftable_PYT2_CZY_KIER))

# Dla danych z zadania 6 p-wartość wynosi 0.03969. Jeśli weźmiemy poziom istotności 0.01 to nie podstaw
# do odrzucenia, natomiast dla poziomu istotności 0.05 należy odrzucić hipotezę zerową.


# Zadanie 9. Zapoznaj si˛e z funkcj ˛a rmultinom z pakietu stats, a nast˛epnie korzystaj ˛ac z niej
# przeprowad´z symulacje w celu oszacowania mocy testu Fishera oraz mocy testu chi-kwadrat
# Pearsona, generuj ˛ac dane z tabeli 2 × 2, w której p11 = 1/40, p12 = 3/40, p21 = 19/40,
# p22 = 17/40. Symulacje wykonaj dla n = 50, n = 100 oraz n = 1000.
set.seed(12345)

simulate_power <- function(MC=500, alpha, n, p){
  power_fisher <- rep(0, MC)
  power_chisq <- rep(0, MC)
  power_chisq_uncorrect <- rep(0, MC)
  
  for (i in 1:MC) {
    multi <- rmultinom(n=1, size=n, prob=p)
    
    fisher_test <- fisher.test(matrix(multi, nrow=2))
    power_fisher[i] <- as.numeric(fisher_test$p.value < alpha)
    
    chisq_test <- chisq.test(matrix(multi+0.00000001, nrow=2), correct = TRUE) 
    power_chisq[i] <- as.numeric(chisq_test$p.value < alpha)
    
    chisq_test_uncorrect <- chisq.test(matrix(multi+0.00000001, nrow=2), simulate.p.value = TRUE)
    power_chisq_uncorrect[i] <- as.numeric(chisq_test_uncorrect$p.value < alpha)
    
    p_value_chisq <- chisq.test(matrix(multi+0.00000001, nrow=2), correct = TRUE)$p.value
    p_value_chisq_uncorrect <- chisq.test(matrix(multi+0.00000001, nrow=2), simulate.p.value = TRUE)$p.value
  }
  
  return(c(mean(power_fisher), mean(power_chisq), mean(power_chisq_uncorrect)))
}

p <- c(1/40, 3/40, 19/40, 17/40)
# dla alpha 0.05
power_50 <- simulate_power(MC=500, alpha=0.05, n=50, p)
power_100 <- simulate_power(MC=500, alpha=0.05, n=100, p)
power_1000 <- simulate_power(MC=500, alpha=0.05, n=1000, p)

df_05 <- data.frame('Typ testu' = c('test Fishera', 'test Chi2 z poprawką', 'test Chi2 bez poprawki'), 'n=50' = power_50, 'n=100' = power_100, 'n=1000'= power_1000)
view(df_05)

power_50_01 <- simulate_power(MC=500, alpha=0.01, n=50, p)
power_100_01 <- simulate_power(MC=500, alpha=0.01, n=100, p)
power_1000_01 <- simulate_power(MC=500, alpha=0.01, n=1000, p)

df_01 <- data.frame('Typ testu' = c('test Fishera', 'test Chi2 z poprawką', 'test Chi2 bez poprawki'), 'n=50' = power_50_01, 'n=100' = power_100_01, 'n=1000'= power_1000_01)
view(df_01)




##  zadanie 11. Przeprowadzone wsród brytyjskich m˛e˙zczyzn badanie trwaj ˛ace 20 lat wykazało, ˙ze
##  odsetek zmarłych (na rok) z powodu raka płuc wynosił 0, 00140 wsród osób pal ˛acych papierosy
##  i 0,00010 wsród osób niepal ˛acych. Odsetek zmarłych z powodu choroby niedokrwiennej
##  serca wynosił 0, 00669 dla palaczy i 0, 00413 dla osób niepal ˛acych. Opisz zwi ˛azek pomi˛edzy
##  paleniem papierosów a ´smierci ˛a z powodu raka płuc oraz zwi ˛azek pomi˛edzy paleniem
##  papierosów a ´smierci ˛a z powodu choroby serca. Skorzystaj z ró˙znicy proporcji, ryzyka
##  wzgl˛ednego i ilorazu szans. Zinterpretuj warto´sci. Zwi ˛azek której pary zmiennych jest
##  silniejszy?

smokers_tab <- matrix(c(0.00669,0.00140, 0.00413, 0.00010), nrow = 2, ncol = 2)


## ischemic heart disease and smoking
proportion_differences_disease_smoking = smokers_tab[1,1] - smokers_tab[1,2]
RR_disease_smoking = smokers_tab[1,1]/smokers_tab[1,2]
OR_disease_smoking <- RR_disease_smoking * (1 - smokers_tab[1,2])/ (1 - smokers_tab[1,1])
## lung cancer and smoking 

proportion_differences_cancer_smoking<- smokers_tab[2,1] - smokers_tab[2,2]
RR_cancer_smoking<- smokers_tab[2,1]/smokers_tab[2,2]

OR_cancer_smoking <- RR_disease_smoking * (1 - smokers_tab[2,2])/ (1 - smokers_tab[2,1]) 

### Z uwagi na ryzyko wzgledne, silniejszy związek to związek palenia papierosów ze śmiercią z powodu raka płuc


##  zadanie 12. Tabela przedstawia wyniki dotycz ˛ace ´smiertelno´sci kierowców i pasa˙zerów w
##  wypadkach samochodowych na Florydzie w 2008 roku, w zale˙zno´sci od tego, czy osoba miała
##  zapi˛ety pas bezpiecze´nstwa czy nie.


accidents <- matrix(c(1085, 703, 55623, 441239), nrow = 2, ncol = 2)
accidents_probs <-  accidents/sum(accidents)
sum_without_seatbelts <- 1085 + 55623
sum_seatbelts <- 703 + 441239
sum_mortal <-  1085 + 703
sum_immortal <- 55623 + 441239

prob_death_seatbelts = 703/sum_seatbelts

prob_death_without_seatbelts = sum_without_seatbelts/sum(accidents) *  1085/sum_without_seatbelts


prob_seatbelts_death <- (sum_seatbelts*(703/sum_seatbelts)/sum(accidents))/(sum_seatbelts*(703/sum_seatbelts)/sum(accidents) + sum_without_seatbelts*(1085/sum_without_seatbelts)/sum(accidents))

prob_without_seatbelts_death <- (sum_without_seatbelts*(1085/sum_without_seatbelts)/sum(accidents))/(sum_seatbelts*(703/sum_seatbelts)/sum(accidents) + sum_without_seatbelts*(1085/sum_without_seatbelts)/sum(accidents))


## najbardziej naturalny wybór jest, aby wybrać zmienną objaśnianą śmiertelność wypadku - wtedy zmienna objaśniająca, czyli fakt zapięcia pasów bądź nie pozwala ustalić wiele rzeczy, takich jak, przyczyny zgonu chociazby

proportion_differences_mortal<- accidents_probs[1,1] - accidents_probs[2,1]
RR_mortal <-  accidents_probs[1,1]/accidents_probs[2,1]
OR_mortal <-  RR_mortal * (1 - smokers_tab[2,1])/ (1 - smokers_tab[1,1]) 

##  zadanie 13. Oblicz warto´sci odpowiednich miar współzmienno´sci (współczynnik tau lub
##  współczynnik gamma) dla zmiennych:
##    • zadowolenie z wynagrodzenia w pierwszym badanym okresie i zajmowane stanowisko,
##    • zadowolenie z wynagrodzenia w pierwszym badanym okresie i sta˙z pracy,
##    • zajmowane stanowisko i sta˙z pracy


czy_zadw_czy_kier_tau <- GoodmanKruskalTau(data$CZY_ZADOW,data$CZY_KIER)
pyt_2_staż_gamma <-  GoodmanKruskalGamma(data$PYT_2, data$STAZ)




### czy_zadw, czy kier to typowe zmienne nominalne (dychotomiczne), staż to zmienna kategoryczna(porządkowa) tak samo jak Pyt_2
## do 3. podpunktu stworzymy nominalny odpowiednik zmiennej staz CZY_STAZ: 1, jeżeli staż jest dłuższy niż 1. rok, 0 jeżeli nie

data <- mutate(data, CZY_STAZ = ifelse(as.numeric(STAZ) == 1, 0, 1))

czy_kier_staż_tau<- GoodmanKruskalTau(ftable_CZY_KIER_STAZ)


##  zadanie 14. Na podstawie informacji przedstawionych na wykładzie napisz własn ˛a funkcj˛e
##  do przeprowadzania analizy korespondencji. Funkcja powinna przyjmowa´c jako argument
##  tablic˛e dwudzielcz ˛a i zwraca´c obliczone warto´sci odpowiednich wektorów i macierzy,
##  współrz˛ednych punktów oraz odpowiedni wykres. Korzystaj ˛ac z napisanej funkcji wykonaj
##  analiz˛e korespondencji dla danych dotycz ˛acych zadowolenia z wynagrodzenia w pierwszym
##  badanym okresie i sta˙zu pracy.

correspondence <- function(crosstab){
  N <- crosstab %>% as.matrix()
  P <-  N/sum(N)
  r <- P %>%  rowSums()
  c <- P %>%  colSums()
  D_r <-diag(r)
  D_c <-diag(c)
  R <-  solve(D_r)%*%P
  C <- P%*%solve(D_c)
  A <- solve(D_r)^(1/2)%*%(P - r%*%t(c))%*%solve(D_c)^(1/2)
  chi_sq <- sum(N) *sum(A^2)
  U <-  svd(A)$u
  Gamma_vec <-  svd(A)$d
  V <-  svd(A)$v
  nd <-  min(nrow(crosstab), ncol((crosstab))) - 1
  F_matrix <-  solve(D_r)^(1/2)%*%U[,1:nd]%*%diag(Gamma_vec[1:nd])
  G_matrix <-  solve(D_c)^(1/2)%*%V[,1:nd]%*%diag(Gamma_vec[1:nd])
  inertia <-  sum(diag(A%*%t(A)))
  return(list(F_matrix, G_matrix, r,c, R, C, P, inertia, chi_sq))
}

pyt_2_staz <- table(data[c("STAZ", "PYT_2")])

ca_pyt_2_staz <- correspondence(pyt_2_staz)
F_matrix <- ca_pyt_2_staz[[1]]
G_matrix <- ca_pyt_2_staz[[2]]

rows <-  data.frame("D1" = F_matrix[,1], "D2"=F_matrix[,2], "Value" = rownames(pyt_2_staz))
columns <-  data.frame("D1" = G_matrix[,1], "D2"=G_matrix[,2], "Value" = colnames(pyt_2_staz))
coordinates <- rbind( data.frame("D1" = rows$D1, "D2" =rows$D2, "Value" = rows$Value, "Kind" = "STAZ"), data.frame("D1" = columns$D1, "D2" = columns$D2, "Value" = columns$Value, "Kind" ="PYT_2"))

ggplot(coordinates, aes(x = D1, y = D2, color = Kind, shape = Kind, label = Value)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.5) +
  theme_minimal() +
  labs(x = "Dimension 1", y = "Dimension 2") +
  ggtitle("Analiza korespondencji dla zmiennych STAŻ oraz PYT_2")+
  theme(plot.title = element_text(hjust=0.5), legend.position = "left")

plot.ca(ca(pyt_2_staz, nd =2))

ca(pyt_2_staz, nd =2)
ca_pyt_2_staz

