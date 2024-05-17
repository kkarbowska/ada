## Lista 2. Część 2.###
### operacje na danych jak wcześniej 

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

