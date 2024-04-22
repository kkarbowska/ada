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
ftable_PYT2_WIEK_KAT <- ftable('Poziom zadowolenia w 1. badanym okresie' = data$PYT_2, 'Kategoria wiekowa' = data$CZY_KIER)
ftable_PYT2_WIEK_KAT

fisher_test_6 <- fisher.test(ftable_PYT2_WIEK_KAT, conf.int = TRUE)
fisher_test_6
# cat('Przedziały ufności:',fisher_test_1$conf.int)
cat('P-wartość:',fisher_test_6$p.value)

# Warto zauważyć, że wynik testu Freedmana-Holdana wykazał p-wartość równą 0.0443, 
# co jest niższe od ustalonego poziomu istotności alpha = 0.05. Oznacza to, 
# że istnieją statystycznie istotne dowody na odrzucenie hipotezy zerowej, 
# która zakłada, że zadowolenie z wynagrodzenia w pierwszym badanym okresie nie zależy od wieku.
# Innymi słowy, istnieje prawdopodobieństwo, że wiek ma istotny wpływ na poziom zadowolenia z wynagrodzenia 
# w badanym okresie.

