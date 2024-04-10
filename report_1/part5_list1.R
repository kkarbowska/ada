library(stats)
library(tidyverse)
library(likert)
library(dbplyr)
katalog = dirname(normalizePath(file.choose())) # wybiera sie plik na którym chcemy pracować, i najlepiej ten w którym są dane
setwd(katalog)

data <- read.csv("ankieta.csv", sep = ';', col.names = c('DZIAL', 'STAZ', 'CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PLEC', 'WIEK'))
data <- data %>% mutate_at(vars(DZIAL, STAZ, CZY_KIER, PYT_1, PYT_2, PYT_3, PLEC), as.factor)

przedzialy_wiekowe <- c(0, 35, 45, 55, Inf)
nazwy_kategori <- c("0-35", "36-45", "46-55", "55+")
data$WIEK_KAT <- cut(data$WIEK, przedzialy_wiekowe, labels = nazwy_kategori, include.lowest = TRUE)
data <- mutate(data, CZY_ZADOW = ifelse(as.numeric(as.character(PYT_2)) == -2, 0,
                                          ifelse(as.numeric(as.character(PYT_2)) == -1, 0,
                                                ifelse(as.numeric(as.character(PYT_2)) == 1, 1,
                                                   ifelse(as.numeric(as.character(PYT_2)) == 2, 1, "_"))))

data <- mutate(data, CZY_ZADOW_2 = ifelse(as.numeric(as.character(PYT_3)) == -2, 0,
                                                         ifelse(as.numeric(as.character(PYT_3)) == -1, 0,
                                                                ifelse(as.numeric(as.character(PYT_3)) == 1, 1,
                                                                       ifelse(as.numeric(as.character(PYT_3)) == 2, 1, "_"))))) 
data$CZY_ZADOW_2 <- as.factor(as.integer(data$CZY_ZADOW_2))

# 1. Prawdopodobienstwo, ´ ze w firmie pracuje kobieta wynosi 0.5.
sum_woman_work <- count(subset(data, as.character(PLEC) == 'K')) %>% as.integer()

# Wykonamy binom test prawdopodobieństwa sukcesu. Hipoteza zerowa zakłada, że prawdobodobieństwo, że w firmie pracuje kobieta
# wynosi 0.5, natomiast hipoteza alternatywna jest zaprzeczeniem hipotezy zerowej. Przyjmujemy poziom ufności 0.95.
# Zweryfikowaliśmy przedziały ufności i p-wartość by ocenić prawdziwość hipotezy zerowej. Wyniki są widoczne poniżej.

test_1 <- binom.test(sum_woman_work, nrow(data), p = 0.5, alternative = "two.sided")
cat('P - wartość wynosi:', test_1$p.value, ', przedział ufności jest postaci: ', test_1$conf.int)

# Zauważmy że p-wartość jest dużo mniejsza od poziomu istotności alpha = 0.05, co jest przesłanką do odrzucenia hipotezy zerowej.
# Dodatkowo jeśli spojrzymy na przedział ufności dla prawdopodobieństwa sukcesu, widzimy że p = 0.5 nie jest w tym przedziale, 
# co potwierdza teze o odrzuceniu hipotezy zerowej. Podsumowując, hipoteza że prawdobodobieństwo, że w firmie pracuje kobieta
# wynosi 0.5 jest nieprawdziwa.


# 2. Prawdopodbienstwo, że pracownik jest zadowolony ze swojego wynagrodzenia w pierwszym badanym okresie jest 
# większe bądź równe 0.7.
sum_zadow <- count(subset(data, as.integer(as.factor(data$CZY_ZADOW)) == 1 )) %>% as.integer()
test_2 <- binom.test(sum_zadow, nrow(data), p = 0.7, alternative = "less")

# Zweryfikowaliśmy hipotezę, że prawdobodobieństwo, że pracownik jest zadowolony ze swojego wynagrodzenia w 
# pierwszym badanym okresie jest większe bądź równe 0.7, co było naszą hipotezą zerową. Podobnie jak w pierwszym teście
# przyjęto poziom ufności 0.95 i zweryfikowaliśmy prawdziwość hipotezy zerowej na podstawie przedziału ufności oraz p-wartości.
# Wyniki zostały wyświetlone poniżej.

cat('P - wartość wynosi:', test_2$p.value, ', przedział ufności jest postaci: ', test_2$conf.int)
# Test dał nam p-wartość mniejszą od zakładanego poziomu istotności, co sugeruje by odrzucić hipotezę zerową.
# Co więcej możemy zauważyć, że p > = 0.7 jest poza przedziałem ufności dla prawdopodobieństwo, co podkreśla
# słuszność odrzucenia hipotezy zerowej na rzecz alternatywnej. 

# 3. Prawdopodobienstwo, że kobieta pracuje na stanowisku menedżerskim jest równe 
# prawdopodobienstwu, że mężczyzna pracuje na stanowisku menedzerskim

# Przeprowadzimy testy proporcji z poprawką na ciągłość i bez aby określić prawdziwość hipotezy zerowej że
# Prawdopodobienstwo, że kobieta pracuje na stanowisku menedżerskim jest równe 
# prawdopodobienstwu, że mężczyzna pracuje na stanowisku menedzerskim. Hipoteza alternatywna mówi, że te prawdopodobieństwa
# nie są równe. Wykonamy testy na poziomie ufności 0.95.  Zweryfikujemy hipoteze zerową dzięki p-wartości. 
# Wyniki testu są umieszczone w poniższej tabeli.


sum_menadzer_woman <- length(data$PLEC[data$PLEC == "K" & data$CZY_KIER == 'Tak'])
sum_menadzer_man <- length(data$PLEC[data$PLEC == "M" & data$CZY_KIER == 'Tak'])

test_3_con <- prop.test(c(sum_menadzer_woman, sum_menadzer_man), 
                        c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                        alternative = "two.sided", correct = TRUE)

test_3_ucon <- prop.test(c(sum_menadzer_woman, sum_menadzer_man), 
                        c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                        alternative = "two.sided", correct = FALSE)

test_3_df <- data.frame('Testy' = c('Test z poprawką na ciągłość', 'Test bez poprawki na ciągłość'), 
           'P-wartość' = c(test_3_con$p.value, test_3_ucon$p.value))
# Zauważmy, że dla obu testów p-wartość jest większa niż poziom istotności alpha = 0.05, zatem nie podstaw do odrzucenia
# hipotezy zerowej.

# 4. Prawdopodobieństwo, że kobieta jest zadowolona ze swojego wynagrodzenia w
# pierwszym badanym okresie jest równe prawdopodobieństwu, że mężczyzna jest
# zadowolony ze swojego wynagrodzenia w pierwszym badanym okresie.

# Będziemy testować hipotezę zerową że prawdopodobieństwo, że kobieta jest zadowolona ze swojego wynagrodzenia w
# pierwszym badanym okresie jest równe prawdopodobieństwu, że mężczyzna jest
# zadowolony ze swojego wynagrodzenia w pierwszym badanym okresie przeciw hipotezie alteratywnej, że te prawdopodobieństwa 
# nie są równe. Przyjmujemy poziom ufności 0.95. Będziemy brali głównie pod uwagę p-wartość. Wyniki zostały umieszczone 
# w tabeli poniżej

sum_zadow_woman <- length(data$PLEC[data$PLEC == "K" & data$CZY_ZADOW == 1])
sum_zadow_man <- length(data$PLEC[data$PLEC == "M" & data$CZY_ZADOW == 1])

test_4_con <- prop.test(c(sum_zadow_woman, sum_zadow_man), 
                        c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                        alternative = "two.sided", correct = TRUE)

test_4_ucon <- prop.test(c(sum_zadow_woman, sum_zadow_man), 
                         c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                         alternative = "two.sided", correct = FALSE)

test_4_df <- data.frame('Testy' = c('Test z poprawką na ciągłość', 'Test bez poprawki na ciągłość'), 
                        'P-wartość' = c(test_4_con$p.value, test_4_ucon$p.value))
print(test_4_df)
# Zauważmy, że dla obu testów p-wartość jest większa niż poziom istotności alpha = 0.05, zatem nie podstaw do odrzucenia
# hipotezy zerowej.


# 5. Prawdopodobie´nstwo, ˙ze kobieta pracuje w dziale obsługi kadrowo-płacowej jest wi˛eksze
# lub równe prawdopodobie´nstwu, ˙ze m˛e˙zczyzna pracuje w dziale obsługi kadrowo-
# płacowej.

# Wykonaliśmy test proporcji aby zweryfikować hipotezę zerową że prawdopodobieństwo, ˙ze kobieta pracuje w dziale obsługi kadrowo-płacowej jest wi˛eksze
# lub równe prawdopodobie´nstwu, ˙ze m˛e˙zczyzna pracuje w dziale obsługi kadrowo-
# płacowej. Hipotezą alternatywną dla tej hipotezy zerowej jest prawdopodobieństwo, ˙ze kobieta pracuje w dziale obsługi kadrowo-płacowej jest mniejsze 
# od prawdopodobieństwa, ˙ze m˛e˙zczyzna pracuje w dziale obsługi kadrowo-
# płacowej. Test wykonaliśmy  na poziomie ufności 0.95. Wyniki przedstawiliśmy w tabeli poniżej.
sum_hr_woman <- length(data$PLEC[data$PLEC == "K" & data$DZIAL == 'HR'])
sum_hr_man <- length(data$PLEC[data$PLEC == "M" & data$DZIAL == 'HR'])

test_5_con <- prop.test(c(sum_hr_woman, sum_hr_man), 
                        c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                        alternative = "less", correct = TRUE)

test_5_ucon <- prop.test(c(sum_hr_woman, sum_hr_man), 
                         c(length(data$PLEC[data$PLEC == "K"]), length(data$PLEC[data$PLEC == "M"])), 
                         alternative = "less", correct = FALSE)

test_5_df <- data.frame('Testy' = c('Test z poprawką na ciągłość', 'Test bez poprawki na ciągłość'), 
                        'P-wartość' = c(test_5_con$p.value, test_5_ucon$p.value))
print(test_5_df)

# Zauważmy, że dla obu testów p-wartość jest mniejsza niż poziom istotności alpha = 0.05, zatem należy
# odrzucić hipotezę zerową. 




###
###zadanie 12. Wyznacz symulacyjnie moc testu dokładnego oraz moc testu asymptotycznego
###w przypadku weryfikacji hipotezy zerowej H0 : p = 0.9 przeciwko H1 : p̸ = 0.9 przyjmujac
###wartosc 1 − α = 0.95. Uwzglednij rózne wartozci alternatyw i rózne rozmiary próby. Sformułuj
###wnioski.

p_s <-  seq(from=0.01, to= 0.99, length.out = 98)

seq
### n = 30

pwr_binom_test_30 = rep(0,98)
pwr_prop_test_corrected_30 = rep(0,98)
pwr_prop_test_30 = rep(0,98)
for (k in 1:98) {
  temp_binom = 0 
  temp_prop_corrected = 0
  temp_prop = 0
  for (i in 1:1000) {
    sample_binom = rbinom(n = 100, size = 30, prob = p_s[k])
    ci_binom = binom.test(sum(sample_binom),n = 100*30, p = .9)$conf.int
    if (!(ci_binom[1] < 0.9 & 0.9 < ci_binom[2])) {
      temp_binom = temp_binom + 1
    }
    
    
    ci_prop_corrected = prop.test(sum(sample_binom),n = 100*30, p = .9)$conf.int
    if (!(ci_prop_corrected[1] < 0.9 & 0.9 < ci_prop_corrected[2])) {
      temp_prop_corrected = temp_prop_corrected + 1
    }
    
    
    ci_prop = prop.test(sum(sample_binom),n = 100*30, p = .9, correct = FALSE)$conf.int
    if (!(ci_prop[1] < 0.9 & 0.9 < ci_prop[2])) {
      temp_prop = temp_prop + 1
    }
    
  }
  pwr_binom_test_30[k] <- temp_binom/1000
  pwr_prop_test_corrected_30[k] <- temp_prop_corrected/1000
  pwr_prop_test_30[k] <- temp_prop/1000
}
plot(p_s,pwr_binom_test_30, type = "l", frame = FALSE, pch = 19,
     col = "red",main="         Porównanie mocy testów H0: p=0.9, 
        n = 30, długość próbki = 100, 1000 MCS", xlab = "p", ylab = "moc testu", 
     lty = 1, lwd = 2)

lines(p_s, pwr_prop_test_corrected_30, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, pwr_prop_test_30, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("left", legend = c("binom.test(_)", "prop.test(_)", "prop.test(_, correct = FALSE"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)


### n = 100

pwr_binom_test_100 = rep(0,98)
pwr_prop_test_corrected_100 = rep(0,98)
pwr_prop_test_100 = rep(0,98)
for (k in 1:98) {
  temp_binom = 0 
  temp_prop_corrected = 0
  temp_prop = 0
  for (i in 1:1000) {
    sample_binom = rbinom(n = 100, size = 100, prob = p_s[k])
    ci_binom = binom.test(sum(sample_binom),n = 100*100, p = .9)$conf.int
    if (!(ci_binom[1] < 0.9 & 0.9 < ci_binom[2])) {
      temp_binom = temp_binom + 1
    }
    
    
    ci_prop_corrected = prop.test(sum(sample_binom),n = 100*100, p = .9)$conf.int
    if (!(ci_prop_corrected[1] < 0.9 & 0.9 < ci_prop_corrected[2])) {
      temp_prop_corrected = temp_prop_corrected + 1
    }
    
    
    ci_prop = prop.test(sum(sample_binom),n = 100*100, p = .9, correct = FALSE)$conf.int
    if (!(ci_prop[1] < 0.9 & 0.9 < ci_prop[2])) {
      temp_prop = temp_prop + 1
    }
    
  }
  pwr_binom_test_100[k] <- temp_binom/1000
  pwr_prop_test_corrected_100[k] <- temp_prop_corrected/1000
  pwr_prop_test_100[k] <- temp_prop/1000
}
plot(p_s,pwr_binom_test_100, type = "l", frame = FALSE, pch = 19,
     col = "red",main="         Porównanie mocy testówm H0: p=0.9, 
        n = 100, długość próbki = 100, 1000 MCS", xlab = "p", ylab = "moc testu", 
     lty = 1, lwd = 2)

lines(p_s, pwr_prop_test_corrected_100, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, pwr_prop_test_100, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("left", legend = c("binom.test(_)", "prop.test(_)", "prop.test(_, correct = FALSE"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)


### n = 1000

pwr_binom_test_1000 = rep(0,98)
pwr_prop_test_corrected_1000 = rep(0,98)
pwr_prop_test_1000 = rep(0,98)
for (k in 1:98) {
  temp_binom = 0 
  temp_prop_corrected = 0
  temp_prop = 0
  for (i in 1:1000) {
    sample_binom = rbinom(n = 100, size = 1000, prob = p_s[k])
    ci_binom = binom.test(sum(sample_binom),n = 100*1000, p = .9)$conf.int
    if (!(ci_binom[1] < 0.9 & 0.9 < ci_binom[2])) {
      temp_binom = temp_binom + 1
    }
    
    
    ci_prop_corrected = prop.test(sum(sample_binom),n = 100*1000, p = .9)$conf.int
    if (!(ci_prop_corrected[1] < 0.9 & 0.9 < ci_prop_corrected[2])) {
      temp_prop_corrected = temp_prop_corrected + 1
    }
    
    
    ci_prop = prop.test(sum(sample_binom),n = 100*1000, p = .9, correct = FALSE)$conf.int
    if (!(ci_prop[1] < 0.9 & 0.9 < ci_prop[2])) {
      temp_prop = temp_prop + 1
    }
    
  }
  pwr_binom_test_1000[k] <- temp_binom/1000
  pwr_prop_test_corrected_1000[k] <- temp_prop_corrected/1000
  pwr_prop_test_1000[k] <- temp_prop/1000
}
plot(p_s,pwr_binom_test_1000, type = "l", frame = FALSE, pch = 19,
     col = "red",main="         Porównanie mocy testówm H0: p=0.9, 
        n = 1000, długość próbki = 100, 1000 MCS", xlab = "p", ylab = "moc testu", 
     lty = 1, lwd = 2)

lines(p_s, pwr_prop_test_corrected_1000, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, pwr_prop_test_1000, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("left", legend = c("binom.test(_)", "prop.test(_)", "prop.test(_, correct = FALSE"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



