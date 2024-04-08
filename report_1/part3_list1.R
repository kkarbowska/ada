# CZĘŚĆ TRZECIA
library(binomCI)
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


### task 9

### 
Wald_CI = function(X, n, size, alpha) {
  p_est <- sum(X)/(n*size)
  quantile <- qnorm(1-alpha/2, mean = 0, sd = 1)
  lower_bound <-p_est - quantile * sqrt(p_est * (1-p_est) / n)
  upper_bound <-p_est + quantile * sqrt(p_est * (1-p_est) / n)
  CI <- c(lower_bound, upper_bound)
  return(CI)
} 


### probabilities vector 
p_s = seq(from=0.01, to= 0.99, length.out = 99)
### n = 30 
wald_res_coverage_30 = rep(0,99)
wald_res_len_30 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 30, prob = p_s[i])
    ##wald
    ci_wald = Wald_CI(sample_binom, n = 30, 100, alpha = 0.05)
    temp_len[mcs] <- ci_wald[2] - ci_wald[1]
    temp_coverage[mcs] <-sum(ci_wald[1] < sample_binom/30& sample_binom/30<ci_wald[2])/100
  }
  wald_res_len_30[i] <-  mean(temp_len)
  wald_res_coverage_30[i] <-  mean(temp_coverage)
}

cp_res_coverage_30 = rep(0,99)
cp_res_len_30 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 30, prob = p_s[i])
    ##cp
    ci_cp = binom.confint(sample_binom, n = 30,alpha = 0.05, methods = "exact")
    temp_len[mcs] <- mean(ci_cp$upper- ci_cp$lower)
    temp_coverage[mcs] <-sum(mean(ci_cp$lower) < sample_binom/30& sample_binom/30<mean(ci_cp$upper))/100
  }
  cp_res_len_30[i] <-  mean(temp_len)
  cp_res_coverage_30[i] <-  mean(temp_coverage)
}


ac_res_coverage_30 = rep(0,99)
ac_res_len_30 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 30, prob = p_s[i])
    ##cp
    ci_ac = binom.confint(sample_binom, n = 30,alpha = 0.05, methods = "agresti-coull")
    temp_len[mcs] <- mean(ci_ac$upper- ci_ac$lower)
    temp_coverage[mcs] <-sum(mean(ci_ac$lower) < sample_binom/30& sample_binom/30<mean(ci_ac$upper))/100
  }
  ac_res_len_30[i] <-  mean(temp_len)
  ac_res_coverage_30[i] <-  mean(temp_coverage)
}

plot(p_s, ac_res_len_30)
plot(p_s, ac_res_coverage_30, type = "l")




plot(p_s, wald_res_len_30, type = "l", frame = FALSE, pch = 19,
main="Średnia długość przedziału ufności, 
        n = 30, długość próbki = 100, 1000 MCS",
     col = "red", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2)+
  lines(p_s, cp_res_len_30, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)+
  lines(p_s, ac_res_len_30, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)+
  legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



plot(p_s,wald_res_coverage_30, type = "l", frame = FALSE, pch = 19,
     col = "red",main="Coverage probabilities of CIs for n = 30", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2,ylim = c(0.8,1))

lines(p_s, cp_res_coverage_30, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, ac_res_coverage_30, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



### n = 100
wald_res_coverage_100 = rep(0,99)
wald_res_len_100 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 100, prob = p_s[i])
    ##wald
    ci_wald = Wald_CI(sample_binom, n = 100, 100, alpha = 0.05)
    temp_len[mcs] <- ci_wald[2] - ci_wald[1]
    temp_coverage[mcs] <-sum(ci_wald[1] < sample_binom/100& sample_binom/100<ci_wald[2])/100
  }
  wald_res_len_100[i] <-  mean(temp_len)
  wald_res_coverage_100[i] <-  mean(temp_coverage)
}





cp_res_coverage_100 = rep(0,99)
cp_res_len_100 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 100, prob = p_s[i])
    ##cp
    ci_cp = binom.confint(sample_binom, n = 100,alpha = 0.05, methods = "exact")
    temp_len[mcs] <- mean(ci_cp$upper- ci_cp$lower)
    temp_coverage[mcs] <-sum(mean(ci_cp$lower) < sample_binom/100& sample_binom/100<mean(ci_cp$upper))/100
  }
  cp_res_len_100[i] <-  mean(temp_len)
  cp_res_coverage_100[i] <-  mean(temp_coverage)
}


ac_res_coverage_100 = rep(0,99)
ac_res_len_100 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 100, prob = p_s[i])
    ##cp
    ci_ac = binom.confint(sample_binom, n = 100,alpha = 0.05, methods = "agresti-coull")
    temp_len[mcs] <- mean(ci_ac$upper- ci_ac$lower)
    temp_coverage[mcs] <-sum(mean(ci_ac$lower) < sample_binom/100& sample_binom/100<mean(ci_ac$upper))/100
  }
  ac_res_len_100[i] <-  mean(temp_len)
  ac_res_coverage_100[i] <-  mean(temp_coverage)
}

plot(p_s,wald_res_len_100, type = "l", frame = FALSE, pch = 19,
     col = "red",main="Average lengths of CIs for n = 100", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2)

lines(p_s, cp_res_len_100, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, ac_res_len_100, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



plot(p_s,wald_res_coverage_100, type = "l", frame = FALSE, pch = 19,
     col = "red",main="Coverage probabilities of CIs for n = 100", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2,ylim = c(0.8,1))

lines(p_s, cp_res_coverage_100, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, ac_res_coverage_100, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



### n = 1000
wald_res_coverage_1000 = rep(0,99)
wald_res_len_1000 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 1000, prob = p_s[i])
    ##wald
    ci_wald = Wald_CI(sample_binom, n = 1000, 100, alpha = 0.05)
    temp_len[mcs] <- ci_wald[2] - ci_wald[1]
    temp_coverage[mcs] <-sum(ci_wald[1] < sample_binom/1000& sample_binom/1000<ci_wald[2])/100
  }
  wald_res_len_1000[i] <-  mean(temp_len)
  wald_res_coverage_1000[i] <-  mean(temp_coverage)
}


cp_res_coverage_1000 = rep(0,99)
cp_res_len_1000 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 1000, prob = p_s[i])
    ##cp
    ci_cp = binom.confint(sample_binom, n = 1000,alpha = 0.05, methods = "exact")
    temp_len[mcs] <- mean(ci_cp$upper- ci_cp$lower)
    temp_coverage[mcs] <-sum(mean(ci_cp$lower) < sample_binom/1000& sample_binom/1000<mean(ci_cp$upper))/100
  }
  cp_res_len_1000[i] <-  mean(temp_len)
  cp_res_coverage_1000[i] <-  mean(temp_coverage)
}

ac_res_coverage_1000 = rep(0,99)
ac_res_len_1000 = rep(0,99)
for (i in 1:99) {
  temp_len = rep(0,1000)
  temp_coverage = rep(0,1000)
  for (mcs in 1:1000) {
    sample_binom = rbinom(n = 100, size = 1000, prob = p_s[i])
    ##cp
    ci_ac = binom.confint(sample_binom, n = 1000,alpha = 0.05, methods = "agresti-coull")
    temp_len[mcs] <- mean(ci_ac$upper- ci_ac$lower)
    temp_coverage[mcs] <-sum(mean(ci_ac$lower) < sample_binom/1000& sample_binom/1000<mean(ci_ac$upper))/100
  }
  ac_res_len_1000[i] <-  mean(temp_len)
  ac_res_coverage_1000[i] <-  mean(temp_coverage)
}

plot(p_s,wald_res_len_1000, type = "l", frame = FALSE, pch = 19,
     col = "red",main="Average lengths of CIs for n = 1000", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2)

lines(p_s, cp_res_len_1000, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, ac_res_len_1000, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)



plot(p_s,wald_res_coverage_1000, type = "l", frame = FALSE, pch = 19,
     col = "red",main="Coverage probabilities of CIs for n = 1000", xlab = "probability of success", ylab = "average length of CI", 
     lty = 1, lwd = 2,ylim = c(0.92,1))

lines(p_s, cp_res_coverage_1000, pch = 23, col = "blue", type = "l", 
      lty = 2, lwd = 2)

lines(p_s, ac_res_coverage_1000, pch = 18, col = "green", type = "b", 
      lty = 3, lwd = 2)

legend("topleft", legend = c("Wald CI", "C-P CI", "A-C CI"),
       col = c("red", "blue", "green"), lty = 1:3, cex = 0.8)

