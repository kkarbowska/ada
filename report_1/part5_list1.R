library(stats)


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



