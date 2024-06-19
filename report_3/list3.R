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
library(DescTools)
library(xtable)
library(Rlab)
library(tidyr)
library(sjmisc)
library(vcd)
library(grid)
library(graphics)
library(ca)
library("exact2x2")
library(gnm)
library(rcompanion)
katalog = dirname(normalizePath(file.choose())) # wybiera sie plik na którym chcemy pracować, i najlepiej ten w którym są dane
setwd(katalog)

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


# Zadanie 1. Napisz funkcję, która zwraca p-wartość w omówionym na wykładzie warunkowym ´
# tescie symetrii dla tabel 2×2

# utworzenie funkcji conditional symetry test, która przyjmuje tabelę dwudzielczą 2x2
# którą należy badać

conditional_symetry_test <- function(table){
  
  if (!all(dim(table) == c(2, 2))) {
    stop("Tabela musi być macierzą 2x2.")
  }
  
  y12 <- table[1,2]
  y21 <- table[2,1]
  n_star <- y12 + y21
   
  y_l <- min(c(y12, n_star - y12))
  y_p <- max(c(y12, n_star - y12))
  
  first_component <- sum(dbinom(0:y_l, size=n_star, prob=0.5))
  second_component <- sum(dbinom(y_p:n_star, size=n_star, prob=0.5))
  p_value <- first_component + second_component
  
  return(p_value)
}

## zadanie 2. W tabeli 1 umieszczono dane dotycz ˛ace reakcji na lek po godzinie od jego
## przyj˛ecia dla dwóch róznych leków przeciwbólowych stosowanych w migrenie. Leki zostały ˙
## zaaplikowane grupie pacjentów w dwóch róznych atakach bólowych. Na podstawie danych ˙
## zweryfikuj hipotez˛e, ze leki te s ˛a jednakowo skuteczne korzystaj ˛ac z testu ˙
## • McNemara z poprawka na ciagłos´c,´
## • warunkowego (korzystajac z funkcji zadeklarowanej w zadaniu 1.).

# Stworzeni tabeli z danymi
medicin_reaction <- matrix(c(1, 5, 2, 4), nrow=2, byrow = TRUE,
                           dimnames = list("Reakcja na lek A" = c("Negatywna", "Pozytywna"),
                                           "Reakcja na lek B" = c("Negatywna", "Pozytywna")))
medicin_reaction

# test McNehmara z poprawką na ciągłość 
mcnemar <- mcnemar.test(x = medicin_reaction, correct=TRUE)
cat('Statystyka testu: ', mcnemar$statistic) #0.5714286
cat('P-wartość:', mcnemar$p.value)

# warunkowy test symetrii
cat('P-wartość :' ,conditional_symetry_test(medicin_reaction))

results_zad2 <- data.frame(Testy = c('Warunkowy test symetrii (funkcja z zad 1.)', 'Test McNemara z poprawką na ciągłość'),
                           P_value = c(conditional_symetry_test(medicin_reaction), mcnemar$p.value))

xtable(results_zad2, digits = c(4,4,4))
## zadanie 3. Przeprowad´z symulacje w celu porównania mocy testu Z i testu Z0 przedstawionych
## na wykładzie. Rozwaz ró ˙ zne długości prób.

test_Z <- function(tabela) {
  Y_12 <- tabela[1, 2]
  Y_21 <- tabela[2, 1]
  
  n <- sum(tabela)
  prob_table <- tabela / n
  p11 <- prob_table[1, 1]
  p12 <- prob_table[1, 2]
  p21 <- prob_table[2, 1]
  p22 <- prob_table[2, 2]
  
  p_1_plus <- sum(tabela[1, ]) / n
  p_plus_1 <- sum(tabela[, 1]) / n
  
  D <- (Y_12 - Y_21) / n
  sigma2 <- ((p_1_plus * (1 - p_1_plus)) + (p_plus_1 * (1 - p_plus_1)) - (2 * (p11 * p22 - p12 * p21))) / n
  sigma <- sqrt(sigma2)
  
  Z <- D / sigma
  
  p_value <- 2 * (1 - pnorm(abs(Z)))
  
  return(p_value)
}

test_Z0 <- function(tabela) {

  Y_12 <- tabela[1, 2]
  Y_21 <- tabela[2, 1]
  
  Z0 <- (Y_12 - Y_21) / sqrt(Y_12 + Y_21)
  p_value <- 2 * (1 - pnorm(abs(Z0)))
  
  return(p_value)
}

set.seed(12345)
simulate_power_tests_Z <- function(p2, n, p1 = 0.5, num_simulations = 1000) {
  power_Z <- numeric(length(p2))
  power_Z0 <- numeric(length(p2))
  
  for (i in 1:length(p2)) {
    rejections_Z <- 0
    rejections_Z0 <- 0
    
    for (j in 1:num_simulations) {
      
      valid_table <- FALSE
      while (!valid_table) {
        X <- rbinom(n = n, size = 1, prob = p1)
        Y <- rbinom(n = n, size = 1, prob = p2[i])
        table_test <- table(X, Y)
        
        if (nrow(table_test) >= 2 && ncol(table_test) >= 2) {
          valid_table <- TRUE
        }
      }
      
      
      p_value_Z <- test_Z(table_test)
      p_value_Z0 <- test_Z0(table_test)
      
      if (p_value_Z < 0.05) {
        rejections_Z <- rejections_Z + 1
      }
      if (p_value_Z0 < 0.05) {
        rejections_Z0 <- rejections_Z0 + 1
      }
    }
    power_Z[i] <- rejections_Z / num_simulations
    power_Z0[i] <- rejections_Z0 / num_simulations
  }
  return(data.frame(Probabilities = p2, Power_Z = power_Z, Power_Z0 = power_Z0))
}

p2 <- c(seq(0.01, 0.49, by = 0.01), seq(0.51, 0.99, by = 0.01))
df_20 <- simulate_power_tests_Z(p2 = p2, n = 20, num_simulations = 1000)
df_50 <- simulate_power_tests_Z(p2 = p2, n = 50, num_simulations = 1000)
df_100 <- simulate_power_tests_Z(p2 = p2, n = 100, num_simulations = 1000)
df_1000 <- simulate_power_tests_Z(p2 = p2, n = 1000, num_simulations = 1000)

view(df_20)
ggplot(data = df_20) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z, color = 'Moc testu Z')) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z0, color = 'Moc testu Z0')) +
  labs(title = 'Porównanie mocy testów Z i Z0; n=20',
       x = expression(italic(p[2])),
       y = 'Wartości mocy testów',
       color = 'Typ testu') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c('Moc testu Z' = 'blue', 'Moc testu Z0' = 'red'))

ggplot(data = df_50) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z, color = 'Moc testu Z')) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z0, color = 'Moc testu Z0')) +
  labs(title = 'Porównanie mocy testów Z i Z0; n=50',
       x = expression(italic(p[2])),
       y = 'Wartości mocy testów',
       color = 'Typ testu') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c('Moc testu Z' = 'blue', 'Moc testu Z0' = 'red'))

ggplot(data = df_100) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z, color = 'Moc testu Z')) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z0, color = 'Moc testu Z0')) +
  labs(title = 'Porównanie mocy testów Z i Z0; n=100',
       x = expression(italic(p[2])),
       y = 'Wartości mocy testów',
       color = 'Typ testu') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c('Moc testu Z' = 'blue', 'Moc testu Z0' = 'red'))


ggplot(data = df_1000) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z, color = 'Moc testu Z')) +
  geom_line(mapping = aes(x = Probabilities, y = Power_Z0, color = 'Moc testu Z0')) +
  labs(title = 'Porównanie mocy testów Z i Z0; n=1000',
       x = expression(italic(p[2])),
       y = 'Wartości mocy testów',
       color = 'Typ testu') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c('Moc testu Z' = 'blue', 'Moc testu Z0' = 'red'))

## Zadanie 4. danych doł ˛aczonych do pierwszej listy zadan, na podstawie zmiennych ´
## CZY_ZADW oraz CZY_ZADW_2, zweryfikuj hipotez˛e, ze zadowolenie z wynagrodzenia w ˙
## pierwszym badanym okresie i po roku od pierwszego badania odpowiada modelowi symetrii.
## Czy na podstawie uzyskanych wników mozemy wnioskowa ˙ c,´ ze poziom zadowolenia z ˙
## wynagrodzenia nie uległ zmianie? Przyjmij poziom istotnosci 0 ´ .05


# Z wykładu wiemy że Rozkład {pij : i, j = 1, . . . , I} 
# podlega modelowi symetrii wtedy i tylko wtedy, gdy
# pij = pji, ∀i, j = 1, . . . , I. W przypadku tabeli dwuwymiarowej 2 × 2, 
# testy symetrii s ˛a równie ˙z testami jednorodnosci rozkładów brzegowych.
# Sprawdzimy zatem różne testy statystystyczne. Test warunkowy symetrii, 
# test Z, test Z0, test McNemara

table_CZY_ZADOW_CZY_ZADOW_2 <- table(data$CZY_ZADOW, data$CZY_ZADOW_2)
table_CZY_ZADOW_CZY_ZADOW_2 

df_zad4 <- data.frame(
  Test = c('Test warunkowy symetrii', 'Test Z', 'Test Z0', 'Test McNemara'),
  P_value = c(
    conditional_symetry_test(table_CZY_ZADOW_CZY_ZADOW_2),
    test_Z(table_CZY_ZADOW_CZY_ZADOW_2),
    test_Z0(table_CZY_ZADOW_CZY_ZADOW_2),
    mcnemar.test(table_CZY_ZADOW_CZY_ZADOW_2)$p.value
  )
)
view(df_zad4)
xtable(df_zad4, digits = c(4, 4, 4))

## zadanie 5. W korporacji, o której mowa w zadaniu 1 z listy 1, wdrozono pewne ˙
## działania w celu poprawy komfortu pracy. Nast˛epnie badan ˛a grup˛e respondentów 
## ponownie poproszono o odpowied´z na pytanie dotycz ˛ace oceny podejscia firmy 
## do utrzymania równowagi ´ mi˛edzy zyciem zawodowym a prywatnym. 
## W Tabeli 2 przedstawiono tablic˛e dwudzielcz ˛a ˙ uwzgl˛edniaj ˛ac ˛a odpowiedzi 
## na pytanie w obu tych okresach. Na podstawie danych zweryfikuj hipotez˛e, 
## ze odpowiedzi w pierwszym badanym okresie i w drugim okresie odpowiadaj ˛a ˙
## modelowi symetrii. Na podstawie wyników uwzyskanych przy weryfikacji hipotezy 
## dotycz ˛acej symetrii, sformułuj wniosek dotycz ˛acy hipotezy, ze ocena podej ˙ scia 
## firmy nie uległa zmianie.

# Testy symetrii dla tabeli większych wymiarów niż 2x2 to między innymi test Bowkera oraz
# test oparty na ilorazie wiarygodności. Test Bowkera jest wykonywany przez
# funkcje mcnemar.test w R. Podczas analizy większej liczby obserwacji właśnie to
# on jest wykonywany

# test Bowkera
works_approach <- matrix(c(10, 2, 1, 1, 0, 
                           0, 15, 1, 1, 0,
                           1, 1, 32, 6, 0,
                           0, 0, 1, 96, 3,
                           1, 1, 0, 1, 26), 
                         nrow = 5, byrow = TRUE, 
                         dimnames = list("Pytanie 1" = c(-2, -1, 0, 1, 2),
                                         "Pytanie 2" = c(-2, -1, 0, 1, 2)))

test_bowker <- mcnemar.test(works_approach, correct = FALSE)
print(test_bowker)


# test IW został wykonany w sposób analogiczny jak na wykładzie
library(gnm)
count <- c(10, 2, 1, 1, 0, 
           0, 15, 1, 1, 0,
           1, 1, 32, 6, 0,
           0, 0, 1, 96, 3,
           1, 1, 0, 1, 26)

Pytanie1 <- gl(5, 5, labels = c(-2, -1, 0, 1, 2))
Pytanie2 <- gl(5, 1, labels = c(-2, -1, 0, 1, 2))
Dane <- data.frame(Pytanie1, Pytanie2, count)

symmetry <- glm(count~Symm(Pytanie1, Pytanie2), data=Dane,
                family = poisson)
x=symmetry$deviance
r=5
p=1-pchisq(x,r) # 0.02050225

cat('P-wartosc testu IW:', p)

# problem 6

entire_population <- addmargins(array(data = c(117, 177, 104, 44), 
                                      dim = c(2,2), 
                                      dimnames = list("treatment" = c("A","B"),
                                                      "treatment result" = c("yes","no"))))
entire_population


with_comorbidities <- addmargins(array(data = c(17, 2, 101, 36), 
                                       dim = c(2,2), 
                                       dimnames = list("treatment" = c("A","B"),
                                                       "treatment result" = c("yes","no")
                                       )))
with_comorbidities


without_comorbidities <- addmargins(array(data = c(100, 175, 3, 8), 
                                          dim = c(2,2), 
                                          dimnames = list("treatment" = c("A","B"),
                                                          "treatment result" = c("yes","no")
                                          )))
without_comorbidities

### Probability of improvement applying A treatment 
prob_A_entire_population <- entire_population[1,1]/entire_population[1,3]
prob_A_with_comorbidities <- with_comorbidities[1,1]/with_comorbidities[1,3]
prob_A_without_comorbidities <- without_comorbidities[1,1]/without_comorbidities[1,3]

### Probability of improvement applying B treatment 
prob_B_entire_population <- entire_population[2,1]/entire_population[2,3]
prob_B_with_comorbidities <- with_comorbidities[2,1]/with_comorbidities[2,3]
prob_B_without_comorbidities <- without_comorbidities[2,1]/without_comorbidities[2,3]

probs_comparison_df <- data.frame(
  prob_of_improvement_given_A = c(prob_A_entire_population, prob_A_with_comorbidities, prob_A_without_comorbidities),
  prob_of_improvement_give_B  = c(prob_B_entire_population, prob_B_with_comorbidities, prob_B_without_comorbidities)
)
rownames(probs_comparison_df) <- c("Entire population", "With comorbidities", " Without comorbidities")
view(probs_comparison_df)

binom.test(entire_population[1,1], entire_population[1,3], prob_B_entire_population,alternative = "less") ## p_val < 2e-16

binom.test(with_comorbidities[1,1], with_comorbidities[1,3], prob_B_with_comorbidities,alternative = "less") ## p_val = 1

binom.test(without_comorbidities[1,1], without_comorbidities[1,3], prob_B_without_comorbidities,alternative = "less") ## p_val = 0.83

## Binomial test at conf.level = 0.95 allows to state, that there happens to be a Simpsons' paradox

prop.test(entire_population[1,1], entire_population[1,3], prob_B_entire_population,alternative = "less") ## p_val < 2e-16

prop.test(with_comorbidities[1,1], with_comorbidities[1,3], prob_B_with_comorbidities,alternative = "less") ## p_val = 1

prop.test(without_comorbidities[1,1], without_comorbidities[1,3], prob_B_without_comorbidities,alternative = "less") ## p_val = 0.83

## From the results of analysis of an entire population one should think, that probability of improvement applying treatment A
## is lower than probability of improvement applying treatment B. What is paradoxical, is the fact, that if we consider also
## 3rd variable - fact of comorbidities presence, the direction of our relation changes. Let's test those hypotheses using prop.test
## and check whether p_values will lead to Simspon's paradox or not.

### problem 8

data$CZY_KIER <- as.factor(data$CZY_KIER)
data$PYT_2 <- as.factor(data$PYT_2)
data$STAZ <- as.factor(data$STAZ)

table_data <- xtabs(~ CZY_KIER + PYT_2 + STAZ, data = data)

table_data
addmargins(table_data)


counts_of_combinations <- as.data.frame(as.table(table_data))
counts_of_combinations[,-4] <- lapply(counts_of_combinations[,-4], relevel, ref = 1) #ustawiamy referencje na 1
counts_of_combinations

model_123 <-  glm(Freq ~ CZY_KIER + PYT_2 + STAZ + 
                    (CZY_KIER*PYT_2) + 
                    (CZY_KIER*STAZ) + 
                    (PYT_2*STAZ) +
                    (CZY_KIER*PYT_2*STAZ), data = counts_of_combinations, family = poisson)
summary(model_123)

model_12_23<- glm(Freq ~ CZY_KIER + PYT_2 + STAZ + 
                    (CZY_KIER*PYT_2) + 
                    (PYT_2*STAZ), data = counts_of_combinations, family = poisson)

summary(model_12_23)

fitted_models_freqs <- cbind(model_123$data, fitted(model_123), fitted(model_12_23))
## a)
fitted_models_freqs %>% filter(CZY_KIER == "Tak") %>% filter(PYT_2 == 2) %>% summarise(sum(Freq))/(fitted_models_freqs %>% 
                                                                                                     filter(CZY_KIER == "Tak") %>% summarise(sum(Freq))) ## ~ 0.4814815

fitted_models_freqs %>% filter(CZY_KIER == "Tak") %>% filter(PYT_2 == 2) %>% summarise(sum(`fitted(model_123)`))/(fitted_models_freqs %>% 
                                                                                                                    filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_123)`))) ## ~ 0.481485

fitted_models_freqs %>% filter(CZY_KIER == "Tak") %>% filter(PYT_2 == 2) %>% summarise(sum(`fitted(model_12_23)`))/(fitted_models_freqs %>% 
                                                                                                                      filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_12_23)`))) ## ~ 0.4814815
## b)
fitted_models_freqs %>% filter(STAZ == 1) %>% filter(CZY_KIER == "Tak") %>% summarise(sum(Freq))/(fitted_models_freqs %>% 
                                                                                                    filter(STAZ == 1) %>% summarise(sum(Freq))) ## ~ 0.02

fitted_models_freqs %>% filter(STAZ == 1)  %>% filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_123)`))/(fitted_models_freqs %>% 
                                                                                                                    filter(STAZ == 1) %>% summarise(sum(`fitted(model_123)`))) ## ~ 0.02

fitted_models_freqs %>% filter(STAZ == 1)  %>% filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_12_23)`))/(fitted_models_freqs %>% 
                                                                                                                      filter(STAZ == 1) %>% summarise(sum(`fitted(model_12_23)`))) ## ~0.12
## c)
1 - fitted_models_freqs %>% filter(STAZ == 3) %>% filter(CZY_KIER == "Tak") %>% summarise(sum(Freq))/(fitted_models_freqs %>% 
                                                                                                        filter(STAZ == 3) %>% summarise(sum(Freq))) ## ~ 0.52

1 - fitted_models_freqs %>% filter(STAZ == 3)  %>% filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_123)`))/(fitted_models_freqs %>% 
                                                                                                                        filter(STAZ == 3) %>% summarise(sum(`fitted(model_123)`))) ## ~ 0.52

1 - fitted_models_freqs %>% filter(STAZ == 3)  %>% filter(CZY_KIER == "Tak") %>% summarise(sum(`fitted(model_12_23)`))/(fitted_models_freqs %>% 
                                                                                                                          filter(STAZ == 3) %>% summarise(sum(`fitted(model_12_23)`))) ## ~0.77
#### zadanie 9
## a)
model_1_2_3 <-  glm(Freq~CZY_KIER + PYT_2 + STAZ, data = counts_of_combinations, family = poisson)
model_12_23_13 <-  glm(Freq~(CZY_KIER + PYT_2 + STAZ)^2, data = counts_of_combinations, family = poisson)

##testowanie bez ustalania przeciwko h1: model nie jest modelem [1 2 3]
1-pchisq(deviance(model_1_2_3), df = df.residual(model_1_2_3)) # ~0.006
### h1:model [12 23 13]
test_a_1 <- anova(model_1_2_3, model_12_23_13)
1-pchisq(test_a_1$Deviance[2],df = test_a_1$Df[2]) # 2.77101e-05
### h1:model [123]
test_a_2 <- anova(model_1_2_3, model_123)
1-pchisq(test_a_2$Deviance[2],df = test_a_2$Df[2]) #0.0006


## b)
model_12_3 <- glm(Freq ~ CZY_KIER + PYT_2 + STAZ +(CZY_KIER*PYT_2), data = counts_of_combinations, family = poisson)

##testowanie bez ustalania przeciwko h1: model nie jest modelem [12 3]
1-pchisq(deviance(model_12_3), df = df.residual(model_12_3)) # ~0.002
##testowanie z ustalona h1: [123]
test_b_1 <- anova(model_12_3, model_123)
1 - pchisq(test_b_1$Deviance[2], df = test_b_1$Df[2])# ~ 0.002
##testowanie z ustalona h1: [12 23 31]
test_b_2 <- anova(model_12_3, model_12_23_13)
1 - pchisq(test_b_2$Deviance[2], df = test_b_2$Df[2])# ~8.173e-05

## c)
## testowanie h1: m = [123] i m !=[12 23]
1 - pchisq(deviance(model_12_23) - deviance(model_123), df =df.residual(model_12_23) - df.residual(model_123)) #~0.047
## testowanie h1: m = [12 23 31] i m !=[12 23]
1 - pchisq(deviance(model_12_23) - deviance(model_12_23_13), df =df.residual(model_12_23) - df.residual(model_12_23_13)) #~0.0008

### modele [123] i [12 23] biorę z zadania 8!



