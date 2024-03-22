library(tidyverse)
library(here)
library(ggmosaic)
#setwd(paste(here(), "/report_1", sep = "")) # działa w VSCode
katalog = dirname(normalizePath(file.choose())) # wybiera sie plik na którym chcemy pracować, i najlepiej ten w którym są dane
setwd(katalog)




###################### ZADANIE 1 #################################################
# Wczytaj dane i przygotuj je do analizy. Zadbaj o odpowiednie typy zmiennych,
# zweryfikuj czy przyjmuja wartosci zgodne z powyzszym opisem, zbadaj czy nie
# wystepuja braki w danych.

# wczytanie danych - odrazu zmiana nazw kolumn ponieważ R nie odczytał polskich znaków
data <- read.csv("ankieta.csv", sep = ';', col.names = c('DZIAL', 'STAZ', 'CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PLEC', 'WIEK'))
#names(data) = c('DZIAL', 'STAZ', 'CZY_KIER', 'PYT_1', 'PYT_2', 'PYT_3', 'PLEC', 'WIEK')
view(data)

# sprawdzenie wartości w kolumnach
sort(unique(data$DZIAL))    # ok! przyjmuje wartości DK, DS, HR, IT
sort(unique(data$STAZ))     # ok! przyjmuje wartości 1, 2, 3
unique(data$CZY_KIER)       # ok! przyjmuje wartośći TAK LUB NIE
sort(unique(data$PYT_1))    # ok! przyjmuje wartosci -2, -1, 0, 1, 2
sort(unique(data$PYT_2))    # ok! przyjmuje wartosci -2, -1, 1, 2

# sprawdzenie ile jest braków danych w danych kolumnach
data %>% sapply(function(x) sum(is.na(x)))

# typy danych
str(data)
### Zauważmy, że dane DZIAŁ, STAŻ, CZY_KIER, PYT_1, PYT_2, PYT_3, PLEC 
### zgodnie z opisem danych są zmiennymi kategorycznymi, zatem więc typ zmiennych w zbiorze danych
### jest błędny, co należy zmienić

data <- data %>%
  mutate_at(vars(DZIAL, STAZ, CZY_KIER, PYT_1, PYT_2, PYT_3, PLEC), as.factor)

str(data)
########################## ZADANIE 2 ##############################################
# Utwórz zmienna WIEK_KAT przeprowadzajac kategoryzacje zmiennej WIEK korzys-
# tajac z nastepujacych przedziałów: do 35 lat, miedzy 36 a 45 lat, miedzy 46 a 55 lat,
#powyzej 55 lat.

# do 35 lat          
# miedzy 36 a 45 lat 
# miedzy 46 a 55 lat
# powyzej 55 lat

przedzialy_wiekowe <- c(0, 35, 45, 55, Inf)
nazwy_kategori <- c("0-35", "36-45", "46-55", "55+")
data$WIEK_KAT <- cut(data$WIEK, przedzialy_wiekowe, labels = nazwy_kategori, include.lowest = TRUE)
view(data)

########################### ZADANIE 3 ################################################
# Sporzadz tablice licznosci dla zmiennych: DZIAŁ, STAZ, CZY_KIER, PŁEC,
# WIEK_KAT.

amount_dzial <- data %>% group_by(DZIAL) %>% summarise(ile = n())
view(amount_dzial)

amount_staz <- data %>% group_by(STAZ) %>% summarise(ile = n())
view(amount_staz)

amount_czy_kier <- data %>% group_by(CZY_KIER) %>% summarise(ile = n())
view(amount_czy_kier)

amount_plec <- data %>% group_by(PLEC) %>% summarise(ile = n())
view(amount_plec)

amount_wiek_kat <- data %>% group_by(WIEK_KAT) %>% summarise(ile = n())
view(amount_wiek_kat)

######################### ZADANIE 4  #####################################################
# Sporządzono wykresy kołowe oraz wykresy słupkowe dla zmiennych: PYT_1 oraz PYT_2

kolory <- c("#66c2a5", "#fc8d62", "#8da0cb", "#e78ac3", "#a6d854") # paleta kolorów
# #66c2a5 <- może być to nasz kolor przewodni

# WYKRESY SŁUPKOWE

# zmienna PYT_1
data %>% 
  ggplot(aes(x = factor(PYT_1), fill = factor(PYT_1))) +
  geom_bar(fill='#66c2a5') +
  xlab('Odpowiedzi') + 
  ylab('Liczba obserwacji') +
  ggtitle('Wykres słupkowy zmiennej PYT_1', 
          subtitle = '"Jak bardzo zgadzasz się ze stwierdzeniem, że firma pozwala na elastyczne \ngodziny pracy tym samym umożliwiając zachowanie równowagi między pracą \na życiem prywatnym?"') +
  scale_x_discrete(labels = c("zdecydowanie się \n nie zgadzam", "nie zgadzam się", "nie mam zdania", "zgadzam się", "zdecydowanie \n się zgadzam")) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# zmienna PYT_2
data %>% 
  ggplot(aes(x = factor(PYT_2), fill = factor(PYT_2))) +
  geom_bar(fill='#66c2a5') +
  xlab('Odpowiedzi') + 
  ylab('Liczba obserwacji') +
  ggtitle('Wykres słupkowy zmiennej PYT_2', 
          subtitle = '"Jak bardzo zgadzasz się ze stwierdzeniem, że twoje wynagrodzenie adekwatnie \nodzwierciedla zakres wykonywanych przez ciebie obowiązków?"') +
  scale_x_discrete(labels = c("zdecydowanie się \n nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie \n się zgadzam"))
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

# WYKRESY KOŁOWE

# zmienna PYT_1
data %>% 
    ggplot(aes(x = '', fill = factor(PYT_1))) +
    geom_bar(color = 'white') +
    scale_fill_manual(values = kolory,
                      name = 'Odpowiedzi',
                      labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "nie mam zdania", "zgadzam się", "zdecydowanie się zgadzam")) +
    coord_polar('y', start = pi / 2) +
    labs(title = 'Wykres kołowy dla zmiennej PYT_1') +
    theme_void()
    

# zmienna PYT_2
data %>% 
  ggplot(aes(x = '', fill = factor(PYT_2))) +
  geom_bar(color = 'white') +
  scale_fill_manual(values = kolory,
                    name = 'Odpowiedzi',
                    labels = c("zdecydowanie się nie zgadzam", "nie zgadzam się", "zgadzam się", "zdecydowanie się zgadzam")) +
  coord_polar('y', start = pi / 2) +
  labs(title = 'Wykres kołowy dla zmiennej PYT_2') +
  theme_void()

### Z wykresu słupkowego i kołowego - najwięcej zgadza się, najmnie zdecydowanie się nie zgadzają

######################################## ZADANIE 5 ##########################################################
####5. Sporządź tablice wielodzielcze dla par zmiennych: PYT_1 i DZIAŁ, PYT_1 i STAŻ,
###PYT_1 i CZY_KIER, PYT_1 i PŁEĆ oraz PYT_1 i WIEK_KAT.


crosstab_dzial_pyt1 <-table(data$DZIAL, data$PYT_1)
print(crosstab_dzial_pyt1)
crosstab_staz_pyt1  <-table(data$STAZ, data$PYT_1)
crosstab_staz_czy_kier_pyt1 <-table(data$CZY_KIER, data$PYT_1)
crosstab_staz_płeć_pyt1 <-table(data$PLEC, data$PYT_1)
crosstab_staz_wiek_kat_pyt1 <-table(data$WIEK_KAT, data$PYT_1)


##################################### ZADANIE 6 ############################################################
###Sporządź tablicę wielodzielczą dla pary zmiennych: PYT_2 i PYT_3.

crosstab_staz_wiek_kat_pyt1 <-table(data$PYT_2, data$PYT_3)

################################### ZADANIE 7 #############################################################
###Utwórz zmienną CZY_ZADOW na podstawie zmiennej PYT_1, łącząc kategorie "nie
### zgadzam się" i "zdecydowanie się nie zgadzam" oraz "zgadzam się" i "zdecydowanie się zgadzam".



### Zmienna CZY_ZADOW przyjmuje następujące wartości:
### 1- osoba jest zadowolona
### 0 - osoba "nie ma zdania"
### -1 - osoba jest niezadowolona

data <- mutate(data, CZY_ZADOW = ifelse(as.numeric(as.character(PYT_1)) == -2, "-1",
                                        ifelse(as.numeric(as.character(PYT_1)) == -1, "-1",
                                               ifelse(as.numeric(as.character(PYT_1)) == 0, "0",
                                                      ifelse(as.numeric(as.character(PYT_1)) == 1, "1",
                                                             ifelse(as.numeric(as.character(PYT_1)) == 2, "1", "_"))))))
view(data) 

################################## ZADANIE 8 #############################################################
###Sporządź wykresy mozaikowe odpowiadające parom zmiennych: CZY_ZADOW i DZIAŁ, CZY_ZADOW i STAŻ,
###CZY_ZADOW i CZY_KIER, CZY_ZADOW i PŁE ´C oraz CZY_ZADOW i WIEK_KAT. 
###Czy na podstawie uzyskanch wykresów mo˙zna postawi´c pewne hipotezy dotyczące
###relacji między powyższymi zmiennymi? Spróbuj sformułować kilka takich hipotez

data %>% ggplot() +
  geom_mosaic(aes(x = product(CZY_ZADOW,DZIAL)), fill='#66c2a5') +
  ylab("Czy osoba jest zadowolona") + 
  xlab("Dział") + 
  ggtitle("Zadowolenie w zależności od działu ankietowanych osób") +
  theme_mosaic()

### Zauważmy, że największy odsetek osób zadowolonych ze swojej sytuacji badaej w firmie
### przypada na dział IT. Nic w tym dziwnego- zarobki są atrakcyjne oraz sama praca z reguły 
### jest dla osób, które w tej pracy lubią się spełniać- praca w dziale komunikacji jest
### łatwiej "dostępna" i osiągalna, co niesie za sobą ryzyko związane z tym, że ludzie tam pracujący
### nie robią tego z pasji.

data %>% ggplot() +
  geom_mosaic(aes(x = product(CZY_ZADOW, STAZ)), fill='#66c2a5') +
  ylab("Czy osoba jest zadowolona") + 
  xlab("Długość stażu") + 
  ggtitle("Zadowolenie w zależności od długości stażu ankietowanych osób") + 
  theme_mosaic()

### Z kolei tutaj zauważamy największe zadowolenie wśród stażystów ze stażem w długości 
### od roku do 3 lat. Jest to dosyć długi czas, jak na staż, jednakże może być to związane z poczuciem
### integralności w firmie, reputacja na swój temat oraz swojej pozycji w firmie rośnie.
### Stażyści powyżej 3 lat mają podzielone zdania- jest to już bardzo długi okres jeżeli chodzi
### o stanowiska stażowe, pracownicy z pewnością chcieliby zostać bardziej docenieni, bardziej
### poważani wśród innych pracowników. 

data %>% ggplot() +
  geom_mosaic(aes(x = product(CZY_ZADOW,CZY_KIER)), fill='#66c2a5') +
  ylab("Czy osoba jest zadwolona") + 
  xlab("Czy osoba jest kierownikiem") + 
  ggtitle("Zadowolenie w zależności od bycia na stanowisku kierowniczym \n wśród ankietowanych osób") +
  theme_mosaic() 

### Zdecydowanie widizmy wieksze, dominujące zadowolenie wśród pracowników niebędących kierownikami.
### Stanowiska kierownicze rządzą się oczywiście swoimi prawami, więc nie powinno to budzić wątpliwości. 

data %>% ggplot() +
  geom_mosaic(aes(x = product(CZY_ZADOW,PLEC)), fill='#66c2a5') +
  ylab("Czy osoba jest zadowolona") + 
  xlab("Płeć") + 
  ggtitle("Zadowolenie w zależności od płci ankietowanych osób") + 
  theme_mosaic() 

### Jeżeli rozważymy zadowolenie w zależności od płci ankietowanych osób, to tutaj nie ma większych różnic.
### Zdecydowanie więcej ankietowanych to mężczyźni, jednakże odsetek osób zadowolonych wśród obu płci jest
### podobny. Można wnioskować o równym traktowaniu pracowników firmy.

data %>% ggplot() +
  geom_mosaic(aes(x = product(CZY_ZADOW,WIEK_KAT)), fill='#66c2a5') +
  ylab("Czy osoba jest zadwolona") + 
  xlab("Przedział wiekowy") + 
  ggtitle("Zadowolenie w zależności od wieku ankietowanych osób") + 
  theme_mosaic() 

### Największa ilość pracowników przypada na przedział wiekowy 36-45.
### Jednymi z najbardziej zadowolnych pracowników są ci w wieku 46-55.


### Ułożenie wykresów mozaikowych przeważnie sugeruje na pewne wzorce w zadowoleniu społeczeństwa.
### Najbardziej niezależnym aspektem jest płeć człowieka.
