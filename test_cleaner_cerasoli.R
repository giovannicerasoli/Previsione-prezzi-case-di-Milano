################################################################################

# Giovanni Giacomo Cerasoli, matricola 932969 ----------------------------------

################################################################################

### PULIZIA DATASET DI TEST ####################################################

################################################################################

knitr::opts_chunk$set(collapse = TRUE)

rm(list = ls())

data = read.csv(
  "C:/Users/ceras/OneDrive/Giogiò/UniMIB/Data Mining/challenge/test.csv", 
  header = TRUE, row.names = "ID")

library(geosphere)
library(visdat)
library(ggcorrplot)
library(VGAM)
library(mice)
library(stringr)
library(lubridate)
library("MASS")
library(tidyverse)


################################################################################

### TRASFORMAZIONI VARIABILI ###################################################

################################################################################

# OTHER_FEATURES:

# modifico la variaible other_featrues inserendo il separatore | quando è 
# presente il materiale pvc nelle finestre
# (ora è attaccato all'informazione successiva)
# si fa lo stesso lavoro tra property land e 1 balcony  
data$other_features = data$other_features %>%
  gsub("(pvc)([a-zA-Z])", "pvc | \\2", .) %>%
  gsub("(property land)([0-9])", "property land | \\2", .)

# creo un nuovo dataset (data2) con all'interno variabili dummy relative alla  
# variabile other_features: viene inserito 1 quando è presente quella 
# caratteristica, 0 altrimenti

# separo le features
data2 = data %>%
  mutate(row_id = row_number()) %>%
  separate_rows(other_features, sep = " \\| ") %>%
  mutate(other_features = trimws(other_features))

# creo colonne dummies (1 se presente, 0 se assente)
data2 = data2 %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = row_id,
    names_from = other_features,
    values_from = value,
    values_fill = list(value = 0)
  )

# visualizzo la struttura e le frequenze del nuovo dataset 
str(data2) 

skimr::skim(data2) 

# elimino colonna row_id poiché inutile per la previsione 
data2 = data2[ , -1]

# ---------------------------------------------------------------------------- #

# unisco in un'unica variabile dummy le variabili
# half-day concierge, full concierge, reception
data2["concierge/reception"] = ifelse(data2$`half-day concierge` == 1 | 
                                        data2$`full day concierge` == 1 |
                                        data2$reception == 1, 1, 0)

# ---------------------------------------------------------------------------- #

# creo variabile categorica exposure con categorie (internal, external, double)
# per sostituire le variabili dummy internal exposure, external exposure,
# double exposure, metto NA se non è nessuna delle tre
data2 = data2 %>% 
  mutate(exposure = case_when(
    `external exposure` == 1 ~ "external",
    `internal exposure` == 1 ~ "internal",
    `double exposure` == 1 ~ "double",
    TRUE ~ NA))

# ---------------------------------------------------------------------------- #

# unisco in un'unica variabile dummy le variabili:
# balcony, 6 balconies, 8 balconies, 1 balcony;
data2["balconies"] = ifelse(data2$balcony == 1 |
                              # data2$`6 balconies` == 1 | non presente nel test
                              # data2$`8 balconies`| non presente nel test
                              data2$`1 balcony`, 1, 0)

# ---------------------------------------------------------------------------- #

# creo variabile categorica garden con categorie: 
# shared, private, no garden per sostituire le variabili dummies 
# private garden, shared garden, private and shared garden 
# (nessuna osservazione ha 1 per almeno due di esse e le osservazioni con 
# private and shared vengono messe in private)
data2 = data2 %>% 
  mutate(garden = case_when(
    `private garden` == 1 ~ "private",
    `shared garden` == 1 ~ "shared",
    `private and shared garden` == 1 ~ "private",
    TRUE ~ "no garden"))

# ---------------------------------------------------------------------------- #

# creo una variabile categorica furnished con categorie (total, partially)
# per sostituire le variabili dummy furnished, partially furnished
data2 = data2 %>% 
  mutate(furnished = case_when(
    `furnished` == 1 ~ "total",
    `partially furnished` == 1 ~ "partially",
    TRUE ~ "no"))

# ---------------------------------------------------------------------------- #

# esiste solo 1 casa con disabled access, dunque verrà eliminata
table(data2$`disabled access`)

# ---------------------------------------------------------------------------- #

# elimino:
# - le variabili trattate sopra (tranne la sovrascritta furnished),
# - quelle relative alla direzione dell'esposizione (north, south, west, east)  
# - quelle relative al sistema TV
# - quelle relative alla cucina 
# - quella relativa all'accesso disabili 
# - la colonna NA poiché quelle case non hanno alcuna caratteristica presente
#   nelle altre variabili e dunque l'informazione è contenuta nelle variabili
#   rimanenti
# - quelle relative al sistema tv
# - property land

data2 = data2 %>%
  dplyr::select(-c(`half-day concierge`, `full day concierge`, reception, 
                   `internal exposure`, `external exposure`, `exposure north, south`, 
                   `exposure east, west`, `exposure south, east`, 
                   `exposure north, west`, `exposure east`, 
                   `exposure north, south, west`,`exposure north, east, west`, 
                   `exposure west`, `exposure south`, `exposure south, west`, 
                   `exposure south, east, west`, `exposure north`,
                   `exposure north, south, east, west`, `exposure north, east`,
                   `exposure north, south, east`, `partially furnished`,
                   balcony, 
                   #`property land`, 
                   `1 balcony`, 
                   #`6 balconies`,`8 balconies`,
                   `centralized tv system`, `single tv system`, 
                   `tv system with satellite dish`, `shared garden`, 
                   `private garden`, `private and shared garden`, `NA`, 
                   `window frames in double glass / pvc`, 
                   `window frames in glass / wood`, 
                   `window frames in glass / metal`,
                   `window frames in double glass / metal`, 
                   `window frames in double glass / wood`,
                   `window frames in triple glass / pvc`, 
                   `window frames in glass / pvc`,
                   `window frames in triple glass / wood`, 
                   `window frames in triple glass / metal`, `double exposure`,
                   kitchen, `only kitchen furnished`, `disabled access`))

# ---------------------------------------------------------------------------- #

# visualizzo di che tipo sono le variabili rimaste in data2
vis_dat(data2)

# trasformo in factor tutte le dummies che ora sono numeriche
data2 = data2 %>%
  mutate(across(c(`optic fiber`, `security door`, cellar, 
                  `video entryphone`, `alarm system`, closet, 
                  `electric gate`, terrace, hydromassage,
                  fireplace, tavern, attic, pool, `tennis court`,
                  `concierge/reception`, balconies),
                as.factor))

vis_dat(data2)

# unisco le dummies rimanenti al dataset ed elimino la variabile other_features
data = cbind(data, data2)

data = data %>%
  dplyr::select(-c(other_features))

################################################################################

# SQUARE_METERS:

class(data$square_meters) # è integer

# trasformo square_meters in numeric
data$square_meters = as.numeric(data$square_meters)

table(data$square_meters, useNA = "always") # 0 NA

# le case con metratura da 12mq in giù si ritengono mal imputate 
# (sono 7 osservazioni): gli ingressi delle altre variabili
# (tipo bathrooms_number e rooms_number) sono totalmente sballati in relazione
# con queste metrature; si decide quindi di sostituire questi valori con NA
data = data %>%
  mutate(square_meters = case_when(
    square_meters <= 12 ~ NA,
    TRUE ~ square_meters))

table(data$square_meters, useNA = "always") # 3 NA

################################################################################

# LIFT:

table(data$lift, useNA = "always") # 90 NA

# metto 1 se lift = yes, metto 0 se lift = no
data$lift = as.numeric(as.factor(data$lift)) - 1

class(data$lift) # è numeric

# trasformo lift in factor
data$lift = as.factor(data$lift)

################################################################################

# TOTAL_FLOORS_IN_BUILDING:

table(data$total_floors_in_building, useNA = "always") # 40 NA

# converto la categoria "1 floor" in "1"
data$total_floors_in_building[data$total_floors_in_building == "1 floor"] = 1

# unisco tutte le case con 10 piani o più (sono 246)
data = data %>%
  mutate(total_floors_in_building = case_when(
    as.numeric(total_floors_in_building) >= 10 ~ "9+",
    TRUE ~ as.character(total_floors_in_building)))

class(data$total_floors_in_building) # è character

# character total_floors_in_building in character
data$total_floors_in_building = as.factor(data$total_floors_in_building)

################################################################################

# CAR_PARKING:

table(data$car_parking, useNA = "always") # 0 NA

levels(as.factor(data$car_parking)) # ci sono 19 possibili valori

# diminuisco le categorie: passo a cinque (1 garage/box, 2 garage/box,
# 2+ garage/box, no, shared)
data <- data %>%
  mutate(car_parking = case_when(
    str_detect(car_parking, "^1 in garage/box") ~ "1 garage/box",
    str_detect(car_parking, "^2 in garage/box") ~ "2 garage/box",
    str_detect(car_parking, "^[3-9][0-9]* in garage/box") ~ "2+ garage/box",
    car_parking == "no" ~ "no",
    TRUE ~ "shared parking" ))

table(data$car_parking) # c'è solo un'osservazione con 2+ garage/box 

# unisco 2 garage/box e 2+ garage/box in 1+ garage/box
data <- data %>%
  mutate(car_parking = case_when(
    car_parking %in% c("2 garage/box", "2+ garage/box") ~ "1+ garage/box",
    TRUE ~ car_parking))

table(data$car_parking)

class(data$car_parking) # è character

# trasformo car_parking in factor ordinati (le categorie sono ordinabili)
data$car_parking = factor(
  data$car_parking,
  levels = c("no", "shared parking", "1 garage/box", "1+ garage/box"),
  ordered = TRUE)

################################################################################

# AVAILABILITY:

table(data$availability, useNA = "always") # 638 NA

# diminuisco le categorie della variabile:  
# raggruppo nella categoria "available in 2 years" le case disponibili nel 2023 
# e nel 2024 e nella categoria "available in more than 2 years" quelle 
# disponibili dal 2025 in poi;
# si sceglie di non mettere le case segnate disponibili nel 2023 e nel 2024 
# come attualmente disponibili poiché il prezzo rilevato alla creazione del 
# dataset dovrebbe essere minore e ciò potrebbe influenzare la previsione
# (si ipotizza che il prezzo di una casa non disponibile sia inferiore di 
# quello della stessa casa disponibile)

# estraggo la data dalle stringhe e modifico le categorie
data = data %>%
  mutate(
    extracted_date = str_extract(availability, "\\d{2}/\\d{2}/\\d{4}"),
    parsed_date = dmy(extracted_date),
    
    availability = case_when(
      availability == "available" ~ "available",
      !is.na(parsed_date) & 
        parsed_date <= ymd("2024-12-31") ~ "available in 2 years",
      !is.na(parsed_date) &
        parsed_date > ymd("2024-12-31") ~ "available in more than 2 years",
      TRUE ~ NA_character_))

# elimino le variabili create che ora mi sono inutili
data = data %>%
  dplyr::select(-c(parsed_date, extracted_date))

table(data$availability, useNA = "always")

################################################################################

# CONDOMINIUM_FEES:

table(data$condominium_fees, useNA = "always") # 525 NA

# metto zero quando il valore è "No condominium fees"
data$condominium_fees[data$condominium_fees == "No condominium fees"] = 0

class(data$condominium_fees) # è character

# trasformo condominium_fees in numeric
data$condominium_fees = as.numeric(data$condominium_fees)

# reputo che i valori > 11000 siano mal imputati e li metto NA 
# (sono 3: 110000, 200000, 250000)
data = data %>%
  mutate(condominium_fees = case_when(
    condominium_fees > 11000 ~ NA,
    TRUE ~ condominium_fees))

################################################################################

# YEAR_OF_CONSTRUCTION:

table(data$year_of_construction, useNA = "always") # 493 NA

# confronto l'anno di costruzione con il momento in cui la casa è disponibile
table(data$availability, data$year_of_construction, useNA = "always")

# sposto le case con anno di costruzione 2023 e 2024 in "available in 2 years"
# e quelle con anno di costruzione dal 2025 in poi in 
# "available in more than 2 years", poichè non ha senso che siano disponibili
data = data %>%
  mutate(availability = case_when(
    year_of_construction %in% c(2023, 2024) & 
      availability == "available" ~ "available in 2 years",
    year_of_construction >= 2025 & 
      availability == "available" ~ "available in more than 2 years",
    TRUE ~ availability))

################################################################################

# ZONE:

table(data$zone, useNA = "always") # 0 NA

length(unique(data$zone)) # 147 possibilità (una è NA)

# imputo manualmente le zone diverse dal train nella zona in cui effettivamente
# si trovano o a cui sono più vicine
data = data %>%
  mutate(zone = case_when(
    zone == "via marignano, 3" ~ "quintosole - chiaravalle",
    zone == "corso magenta" ~ "cadorna - castello",     
    zone == "largo caioroli 2" ~ "cadorna - castello",
    TRUE ~ zone))

# decido di inserire per ogni zona le sue coordinate (latitudine e longitudine)
# e poi calcolare la sua distanza dal Duomo di Milano in km, in modo da avere 
# una variabile numerica che misura la distanza dal centro per ogni abitazione
dist_zone = data.frame(
  zone = c("quadronno - crocetta", "ticinese", "palestro", "brera", 
           "porta venezia", "arco della pace", "sempione", "turati",
           "borgogna - largo augusto", "missori", "vincenzo monti", "lanza", 
           "duomo", "carrobbio", "scala - manzoni", "san babila", 
           "cadorna - castello", "quadrilatero della moda",
           "porta romana - medaglie d'oro", "martini - insubria", 
           "navigli - darsena", "morgagni", "de angeli", "dezza", "pagano", 
           "isola", "indipendenza", "amendola - buonarroti", "corso genova", 
           "piave - tricolore", "cadore", "repubblica", "centrale",
           "buenos aires", "lodi - brenta", "corso san gottardo", "montenero", 
           "cenisio", "bocconi", "san carlo", "paolo sarpi", 
           "vercelli - wagner", "guastalla", "rubattino", "farini", "moscova", 
           "washington", "ascanio sforza", "solari", "melchiorre gioia", "zara", "arena", "city life", "frua", 
           "portello - parco vittoria", "garibaldi - corso como", 
           "piazza napoli", "porta nuova", "san vittore", "gallaratese", 
           "giambellino", "cermenate - abbiategrasso", "vigentino - fatima", 
           "rovereto", "pezzotti - meda", "dergano", "ghisolfa - mac mahon",
           "crescenzago", "villa san giovanni", "barona", "quartiere olmi", 
           "famagosta", "tre castelli - faenza", "viale ungheria - mecenate", 
           "cascina dei pomi", "san siro", "baggio", "cantalupa - san paolo", 
           "corvetto", "bruzzano", "bisceglie", "quinto romano", "qt8", 
           "città studi", "pasteur", "cascina merlata - musocco", "niguarda", 
           "bovisa", "cimiano", "quartiere adriano", "piazzale siena", 
           "parco trotter", "molise - cuoco", "roserio", "greco - segnano", 
           "gambara", "ortica", "rogoredo", "bignami - ponale", "certosa", 
           "tripoli - soderini", "bologna - sulmona", "udine", "precotto", 
           "monte rosa - lotto", "turro", "chiesa rossa", "bande nere", 
           "quartiere forlanini", "ponte nuovo", "gorla", "bovisasca", 
           "primaticcio", "via fra' cristoforo", "affori", "argonne - corsica",
           "quarto oggiaro", "porta vittoria", "maggiolina", "ripamonti", 
           "casoretto", "istria", "ca' granda", "vialba", "prato centenaro", 
           "quintosole - chiaravalle", "santa giulia", "inganni", "comasina", 
           "quarto cagnino", "sant'ambrogio", "gratosoglio", "monte stella", 
           "bicocca", "ponte lambro", "trenno", "lambrate", "via canelli",
           "figino", "via calizzano", "lorenteggio", "plebisciti - susa", 
           "muggiano", "quartiere feltre", "cascina gobba", "parco lambro"),
  longitude = c(9.1918549, 9.1837887, 9.1996026, 9.1874068, 9.2051853,
                9.1742533, 9.1682917, 9.1946796, 9.1980432, 9.1884906,
                9.1690085, 9.1815902, 9.1919429, 9.1813177, 9.1900182,
                9.1969990, 9.1767835, 9.1951715, 9.2101169, 9.2209893,
                9.1696288, 9.2123140, 9.1481373, 9.1616095, 9.1660462,
                9.1898099, 9.2132199, 9.1517190, 9.1755234, 9.2061651,
                9.2129602, 9.1988695, 9.2060969, 9.2110256, 9.2183182,
                9.1798076, 9.2052836, 9.1657118, 9.1871523, 9.1434214,
                9.1751551, 9.1552276, 9.2024805, 9.2554604, 9.1799501,
                9.1849250, 9.1570942, 9.1764880, 9.1623956, 9.1995499,
                9.1946357, 9.1795271, 9.1576894, 9.1498421, 9.1464863,
                9.1874564, 9.1527380, 9.1953301, 9.1713335, 9.1121123,
                9.1423274, 9.1762974, 9.2011449, 9.2194384, 9.1788326,
                9.1762181, 9.1600773, 9.2399984, 9.2266438, 9.1546830,
                9.0816431, 9.1665103, 9.1483846, 9.2508454, 9.2115598,
                9.1304605, 9.0863314, 9.1579914, 9.2242604, 9.1741345,
                9.1139569, 9.0891011, 9.1364844, 9.2236325, 9.2182812,
                9.1041991, 9.1918933, 9.1597554, 9.2426101, 9.2457965,
                9.1370530, 9.2253791, 9.2242630, 9.1242034, 9.2128533,
                9.1421790, 9.2467318, 9.2479677, 9.2071906, 9.1334870,
                9.1399727, 9.2279333, 9.2366280, 9.2276605, 9.1435048,
                9.2227167, 9.1608482, 9.1361252, 9.2447626, 9.2370955,
                9.2249727, 9.1561871, 9.1303891, 9.1723419, 9.1723099,
                9.2289088, 9.1375854, 9.2236994, 9.2013277, 9.2036471,
                9.2282325, 9.1980441, 9.2009948, 9.1291373, 9.1973050,
                9.2085134, 9.2408535, 9.1239775, 9.1633473, 9.1090585,
                9.1638820, 9.1715555, 9.1344428, 9.2075123, 9.2639241,
                9.1010977, 9.2512533, 9.2528104, 9.0771489, 9.1610926,
                9.1409823, 9.2200812, 9.0728595, 9.2448493, 9.2641956,
                9.2419599),
  latitude = c(45.4543369, 45.4555950, 45.4730368, 45.4712356, 45.4721710,
               45.4768829, 45.4751540, 45.4752910, 45.4647364, 45.4607307,
               45.4710942, 45.4713291, 45.4641892, 45.4602594, 45.4680566,
               45.4660079, 45.4681260, 45.4682332, 45.4544252, 45.4539556,
               45.4477479, 45.4786499, 45.4690621, 45.4602568, 45.4715433,
               45.4876665, 45.4676288, 45.4711478, 45.4572295, 45.4678489,
               45.4583853, 45.4805564, 45.4839709, 45.4811965, 45.4427597,
               45.4495428, 45.4567872, 45.4877441, 45.4499951, 45.4830863,
               45.4817502, 45.4684910, 45.4593325, 45.4782664, 45.4932121,
               45.4776319, 45.4629993, 45.4447658, 45.4552222, 45.4884801,
               45.4959496, 45.4562075, 45.4783834, 45.4648742, 45.4887850,
               45.4829767, 45.4529223, 45.4777182, 45.4600816, 45.4302539,
               45.4487613, 45.3990913, 45.4331793, 45.4955590, 45.4418846,
               45.5042041, 45.4948158, 45.5036758, 45.5203204, 45.4373609,
               45.4536934, 45.4373777, 45.4370742, 45.4509889, 45.4972552,
               45.4789394, 45.4622552, 45.4184398, 45.4401682, 45.5265714,
               45.4553814, 45.4769300, 45.4848397, 45.4797591, 45.4911672,
               45.5111849, 45.5179464, 45.5065037, 45.4976348, 45.5163865,
               45.4646212, 45.4929162, 45.4538568, 45.5190854, 45.5048036,
               45.4649337, 45.4699845, 45.4287264, 45.5231604, 45.4981439,
               45.4559037, 45.4446635, 45.4910622, 45.5123635, 45.4795257,
               45.4992513, 45.4067542, 45.4611334, 45.4594607, 45.5078780,
               45.5061964, 45.5188894, 45.4568407, 45.4354349, 45.5157980,
               45.4684086, 45.5107230, 45.4598303, 45.4962340, 45.4244082,
               45.4893009, 45.5018453, 45.5071510, 45.5163128, 45.5108157,
               45.4045183, 45.4350595, 45.4549824, 45.5286305, 45.4715679,
               45.4321961, 45.4131957, 45.4908844, 45.5185100, 45.4431605,
               45.4924291, 45.4838657, 45.4902390, 45.4925633, 45.5296062,
               45.4536158, 45.4678921, 45.4495831, 45.4913758, 45.5101342,
               45.4977392
))
  

# salvo le coordinate del Duomo
duomo = c(9.19429, 45.4641892)

# creo una variabile numerica che misura la distanza dal Duomo in km 
dist_zone = dist_zone %>%
  rowwise() %>%
  mutate(distance_from_duomo_km = ifelse(!is.na(latitude) & !is.na(longitude),
                                         distHaversine(c(longitude, latitude), 
                                                       duomo) / 1000, NA_real_))

# creo la variabile distance (numerica)
data = data %>%
  left_join(dist_zone, by = "zone") %>%
  mutate(distance = distance_from_duomo_km) %>%
  dplyr::select(-longitude, -latitude, -distance_from_duomo_km)

# assegno ad ogni zona la nuova zona 
nuove_zone = c(
  "affori" = 1,
  "amendola - buonarroti" = 2,
  "arco della pace" = 3,
  "arena" = 4,
  "argonne - corsica" = 5,
  "ascanio sforza" = 6,
  "baggio" = 7,
  "bande nere" = 8,
  "barona" = 9,
  "bicocca" = 10,
  "bignami - ponale" = 1,
  "bisceglie" = 11,
  "bocconi" = 12,
  "bologna - sulmona" = 1,
  "borgogna - largo augusto" = 13,
  "bovisa" = 10,
  "bovisasca" = 7,
  "brera" = 14,
  "bruzzano" = 11,
  "buenos aires" = 12,
  "ca' granda" = 1,
  "cadore" = 12,
  "cadorna - castello" = 13,
  "cantalupa - san paolo" = 1,
  "carrobbio" = 3,
  "cascina dei pomi" = 9,
  "cascina gobba" = 11,
  "cascina merlata - musocco" = 10,
  "casoretto" = 8,
  "cenisio" = 15,
  "centrale" = 6,
  "cermenate - abbiategrasso" = 10,
  "certosa" = 10,
  "chiesa rossa" = 1,
  "cimiano" = 1,
  "città studi" = 15,
  "city life" = 16,
  "comasina" = 7,
  "corso genova" = 2,
  "corso san gottardo" = 12,
  "corvetto" = 10,
  "crescenzago" = 1,
  "de angeli" = 6,
  "dergano" = 10,
  "dezza" = 12,
  "duomo" = 17,
  "famagosta" = 9,
  "farini" = 5,
  "figino" = 7,
  "frua" = 12,
  "gallaratese" = 11,
  "gambara" = 8,
  "garibaldi - corso como" = 3,
  "ghisolfa - mac mahon" = 9,
  "giambellino" = 10,
  "gorla" = 10,
  "gratosoglio" = 7,
  "greco - segnano" = 10,
  "guastalla" = 4,
  "indipendenza" = 2,
  "inganni" = 1,
  "isola" = 12,
  "istria" = 10,
  "lambrate" = 10,
  "lanza" = 13,
  "lodi - brenta" = 5,
  "lorenteggio" = 10,
  "maggiolina" = 5,
  "martini - insubria" = 5,
  "melchiorre gioia" = 12,
  "missori" = 3,
  "molise - cuoco" = 1,
  "monte rosa - lotto" = 6,
  "monte stella" = 10,
  "montenero" = 12,
  "morgagni" = 12,
  "moscova" = 13,
  "muggiano" = 7,
  "navigli - darsena" = 6,
  "niguarda" = 11,
  "ortica" = 10,
  "pagano" = 3,
  "palestro" = 16,
  "paolo sarpi" = 2,
  "parco lambro" = 11,
  "parco trotter" = 10,
  "pasteur" = 9,
  "pezzotti - meda" = 8,
  "piave - tricolore" = 4,
  "piazza napoli" = 15,
  "piazzale siena" = 9,
  "plebisciti - susa" = 6,
  "ponte lambro" = 7,
  "ponte nuovo" = 1,
  "porta nuova" = 3,
  "porta romana - medaglie d'oro" = 12,
  "porta venezia" = 2,
  "porta vittoria" = 6,
  "portello - parco vittoria" = 15,
  "prato centenaro" = 1,
  "precotto" = 9,
  "primaticcio" = 10,
  "qt8" = 8,
  "quadrilatero della moda" = 20,
  "quadronno - crocetta" = 4,
  "quartiere adriano" = 1,
  "quartiere feltre" = 10,
  "quartiere forlanini" = 10,
  "quartiere olmi" = 19,
  "quarto cagnino" = 11,
  "quarto oggiaro" = 7,
  "quinto romano" = 7,
  "quintosole - chiaravalle" = 1,
  "repubblica" = 2,
  "ripamonti" = 8,
  "rogoredo" = 11,
  "roserio" = 19,
  "rovereto" = 9,
  "rubattino" = 10,
  "san babila" = 14,
  "san carlo" = 1,
  "san siro" = 9,
  "san vittore" = 4,
  "sant'ambrogio" = 3,
  "santa giulia" = 1,
  "scala - manzoni" = 20,
  "sempione" = 12,
  "solari" = 12,
  "ticinese" = 2,
  "tre castelli - faenza" = 10,
  "trenno" = 1,
  "tripoli - soderini" = 8,
  "turati" = 13,
  "turro" = 9,
  "udine" = 9,
  "vercelli - wagner" = 2,
  "via calizzano" = 7,
  "via canelli" = 7,
  "via fra' cristoforo" = 7,
  "vialba" = 7,
  "viale ungheria - mecenate" = 1,
  "vigentino - fatima" = 10,
  "villa san giovanni" = 10,
  "vincenzo monti" = 13,
  "washington" = 12,
  "zara" = 6
)

# metto le nuove zone in data
data = data %>%
  mutate(zone = nuove_zone[as.character(zone)])

################################################################################

# FLOOR:

table(data$floor, useNA = "always") # 0 NA

# metto le entrate categoriali come numeriche per rendere la variabile 
# numerica: - se floor = semi-basement allora metto -1
#           - se floor = ground floor allora metto 0
#           - se floor = mezzanine allora metto 0.5
data = data %>%
  mutate(floor = case_when(
    floor == "semi-basement" ~ "-1",
    floor == "ground floor" ~  "0",
    floor == "mezzanine" ~ "0.5",
    TRUE ~ floor))

data$floor = as.numeric(data$floor)

################################################################################

# ENERGY_EFFICIENCY_CLASS:

table(data$energy_efficiency_class, useNA = "always") # 518 NA

# metto NA dove c'è valore ","
data = data %>%
  mutate(energy_efficiency_class = case_when(
    energy_efficiency_class == "," ~ NA,
    TRUE ~ energy_efficiency_class))

# riduco a 4 categorie: (high, medium-high, medium-low, low)
data = data %>%
  mutate(energy_efficiency_class = case_when(
    energy_efficiency_class == "a" ~ "high",
    energy_efficiency_class %in% c("b", "c")  ~  "medium-high",
    energy_efficiency_class %in% c("d", "e")  ~  "medium-low",
    energy_efficiency_class %in% c("f", "g")  ~  "low",
    TRUE ~ energy_efficiency_class))

# ordino tali categorie
data = data %>%
  mutate(energy_efficiency_class = factor(energy_efficiency_class, 
                                          levels = c("low", "medium-low", 
                                                     "medium-high", "high"), 
                                          ordered = TRUE))

################################################################################

# ROOMS_NUMBER:

table(data$rooms_number, useNA = "always") # 0 NA

################################################################################

# BATHROOMS_NUMBER:

table(data$bathrooms_number, useNA = "always") # 14 NA

################################################################################

# CONDITIONS:

table(data$conditions, useNA = "always") # 42 NA

################################################################################

# HEATING_CENTRALIZED:

table(data$heating_centralized, useNA = "always") # 51 NA

################################################################################

# visualizzo di che tipo sono le variabili rimaste in data
vis_dat(data)

# trasformo in factor le variabili character bathrooms_number e rooms_number e
# ordino le categorie
data = data %>%
  mutate(across(c(bathrooms_number, rooms_number), as.factor))

data$bathrooms_number = factor(data$bathrooms_number,
                               levels = sort(unique(data$bathrooms_number)))

data$rooms_number = factor(data$rooms_number,
                           levels = sort(unique(data$rooms_number)))


################################################################################

### IMPUTAZIONI ################################################################

################################################################################

# SQUARE_METERS

# imputo i valori NA con una regressione lineare:
# divido il dataset in due: uno contiene le righe che hanno square_meters = NA,
# l'altro le rimanenti osservazioni

# dataset con NA in square_meters
square_meters_na = data[is.na(data$square_meters), ]

# dataset senza NA in square_meters
square_meters_not_na = data[!is.na(data$square_meters), ]

# predico square_meters con la funzione lm con predittori
# bathrooms_number e rooms_number
mod = lm(square_meters ~ bathrooms_number + rooms_number,
         data = square_meters_not_na)

pred = round(predict(mod, newdata = square_meters_na))
table(pred)

# trovo gli indici dove square_meters è NA in data
na_indices = which(is.na(data$square_meters))

# inserisco le previsioni nei punti NA
data$square_meters[na_indices] = pred

table(data$square_meters, useNA = "always") # 0 NA

################################################################################

# BATHROOMS_NUMBER:

# divido il dataset in due: uno contiene le righe che hanno 
# bathrooms_number = NA, l'altro le rimanenti osservazioni

# dataset con NA in bathrooms_number
data_bathroom_na = data[is.na(data$bathrooms_number), ]

# dataset senza NA in bathrooms_number
data_bathroom_not_na = data[!is.na(data$bathrooms_number), ]

# predico il numero di bagni con la funzione polr con predittori
# square_meters e rooms_number
mod_po_lr = polr(bathrooms_number ~ square_meters + rooms_number, 
                 data = data_bathroom_not_na)

pred = predict(mod_po_lr, newdata = data_bathroom_na)
table(pred)

# trovo gli indici dove bathrooms_number è NA in data
na_indices = which(is.na(data$bathrooms_number))

# inserisco le previsioni nei punti NA
data$bathrooms_number[na_indices] = pred

table(data$bathrooms_number, useNA = "always") # 0 NA

################################################################################

# YEAR_OF_CONSTRUCTION:

# year_of_construction e availability dovrebbero essere altamente correlate
# per cui potrei avere informazioni ridondanti (collinearità): 
# infatti le case disponibili in due anni dovrebbero avere come anno di 
# costruzione 2023 o 2024, mentre quelle con anni di costruzione successivi  
# dovrebbero essere disponibili in più di due anni; le rimanenti (costruite 
# prima del 2024) dovrebbero essere prevalentemente disponibili

ggplot(data, aes(x = availability, y = (year_of_construction))) +
  geom_boxplot(fill = "lightblue", color = "blue") +
  ylim(1900, 2030)

# dunque imputo year_of_construction usando availability e poi rimuovo 
# quest'ultimo 

# calcolo la media di year_of_construction raggruppata per conditions, 
# energy_efficiency_class, availability
media = data %>%
  group_by(conditions, energy_efficiency_class, availability) %>%
  summarize(media = as.integer(mean(year_of_construction, na.rm = TRUE)), 
            .groups = "drop")

# se non ho la combinazione delle tre variabili calcolo la media con due di esse
media2 = data %>%
  group_by(conditions, energy_efficiency_class) %>%
  summarize(media2 = as.integer(mean(year_of_construction, na.rm = TRUE)), 
            .groups = "drop")

# aggiungo i valori al dataset dove è NA
data = data %>%
  left_join(media, by = c("conditions", "energy_efficiency_class", 
                          "availability")) %>%
  left_join(media2, by = c("conditions", "energy_efficiency_class")) %>%
  mutate(year_of_construction = case_when(
    !is.na(year_of_construction) ~ year_of_construction,
    !is.na(media) ~ media,
    TRUE ~ media2
  )) %>%
  dplyr::select(-media, -media2,)

table(data$year_of_construction, useNA = "always") # 0 NA

# elimino availability 
data = data %>%
  dplyr::select(-c(availability))

################################################################################

# HEATING_CENTRALIZED:

# metto heating_centralized = 1 se exposure = indipendent, 
# 0 se heating_centralized = central
data$heating_centralized = as.numeric(factor(data$heating_centralized))-1

# divido il dataset in due: uno contiene le righe che hanno 
# heating_centralized = NA, l'altro le rimanenti osservazioni

# dataset con NA in heating_centralized
data_heating_centralized_na = data[is.na(data$heating_centralized), ]

# dataset senza NA in heating_centralized
data_heating_centralized_not = data[!is.na(data$heating_centralized), ]

# predico heating_centralized con una regressione logistica 
# (non è ordinabile) con regressori rooms_number e bathrooms_number
mod_logit = glm(heating_centralized ~ rooms_number + bathrooms_number, 
                family = binomial(link = "logit"), data = data)

pred = predict(mod_logit, newdata = data_heating_centralized_na)

pred = ifelse(pred < 0.5, 1, 0)

table(pred, useNA = "always") 

# trovo gli indici dove heating_centralized è NA in data
na_indices = which(is.na(data$heating_centralized))

# inserisco le previsioni nei punti NA
data$heating_centralized[na_indices] = pred

# rimetto le etichette precedenti 
data = data %>%
  mutate(heating_centralized = case_when(
    heating_centralized == 0 ~ "central",
    heating_centralized == 1 ~ "indipendent"))

table(data$heating_centralized, useNA = "always") # 0 NA

################################################################################

# CONDITIONS:

# ordino le categorie di conditions
data$conditions = factor(data$conditions,levels = sort(unique(data$conditions)))

# divido il dataset in due: uno contiene le righe che hanno 
# conditions = NA, l'altro le rimanenti osservazioni

# dataset con NA in conditions
conditions_na = data[is.na(data$conditions), ]

# dataset senza NA in conditions
conditions_no_na = data[!is.na(data$conditions), ]

# predico conditions con la funzione polr con predittori
# year_of_construction, zone
mod_polr = polr(conditions ~ year_of_construction + zone + furnished,
                data = conditions_no_na)

pred = predict(mod_polr, newdata = conditions_na)
table(pred, useNA = "always")

# trovo gli indici dove conditions è NA in data
na_indices = which(is.na(data$conditions))

# inserisco le previsioni nei punti NA
data$conditions[na_indices] = pred

table(data$conditions, useNA = "always") # 0 NA

################################################################################

#  ENERGY_EFFICIENCY_CLASS:

# divido il dataset in due: uno contiene le righe che hanno 
# energy_efficiency_class = NA, l'altro le rimanenti osservazioni

# ordino le categorie di energy_efficiency_class
data$energy_efficiency_class = factor(data$energy_efficiency_class,
                                      levels = sort(
                                        unique(data$energy_efficiency_class)))

# dataset con NA in energy_efficiency_class
data_energy_efficiency_class_na = data[is.na(data$energy_efficiency_class), ]

# dataset senza NA in energy_efficiency_class
data_energy_efficiency_class_not_na = data[!is.na(data$energy_efficiency_class), ]

# predico energy_efficiency_class con la funzione polr con predittori
# conditions, heating_centralized, bathrooms_number
mod_po_lr = polr(energy_efficiency_class ~ conditions + heating_centralized + 
                   bathrooms_number, 
                 data = data_energy_efficiency_class_not_na)

pred = predict(mod_po_lr, newdata = data_energy_efficiency_class_na)
table(pred, useNA = "always")

# trovo gli indici dove energy_efficiency_class è NA in data
na_indices = which(is.na(data$energy_efficiency_class))

# inserisco le previsioni nei punti NA
data$energy_efficiency_class[na_indices] = pred

table(data$energy_efficiency_class, useNA = "always") # NA

################################################################################

# TOTAL_FLOORS_IN_BUILDING:

# ordino le categorie di total_floors_in_building
data$total_floors_in_building = factor(data$total_floors_in_building,
                                       levels = sort(unique(
                                         data$total_floors_in_building)))

# divido il dataset in due: uno contiene le righe che hanno 
# total_floors_in_building = NA, l'altro le rimanenti osservazioni

# dataset con NA in total_floors_in_building
total_floors_in_building_na = data[is.na(data$total_floors_in_building), ]

# dataset senza NA in total_floors_in_building
total_floors_in_building_not_na = data[!is.na(data$total_floors_in_building), ]

# predico il numero di piani con la funzione polr con predittori
# floor, condominium_fees, year_of_construction, zone
mod_po_lr = polr(total_floors_in_building ~ floor + condominium_fees +
                   year_of_construction + zone, 
                 data = total_floors_in_building_not_na)

pred = predict(mod_po_lr, newdata = total_floors_in_building_na)
table(pred, useNA = "always")

# trovo gli indici dove total_floors_in_building è NA in data
na_indices = which(is.na(data$total_floors_in_building))

# inserisco le previsioni nei punti NA
data$total_floors_in_building[na_indices] = pred

table(data$total_floors_in_building, useNA = "always") #0 NA

################################################################################

# LIFT:

# divido il dataset in due: uno contiene le righe che hanno lift = NA,
# l'altro le rimanenti osservazioni

# dataset con NA in lift
data_lift_na = data[is.na(data$lift), ]

# dataset senza NA in lift
data_lift_not_lift = data[!is.na(data$lift), ]

# predico la presenza dell'ascensore con una regressione logistica 
# (poichè lift non è ordinabile) con regressori
mod_logit = glm(lift ~ total_floors_in_building, 
                family = binomial(link = "logit"), data = data)

pred = predict(mod_logit, newdata = data_lift_na)

pred = ifelse(pred >= 0.5, 1, 0)

table(pred, useNA = "always")

# trovo gli indici dove lift è NA in data
na_indices = which(is.na(data$lift))

# inserisco le previsioni nei punti NA
data$lift[na_indices] = pred

table(data$lift, useNA = "always") # 0 NA

################################################################################

# EXPOSURE:

# unisco i livelli external ed internal in un'unica categoria single
data = data %>%
  mutate(exposure = case_when(
    exposure %in% c("external", "internal") ~ "single",
    TRUE ~ exposure))

# metto exposure = 1 se exposure = single, 0 se exposure = double
data$exposure = as.numeric(factor(data$exposure))-1

# divido il dataset in due: uno contiene le righe che hanno 
# exposure = NA, l'altro le rimanenti osservazioni

# dataset con NA in exposure
data_exposure_na = data[is.na(data$exposure), ]

# dataset senza NA in heating_centralized
data_exposure_not = data[!is.na(data$exposure), ]

# predico exposure con una regressione logistica 
# (non è ordinabile) con regressori square_meters, rooms_number,
# bathrooms_number, energy_efficiency_class
mod_logit = glm(exposure ~ square_meters + rooms_number + bathrooms_number +
                  energy_efficiency_class, 
                family = binomial(link = "logit"), data = data)

pred = predict(mod_logit, newdata = data_exposure_na)

pred = ifelse(pred < 0.5, 1, 0)

table(pred, useNA = "always")

# trovo gli indici dove exposure è NA in data
na_indices = which(is.na(data$exposure))

# inserisco le previsioni nei punti NA
data$exposure[na_indices] = pred

table(data$exposure, useNA = "always") # 0 NA

# rimetto le etichette precedenti 
data = data %>%
  mutate(exposure = case_when(
    exposure == 0 ~ "double",
    exposure == 1 ~ "single"))

################################################################################

# CONDOMINIUM_FEES:

# calcolo la media di condominium_fees raggruppata per lift, 
# total_floors_in_building, heating_centralized
media = data %>%
  group_by(lift, total_floors_in_building, heating_centralized) %>%
  summarize(media = as.integer(mean(condominium_fees, na.rm = TRUE)), 
            .groups = "drop")

# dove non ho la combinazione delle tre variabili calcolo la media con due 
media2 = data %>%
  group_by(total_floors_in_building, heating_centralized) %>%
  summarize(media2 = as.integer(mean(condominium_fees, na.rm = TRUE)), 
            .groups = "drop")

# aggiungo i valori al dataset dove mancante
data = data %>%
  left_join(media, by = c("lift", "total_floors_in_building", 
                          "heating_centralized")) %>%
  left_join(media2, by = c("total_floors_in_building", 
                           "heating_centralized")) %>%
  mutate(condominium_fees = case_when(
    !is.na(condominium_fees) ~ condominium_fees,
    !is.na(media) ~ media,
    TRUE ~ media2
  )) %>%
  dplyr::select(-media, -media2,)

table(data$condominium_fees, useNA = "always") # 0 NA

################################################################################

skimr::skim(data) # ora non ho missing per ogni variabile

# creo un nuovo file csv con i dati puliti
write.csv(data.frame(data), 
          "C:/Users/ceras/OneDrive/Giogiò/UniMIB/Data Mining/challenge/test_clean.csv",  
          row.names = FALSE)