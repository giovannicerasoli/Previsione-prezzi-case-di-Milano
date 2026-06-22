################################################################################

# Giovanni Giacomo Cerasoli, matricola 932969 ----------------------------------

################################################################################

knitr::opts_chunk$set(collapse = TRUE)

rm(list = ls())

# carico il dataset di train pulito
data = read.csv(
  "C:/Users/ceras/OneDrive/Giogiò/UniMIB/Data Mining/challenge/training_clean.csv", 
  header = TRUE)

library(visdat)
library(tidyverse)

################################################################################

### ESPLORAZIONE VARIABILI #####################################################

################################################################################

vis_dat(data)

# trasformo alcuni tipi delle variabili:
data = data %>%
  mutate(across(c(lift, condominium_fees, optic.fiber, security.door, cellar,
                  video.entryphone, alarm.system, closet, electric.gate, 
                  terrace, hydromassage, fireplace, tavern, attic, pool,
                  tennis.court, concierge.reception, balconies, 
                  bathrooms_number, rooms_number, total_floors_in_building,
                  car_parking, conditions, heating_centralized, 
                  energy_efficiency_class, furnished, exposure, garden, zone),
                as.factor),
         square_meters = as.numeric(square_meters),
         condominium_fees = as.numeric(condominium_fees))

# procedo col valutare variabili: distribuzione, relazione con 
# selling_price, relazioni con altre variabili

################################################################################

# SELLING_PRICE:

par(mfrow = c(1, 2))
hist(data$selling_price, xlab = "selling_price", main = "", probability = T,
     col = "springgreen") 
# potrebbe essere opportuno fare una trasformazione logaritmica

hist(log(data$selling_price), xlab = "log(selling_price)", main = "",
     probability = T, , col = "springgreen") 
curve(dnorm(x, mean = mean(log(data$selling_price), na.rm = TRUE), 
            sd = sd(log(data$selling_price), na.rm = TRUE)), 
      col = "darkred",  lwd = 2, add = TRUE)
# la distribuzione è più simile ad una gaussiana
par(mfrow = c(1, 1))

# boxplot
par(mfrow = c(1, 2))
boxplot(data$selling_price, 
        col = "springgreen", 
        horizontal = TRUE)
title(main = "selling_price", cex.main = 0.8, font.main = 1)  
points(mean(data$selling_price, na.rm = TRUE), 1, col = "darkred", pch = 19)

boxplot(log(data$selling_price), 
        col = "springgreen", 
        horizontal = TRUE)
title(main = "log(selling_price)", cex.main = 0.8, font.main = 1)  
points(mean(log(data$selling_price), na.rm = TRUE), 1, col = "darkred", 
       pch = 19)
par(mfrow = c(1, 1))

################################################################################

# SQUARE_METERS:

par(mfrow = c(1, 2))
hist(data$square_meters, xlab = "square_meters", main = "", probability = T,
     col = "springgreen")

hist(log(data$square_meters), xlab = "log(square_meters)", main = "",
     probability = T, , col = "springgreen")
curve(dnorm(x, mean = mean(log(data$square_meters), na.rm = TRUE), 
            sd = sd(log(data$square_meters), na.rm = TRUE)), 
      col = "darkred",  lwd = 2, add = TRUE)
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
plot(data$square_meters, log(data$selling_price), xlab = "square_meters",
     ylab = "log(selling_price)")
# anche per square_meters sembra meglio fare una trasformazione logaritmica

plot(log(data$square_meters), log(data$selling_price), 
     xlab = "log(square_meters)", ylab = "log(selling_price)")
par(mfrow = c(1, 1))

cor(log(data$square_meters), log(data$selling_price))
# i metri quadri influenzano il prezzo

################################################################################

# BATHROOMS_NUMBER:

table(data$bathrooms_number)

ggplot(data, aes(x = bathrooms_number, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# il numero di bagni sembra influire sul prezzo

# considero bathrooms_number come numerica (se 3+ metto 4)
data$bathrooms_number = as.numeric(data$bathrooms_number)

cor(data$bathrooms_number, log(data$selling_price))

################################################################################

# LIFT:

table(data$lift)

ggplot(data, aes(x = lift, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# la presenza dell'ascensore sembra influire sul prezzo

################################################################################

# ROOMS_NUMBER:

table(data$rooms_number)

ggplot(data, aes(x = rooms_number, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# il numero di stanze sembra influire sul prezzo

# considero rooms_number come numerica (se 5+ metto 6)
data$rooms_number = as.numeric(data$rooms_number)

cor(data$rooms_number, log(data$selling_price))

################################################################################

# TOTAL_FLOORS_IN_BUILDING:

table(data$total_floors_in_building)

ggplot(data, aes(x = total_floors_in_building, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# non pare vi sia grande influenza sul prezzo

# inoltre capita che ci sia discrepanza con floor: per alcune osservazioni si ha
# un piano più alto che i piani totali della casa
table(data$total_floors_in_building, data$floor)

################################################################################

# CAR_PARKING:

table(data$car_parking)

# ordino i fattori
data$car_parking = factor(data$car_parking,
  levels = c("no", "shared parking", "1 garage/box", "1+ garage/box"),
  ordered = TRUE)

ggplot(data, aes(x = car_parking, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra che la presenza di garage/box influenzi il prezzo, diversamente dal
# parcheggio condiviso

################################################################################

# CONDOMINIUM_FEES:

hist(data$condominium_fees)

plot(data$condominium_fees, log(data$selling_price))

cor(data$condominium_fees, log(data$selling_price))
# le spese condominiale sembrano avere influenza sul prezzo

################################################################################

# YEAR_OF_CONSTRUCTION:

table(data$year_of_construction)

plot(data$year_of_construction, log(data$selling_price))

cor(data$year_of_construction, log(data$selling_price))
# sembra esserci poca influenza sul prezzo

# raggruppo le date in categorie ordinate
data = data %>%
  mutate(
    year_of_construction = case_when(
      year_of_construction <= 1900 ~ "pre1900",
      year_of_construction <= 1945 ~ "1901-1945",
      year_of_construction <= 1969 ~ "1946-1969",
      year_of_construction <= 1989 ~ "1970-1989",
      year_of_construction <= 2009 ~ "1990-2009",
      year_of_construction <= 2019 ~ "2010-2019",
      year_of_construction >= 2020 ~ "2020plus",
      TRUE ~ NA_character_
    ),
    year_of_construction = factor(
      year_of_construction,
      levels = c("pre1900", "1901-1945", "1946-1969", "1970-1989",
                 "1990-2009", "2010-2019", "2020plus"),
      ordered = TRUE))

################################################################################

# CONDITIONS:

table(data$conditions)

ggplot(data, aes(x = conditions, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# non c'è grade differenze a differenza di condizione, anche se alcune
# variazioni ci sono

################################################################################

# DISTANCE:

hist(data$distance)

plot(data$distance, log(data$selling_price))

cor(data$distance, log(data$selling_price))
# la distribuzione di distance è circa gaussiana e c'è influenza sul prezzo

################################################################################

# ZONE:

table(data$zone)

# ordino i livelli per media di selling_price
data = data %>%
  mutate(zone = fct_reorder(zone, selling_price, .fun = mean))

ggplot(data, aes(x = zone, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# la zona influisce sul prezzo

################################################################################

# FLOOR:

hist((data$floor))

plot(data$floor, log(data$selling_price))

cor(data$floor, log(data$selling_price))
# sembra esserci influenza sul prezzo

################################################################################

# HEATING_CENTRALIZED:

table(data$heating_centralized)

ggplot(data, aes(x = heating_centralized, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci piccola variazione sul prezzo

################################################################################

# ENERGY_EFFICIENCY_CLASS:

table(data$energy_efficiency_class)

# ordino le classi
data$energy_efficiency_class = factor(data$energy_efficiency_class,
  levels = c("low", "medium-low", "medium-high", "high"),
  ordered = TRUE)

ggplot(data, aes(x = energy_efficiency_class, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci piccola variazione sul prezzo

################################################################################

# OPTIC_FIBER:

table(data$optic.fiber)

ggplot(data, aes(x = optic.fiber, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci piccola variazione sul prezzo

################################################################################

# SECURITY_DOOR:

table(data$security.door)

ggplot(data, aes(x = security.door, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# CELLAR:

table(data$cellar)

ggplot(data, aes(x = cellar, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# VIDEO_ENTRYPHONE:

table(data$video.entryphone)

ggplot(data, aes(x = video.entryphone, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# ALARM_SYSTEM:

table(data$alarm.system)

ggplot(data, aes(x = alarm.system, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# CLOSET:

table(data$closet)

ggplot(data, aes(x = closet, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# ELECTRIC_GATE:

table(data$electric.gate)

ggplot(data, aes(x = electric.gate, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra non esserci grande variazione sul prezzo

################################################################################

# FURNISHED:

table(data$furnished)

ggplot(data, aes(x = furnished, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# TERRACE:

table(data$terrace)

ggplot(data, aes(x = terrace, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# HYDROMASSAGE:

table(data$hydromassage)

ggplot(data, aes(x = hydromassage, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# FIREPLACE:

table(data$fireplace)

ggplot(data, aes(x = fireplace, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# TAVERN:

table(data$tavern)

ggplot(data, aes(x = tavern, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# ATTIC:

table(data$attic)

ggplot(data, aes(x = attic, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci piccola variazione sul prezzo

################################################################################

# POOL:

table(data$pool)

ggplot(data, aes(x = pool, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# TENNIS_COURT:

table(data$tennis.court)

ggplot(data, aes(x = tennis.court, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# CONCIERGE/RECEPTION:

table(data$concierge.reception)

ggplot(data, aes(x = concierge.reception, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# EXPOSURE:

table(data$exposure)

ggplot(data, aes(x = exposure, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# BALCONIES:

table(data$balconies)

ggplot(data, aes(x = balconies, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci variazione sul prezzo

################################################################################

# GARDEN:

table(data$garden)

# ordino le classi
data$garden = factor(data$garden,
                     levels = c("no garden", "shared", "private"),
                     ordered = TRUE)

ggplot(data, aes(x = garden, y = log(selling_price))) +
  geom_boxplot(fill = "darkred", color = "darkblue")
# sembra esserci piccola variazione sul prezzo

################################################################################

### MODELLI ####################################################################

################################################################################

# divido il dataset in training e validation
set.seed(123)

sample = sort(sample(1:nrow(data), round(nrow(data)*0.75), replace = F))

train = data[sample,]
validation = data[-sample,]

# scrivo una funzione per MAE ed una per MSE
MAE = function(y, y_fit){
  mean(abs(y - y_fit))
}

################################################################################

# MEDIANA
y_hat_median = rep(median(train$selling_price), nrow(validation)) 

round(MAE(train$selling_price, y_hat_median), 4)

################################################################################

# REGRESSIONE LINEARE

# modello con tutte le variabili non trasformate
lm_mod = lm(selling_price ~ ., data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 71

yhat_lm = (predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 107188.3

# ---------------------------------------------------------------------------- #

# d'ora in poi considero sempre come variabile risposta log(selling_price) 

# modello con tutte le variabili non trasformate
lm_mod = lm(log(selling_price) ~ ., data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 71

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 98605.67

# ---------------------------------------------------------------------------- #

# modello con log(square_meters)
lm_mod = lm(log(selling_price) ~ log(square_meters) + . - square_meters, 
            data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 71

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 77400.74

# ---------------------------------------------------------------------------- #

# funzione di backward selection basata su MAE
backward_selection_mae = function(train, validation, response = "selling_price")
{
  # variabili iniziali
  all_vars = setdiff(names(train), response)
  all_vars = setdiff(all_vars, "square_meters") # rimuovo square_meters
  # ne considero il log
  formula_base = paste0("log(", response, ") ~ log(square_meters)") 
  current_vars = all_vars
  
  # inizializzo valori
  best_mae = Inf
  improved = TRUE
  best_model = NULL
  best_formula = NULL
  
  while (improved && length(current_vars) > 0) {
    improved = FALSE
    maes <- c()
    formulas <- list()
    models <- list()
    
    for (var in current_vars) {
      trial_vars = setdiff(current_vars, var)
      rhs = if (length(trial_vars) > 0) {
        paste(c("log(square_meters)", trial_vars), collapse = " + ")
      } else {
        "log(square_meters)"
      }
      form = as.formula(paste0("log(", response, ") ~ ", rhs))
      model = lm(form, data = train)
      yhat = exp(predict(model, newdata = validation))
      mae = MAE(validation[[response]], yhat)
      
      maes = c(maes, mae)
      formulas[[length(maes)]] = form
      models[[length(maes)]] = model
    }
    
    # trovo il modello con MAE minimo
    min_mae = min(maes)
    min_index = which.min(maes)
    
    if (min_mae < best_mae) {
      best_mae = min_mae
      best_model = models[[min_index]]
      best_formula = formulas[[min_index]]
      current_vars = setdiff(current_vars, 
                     all_vars[which(all_vars %in% current_vars)][min_index])
      improved = TRUE
    }
  }
  
  cat("modello:\n")
  print(best_formula)
  cat("MAE:", round(best_mae, 4), "\n")
  
  return(best_model)
}

best_model = backward_selection_mae(train, validation)

# modello ottenuto
lm_mod = lm(log(selling_price) ~ log(square_meters) + bathrooms_number + 
  rooms_number + total_floors_in_building + condominium_fees + 
  year_of_construction + conditions + zone + floor + heating_centralized + 
  energy_efficiency_class + optic.fiber + cellar + video.entryphone + 
  furnished + terrace + hydromassage + attic + pool + tennis.court + 
  concierge.reception + exposure + balconies + garden, 
  data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 60

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 76641.14


# ---------------------------------------------------------------------------- #

# step backward selection
lm_mod = step(lm(log(selling_price) ~ log(square_meters) + . - square_meters, 
            data = train))

lm_mod = lm(log(selling_price) ~ log(square_meters) + bathrooms_number + 
              lift + rooms_number + total_floors_in_building + car_parking + 
              condominium_fees + year_of_construction + conditions + zone + 
              floor + energy_efficiency_class + optic.fiber + security.door + 
              cellar + alarm.system + closet + electric.gate + furnished + 
              terrace + tavern + pool + tennis.court + concierge.reception + 
              balconies + garden + distance, data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 65

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 77401.88

# ---------------------------------------------------------------------------- #

# modello combinando la backward selection con AIC e con MAE
lm_mod = lm(log(selling_price) ~ log(square_meters) + bathrooms_number + lift
                  + rooms_number + total_floors_in_building + condominium_fees + 
                  year_of_construction + conditions + zone + floor + 
                  energy_efficiency_class + optic.fiber + cellar + furnished + 
                  terrace + pool + concierge.reception + 
                  balconies + garden + distance + security.door + alarm.system 
                  + closet + electric.gate
                  , data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 60

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 77248.17

# ---------------------------------------------------------------------------- #

# unisco alcune variabili che originariamente erano in other_features che 
# si riferiscono a stessi campi (come sicurezza o lusso) o perché
# hanno poche osservazioni con quella caratteristica, al fine di diminuire il 
# numero di covariate, senza considerare le variabili che già erano state 
# eliminate
 
table(data$security.door)
table(data$alarm.system)
table(data$electric.gate)

# creo la variabili security_level 
data = data %>%
  mutate(across(c(security.door, alarm.system, electric.gate), 
                ~ as.numeric(as.character(.)))) %>%
  mutate(security_level = rowSums(across(c(security.door, alarm.system, 
                                           electric.gate)))) %>%
  mutate(security_level = factor(security_level, levels = 0:3, ordered = TRUE))

table(data$security_level)

ggplot(data, aes(x = security_level, y = log(selling_price))) +
  geom_boxplot(fill = "springgreen", color = "darkblue")
# sembra esserci influenza sul prezzo se ho alto livello di sicurezza

table(data$closet)
table(data$pool)

# creo la variabili luxury_level 
data = data %>%
  mutate(across(c(closet, pool), 
                ~ as.numeric(as.character(.)))) %>%
  mutate(luxury_level = factor(
    rowSums(across(c(closet, pool))), levels = 0:2, ordered = TRUE))

table(data$luxury_level)

ggplot(data, aes(x = luxury_level, y = log((selling_price)))) +
  geom_boxplot(fill = "springgreen", color = "darkblue")
# c'è influenza sul prezzo per livello di lusso

# metto luxury_level, security_level come numeriche
data$luxury_level = as.numeric(data$luxury_level)
data$security_level = as.numeric(data$security_level)

# ricreo il train ed il validation con le nuove variabili
set.seed(123)

sample = sort(sample(1:nrow(data), round(nrow(data)*0.75), replace = F))

train = data[sample,]
validation = data[-sample,]

# modello con le nuove variabili
lm_mod = lm(log(selling_price) ~ log(square_meters) + bathrooms_number + lift
            + rooms_number + total_floors_in_building + condominium_fees + 
              year_of_construction + conditions + zone + floor + 
              energy_efficiency_class + optic.fiber + cellar + furnished + 
              terrace + concierge.reception + 
              balconies + garden + distance + luxury_level + security_level
            , data = train)

length(coef(lm_mod)) - 1 # numero di covariate = 57

yhat_lm = exp(predict(lm_mod, validation))

round(MAE(validation$selling_price, yhat_lm), 4) # 77141.58

################################################################################

### SUBMISSION #################################################################

################################################################################

test = read.csv(
  "C:/Users/ceras/OneDrive/Giogiò/UniMIB/Data Mining/challenge/test_clean.csv", 
  header = TRUE)

# faccio le modifiche alle variabili eseguite per ottimizzare il modello
test = test %>%
  mutate(across(c(lift, condominium_fees, optic.fiber, security.door, cellar,
                  video.entryphone, alarm.system, closet, electric.gate, 
                  terrace, hydromassage, fireplace, tavern, attic, pool,
                  tennis.court, concierge.reception, balconies, 
                  bathrooms_number, rooms_number, total_floors_in_building,
                  car_parking, conditions, heating_centralized, 
                  energy_efficiency_class, furnished, exposure, garden, zone,
                  year_of_construction),
                as.factor),
         square_meters = as.numeric(square_meters),
         condominium_fees = as.numeric(condominium_fees),
         rooms_number = as.numeric(rooms_number),
         bathrooms_number = as.numeric(bathrooms_number))

# modifico year_of_construction
test$year_of_construction = as.numeric(test$year_of_construction)

test = test %>%
  mutate(
    year_of_construction = case_when(
      year_of_construction <= 1900 ~ "pre1900",
      year_of_construction <= 1945 ~ "1901-1945",
      year_of_construction <= 1969 ~ "1946-1969",
      year_of_construction <= 1989 ~ "1970-1989",
      year_of_construction <= 2009 ~ "1990-2009",
      year_of_construction <= 2019 ~ "2010-2019",
      year_of_construction >= 2020 ~ "2020plus",
      TRUE ~ NA_character_
    ),
    year_of_construction = factor(
      year_of_construction,
      levels = c("pre1900", "1901-1945", "1946-1969", "1970-1989",
                 "1990-2009", "2010-2019", "2020plus"),
      ordered = TRUE))

# aggiungo a test le variabili luxury_level e security_level
test = test %>%
  mutate(across(c(security.door, alarm.system, electric.gate), 
                ~ as.numeric(as.character(.)))) %>%
  mutate(security_level = rowSums(across(c(security.door, alarm.system, 
                                           electric.gate)))) %>%
  mutate(security_level = factor(security_level, levels = 0:3, ordered = TRUE))

test = test %>%
  mutate(across(c(closet, pool), 
                ~ as.numeric(as.character(.)))) %>%
  mutate(luxury_level = factor(
    rowSums(across(c(closet, pool))), levels = 0:3, ordered = TRUE))

test$luxury_level = as.numeric(test$luxury_level)
test$security_level = as.numeric(test$security_level)

# modello
lm_mod = lm(log(selling_price) ~ log(square_meters) + bathrooms_number + lift 
            + rooms_number + total_floors_in_building + condominium_fees 
            + year_of_construction + conditions + zone + floor 
            + energy_efficiency_class + optic.fiber + cellar + furnished 
            + terrace + concierge.reception + balconies + garden 
            + distance + luxury_level + security_level
            , data = data)

yhat_lm = exp(predict(lm_mod, test))

summary(lm_mod)

plot(fitted(lm_mod), resid(lm_mod))
abline(h = 0, col = "red")

sub = cbind((1:length(yhat_lm)), round(yhat_lm, 2))
colnames(sub) = c('ID', 'prediction')
write.csv(data.frame(sub), "submission_cerasoli.csv", row.names = F)

