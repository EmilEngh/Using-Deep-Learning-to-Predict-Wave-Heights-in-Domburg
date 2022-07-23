library(tidyverse)
library(lubridate)
library(keras)
library(tidymodels)
library(leaflet)
library(terra)
options(scipen = 999)
setwd("C:/Users/Bruker/Documents/University stuff/Semester 4/Data Sci/Deep Learning/Project 2")



#_____________________Map_____________________
options(digits = 15)
# WAVE BUOY COORDINATES
wave_coord <- domburg_waves_upload %>% 
  select(MEETPUNT_IDENTIFICATIE, X, Y) %>% 
  rename("buoy_name" = MEETPUNT_IDENTIFICATIE) %>% 
  group_by(X, Y, buoy_name) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(
    buoy_type = "wave buoy"
  )
# extracting coordinates
lonlat <- wave_coord %>% 
  select(X, Y) %>% 
  group_by(X, Y) %>%
  summarise() %>%
  mutate(
    X = as.numeric(gsub(",",".", X)),
    Y = as.numeric(gsub(",",".", Y))
  ) %>% 
  rename(
    "lon" = X,
    "lat" = Y
  )
# transforming UTM to coordinates 
v <- vect(lonlat, crs="+proj=utm +zone=31 +datum=WGS84  +units=m")
y <- project(v, "+proj=longlat +datum=WGS84")
# normal coordinates
lonlat <- geom(y) %>% 
  as_tibble() %>%  
  select(x, y) 
# replacing UTM with coordinates for the wave buoys
wave_coord <- wave_coord %>% 
  bind_cols(lonlat) %>% 
  select(buoy_name, buoy_type, x, y)
# WIND BUOY COORDINATES
wind_coord <- wind_data_upload %>% 
  select(MEETPUNT_IDENTIFICATIE, X, Y) %>% 
  rename("buoy_name" = MEETPUNT_IDENTIFICATIE) %>% 
  group_by(X, Y, buoy_name) %>% 
  summarise() %>% 
  ungroup() %>% 
  mutate(
    buoy_type = "wind buoy"
  )
lonlat <- wind_coord %>% 
  select(X, Y) %>% 
  group_by(X, Y) %>%
  summarise() %>%
  mutate(
    X = as.numeric(gsub(",",".", X)),
    Y = as.numeric(gsub(",",".", Y))
  ) %>% 
  rename(
    "lon" = X,
    "lat" = Y
  )
# transforming UTM to coordinates 
v <- vect(lonlat, crs="+proj=utm +zone=31 +datum=WGS84  +units=m")
y <- project(v, "+proj=longlat +datum=WGS84")
# normal coordinates
lonlat <- geom(y) %>% 
  as_tibble() %>%  
  select(x, y) 
# replacing UTM with coordinates for the wave buoys
wind_coord <- wind_coord %>% 
  bind_cols(lonlat) %>% 
  select(buoy_name, buoy_type, x, y)
# FINAL COORDINATE SET
Final_coord <- wind_coord %>% 
  bind_rows(wave_coord)
# MAPPING
Map <- leaflet() %>%
  addPopups(data = Final_coord, lat = ~y, lng = ~x, popup = ~buoy_type, options = popupOptions(closeButton = FALSE)) %>% 
  addTiles() %>% 
  setView(lng = 3.4, lat = 52.5, zoom = 7.45)
Map


#____________________DOMBURG WAVE DATA__________________
  domburg_waves_upload <- read.csv("waves.csv", sep = ";")
  domburg_waves <- domburg_waves_upload %>%
    select(WAARNEMINGDATUM, REFERENTIE, GROOTHEID_OMSCHRIJVING, NUMERIEKEWAARDE) %>% 
    unite("date_time", WAARNEMINGDATUM, REFERENTIE, sep = " ") %>%
    mutate(
      date_time = parse_date_time(date_time, "%d%m%Y %H%M%S"),
      NUMERIEKEWAARDE = as.numeric(gsub(",",".", NUMERIEKEWAARDE))
    ) %>%
    pivot_wider(names_from = GROOTHEID_OMSCHRIJVING, values_from = NUMERIEKEWAARDE) %>% 
    rename(
      domburg_wave_height = `Significante golfhoogte in het spectrale domein`
    ) %>% 
    filter(
      nchar(domburg_wave_height) <= 3
    )
  # Density plot
  #domburg_waves %>% 
    #ggplot(aes(x = avg_wave_height)) +
   # geom_density()

  
#__________________WIND DATA____________________ 
  wind_data_upload <- read.csv("wind.csv", sep = ";")
  wind_wrangled <- wind_data_upload %>%
    select(MEETPUNT_IDENTIFICATIE, WAARNEMINGDATUM, REFERENTIE,NUMERIEKEWAARDE) %>% 
    unite("date_time", WAARNEMINGDATUM, REFERENTIE, sep = " ") %>%
    mutate(
      date_time = parse_date_time(date_time, "%d%m%Y %H%M%S"),
      measuring_point = MEETPUNT_IDENTIFICATIE,
      wind_strength = as.numeric(gsub(",",".", NUMERIEKEWAARDE))
    ) %>%
    filter(nchar(wind_strength) <= 5) %>%
    select(-c(NUMERIEKEWAARDE, MEETPUNT_IDENTIFICATIE)) %>%
    pivot_wider(names_from = measuring_point, values_from = wind_strength) %>% 
    mutate(
     one_day_past_date = as.Date(date_time) - ddays(1), 
     two_day_past_date = as.Date(date_time) - ddays(2)
    )
  
  # average day wind data
  avg_daily_wind <- wind_wrangled %>%
    mutate(
      date = as.Date(date_time)
    ) %>% 
    group_by(date) %>% 
    summarise(
      avg_Europlatform = mean(Europlatform),
      avg_K13_alpha = mean(`K13 Alpha`),
      avg_Q1_platform = mean(`Q1 platform`)
    )
  # past two days average wind data is added to the main wind dataset
  wind_final <- wind_wrangled %>%
    inner_join(avg_daily_wind, by = c("one_day_past_date" = "date")) %>% 
    rename(
      "1day_past_avg_Europlatform" = avg_Europlatform,
      "1day_past_avg_K13_alpha" = avg_K13_alpha,
      "1day_past_avg_Q1_platform" = avg_Q1_platform
    ) %>%
    inner_join(avg_daily_wind, by = c("two_day_past_date" = "date")) %>% 
    rename(
      "2day_past_avg_Europlatform" = avg_Europlatform,
      "2day_past_avg_K13_alpha" = avg_K13_alpha,
      "2day_past_avg_Q1_platform" = avg_Q1_platform
    ) %>% 
    select(-c(one_day_past_date, two_day_past_date))
  
  
#___________________Final_wind_wave_set_________________
  # ready for prediction
  wind_wave <- domburg_waves %>% 
  inner_join(wind_final, by = "date_time") %>% 
  na.omit() 
  
  
  #______________ create a matrix with the past 3 days observations in each row________
  
  wave_wind_current <- domburg_waves %>% 
    inner_join(wind_wrangled, by = "date_time") %>%
    select(-c(one_day_past_date, two_day_past_date))
  wave_wind_current <- wave_wind_current[rev(order(wave_wind_current$date_time)),]
  wave_wind_current <-  wave_wind_current %>% 
    mutate(index = row_number())
 
  total_rows <- tibble()
  for (x in 1:864) {
    total_rows[x] <- as.numeric("")
    colnames(total_rows)[x] <- paste(x)
  }
  total_rows <- total_rows %>% 
    mutate(index = as.numeric(""))
  
  
  for (i in 1:nrow(wave_wind_current)) {
    # 72 hrs
    cut_min <- wave_wind_current[i+1,]$index
    cut_max <- wave_wind_current %>% 
      filter(date_time == wave_wind_current[i,]$date_time - ddays(2))
    if (nrow(cut_max) == 0) {
      total_rows <- total_rows
    }
    else {
      cut_max <- cut_max$index
      cut_rows <- wave_wind_current %>% 
        slice(cut_min:cut_max) %>%
        select(-c(index, domburg_wave_height))
      if (nrow(cut_rows) < 288) {
        total_rows <- total_rows
      }
      else {
        cut_rows <- cut_rows %>% 
          pivot_wider(names_from = date_time, values_from = c("Europlatform", "K13 Alpha", "Q1 platform")) %>% 
          mutate(index = i)
        #for (y in 1:ncol(cut_rows)) {
         # colnames(cut_rows)[y] <- paste(y)
        #}
        colnames(cut_rows) <- colnames(total_rows)
        total_rows <- total_rows %>%
          bind_rows(cut_rows) 
      }
    }
  }
  # final wave-wind dataset
final_set <- wave_wind_current %>%
  inner_join(total_rows, by = "index") %>% 
  select(-c(date_time, index)) %>%
  na.omit()

#_______________________ANN_Prediction_Simple_Model_Non_Recurring______________________
  data <- initial_split(wind_wave, prop = 3/4)
  # training set
  training <- training(data) 
  x1 <- training %>%
    select(Europlatform, `K13 Alpha`, `Q1 platform`, `1day_past_avg_Europlatform`,
           `1day_past_avg_K13_alpha`, `1day_past_avg_Q1_platform`, `2day_past_avg_Europlatform`,
           `2day_past_avg_K13_alpha`, `2day_past_avg_Q1_platform`) %>%
    as_tibble()
  y1 <- training %>% 
   pull(domburg_wave_height)
  # testing set
  testing <- testing(data)
  x2 <- testing %>% 
    select(Europlatform, `K13 Alpha`, `Q1 platform`, `1day_past_avg_Europlatform`,
           `1day_past_avg_K13_alpha`, `1day_past_avg_Q1_platform`, `2day_past_avg_Europlatform`,
           `2day_past_avg_K13_alpha`, `2day_past_avg_Q1_platform`) %>%
    as.matrix()
  y2 <- testing %>% 
    pull(domburg_wave_height)
  # recipe
  r <- recipe(~., data = x1) %>% 
    prep()
  # cross validation list 
  cross_v <- vfold_cv(training, v = 4)
  # model 
  model <-  keras_model_sequential() %>%
    layer_dense(units = 100, activation = "relu") %>%
    layer_dense(units = 100, activation = "relu") %>% 
    layer_dense(units = 1)
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = "rmsprop",
      metrics = c("mae")
    )  
  # cross validation function 
  fitdm <- function(split) {
    xan <- r %>% bake(new_data = analysis(split)  , composition = "matrix")
    xas <- r %>% bake(new_data = assessment(split), composition = "matrix")
    yan <- analysis(split)   %>% pull(domburg_wave_height)
    yas <- assessment(split) %>% pull(domburg_wave_height)
    # fit model 
    history <- model %>% 
      fit(x = xan, y = yan, validation_data = list(xas, yas), epochs = 200, batch_size = 125)
    history %>%
      as_tibble()
  }
  # cross-validation residuals
  res <- cross_v %>% 
    mutate(mod = map(splits, fitdm)) %>% 
    unnest(mod)
  # cross validation, lowest mae
  highest <- res %>% filter(metric == "mae", data == "validation") %>% 
    group_by(epoch) %>% 
    summarize(mae = mean(value)) 
  # cross validation plot 
  highest %>% 
    ggplot(aes(epoch, mae)) + geom_smooth()  

  
  
  
  
#______________________ANN_Prediction_Advanced_Model_Non_Recurring____________________
  data2 <- initial_split(final_set, prop = 3/4) 
  # Training set 
  training2 <- training(data2)
  x1 <- training2 %>% 
    select(-c(domburg_wave_height)) %>% 
    as.matrix()
  y1 <- training2 %>% 
    pull(domburg_wave_height)
  # test_set 
  testing2 <- testing(data2)
  x2 <- testing2 %>% 
    select(-c(domburg_wave_height)) %>% 
    as.matrix()
  y2 <- testing2 %>% 
    pull(domburg_wave_height)
  # recipe 
  r <- recipe(~., data = x1) %>% 
    prep()
  # cross validation list 
  cross_v <- vfold_cv(training2, v = 4)
  # model 
  model2 <- keras_model_sequential() %>%
    layer_dense(units = 100, activation = "relu") %>% 
    layer_dense(units = 100,activation = "relu") %>%
    layer_dense(units = 1)
  # compile model 
  model2 %>% 
    compile(
      loss = "mse",
      optimizer = "rmsprop",
      metrics = c("mae")
    )
  # cross validation function 
  fitdm <- function(split) {
    xan <- r %>% bake(new_data = analysis(split)  , composition = "matrix")
    xas <- r %>% bake(new_data = assessment(split), composition = "matrix")
    yan <- analysis(split)   %>% pull(domburg_wave_height)
    yas <- assessment(split) %>% pull(domburg_wave_height)
    # fit model 
    history <- model2 %>% 
      fit(x = xan, y = yan, validation_data = list(xas, yas), epochs = 200, batch_size = 125)
    history %>%
      as_tibble()
  }
  
  # cross-validation residuals
  res <- cross_v %>% 
    mutate(mod = map(splits, fitdm)) %>% 
    unnest(mod)
  # cross validation, lowest mae
  highest <- res %>% filter(metric == "mae", data == "validation") %>% 
    group_by(epoch) %>% 
    summarize(mae = mean(value)) 
  # cross validation plot 
  highest %>% 
    ggplot(aes(epoch, mae)) + geom_smooth()  
  
  # Fitting on whole training set 
  train_fit <- model2 %>% 
    fit(x = x1, y = y1, epochs = 200, batch_size = 125)
  # evaluating mae on testing set
  model2 %>% evaluate(x2, y2)
  model2 %>% keras::get_weights()
  # evaluating mae on testing set for prediction over 100cm 
  pred <- model2 %>% 
    predict_on_batch(x2) %>% 
    as_tibble() %>% 
    rename("pred" = V1) %>% 
    mutate(index = row_number())
  higher_wave_pred <- testing2 %>% 
    mutate(index = row_number()) %>% 
    inner_join(pred, by = "index") %>% 
    select(domburg_wave_height, pred) %>% 
    filter(domburg_wave_height >= 100) 
  higher_wave_pred %>% 
    mae(domburg_wave_height, pred)
  

