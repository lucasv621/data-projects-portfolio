rm(list=ls())
library(dplyr)
library(data.table)
library(caret)
library(pROC)
library(ggplot2)
library(tidyr)

setwd('C:/Users/lucas.veteikis/Desktop/Lucas/Facu')
source('functions.R') #Importo funciones de archivo

################################################
################ Carga de datos ################
################################################

files <- list.files('ads_data')
setwd('C:/Users/lucas.veteikis/Desktop/Lucas/Facu/ads_data')

dt <- rbindlist(lapply(files,function(i){
  load_data(i,sample_ratio = 1)
}))



#############################################
########## Mergeo con contacts ##############
#############################################
setwd('../')
dt$ad_id <- as.numeric(as.character(dt$ad_id)) #Cambio el tipo de variable ad_id

contacts <- fread("train_contacts.csv", sep=",")
contacts$ad_id <- as.numeric(as.character(contacts$ad_id))
contacts$contacts <- ifelse(contacts$contacts >= 3, 1, 0)
dt <- merge(dt, contacts, by="ad_id", all.x=TRUE)
dt$contacts <- ifelse(!is.na(dt$contacts), dt$contacts, 0)
dt$contacts <- ifelse(dt$created_on < strptime("2022-06-16", format = "%Y-%m-%d", tz = "UTC"), dt$contacts, NA)

# Dejo fuera observaciones que no deberían estar en training
dt$train_sample <- dt$created_on < strptime("2022-07-01", format = "%Y-%m-%d", tz = "UTC")
dt <- dt[!(dt$train_sample == TRUE & is.na(dt$contacts)),]

##################################################################
############################### EDA ##############################
##################################################################


#Resumen del dataframe
summary(dt)

#~~~~~~~~~~~~~~~~~~ Gráfico de NAs ~~~~~~~~~~~~~~~~~~~~~~#

na_sum <- sapply(dt,function(x) sum(is.na(x))) #suma de NAs por columna
df_na <- data.frame(variable=names(dt),na_count=na_sum) #armo un df para graficar
ggplot(df_na,aes(x=variable,y=na_count)) +
  geom_bar(stat='identity',fill='black') +
  ggtitle('Cantidad de NA por columna') +
  xlab('') +
  ylab('Cantidad de NA') +
  theme(axis.text.x = element_text(angle=90,hjust=1))

#~~~~~~~~~~~~~~~~~~ Gráfico de contacts a lo largo del tiempo ~~~~~~~~~~~~~~~~~~~~~~#

dt$month <- as.integer(strftime(dt$created_on, format = "%m", tz = "UTC")) 
dt$year <- as.integer(strftime(dt$created_on, format = "%Y", tz = "UTC"))

data_for_plot <- dt %>% 
  filter(train_sample == T) %>% 
  select(year,month,contacts) %>% 
  group_by(year,month) %>% 
  summarize(contacts_prop = sum(mean(contacts), na.rm = TRUE))
data_for_plot$contacts_prop <- data_for_plot$contacts_prop * 100


ggplot(data=data_for_plot, aes(x=month, y=contacts_prop)) +
  geom_line(aes(group = year), lwd=1, color='red') +
  labs(x = "Mes", y = "Proporción de Contactos", title = "Proporción de publicacionesContactos por Mes y Año") +
  theme(axis.text.x = element_text(angle=90,hjust=1)) +
  facet_wrap(~ year) +
  scale_x_continuous(breaks = seq(min(data_for_plot$month), max(data_for_plot$month), by = 1))

#~~~~~~~~~~~~~~~~~~ Pivot de contacts a lo largo del tiempo ~~~~~~~~~~~~~~~~~~~~~~#

data_for_plot <- data_for_plot %>% 
  filter(month== c(1,2,3,4,5,6))

data_for_plot <- data_for_plot %>%
  group_by(year) %>%
  mutate(var = (contacts_prop - lag(contacts_prop, default = first(contacts_prop)))) %>%
  ungroup() %>%
  pivot_longer(-c(year, month), names_to = "variable", values_to = "contacts_prop") %>%
  mutate(variable = paste0("var_", variable)) %>%
  pivot_wider(names_from = "year", values_from = "contacts_prop")

## Tabla pivote para ver la diferencia 
## respecto al mes anterior en los primeros 6 meses
data_for_plot %>% 
  filter(variable == 'var_var') %>% 
  select('month','2020','2021','2022')
# Es diferente año a año la evolución

#~~~~~~~~~~~~~~~~~~ Gráfico de precio y contacts ~~~~~~~~~~~~~~~~~~~~~~#

# Voy a dividir el precio en 5 cuantiles y calcular la proporcion de >=3 contacts
data_for_plot <- dt %>%
  filter(!is.na(price_usd), is.finite(price_usd)) %>%
  select(price_usd, contacts, operation) %>%
  group_by(operation) %>% 
  mutate(price_quantile = ntile(price_usd, 5)) %>%
  group_by(operation, price_quantile) %>%
  summarise(mean_contacts = mean(contacts, na.rm = TRUE), .groups = 'drop')

ggplot(data_for_plot, aes(x = factor(price_quantile), y = mean_contacts, fill = operation)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cuantil de precio", y = "Contactos promedio", title = "Contactos promedio por cuantil de precio y tipo de operación") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

#~~~~~~~~~~~~~~~~~~ Gráfico relacion room, bedrooms, bathrooms con contacts ~~~~~~~~~~~~~~~~~~~~~~#

data_for_plot <- dt %>% 
  select(bathrooms, contacts) %>% 
  group_by(bathrooms) %>% 
  filter(bathrooms < 8) %>% 
  summarise(mean_contacts = mean(contacts, na.rm = T)) %>%
  rename(cantidad = bathrooms) %>%
  mutate(category = "bathrooms")

data_for_plot_bedrooms <- dt %>% 
  select(bedrooms, contacts) %>% 
  group_by(bedrooms) %>% 
  filter(bedrooms < 8) %>% 
  filter(bedrooms > 0) %>% # 
  summarise(mean_contacts = mean(contacts, na.rm = T)) %>%
  rename(cantidad = bedrooms) %>%
  mutate(category = "bedrooms")

data_for_plot_rooms <- dt %>% 
  select(rooms, contacts) %>% 
  group_by(rooms) %>% 
  filter(rooms < 8) %>% 
  filter(rooms >= 0) %>% 
  summarise(mean_contacts = mean(contacts, na.rm = T)) %>%
  rename(cantidad = rooms) %>%
  mutate(category = "rooms")

combined_data <- bind_rows(data_for_plot, data_for_plot_bedrooms, data_for_plot_rooms)
combined_data$mean_contacts <- combined_data$mean_contacts * 100

ggplot(combined_data, aes(x = factor(cantidad), y = mean_contacts, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ category, scales = "free_x", nrow = 1) +
  labs(x = "Cantidad", y = "(%) de publicaciones con más de 3 contactos", title = "% con más de 3 contactos según cantidad de habitaciones") +
  theme(legend.position = "None", legend.title = element_blank())

#~~~~~~~~~~~~~~~~~~ Filtro las fechas que me voy a quedar ~~~~~~~~~~~~~~~~~~~~~~#
dt <- dt %>% filter(created_on >= '2022-01-01') # --> Hago el filtrado de fecha decidido por EDA

#~~~~~~~~~~~~~~~~~~ Borro variables que ya no van a servir ~~~~~~~~~~~~~~~~~~~~~~#
rm(combined_data)
rm(contacts)
rm(data_for_plot)
rm(data_for_plot_bedrooms)
rm(data_for_plot_rooms)

#~~~~~~~~~~~~~~~~~~ Revisión de división train set y test set ~~~~~~~~~~~~~~~~~~~~~~#
# Tomamos los datos de 'contacts' de tus datos originales
train_contacts <- dt %>% filter(month %in% c(1,2,3,4)) %>% select(contacts) %>% mutate(label='train')
valid_contacts <- dt %>% filter(month %in% c(5,6)) %>% select(contacts) %>% mutate(label='valid')

# Unir los dos data frames en uno solo
all_contacts_df <- rbind(train_contacts, valid_contacts)

# Calculamos el recuento de 'contacts' para cada conjunto
all_contacts_df <- all_contacts_df %>%
  count(contacts, label)
all_contacts_df
# Calculamos el porcentaje de cada clase dentro de cada 'label'
all_contacts_df <- all_contacts_df %>%
  group_by(label) %>% 
  mutate(percentage = n / sum(n) * 100)

ggplot(all_contacts_df, aes(x = contacts, y = percentage, fill = label)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5, size = 3.5) +
  facet_wrap(~ label, scales = "free_x") +
  labs(x = "Contacts", y = "Porcentaje(%)") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = c(0, 1), labels = c("0", "1"))


#################################################################################
####################### Feature engineering #####################################
#################################################################################

#~~~~~~~~~~~~~~~~~~ Transformación precio, room, superficie ~~~~~~~~~~~~~~~~~~~~~~#

dt <- dt %>% mutate(price_per_room = price_usd/rooms,
                    price_per_bathroom = price_usd/bathrooms,
                    price_per_bedroom = price_usd/bedrooms,
                    surface_covered = surface_covered/surface_total,
                    surface_uncovered = surface_total - surface_covered,
                    price_per_surf_cov = price_usd/surface_covered,
                    price_per_surf_total = price_usd/surface_total,
                    bedroom_proportion = bedrooms/rooms,
                    bathroom_proportion = bathrooms/rooms)

ratios = c('price_per_room','price_per_bathroom','price_per_bedroom',
           'price_per_surf_cov','price_per_surf_total','bedroom_proportion','bathroom_proportion')

# Reemplazo infinitos con NA
dt[, ratios] <- lapply(dt[, ..ratios], function(x) ifelse(is.infinite(x), NA, x))

#~~~~~~~~~~~~~~~~~~ Transformación logaritmica de precio ~~~~~~~~~~~~~~~~~~~~~~# 
dt$log_price_usd <- log1p(dt$price_usd) #reducir el efecto de los valores extremos 
#y mejorar la linealidad de la relación entre el precio y el objetivo.

#~~~~~~~~~~~~~~~~~~ Transformación place_l5 y l6 ~~~~~~~~~~~~~~~~~~~~~~#
dt$has_l5 <- ifelse(is.na(as.character(dt$place_l5)),0,1)
place_l5 <- NULL
dt$has_l6 <- ifelse(is.na(as.character(dt$place_l6)),0,1)
place_l6 <- NULL

#~~~~~~~~~~~~~~~~~~ Más transformaciones de precio ~~~~~~~~~~~~~~~~~~~~~~#

## Métrica que mide el ratio entre el promedio de un tipo de operacion en un país
## y el precio de la publicacion
avg_price_l1 <- aggregate(price_usd ~ place_l1 + operation, data=dt,FUN= mean)
colnames(avg_price_l1)[3] <- 'avg_price_l1'
dt <- left_join(dt, avg_price_l1, by = c('place_l1','operation'))
dt$precio_comparado_con_pais <- dt$price_usd / dt$avg_price_l1 
dt %>% select(price_usd,place_l1,operation,avg_price_l1,precio_comparado_con_pais)

# Lo mismo pero con ciudades
avg_price_l2 <- aggregate(price_usd ~ place_l2 + operation, data=dt,FUN= mean)
colnames(avg_price_l2)[3] <- 'avg_price_l2'
dt <- left_join(dt, avg_price_l2, by = c('place_l2','operation'))
dt$precio_comparado_con_ciudad <- dt$price_usd / dt$avg_price_l2 
dt %>% select(price_usd,place_l2,operation,avg_price_l2,precio_comparado_con_ciudad)

# Lo mismo pero con l3
avg_price_l3 <- aggregate(price_usd ~ place_l3 + operation, data=dt,FUN= mean)
colnames(avg_price_l3)[3] <- 'avg_price_l3'
dt <- left_join(dt, avg_price_l3, by = c('place_l3','operation'))
dt$precio_comparado_con_l3 <- dt$price_usd / dt$avg_price_l3 

# Lo mismo pero con l4
avg_price_l4 <- aggregate(price_usd ~ place_l4 + operation, data=dt,FUN= mean)
colnames(avg_price_l4)[3] <- 'avg_price_l4'
dt <- left_join(dt, avg_price_l4, by = c('place_l4','operation'))
dt$precio_comparado_con_l4 <- dt$price_usd / dt$avg_price_l4 


## Lo mismo pero con log para l1 y l2

avg_price_l1_log <- aggregate(price_usd ~ place_l1 + operation, data=dt,FUN= mean)
colnames(avg_price_l1_log)[3] <- 'avg_price_l1_log'
dt <- left_join(dt, avg_price_l1_log, by = c('place_l1','operation'))
dt$precio_comparado_con_pais_log <- log1p(dt$price_usd) / log1p(dt$avg_price_l1) 

avg_price_l2_log <- aggregate(price_usd ~ place_l2 + operation, data=dt,FUN= mean)
colnames(avg_price_l2_log)[3] <- 'avg_price_l2_log'
dt <- left_join(dt, avg_price_l2_log, by = c('place_l2','operation'))
dt$precio_comparado_con_ciudad_log <- log(dt$price_usd) / log(dt$avg_price_l2) 
dt %>% select(price_usd,place_l2,operation,avg_price_l2,precio_comparado_con_ciudad)

agrupacion = c('precio_comparado_con_pais','precio_comparado_con_ciudad','precio_comparado_con_l3',
               'precio_comparado_con_l4',
               'precio_comparado_con_pais_log','precio_comparado_con_ciudad_log')

# Lo corrijo por ser ratio, saco infinitos
dt[, agrupacion] <- lapply(dt[, ..agrupacion], function(x) ifelse(is.infinite(x), NA, x))

# Diferencia de precio en usd vs precio promedio
dt$precio_comparado_con_pais_abs <- dt$price_usd - dt$avg_price_l1
dt$precio_comparado_con_pais_abs_cuadr <- (dt$price_usd - dt$avg_price_l1)**2
dt$precio_comparado_con_ciudad_abs <- dt$price_usd - dt$avg_price_l2
dt$precio_comparado_con_pais_ciudad_cuadr <- (dt$price_usd - dt$avg_price_l2)**2

#~~~~~~~~~~~~~~~~~~ Transformaciones temporales ~~~~~~~~~~~~~~~~~~~~~~#
dt$day <- as.integer(strftime(dt$created_on, format = "%d", tz = "UTC"))
dt$week_day <- as.integer(strftime(dt$created_on, format = "%w", tz = "UTC")) 
dt$hour <- as.integer(strftime(dt$created_on, format = "%H",tz='UTC'))
dt$created_on <- as.numeric(dt$created_on) #la transfomrmo a numeric para meterla en el modelo

#dt&month y dt$year fueron creados en el EDA.

#~~~~~~~~~~~~~~~~~~ Transformaciones de texto a dummies ~~~~~~~~~~~~~~~~~~~~~~#

dt$luminoso <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("luz|luminoso|mucha luz", x, ignore.case = TRUE)))
dt$costera <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("costa|costera|playa|mar", x, ignore.case = TRUE)))
dt$montañoso <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("montaña|trekking|montañoso", x, ignore.case = TRUE)))
dt$garage <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("cochera|garaje|aparcamiento|parqueadero|parking|estacionamiento", x, ignore.case = TRUE)))
dt$espacios_verdes <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("plaza|parque|pastizal|arboleda|plazita", x, ignore.case = TRUE)))
dt$transporte <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("autobus|metro|subte|tren|parada|colectivo|estacion|bondi", x, ignore.case = TRUE)))
dt$autopista <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("autopista|ruta", x, ignore.case = TRUE)))
dt$centro <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("centro|centrica", x, ignore.case = TRUE)))
dt$shopping <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("shopping|mall|comercial|tiendas", x, ignore.case = TRUE)))
dt$ventanas <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("ventanas|ventanales|luminoso", x, ignore.case = TRUE)))
dt$seguridad <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("seguro|seguridad", x, ignore.case = TRUE)))
dt$mascotas <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("admito mascotas|mascotas permitidas|pet friendly|pet-friendly", x, ignore.case = TRUE)))
dt$moderno <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("moderno|nuevo|reformado|lujoso", x, ignore.case = TRUE)))
dt$vista <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("vista a las montañas|vista al mar", x, ignore.case = TRUE)))
dt$parrilla <-  apply(dt[, c("description", "title")], 1, function(x) any(grepl("parrilla", x, ignore.case = TRUE)))
dt$pileta <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("pileta|piscina", x, ignore.case = TRUE)))
dt$ventilacion_aire <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("aire acondicionado|ventilador", x, ignore.case = TRUE)))
dt$calefaccion <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("estufa|calefaccion", x, ignore.case = TRUE)))
dt$amenities <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("balcon|terraza|jardin|quincho", x, ignore.case = TRUE)))
dt$ascensor <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("ascensor|elevador", x, ignore.case = TRUE)))
dt$encargado <- apply(dt[, c("description", "title")], 1, function(x) any(grepl("encargado|conserje", x, ignore.case = TRUE)))




#saveRDS(dt,'base.RDS')
dt <- readRDS('base.RDS')
##~~~~~~~~~~~~~~~~~~ Borro columnas que ya no son de utilidad ~~~~~~~~~~~~~~~~~~~~~~#

dt$description <- NULL #No la voy a pasar a OHE
dt$current_state <- NULL #llena de NAs
dt$development_name <- NULL #llena de NAs

dt$short_description <- NULL #No la voy a pasar a OHE
dt$title <- NULL #No la voy a pasar a OHE
# Saco promedios que me sirvieron para calcular otras variables
dt$avg_price_l1 <- NULL
dt$avg_price_l1_log <- NULL
dt$avg_price_l2 <- NULL
dt$avg_price_l2_log <- NULL
dt$avg_price_l3 <- NULL
dt$avg_price_l4 <- NULL


#~~~~~~~~~~~~~~~~~~ División train_set y eval_set ~~~~~~~~~~~~~~~~~~~~~~#

train_set <- subset(dt, as.logical(train_sample))
train_set$train_sample <- NULL
eval_set <- subset(dt, !as.logical(train_sample))
eval_set$train_sample <- NULL


#~~~~~~~~~~~~~~~~~~ One Hot Encoding ~~~~~~~~~~~~~~~~~~~~~~#

train_set <- one_hot_sparse(train_set)
eval_set <- one_hot_sparse(eval_set)


#~~~~~~~~~~~~~~~~~~ Borro variables que no voy a usar más ~~~~~~~~~~~~~~~~~~~~~~#
rm(dt)
gc()

#################################################################################
############################ Modelado ###########################################
#################################################################################

library(xgboost)

# Si hacemos kmeans, esta división hay que hacerla más arriba

#~~~~~~~~~~~~~~~~~~ División de train set en train y val ~~~~~~~~~~~~~~~~~~~~~~#

month_column <- as.vector(train_set[, "month"])
val_index <- which(month_column %in% c(5, 6)) #conjunto de val van a ser los últimos meses
train_index <- setdiff(c(1:nrow(train_set)), val_index)

#~~~~~~~~~~~~~~~~~~ Creación de matrices para xgboost ~~~~~~~~~~~~~~~~~~~~~~#
dtrain <- xgb.DMatrix(data = train_set[train_index, colnames(train_set) != "contacts"], #X=predictoras
                      label = train_set[train_index, colnames(train_set) == "contacts"]) #y= valor a predecir

dvalid <- xgb.DMatrix(data = train_set[val_index, colnames(train_set) != "contacts"],
                      label = train_set[val_index, colnames(train_set) == "contacts"])

#~~~~~~~~~~~~~~~~~~ Random Hyperparamether search ~~~~~~~~~~~~~~~~~~~~~~#

rgrid <- random_grid(size = 100,
                     min_nrounds = 60, max_nrounds = 200, 
                     min_max_depth = 6, max_max_depth = 20, 
                     min_eta = 0.01, max_eta = 0.5, 
                     min_gamma = 0, max_gamma = 5, 
                     min_min_child_weight = 3, max_min_child_weight = 15, #numero minimo de obs en una hoja para crear hijo (0,Inf)
                     min_colsample_bytree = 0.3, max_colsample_bytree = 1, #columnas sampleadas por arbol (0,1]
                     min_subsample = 0.7, max_subsample = 0.9)

 
predicted_models <- train_xgboost(dtrain, dvalid, rgrid) #entrenamiento, validacion y grilla
gc()

#~~~~~~~~~~~~~~~~~~ Resultados ~~~~~~~~~~~~~~~~~~~~~~#

res_table <- result_table(predicted_models)
print(res_table)
var_imp <- as.data.frame(xgb.importance(model = predicted_models[[res_table[1, "i"]]]$model))
var_imp
# res_table[1,]
#res_table <- readRDS('res_table_11_5_100_modelos.RDS') 

#Vuelvo a leer esta tabla y reentreno una vez más
res_table
optimized_hyperparamethers <- get_new_min_max(res_table,min_perf_vd = 96,max_perf_tr = 99)
list2env(optimized_hyperparamethers, envir = .GlobalEnv)

#Entreno otros 20 modelos con los hiperparametros más limitados

rgrid <- random_grid(size = 20, 
                     min_nrounds = min_nrounds, max_nrounds = max_nrounds,
                     min_max_depth = min_max_depth, max_max_depth = max_max_depth, 
                     min_eta = min_eta, max_eta = max_eta, 
                     min_gamma = min_gamma, max_gamma = max_gamma, 
                     min_colsample_bytree = min_colsample_bytree, max_colsample_bytree = max_colsample_bytree, 
                     min_min_child_weight = min_min_child_weight, max_min_child_weight = max_min_child_weight,
                     min_subsample = min_subsample, max_subsample = max_subsample)
predicted_models <- train_xgboost(dtrain, dvalid, rgrid)
gc()
res_table_reintento <- result_table(predicted_models)
print(res_table_reintento)
var_imp_reintento <- as.data.frame(xgb.importance(model = predicted_models[[res_table[1, "i"]]]$model))
var_imp_reintento
res_table[1,]
#saveRDS(res_table,'res_table_reintento_2_12_5.RDS')
#saveRDS(var_imp,'var_imp_reintento__2_12_5.RDS')
res_table_reintento <- readRDS('res_table_reintento_2_12_5.RDS')
res_table_reintento[1,]
#~~~~~~~~~~~~~~~~~~ Re-entreno modelo con todos los datos ~~~~~~~~~~~~~~~~~~~~~~#

dall <- xgb.DMatrix(data = train_set[, colnames(train_set) != "contacts"],
                    label = train_set[, colnames(train_set) == "contacts"])

best_model_index <- which.max(sapply(predicted_models, function(x) x$results$perf_vd))

best_hyperparameters <- predicted_models[[best_model_index]]$results
best_hyperparameters <- best_hyperparameters %>% 
  select(-perf_tr,-perf_vd)
best_hyperparameters_list <- as.list(unlist(best_hyperparameters))

final_model <- xgb.train(data = dall,
                         params = best_hyperparameters_list,
                         nrounds = best_hyperparameters_list$nrounds,
                         watchlist = list(train = dall),
                         objective = "binary:logistic",
                         eval.metric = "auc",
                         print_every_n = 10)


#~~~~~~~~~~~~~~~~~~ Pruebo modelo sobre datos de evaluación ~~~~~~~~~~~~~~~~~~~~~~#

eval_preds <- data.frame(ad_id = eval_set[, "ad_id"],
                         contacts = predict(final_model,
                                            newdata = eval_set[,setdiff(colnames(eval_set), "contacts")]))

setwd("C:/Users/lucas.veteikis/Desktop/Lucas/Facu")

options(scipen=10)
write.table(eval_preds, "xgboost_intento_12_5_100_mod_reintento.csv", sep=",", row.names=FALSE, quote=FALSE)
options(scipen=0)


