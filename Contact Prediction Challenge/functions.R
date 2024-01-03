###########
#Definicion de funciones
###########


### Función para cargar archivos

load_data <- function(ads_file,sample_ratio=1,
                      drop_cols=NULL,sel_cols=NULL){
  print(paste0("Loading: ", ads_file, " - Sample ratio: ", sample_ratio))
  dt <- fread(ads_file, header = TRUE, quote = "\"",
              stringsAsFactors = TRUE, na.strings = "",
              drop = drop_cols, select = sel_cols,
              showProgress = TRUE, encoding = "UTF-8")
  if (sample_ratio < 1) {
    sample_size <- as.integer(sample_ratio * nrow(dt))
    dt <- dt[sample(.N, sample_size)] ## corregir esto para que no samplee mas alla de julio
  }
  
  return(dt)
}


### Función para crear matriz rala

one_hot_sparse <- function(data_set) {
  
  data_set <- as.data.table(data_set)
  
  require(Matrix)
  
  created <- FALSE
  if (sum(sapply(data_set, is.numeric)) > 0) {  # Si hay numéricos, pasamos los numéricos a una matriz esparsa (sería raro que no estuviese)
    out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
    # A las columnas numericas del dataset, las convierte en una matriz y despues en una matriz esparsa
    created <- TRUE
  }
  
  if (sum(sapply(data_set, is.logical)) > 0) {  # Si hay lógicos, pasamos los lógicos a una matriz esparsa y lo unimos con la matriz anterior
    if (created) {
      out_put_data <- cbind2(out_put_data,
                             as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix"))
    } else {
      out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
      created <- TRUE
    }
  }
  
  # Identificamos las columnas que son factor (OJO: el data.frame no debería tener character)
  fact_variables <- names(which(sapply(data_set, is.factor)))
  
  # Para cada columna factor hago one hot encoding
  i <- 0
  # Comenzar un bucle 'for' que itera a través de las variables categóricas en 'fact_variables'
  for (f_var in fact_variables) {
    
    # Obtener los niveles (categorías) de la variable categórica 'f_var' en el dataframe 'data_set'
    f_col_names <- levels(data_set[[f_var]])
    
    # Combinar el nombre de la variable categórica 'f_var' con cada uno de sus niveles
    # usando un guion bajo ('_') como separador, y reemplazar todos los espacios en blanco
    # en estos nombres combinados con un punto ('.')
    f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
    
    # Convertir la variable categórica 'f_var' del dataframe 'data_set' en un vector numérico,
    # asignando a cada nivel un número entero que representa su índice, y guardar el resultado
    # en la variable 'j_values'
    j_values <- as.numeric(data_set[[f_var]])
    
    
    if (sum(is.na(j_values)) > 0) {  # En categóricas, trato a NA como una categoría más
      j_values[is.na(j_values)] <- length(f_col_names) + 1 #le agrego un +1 al indice numerico de la variable
      #y se lo asigno a los valores faltantes
      f_col_names <- c(f_col_names, paste(f_var, "NA", sep = "_"))
      #le pongo el nombre NA
    }
    
    if (i == 0) {
      fact_data <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                x = rep(1, nrow(data_set)),
                                dims = c(nrow(data_set), length(f_col_names)))
      fact_data@Dimnames[[2]] <- f_col_names
    } else {
      fact_data_tmp <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                    x = rep(1, nrow(data_set)),
                                    dims = c(nrow(data_set), length(f_col_names)))
      fact_data_tmp@Dimnames[[2]] <- f_col_names
      fact_data <- cbind(fact_data, fact_data_tmp)
    }
    i <- i + 1
  }
  
  if (length(fact_variables) > 0) {
    if (created) {
      out_put_data <- cbind(out_put_data, fact_data)
    } else {
      out_put_data <- fact_data
      created <- TRUE
    }
  }
  return(out_put_data)
}

### Función para random_grid

random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {
  
  rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
    rep(min_nrounds, size)
  } else {
    sample(c(min_nrounds:max_nrounds),
           size = size, replace = TRUE)
  },
  max_depth = if (min_max_depth == max_max_depth) {
    rep(min_max_depth, size)
  } else {
    sample(c(min_max_depth:max_max_depth),
           size = size, replace = TRUE)
  },
  eta = if (min_eta == max_eta) {
    rep(min_eta, size)
  } else {
    round(runif(size, min_eta, max_eta), 7)
  },
  gamma = if (min_gamma == max_gamma) {
    rep(min_gamma, size)
  } else {
    round(runif(size, min_gamma, max_gamma), 7)
  },
  colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
    rep(min_colsample_bytree, size)
  } else {
    round(runif(size, min_colsample_bytree, max_colsample_bytree), 7)
  },
  min_child_weight = if (min_min_child_weight == max_min_child_weight) {
    rep(min_min_child_weight, size)
  } else {
    round(runif(size, min_min_child_weight, max_min_child_weight), 7)
  },
  subsample = if (min_subsample == max_subsample) {
    rep(min_subsample, size)
  } else {
    round(runif(size, min_subsample, max_subsample), 7)
  })
  
  return(rgrid)
}

### Función para entrenar xgboost

train_xgboost <- function(data_train, data_val, rgrid) {
  
  watchlist <- list(train = data_train, valid = data_val)
  
  predicted_models <- list()
  
  for (i in seq_len(nrow(rgrid))) {
    print(i)
    print(rgrid[i,])
    
    trained_model <- xgb.train(data = data_train,
                               params=as.list(rgrid[i, c("max_depth",
                                                         "eta",
                                                         "gamma",
                                                         "colsample_bytree",
                                                         "subsample",
                                                         "min_child_weight")]),
                               nrounds = rgrid[i, "nrounds"],
                               watchlist = watchlist,
                               objective = "binary:logistic",
                               eval.metric = "auc",
                               print_every_n = 10)
    
    perf_tr <- tail(trained_model$evaluation_log, 1)$train_auc
    perf_vd <- tail(trained_model$evaluation_log, 1)$valid_auc
    print(c(perf_tr, perf_vd))
    
    predicted_models[[i]] <- list(results = data.frame(rgrid[i,],
                                                       perf_tr = perf_tr,
                                                       perf_vd = perf_vd),
                                  model = trained_model)
    
    rm(trained_model)
    
    gc()
  }
  
  return(predicted_models)
}


### Función para traer result_table

result_table <- function(pred_models, higher_is_better = TRUE) {
  
  if (higher_is_better != TRUE) {
    order_coef <- 1
  } else {
    order_coef <- -1
  }
  
  res_table <- data.frame()
  i <- 1
  
  for (m in pred_models) {
    res_table <- rbind(res_table, data.frame(i = i, m$results))
    i <- i + 1
  }
  
  res_table <- res_table[order(order_coef * res_table$perf_vd),]
  
  return(res_table)
}

## Función para limitar hiperparametros

get_new_min_max <- function(res_table,min_perf_vd,max_perf_tr) {
  res_table$perf_tr = res_table$perf_tr * 100
  res_table$perf_vd = res_table$perf_vd * 100
  res_table <- 
    res_table %>% 
    filter(perf_vd > min_perf_vd) %>% 
    filter(perf_tr < max_perf_tr)
  best_params <- res_table %>% select(-perf_tr,-perf_vd)
  
  min_max_values <- list()
  print(paste0('Cantidad de modelos incluidos: ',length(best_params)))
  for (i in colnames(best_params)) {
    min_val <- min(best_params[[i]])
    max_val <- max(best_params[[i]])
    
    min_max_values[[paste0('min_', i)]] <- min_val
    min_max_values[[paste0('max_', i)]] <- max_val
    
    print(paste0('min_', i, ':', min_val))
    print(paste0('max_', i, ':', max_val))
  }
  
  return(min_max_values)
}
