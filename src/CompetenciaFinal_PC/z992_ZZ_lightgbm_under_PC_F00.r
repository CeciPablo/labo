#Necesita para correr en Google Cloud
# 128 GB de memoria RAM
# 256 GB de espacio en el disco local
#   8 vCPU

# ZZ final que necesita de UNDERSAMPLING

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

require("lightgbm")

<<<<<<< HEAD
semillas <- c(102191, 869587, 417227, 290923, 666617, 666637, 666647, 500009, 600011, 700001, 100057,
              300007, 562361, 562399, 562403, 562409, 562417, 807299, 962041, 705689, 909463, 637597,
              700423, 700429, 700433, 700459, 700471, 838609, 882019, 124987, 348431, 819503, 888809,
              888827, 888857, 888869, 888887, 668111, 945577, 433889, 914371, 676241, 732497, 681979,
              281887, 936659, 692089, 999979, 100019, 101377, 131071, 174763, 238001, 257687, 564227,
              785501, 956341, 335897, 679909, 320923, 629243, 659819, 864379, 300647, 125707, 962303,
              983363, 113111, 724111, 725111, 742111, 644131, 734131, 200177, 410551, 552581, 892237,
              100049, 100069, 100103, 191911, 690143, 976279, 663589, 210127, 469363, 502133, 621923,
              704017, 839369, 490577, 915697, 879181, 283117, 902389, 635837, 450473, 886007, 633337,
              268123)

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "COMPfinal/ZZ9420_M17_US100_ST100"
PARAM$exp_input  <- "COMPfinal/HT9420_M17_US100_ST100"


for (q in semillas ){
  
  PARAM$submit <- paste0("ZZ9420_M17_US100_S",q)
  
  PARAM$modelos  <- 1
  # FIN Parametros del script
  
  ksemilla  <- q
  
  #------------------------------------------------------------------------------
  options(error = function() { 
    traceback(20); 
    options(error = NULL); 
    stop("exiting after script error") 
  })
  #------------------------------------------------------------------------------
  #------------------------------------------------------------------------------
  #Aqui empieza el programa
  
  base_dir <- "~/buckets/b1/"
  
  #creo la carpeta donde va el experimento
  dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
  setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO
  
  #leo la salida de la optimizaciob bayesiana
  arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
  tb_log  <- fread( arch_log )
  setorder( tb_log, -ganancia )
  
  #leo el nombre del expermento de la Training Strategy
  arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
  TS  <- readLines( arch_TS, warn=FALSE )
  
  #leo el dataset donde voy a entrenar el modelo final
  arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
  dataset  <- fread( arch_dataset )
  
  ## Filtro los meses porque Training Stretegy tiene dos meses de mas ##Lo quito xq esto era del experimento colectivo
  #unique_foto_mes <- unique(dataset$foto_mes)
  #unique_foto_mes_sorted <- sort(unique_foto_mes)
  #meses_a_quedarse <- unique_foto_mes_sorted[3:length(unique_foto_mes_sorted)]
  #dataset <- dataset[foto_mes %in% meses_a_quedarse, ]
  
  
  #leo el dataset donde voy a aplicar el modelo final
  arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
  dfuture <- fread( arch_future )
  
  
  #defino la clase binaria
  dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]
  
  campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )
  
  
  #genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
  for( i in  1:PARAM$modelos )
  {
    parametros  <- as.list( copy( tb_log[ i ] ) )
    iteracion_bayesiana  <- parametros$iteracion_bayesiana
    
    arch_modelo  <- paste0( "modelo_S" , q,
                            sprintf( "%02d", i ),
                            "_",
                            sprintf( "%03d", iteracion_bayesiana ),
                            ".model" )
    
    
    #creo CADA VEZ el dataset de lightgbm
    dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                            label=   dataset[ , clase01],
                            weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                            free_raw_data= FALSE
    )
    
    ganancia  <- parametros$ganancia
    
    #elimino los parametros que no son de lightgbm
    parametros$experimento  <- NULL
    parametros$cols         <- NULL
    parametros$rows         <- NULL
    parametros$fecha        <- NULL
    parametros$prob_corte   <- NULL
    parametros$estimulos    <- NULL
    parametros$ganancia     <- NULL
    parametros$iteracion_bayesiana  <- NULL
    
    if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
    if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )
    
    #Primero defino el tamaño de las hojas
    parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
    #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
    parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
    cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )
    
    #ya no me hacen falta
    parametros$leaf_size_log  <- NULL
    parametros$coverage  <- NULL
    
    #Utilizo la semilla definida en este script
    parametros$seed  <- ksemilla
    
    #genero el modelo entrenando en los datos finales
    set.seed( parametros$seed )
    modelo_final  <- lightgbm( data= dtrain,
                               param=  parametros,
                               verbose= -100 )
    
    #grabo el modelo, achivo .model
    lgb.save( modelo_final,
              file= arch_modelo )
    
    #creo y grabo la importancia de variables
    tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
    fwrite( tb_importancia,
            file= paste0( "impo_S", q, 
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".txt" ),
            sep= "\t" )
    
    
    #genero la prediccion, Scoring
    prediccion  <- predict( modelo_final,
                            data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )
    
    tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
    tb_prediccion[ , prob := prediccion ]
    
    
    nom_pred  <- paste0( "pred_S", q,
                         sprintf( "%02d", i ),
                         "_",
                         sprintf( "%03d", iteracion_bayesiana),
                         ".csv"  )
    
    fwrite( tb_prediccion,
            file= nom_pred,
            sep= "\t" )
    
    
    ##genero los archivos para Kaggle
    #cortes  <- seq( from=  7000,
    #                to=   11000,
    #                by=     500 )
    #
    #
    #setorder( tb_prediccion, -prob )
    #
    #for( corte in cortes )
    #{
    #  tb_prediccion[  , Predicted := 0L ]
    #  tb_prediccion[ 1:corte, Predicted := 1L ]
    #  
    #  nom_submit  <- paste0( PARAM$submit, 
    #                         "_",
    #                         sprintf( "%02d", i ),
    #                         "_",
    #                         sprintf( "%03d", iteracion_bayesiana ),
    #                         "_",
    #                         sprintf( "%05d", corte ),
    #                         ".csv" )
    #  
    #  fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
    #           file= nom_submit,
    #           sep= "," )
    #  
    #}
    
    
    #borro y limpio la memoria para la vuelta siguiente del for
    rm( tb_prediccion )
    rm( tb_importancia )
    rm( modelo_final)
    rm( parametros )
    rm( dtrain )
    gc()
  }
  
}  
=======
#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "ZZ9420"
PARAM$exp_input  <- "HT9420"

PARAM$modelos  <- 2
# FIN Parametros del script

ksemilla  <- 102191

#------------------------------------------------------------------------------
options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

base_dir <- "~/buckets/b1/"

#creo la carpeta donde va el experimento
dir.create( paste0( base_dir, "exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd(paste0( base_dir, "exp/", PARAM$experimento, "/"))   #Establezco el Working Directory DEL EXPERIMENTO

#leo la salida de la optimizaciob bayesiana
arch_log  <- paste0( base_dir, "exp/", PARAM$exp_input, "/BO_log.txt" )
tb_log  <- fread( arch_log )
setorder( tb_log, -ganancia )

#leo el nombre del expermento de la Training Strategy
arch_TS  <- paste0( base_dir, "exp/", PARAM$exp_input, "/TrainingStrategy.txt" )
TS  <- readLines( arch_TS, warn=FALSE )

#leo el dataset donde voy a entrenar el modelo final
arch_dataset  <- paste0( base_dir, "exp/", TS, "/dataset_train_final.csv.gz" )
dataset  <- fread( arch_dataset )

#leo el dataset donde voy a aplicar el modelo final
arch_future  <- paste0( base_dir, "exp/", TS, "/dataset_future.csv.gz" )
dfuture <- fread( arch_future )


#defino la clase binaria
dataset[ , clase01 := ifelse( clase_ternaria %in% c("BAJA+1","BAJA+2"), 1, 0 )  ]

campos_buenos  <- setdiff( colnames(dataset), c( "clase_ternaria", "clase01") )


#genero un modelo para cada uno de las modelos_qty MEJORES iteraciones de la Bayesian Optimization
for( i in  1:PARAM$modelos )
{
  parametros  <- as.list( copy( tb_log[ i ] ) )
  iteracion_bayesiana  <- parametros$iteracion_bayesiana

  arch_modelo  <- paste0( "modelo_" ,
                          sprintf( "%02d", i ),
                          "_",
                          sprintf( "%03d", iteracion_bayesiana ),
                          ".model" )


  #creo CADA VEZ el dataset de lightgbm
  dtrain  <- lgb.Dataset( data=    data.matrix( dataset[ , campos_buenos, with=FALSE] ),
                          label=   dataset[ , clase01],
                          weight=  dataset[ , ifelse( clase_ternaria %in% c("BAJA+2"), 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  ganancia  <- parametros$ganancia

  #elimino los parametros que no son de lightgbm
  parametros$experimento  <- NULL
  parametros$cols         <- NULL
  parametros$rows         <- NULL
  parametros$fecha        <- NULL
  parametros$prob_corte   <- NULL
  parametros$estimulos    <- NULL
  parametros$ganancia     <- NULL
  parametros$iteracion_bayesiana  <- NULL

  if( ! ("leaf_size_log" %in% names(parametros) ) )  stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  lead_size_log.\n" )
  if( ! ("coverage" %in% names(parametros) ) ) stop( "El Hyperparameter Tuning debe tener en BO_log.txt  el pseudo hiperparametro  coverage.\n" )
  
  #Primero defino el tamaño de las hojas
  parametros$min_data_in_leaf  <- pmax( 1,  round( nrow(dtrain) / ( 2.0 ^ parametros$leaf_size_log ))  )
  #Luego la cantidad de hojas en funcion del valor anterior, el coverage, y la cantidad de registros
  parametros$num_leaves  <-  pmin( 131072, pmax( 2,  round( parametros$coverage * nrow( dtrain ) / parametros$min_data_in_leaf ) ) )
  cat( "min_data_in_leaf:", parametros$min_data_in_leaf,  ",  num_leaves:", parametros$num_leaves, "\n" )

  #ya no me hacen falta
  parametros$leaf_size_log  <- NULL
  parametros$coverage  <- NULL

  #Utilizo la semilla definida en este script
  parametros$seed  <- ksemilla
  
  #genero el modelo entrenando en los datos finales
  set.seed( parametros$seed )
  modelo_final  <- lightgbm( data= dtrain,
                             param=  parametros,
                             verbose= -100 )

  #grabo el modelo, achivo .model
  lgb.save( modelo_final,
            file= arch_modelo )

  #creo y grabo la importancia de variables
  tb_importancia  <- as.data.table( lgb.importance( modelo_final ) )
  fwrite( tb_importancia,
          file= paste0( "impo_", 
                        sprintf( "%02d", i ),
                        "_",
                        sprintf( "%03d", iteracion_bayesiana ),
                        ".txt" ),
          sep= "\t" )


  #genero la prediccion, Scoring
  prediccion  <- predict( modelo_final,
                          data.matrix( dfuture[ , campos_buenos, with=FALSE ] ) )

  tb_prediccion  <- dfuture[  , list( numero_de_cliente, foto_mes ) ]
  tb_prediccion[ , prob := prediccion ]


  nom_pred  <- paste0( "pred_",
                       sprintf( "%02d", i ),
                       "_",
                       sprintf( "%03d", iteracion_bayesiana),
                       ".csv"  )

  fwrite( tb_prediccion,
          file= nom_pred,
          sep= "\t" )


  #genero los archivos para Kaggle
  cortes  <- seq( from=  7000,
                  to=   11000,
                  by=     500 )


  setorder( tb_prediccion, -prob )

  for( corte in cortes )
  {
    tb_prediccion[  , Predicted := 0L ]
    tb_prediccion[ 1:corte, Predicted := 1L ]

    nom_submit  <- paste0( PARAM$experimento, 
                           "_",
                           sprintf( "%02d", i ),
                           "_",
                           sprintf( "%03d", iteracion_bayesiana ),
                           "_",
                           sprintf( "%05d", corte ),
                           ".csv" )

    fwrite(  tb_prediccion[ , list( numero_de_cliente, Predicted ) ],
             file= nom_submit,
             sep= "," )

  }


  #borro y limpio la memoria para la vuelta siguiente del for
  rm( tb_prediccion )
  rm( tb_importancia )
  rm( modelo_final)
  rm( parametros )
  rm( dtrain )
  gc()
}

>>>>>>> 7d92b54291d1de4804a0e105b78075cc20531a0e
