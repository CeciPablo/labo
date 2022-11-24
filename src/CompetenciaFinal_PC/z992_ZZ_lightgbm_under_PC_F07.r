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

semillas <- c(983363, 962303, 125707, 300647, 864379, 659819, 629243, 320923, 679909, 335897, 956341,
              785501, 564227, 257687, 238001, 174763, 131071, 101377, 100019, 999979, 692089, 936659,
              281887, 681979, 732497, 676241, 914371, 433889, 945577, 668111, 888887, 888869, 888857,
              888827, 888809, 819503, 348431, 124987, 882019, 838609, 700471, 700459, 700433, 700429,
              268123, 633337, 886007, 450473, 635837, 902389, 283117, 879181, 915697, 490577, 839369,
              704017, 621923, 502133, 469363, 210127, 663589, 976279, 690143, 191911, 100103, 100069,
              100049, 892237, 552581, 410551, 200177, 734131, 644131, 742111, 725111, 724111, 113111,
              700423, 637597, 909463, 705689, 962041, 807299, 562417, 562409, 562403, 562399, 562361,
              300007, 100057, 700001, 600011, 500009, 666647, 666637, 666617, 290923, 417227, 869587,
              102191)

#Parametros del script
PARAM  <- list()
PARAM$experimento  <- "COMPfinal/ZZ9420_M17_US100_ST100_r7"
PARAM$exp_input  <- "COMPfinal/HT9420_M17_US100_ST100_r7"

for (q in semillas ){
  
  PARAM$submit <- paste0("ZZ9420_M17_US100_S",q)
  
  PARAM$modelos  <- 3
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
                            sprintf( "_M%02d", i ),
                            "_",
                            sprintf( "IOB%03d", iteracion_bayesiana ),
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
                         sprintf( "_M%02d", i ),
                         "_",
                         sprintf( "IOB%03d", iteracion_bayesiana),
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