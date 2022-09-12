#Experimento 00
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

      #Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dtest  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo

#train_rows <- createDataPartition(dataset$clase_ternaria, p= 0.70,list= FALSE)
#dtrain  <- dataset[ train_rows]
#dtest  <-  dataset[ -train_rows]

#n_distinct(dapply$numero_de_cliente)
#n_distinct(dataset$numero_de_cliente)

param  <- list("cp"= -1,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

prp(modelo, extra=101, digits=5, branch=.2, type=4, varlen=0, faclen=0, tweak=0.3, cex=0.8,
    split.cex = 1.8, uniform = TRUE, compress = TRUE)


prediccion  <- predict( modelo, dtest, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )


ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
ganancia_testing

ganancia_testing_normalizada  <-  ganancia_testing/0.3
ganancia_testing_normalizada

fwrite( dtest[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_00.csv",
        sep=  "," )


# ganancia_testing              -15256000
# ganancia_testing_normalizada  -50853333

################################################################################
################################################################################
#Experimento 02
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "BAJA+2",
  "CONTINUA")]

# Borramos el target viejo
dataset[, clase_ternaria := NULL]


dtrain  <- dataset[ foto_mes==202101 ]  #defino donde voy a entrenar
dtest  <- dataset[ foto_mes==202103 ]  #defino donde voy a aplicar el modelo


#Buscando con una Opt. Bayesiana para 3 parámetros (Corrida en zero2hero_0101)
      #Recommended parameters:
      #  maxdepth=6; minsplit=155; minbucket=0.424
      #Objective: y = 0.892

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= 0.424,
               "maxdepth"= 6)

modelo <-  rpart::rpart(formula= "clase_binaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0, tweak=0.3, cex=0.8)

prediccion  <- predict( modelo, dtest, type = "prob")

prob_baja2  <- prediccion[, "BAJA+2"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )


ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_binaria=="BAJA+2", 78000, -2000) )]
ganancia_testing

ganancia_testing_normalizada  <-  ganancia_testing/0.3
ganancia_testing_normalizada

fwrite( dtest[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_01.csv",
        sep=  "," )


# ganancia_testing              -15256000
# ganancia_testing_normalizada  -50853333

################################################################################
################################################################################
#Experimento 03
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD:20.12

################################################################################
################################################################################
#Experimento 04 - Con parámetros a partir de OB

rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= 0,
               "minsplit"=  155,
               "minbucket"= 0.424,
               "maxdepth"= 6)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ ." ,
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD:20.76

################################################################################
################################################################################
#Experimento 05
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

dataset[ , campoJ := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo < 2000.5 ) ]               
dataset[ , c( "active_quarter","mcaja_ahorro", "mtarjeta_visa_consumo"):=NULL ]               


dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= 0,
               "minsplit"=  155,
               "minbucket"= 0.424,
               "maxdepth"= 6)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD:20.76 ==> Agregar la variable JUAN NO MEJORÓ EL RESULTADO


################################################################################
################################################################################
#Experimento 06
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

modelo <-  rpart::rpart(formula= "clase_ternaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD:20.17333 ==> Agregar Columnas para ayudar al árbol está dando resultados Vs 20.12 del original

################################################################################
################################################################################
#Experimento 07 - Vuelvo a intentar Canaritos

rm( list=ls() )
gc()
require("data.table")
require("rpart")
require("rpart.plot")
setwd("/dmef" )
dataset  <- fread( "./datasets/competencia1_2022.csv")
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula= "clase_ternaria ~ .",
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

#Quiero ver como performa CANARITOS con respecto a lo que venía haciendo
set.seed(semillas[1])
pariticion <- 0.7
train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain2  <- dtrain[ train_rows]
dtest  <-  dtrain[ -train_rows]

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo_pruned, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)

ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)


#GNORM MM USD:25.55333 ==> Aplicar CANARITOS ha generado una mejora sustancial

prediccion  <- predict( modelo_pruned, dapply, type = "prob")[,"BAJA+2"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_07.csv",
        sep=  "," )

# EN KAGGLE TIRÓ GANANCIA NEGATIVA!!!!!!!!

################################################################################
################################################################################
#Experimento 08 - Vuelvo a intentar Canaritos

rm( list=ls() )
gc()
require("data.table")
require("rpart")
require("rpart.plot")
setwd("/dmef" )
dataset  <- fread( "./datasets/competencia1_2022.csv")
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula=  "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

#Quiero ver como performa CANARITOS con respecto a lo que venía haciendo
set.seed(semillas[1])
pariticion <- 0.7
train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain2  <- dtrain[ train_rows]
dtest  <-  dtrain[ -train_rows]

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo_pruned, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)

ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)


#GNORM MM USD:25.55333 ==> Aplicar CANARITOS ha generado una mejora sustancial sobre testing de pruebas anteriores

#Veamos cómo resulta sobre dapply
prediccionII  <- predict( modelo_pruned, dapply, type = "prob")
prob_baja2II  <- prediccionII[, "BAJA+2"]

ganancia_dapply <- dapply[ , sum(  (prob_baja2II>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_dapply/1000000)

# GANANCIA NEGATIVA!!!!!!!!


Predicted   <- ifelse( prob_baja2II > 0.025, 1, 0 )

fwrite( dapply[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_08.csv",
        sep=  "," )


################################################################################
################################################################################
#Experimento 09 - Vuelvo a intentar Canaritos

rm( list=ls() )
gc()
require("data.table")
require("rpart")
require("rpart.plot")
setwd("/dmef" )
dataset  <- fread( "./datasets/competencia1_2022.csv")
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#agrego 30 canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]

dtrain <- dataset[ foto_mes==202101 ]
dapply <- dataset[ foto_mes==202103 ]

#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula=  "clase_ternaria ~ . -mcomisiones_mantenimiento -Visa_mpagado",
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )

#Quiero ver como performa CANARITOS con respecto a lo que venía haciendo
set.seed(semillas[1])
pariticion <- 0.7
train_rows <- createDataPartition(dataset$clase_ternaria, p= pariticion,list= FALSE)
dtrain2  <- dtrain[ train_rows]
dtest  <-  dtrain[ -train_rows]

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo_pruned, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "BAJA+2"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_ternaria=="BAJA+2", 78000, -2000) )]
print(ganancia_testing/1000000)

ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)


#GNORM MM USD:25.54667 ==> Aplicar CANARITOS ha generado una mejora sustancial sobre testing de pruebas anteriores

#Veamos cómo resulta sobre dapply
prediccionII  <- predict( modelo_pruned, dapply, type = "prob")
prob_baja2II  <- prediccionII[, "BAJA+2"]

Predicted   <- ifelse( prob_baja2II > 0.025, 1, 0 )

dapply[ , predicho:=ifelse( prob_baja2II > 0.025, 1, 0 ) ]

ganancia_dapply <- dapply[ , sum(ifelse( predicho ==1, 78000, -2000) )]
print(ganancia_dapply/1000000)



fwrite( dapply[ , list(numero_de_cliente, predicho) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_09.01.csv",
        sep=  "," )

################################################################################
################################################################################
#Experimento 10
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo


pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

modelo <-  rpart::rpart(formula= "clase_binaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "evento"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_binaria=="evento", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD: 21.97333 ==> Binarizar mejoró el resultado

set.seed(semillas[1])
prediccionII  <- predict( modelo, dataset_PRED, type = "prob")
prob_baja2II  <- prediccionII[, "evento"]

Predicted   <- ifelse( prob_baja2II > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2II > 0.025, 1, 0 ) ]

ganancia_dataset_PRED <- dataset_PRED[ , sum(ifelse( Predicted ==1, 78000, -2000) )]
print(ganancia_dataset_PRED/1000000)



fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_10.csv",
        sep=  "," )



################################################################################
################################################################################
#Experimento 11
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo


pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]

param  <- list("cp"= -0.5,
               "minsplit"=  900,
               "minbucket"= 440,
               "maxdepth"= 5)

modelo <-  rpart::rpart(formula= "clase_binaria ~ .",
                        data= dtrain,
                        xval= 0,
                        control= param)

#aplico el modelo a los datos de testing
prediccion  <- predict( modelo, dtest, type = "prob")

#Calculo la ganancia del modelo en los datos de testing
prob_baja2  <- prediccion[, "evento"] #a partir de la prediccion, calculo la probabilidad de BAJA+2 de cada registro de testing dtest


# Para calcular la ganancia del modelo aplicado a testing debo tener en cuenta:
# Solo estimulo a registros que el modelo asigno prob > 0.025
# Si no estimulo, no gano ni pierdo.
# Si estimulo un BAJA+2 => ganancia +78000
# Si estimulo un BAJA+1 => ganancia -2000
# Si estimulo un CONTINUA => ganancia -2000

ganancia_testing <- dtest[ , sum(  (prob_baja2>0.025) * ifelse( clase_binaria=="evento", 78000, -2000) )]
print(ganancia_testing/1000000)


# Es importante entender que la ganancia esta calculada sobre los datos de testing,
# que en este caso son apenas el 30%
# Si quiero extrapolar a todo el dataset, debo hacer el cociente de esa ganancia por 0.30


ganancia_testing_normalizada  <-  ganancia_testing/(1-pariticion)
print(ganancia_testing_normalizada/1000000)

#GNORM MM USD: 21.97333 ==> Binarizar mejoró el resultado

set.seed(semillas[1])
prediccionII  <- predict( modelo, dataset_PRED, type = "prob")
prob_baja2II  <- prediccionII[, "evento"]

Predicted   <- ifelse( prob_baja2II > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2II > 0.025, 1, 0 ) ]

ganancia_dataset_PRED <- dataset_PRED[ , sum(ifelse( Predicted ==1, 78000, -2000) )]
print(ganancia_dataset_PRED/1000000)



fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_11.csv",
        sep=  "," )



################################################################################
################################################################################
#Experimento 12
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo

set.seed(semillas[1])

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]


calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / (0.3)
  )
}


# Antes de empezar vamos a ver la importancia de variables

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= (0.424*155),
               "maxdepth"= 6)



modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                control = param)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


summary(modelo)

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "cliente_edad",
                   "cliente_antiguedad",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente"
                    )

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dataset_PRED[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)] #se lo agrego porque canaritos lo necesita
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                     "mrentabilidad_annual",
                     "mcaja_ahorro_dolares",
                     "cprestamos_personales",
                     "cprestamos_prendarios",
                     "cprestamos_hipotecarios",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos",
                     "cseguro_vida",
                     "cseguro_vivienda",
                     "ccaja_seguridad",
                     "mcuenta_debitos_automaticos",
                     "mttarjeta_visa_debitos_automaticos",
                     "mttarjeta_master_debitos_automaticos",
                     "ccheques_emitidos",
                     "cliente_vip",
                     "mprestamos_personales",
                     "mprestamos_prendarios",
                     "mprestamos_hipotecarios",
                     "r_ctrx_quarter",
                     "r_cliente_edad",
                     "r_mprestamos_personales",
                     "r_mactivos_margen",
                     "r_mcuenta_corriente",
                     "r_cliente_antiguedad",
                     "r_mcuentas_saldo",
                     "r_mcaja_ahorro"
                      )


campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
  for (var2 in mis_variables_2) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dtrain[, (nueva) := get(var1) * get(var2)]
      dtest[, (nueva) := get(var1) * get(var2)]
      dataset_PRED[, (nueva) := get(var1) * get(var2)] #se lo agrego porque canaritos lo va a necesitar
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo6$variable.importance)


##########AHORA LE HAGO CANARITOS!!!!!!

#agrego 30 canaritos
for( i in 1:33 ) dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain)) ]

set.seed(semillas[1])
#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula2,
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )


set.seed(semillas[1])
prediccion  <- predict( modelo_pruned, dataset_PRED, type = "prob")
prob_baja2  <- prediccion[, "evento"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2 > 0.025, 1, 0 ) ]

fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_12.csv",
        sep=  "," )


################################################################################
################################################################################
#Experimento 13
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo

set.seed(semillas[1])

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]


calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / (0.3)
  )
}


# Antes de empezar vamos a ver la importancia de variables

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= (0.424*155),
               "maxdepth"= 6)



modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                control = param)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


summary(modelo)

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "cliente_edad",
                   "cliente_antiguedad",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente"
)

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dataset_PRED[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)] #se lo agrego porque canaritos lo necesita
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                     "mrentabilidad_annual",
                     "mcaja_ahorro_dolares",
                     "cprestamos_personales",
                     "cprestamos_prendarios",
                     "cprestamos_hipotecarios",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos",
                     "cseguro_vida",
                     "cseguro_vivienda",
                     "ccaja_seguridad",
                     "mcuenta_debitos_automaticos",
                     "mttarjeta_visa_debitos_automaticos",
                     "mttarjeta_master_debitos_automaticos",
                     "ccheques_emitidos",
                     "cliente_vip",
                     "mprestamos_personales",
                     "mprestamos_prendarios",
                     "mprestamos_hipotecarios",
                     "r_ctrx_quarter",
                     "r_cliente_edad",
                     "r_mprestamos_personales",
                     "r_mactivos_margen",
                     "r_mcuenta_corriente",
                     "r_cliente_antiguedad",
                     "r_mcuentas_saldo",
                     "r_mcaja_ahorro"
)


campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
  for (var2 in mis_variables_2) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dtrain[, (nueva) := get(var1) / get(var2)]
      dtest[, (nueva) := get(var1) / get(var2)]
      dataset_PRED[, (nueva) := get(var1) / get(var2)] #se lo agrego porque canaritos lo va a necesitar
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo6$variable.importance)


##########AHORA LE HAGO CANARITOS!!!!!!

#agrego 30 canaritos
for( i in 1:33 ) dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain)) ]

set.seed(semillas[1])
#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula2,
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )


set.seed(semillas[1])
prediccion  <- predict( modelo_pruned, dataset_PRED, type = "prob")
prob_baja2  <- prediccion[, "evento"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2 > 0.025, 1, 0 ) ]

fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_13.csv",
        sep=  "," )


################################################################################
################################################################################
################################################################################
################################################################################
#Experimento 14
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo

# Creo nuevas variables
dataset[ , campoJ0 := as.integer( mprestamos_personales > 0) ]               
dataset[ , campoJ1 := as.integer( mprestamos_prendarios > 0) ]
dataset[ , campoJ2 := as.integer( mprestamos_hipotecarios > 0)]
dataset[ , campoJ3 := as.integer( cseguro_vida > 0 | cseguro_vivienda > 0 | cseguro_auto >0 | cseguro_accidentes_personales)]
dataset[ , campoJ4 := as.integer( cseguro_vida > 0 & cseguro_vivienda > 0 & cseguro_auto >0)]
#"""""""""""""""""""""""""""""""""""""""""

set.seed(semillas[1])

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]


calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / (0.3)
  )
}


# Antes de empezar vamos a ver la importancia de variables

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= (0.424*155),
               "maxdepth"= 6)



modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                control = param)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


summary(modelo)

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "cliente_edad",
                   "cliente_antiguedad",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente"
)

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dataset_PRED[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)] #se lo agrego porque canaritos lo necesita
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                     "mrentabilidad_annual",
                     "mcaja_ahorro_dolares",
                     "cprestamos_personales",
                     "cprestamos_prendarios",
                     "cprestamos_hipotecarios",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos",
                     "cseguro_vida",
                     "cseguro_vivienda",
                     "ccaja_seguridad",
                     "mcuenta_debitos_automaticos",
                     "mttarjeta_visa_debitos_automaticos",
                     "mttarjeta_master_debitos_automaticos",
                     "ccheques_emitidos",
                     "cliente_vip",
                     "mprestamos_personales",
                     "mprestamos_prendarios",
                     "mprestamos_hipotecarios",
                     "r_ctrx_quarter",
                     "r_cliente_edad",
                     "r_mprestamos_personales",
                     "r_mactivos_margen",
                     "r_mcuenta_corriente",
                     "r_cliente_antiguedad",
                     "r_mcuentas_saldo",
                     "r_mcaja_ahorro"
)


campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
  for (var2 in mis_variables_2) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dtrain[, (nueva) := get(var1) * get(var2)]
      dtest[, (nueva) := get(var1) * get(var2)]
      dataset_PRED[, (nueva) := get(var1) * get(var2)] #se lo agrego porque canaritos lo va a necesitar
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo6$variable.importance)


##########AHORA LE HAGO CANARITOS!!!!!!

#agrego 30 canaritos
for( i in 1:33 ) dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain)) ]

set.seed(semillas[1])
#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula2,
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )


set.seed(semillas[1])
prediccion  <- predict( modelo_pruned, dataset_PRED, type = "prob")
prob_baja2  <- prediccion[, "evento"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2 > 0.025, 1, 0 ) ]

fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_14.csv",
        sep=  "," )


################################################################################
################################################################################
#Experimento 15
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

library( "data.table")   #cargo la libreria  data.table
library( "rpart")  #cargo la libreria  rpart
library("rpart.plot") #cargo la libreria  rpart.plot
library("caret") # cargo librería para particionar
library(dplyr)

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo

# Creo nuevas variables
dataset[ , campoJ0 := as.integer( mprestamos_personales > 0) ]               
dataset[ , campoJ1 := as.integer( mprestamos_prendarios > 0) ]
dataset[ , campoJ2 := as.integer( mprestamos_hipotecarios > 0)]
dataset[ , campoJ3 := as.integer( cseguro_vida > 0 | cseguro_vivienda > 0 | cseguro_auto >0 | cseguro_accidentes_personales)]
dataset[ , campoJ4 := as.integer( cseguro_vida > 0 & cseguro_vivienda > 0 & cseguro_auto >0)]

dataset[ , campoJ5 := log10(mprestamos_personales)]               
dataset[ , campoJ6 := log10(mprestamos_prendarios)]
dataset[ , campoJ7 := log10(mprestamos_hipotecarios)]
#"""""""""""""""""""""""""""""""""""""""""

set.seed(semillas[1])

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]


calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / (0.3)
  )
}


# Antes de empezar vamos a ver la importancia de variables

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= (0.424*155),
               "maxdepth"= 6)



modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                control = param)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


summary(modelo)

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "cliente_edad",
                   "cliente_antiguedad",
                   "mprestamos_personales",
                   "mcuentas_saldo",
                   "mactivos_margen",
                   "mcaja_ahorro",
                   "mcuenta_corriente"
)

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dataset_PRED[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)] #se lo agrego porque canaritos lo necesita
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                     "mrentabilidad_annual",
                     "mcaja_ahorro_dolares",
                     "cprestamos_personales",
                     "cprestamos_prendarios",
                     "cprestamos_hipotecarios",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos",
                     "cseguro_vida",
                     "cseguro_vivienda",
                     "ccaja_seguridad",
                     "mcuenta_debitos_automaticos",
                     "mttarjeta_visa_debitos_automaticos",
                     "mttarjeta_master_debitos_automaticos",
                     "ccheques_emitidos",
                     "cliente_vip",
                     "mprestamos_personales",
                     "mprestamos_prendarios",
                     "mprestamos_hipotecarios",
                     "r_ctrx_quarter",
                     "r_cliente_edad",
                     "r_mprestamos_personales",
                     "r_mactivos_margen",
                     "r_mcuenta_corriente",
                     "r_cliente_antiguedad",
                     "r_mcuentas_saldo",
                     "r_mcaja_ahorro"
)


campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
  for (var2 in mis_variables_2) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dtrain[, (nueva) := get(var1) * get(var2)]
      dtest[, (nueva) := get(var1) * get(var2)]
      dataset_PRED[, (nueva) := get(var1) * get(var2)] #se lo agrego porque canaritos lo va a necesitar
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo6$variable.importance)


##########AHORA LE HAGO CANARITOS!!!!!!

#agrego 30 canaritos
for( i in 1:33 ) dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain)) ]

set.seed(semillas[1])
#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula2,
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )


set.seed(semillas[1])
prediccion  <- predict( modelo_pruned, dataset_PRED, type = "prob")
prob_baja2  <- prediccion[, "evento"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2 > 0.025, 1, 0 ) ]

fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_15_prueba.csv",
        sep=  "," )


################################################################################
################################################################################
################################################################################
################################################################################
#Experimento 16
rm( list=ls() )  #limpio la memoria - remove all objects
gc()             #limpio la memoria - garbage collection

require("data.table")
require("rpart")
require("ROCR")
require("ggplot2")
require("lubridate")
require("lhs")
require("DiceKriging")
require("mlrMBO")
require("rgenoud")
require("dplyr")
require("rlist")
#install.packages("Hmisc")
require("Hmisc")


#library( "data.table")   #cargo la libreria  data.table
#library( "rpart")  #cargo la libreria  rpart
#library("rpart.plot") #cargo la libreria  rpart.plot
#library("caret") # cargo librería para particionar
#library(dplyr)
##install.packages("rlist")
#library("rlist")

options(repr.plot.width=50, repr.plot.height=50)  #para que los gráficos me salgan legibles

setwd("/dmef") #Aqui se debe poner la ruta de la PC local
dataset <- fread("./datasets/competencia1_2022.csv")

#Entrenar arbol de decision
semillas <- c(100057, 300007, 500009, 600011, 700001)
set.seed(semillas[1])

#dataset[ , campoJ := as.integer(ctrx_quarter<14 & mcuentas_saldo < -1256) ]               

# Bining


for (campo in colnames(dataset)) {
  if (dataset[, length(unique(get(campo))) > 100]) {
    dataset[, paste0(campo, "_bin") := as.integer(cut2(dataset[, get(campo)], m = 1, g = 63))]
    if (campo != "numero_de_cliente") dataset[, paste0(campo) := NULL]
  }
  #cat(campo, " ")
}


#'''''''''''''''''''''''''''''''''''''


dataset_PRED <- dataset[ foto_mes==202103 ]  # me quedo solo con el periodo 202103
dataset <- dataset[ foto_mes==202101 ]  # me quedo solo con el periodo 202101


# Creamos una clase binaria
dataset[, clase_binaria := ifelse(
  (clase_ternaria == "BAJA+2"),
  "evento",
  "noevento"
)]

dataset[, clase_ternaria := NULL] # Borramos el target viejo

# Creo nuevas variables
dataset[ , campoJ0 := as.integer( mprestamos_personales > 0) ]               
dataset[ , campoJ1 := as.integer( mprestamos_prendarios_bin > 0) ]
dataset[ , campoJ2 := as.integer( mprestamos_hipotecarios_bin > 0)]
dataset[ , campoJ3 := as.integer( cseguro_vida > 0 | cseguro_vivienda > 0 | cseguro_auto >0 | cseguro_accidentes_personales)]
dataset[ , campoJ4 := as.integer( cseguro_vida > 0 & cseguro_vivienda > 0 & cseguro_auto >0)]

dataset[ , campoJ5 := log10(mprestamos_personales)]               
#dataset[ , campoJ6 := log10(mprestamos_prendarios)]
#dataset[ , campoJ7 := log10(mprestamos_hipotecarios)]
#"""""""""""""""""""""""""""""""""""""""""





set.seed(semillas[1])

pariticion <- 0.7

train_rows <- createDataPartition(dataset$clase_binaria, p= pariticion,list= FALSE)
dtrain  <- dataset[ train_rows]
dtest  <-  dataset[ -train_rows]


calcular_ganancia <- function(modelo, test) {
  pred_testing <- predict(modelo, test, type = "prob")
  sum(
    (pred_testing[, "evento"] >= 0.025) * ifelse(test$clase_binaria == "evento",
                                                 78000, -2000) / (0.3)
  )
}


# Antes de empezar vamos a ver la importancia de variables

param  <- list("cp"= -1,
               "minsplit"=  155,
               "minbucket"= (0.424*155),
               "maxdepth"= 6)



modelo <- rpart(clase_binaria ~ .,
                data = dtrain,
                xval = 0,
                control = param)

calcular_ganancia(modelo, dtest)

print(modelo$variable.importance)


summary(modelo)

## ---------------------------
## Step 8: Un poco de R, como procesar multiples variables con una técnica 
## ---------------------------

# Supongamos que tenemos una lista de variables a las que queremos transformar
mis_variables <- c("ctrx_quarter",
                   "cliente_edad",
                   "cliente_antiguedad_bin",
                   "mprestamos_personales",
                   "mcuentas_saldo_bin",
                   "mactivos_margen_bin",
                   "mcaja_ahorro_bin",
                   "mcuenta_corriente"
)

# A todas las vamos a rankear

prefix <- "r_"
for (var in mis_variables) {
  dtrain[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dtest[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)]
  dataset_PRED[, (paste(prefix, var, sep = "")) := ntile(get(var), 10)] #se lo agrego porque canaritos lo necesita
}

## ---------------------------
## Step 9: Un + poco de R, seleccionar las variables para modelar 
## ---------------------------

# Las mejores más la variables rankeadas
mis_variables_2 <- c("active_quarter",
                     "mrentabilidad_annual_bin",
                     "mcaja_ahorro_dolares_bin",
                     "cprestamos_personales_bin",
                     "cprestamos_prendarios",
                     "cprestamos_hipotecarios",
                     "mplazo_fijo_pesos",
                     "minversion1_pesos_bin",
                     "cseguro_vida",
                     "cseguro_vivienda",
                     "ccaja_seguridad",
                     "mcuenta_debitos_automaticos_bin",
                     "mttarjeta_visa_debitos_automaticos_bin",
                     "mttarjeta_master_debitos_automaticos_bin",
                     "ccheques_emitidos",
                     "cliente_vip",
                     "mprestamos_personales",
                     "mprestamos_prendarios_bin",
                     "mprestamos_hipotecarios_bin",
                     "r_ctrx_quarter",
                     "r_cliente_edad",
                     "r_mprestamos_personales",
                     "r_mactivos_margen_bin",
                     "r_mcuenta_corriente",
                     "r_cliente_antiguedad_bin",
                     "r_mcuentas_saldo_bin",
                     "r_mcaja_ahorro_bin"
)


campos <- paste(mis_variables_2, collapse = " + ")
formula <- paste0( "clase_binaria ~ ", campos )

modelo5 <- rpart(formula,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo5$variable.importance)

## ---------------------------
## Step 10: Embeddings (Caseros)
## ---------------------------

# Hagamos interactuar algunas variables para ver si conseguimos alguna mejor
nuevas <- c()
for (var1 in mis_variables_2) {
  for (var2 in mis_variables_2) {
    if (var1 != var2) {
      nueva <- paste(var1, var2, sep = "___")
      dtrain[, (nueva) := get(var1) * get(var2)]
      dtest[, (nueva) := get(var1) * get(var2)]
      dataset_PRED[, (nueva) := get(var1) * get(var2)] #se lo agrego porque canaritos lo va a necesitar
      nuevas <- c(nuevas, nueva)
    }
  }
}

mis_variables_3 <- c(nuevas, mis_variables_2) 

campos2 <- paste(mis_variables_3, collapse = " + ")
formula2 <- paste0( "clase_binaria ~ ", campos2 )

modelo6 <- rpart(formula2,
                 data = dtrain,
                 xval = 0,
                 control = param)

print(modelo6$variable.importance)


##########AHORA LE HAGO CANARITOS!!!!!!

#agrego 30 canaritos
for( i in 1:33 ) dtrain[ , paste0("canarito", i ) :=  runif( nrow(dtrain)) ]

set.seed(semillas[1])
#Primero  veo como quedan mis arboles
modelo_original <- rpart(
  formula2,
  data= dtrain,
  model= TRUE,
  xval= 0,
  cp= -1,
  minsplit= 2, # dejo que crezca y corte todo lo que quiera
  minbucket= 1,
  maxdepth= 30 )

#hago el pruning de los canaritos
#haciendo un hackeo a la estructura  modelo_original$frame
# -666 es un valor arbritrariamente negativo que jamas es generado por rpart
modelo_original$frame[ modelo_original$frame$var %like% "canarito", "complexity"] <- -666
modelo_pruned  <- prune(  modelo_original, -666 )


set.seed(semillas[1])
prediccion  <- predict( modelo_pruned, dataset_PRED, type = "prob")
prob_baja2  <- prediccion[, "evento"]

Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )

dataset_PRED[ , Predicted:=ifelse( prob_baja2 > 0.025, 1, 0 ) ]

fwrite( dataset_PRED[ , list(numero_de_cliente, Predicted) ], #solo los campos para Kaggle
        file= "./exp/KA2022_09_11/entrega_220911_16.csv",
        sep=  "," )


################################################################################
################################################################################
################################################################################
################################################################################


#dataset[ foto_mes==202101 , ctrx_quarter_cuatro :=  4*ctrx_quarter ]
#names(modelo$variable.importance)

#modelo$frame$var


#Crear una columna
#dataset[ , campo12 := as.integer( active_quarter==1 & mcaja_ahorro >= 1325.9 & mtarjeta_visa_consumo < 2000.5 ) ]   

#Eliminar una columna
#dataset[,campo12:=NULL]

#Eliminar varias columnas
#df3[, c("foo","bar"):=NULL]  # remove two columns

