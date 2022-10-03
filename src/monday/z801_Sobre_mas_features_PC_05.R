##
## Sobre más features
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## <Insert a smart quote here about more is better>.
## --- Ale

## INTENTO CORRIDA EN CLOUD

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("lightgbm")
require("xgboost")
require("rlist")
require("DiceKriging")
require("mlrMBO")

options(error = function() { 
  traceback(20); 
  options(error = NULL); 
  stop("exiting after script error") 
})

# Poner la carpeta de la materia de SU computadora local
#setwd("/home/aleb/dmeyf2022")
setwd("~/buckets/b1/")   #Establezco el Working Directory
# Poner sus semillas
#semillas <- c(17, 19, 23, 29, 31)
semillas <- c(100057, 300007, 500009, 600011, 700001)

PARAM  <- list()
PARAM$experimento <- "Z801_cloud"

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/dataset_7110_06.csv.gz")

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(dataset$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- dataset$clase_ternaria
dataset$clase_ternaria <- NULL


## ---------------------------
## Step 2: XGBoost, un modelo simple ...
## ---------------------------
dtrain <- xgb.DMatrix(
  data = data.matrix(dataset),
  label = clase_binaria, missing = NA)

## ---------------------------
## Step 6: Jugando un poco más con los parámetros del XGBoost
## ---------------------------

set.seed(semillas[1])
param_fe2 <- list(
  colsample_bynode = 0.8,
  learning_rate = 1,
  max_depth = 7, # <--- IMPORTANTE CAMBIAR
  num_parallel_tree = 40, # <--- IMPORTANTE CAMBIAR
  subsample = 0.8,
  objective = "binary:logistic"
)

xgb_model2 <- xgb.train(params = param_fe2, data = dtrain, nrounds = 1)

# Veamos un paso a paso
new_features2 <- xgb.create.features(model = xgb_model2, data.matrix(dataset))

final_dataset <- as.data.table(as.matrix(new_features2))

final_dataset[,clase_ternaria := clase_real]

#grabo el dataset
#creo la carpeta donde va el experimento
dir.create( "./exp/",  showWarnings = FALSE ) 
dir.create( paste0( "./exp/", PARAM$experimento, "/"), showWarnings = FALSE )
setwd( paste0( "./exp/", PARAM$experimento, "/") )   #Establezco el Working Directory DEL EXPERIMENTO

fwrite( final_dataset,
        #"dataset_801_04.csv.gz", #30 árboles paralelos
        #"dataset_801_05.csv.gz", #40 árboles paralelos
        "dataset_801_06.csv.gz", #40 árboles paralelos - A partir del data Set de Gustavo con -1 en imputación
        logical01= TRUE,
        sep= "," )
