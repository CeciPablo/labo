##
## Sobre más features
##
## ---------------------------
## Step 1: Setup
## ---------------------------
##
## <Insert a smart quote here about more is better>.
## --- Ale

rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")
require("lightgbm")
<<<<<<< HEAD
#install.packages("xgboost")
=======
install.packages("xgboost")
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4
require("xgboost")

# Poner la carpeta de la materia de SU computadora local
#setwd("/home/aleb/dmeyf2022")
setwd("/dmef")
# Poner sus semillas
#semillas <- c(17, 19, 23, 29, 31)
semillas <- c(100057, 300007, 500009, 600011, 700001)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
<<<<<<< HEAD
#dataset <- fread("./datasets/dataset_7110_02.csv.gz")
dataset <- fread("./exp/FE7110/dataset_7110_02.csv.gz")

=======
dataset <- fread("./datasets/competencia2_2022.csv")
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4
marzo <- dataset[foto_mes == 202103]
mayo <- dataset[foto_mes == 202105]
rm(dataset)

# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo$clase_ternaria == "CONTINUA", 0, 1)
<<<<<<< HEAD
#clase_binaria <- ifelse(dataset$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo$clase_ternaria
#clase_real <- dataset$clase_ternaria
marzo$clase_ternaria <- NULL
#dataset$clase_ternaria <- NULL
=======
clase_real <- marzo$clase_ternaria
marzo$clase_ternaria <- NULL
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4
mayo$clase_ternaria <- NULL

## ---------------------------
## Step 2: XGBoost, un modelo simple ...
## ---------------------------



dtrain <- xgb.DMatrix(
<<<<<<< HEAD
        data = data.matrix(marzo), 
#         data = data.matrix(dataset), #le cambio "marzo" por el data set propio
          label = clase_binaria, missing = NA)
=======
        data = data.matrix(marzo),
        label = clase_binaria, missing = NA)
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4

# Empecemos con algo muy básico
param_fe <- list(
            max_depth = 2,
            eta = 0.1,
            objective = "binary:logistic")
nrounds <- 5

xgb_model <- xgb.train(params = param_fe, data = dtrain, nrounds = nrounds)

## ---------------------------
## Step 3: XGBoost, ... para generar nuevas variables
## ---------------------------

# https://research.facebook.com/publications/practical-lessons-from-predicting-clicks-on-ads-at-facebook/

<<<<<<< HEAD
#new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))
#colnames(new_features)[150:173]
=======
new_features <- xgb.create.features(model = xgb_model, data.matrix(marzo))
colnames(new_features)[150:173]
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4

## ---------------------------
## Step 4: Entendiendo como se construyen.
## ---------------------------

<<<<<<< HEAD
#install.packages("DiagrammeR")
#require("DiagrammeR")
#
#xgb.plot.tree(colnames(new_features), xgb_model, trees = 0)
=======
install.packages("DiagrammeR")
require("DiagrammeR")

xgb.plot.tree(colnames(new_features), xgb_model, trees = 0)
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4


## ---------------------------
## Step 5: Viendo cuán importantes son las nuevas variables, pero con un LGBM!!!
## ---------------------------

<<<<<<< HEAD
#dtrain_lgb  <- lgb.Dataset(
#            data = data.matrix(new_features),
#            label = clase_binaria)
#
#mlgb <- lgb.train(
#            dtrain_lgb,
#            params = list(
#                objective = "binary",
#                max_bin = 15,
#                min_data_in_leaf = 4000,
#                learning_rate = 0.05),
#            verbose = -1)
#
#lgb.importance(mlgb)
=======
dtrain_lgb  <- lgb.Dataset(
            data = data.matrix(new_features),
            label = clase_binaria)

mlgb <- lgb.train(
            dtrain_lgb,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb)
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4

## ---------------------------
## Step 6: Jugando un poco más con los parámetros del XGBoost
## ---------------------------

set.seed(semillas[1])
param_fe2 <- list(
                colsample_bynode = 0.8,
                learning_rate = 1,
<<<<<<< HEAD
                max_depth = 7, # <--- IMPORTANTE CAMBIAR
                num_parallel_tree = 15, # <--- IMPORTANTE CAMBIAR
=======
                max_depth = 3, # <--- IMPORTANTE CAMBIAR
                num_parallel_tree = 10, # <--- IMPORTANTE CAMBIAR
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4
                subsample = 0.8,
                objective = "binary:logistic"
            )

xgb_model2 <- xgb.train(params = param_fe2, data = dtrain, nrounds = 1)

# Veamos un paso a paso
new_features2 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

colnames(new_features2)[150:230]

dtrain_lgb2  <- lgb.Dataset(
            data = data.matrix(new_features2),
            label = clase_binaria)

mlgb2 <- lgb.train(
            dtrain_lgb2,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05),
            verbose = -1)

lgb.importance(mlgb2)$Feature

# Filtrando las features que entraron
## Preguntas
## - ¿Entraron todas las variables?

<<<<<<< HEAD
#PC:
  #https://www.rdocumentation.org/packages/xgboost/versions/1.6.0.1/topics/xgb.create.features

  #https://slowkow.com/notes/sparse-matrix/

  #install.packages("Matrix")
  #library(Matrix)

### ---------------------------
### Step 7: Sumando canaritos
### ---------------------------
#
#set.seed(semillas[1])
#for (i in 1:20)  {
#    marzo[, paste0("canarito", i) := runif(nrow(marzo))]
#}
#
#new_features3 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))
#
## Veamos que están las variables que generamos
#colnames(new_features3)[150:230]
#
#dtrain_lgb3  <- lgb.Dataset(
#            data = data.matrix(new_features3),
#            label = clase_binaria)
#
#mlgb3 <- lgb.train(
#            dtrain_lgb3,
#            params = list(
#                objective = "binary",
#                max_bin = 15,
#                min_data_in_leaf = 4000,
#                learning_rate = 0.05,
#                num_iterations = 500 ## <-- aumento las iteraciones
#            ),
#            verbose = -1)
#
#var_importance <- lgb.importance(mlgb3)$Feature
#
## Veamos cuantas canaritos aparecieron
#list_canaritos <- grepl("canarito", var_importance)
#
## Cuantos canaritos aparecieron?
#length(var_importance[list_canaritos])
#
## En que posiciones
#idx <- seq(length(list_canaritos))
#idx[list_canaritos]
#
## En que posiciones aprecieron el resto de las variables generadas
#list_new_features <- grepl("V\\d+", var_importance)
#idx[list_new_features]
=======
## ---------------------------
## Step 7: Sumando canaritos
## ---------------------------

set.seed(semillas[1])
for (i in 1:20)  {
    marzo[, paste0("canarito", i) := runif(nrow(marzo))]
}

new_features3 <- xgb.create.features(model = xgb_model2, data.matrix(marzo))

# Veamos que están las variables que generamos
colnames(new_features3)[150:230]

dtrain_lgb3  <- lgb.Dataset(
            data = data.matrix(new_features3),
            label = clase_binaria)

mlgb3 <- lgb.train(
            dtrain_lgb3,
            params = list(
                objective = "binary",
                max_bin = 15,
                min_data_in_leaf = 4000,
                learning_rate = 0.05,
                num_iterations = 500 ## <-- aumento las iteraciones
            ),
            verbose = -1)

var_importance <- lgb.importance(mlgb3)$Feature

# Veamos cuantas canaritos aparecieron
list_canaritos <- grepl("canarito", var_importance)

# Cuantos canaritos aparecieron?
length(var_importance[list_canaritos])

# En que posiciones
idx <- seq(length(list_canaritos))
idx[list_canaritos]

# En que posiciones aprecieron el resto de las variables generadas
list_new_features <- grepl("V\\d+", var_importance)
idx[list_new_features]
>>>>>>> ea742b1c1cad4a09f224d4e78897f11453484db4
