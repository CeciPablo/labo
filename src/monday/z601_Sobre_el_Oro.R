##
## Sobre el Oro
##
## ---------------------------
## Step 1: Armando un modelo para usar.
## ---------------------------
##
## All that gliters is not gold
## --- William Shakespeare

# Limpiamos el entorno
rm(list = ls())
gc(verbose = FALSE)

# Librerías necesarias
require("data.table")
require("rpart")
require("ggplot2")

# Poner la carpeta de la materia de SU computadora local
setwd("/dmef")
# Poner sus semillas
semillas <- c(17, 19, 23, 29, 31)

# Cargamos los datasets y nos quedamos solo con 202101 y 202103
dataset <- fread("./datasets/competencia2_2022.csv.gz")
dataset <- fread("./datasets/competencia2_2022.csv")
enero <- dataset[foto_mes == 202101]
marzo <- dataset[foto_mes == 202103]

# Borramos el dataset para liberar memoria.
rm(dataset)

# Armamos diferentes clases binarias:
# Sólo es evento las clase BAJA+2
enero[, clase_binaria1 := ifelse(
                            clase_ternaria == "BAJA+2",
                                "evento",
                                "noevento"
                            )]

# Entrenamos en Enero para ver como funciona nuestro modelo en Marzo.
parametros <- list(cp = -1, minsplit = 1073, minbucket = 278, maxdepth = 9)
modelo <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros)

## ---------------------------
## Step 2: Aplicando ese modelo a los datos de Marzo
## ---------------------------

# Predigo la probabilidad de marzo.
marzo$pred <- predict(modelo, marzo, type = "prob")[, "evento"]

# Marzo entero
marzo[, sum(ifelse(pred > 0.025,
                ifelse(clase_ternaria == "BAJA+2", 78000, -2000)
            , 0))]

#Como no hay nada random, no importa las semillas, a todos nos va a dar igual.
# rpart no utiliza nunca algo random para separar las decisiones

## ---------------------------
## Step 3: Creando 100 leaderboards ==> Tratar de simular muchos Kaggles para ver cómo me va
## ---------------------------

semillas <- c(100057, 300007, 500009, 600011, 700001)

leaderboad <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                     p = 0.50, list = FALSE) #0.7
  privado <- sum((marzo$pred[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.5#0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.5#0.3
  leaderboad <- rbindlist(list(leaderboad,
                data.table(privado = privado, publico = publico)))
}

leaderboad$r_privado <- frank(leaderboad$privado)
leaderboad$r_publico <- frank(leaderboad$publico)

leaderboad

# Guardar la salida para comparar más adelante
summary(leaderboad)

#corremos y guardamos summary

# privado            publico           r_privado        r_publico     
# Min.   :15885714   Min.   :13540000   Min.   :  1.00   Min.   :  1.00  
# 1st Qu.:17776429   1st Qu.:17081667   1st Qu.: 25.75   1st Qu.: 25.75  
# Median :18400000   Median :18766667   Median : 50.50   Median : 50.50  
# Mean   :18447657   Mean   :18655467   Mean   : 50.50   Mean   : 50.50  
# 3rd Qu.:19122143   3rd Qu.:20221667   3rd Qu.: 75.25   3rd Qu.: 75.25  
# Max.   :20640000   Max.   :24633333   Max.   :100.00   Max.   :100.00  

# Y luego probamos cambiando la proporción a 50% y 50% y corregir el cociente de ganancia

# con 50% y 50% me da para comparar con el de arriba:
# privado            publico           r_privado        r_publico     
# Min.   :15012000   Min.   :15300000   Min.   :  1.00   Min.   :  1.00  
# 1st Qu.:17414000   1st Qu.:17823000   1st Qu.: 25.75   1st Qu.: 25.75  
# Median :18138000   Median :18882000   Median : 50.50   Median : 50.50  
# Mean   :18268200   Mean   :18751800   Mean   : 50.50   Mean   : 50.50  
# 3rd Qu.:19197000   3rd Qu.:19606000   3rd Qu.: 75.25   3rd Qu.: 75.25  
# Max.   :21720000   Max.   :22008000   Max.   :100.00   Max.   :100.00  

# moraleja no importa la proporción público, privado

# Ahora volvemos a los valores 70% 30%

semillas <- c(100057, 300007, 500009, 600011, 700001)

leaderboad <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                                      p = 0.70, list = FALSE) 
  privado <- sum((marzo$pred[split] > 0.025) *
                   ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
                   ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3
  leaderboad <- rbindlist(list(leaderboad,
                               data.table(privado = privado, publico = publico)))
}

leaderboad$r_privado <- frank(leaderboad$privado)
leaderboad$r_publico <- frank(leaderboad$publico)

leaderboad


## Preguntas
## ¿Qué conclusiones saca al ver los valores?
## - Respecto al valor real
## - Respecto a la relación entre el **público** y el **privado**

## ---------------------------
## Step 4: Graficando leaderboads
## ---------------------------

df <- melt(leaderboad, measure.vars =  c("privado", "publico"))
ggplot(df, aes(x = value, color = variable)) + geom_density()

## Observaciones?

#Así te da con 70-30 pero si lo corres en 50-50 las curvas te dan muy parecidas.

## ---------------------------
## Step 5: Compitiendo entre dos modelos
## ---------------------------

# Sumamos un modelo básico

parametros2 <- list(cp = -1, minsplit = 2, minbucket = 1, maxdepth = 5)
modelo2 <- rpart(clase_binaria1 ~ . - clase_ternaria,
                data = enero,
                xval = 0,
                control = parametros2)

marzo$pred2 <- predict(modelo2, marzo, type = "prob")[, "evento"]

# Marzo entero
marzo[, sum(ifelse(pred2 >= 0.025,
                ifelse(clase_ternaria == "BAJA+2", 78000, -2000)
            , 0))]

#los árboles más sencillos son más robustos a los cambios. Sufren menos en el tiempo

## Preguntas
## Abriendo la caja de pandora, ¿Cúal de los dos modelos era mejor?

## ---------------------------
## Step 6: Compitiendo entre dos modelos, ahora en los leaderboards
## ---------------------------

leaderboad2 <- data.table()
set.seed(semillas[1])
for (i in 1:100) {
  split <- caret::createDataPartition(marzo$clase_ternaria,
                     p = 0.70, list = FALSE)

  privado <- sum((marzo$pred[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico <- sum((marzo$pred[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3

  privado2 <- sum((marzo$pred2[split] > 0.025) *
        ifelse(marzo$clase_ternaria[split] == "BAJA+2", 78000, -2000)) / 0.7
  publico2 <- sum((marzo$pred2[-split] > 0.025) *
        ifelse(marzo$clase_ternaria[-split] == "BAJA+2", 78000, -2000)) / 0.3

  leaderboad2 <- rbindlist(list(leaderboad2,
                data.table(privado = privado,
                           publico = publico,
                           privado2 = privado2,
                           publico2 = publico2)))
}

leaderboad2


## Preguntas
## Viendo la tabla anterior, ¿En cuántos leaderboard hubiera elegido el modelo
## correcto usando el público?

## ---------------------------
## Step 7: Compitiendo entre dos modelos, las curvas!
## ---------------------------

df2 <- melt(leaderboad2, measure.vars =  c("publico", "publico2"))
ggplot(df2, aes(x = value, color = variable)) + geom_density()

df3 <- melt(leaderboad2, measure.vars =  c("privado", "privado2"))
ggplot(df3, aes(x = value, color = variable)) + geom_density()

## Active learning ... entender que pasa.