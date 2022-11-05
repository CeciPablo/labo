#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")
require("ranger")

setwd( "/dmef/" )
dataset  <- fread( "./datasets/competencia3_2022.csv.gz", stringsAsFactors= TRUE)

setwd( "/dmef/" )
dataCL <- fread( "./exp/CLU1262_local00/cluster_de_bajas.txt", stringsAsFactors= TRUE)


#me quedo SOLO con la foto_mes utilizada para generar los Clusters
dataset  <- dataset[foto_mes>=202006  & foto_mes<=202105, ] 

#me quedo SOLO con los registros de los Clientes perdidos
datasetF <- dataset[foto_mes>=202006  & foto_mes<=202105 & (dataset$numero_de_cliente %in% dataCL$numero_de_cliente),]

fwrite( datasetF, 
        file= "cluster_de_bajas_12meses_historia.txt",
        sep= "\t" )


boxplot(cpayroll_trx ~ cluster2, data = datasetF)

A <- dataset[,.(foto_mes,clase_ternaria)]

A <- A[, .N, by = .(foto_mes,clase_ternaria)]


B <- dataset[,.(foto_mes,mrentabilidad)]

B <- B[, sum(mrentabilidad), by = foto_mes]

C <- dataset[,.(foto_mes,mrentabilidad_annual)]

C <- C[, sum(mrentabilidad_annual), by = foto_mes]


fwrite( C, 
        file= "C.txt",
        sep= "\t" )

#rm(datasetF)
