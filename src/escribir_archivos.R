#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

#setwd("~/buckets/b1/crudoB" )  #establezco la carpeta donde voy a trabajar
setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")
#cargo el dataset
dataset_777  <- fread("./kaggle/lightgbm_con_los_economistas_no_modelito.csv.gz")
#dataset  <- fread( "./datasets/paquete_premium_202009_ext2.csv" )

dataset_666  <- fread("./kaggle/lightgbm_con_los_pibes_no_modelito.csv.gz")

E_777 <- as.data.table( list( "numero_de_cliente"= dataset_777[  , numero_de_cliente],
                              "foto_mes"= dataset_777[,foto_mes],
                              "E_777"= dataset_777[,prediccion] ) )  

stacking_v1 <- fread("./datasets/dataset_stacking_v001.csv.gz")
stacking_v2 <- fread("./datasets/dataset_stacking_v002.csv.gz")

names(stacking_v1)
names(stacking_v2)
fwrite( E_777, 
        file= "./datasets/lightgbm_con_los_economistas_no_pro_train.csv.gz",
        sep=  "," )
  
  
  dataset_777[,c(numero_de_cliente,foto_mes,prediccion)]

"numero_de_cliente"= dataset[  , numero_de_cliente],
"foto_mes"= dataset[,foto_mes],
"E_666"= as.numeric(prediccion_train ) ) )