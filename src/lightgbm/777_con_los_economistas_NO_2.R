#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

#setwd("~/buckets/b1/crudoB" )  #establezco la carpeta donde voy a trabajar
setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")
#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")
#dataset  <- fread( "./datasets/paquete_premium_202009_ext2.csv" )

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]



#Quito el Data Drifting de  "ccajas_transacciones"

#campos_buenos  <- setdiff( colnames(dataset),
#                           c("clase_ternaria", "clase01","ccajas_transacciones" ) )

campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01","internet","ctarjeta_visa_descuentos" , "mtarjeta_visa_descuentos" ,"ccajas_transacciones", "ctarjeta_master_descuentos" , "mtarjeta_master_descuentos" , "cextraccion_autoservicio" , "ccajas_otras" , "catm_trx" , "matm_other" , "tmobile_app" , "cmobile_app_trx" , "Master_Finiciomora" , "Master_mconsumosdolares" , "Master_madelantodolares" , "Master_cconsumos" , "Master_cadelantosefectivo" , "Visa_Finiciomora" , "Visa_msaldodolares" , "Visa_mpagado" , "Visa_mpagominimo" ) )

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )


modelo  <- lightgbm( data= dtrain,
                      params= list( objective= "binary",
                                    max_bin= 5,   # https://www.youtube.com/watch?v=0mtctl8ba4g
                                    min_data_in_leaf= 100,
                                    num_leaves= 20,
                                    learning_rate= 0.013,
                                    num_iterations = 430,
                                    seed= 999983
                                   )  )


#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
#dapply  <- fread("./datasets/paquete_premium_202011_ext2.csv")

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))


#la probabilidad de corte ya no es 0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.038) ) ) #genero la salida


#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/con_los_economistas_NO.csv",
        sep=  "," )

#----------------------------------------------------------------------------------


#la probabilidad de corte ya no es 0.025


entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.038) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/con_los_economistas_NO.csv",
        sep=  "," )
