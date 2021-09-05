#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF")

#cargo los datos
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


dtrain$tx_total <-rowSums(cbind(dtrain$ctarjeta_master_transacciones,
                                dtrain$ctarjeta_visa_transacciones,
                                dtrain$ctarjeta_debito_transacciones,
                                dtrain$cforex,
                                dtrain$ccallcenter_transacciones,
                                dtrain$chomebanking_transacciones,
                                dtrain$ccajas_transacciones,
                                dtrain$ccajas_otras,
                                dtrain$catm_trx,
                                dtrain$catm_trx_other,
                                dtrain$cmobile_app_trx,
                                dtrain$ctrx_quarter))

dapply$tx_total <-rowSums(cbind(dapply$ctarjeta_master_transacciones,
                                dapply$ctarjeta_visa_transacciones,
                                dapply$ctarjeta_debito_transacciones,
                                dapply$cforex,
                                dapply$ccallcenter_transacciones,
                                dapply$chomebanking_transacciones,
                                dapply$ccajas_transacciones,
                                dapply$ccajas_otras,
                                dapply$catm_trx,
                                dapply$catm_trx_other,
                                dapply$cmobile_app_trx,
                                dapply$ctrx_quarter))


dtrain[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "evento", "noevento")]
# Sacamos la clase ternaria
dtrain[, clase_ternaria:= NULL]


dapply[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "evento", "noevento")]
# Sacamos la clase ternaria
dapply[, clase_ternaria:= NULL]

unique(dtrain$clase_binaria)
table(dtrain$foto_mes, dtrain$clase_binaria)


for( minbuck  in  c(10,11) )
{
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ . -ctarjeta_visa_descuentos -mtarjeta_visa_descuentos -ctarjeta_master_descuentos -mtarjeta_master_descuentos -cextraccion_autoservicio -ccajas_otras -catm_trx -matm_other -tmobile_app -cmobile_app_trx -Master_Finiciomora -Master_mconsumosdolares -Master_madelantodolares -Master_cconsumos -Master_cadelantosefectivo -Visa_Finiciomora -Visa_msaldodolares -Visa_mpagado -Visa_mpagominimo",
                   data= dtrain,
                   xval= 0,
                   cp= -0.5,
                   maxdepth= 8,
                   minbucket=minbuck)

  prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo

  #prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
  #cada columna es el vector de probabilidades 

  dapply[ , prob_baja2 := prediccion[, "evento"] ]
  dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

  entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida

  #genero el archivo para Kaggle
  fwrite( entrega, 
          file= paste0( "./kaggle/K110_h",  minbuck, ".csv"), 
          sep= "," )
}
