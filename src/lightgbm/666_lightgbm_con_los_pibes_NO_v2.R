#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("/Users/clara/Documents/00-Posgrado/4_DM_Eco_y_Finanzas/")  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01", "ccajas_transacciones", "Master_mpagominimo" , "internet","tmobile_app", "Master_Finiciomora", "cmobile_app_trx") )

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )

#Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
#Dadme un punto de apoyo y movere el mundo, Arquimedes
modelo  <- lightgbm( data= dtrain,
                     params= list( objective= "binary",
                                   max_bin= 15, #15
                                   min_data_in_leaf= 4000,
                                   learning_rate= 0.06, #0.05
                                   num_iterations=100)  )


#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))


#la probabilidad de corte ya no es 0.025,  sino que 0.031
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.031) ) ) #genero la salida 0.031

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm_con_los_pibes_no.csv",
        sep=  "," )




#---------------------------------------------------------------------------------
#aplico el modelo a los datos nuevos, dapply
prediccion_test  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))


entrega_test  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                      "foto_mes"= dapply[,foto_mes],
                                 "E_666"= as.numeric(prediccion_test))) #genero la

#genero el archivo para Kaggle
fwrite( entrega_test, 
        file= "./kaggle/lightgbm_con_los_pibes_no_pro_test.csv",
        sep=  "," )


#---------------------------------------------------------------------------------
#aplico el modelo a los datos nuevos, dtrain
prediccion_train  <- predict( modelo,  data.matrix( dataset[  , campos_buenos, with=FALSE]))

entrega_train  <- as.data.table( list( "numero_de_cliente"= dataset[  , numero_de_cliente],
                                       "foto_mes"= dataset[,foto_mes],
                                      "E_666"= as.numeric(prediccion_train ) ) ) #genero la

#genero el archivo para Kaggle
fwrite( entrega_train, 
        file= "./kaggle/lightgbm_con_los_pibes_no_pro_train.csv.gz",
        sep=  "," )

entre_los_pibes_no <- rbind(entrega_train, entrega_test)

#genero el archivo para Kaggle
fwrite( entre_los_pibes_no , 
        file= "./kaggle/lightgbm_con_los_pibes_no_modelito.csv.gz",
        sep=  "," )

#-----------------------------------------------------------------------------------


