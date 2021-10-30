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
dataset  <- fread("./datasets/paquete_premium_202009_ext2.csv")

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

# campos_malos  <- c("internet","ctarjeta_visa_descuentos" , "mtarjeta_visa_descuentos" ,"ccajas_transacciones", "ctarjeta_master_descuentos" , "mtarjeta_master_descuentos" , "cextraccion_autoservicio" , "ccajas_otras" , "catm_trx" , "matm_other" , "tmobile_app" , "cmobile_app_trx" , "Master_Finiciomora" , "Master_mconsumosdolares" , "Master_madelantodolares" , "Master_cconsumos" , "Master_cadelantosefectivo" , "Visa_Finiciomora" , "Visa_msaldodolares" , "Visa_mpagado" , "Visa_mpagominimo")


#Quito el Data Drifting de  "ccajas_transacciones"
campos_buenos  <- setdiff( colnames(dataset),
                           c("numero_cliente", "clase_ternaria","clase01", "internet","ctarjeta_visa_descuentos" , "mtarjeta_visa_descuentos" ,"ccajas_transacciones", "ctarjeta_master_descuentos" , "mtarjeta_master_descuentos" , "cextraccion_autoservicio" , "ccajas_otras" , "catm_trx" , "matm_other" , "tmobile_app" , "cmobile_app_trx" , "Master_Finiciomora" , "Master_mconsumosdolares" , "Master_madelantodolares" , "Master_cconsumos" , "Master_cadelantosefectivo" , "Visa_Finiciomora" , "Visa_msaldodolares" , "Visa_mpagado" , "Visa_mpagominimo") )

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )


params <- list( objective= "binary",
               metric= "custom",
               first_metric_only= TRUE,
               boost_from_average= TRUE,
               feature_pre_filter= FALSE,
               verbosity= -100,
               seed= 999983,
               max_depth= 20, # -1 significa no limitar, por ahora lo dejo fijo
               min_gain_to_split= 0.48, #por ahora, lo dejo fijo
               lambda_l1= 0.016934, #por ahora, lo dejo fijo
               lambda_l2= 0.001083, #por ahora, lo dejo fijo
               max_bin= 31, #por ahora, lo dejo fijo
               num_iterations= 86, #un numero muy grande, lo limita early_stopping_rounds
               force_row_wise= TRUE, #para que los alumnos no se atemoricen con tantos warning
               learning_rate= 0.069934,
               feature_fraction= 0.32801,
               min_data_in_leaf=3374,
               num_leaves=395,
               prob_corte=0.044891
)

modelo  <- lightgbm( data= dtrain,
                      params= params )


#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasets/paquete_premium_202011_ext2.csv")

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE])) ##predict sobre noviembre

dataset_w_pred <- cbind(dapply,prediccion)

entrega_nov_best  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                     "foto_mes" = dapply[  , foto_mes],
                                     "Predicted"= as.numeric(prediccion) ) ) #genero la salida

#-Entrega Kaggle--------------------------------------------------------------------------


#la probabilidad de corte ya no es 0.025
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.038) ) ) #genero la salida


#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/682_Best.csv",
        sep=  "," )


#-Entrega Septiembre--------------------------------------------------------------------------


#rm( list=ls() )
#gc()

#library("data.table")
#library("lightgbm")


# Poner el dataset con los campos creados por FE y sin los malos.
ds <- dataset

# Creación de clase target unificando CONTINUA con BAJA+1
# BAJA+1 y BAJA+2 son indistinguibles.
clase_binaria <- dataset$clase01
ds$clase_ternaria <- NULL # Eliminación de la clase ternaria.

######## STACKING #########

params_lgb= params

# Definición de 5 folds (lista de indices de elementos estratificados según clase binaria)
folds <- splitTools::create_folds(clase_binaria, k = 2, seed = 700001)

### Para un solo modelo creamos una variables con datos "insesgados" solo con validación

validation <- numeric(length(clase_binaria))
for (f in folds) {
        ds_train  <- lgb.Dataset( data=  data.matrix(ds[f]), label= clase_binaria[f] )
        m <- lgb.train(ds_train, params = params_lgb, verbose = -1)
        validation[-f] <- predict(m,data.matrix(ds[-f]))
}

entrega_sep_best <- cbind(dataset$numero_de_cliente,dataset$foto_mes, validation) # CON TODAS LAS VARS !!!!!!!!!!!!!!!!!!!!!!! E
colnames(entrega_sep_best)<-c("numero_de_cliente","foto_mes","Predicted")

entrega_sep_best <- as.data.table(entrega_sep_best)


#-Entrega Modelito--------------------------------------------------------------------------

#la probabilidad de corte ya no es 0.025

modelito_682_best <- rbind(entrega_sep_best,entrega_nov_best)

fwrite( modelito_682_best,
        file="./modelitos/E682_best_modelitos.csv.gz")




