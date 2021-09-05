#Arbol elemental con libreria  rpart

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Diego/diegodelucag_gmail/Maestria_Data_Science/DM_EyF")

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
ds  <- fread("./datasetsOri/paquete_premium_202009.csv")

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = ds[,3:158],
                 xval=0,
                 cp=        0, 
                 minsplit=  20,
                 minbucket=  3,
                 maxdepth=   8 )

var_imp <-data.table(names(modelo$variable.importance),round(modelo$variable.importance,4))
fwrite( var_imp, file="./work/K2021_09_02_4_var_imp.csv", sep="," )

#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply [,3:158] , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K2021_09_02_4.csv", sep="," )


#########################################################################################################
#Voy a probar aumentando la probabilidad de la tasa de corte a 0.030 


#genero el modelo
modelo  <- rpart("clase_binaria ~ .",
                 data = ds[,3:158],
                 xval=0,
                 cp=        0, 
                 minsplit=  20,
                 minbucket=  3,
                 maxdepth=   8 )

var_imp <-data.table(names(modelo$variable.importance),round(modelo$variable.importance,4))
fwrite( var_imp, file="./work/K2021_09_04_1_var_imp.csv", sep="," )

#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply [,3:158] , type = "prob") #aplico el modelo

#prediccion es una matriz con DOS columnas, llamadas "evento"  y "noevento"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "evento"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.030) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K2021_09_04_1.csv", sep="," )

#########################################################################################################

#creamos la variable tx_total tanto en sep como en nov
#eliminamos las features que presentan "Data Drift" del modelado del árbol

#binarizamos la variable target agrupando los clientes baja+1 y baja+2
ds[, clase_binaria := ifelse(clase_ternaria == "BAJA+2", "evento", "noevento")]
# Sacamos la clase ternaria
ds[, clase_ternaria:= NULL]


ds$tx_total <-rowSums(cbind(ds$ctarjeta_master_transacciones,
                            ds$ctarjeta_visa_transacciones,
                            ds$ctarjeta_debito_transacciones,
                            ds$cforex,
                            ds$ccallcenter_transacciones,
                            ds$chomebanking_transacciones,
                            ds$ccajas_transacciones,
                            ds$ccajas_otras,
                            ds$catm_trx,
                            ds$catm_trx_other,
                            ds$cmobile_app_trx,
                            ds$ctrx_quarter))



parametros  <-  list( "cp"=-1, "minsplit"=750,  "minbucket"=375, "maxdepth"=8)


#genero el modelo
modelo1  <- rpart("clase_binaria ~ .-numero_de_cliente-foto_mes-internet-mtarjeta_visa_descuentos-ctarjeta_master_descuentos-matm_other-tmobile_app-cmobile_app_trx-Master_Finiciomora",
                 data= ds,
                 xval= 0,
                 control= parametros)

var_imp <-data.table(names(modelo1$variable.importance),round(modelo1$variable.importance,4))
fwrite( var_imp, file="./work/K2021_09_04_2_var_imp.csv", sep="," )


#aplico al modelo  a los datos de 202011
#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")


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

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con DOS columnas, llamadas "evento"  y "noevento"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "evento"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K2021_09_04_1.csv", sep="," )

#########################################################################################################

#vamos a correr optimización bayesiana para los hps 

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "320_rpart_BO"
karch_generacion  <- ds#"./datasetsOri/paquete_premium_202009.csv"
karch_aplicacion  <- dapply#"./datasetsOri/paquete_premium_202011.csv"
kBO_iter    <-  10000   #cantidad de iteraciones de la Optimizacion Bayesiana

hs  <- makeParamSet(
  makeNumericParam("cp"       , lower= -1   , upper=    0.1),
  makeIntegerParam("minsplit" , lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
  makeIntegerParam("minbucket", lower=  1L  , upper= 2000L),
  makeIntegerParam("maxdepth" , lower=  3L  , upper=   20L),
  forbidden = quote( minbucket > 0.5*minsplit ) ) #que no pruebe casos sin sentido: un hijo no puede ser mas grande que el padre


ksemilla_azar  <- 589481 #102191
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#funcion para particionar, es la que Andres reemplaza con caret

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_binaria ~ .-numero_de_cliente-foto_mes-internet-mtarjeta_visa_descuentos-ctarjeta_master_descuentos-matm_other-tmobile_app-cmobile_app_trx-Master_Finiciomora", 
                   data= data[ fold != fold_test, ],
                   xval= 0,
                   control= param )
  
  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")
  
  prob_baja2  <- prediccion[, "evento"]
  
  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="evento", 48750, -1250 ) ] )
  
  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )
  
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS
  
  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia  <- function( x )
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  xval_folds  <- 5
  ganancia  <-  ArbolesCrossValidation( dataset, param=x, qfolds= xval_folds, pagrupa="clase_binaria", semilla=ksemilla_azar )
  
  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if(  ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max <<-  ganancia  #asigno la nueva maxima ganancia
    
    modelo  <- rpart("clase_binaria ~ .-numero_de_cliente-foto_mes-internet-mtarjeta_visa_descuentos-ctarjeta_master_descuentos-matm_other-tmobile_app-cmobile_app_trx-Master_Finiciomora",
                     data= dataset,
                     xval= 0,
                     control= x )
    
    #genero el vector con la prediccion, la probabilidad de ser positivo
    prediccion  <- predict( modelo, dapply)
    
    prob_baja2  <- prediccion[, "evento"]
    Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )
    
    entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )
    
    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
            sep=  "," )
  }
  
  #logueo 
  xx  <- x
  xx$xval_folds  <-  xval_folds
  xx$ganancia  <- ganancia
  loguear( xx,  arch= klog )
  
  
  return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_rpart.RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_rpart_log.txt" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_rpart_kaggle_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

if( file.exists(klog) )
{
  tabla_log  <- fread( klog)
  GLOBAL_iteracion  <- nrow( tabla_log ) -1
  GLOBAL_ganancia_max  <-  tabla_log[ , max(ganancia) ]
}


#cargo los datasets
dataset  <- karch_generacion   #donde entreno
dapply  <- karch_aplicacion    #donde aplico el modelo

#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar,
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,
  has.simple.signature = FALSE
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista


#quit( save="no" )



