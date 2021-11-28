#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("randomForest")


#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "G:/Mi unidad/Maestria_Data_Science/DM_EyF/" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
)
#defino la carpeta donde trabajo
setwd( directory.root )


#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/dataset_bajas_0000.csv.gz", stringsAsFactors= TRUE)
gc()

d

##############################################################################

#-----------armo nueva col con meses antes de muerte---------------------------

#quito cols de palancas
dataset<-dataset[, .SD, .SDcols = ! names(dataset) %like% "avg|delta|tend|lag"]

setorder(  dataset,  numero_de_cliente, -foto_mes )   #ordeno, pero a la inversa

dataset[   , morire := 0 ]

dataset[ clase_ternaria=="BAJA+1" , morire := 1 ]  #si tengo un BAJA+1 , ese mes se que voy a morir

dataset[  , morire := cummax( morire ), numero_de_cliente ]   #calculo el maximo acumulado hace atras
dataset[  , meses_muerte := cumsum( morire ), numero_de_cliente ]   #calculo la suma acumulada

dataset[  meses_muerte==0,  meses_muerte := NA ]
dataset[  , morire := NULL ]


##############################################################################

#me guardo el dataset para hacer un  merge con los clusters
dt_final<-dataset

#achico el dataset: armo los clusters a partir de los clientes que están a punto de irse
#dataset[  ,  azar := runif( nrow(dataset) ) ]
dataset  <-  dataset[  meses_muerte == 1] #  & foto_mes>=202001  & foto_mes<=202011, ]
gc()


#paso los infinitos a NULOS
invisible(lapply(names(dataset),function(.name) set(dataset, which(is.infinite(dataset[[.name]])), j = .name,value =NA)))


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()

#campos buenos surge de las var_imp de todas las pruebas 
campos_buenos  <- c( "cr_consumo_tarjeta","mdescubierto_preacordado","mcaja_ahorro","ctrx_quarter",
                     "cr_constarj_cprod","ctarjeta_visa_transacciones","cpayroll_trx","Visa_status",
                     "cmobile_app_trx","mv_status01","mtarjeta_visa_consumo","ctarjeta_visa","mcuentas_saldo",
                     "cr_constarj_gastos","Visa_Finiciomora","mcuenta_corriente","cr_ah_pay","Visa_delinquency","mpayroll",
                     "mv_status06","mv_Finiciomora","cr_activos","mprestamos_personales","ctarjeta_debito_transacciones",
                     "Master_status","cprestamos_personales","cr_pasivos","cr_eg_total","cr_saldopayroll_desc_pay",
                     "Visa_mfinanciacion_limite","cr_mpayroll_descpay","cr_gastos","ccomisiones_mantenimiento","cr_desc_pay","Visa_mpagospesos",
                     "mautoservicio","mpasivos_margen","cr_ing_total","cr_totsaldo_payroll","Visa_cconsumos",
                     "cproductos","Visa_mpagominimo","Visa_msaldototal","ccaja_ahorro","cr_cprod_pasivos",
                     "cextraccion_autoservicio","tmobile_app","cr_constarj_ing","mtransferencias_recibidas",
                     "cr_trx_constarj","mactivos_margen","mv_status07","Visa_msaldopesos","Visa_mconsumospesos",
                     "mv_status04","mpagomiscuentas","Master_Finiciomora","mcomisiones_mantenimiento","cr_ing_edad",
                     "cr_cprod_desc_pay" )


#chequeo tipo de campo y si hay na's
str(dataset[ , campos_buenos, with=FALSE])
sum(is.na(dataset[ , campos_buenos, with=FALSE]))


#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 2000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico.pdf" ) ))
plot( hclust.rf )
dev.off()

----------------------------#esto no se q hace----------------------------------
h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter

#guardo el dataset con el clustering 
dataset_cluster <- dataset


#le asigno los clusters a todo el dataset completo
dt_final<-merge(dt_final, dataset_cluster[,.(numero_de_cliente,cluster2)], by='numero_de_cliente', all.x=TRUE, all.y=FALSE)

#verifico existencia de clusters
dt_final$cluster2

#guardo el dataset: SI HAY VERSIONES NUEVAS OJO!!!
fwrite( dt_final,
        paste0( "./datasets/clusters_v0000",".csv.gz" ),
        logical01 = TRUE,
        sep= "," )