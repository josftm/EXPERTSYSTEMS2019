"DOC: 
https://rtask.thinkr.fr/blog/installation-to-connect-spark-and-h2o-in-r/
https://github.com/h2oai/rsparkling/blob/master/inst/examples/using-sparkling-water-in-YARN.md
https://github.com/h2oai/sparkling-water/tree/master/r
https://aichamp.wordpress.com/2017/05/28/starter-script-for-rsparkling-h2o-on-spark-with-r/"
library(h2o)
library(rsparkling)
library(sparklyr)
library(dplyr)
library(Metrics)

setwd("~/Dropbox/SOCO_SPECIALISSUE/experimentation")
dir.create("Results/PV_Weather/models", recursive = T, showWarnings = FALSE)

"#########################################
#### Variables e instalación de Spark ####
#########################################"
H = 20
spark_install(version="2.0.2")
options(rsparkling.sparklingwater.version="2.0.2")

"########################################
#### DL hyperparameters #################
########################################"
neurons = list()
capa = 1
neuron=10
for (x in c(1:20)){
  neurons[[x]] = rep(neuron, times=capa)
  if(neuron==40){
    neuron=10
    capa = capa+1
  }else{ neuron = neuron+10 }
}

#neurons=list(c(29,29,29),c(9,9,9))
initial_weight_distribution = "Uniform"
rho=0.99
epsilon=1e-9
activation=c("Tanh")
distribution=c("gaussian")
stop.metric="mae"
hyper_params<-list(hidden=neurons, 
                   rho=rho, 
                   epsilon=epsilon, 
                   activation=activation,
                   distribution=distribution,
                   initial_weight_distribution=initial_weight_distribution
)

"########################################
#### Apply DL to each W dataset #########
########################################"
for (W in seq(from = 20, to = 140, by = 20)){
  print(paste0("## Inicio entrenamiento PV_W",W))
  
  PVTrain = read.csv(file = paste0("data/training/2015_PV_W",W,"_H",H,".csv"), header = TRUE, sep = ";", dec = ",")
  PVTest = read.csv(file = paste0("data/test/2016_PV_W",W,"_H",H,".csv"), header = TRUE, sep = ";", dec = ",")
  dt = rbind(PVTrain, PVTest)
  MAX = max(dt)
  MIN = min(dt)
  dt = (dt-MIN)/(MAX-MIN)
  PVTrain = dt[1:365,]
  PVTest = dt[366:nrow(dt),]
  
  WTrain = read.csv(file = paste0("data/training/2015_Weather_W",(W/20),".csv"), header = TRUE, sep = ",", dec = ".")
  WTrain = WTrain[1:nrow(PVTrain),]
  train = cbind(PVTrain, WTrain)
  train = train[,c(1:W,(W+H+1):ncol(train),(W+1):(W+H))]
  
  WTest = read.csv(file = paste0("data/test/2016_Weather_W",(W/20),".csv"), header = TRUE, sep = ",", dec = ".")
  WTest = WTest[1:nrow(PVTest),]
  test = cbind(PVTest, WTest)
  test = test[,c(1:W,(W+H+1):ncol(test),(W+1):(W+H))]
  
  
  predictors=c(1:(W+(14*W/20)))
  ## Normalizacion usando el min y max del training para todo
  split = floor(0.7*nrow(train))
  training = train[1:split,]
  validation = train[(split+1):nrow(train),]
  sc <- spark_connect(master="local[*]", version="2.0.2", config=list("sparklyr.shell.conf"="spark.executor.memory=16G",
                                                                      "sparklyr.shell.driver-memory"="16G"))
  h2o_context(spark_connection(sc))
  training = as.h2o(training)
  validation = as.h2o(validation)
  test = as.h2o(test)
  
  "########################################
  #### For each subproblem... #############
  ########################################"
  trainingTimeTotal = 0
  testTimeTotal=0
  for(h in 1:H){
    print(paste0("## Inicio entrenamiento Sub-",h))
    target<-(W+(14*W/20))+h
    tini = proc.time()[3]
    model_grid=h2o.grid("deeplearning",
                        hyper_params=hyper_params,
                        x=predictors,
                        y=target,
                        training_frame=training,
                        validation_frame = validation
    )
    trainingTime = proc.time()[3] - tini
    
    trainingTimeTotal = trainingTimeTotal+trainingTime
    model=h2o.getModel(model_grid@model_ids[[1]])
    # Extract model parameters and save it. Save the model too.
    h2o.saveModel(object=model, path=paste0("Results/PV_Weather/models/model_W",W,"_H",h), force=TRUE)
    sink(paste0("Results/PV_Weather/models/DetailsModel_W",W,"_H",h,".txt"))
    print(summary(model))
    print(paste0("TRAINING TIME: ",trainingTime, " seconds"))
    sink()
    print(paste0("\t## Fin entrenamiento Sub-",h))
    
    print(paste0("\t## Inicio prediccion Sub-",h))
    tini = proc.time()[3]
    prediction=h2o.predict(object=model,newdata=test[, 1:W]) #No estoy seguro si es [, 1:W] o si va sin nada, porque asi usaría la etiqueta
    testTime = proc.time()[3] - tini
    testTimeTotal = testTimeTotal+testTime
    
    realVsPred = as.data.frame(test[,h])
    realVsPred = cbind(realVsPred, as.data.frame(prediction))
    colnames(realVsPred) = c("Real", "Pred")
    realVsPred = (realVsPred+MIN)*(MAX-MIN) # Denormalizacion de los resultados
    write.csv(realVsPred, paste0("Results/PV_Weather/W",W,"_H",h,".csv"), col.names = TRUE, row.names = FALSE)
    print(paste0("\t## Fin prediccion Sub-",h))
  } #Termina el entrenamiento de todos los subproblemas
  
  sink(paste0("Results/PV_Weather/models/timesFor_W",W,"_H",h,".txt"))
  print(paste0("TRAINING TIME IN TOTAL: ",trainingTimeTotal))
  print(paste0("TEST TIME IN TOTAL: ",testTimeTotal))
  sink()
  
  print(paste0("## Fin entrenamiento PV_W",W))
}

