library(R.matlab)
library(dplyr)
setwd("~/Documents/lab/artículos/SOCO_SPECIALISSUE/experimentation")
library(tidyr)

"Se define H, que será siempre 1 día (20 registros)"
H = 20

dir.create("data/training", showWarnings = FALSE)
dir.create("data/test", showWarnings = FALSE)

"###########################################################################################################
Generación de los datasets correspondientes a los datos solares.
Se generan para W={40,60,80,100,120,140}, que corresponden a {1,2,3,4,5,6,7} días y H=20
Almacenados en los directorios ./data/training y .data/test con formato de nombre 2016_PV_W{W}_H{H}.csv
###########################################################################################################"
"Función para generar las matrices formateadas"
matrixGenerator = function(set, w, h=20, cnames){
  ncols = w+h
  matrix = matrix(set[1:ncols], ncol=ncols)
  colnames(matrix) = cnames
  i=1
  while(!any(is.na(set[(i+h):(ncols+i-1+h)]))){
    matrix = rbind(matrix, set[(i+h):(ncols+i-1+h)])
    i= i+h
  }
  return (matrix)
}

data = readMat("data/original/PV30min_cmb.mat")
d15 = as.vector(unlist(data$reqData[[3]]))
d16 = as.vector(unlist(data$reqData[[4]]))

for (W in seq(from = 20, to = 140, by = 20)){
  names = c(sprintf("PV_W%d",seq(1:W)), sprintf("PV_H%d",seq(1:H)))
  training = matrixGenerator(d15, W, cnames=names)
  test = matrixGenerator(d16, W, cnames = names)
  write.csv2(x = training, file = paste0("data/training/2015_PV_W",W,"_H",H,".csv"), col.names = T, row.names = F)
  write.csv2(x = test, file = paste0("data/test/2016_PV_W",W,"_H",H,".csv"), col.names = T, row.names = F)
}


"###########################################################################################################
Generación de los datasets correspondientes a los datos de tiempo.
Se generan para W={1,2,3,4,5,6,7}, que corresponden de 1 a 7 días
Almacenados en los directorios ./data/training y .data/test con formato de nombre 2016_Weather_W{W}.csv
###########################################################################################################"
lagWeatherGenerator = function(data, w, names, tipo, ruido=0){
  W=w-1
  set = data.frame()
  for (i in c(1:nrow(data))){
    v = matrix()
    for (j in c(0:W)){  v = cbind(v, data[(i+j),])   }
    v$v=NULL
    set = rbind(set, v)
  }
  set = as.data.frame(set[complete.cases(set), ])
  colnames(set) =  names
  
  if(tipo=="training"){
    write.csv(x = set, file = paste0("data/training/2015_Weather_W",w,".csv"), col.names = T, row.names = F)  
  }else if(tipo=="test"){
    write.csv(x = set, file = paste0("data/test/2016_Weather_W",w,".csv"), col.names = T, row.names = F)
  }else if(tipo=="forecastTraining"){
    write.csv(x = set, file = paste0("data/training/2015_ForWeather_W",w,"_Noise",ruido,".csv"), col.names = T, row.names = F)
  }else if(tipo=="forecastTest"){
    write.csv(x = set, file = paste0("data/test/2016_ForWeather_W",w,"_Noise",ruido,".csv"), col.names = T, row.names = F)
  }
}

weather = readMat("data/original/WeatherData.mat")
weather = weather$Weather
w15 = as.data.frame(weather[1:365,])
w16 = as.data.frame(weather[366:nrow(weather),])
rm(weather)
names(w15) = names(w16) = c("min_temp", "max_temp","rainfall","sun_hours","max_wind_speed", "temp_9", "Rel_hum_9","cloud_cover", 
                            "wind_speed_9", "temp_15","Rel_hum_3","cloud",  "wind_speed_3", "solar_irr")
a15 = w15
a16 = w16
names(a15) = names(a16) = c(sprintf("%s_d1",names(w15)))
W=1
write.csv(x = a15, file = paste0("data/training/2015_Weather_W",W,".csv"), col.names = T, row.names = F)
write.csv(x = a16, file = paste0("data/test/2016_Weather_W",W,".csv"), col.names = T, row.names = F)
rm(a15, a16)

for (w in seq(2:8)){
  names = vector()
  for (i in seq(1:w)){ names = append(names, paste(colnames(w15), i, sep = "_d")) }
  lagWeatherGenerator(w15, w, names, "training")
  lagWeatherGenerator(w16, w, names, "test")
}
  

"###########################################################################################################
Generación de los datasets correspondientes a los datos de tiempo pronosticados.
Se generan para W={1,2,3,4,5,6,7}, que corresponden de 1 a 7 días con niveles de ruido R={1,2,3}
Almacenados en los directorios ./data/training y .data/test con formato de nombre 2016_ForeWeather_W{W}_N{R}.csv
###########################################################################################################"
fweather =  readMat("data/original/WeatherForecastData.mat")

for (ruido in c(1:3)){
  d = fweather[["WF"]][[ruido]][[1]]
  d15 = as.data.frame(d[1:365,])
  d16 = as.data.frame(d[366:nrow(d),])
  
  names(d15) = names(d16) = c("for_min_temp", "for_max_temp","for_rainfall","for_solar_irr")
  a15 = d15
  a16 = d16
  names(a15) = names(a16) = c(sprintf("%s_d1",names(d15)))
  
  W=1
  write.csv(x = a15, file = paste0("data/training/2015_ForWeather_W",W,"_Noise",ruido,".csv"), col.names = T, row.names = F)
  write.csv(x = a16, file = paste0("data/test/2016_ForWeather_W",W,"_Noise",ruido,".csv"), col.names = T, row.names = F)
  
  for (w in seq(2:8)){
    names = vector()
    for (i in seq(1:w)){ names = append(names, paste(colnames(d15), i, sep = "_d")) }
    lagWeatherGenerator(d15, w, names, "forecastTraining", ruido=ruido)
    lagWeatherGenerator(d16, w, names, "forecastTest", ruido = ruido)
  }
}

  
#  for (i in c(1:nrow(w16))){
#    datasetTest = rbind(datasetTest, cbind(as.matrix(w16[i,]), as.matrix(w16[(i+1),])))
#  }
#  datasetTest = as.data.frame(datasetTest[complete.cases(datasetTest), ])