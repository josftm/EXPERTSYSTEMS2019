library(Metrics)
library(openxlsx)
setwd("~/Dropbox/SOCO_SPECIALISSUE/experimentation")

Results = data.frame()

## RESULTADOS DE PV y PV_WEATHER
for (W in seq(from = 20, to = 140, by = 20)){
  totalMAE_PV = totalRMSE_PV = 0
  totalMAE_PV_Weather = totalRMSE_PV_Weather = 0
  totalMAE_PV_Forecast = totalRMSE_PV_Forecast =0
  totalMAE_PV_Weather_Forecast = totalRMSE_PV_Weather_Forecast =  0
  
  for (h in c(1:20)){
    PV = read.csv(paste0("Results/PV/W",W,"_H",h,".csv"), header = TRUE)
    PV_Weather = read.csv(paste0("Results/PV_Weather/W",W,"_H",h,".csv"), header = TRUE)
    totalMAE_PV=totalMAE_PV+mae(PV$Real, PV$Pred)
    totalRMSE_PV=totalRMSE_PV+rmse(PV$Real, PV$Pred)
    
    totalMAE_PV_Weather=totalMAE_PV_Weather+mae(PV_Weather$Real, PV_Weather$Pred)
    totalRMSE_PV_Weather=totalRMSE_PV_Weather+rmse(PV_Weather$Real, PV_Weather$Pred)
  }
  totalMAE_PV=totalMAE_PV/20
  totalRMSE_PV=totalRMSE_PV/20
  totalMAE_PV_Weather=totalMAE_PV_Weather/20
  totalRMSE_PV_Weather=totalRMSE_PV_Weather/20
  Results = rbind(Results, c(W, totalMAE_PV, totalRMSE_PV, totalMAE_PV_Weather, totalRMSE_PV_Weather))
}
colnames(Results) = c("W", "MAE_PV", "RMSE_PV", "MAE_PV_Weather", "RMSE_PV_Weather" )


## RESULTADOS DE PV_FORECAST
forecast = data.frame
for(ruido in c(1:3)){
  print(paste0("RUIDO ",ruido))
  aux = data.frame()
  for (W in seq(from = 20, to = 140, by = 20)){
    print(paste0("W ",W))
    totalMAE_PV_Forecast = totalRMSE_PV_Forecast = 0
    for (h in c(1:20)){
      PV_Forecast = read.csv(paste0("Results/PV_Forecast/W",W,"_H",h,"_Ruido",ruido,".csv"), header = TRUE)
      totalMAE_PV_Forecast=totalMAE_PV_Forecast+mae(PV_Forecast$Real, PV_Forecast$Pred)
      totalRMSE_PV_Forecast=totalRMSE_PV_Forecast+rmse(PV_Forecast$Real, PV_Forecast$Pred)
    }
    totalMAE_PV_Forecast = totalMAE_PV_Forecast/20
    totalRMSE_PV_Forecast = totalRMSE_PV_Forecast/20
    print(totalMAE_PV_Forecast)
    aux = rbind(aux, c(totalMAE_PV_Forecast, totalRMSE_PV_Forecast))
    names(aux) = c(paste0("MAE_PV_Forecast_Ruido",ruido),paste0("RMSE_PV_Forecast_Ruido",ruido))
  }
  Results = cbind(Results, aux)
}



## RESULTADOS DE PV_WEATHER_FORECAST
forecast = data.frame
for(ruido in c(1:3)){
  aux = data.frame()
  for (W in seq(from = 20, to = 140, by = 20)){
    totalMAE_PV_Forecast = totalRMSE_PV_Forecast = 0
    for (h in c(1:20)){
      PV_Forecast = read.csv(paste0("Results/PV_ForecastWeather/W",W,"_H",h,"_Ruido",ruido,".csv"), header = TRUE)
      totalMAE_PV_Forecast=totalMAE_PV_Forecast+mae(PV_Forecast$Real, PV_Forecast$Pred)
      totalRMSE_PV_Forecast=totalRMSE_PV_Forecast+rmse(PV_Forecast$Real, PV_Forecast$Pred)
    }
    totalMAE_PV_Forecast = totalMAE_PV_Forecast/20
    totalRMSE_PV_Forecast = totalRMSE_PV_Forecast/20
    aux = rbind(aux, c(totalMAE_PV_Forecast, totalRMSE_PV_Forecast))
    names(aux) = c(paste0("MAE_PV_Forecast_Weather_Ruido",ruido),paste0("RMSE_PV_Forecast_Weather_Ruido",ruido))
  }
  Results = cbind(Results, aux)
}

write.xlsx(Results, "Results/Results.xlsx")
