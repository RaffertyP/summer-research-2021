# 
# EV_data_matrix = data.frame(model.matrix(consumption ~ HDD + CDD + weather_region + model,EV_data)[,-1])
# eff_lm_model = lm(EV_data$consumption ~ ., data = EV_data_matrix, na.action=na.omit, weights = EV_data$distance)
# new_EV_data_matrix = EV_data_matrix[1,][-1,]
# new_EV_data_matrix[1,] = 0
# save(eff_lm_model, new_EV_data_matrix, file = "processed_data/matrix_lm_model.rda")


load(file = "processed_data/matrix_lm_model.rda")
#' Title
#'
#' @param HDD number of heating degree days per day
#' @param CDD number of cooling degree days per day
#' @param region can be used in vector form to select a proportion of regions in the order of reigon_pop region or a single number corresponding to index of a single region from region_pop region. 
#' @param car_model can be used in vector form to select a proportion of car models in the order of model_pop models or a single number corresponding to index of a single car from model_pop model. default uses model proportions from the EV data
#'
#' @return predicted average power usage of all EV's in a region or regions with upper and lower confidence intervals
#' @export
#'
#' @examples
car_power_consumption = function(HDD = 0, CDD = 0, region = 0, car_model = model_pop$freq[-1]) {



    if (length(region) == 1) {
        new_EV_data_matrix[1, colnames(new_EV_data_matrix)[grepl("weather_region", colnames(new_EV_data_matrix))][region]] = 1
    } 

    if (length(car_model) == 1) {
        new_EV_data_matrix[1, colnames(new_EV_data_matrix)[grepl("model", colnames(new_EV_data_matrix))][car_model]] = 1
    } else {
        new_EV_data_matrix[1, colnames(new_EV_data_matrix)[grepl("model", colnames(new_EV_data_matrix))]] = car_model
    }
    new_EV_data_matrix$HDD[1] = HDD
    new_EV_data_matrix$CDD[1] = CDD

    return(predict(eff_lm_model, new_EV_data_matrix, interval = "confidence"))
}

calc_region_usage = function(vkt, weather_region, HDD = 0, CDD = 0, car_model = model_pop$freq[-1]) {
    regionIndex = which(levels(EV_data$weather_region) == weather_region) - 1
    car_consumption = car_power_consumption(HDD, CDD, regionIndex, car_model)
    return(car_consumption * vkt)
}


comb_vkt_consumption = function(vkt_region, HDD = 0, CDD = 0, car_model = model_pop$freq[-1]) {
    vkt = vkt_pass[vkt_pass$year == 2019, vkt_region] / 12
    weather_region = vkt_regions[vkt_region]

    return(calc_region_usage(vkt, weather_region, HDD, CDD, car_model))
}

comb_vkt_consumption_weather = function(vkt_region, car_model = model_pop$freq[-1]) {
    weather_region = vkt_regions[vkt_region]
    power_monthly = data.frame()
    for (i in 1:12) {
        HDD = mean(HDD_data$HDD[HDD_data$Month == i & HDD_data$City == weather_region])
        CDD = mean(CDD_data$CDD[CDD_data$Month == i & CDD_data$City == weather_region])
        # cat(" ",i, " ", comb_vkt_consumption(vkt_region, HDD, CDD))
        power_monthly = rbind(power_monthly, comb_vkt_consumption(vkt_region, HDD, CDD, car_model))
    }
    #rownames(power_monthly) = month.abb[1:12]
    rownames(power_monthly) = 1:12
    # colnames(vkt_monthly) = paste(colnames(vkt_monthly), "(GWh)")
    colnames(power_monthly) = paste(colnames(power_monthly))

    return(power_monthly)
}



# for running with the times model
times_comb_vkt_consumption = function(est_tot_vkt, vkt_region, HDD = 0, CDD = 0, car_model = model_pop$freq[-1]) {
    #under assumtion that region km driven proportions do not change
    vkt_prop = vkt_pass[vkt_pass$year == 2019, vkt_region] / 12 / vkt_pass[vkt_pass$year == 2019,"total"]
    weather_region = vkt_regions[vkt_region]
    
    vkt = vkt_prop * est_tot_vkt
    return(calc_region_usage(vkt, weather_region, HDD, CDD, car_model))
}

times_comb_vkt_consumption_weather = function(est_tot_vkt, vkt_region, car_model = model_pop$freq[-1]) {
  weather_region = vkt_regions[vkt_region]
  power_monthly = data.frame()
  for (i in 1:12) {
    HDD = mean(HDD_data$HDD[HDD_data$Month == i & HDD_data$City == weather_region])
    CDD = mean(CDD_data$CDD[CDD_data$Month == i & CDD_data$City == weather_region])
    power_monthly = rbind(power_monthly, times_comb_vkt_consumption(est_tot_vkt, vkt_region, HDD, CDD, car_model))
  }
  rownames(power_monthly) = month.abb[1:12]
  colnames(power_monthly) = paste(colnames(power_monthly))
  
  return(power_monthly)
}


# repeat of first few function for getting average consumption probably nicer if combine but not needed
# commented out as not needed if only all of NZ average consumption needed. if regional average needed will needed

#calc_region_consumption = function(vkt_region, HDD = 0, CDD = 0, car_model = model_pop$freq[-1]) {
#  weather_region = vkt_regions[vkt_region]
#  regionIndex = which(levels(EV_data$weather_region) == weather_region) - 1
#  car_consumption = car_power_consumption(HDD, CDD, regionIndex, car_model)
#  return(car_consumption)
#}




