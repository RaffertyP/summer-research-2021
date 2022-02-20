library(dplyr)


vkt_quart = read.csv("downloaded_stats/VKT_main_quart.csv")
vkt_yearly = read.csv("downloaded_stats/VKT_main_yearly.csv")
vkt_other_yearly = read.csv("downloaded_stats/VKT_other_yearly.csv")
vkt_type_yearly = read.csv("downloaded_stats/VKT_fuel_vehicle_type_yearly.csv")

#combining the main regions of VKT with the other regions
vkt_yearly = cbind(vkt_yearly, vkt_other_yearly[,-1])
vkt_yearly = subset(vkt_yearly, select = -c(Other.Regions))
names(vkt_yearly)[1] = "year"

#creates VKT of passenger vehicles
#under the assumption that proportion of large vehicles and passenger vehicles is same in all regions
vkt_pass = cbind(year = vkt_yearly$year,vkt_yearly[,-1]*(vkt_type_yearly$Light.passenger.vehicles/vkt_type_yearly$total))

#list index (first) is the VKT region to use and value (second) is the weather data region to use
vkt_regions = list("Northland" = "Auckland", "Auckland" = "Auckland", "Gisborne" = "Gisborne", "Hawkes.Bay" = "Napier", "Taranaki" = "Stratford", "Manawatu.Wanganui" = "Palmerston North", "Wellington" = "Upper Hutt", "Nelson.Marlborough" = "Nelson", "Canterbury" = "Christchurch", "Southland" = "Invercargill", "Waikato" = "Hamilton", "Bay.of.Plenty" = "Rotorua", "West.Coast" = "Greymouth Aero Ews", "Otago" = "Dunedin")

times_kea_VKT = read.csv("downloaded_stats/times_kea_VKT.csv")
times_tui_VKT = read.csv("downloaded_stats/times_tui_VKT.csv")

kea_or_power_usage = read.csv('downloaded_stats/times_kea_energy_usage.csv')
tui_or_power_usage = read.csv('downloaded_stats/times_tui_energy_usage.csv')

#converting PJ to GWh and adding to VKT file
times_kea_VKT$power_usage = kea_or_power_usage$Electricity/3.6*1000
times_tui_VKT$power_usage = tui_or_power_usage$Electricity/3.6*1000

times_kea_VKT$consumption = times_kea_VKT$power_usage/times_kea_VKT$Electricity 
times_tui_VKT$consumption = times_tui_VKT$power_usage/times_tui_VKT$Electricity


save(vkt_quart, vkt_yearly, vkt_regions, vkt_pass,times_kea_VKT, times_tui_VKT, file = "processed_data/VKT_data.rda")

rm(list = ls())
