set.seed(1234567890)
library(geosphere)

stations = read.csv("stations.csv")
temps = read.csv("temps50k.csv")

st = merge(stations,temps,by="station_number")

h_distance = 1
h_date = 1
h_time = 1

longitud = 58.4274 # The point to predict
latitud = 14.826
date = "2013-11-04" # The date to predict

a = as.matrix(temps)
test = temps[,temps$date]
difftime(date1, date2, units = "days")


times = c("04:00:00", "06:00:00","08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

help("distHaversine")

temp = vector(length=length(times))

distanceDiff = function(longitud1, latitud1, longitud2, latitud2){
  point1 = c(longitud1, latitud1)
  point2 = c(longitud2, latitud2)
  distance = distHaversine(point1, point2)
  weighted_distance = abs(distance) / h_distance
  return(weighted_distance)
}

dateDiff = function(date1, date2){
  days = difftime(date1, date2, units = "days")
  weighted_days = abs(days) / h_date
  return(weighted_days)
}

timeDiff = function(time1, time2){
  hours = difftime(time1, time2, units = "hours")
  weighted_hours = abs(hours) / h_time
  return(weighted_hours)
}

calc_gaussian_kernel = function(distance, days, hours){
  vec = c(distance, days, hours)
  sqrt(sum(vec^2))
}


plot(temp, type="o")