set.seed(1234567890)
library(geosphere)

stations = read.csv("stations.csv", header = TRUE)
temps = read.csv("temps50k.csv", header = TRUE)

st = merge(stations,temps,by="station_number")

h_distance = 1
h_date = 1
h_time = 1

longitud = 58.4274 # The point to predict
latitud = 14.826
date = "2013-11-04" # The date to predict

# Filter out measurements with posterior date
filtered_st = st[difftime(st[,"date"], date, units = "days") <= 0,]

# Remove columns that are not used in this task
filtered_st = within(filtered_st, rm("station_number", "station_name", 
                                     "measurement_height", "readings_from",
                                     "readings_to", "elevation", "quality"))

a = as.matrix(temps["date"], "%Y-%m-%d")
a = as.Date(a, "%Y-%m-%d")
x<-format(a, format="%m-%d")

x[1]-x[2]
# TODO: Klura ut hur man räknar ut diff mellan dagar utan att år ska spela roll
test = as.matrix(temps[2])
date1 = as.Date(test[2])
date2 = test[1]
x<-format(a, format="%m-%d")

difftime(date1, date2, units = "days")

times = c("04:00:00", "06:00:00","08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")


temp = vector(length=length(times))

#function to calculate Gaussian kernel
gausinKernel = function(x,h){
  K = exp(-((x/h)^2))
  return(K)
}

distanceDiff = function(longitud1, latitud1, longitud2, latitud2){
  point1 = c(longitud1, latitud1)
  point2 = c(longitud2, latitud2)
  distance = distHaversine(point1, point2)
  # weighted_distance = abs(distance) / h_distance
  K = gausinKernel(distance, h_distance)
  return(K)
}

dateDiff = function(date1, date2){
  date1 = as.matrix(date1, "%Y-%m-%d")
  date1 = as.Date(date1, "%Y-%m-%d")
  date1 = format(date1, format="%m-%d")
  
  date2 = as.matrix(date2, "%Y-%m-%d")
  date2 = as.Date(date2, "%Y-%m-%d")
  date2 = format(date2, format="%m-%d")
  
  days = difftime(date1, date2, units = "days")
  # weighted_days = abs(days) / h_date
  K = gausinKernel(days, h_date)
  return(K)
}

timeDiff = function(time1, time2){
  hours = difftime(time1, time2, units = "hours")
  # weighted_hours = abs(hours) / h_time
  K = gausinKernel(hours, h_time)
  return(K)
}

long_st = filtered_st[,5]
lat_st = filtered_st[,4]


distance_test = distanceDiff(longitud1 = longitud, latitud1 = latitud,
                             longitud2 = long_st, latitud2 = lat_st)

plot(temp, type="o")
