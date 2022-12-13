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
# longitud = 12.3836
# latitud = 55.8203
date = "2013-11-04" # The date to predict

# Filter out measurements with posterior date
filtered_st = st[difftime(st[,"date"], date, units = "days") <= 0,]

# Remove columns that are not used in this task
filtered_st = within(filtered_st, rm("station_number", "station_name", 
                                     "measurement_height", "readings_from",
                                     "readings_to", "elevation", "quality"))


times = c("04:00:00", "06:00:00","08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")


temp = vector(length=length(times))

#function to calculate Gaussian kernel
gausinKernel = function(x,h){
  x = x/h
  K = exp(-((x)^2))
  return(K)
}

distanceDiff = function(longitud1, latitud1, longitud2, latitud2){
  point1 = c(longitud1, latitud1)
  point2 = c(longitud2, latitud2)
  distance = distHaversine(point1, point2)
  K = gausinKernel(distance, h_distance)
  return(K)
}

dateDiff = function(date1, date2){
  days = difftime(date1, date2, units = "days")
  days = as(days, "numeric")
  K = gausinKernel(days, h_date)
  return(K)
}

timeDiff = function(time1, time2){
  hours = difftime(time1, time2, units = "hours")
  hours = as(hours, "numeric")
  K = gausinKernel(hours, h_time)
  return(K)
}


create_Kernel = function(longitud, latitud, date, times, st){
  k_sum = matrix(0, nrow = nrow(st), ncol = length(times))
  for (column in 1:length(times)){
    for (row in 1:nrow(st)){
      this_column = c()
      k_distance = distanceDiff(longitud, latitud, st[row,2], st[row,1])
      k_date = dateDiff(date1 = date, date2 = st[row,3])
      k_time = timeDiff(time1 = as.POSIXct(times[1], format = "%H:%M:%S"), 
                        time2 = as.POSIXct(st[row,4], format = "%H:%M:%S"))
      k_sum[row,column] = sum(k_distance, k_date, k_time) 
      this_column = append(this_column, k_sum[row,column]*st[row,5]) 
      
    }
    first = sum(row,column)
    second = first * st[,5]
    third = second/first
    
    
  }
  return(k_sum)
}

k = create_Kernel(longitud, latitud, date, times, filtered_st)

plot(temp, type="o")
