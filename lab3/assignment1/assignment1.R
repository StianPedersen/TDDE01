set.seed(1234567890)
library(geosphere)

stations = read.csv("stations.csv", header = TRUE)
temps = read.csv("temps50k.csv", header = TRUE)

st = merge(stations,temps,by="station_number")

h_distance = 300000
h_date = 15
h_time = 3

longitud = 58.4274 # The point to predict
latitud = 14.826

date = "2013-11-04" # The date to predict

# Filter out measurements with posterior date
filtered_st = st[difftime(st[,"date"], date, units = "days") <= 0,]

# Remove columns that are not used in this task
filtered_st = within(filtered_st, rm("station_number", "station_name", 
                                     "measurement_height", "readings_from",
                                     "readings_to", "elevation", "quality"))
# Order data by date in increasing order
ordered_st = filtered_st[order(filtered_st$date, decreasing = FALSE),]

times = c("04:00:00", "06:00:00","08:00:00", "10:00:00", "12:00:00", "14:00:00",
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")

# Actual temperatures (taken from SMHI) measured in Linköping (deemed as close
# enough)
actual_temps = c(5.3, 5.6, 6.3, 7.4, 7.8, 7.0, 4.9, 5.6, 5.0, 4.7, 4.7)

#function to calculate Gaussian kernel
gausinKernel = function(x,h){
  x = x/h
  K = exp(-((x)^2))
  return(K)
}

# function to calculate physical distance between two points and get the 
# Gaussian kernel
distanceDiff = function(longitud1, latitud1, longitud2, latitud2){
  point1 = c(longitud1, latitud1)
  point2 = c(longitud2, latitud2)
  distance = distHaversine(point1, point2)
  K = gausinKernel(distance, h_distance)
  return(K)
}

# function to calculate the amount of days between two dates and get the 
# Gaussian kernel
dateDiff = function(date1, date2){
  days = difftime(date1, date2, units = "days")
  days = as(days, "numeric")
  days = days %% 365
  days = ifelse(days < 182.5, days, 365 - days)
  K = gausinKernel(days, h_date)
  return(K)
}

# function to calculate amount of hours between time points and get the 
# Gaussian kernel
timeDiff = function(time1, time2){
  hours = difftime(time1, time2, units = "hours")
  hours = as(hours, "numeric")
  hours = ifelse(hours < 12.5, hours, 24 - hours)
  K = gausinKernel(hours, h_time)
  return(K)
}

# Calculate kernel of three Gaussian kernels for both sum and product
create_Kernel = function(longitud, latitud, date, times, st){
  
  # Create a vector to fill with predictions
  temp = matrix(0, nrow = 2, ncol = 11) 
  
  for (column in 1:length(times)){ 
    # Function to filter out samples from future measurements relative to 
    # time and date och prediction
    if (times[column] >= "06:00:00"){
      nr_of_rows = nrow(st) 
    } else {
      nr_of_rows = nrow(st) - 2
    }
    
    # Create matrices to fill so we can calculate the sum/product of all kernels 
    # for a given time
    k_column_sum = matrix(0, nrow = nr_of_rows, ncol = 1) 
    k_weighted_temp_sum = matrix(0, nr_of_rows, ncol = 1)
    
    k_column_product = matrix(0, nrow = nr_of_rows, ncol = 1) 
    k_weighted_temp_product = matrix(0, nr_of_rows, ncol = 1)
    
    for (row in 1:nr_of_rows){
      
      # Calculate all kernels for a given measurement
      k_distance = distanceDiff(longitud, latitud, st[row,2], st[row,1])
      k_date = dateDiff(date1 = date, date2 = st[row,3])
      k_time = timeDiff(time1 = as.POSIXct(times[column], format = "%H:%M:%S"), 
                        time2 = as.POSIXct(st[row,4], format = "%H:%M:%S"))
      
      # Calculate the sum of all kernels for a specific time
      k_column_sum[row] = sum(k_distance, k_date, k_time) 
      k_weighted_temp_sum[row] =  k_column_sum[row] * st[row,5]
      
      # Calculate the product of all kernels for a specific time
      k_column_product[row] = k_distance * k_date * k_time 
      k_weighted_temp_product[row] =  k_column_product[row] * st[row,5]
      
    }
    
    # Calculate predictions using kernels calculated as sum of kernels for a 
    # specific time
    temp[1, column] = colSums(k_weighted_temp_sum) / colSums(k_column_sum)
    
    # Calculate predictions using kernels calculated as product of kernels for 
    # a specific time
    temp[2, column] = colSums(k_weighted_temp_product) / colSums(k_column_product)
    }
  
  return(temp)
}

temp = create_Kernel(longitud, latitud, date, times, ordered_st)

# Plot everything
plot(y = temp[1,], x = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24), type="o", 
     ylab = "Predicted temperature (°C)",xlab = "Time of day (h)", xaxt = "n", 
     ylim = c(0,max(max(temp), max(actual_temps))), col = "red")
points(y = temp[2,], x = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24), 
       type="o",  col = "blue")
points(y = actual_temps, x = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24), 
       type="o",  col = "grey")
axis(1, at = c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24))
legend("topright", legend = c("Sum Kernel", 
                              "Product Kernel", "Actual temperatures"), 
       col = c("red", "blue", "grey"), pch = 16)


