library(rnoaa)
library(RCurl)

## readclipboard for lat, lon, gpsdatetime

#years = format(as.Date(readClipboard(), format="%m/%d/%Y"), "%Y")
years = format(as.Date(gpsdatetime, format="%m/%d/%Y"), "%Y")

files2017 <- read.table(text = gsub(",", "\t", readLines(curl("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2017/"))))
files2018 <- read.table(text = gsub(",", "\t", readLines(curl("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2018/"))))
files2019 <- read.table(text = gsub(",", "\t", readLines(curl("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/2019/"))))
files = rbind(files2017, files2018, files2019)

x <- isd_stations_search(lat = lat[1], lon = lon[1], radius = 200)
x <- subset(x, icao!="" & wban < 99999 & usaf<999999)
for (j in 1:nrow(x)){
  fn <- paste(x[j,]$usaf, x[j,]$wban, paste(years[i], ".gz", sep=""), sep="-")
  if (length(which(files$V9==fn))){
    df <- as.data.frame(x[j,])
    break
  }
}

for (i in 2:length(lat)){
  x <- isd_stations_search(lat = lat[i], lon = lon[i], radius = 200)
  x <- subset(x, icao!="" & wban < 99999 & usaf<999999)
  for (j in 1:nrow(x)){
    fn <- paste(x[j,]$usaf, x[j,]$wban, paste(years[i], ".gz", sep=""), sep="-")
    if (length(which(files$V9==fn))){
      df <- rbind(df,x[j,])
      break
    }
  }
}

df["year"] <- years
df_sub = unique(df[,-12]) # ignore distance from each lat/lon point as a unique column
df_sub <- unique(df_sub)

x <- df_sub[1,]
dat <- isd(usaf = x$usaf, wban = x$wban, year = x$year)
temp = data.frame(dat$usaf_station, dat$wban_station, dat$date, dat$time, dat$temperature, dat$temperature_quality)

for (i in 2:dim(df_sub)[1]){
    x <- df_sub[i,]
    dat <- isd(usaf = x$usaf, wban = x$wban, year = x$year)
    temp <- rbind(temp, data.frame(dat$usaf_station, dat$wban_station, dat$date, dat$time, dat$temperature, dat$temperature_quality))
}
temp$dat.date = as.Date(temp$dat.date, format="%Y%m%d")
gpsdatetime_p = as.POSIXct(gpsdatetime, format="%m/%d/%Y %I:%M:%OS %p")

x = subset(temp, dat.temperature_quality != 9)
temp = x
rm(x)

dt = apply(temp[,c('dat.date', 'dat.time')], 1, function(x) paste(x[1],x[2]))
dt = as.POSIXct(dt, format="%Y-%m-%d %H%M")
temp["datetime"] = dt

x <- subset(temp, dat.date==as.Date(gpsdatetime_p[1]) & dat.usaf_station==df[1,]$usaf)
y = which.min(abs(x$datetime - gpsdatetime_p[1]))
df_temps = rep(-1000,nrow(df))
df_temps[1] = as.numeric(as.character(x[y,]$dat.temperature))

for (i in 2:dim(df)[1]){
  x <- subset(temp, dat.date==as.Date(gpsdatetime_p[i]) & dat.usaf_station==df[i,]$usaf)
  y = which.min(abs(x$datetime - gpsdatetime_p[i]))
  if (length(y)){
    df_temps[i] = as.numeric(as.character(x[y,]$dat.temperature))
  }
}
