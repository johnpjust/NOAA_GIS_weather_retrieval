substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

df = read.csv('C:/Users/just/Desktop/HFdt.csv')
df[,2] <- as.POSIXct(df[,2], format="%m/%d/%Y %I:%M:%OS %p")
df[,1] <- as.character(df[,1])
df[,3] <- as.numeric(df[,3])
df['date'] <- as.Date(df[,2])
df['mach'] <- substrRight(df[,1],2)

dfall = read.csv('C:/Users/just/Desktop/needtomatch.csv')
dfall[,2] <- as.POSIXct(dfall[,2], format="%m/%d/%Y %I:%M:%OS %p")
dfall[,1] <- as.character(dfall[,1])
dfall['date'] <- as.Date(dfall[,2])
dfall['mach'] <- substrRight(dfall[,1],2)

x <- subset(df, df$date==dfall$date[1] & df$mach==dfall$mach[1])
y = which.min(abs(x$Median.GPSdatetime. - dfall$Median.GPSdatetime.[1]))
df_mois = rep(-1000,nrow(dfall)) #prime dataframe
if (length(y)){
  df_mois[1] = x[y,]$Median.MeanMC.
}

for (i in 2:dim(dfall)[1]){
  x <- subset(df, df$date==dfall$date[i] & df$mach==dfall$mach[i])
  y = which.min(abs(x$Median.GPSdatetime. - dfall$Median.GPSdatetime.[i]))
  if (length(y)){
    df_mois[i] = x[y,]$Median.MeanMC.
  }
}
