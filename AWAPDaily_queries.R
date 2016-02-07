# Queries for AWAPDaily, M. Kearney Feb 2012
#
# Tables: 1900-2010
#
# [id]
# [day]
# [rr] rainfall (mm), 1900-2010
# [tmax] maximum air temperature, 1911-2010
# [tmin] minimum air temperature, 1911-2010
# [vpr] vapour pressure hPa
# [sol] global daily solar radiation MJ m-2 d-1
#
# Table: latlon
#
# [id]
# [latitude]
# [longitude]


####################################################################################
####################################################################################
####################################################################################

# begin query for a specific site for a given period, site specified with Google Maps lookup

####### BEGIN USER INPUT #######################################
loc <- "Newman, Western Australia" # type in a location here
datestart<-"01/01/2015" # day, month, year
datefinish<-"31/12/2015" # day, month, year
####### END USER INPUT #########################################

library(RODBC)
library(chron)
channel <- odbcConnect("AWAPDaily")

library(dismo)
longlat <- geocode(loc)[1, 3:4] # assumes first geocode match is correct
# specify bounding box around selected lat/long
lat<-longlat[2]
lon<-longlat[1]
lat1<-lat-0.025
lat2<-lat+0.025
lon1<-lon-0.025
lon2<-lon+0.025

datestart<-strptime(datestart, "%d/%m/%Y") # convert to date format
datefinish<-strptime(datefinish, "%d/%m/%Y") # convert to date format
yearstart<-as.numeric(format(datestart, "%Y")) # yet year start
yearfinish<-as.numeric(format(datefinish, "%Y")) # yet year finish
years<-seq(yearstart,yearfinish,1) # get sequence of years to do
juldaystart<-datestart$yday+1 # get Julian day of year at start
juldayfinish<-datefinish$yday+1 # get Julian day of year at finish

for(i in 1:length(years)){ # start loop through years
# syntax for query

if(length(years)==1){ # doing a period within a year
query<-paste("SELECT a.latitude, a.longitude, b.*
    FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day between ",juldaystart," and ",juldayfinish,")
  order by b.day",sep="")
}else{
if(i==1){ # doing first year, start at day requested
query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day >= ",juldaystart,")
  order by b.day",sep="")
}else{
if(i==length(years)){ # doing last year, only go up to last day requested
query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day <= ",juldayfinish,")
  order by b.day",sep="")
}else{ # doing in between years, so get all data for this year
query<-paste("SELECT a.latitude, a.longitude, b.*
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,")
  order by b.day",sep="")
}}}


# exectue query and concatenate if necessary
if(i==1){
output<- sqlQuery(channel,query)
}else{
output<-rbind(output,sqlQuery(channel,query))
}
} # end loop through years
output$sol<-as.numeric(as.character(output$sol))
# plot data
dates<-seq(datestart,datefinish,"days")
output<-cbind(dates,output)
plot(output$dates,output$rr, type='h', xlab = "month", ylab = "rainfall (mm/d)")
plot(output$dates,output$tmin, type='l', xlab = "month", col='4', ylab = expression("min/max air temp ("*degree*"C)"), ylim=c(min(output$tmin),max(output$tmax)))
points(output$dates,output$tmax, type='l', col='2')
plot(output$dates,output$vpr, type='l', xlab = "month", ylab = "vapour pressure (hPa)")
plot(output$dates,output$sol, type='l', xlab = "month", ylab = "solar radiation (MJ/m^2/d)")


write.csv(output,'/NicheMapR_Working/test_output.csv')
# end query for a specific site for a given period, site specified with Google Maps lookup

####################################################################################
####################################################################################
####################################################################################

# begin query AWAPDaily from a list of sites specified in a .csv file for a given time period

####### BEGIN USER INPUT #######################################
sitesfile <- "C:/NicheMapR_Working/PredEcolServer/tawny_points.csv" # file with longitude/latitudes
datestart<-"1/1/1960" # day, month, year
datefinish<-"31/12/2013" # day, month, year
####### END USER INPUT #########################################

library(RODBC)
channel <- odbcConnect("AWAPDaily")

# get sites list
x<-as.data.frame(read.table(file = sitesfile, sep = ",",head=TRUE))
numsites<-length(x[,1])
colnames(x)<-c("lat","lon")
x<-cbind(x$lon,x$lat)

# process period of interest
datestart<-strptime(datestart, "%d/%m/%Y") # convert to date format
datefinish<-strptime(datefinish, "%d/%m/%Y") # convert to date format
yearstart<-as.numeric(format(datestart, "%Y")) # get year start
yearfinish<-as.numeric(format(datefinish, "%Y")) # yet year finish
years<-seq(yearstart,yearfinish,1) # get sequence of years to do
juldaystart<-datestart$yday+1 # get Julian day of year at start
juldayfinish<-datefinish$yday+1 # get Julian day of year at finish
dates<-seq(datestart,datefinish+3600,"days") # sequence of dates

# loop through sites, make queries and collate data
for(j in 1:numsites){ # start loop through sites

lat<-x[j,2]
lon<-x[j,1]
lat1<-lat-0.025
lat2<-lat+0.025
lon1<-lon-0.025
lon2<-lon+0.025

for(i in 1:length(years)){ # start loop through years
# syntax for query

if(length(years)==1){ # doing a period within a year
query<-paste("SELECT a.latitude, a.longitude, b.tmax, b.tmin, b.rr
    FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day between ",juldaystart," and ",juldayfinish,")
  order by b.day",sep="")
}else{
if(i==1){ # doing first year, start at day requested
query<-paste("SELECT a.latitude, a.longitude, b.tmax, b.tmin, b.rr
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day >= ",juldaystart,")
  order by b.day",sep="")
}else{
if(i==length(years)){ # doing last year, only go up to last day requested
query<-paste("SELECT a.latitude, a.longitude, b.tmax, b.tmin, b.rr
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day <= ",juldayfinish,")
  order by b.day",sep="")
}else{ # doing in between years, so get all data for this year
query<-paste("SELECT a.latitude, a.longitude, b.tmax, b.tmin, b.rr
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,")
  order by b.day",sep="")
}}}

# exectue query and concatenate if necessary
if(i==1){
output<- sqlQuery(channel,query)
}else{
output<-rbind(output,sqlQuery(channel,query))
}
} # end loop through years
if(j==1){
  results<-output
}else{
  results<-rbind(results,output)
}
} # end loop through sites
results<-cbind(rep(dates,nrow(results)/nrow(output)),results)
colnames(results)[1]<-"date"
# write output
setwd("C:/NicheMapR_Working/projects/Tawny Dragons/")
filename<-paste("results_flinders.csv")
write.table(results, file = filename, sep = ",", col.names = NA, qmethod = "double")

# end query AWAPDaily from a list of sites specified in a .csv file for a given time period

####################################################################################
####################################################################################
####################################################################################

# begin query AWAPDaily for a grid of pixels of a particular variable at a particular time

####### BEGIN USER INPUT #######################################
loc <- "Kulgera, Australia" # type in a location here
datestart<-"17/11/2015" # day, month, year
datefinish<-"17/11/2015" # day, month, year
variable<-"sol"
quadlen<-1000 # quadrangle length (# of 0.05 degree steps north/south, centred on location)
quadwid<-1000 # quadrangel width (# of 0.05 degree steps east/west, centred on location)
####### END USER INPUT #########################################

library(RODBC)
channel <- odbcConnect("AWAPDaily")

# process dates
datestart<-strptime(datestart, "%d/%m/%Y") # convert to date format
datefinish<-strptime(datefinish, "%d/%m/%Y") # convert to date format
yearstart<-as.numeric(format(datestart, "%Y")) # get year start
yearfinish<-as.numeric(format(datefinish, "%Y")) # yet year finish
years<-seq(yearstart,yearfinish,1) # get sequence of years to do
juldaystart<-datestart$yday+1 # get Julian day of year at start
juldayfinish<-datefinish$yday+1 # get Julian day of year at finish
dates<-seq(datestart,datefinish,"days") # sequence of dates

library(dismo)
longlat <- geocode(loc)[1, 2:3] # assumes first geocode match is correct
longlat<-c(131.1676,-22.724)
quadres<-0.05 # quadrangle resolution (degrees)

# make a grid of points centred on the chosen location
gt = GridTopology(cellcentre.offset = c(as.numeric(longlat[2]-(quadlen-1)*quadres/2), as.numeric(longlat[1]-(quadwid-1)*quadres/2)), cellsize = c(quadres,quadres), cells.dim = c(quadlen, quadwid))
grd = SpatialGrid(gt)
x<-data.frame(coordinates(grd))
names(x)<-c("lat","lon")
x<-subset(x,select=c(lon,lat))

lat1<-min(x[,2]) # min latitude
lat2<-max(x[,2]) # max latitude
lon1<-min(x[,1]) # min longitude
lon2<-max(x[,1]) # max longitude

# query syntax
for(i in 1:length(years)){ # start loop through years
# syntax for query

if(length(years)==1){ # doing a period within a year
query<-paste("SELECT a.latitude, a.longitude, b.",variable,"
    FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day between ",juldaystart," and ",juldayfinish,")
  order by b.day",sep="")
}else{
if(i==1){ # doing first year, start at day requested
query<-paste("SELECT a.latitude, a.longitude, b.",variable,"
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day >= ",juldaystart,")
  order by b.day",sep="")
}else{
if(i==length(years)){ # doing last year, only go up to last day requested
query<-paste("SELECT a.latitude, a.longitude, b.",variable,"
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,") and (b.day <= ",juldayfinish,")
  order by b.day",sep="")
}else{ # doing in between years, so get all data for this year
query<-paste("SELECT a.latitude, a.longitude, b.",variable,"
  FROM [AWAPDaily].[dbo].[latlon] as a
  , [AWAPDaily].[dbo].[",years[i],"] as b
  where (a.id = b.id) and (a.latitude between ",lat1," and ",lat2,") and (a.longitude between ",lon1," and ",lon2,")
  order by b.day",sep="")
}}}


# exectue query and concatenate if necessary
if(i==1){
output<- sqlQuery(channel,query)
}else{
output<-rbind(output,sqlQuery(channel,query))
}
} # end loop through years

alldates<-rep(dates,nrow(output)/length(dates))
alldates<-sort(alldates)
output<-cbind(alldates,output)
colnames(output)[1]<-"date"

# make raster for a given variable

# create an empty raster
lat1<-min(output$latitude)-.025 # min latitude
lat2<-max(output$latitude)-.025 # max latitude
lon1<-min(output$longitude)-.025 # min longitude
lon2<-max(output$longitude)-.025 # max longitude
quadwid<-(lon2-lon1)/.05
quadlen<-(lat2-lat1)/.05
gridout <- raster(ncol=quadwid, nrow=quadlen, xmn=lon1, xmx=lon2, ymn=lat1, ymx=lat2)

output[ is.na(output) ] <- -1 # get rid of na values which are often in the solar dataset (otherwise can't make grids)
brks<-round(seq(min(output[4]),max(output[4]),(max(output[4])-min(output[4]))/10))

# setwd("c:/nichemapr/temp")
for(i in 1:length(dates)){
vals <- subset(output,  subset=(date==dates[i]))
x<-cbind(vals$longitude,vals$latitude) # list of co-ordinates
# make, plot and write grid
grid <- rasterize(x, gridout, vals[,4])
if(i==1){s<-grid}else{s<-stack(s,grid)}
#plot(grid,breaks=brks)
#filename<-paste(variable,"_",i,"_",year,".tif",sep="")
# writeRaster(gridout, filename=filename, progress='window', overwrite=TRUE)
}
plot(grid)

#plot(s,breaks=brks,col=terrain.colors(length(brks)))
