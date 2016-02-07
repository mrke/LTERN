library(NicheMapR)
source("../micro_australia/get.soil.R")

longlat<-c(138.466, -23.766) # Ethabuka
loc<-longlat
ystart <- 2014# start year
yfinish <- 2014# end year
nyears<-yfinish-ystart+1# integer, number of years for which to run the microclimate model

DEP <- c(0., 1.,  3, 5.,  10,  15,  30.,  60.,  100.,  200.) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
soil.hydro<-get.soil()
PE<-soil.hydro$PE
BB<-soil.hydro$BB
BD<-soil.hydro$BD
KS<-soil.hydro$KS
PE[1:9]<-CampNormTbl9_1[3,4] #air entry potential J/kg 
KS[1:9]<-CampNormTbl9_1[3,6] #saturated conductivity, kg s/m3
BB[1:9]<-CampNormTbl9_1[3,5] #soil 'b' parameter
PE[10:13]<-CampNormTbl9_1[4,4] #air entry potential J/kg 
KS[10:13]<-CampNormTbl9_1[4,6] #saturated conductivity, kg s/m3
BB[10:13]<-CampNormTbl9_1[4,5] #soil 'b' parameter
BulkDensity <- BD[seq(1,19,2)]*1000#rep(1360,10) # soil bulk density (kg/m3)

# run microclimate model to get microclimate for ectotherm model and soil temps for predicting egg development and food availability
micro<-micro_aust(loc = longlat, ystart = ystart, yfinish = yfinish, PE = PE, BB = BB, BD = 
    BD, KS = KS, BulkDensity = BulkDensity, maxshade = 50, Usrhyt = 0.03, DEP = DEP, REFL = 0.2)

#save(micro,file = 'micro.Rda') # for use if no access to AWAP server
#load('micro.Rda')

# append dates
tzone=paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates=subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
metout=as.data.frame(micro$metout)
shadmet=as.data.frame(micro$shadmet)
soil=as.data.frame(micro$soil)
shadsoil=as.data.frame(micro$shadsoil)
rainfall=as.data.frame(micro$RAINFALL)
metout=cbind(dates,metout)
soil=cbind(dates,soil)
shadmet=cbind(dates,shadmet)
shadsoil=cbind(dates,shadsoil)

debpars=as.data.frame(read.csv('DEB model/DEB_pars_Tiliqua_rugosa.csv',header=FALSE))$V1
startday=1#365*6 # make it 90 for T. rugosa loop day of year at which DEB model starts
TBASK = 26
raindrink=2.5
mu_E = 585000

nicheout=ectotherm(ABSMAX = 0.866, ABSMIN = 0.866, VTMAX = 39, VTMIN = 26, write_input = 0, EMISAN = 1, gutfill = 75,
  TBASK = TBASK, TEMERGE = 8.5, ctmax = 39, ctmin = 3.5, TPREF = 33.5,  peyes = 0.03, ptcond = 0.1, skinwet = 0.2,
  shdburrow = 1, minwater = 15, maxshades = micro$MAXSHADES, mindepth = 3, raindrink = raindrink, DEB = 1, z=debpars[8], del_M=debpars[9], F_m = 13290,  
  kap_X=debpars[11],   v=debpars[13]/24, kap=debpars[14], p_M=debpars[16]/24, 
  E_G=debpars[19],   kap_R=debpars[15], k_J=debpars[18]/24, E_Hb=debpars[20],
  E_Hp=debpars[21], h_a=debpars[22]*10^-1/(24^2),   s_G=debpars[23],   E_0=debpars[24],  mu_E = mu_E,
  T_REF = debpars[1]-273, TA = debpars[2], TAL = debpars[5], TAH = debpars[6], TL = debpars[3], TH = debpars[4], 
  E_sm=186.03*6, K = 500, X = 111.7, plantsim = c(4, 4, 14, -200, -1500, 82), clutch_ab = c(0.085,0.7), viviparous = 1,
  photostart = 4, E_init = ((debpars[16]/24)*debpars[8]/debpars[14])/(debpars[13]/24), E_H_init = debpars[20]+5,  
  v_init = debpars[25]^3, stage = 1, mh = 1,  minclutch = 0, clutchsize = 2., startday = startday)

# retrieve output
metout=as.data.frame(nicheout$metout)
shadmet=as.data.frame(nicheout$shadmet)
soil=as.data.frame(nicheout$soil)
shadsoil=as.data.frame(nicheout$shadsoil)
rainfall=as.data.frame(nicheout$RAINFALL)
foodwaters=as.data.frame(nicheout$foodwaters)
foodlevels=as.data.frame(nicheout$foodlevels)
environ=as.data.frame(nicheout$environ)
enbal=as.data.frame(nicheout$enbal)
masbal=as.data.frame(nicheout$masbal)
humid=as.data.frame(nicheout$humid)
debout=as.data.frame(nicheout$debout)
yearout=as.data.frame(nicheout$yearout)
foodwaters=as.data.frame(nicheout$foodwaters)
foodlevels=as.data.frame(nicheout$foodlevels)
if(nyears>1){
  yearsout=as.data.frame(nicheout$yearsout[1:nyears,])
}else{
  yearsout=t(as.data.frame(nicheout$yearsout))
}


# append dates
tzone=paste("Etc/GMT-",10,sep="") # doing it this way ignores daylight savings!
dates=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="hours")
dates=subset(dates, format(dates, "%m/%d")!= "02/29") # remove leap years
debout=cbind(dates,debout)
environ=cbind(dates,environ)
masbal=cbind(dates,masbal)
enbal=cbind(dates,enbal)
soil=cbind(dates,soil)
metout=cbind(dates,metout)
shadsoil=cbind(dates,shadsoil)
shadmet=cbind(dates,shadmet)


dates2=seq(ISOdate(ystart,1,1,tz=tzone)-3600*12, ISOdate((ystart+nyears),1,1,tz=tzone)-3600*13, by="days") 
dates2=subset(dates2, format(dates2, "%m/%d")!= "02/29") # remove leap years
grass=cbind(dates2,foodwaters,foodlevels)
colnames(grass)=c("dates","growth","tsdm")
rainfall=as.data.frame(cbind(dates2,rainfall))
colnames(rainfall)=c("dates","rainfall")

############### plot results ######################

environ=as.data.frame(environ)

addTrans = function(color,trans)
{
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color = rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans = rep(trans,length(color))
  
  num2hex = function(x)
  {
    hex = unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb = rbind(col2rgb(color),trans)
  res = paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

# For Fig. S6
with(debout, {plot(WETMASS~dates,type = "l",ylab = "wet mass (g)/rainfall(mm*10)",ylim=c(0,1300),col='black')})
points(rainfall$rainfall*10~dates2,type='h',col='blue')

# plot mass and reproduction phenology
plotdebout=subset(debout,as.numeric(format(dates, "%Y"))<1994) # use this if you want to subset particular years
plotdebout=debout # this just keeps it to all years
year_vals=subset(plotdebout,as.character(dates,format='%d/%m')=="01/01")
year_vals=subset(year_vals,as.character(year_vals$dates,format='%H')=="00") # get midnight
plot(plotdebout$WETMASS~plotdebout$dates,type='l', ylab="wet mass, g",xlab="date") # plot wet mass as a function of date
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$CUMBATCH/1000~debout$dates,type='l', ylab='total energy, kJ',xlab="date") # plot energy in batch buffer (yolking eggs)
points(debout$CUMREPRO/1000~debout$dates,type='l',col='red') # plot energy in the reproduction buffer (captial for egg production, but not currently transformed to eggs)
abline(v=year_vals$dates,col='grey',lty=2) # add lines to show beginning of each year
plot(debout$V_baby~debout$dates,type='l', ylab='embryo structure (cm3)',xlab="date") # plot embryo development (volume of structure)
points(debout$Breeding/10~debout$dates,type='l',col='light blue',lty=1)

# get birthdays
V_baby2=debout[1:(nrow(debout)-1),15]-debout[2:(nrow(debout)),15]
V_baby2[V_baby2>0.1]=1
V_baby2[V_baby2!=1]=0
V_baby2=c(V_baby2,0)
debout$birth=V_baby2
format(subset(debout,birth==1)$dates,"%d/%m/%Y")

# Body mass with and without lost water
with(debout, {plot((WETMASS-WETMASS*(Body_cond/100))~dates,type = "l",xlab = "day of year",ylab = "wet mass (g)",col='blue')})
with(debout, {points(WETMASS~dates,type = "l",ylab = "wet mass (g)/grass growth")})

library(lattice)
with(environ, {xyplot(TC+ACT*5+SHADE/10+DEP/10~dates,type = "l")})

