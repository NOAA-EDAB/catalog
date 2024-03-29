### species distribution analysis

#```{r, echo = T, eval = F}

library(raster)
library(ncdf4)
library(stats)
library(geosphere)
library(plyr)

# set wd C:\1_analyses_ne_shelf\along shelf pos
#setwd(choose.dir(default=getwd()))
setwd("C:/1_analyses_ne_shelf/along shelf pos")
wd=getwd()


# name of survey data file select season <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
surfilename="Survdat_8_2017.Rdata"

# select season SPRING <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
selseaon="SPRING"
outfile="dhdc_8_2017_fallASS_sprDATA.csv"
outfile="dhdc_8_2017_fallASS_sprDATA jc.csv"
outfile="dhdc_8_2017_fallASS_sprDATA joe.csv"
outfile="dhdc_8_2017_fallASS_sprDATA all.csv"

# select season FALL <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
selseaon="FALL"
#outfile="dhdc_1_2017_sprASS_fallDATA.csv"
outfile="dhdc_1_2017_sprASS_fallDATA jc.csv"
outfile="dhdc_1_2017_sprASS_fallDATA joe.csv"
outfile="dhdc_1_2017_sprASS_fallDATA all.csv"


# read species list  sps.csv
#sps=read.csv(file.choose(), header = TRUE)
sps=read.csv(file="sps.csv", header = TRUE)
sps=read.csv(file="sps 312.csv", header = TRUE)
sps=read.csv(file="sps_joe.csv", header = TRUE)
sps=read.csv(file="sps_spring.csv", header = TRUE)
sps=read.csv(file="sps_fall.csv", header = TRUE)
numsps=nrow(sps)


# select species to analyze <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
spptokeep=sps$SVSPP
#spptokeep=c(73,74,75,76,77)
#spptokeep=c(312)  #Jonah Crab	312


# read in depth grid
gdepth=raster("nes_bath_data.nc", band=1)

# read in coordinates for along shelf diagnal  diag.csv
#diag=read.csv(file.choose(), header = TRUE)
diag=read.csv(file="diag.csv", header = TRUE)

# read in coordinate for coast     nes_coastline.csv
#nescoast=read.csv(file.choose(), header = TRUE)
nescoast=read.csv(file="nes_coastline.csv", header = TRUE)

# read in coordinate for coast     nes_coast_2.csv
#nescoast2=read.csv(file.choose(), header = TRUE)
nescoast2=read.csv(file="nes_coast_2.csv", header = TRUE)

# read in coordinate for coast V2     hersey_high_2.csv
#nesc2=read.csv(file.choose(), header = TRUE)
nesc2=read.csv(file="hersey_high_2.csv", header = TRUE)

# constants
radt=pi/180
R <- 6371 # Earth mean radius [km]




# CODE TO READ IN STRATA COMPUTE AREAS, NOW JUST READ IN STRATAREAS dataframe
# readin in strata.shp and compute areas of strata
#TrawlStrata<-shapefile(file.choose())
#plot(TrawlStrata)
#AREA<-areaPolygon(TrawlStrata, r=6371000)/10^6

# for array of strata and area and make into dataframe
#stratareas=cbind(TrawlStrata@data$STRATA, AREA)
#colnames(stratareas) <- c("STRATA","AREA")
#stratareas=data.frame(stratareas)
#save(stratareas, file="stratareas.rdata")

# load stratareas
load("stratareas.rdata")


# get durvey datafile  Survdat.RData
#load(file.choose())
#load(file="Survdat.RData")
load(file=surfilename)

# trin the data.... need to choose season
retvars <- c("CRUISE6","STATION","STRATUM","SVSPP","YEAR","SEASON",
             "LAT","LON","DEPTH","ABUNDANCE","BIOMASS")
survdat <- survdat[retvars]  		
survdat <- survdat[(survdat$SEASON==selseaon),]

# stata to use
# offshore strata to use
CoreOffshoreStrata<-c(seq(1010,1300,10),1340, seq(1360,1400,10),seq(1610,1760,10))
# inshore strata to use, still sampled by Bigelow
CoreInshore73to12=c(3020, 3050, 3080 ,3110 ,3140 ,3170, 3200,
                    3230, 3260, 3290, 3320, 3350 ,3380, 3410 ,3440)
# combine
strata_used=c(CoreOffshoreStrata,CoreInshore73to12)

# find records to keep based on core strata
rectokeep=survdat$STRATUM %in% strata_used

#table(rectokeep)
# add rec to keep to survdat
survdat=cbind(survdat,rectokeep)

# delete record form non-core strata
survdat=survdat[!survdat$rectokeep=="FALSE",]

# get rid of species
survdat$rectokeep=survdat$SVSPP %in% spptokeep
survdat=survdat[!survdat$rectokeep=="FALSE",]


# find unique tow records only, since length and weight removed
# unique deletes to one record per species
survdat <- unique(survdat)

# add field with rounded BIOMASS scaler used to adjust distributions
survdat$LOGBIO <- round(log10(survdat$BIOMASS * 10+10))
# take a look go from 1 to 5?
table(survdat$LOGBIO)

# trim the data.... to prepare to find stations only
retvars <- c("CRUISE6","STATION","STRATUM","YEAR")
survdat_stations <- survdat[retvars]  		
# unique reduces to a record per tow
survdat_stations <- unique(survdat_stations)


# make table of strata by year
numtowsstratyr=table(survdat_stations$STRATUM,survdat_stations$YEAR)

# find records to keep based on core strata
rectokeep=stratareas$STRATA %in% strata_used

# add rec to keep to survdat
stratareas=cbind(stratareas,rectokeep)

# delete record form non-core strata
stratareas_usedonly=stratareas[!stratareas$rectokeep=="FALSE",]

# creat areapertow
areapertow=numtowsstratyr

#compute area covered per tow per strata per year
for(i in 1:47){
  areapertow[,i]=stratareas_usedonly$AREA/numtowsstratyr[,i]
}

# change inf to NA and round and out in DF
areapertow[][is.infinite(areapertow[])]=NA
areapertow=round(areapertow)
areapertow=data.frame(areapertow)
colnames(areapertow) <- c("STRATA","YEAR","AREAWT")

# add col to survdat for strata weight
survdat$AREAPERTOW=NA

#fill AREAPERTOW
dimsurvdat=dim(survdat)
for (i in 1:dimsurvdat[1]){
  survdat$AREAPERTOW[i]=
    areapertow$AREAWT[which(survdat$STRATUM[i]==
                              areapertow$STRATA & 
                              survdat$YEAR[i]==
                              areapertow$YEAR)]
}

table(ceiling(survdat$AREAPERTOW/1000))
table(survdat$LOGBIO)
table(ceiling(survdat$AREAPERTOW/1000*survdat$LOGBIO/9))

# add col to survdat for PLOTWT
survdat$PLOTWT=NA
survdat$PLOTWT= ceiling(survdat$AREAPERTOW/1000*survdat$LOGBIO/9)

table(survdat$PLOTWT)

# Plot stations
plot(survdat$LON[survdat$YEAR==1974],survdat$LAT[survdat$YEAR==1974])


# put in shorter name
sdat=survdat

# clear some space
remove(survdat)

# number of records to evaluate
numrecs=nrow(sdat)


#======================================================================================

# TAKEN OUT SINCE THE SAME AS GASDIST

#blank array for ASDIST
#d = array(data = NA, dim =  nrow(diag))

# block to calculate diag distance  ASDIST
#for (j in 1:numrecs) {
#   print(numrecs-j)
# 
#  lat1=sdat$LAT[j]* radt
#  long1=sdat$LON[j]* radt
#  for (i in 1:150){
#    lat2=diag$LAT[i]* radt
#    long2=diag$LON[i]* radt
#    d[i] <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
#  }
#  dindex=which(d==min(d))
##  
#  lat1=34.60* radt
#  long1=-76.53* radt
#  
#  lat2=diag$LAT[dindex]* radt
#  long2=diag$LON[dindex]* radt
#  sdat$ASDIST[j] = acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
#}


#======================================================================================

# TAKEN OUT SINCE THE SAME AS GDTOC

#blank array for DTOC
#d = array(data = NA, dim = nrow(nescoast))

# block to calculate diag distance  DTOC
#for (j in 1:numrecs) {
##  print(numrecs-j)

#  lat1=sdat$LAT[j]* radt
#  long1=sdat$LON[j]* radt
#  for (i in 1:nrow(nescoast)){
#    lat2=nescoast$LAT[i]* radt
#    long2=nescoast$LON[i]* radt
#   d[i] <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
#  }
#  sdat$DTOC[j] = d[which(d==min(d))]

#}



#======================================================================================
print("distance to coast using geosphere")

####  Geosphere package to calc distance to coastline from pts (lon,lat), returns meters
dd = array(data = NA, dim = nrow(sdat))
pts = data.frame(sdat$LON, sdat$LAT)
#line = t(rbind(nescoast$Longitude, nescoast$Latitude))
#line = t(rbind(nesc2$LON, nesc2$LAT))
line_nescoast2 = t(rbind(nescoast2$LON, nescoast2$LAT))

#dd=dist2Line(pts[,], line)
dd=dist2Line(pts[,], line_nescoast2)
sdat$GDTOC=dd/1000 # convert meters to KM

# TESTING (look at sdat to compare dtc (geosphere) to DTOC (from loop))
# plot(line)
# lines(diag)
# lines(line)
# points(nescoast)
#ddtest=data.frame(dd)
#ddtest$distance=NULL
#plot(nescoast2)
#lines(nescoast2)
#ptt=105
#points(sdat$LON[ptt], sdat$LAT[ptt], col="red"); sdat$DTOC[ptt]; 
#sdat$dtc[ptt]; points(ddtest[ptt,], col="green")

#======================================================================================

print("diag distance using geosphere")
# Find distance to diagonal line (diag), use coordinates of 
# nearest point to find distance to NC outerbanks (min(diag))
dd2 = array(data = NA, dim = nrow(sdat))
dd2 = dist2Line(pts[,], diag, distfun=distHaversine)
#Distance of closest point to data along diag line to NC coast
p1 = diag[1,] #start of line
p3 = diag[150,] #end of line
p2 = data.frame(dd2[,2], dd2[,3])
distNC = distCosine(p1, p2, r=6378137) /1000 # convert to KM (Great circle distance)
sdat$GASDIST = distNC


#======================================================================================


# create column for missing depth data intially with depth data
sdat$MISDEPTH=sdat$DEPTH

# find cases with missing depth data
missingdepth=which(is.na(sdat$DEPTH))

# fill only those records in misdepth with depth from grid
for(k in missingdepth){
  sdat$MISDEPTH[k] = extract(gdepth,cbind(sdat$LON[k],sdat$LAT[k])) * -1
}

#=========================================================================================




#outline=paste("YR",",","SVSPP",",","mASDIST",",","mDTOC",",",
#"mMISDEPTH",",","mLAT",",","mLON",",","mGASDIST",",","mGDTOC")
#write.table(outline,file=outfile,row.name=F,col.names=F,append=TRUE)



out_data=array(NA,c((max(sdat$YEAR)-min(sdat$YEAR)+1)*numsps,7))

row_c=0

for (i in 1:numsps){
  print (i)
  for(j in min(sdat$YEAR):max(sdat$YEAR)){
    
    row_c=row_c+1   
    #sumdist=sum(sdat$ASDIST[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
    #*sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    #lendist=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    #mASDIST =sumdist / lendist
    
    #sumdist=sum(sdat$DTOC[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
    #*sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    #lendist=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    #mDTOC =sumdist / lendist
    
    sumdepth=sum(sdat$MISDEPTH[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
                 *sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    lendepth=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    mMISDEPTH =sumdepth / lendepth
    
    sumdepth=sum(sdat$LAT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
                 *sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    lendepth=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    mLAT =sumdepth / lendepth
    
    sumdepth=sum(sdat$LON[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
                 *sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    lendepth=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    mLON =sumdepth / lendepth
    
    
    sumdepth=sum(sdat$GASDIST[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
                 *sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    lendepth=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    mGASDIST =sumdepth / lendepth
    
    sumdepth=sum(sdat$GDTOC[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]] 
                 *sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    lendepth=sum(sdat$PLOTWT[sdat$YEAR==j & sdat$SVSPP==sps$SVSPP[i]])
    mGDTOC =sumdepth / lendepth
    
    
    out_data[row_c,1]=j
    out_data[row_c,2]=sps$SVSPP[i]
    out_data[row_c,3]=mMISDEPTH
    out_data[row_c,4]=mLAT
    out_data[row_c,5]=mLON
    out_data[row_c,6]=mGASDIST
    out_data[row_c,7]=mGDTOC
    
    
    
  }  
}


#outline=paste(j,",",sps$SVSPP[i],",",mMISDEPTH,",",mLAT,",",mLON,",",mGASDIST,",",mGDTOC)
#write.table(outline,file=outfile,row.name=F,col.names=F,append=TRUE)

out_data=data.frame(out_data)



names(out_data)[names(out_data)=="X1"] <- "YR"
names(out_data)[names(out_data)=="X2"] <- "SP"
names(out_data)[names(out_data)=="X3"] <- "DEPTH"
names(out_data)[names(out_data)=="X4"] <- "LAT"
names(out_data)[names(out_data)=="X5"] <- "LON"
names(out_data)[names(out_data)=="X6"] <- "ASDIST"
names(out_data)[names(out_data)=="X7"] <- "DTEOC"


write.csv(out_data,file=outfile )

#```