# species density analysis

#<!--Include accompanying R code, pseudocode, flow of scripts, and/or link to location of code used in analyses.-->
#  ```{r, echo = T, message= F, warning=F, include=T, eval = T}
library(ecodata)
library(maps)
library(mapdata)
library(ks)
library(marmap)
library(raster)
library(geosphere)
library(dplyr)

#  plot ks plots

#----STUFF TO SET-----------------
data.dir <- here::here("data")
gis.dir <- here::here("gis")

# Gives most recent period. 2015-2019 input is 2016-2018 data
rminyr <- 2015
rmaxyr <- 2019

# tlevel is density of color for KD contours areas
tlevel=75  # move later
color_b="blue"
color_r="orange3"
color_r="tomato3"

# Code to read in strata and compute areas. Or read from cache.

# readin in strata.shp and compute areas of strata
# TrawlStrata <- raster::shapefile(file.path(gis.dir,"BTS_Strata.shp"))
# AREA <- geosphere::areaPolygon(TrawlStrata, r=6371000)/10^6

# for array of strata and area and make into dataframe
# stratareas <- cbind(TrawlStrata@data$STRATA, AREA)
# colnames(stratareas) <- c("STRATA","AREA")
# stratareas <- data.frame(stratareas)
load(file.path(data.dir, "StratAreas.Rdata"))

# Query bathymetry or load from cache
# getNOAA.bathy(lon1 = -77, lon2 = -65, lat1 = 35, lat2 = 45,
#               resolution = 10) -> nesbath
load(file.path(data.dir, "nesbath.Rdata"))

#Load raw survey data
load(file.path(data.dir, "Survdat.RData"))

# MUST run addTrans function
# color transparency
addTrans <- function(color,trans){
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),
                                          length(trans))==1)) 
    stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}

plot_kd <- function(species, season, exclude_years){
  
  # stata to use
  # offshore strata to use
  CoreOffshoreStrata <- c(seq(1010,1300,10),1340, seq(1360,1400,10),seq(1610,1760,10))
  
  # inshore strata to use, still sampled by Bigelow
  CoreInshore73to12 <- c(3020, 3050, 3080 ,3110 ,3140 ,3170, 3200, 3230,
                         3260, 3290, 3320, 3350 ,3380, 3410 ,3440)
  # combine
  strata_used <- c(CoreOffshoreStrata,CoreInshore73to12)
  
  survdat <- survdat %>%
    dplyr::select(c(CRUISE6,STATION,STRATUM,SVSPP,YEAR,
                    SEASON,LAT,LON,ABUNDANCE,BIOMASS)) %>% 
    filter(SEASON == season,
           STRATUM %in% strata_used) %>% # delete record form non-core 
    #strata and get unique records,
    # should be one per species
    distinct() %>% 
    # add field with rounded BIOMASS scaler used to adjust distributions
    mutate(LOGBIO = round(log10(BIOMASS * 10+10)))
  
  # trim the data....to prepare to find stations only
  survdat_stations <- survdat %>% 
    dplyr::select(CRUISE6, STATION, STRATUM, YEAR) %>% 
    distinct()
  
  # make table of strata by year
  numtowsstratyr <- table(survdat_stations$STRATUM,survdat_stations$YEAR)
  
  # find records to keep based on core strata
  rectokeep <- stratareas$STRATA %in% strata_used
  
  # add rec to keep to survdat
  stratareas <- cbind(stratareas,rectokeep)
  
  # delete record form non-core strata
  stratareas_usedonly <- stratareas[!stratareas$rectokeep=="FALSE",]
  
  areapertow=numtowsstratyr
  
  #compute area covered per tow per strata per year
  for(i in 1:50){
    areapertow[,i]=stratareas_usedonly$AREA/numtowsstratyr[,i]
  }
  
  # change inf to NA and round and out in DF
  areapertow[][is.infinite(areapertow[])]=NA
  areapertow=round(areapertow)
  areapertow=data.frame(areapertow)
  colnames(areapertow) <- c("STRATUM","YEAR","AREAWT")
  areapertow$STRATUM <- as.numeric(as.character(areapertow$STRATUM))
  areapertow$YEAR <- as.numeric(as.character(areapertow$YEAR))
  
  survdat <- survdat %>% 
    inner_join(.,areapertow, by= c("STRATUM","YEAR")) %>% 
    dplyr::rename(AREAPERTOW = AREAWT)
  
  # add col to survdat for PLOTWT
  survdat$PLOTWT <- NA
  survdat$PLOTWT <- ceiling(survdat$AREAPERTOW/1000*survdat$LOGBIO/9)
  
  if (!is.null(exclude_years)){
    sdat <- survdat %>% filter(!YEAR %in% exclude_years)
  } else {
    sdat <- survdat
  }
  
  # read species list
  sps <- ecodata::species_groupings %>% filter(!is.na(SVSPP)) %>% 
    dplyr::select(COMNAME, SVSPP)
  sps <- sps[!duplicated(sps),]
  numsps <- nrow(sps)
  
  # graph par
  par(mar = c(0,0,0,0))
  par(oma = c(0,0,0,0))
  
  # index 1:numsps, or by species record number for one species, i.e.25:25
  tspe <- sps %>% filter(COMNAME == species)
  
  # start map
  map("worldHires", xlim=c(-77,-65),ylim=c(35,45), fill=T,border=0,col="gray")
  map.axes()
  
  plot(nesbath,deep=-200, shallow=-200, step=1,add=T,lwd=1,col="gray50",lty=2)
  
  
  # for base period, 1970 to 1979, find call lons for species and by biomass weighting
  minyr=1969;maxyr=1980
  clons1 = 
    sdat$LON[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==1)]
  clons2 = 
    sdat$LON[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==2)]
  clons3 = 
    sdat$LON[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==3)]
  clons4 = 
    sdat$LON[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  clons5 = 
    sdat$LON[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  
  # get rid of missings, KS does not like
  clons1 <- na.omit(clons1)
  clons2 <- na.omit(clons2)
  clons3 <- na.omit(clons3)
  clons4 <- na.omit(clons4)
  clons5 <- na.omit(clons5)
  
  # accumulate all lons, repeating for weighting 
  clons=c(clons1,clons2,clons2,clons3,clons3,clons3,clons4,clons4,clons4,clons4,
          clons5,clons5,clons5,clons5,clons5)
  
  # same for lats
  clats1 = 
    sdat$LAT[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==1)]
  clats2 = 
    sdat$LAT[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==2)]
  clats3 = 
    sdat$LAT[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==3)]
  clats4 = 
    sdat$LAT[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  clats5 = 
    sdat$LAT[(sdat$YEAR>minyr & sdat$YEAR<maxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==5)]
  clats1 <- na.omit(clats1)
  clats2 <- na.omit(clats2)
  clats3 <- na.omit(clats3)
  clats4 <- na.omit(clats4)
  clats5 <- na.omit(clats5)
  clats=c(clats1,clats2,clats2,clats3,clats3,clats3,clats4,clats4,clats4,clats4,
          clats5,clats5,clats5,clats5,clats5)
  
  # combine lons and lats
  x=cbind(clons,clats)
  # compute KD using KS routine
  Hscv1 <- Hscv.diag(x=x)
  #fhat.pi1 <- kde(x=x, H=Hscv1)
  
  fhat.pi1 <- kde(x, compute.cont=T, binned=F, 
                  xmin=c(-77, 35), xmax=c(-65, 45)) 
  # specify grid to match raster stack of OISST... etc.
  
  # add to plot each probability separately
  contour.25 <- with(fhat.pi1, 
                     contourLines(x=eval.points[[1]],y=eval.points[[2]], 
                                  z=estimate,levels=cont["25%"]))
  contour.50 <- with(fhat.pi1, 
                     contourLines(x=eval.points[[1]],y=eval.points[[2]], 
                                  z=estimate,levels=cont["50%"]))
  contour.75 <- with(fhat.pi1, 
                     contourLines(x=eval.points[[1]],y=eval.points[[2]], 
                                  z=estimate,levels=cont["75%"]))
  
  for (j in 1:length(contour.75)){
    polygon(unlist(contour.75[[j]][2]), unlist(contour.75[[j]][3]),
            col=addTrans(color_b,tlevel), border=F)
  }
  for (j in 1:length(contour.50)){
    polygon(unlist(contour.50[[j]][2]), unlist(contour.50[[j]][3]),
            col=addTrans(color_b,tlevel), border=F)
  }
  for (j in 1:length(contour.25)){
    polygon(unlist(contour.25[[j]][2]), unlist(contour.25[[j]][3]),
            col=addTrans(color_b,tlevel), border=F)
  }
  
  clons1 = 
    sdat$LON[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==1)]
  clons2 = 
    sdat$LON[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==2)]
  clons3 = 
    sdat$LON[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==3)]
  clons4 = 
    sdat$LON[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  clons5 = 
    sdat$LON[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  # get rid of missings, KS does not like
  clons1 <- na.omit(clons1)
  clons2 <- na.omit(clons2)
  clons3 <- na.omit(clons3)
  clons4 <- na.omit(clons4)
  clons5 <- na.omit(clons5)
  # accumulate all lons, repeating for weighting 
  clons=c(clons1,clons2,clons2,clons3,clons3,clons3,clons4,clons4,clons4,clons4,
          clons5,clons5,clons5,clons5,clons5)
  
  # same for lats
  clats1 = 
    sdat$LAT[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==1)]
  clats2 = 
    sdat$LAT[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==2)]
  clats3 = 
    sdat$LAT[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==3)]
  clats4 = 
    sdat$LAT[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==4)]
  clats5 = 
    sdat$LAT[(sdat$YEAR>rminyr & sdat$YEAR<rmaxyr & sdat$SVSPP==tspe$SVSPP & sdat$PLOTWT ==5)]
  clats1 <- na.omit(clats1)
  clats2 <- na.omit(clats2)
  clats3 <- na.omit(clats3)
  clats4 <- na.omit(clats4)
  clats5 <- na.omit(clats5)
  clats=c(clats1,clats2,clats2,clats3,clats3,clats3,clats4,clats4,clats4,clats4,
          clats5,clats5,clats5,clats5,clats5)
  
  x=cbind(clons,clats)
  Hscv2 <- Hscv.diag(x=x)
  
  
  #fhat.pi2 <- kde(x=x, H=Hscv2)
  fhat.pi2 <- kde(x, compute.cont=T, 
                  binned=F, xmin=c(-77, 35), xmax=c(-65, 45)) 
  # specify grid to match raster stack of OISST... etc.
  
  # add to plot each probability separately
  contour.25 <-
    with(fhat.pi2, contourLines(x=eval.points[[1]],y=eval.points[[2]], 
                                z=estimate,levels=cont["25%"]))
  contour.50 <- 
    with(fhat.pi2,contourLines(x=eval.points[[1]],y=eval.points[[2]],
                               z=estimate,levels=cont["50%"]))
  contour.75 <-
    with(fhat.pi2, contourLines(x=eval.points[[1]],y=eval.points[[2]], 
                                z=estimate,levels=cont["75%"]))
  
  for (j in 1:length(contour.75)){
    polygon(unlist(contour.75[[j]][2]), unlist(contour.75[[j]][3]),
            col=addTrans(color_r,tlevel), border=F)
  }
  for (j in 1:length(contour.50)){
    polygon(unlist(contour.50[[j]][2]), unlist(contour.50[[j]][3]),
            col=addTrans(color_r,tlevel), border=F)
  }
  for (j in 1:length(contour.25)){
    polygon(unlist(contour.25[[j]][2]), unlist(contour.25[[j]][3])
            ,col=addTrans(color_r,tlevel), border=F)
  }
  
  
  text(-70,37.5, pos=4,labels = species)
  segments(-69.5, 37,-68.5, 37,lwd=1,col="gray50",lty=2)
  text(-68.5,37, pos=4,labels = "200m")
  
  stline=36.5
  text(-70.4,stline, pos=4,labels = "25%    50%    75%")
  incline=-.3
  segments(-70, stline+incline,-69,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  segments(-70, stline+incline,-68,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  segments(-70, stline+incline,-67,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  text(-66.8,stline+incline, pos=4,labels = "Base")
  
  incline=-.7
  segments(-70, stline+incline,-69,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  segments(-70, stline+incline,-68,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  segments(-70, stline+incline,-67,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  text(-66.8,stline+incline, pos=4,labels = "Recent")
  
  incline=-1.1 
  segments(-70, stline+incline,-69,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  segments(-70, stline+incline,-68,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  segments(-70, stline+incline,-67,
           stline+incline ,lwd=20,col=addTrans(color_b,tlevel))
  segments(-70, stline+incline,-69,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  segments(-70, stline+incline,-68,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  segments(-70, stline+incline,-67,
           stline+incline ,lwd=20,col=addTrans(color_r,tlevel))
  text(-66.8,stline+incline, pos=4,labels = "Overlap")
  
} 


#```