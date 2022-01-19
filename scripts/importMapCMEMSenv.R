#######################################################################################################
# Download daily imagery from CMEMS, map for southern bluefin study region, and export
# acquire_cmems adapted from code written by Heather Welch (UCSC/NOAA-ERD)
# Questions or comments: Barbara.Muhling@noaa.gov
#######################################################################################################

library(ncdf4)
library(ggplot2)
library(reshape2)
library(lubridate)
library(raster)
library(dplyr)
library(RCurl)
library(mgcv)
library(rgdal)
library(tools)

################################################################################################################
############################################ Specify directories ###############################################
################################################################################################################
# Specify where you want the downloaded environmental files to be saved
tmpdir <- "./env" 
# Specify where to look for the ship's daily locations, the larval BFT GAM, and the EEZ shapefile
datadir <- "./data" 
# Specify where to save the output maps
mapdir <- "./plots"

################################################################################################################
############# Load the other objects you will need: EEZ for mapping, larval GAM ################################
################################################################################################################
eez <- readRDS(paste0(datadir, "/eez.rds"))
# Load the GAM predicting probability of occurrence of ABFT in the Gulf of Mexico
load(paste0(datadir, "/gomgam.rda")) 
# Examine model summary or partial plots if you like
# summary(gam1)
# plot(gam1, scale = 0, pages = 1)

################################################################################################################
################################ Set a date to extract variables for ###########################################
################################################################################################################
# Today's date
dat <- Sys.Date() 
# Most products have a 1 day delay, so we will extract for yesterday 
datToExtract <- dat - days(1)  
# Or you can manually set another date if you want
# datToExtract <- as.Date("2022-01-07")

# Re-load the csv with the ship's position (assuming this is manually updated regularly from 
# https://mfp.us/programme/mappage or AIS)
ship <- read.csv(paste0(datadir, "/revelleLocation.csv")) # assuming columns named lon, lat, and date in m/d/YYYY
ship$date <- as.Date(ship$date, format = "%m/%d/%Y")

################################################################################################################
#################### Define the function below, then run it for your date ######################################
################################################################################################################
makeMaps(datToExtract = datToExtract, saveMaps = "yes", add80sLarvae = "yes",
         tmpdir = tmpdir, datadir = datadir, mapdir = mapdir)

# # Or can run for multiple dates
# dtsToExtract <- seq(as.Date("2022-01-02"), by = "day", length = 13)
# for(j in 1:length(dtsToExtract)) {
#   makeMaps(datToExtract = dtsToExtract[j], saveMaps = "yes", tmpdir = tmpdir, datadir = datadir, mapdir = mapdir)
# }

################################################################################################################
###################################### Here is the function ####################################################
################################################################################################################
makeMaps <- function(datToExtract, saveMaps, add80sLarvae, tmpdir, datadir, mapdir) { 
  yr <- year(datToExtract)
  mo <- month(datToExtract)
  da <- day(datToExtract)
  # Month must be in 2-digit form
  if(nchar(mo) == 1) {
    mo <- paste0(0, mo)
  }
  # We need credentials to get data from CMEMS. You can use mine or create your own (free) account if you like
  userpwd <- "bmuhling:BarbaraCMEMS2017"

  #######################################################################################################
  # Define extraction functions:
  # waitfor pauses system for a period of time to allow url requests to go through
  waitfor <- function(x){
    p1 <- proc.time()
    Sys.sleep(x)
    print(proc.time() - p1) # The cpu usage should be negligible
  }
  
  # acquire_cmems downloads global values for a specified date
  acquire_cmems = function(url, date, userpwd, name, tmpdir){ 
    filenames = getURL(url, userpwd = userpwd, # the CMEMS files are named by the date they were uploaded 
                       ftp.use.epsv = FALSE, ssl.verifypeer = FALSE, dirlistonly = TRUE) 
    waitfor(3)
    list_filenames = unlist(strsplit(filenames, ".nc")) # get a list of all the files in the CMEMS directory
    string = grep(date, list_filenames, value = TRUE)
    string <- gsub("\r\n", "", string) # remove strange leader to fnames
    if(length(string) > 0){
      #string = gsub("[^[:alnum:]_.]","", string) ## for trailing backslashes, not needed for this product
      data = getBinaryURL(paste(url, string, ".nc", sep = ""), userpwd = userpwd,ftp.use.epsv = FALSE, 
                          ssl.verifypeer = FALSE, noprogress = FALSE) # grab data behind url
      waitfor(3)
      con <- file(paste(tmpdir, "/", name, ".nc", sep = ""), open = "wb") # write data to a file
      writeBin(data, con)
      waitfor(3)
      close(con)
    }
  }
  
  #######################################################################################################
  # Download SST. Using Global Ocean OSTIA Sea Surface Temperature and Sea Ice Analysis
  # (https://resources.marine.copernicus.eu/product-detail/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_001/INFORMATION)
  # Build base of url
  urlsst <- paste0("ftp://nrt.cmems-du.eu/Core/SST_GLO_SST_L4_NRT_OBSERVATIONS_010_", 
                   "001/METOFFICE-GLO-SST-L4-NRT-OBS-SST-V2/", yr, "/", mo, "/")
  datesst = paste(gsub("-", "", datToExtract), sep = "") 
  namesst = paste0("sst_", yr, "_", mo, "_", day(datToExtract)) # For output fname
  # Download the data. SST file is about 15mb
  acquire_cmems(url = urlsst, date = datesst, userpwd = userpwd, name = namesst, tmpdir = tmpdir)
  
  #######################################################################################################
  # Download chlorophyll. Using Copernicus-GlobColour
  # (https://resources.marine.copernicus.eu/product-detail/OCEANCOLOUR_GLO_CHL_L4_NRT_OBSERVATIONS_009_033/INFORMATION)
  # Build base of url
  urlchl <- paste0("ftp://nrt.cmems-du.eu/Core/OCEANCOLOUR_GLO_CHL_L4_NRT_OBSERVATIONS_009_", 
                   "033/dataset-oc-glo-bio-multi-l4-chl_interpolated_4km_daily-rt/", yr, "/", mo, "/")
  datechl = paste(gsub("-", "", datToExtract), sep = "") 
  namechl = paste0("chl_", yr, "_", mo, "_", day(datToExtract)) # For output fname
  # Download the data. CHL file is about 92mb, so will take a minute
  acquire_cmems(url = urlchl, date = datechl, userpwd = userpwd, name = namechl, tmpdir = tmpdir)
  
  #######################################################################################################
  # Download sea surface height anomaly. Using gridded altimetry observations from satellite
  # (https://resources.marine.copernicus.eu/product-detail/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_046/INFORMATION)
  # Build base of url
  urlsla <- paste0("ftp://nrt.cmems-du.eu/Core/SEALEVEL_GLO_PHY_L4_NRT_OBSERVATIONS_008_", 
                   "046/dataset-duacs-nrt-global-merged-allsat-phy-l4","/", yr, "/", mo, "/")
  datesla = paste("l4_", gsub("-", "", datToExtract), sep = "")
  namesla = paste0("sla_", yr, "_", mo, "_", day(datToExtract)) # For output fname
  # Download the data. SLA file is about 9mb
  acquire_cmems(url = urlsla, date = datesla, userpwd = userpwd, name = namesla, tmpdir = tmpdir)
  
  ######################################################################################################
  # Import each variable from folder so it can be mapped. We can use a function to speed things up
  importnc <- function(fname, varName, dateToMap, tmpdir) {
    ncFile <- nc_open(paste0(tmpdir, "/", fname, "_", yr, "_", mo, "_", da, ".nc"))
    if(varName == "sla") {
      lon <- ncvar_get(ncFile, "longitude")
      lat <- ncvar_get(ncFile, "latitude")
      var <- ncvar_get(ncFile, "sla")
    } else if(varName == "ugos") {
       lon <- ncvar_get(ncFile, "longitude")
       lat <- ncvar_get(ncFile, "latitude")
       var <- ncvar_get(ncFile, "ugos")
    } else if(varName == "vgos") {
       lon <- ncvar_get(ncFile, "longitude")
       lat <- ncvar_get(ncFile, "latitude")
       var <- ncvar_get(ncFile, "vgos")
    } else {
      lon <- ncvar_get(ncFile, "lon")
      lat <- ncvar_get(ncFile, "lat")
      if(varName == "sst"){
        var <- ncvar_get(ncFile, "analysed_sst")
      } else if(varName == "chl") {
        var <- ncvar_get(ncFile, "CHL") 
      }
    }
    dimnames(var) <- list(lon = lon, lat = lat)
    varMelt <- reshape2::melt(var, value.name = varName)
    if(varName == "sst") {
      varMelt$sst <- varMelt$sst - 273.15 # convert from Kelvin to Celcius
    }
    # Trim from global extent down to NE Indian Ocean. You can adjust these limits if you like
    varTrim <- subset(varMelt, lon > 100 & lon < 130 & lat > -25 & lat < -5)
    varTrim$dt <- dateToMap
    nc_close(ncFile)
    return(varTrim)
  }
  
  ########################################################################################################
  # Provide a variable name and a date to the function to get a long-format dataframe suitable for mapping
  # They're all different lengths because each product has a different spatial resolution
  # Note that ugos and vgos are just geostrophic currents, finer-scale eddies/features won't be visible
  sst <- importnc(fname = "sst", varName = "sst", dateToMap = datToExtract, tmpdir = tmpdir)
  chl <- importnc(fname = "chl", varName = "chl", dateToMap = datToExtract, tmpdir = tmpdir)
  sla <- importnc(fname = "sla", varName = "sla", dateToMap = datToExtract, tmpdir = tmpdir)
  ugos <- importnc(fname = "sla", varName = "ugos", dateToMap = datToExtract, tmpdir = tmpdir)
  vgos <- importnc(fname = "sla", varName = "vgos", dateToMap = datToExtract, tmpdir = tmpdir)
  # Combine geostrophic currents in one df
  geos <- cbind(ugos, "vgos" = vgos$vgos)
  # Coarsen to 0.5x0.5 so easier to see on map
  geos$lonrd <- (round(geos$lon * 20, -1)) / 20
  geos$latrd <- (round(geos$lat * 20, -1)) / 20
  geosAgg <- aggregate(cbind(ugos, vgos) ~ lonrd + latrd, geos, FUN = mean, na.rm = TRUE)
  
  ########################################################################################################
  # Now let's map each variable: first define a coastline and some color scales
  oicoast <- borders("world", colour = "gray50", fill="gray50", xlim = c(100, 130), ylim = c(-30, 0))
  mycolsst <- colors()[c(473,562,71,610,655,653,621,34)]
  mypalettesst <- colorRampPalette(mycolsst)(255)
  mycolsla <- colors() [c(490,461,564,430,1,652,585,555,645)]
  mypalettesla <- colorRampPalette(mycolsla)(255)
  mycolchl <- colors()[c(473,562,411,259,258,146)]
  mypalettechl <- colorRampPalette(mycolchl)(255)
  
  # Now define the ship's position on the day the environmental vars were extracted, plus the past track
  # Also remove any future ship locations (if you're rerunning a date from a little while ago)
  ship$locn <- ifelse(ship$date == datToExtract, "today", ifelse(ship$date < datToExtract, "past", "future"))
  ship <- subset(ship, locn != "future")
  
  # Add location of sbt larval collections back in the 1980s
  oldsbt <- data.frame("lon" = 115.8333, "lat" = -16.5)
  # Now map: SST
  sstMap <- ggplot(sst) + geom_tile(aes(x = lon, y = lat, fill = sst)) + 
    scale_fill_gradientn(colours = mypalettesst, limits = c(), na.value = NA) + # can manually set limits if wanted
    geom_path(data = ship, aes(x = lon, y = lat)) +
    geom_point(data = ship, aes(x = lon, y = lat, color = locn), size = 2) + 
    scale_color_manual("ship location", values = c("magenta", "black")) +
    geom_path(data = eez, aes(x = long, y = lat, group = group)) +
    # If you don't want to show the old 1987 larval catch locations, comment out the next 3 lines
    # geom_point(data = oldsbt, aes(x = lon, y = lat, shape = 1, size = 1), pch = 23, fill = "red") +
    # scale_size_continuous("1987 larvae", range = 4, labels = NULL) +
    # guides(size = guide_legend(order = 1), color = guide_legend(order = 2)) +
    oicoast + ggtitle(paste0(sst$dt[1], " Sea Surface Temperature")) + xlab("Longitude") + ylab("Latitude") +
    coord_quickmap(xlim = c(100, 130), ylim = c(-25, -5)) + theme_bw()
  # sstMap
  
  # CHL. Note 4th root transform so is easier to see gradients
  chlMap <- ggplot(chl) + geom_tile(aes(x = lon, y = lat, fill = chl^0.25)) + # Note chl transform 
    scale_fill_gradientn(colours = mypalettechl, limits = c(), na.value = NA) + 
    geom_path(data = ship, aes(x = lon, y = lat)) +
    geom_point(data = ship, aes(x = lon, y = lat, color = locn), size = 2) + 
    scale_color_manual("ship location", values = c("magenta", "black")) +
    geom_path(data = eez, aes(x = long, y = lat, group = group)) +
    oicoast + ggtitle(paste0(chl$dt[1], " Sea Surface Chlorophyll")) + xlab("Longitude") + ylab("Latitude") +
    coord_quickmap(xlim = c(100, 130), ylim = c(-25, -5)) + theme_bw()
  # chlMap
  
  # SLA. This product is coarser than sst or chl (0.25x0.25 degrees), which is why it looks chonky
  # Multiplying ugos/vgos by 2 just to make the arrows look clearer on the map. You can play with changing this value
  # I'm fixing the scale limits so animations are consistent
  slaMap <- ggplot() + geom_tile(data = sla, aes(x = lon, y = lat, fill = sla)) + 
    scale_fill_gradientn(colours = mypalettesla, limits = c(-0.25, 0.5), na.value = NA) + 
    geom_path(data = ship, aes(x = lon, y = lat)) +
    geom_point(data = ship, aes(x = lon, y = lat, color = locn), size = 2) + 
    scale_color_manual("ship location", values = c("magenta", "black")) +
    geom_path(data = eez, aes(x = long, y = lat, group = group)) +
    geom_segment(data = geosAgg, aes(x = lonrd, y = latrd, xend = lonrd + (ugos * 2), yend = latrd + (vgos * 2)), 
                 arrow = arrow(length = unit(0.1, "cm")), na.rm = TRUE) +
    oicoast + ggtitle(paste0(sla$dt[1], " Sea Surface Height Anomaly Plus Geostrophic Currents")) + 
    xlab("Longitude") + ylab("Latitude") +
    coord_quickmap(xlim = c(100, 130), ylim = c(-25, -5)) + theme_bw()
  # slaMap
  
  ######################################################################################################
  # We can map predictions from a species distribution model trained on larval occurrences from the GOM
  # Model is a simple binomial GAM using SST, chl (4th root), and sea level anomaly (sla)
  # Take with a large grain of salt! We don't know if SBFT spawning habitat = ABFT spawning habitat
  # Combine SST, chl, and sla at 0.25x0.25 resolution so we can score them through the GAM
  sst$lonrd <- (round(sst$lon * 4, 0)) / 4
  sst$latrd <- (round(sst$lat * 4, 0)) / 4
  sstAgg <- aggregate(sst ~ lonrd + latrd, sst, FUN = mean, na.rm = TRUE)
  chl$lonrd <- (round(chl$lon * 4, 0)) / 4
  chl$latrd <- (round(chl$lat * 4, 0)) / 4
  chlAgg <- aggregate(chl ~ lonrd + latrd, chl, FUN = mean, na.rm = TRUE)
  chlAgg$chl4th <- chlAgg$chl ^ 0.25
  sla$lonrd <- sla$lon - 0.125
  sla$latrd <- sla$lat - 0.125
  slaAgg <- aggregate(sla ~ lonrd + latrd, sla, FUN = mean, na.rm = TRUE)
  toScore <- inner_join(sstAgg, chlAgg, by = c("lonrd", "latrd"))
  toScore <- inner_join(toScore, slaAgg, by = c("lonrd", "latrd"))
  toScore$pred <- predict(gam1, toScore, type = "response")
  # Set a flag for pixels with environmental conditions not seen in the GOM
  # (We assume predictions will be much more uncertain here)
  toScore$outOfRange <- ifelse(toScore$sst > 30.3 | toScore$sst < 21 | 
                                 toScore$chl4th > 1.1 | toScore$chl4th < 0.45 |
                                 toScore$sla > 0.7 | toScore$sla < -0.4, 1, 0)
  # Now map. Note optional "add blank" line, which masks environments outside GOM training data 
  larvalMap <- ggplot(toScore) + geom_tile(aes(x = lonrd, y = latrd, fill = pred)) + 
    scale_fill_gradientn(colours = mypalettesst, limits = c(0,1), na.value = NA) + 
    geom_tile(data = subset(toScore, outOfRange == 1), aes(x = lonrd, y = latrd), fill = "white") + # add blank
    geom_path(data = ship, aes(x = lon, y = lat)) +
    geom_point(data = ship, aes(x = lon, y = lat, color = locn), size = 2) + 
    scale_color_manual("ship location", values = c("magenta", "black")) +
    geom_path(data = eez, aes(x = long, y = lat, group = group)) +
    oicoast + ggtitle(paste0(datToExtract, " Potential Larval Habitat")) + xlab("Longitude") + ylab("Latitude") +
    coord_quickmap(xlim = c(100, 130), ylim = c(-25, -5)) + theme_bw()
  # larvalMap
  
  # If desired, add general location of 1987 larval occurrences to all maps
  if(add80sLarvae == "yes") {
    sstMap <- sstMap + geom_point(data = oldsbt, aes(x = lon, y = lat, shape = 1, size = 1), pch = 23, fill = "red") +
      scale_size_continuous("1987 larvae", range = 4, labels = NULL) +
      guides(size = guide_legend(order = 1), color = guide_legend(order = 2))
    chlMap <- chlMap + geom_point(data = oldsbt, aes(x = lon, y = lat, shape = 1, size = 1), pch = 23, fill = "red") +
      scale_size_continuous("1987 larvae", range = 4, labels = NULL) + 
      guides(size = guide_legend(order = 1), color = guide_legend(order = 2))
    slaMap <- slaMap + geom_point(data = oldsbt, aes(x = lon, y = lat, shape = 1, size = 1), pch = 23, fill = "red") +
      scale_size_continuous("1987 larvae", range = 4, labels = NULL) + 
      guides(size = guide_legend(order = 1), color = guide_legend(order = 2))
    larvalMap <- larvalMap + geom_point(data = oldsbt, aes(x = lon, y = lat, shape = 1, size = 1), pch = 23, fill = "red") +
      scale_size_continuous("1987 larvae", range = 4, labels = NULL) + 
      guides(size = guide_legend(order = 1), color = guide_legend(order = 2))
  }
  
  #####################################################################################################
  # Save maps as images. If files are too large, adjust "res" to be smaller
  # Doing pngs, as smaller and can animate easier. Tiff is similar call, just add compression = "lzw"
  if(saveMaps == "yes") {
  # SST
  png(filename = paste0(mapdir, "/sst_", yr, "_", mo, "_", da, ".png"), 
       width = 2600, height = 1600, res = 300, restoreConsole = TRUE)
  print(sstMap)
  dev.off()
  # CHL
  png(filename = paste0(mapdir, "/chl_", yr, "_", mo, "_", da, ".png"), 
       width = 2600, height = 1600, res = 300, restoreConsole = TRUE)
  print(chlMap)
  dev.off()
  # SLA
  png(filename = paste0(mapdir, "/sla_", yr, "_", mo, "_", da, ".png"), 
       width = 2600, height = 1600, res = 300, restoreConsole = TRUE)
  print(slaMap)
  dev.off()
  # Larval Habitat
  png(filename = paste0(mapdir, "/larvae_", yr, "_", mo, "_", da, ".png"), 
       width = 2600, height = 1600, res = 300, restoreConsole = TRUE)
  print(larvalMap)
  dev.off()
  }
}
