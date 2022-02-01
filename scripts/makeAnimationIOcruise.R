############################################################################################
# Join daily images to make a short animation
############################################################################################

library(gifski)
library(lubridate)
library(stringr)

# Where to look for the maps to animate
mapdir <- "./plots" 

# An example SLA animation
# First get all sla images in /plots
slaFiles <- list.files(path = mapdir, pattern = "sla", full.names = TRUE, recursive = TRUE)
slaFiles <- slaFiles[grepl(".png", slaFiles)]
fileInfo <- data.frame("name" = slaFiles)
# Sort the images by name (date), so can select most recent to animate
fileInfo$dt <- fileInfo$name
fileInfo$dt <- str_sub(fileInfo$dt, start = -14)
fileInfo$dt <- gsub(".png", "", fileInfo$dt)
fileInfo$dt <- gsub("_", "-", fileInfo$dt)
fileInfo$dt <- as.Date(fileInfo$dt, format = "%Y-%m-%d") 
fileInfoSorted <- fileInfo[order(fileInfo$dt, decreasing = TRUE), ]
# Subset down to just last (e.g.) 5-7 days
fileInfoRecent <- fileInfoSorted[1:7,]
# Resort ascending so gif runs in the right order
fileInfoRecent <- fileInfoRecent[order(fileInfoRecent$dt, decreasing = FALSE), ]
slaFilesAnim <- c(fileInfoRecent$name)

# Extract dates covered so can use in output filename
st <- min(fileInfoRecent$dt)
en <- max(fileInfoRecent$dt)
# Make and save the animation
# I am saving them to a subfolder of ./plots called "animations"
yr <- year(datToExtract)
mo <- month(datToExtract)
da <- day(datToExtract)
if(!dir.exists(paste0(mapdir, "/animations/", as.numeric(mo), "_", da, "_", yr))) {
   dir.create(paste0(mapdir, "/animations/", as.numeric(mo), "_", da, "_", yr))
}
gifski(slaFilesAnim, gif_file = paste0(mapdir, "/animations/", month(en), "_", day(en), "_", year(en), "/",
                                       "sla", st, "_to_", en, ".gif"), 
       width = 800, height = 600, delay = 1)

##################################################################################################
# Larval habitat animation
# First get all  images in /plots
larvaeFiles <- list.files(path = mapdir, pattern = "larvae", full.names = TRUE, recursive = TRUE)
larvaeFiles <- larvaeFiles[grepl(".png", larvaeFiles)]
fileInfoLarvae <- data.frame("name" = larvaeFiles)
# Sort the images by name (date), so can select most recent to animate
fileInfoLarvae$dt <- fileInfoLarvae$name
fileInfoLarvae$dt <- str_sub(fileInfoLarvae$dt, start = -14)
fileInfoLarvae$dt <- gsub(".png", "", fileInfoLarvae$dt)
fileInfoLarvae$dt <- gsub("_", "-", fileInfoLarvae$dt)
fileInfoLarvae$dt <- as.Date(fileInfoLarvae$dt, format = "%Y-%m-%d")
fileInfoLarvaeSorted <- fileInfoLarvae[order(fileInfoLarvae$dt, decreasing = TRUE), ]
# Subset down to just last (e.g.) 5-7 days
fileInfoLarvaeRecent <- fileInfoLarvaeSorted[1:7,]
# Resort ascending so gif runs in the right order
fileInfoLarvaeRecent <- fileInfoLarvaeRecent[order(fileInfoLarvaeRecent$dt, decreasing = FALSE), ]
larvaeFilesAnim <- c(fileInfoLarvaeRecent$name)

# Extract dates covered so can use in output filename
stLarvae <- min(fileInfoLarvaeRecent$dt)
enLarvae <- max(fileInfoLarvaeRecent$dt)
# Make and save the animation
# I am saving them to a subfolder of ./plots called "animations"
gifski(larvaeFilesAnim, gif_file = paste0(mapdir, "/animations/", month(enLarvae), "_", day(enLarvae), "_", 
                                          year(enLarvae), "/", "larvae", stLarvae, "_to_", enLarvae, ".gif"), 
       width = 800, height = 600, delay = 1)

##################################################################################################
# SST animation
# First get all  images in /plots
sstFiles <- list.files(path = mapdir, pattern = "sst", full.names = TRUE, recursive = TRUE)
sstFiles <- sstFiles[grepl(".png", sstFiles)]
fileInfosst <- data.frame("name" = sstFiles)
# Sort the images by name (date), so can select most recent to animate
fileInfosst$dt <- fileInfosst$name
fileInfosst$dt <- str_sub(fileInfosst$dt, start = -14)
fileInfosst$dt <- gsub(".png", "", fileInfosst$dt)
fileInfosst$dt <- gsub("_", "-", fileInfosst$dt)
fileInfosst$dt <- as.Date(fileInfosst$dt, format = "%Y-%m-%d")
fileInfosstSorted <- fileInfosst[order(fileInfosst$dt, decreasing = TRUE), ]
# Subset down to just last (e.g.) 5-7 days
fileInfosstRecent <- fileInfosstSorted[1:7,]
# Resort ascending so gif runs in the right order
fileInfosstRecent <- fileInfosstRecent[order(fileInfosstRecent$dt, decreasing = FALSE), ]
sstFilesAnim <- c(fileInfosstRecent$name)

# Extract dates covered so can use in output filename
stsst <- min(fileInfosstRecent$dt)
ensst <- max(fileInfosstRecent$dt)
# Make and save the animation
# I am saving them to a subfolder of ./plots called "animations"
gifski(sstFilesAnim, gif_file = paste0(mapdir, "/animations/", month(ensst), "_", day(ensst), "_", year(ensst), "/",
                                       "sst", stsst, "_to_", ensst, ".gif"), 
       width = 800, height = 600, delay = 1)
