############################################################################################
# Join daily images to make a short animation
############################################################################################

library(gifski)

# Where to look for the maps to animate
mapdir <- "./plots" 

# An example SLA animation
# First get all sla images in /plots
slaFiles <- list.files(path = mapdir, pattern = "sla", full.names = TRUE)
fileInfo <- data.frame("name" = slaFiles)
# Sort the images by name (date), so can select most recent to animate
fileInfo$dt <- fileInfo$name
fileInfo$dt <- gsub("./plots/sla_", "", fileInfo$dt)
fileInfo$dt <- gsub(".png", "", fileInfo$dt)
fileInfo$dt <- gsub("_", "-", fileInfo$dt)
fileInfo$dt <- as.Date(fileInfo$dt, format = "%Y-%M-%d")
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
gifski(slaFilesAnim, gif_file = paste0(mapdir, "/animations/sla", st, "_to_", en, ".gif"), 
       width = 800, height = 600, delay = 1)

##################################################################################################
# Larval habitat animation
# First get all  images in /plots
larvaeFiles <- list.files(path = mapdir, pattern = "larvae", full.names = TRUE)
fileInfoLarvae <- data.frame("name" = larvaeFiles)
# Sort the images by name (date), so can select most recent to animate
fileInfoLarvae$dt <- fileInfoLarvae$name
fileInfoLarvae$dt <- gsub("./plots/larvae_", "", fileInfoLarvae$dt)
fileInfoLarvae$dt <- gsub(".png", "", fileInfoLarvae$dt)
fileInfoLarvae$dt <- gsub("_", "-", fileInfoLarvae$dt)
fileInfoLarvae$dt <- as.Date(fileInfoLarvae$dt, format = "%Y-%M-%d")
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
gifski(larvaeFilesAnim, gif_file = paste0(mapdir, "/animations/larvae", stLarvae, "_to_", enLarvae, ".gif"), 
       width = 800, height = 600, delay = 1)
