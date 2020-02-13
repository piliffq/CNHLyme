rm(list=ls(all=TRUE))

#generate rasters (landscapes)
library(secr)
tempmask <- make.mask(spacing = 10, nx = 100, ny = 100)

#define variables
count<-1
p <-0
A <-0

#generate dataset to save this information for reference
landscape_ref<-cbind(count, p, A)

#path to save rasters
outpath <- "C:/Users/mpf2131/Dropbox/8- Scripts R general/CNHLyme/landscape/rasters/"
dir.create(outpath)

#plot arrangement
#par(mfrow = c(3, 3))

#function to generate file name using count (iteration)
outfiles <- function (count) {

            name <- paste0(outpath,count)

            return(name)}

outfiles(count)

#generate landscapes:
#Percolation 0.05-0.55, every 0.05
#Habitat occupancy A= 0.01-0.3, every 0.01 and 0.4-1, every 0.1
library(raster)
for(i in seq(0.05, 0.55, 0.1)) {
    p<-i

    for (j in c(seq(0.01, 0.3, 0.01),seq(0.4, 1, 0.1))) {

        A<-j

        #generate landscape
        landscape <- randomHabitat(tempmask, p=p, A=A, drop= FALSE, covname = "habitat")

        #plot(landscape, covariate = "habitat", dots = FALSE,
        #     col = c("grey","green"), breaks = 2)

        #convert to raster
        landscape <-raster(landscape, "habitat", values=1)

        #write raster as tif and ascii format
        writeRaster(landscape, outfiles(count), format="GTiff", overwrite=T)
        writeRaster(landscape, outfiles(count), format="ascii", overwrite=T)

        #create p and A reference for this iteration (count)
        Ref <-c(count, p, A)
        landscape_ref <- rbind(landscape_ref, Ref)

        #update count
        count<-count+1
    }

}

#remove first line in ref file and write a csv file.
landscape_ref <- as.data.frame(landscape_ref[-1,])
write.csv(landscape_ref, "landscape/rasters/landscape_ref.csv", row.names=F)
