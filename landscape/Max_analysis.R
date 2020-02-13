##landscapes
library(rgdal)
library(rgeos)
library(spdep)
library(raster)
library(SDMTools)
library(maptools)
library(igraph)


#generate rasters (landscapes)
library(secr)
tempmask <- make.mask(spacing = 10, nx = 100, ny = 100)

landscapes<-list()

count<-1

#Percolation 0.05-0.55 de a 0.05
#Habitat occupancy A= 0.01-0.3 de a 0.01 / de 0.4, 1, 0.1

for(i in seq(0.05, 0.55, 0.05)) {

    p<-i

    for(j in c(seq(0.01, 0.3, 0.01),seq(0.4, 1, 0.1))) {

        A<-j

        landscapes[[count]]<-raster(randomHabitat(tempmask, p=p, A=A))

        count<-count+1

    }

}

plot(landscapes, covariate = "habitat", dots = FALSE,
     col = c("grey","green"), breaks = 2)

#label patches
art_conn<-lapply(landscapes, ConnCompLabel)

art_conn


#fragstats
art_fstats<-lapply(art_conn, PatchStat)

#generate polygons (tracts)

extent(landscapes[[1]])

grid <- raster(xmn=0, xmx=1000, ymn=0, ymx=1000)

res(grid)<-50

gridpolygon<-rasterToPolygons(grid)

saveRDS(gridpolygon, 'gridpolygon.rds')


#taking into account population effects
all_art2<-list()

for(i in 1:(length(art_conn))){

    all_art2[[i]]<-list()
}


#risk analysis

for(i in 1:(length(art_conn))) {

    for(j in 1:length(gridpolygon)) {

        tract<-gridpolygon[j,]



        tract_inter_p<-extract(art_conn[[i]], tract, buffer=1)

        tract_table_p<-lapply(tract_inter_p, table)

        tract_inter_a<-extract(art_conn[[i]], tract, buffer=1)

        tract_table_a<-lapply(tract_inter_a, table)



        forest_cover<-extract(art_conn[[i]], tract)

        cover_table<-lapply(forest_cover, table)



        risk1<-lapply(tract_table_p, function(x){

            patches<-as.numeric(rownames(x))

            area<-art_fstats[[i]]$area[art_fstats[[i]]$patchID%in%patches]

            p_inter<-x

            return(sum(p_inter*exp(-area)))

        })







        census<-5*5-sum(unlist(cover_table))



        tract@data$risk1<-as.numeric(risk1)*census



        tract@data$census<-census



        all_art2[[i]][[j]]<-tract



    }

}