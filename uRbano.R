library(dplyr)
library(ggplot2)
library(sf)
library(maps)
library(osmdata)
library(raster)
library(data.table)
library(R.utils)

###########
#script for calculating road density (total road lengths per unit area) and building density (sum of area of rooftops per unit area)
#and generating sample locations across density levels
##########

#function to create a radius around user-specified city center
city_radius<-function(city, radius){
  cty<-subset(world.cities, world.cities$name==city)
  cty_coords<-st_as_sf(cty, coords = c("long","lat"), crs=4326)
  cty_crc<-st_buffer(cty_coords, radius)
  return(cty_crc)
}

rad<- city_radius("Temuco", 40000)

cty_ex<-extent(rad)

#get roads from open street map API
rds <- opq(bbox = c (cty_ex@xmin[[1]],cty_ex@ymin[[1]],cty_ex@xmax[[1]],cty_ex@ymax[[1]] )) %>%
  add_osm_feature(key = "highway", value=c("primary" , "motorway_link",  "unclassified" ,  "tertiary"  ,  "residential",   
                                           "motorway" , "secondary"  ,    "service" ,  "trunk" ,        
                                           "living_street" , "trunk_link" , "primary_link",     "secondary_link" ,         
                                           "tertiary_link",  "crossing"  ,     "road" )) %>% osmdata_sf()
wrds<-rds$osm_lines

### building footprints ###

###if in US use this source, need to add user input to change the state file
inUrl1  <- "https://usbuildingdata.blob.core.windows.net/usbuildings-v2/Minnesota.geojson.zip"

infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
foots<-st_read(unzip(infile1))
unlink(infile1)

sf_use_s2(FALSE) #turn off spherical geometry

blds<-st_intersection(foots$geometry,rad)
sf_use_s2(TRUE) 
#######

#Get buildings from open data - in Temuco's case there are two extents that need to be pulled
#still need to create an interface to these tiles
footdt2<-fread("https://storage.googleapis.com/open-buildings-data/v3/polygons_s2_level_4_gzip/967_buildings.csv.gz")
footdt3<-fread("https://storage.googleapis.com/open-buildings-data/v3/polygons_s2_level_4_gzip/961_buildings.csv.gz")

footdt<-rbind(footdt2, footdt3) #if more than one tile

foots<-st_as_sf(footdt, wkt = "geometry", crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

rm(footdt2,footdt3,footdt)

blds<-st_intersection(foots$geometry,rad)



chlrad<-st_transform(rad,crs=9150)
chlrds<-st_transform(wrds,crs=9150)
chlblds<-st_transform(blds,crs=9150)


gr <- st_make_grid(chlrad, square = T, cellsize = c(1000, 1000)) %>% st_sf() 

#create grided radius for calculating density in ea Km sq
Rgrid<-st_intersection(gr,chlrad)
R_grid<-st_cast(Rgrid, "POLYGON")

# get road density

rd_lens<-c() #empty vector to contain results of loop
for(i in seq_along(R_grid$geometry)){ # for each grid square
  rd_clip<-st_intersection(chlrds, R_grid$geometry[i]) 
  lens<-st_length(rd_clip)# Find the length of each line
  len_sum<-sum(lens) #sum length of all lines in the grid square
  rd_lens<-c(rd_lens, len_sum) #append to all grid square summed lengths
}

R_grid<-R_grid %>% mutate(rd_len=rd_lens) #add rd length to grid data
R_grid<-R_grid %>% mutate(rd_lvls=cut(rd_lens,5, labels= c(1:5))) #factor into 5 equal interval levels

ggplot(data = R_grid, aes(fill = rd_lvls, color= rd_lvls)) +
  geom_sf() +
  scale_fill_manual(values = c("white","pink", "hotpink", "red", "maroon"))+
  scale_color_manual(values = c("white","pink", "hotpink", "red", "maroon"))

#get bld areas

bld_ar<-c()
for(i in seq_along(R_grid$geometry)){ # for each grid square
  b_clip<-st_intersection(chlblds, R_grid$geometry[i]) #clip buildings to square
  ars<-st_area(b_clip)# Find the area of each building
  ar_sum<-sum(ars) #sum areas
  bld_ar<-c(bld_ar, ar_sum) #append to all grid square summed areas
}


R_grid<-R_grid %>% mutate(bldg_ars=bld_ar) 
R_grid<-R_grid %>% mutate(bldg_lvls=cut(bldg_ars,5, labels= c(1:5))) #factor into 5 equal interval levels

ggplot(data = R_grid, aes(fill = bldg_lvls, color= bldg_lvls)) +
  geom_sf() +
  scale_fill_manual(values = c("white","pink","hotpink", "red","maroon"))+
  scale_color_manual(values = c("white", "pink","hotpink", "red","maroon"))

R_grid<-R_grid %>% mutate(dens_dex=as.numeric(rd_lvls)+as.numeric(bldg_lvls)) 

ggplot(data = R_grid, aes(fill = dens_dex, color= dens_dex)) +
  geom_sf() +
  scale_fill_continuous()


#buffer roads for selecting sampling locations in the buffer
rds_buff<-st_buffer(chlrds, 10)
rds_dis<-st_union(rds_buff) #basically a dissolve on the road segments

#probably will write a function to do these repetitive lines below but `\_('~')_/`

#get just squares from grid at each level of density
lvl1clip<- R_grid%>%filter(dens_dex<=3)
lvl2clip<- R_grid%>%filter(dens_dex>3 & dens_dex <= 4)
lvl3clip<- R_grid%>%filter(dens_dex>4 & dens_dex <= 6)
lvl4clip<- R_grid%>%filter(dens_dex>6 & dens_dex <= 7)
lvl5clip<- R_grid%>%filter(dens_dex>7)

#intersect them with the roads buffer
rdclip1<-st_intersection(rds_dis,lvl1clip)
rdclip2<-st_intersection(rds_dis,lvl2clip)
rdclip3<-st_intersection(rds_dis,lvl3clip)
rdclip4<-st_intersection(rds_dis,lvl4clip)
rdclip5<-st_intersection(rds_dis,lvl5clip)

#sample random points from each
pnts1<-st_sample(rdclip1,size=15)
pnts2<-st_sample(rdclip2,size=15)
pnts3<-st_sample(rdclip3,size=15)
pnts4<-st_sample(rdclip4,size=15)
pnts5<-st_sample(rdclip5,size=15)


ggplot() + 
  geom_sf(data = R_grid, aes(fill = dens_dex, color= dens_dex)) + 
  scale_fill_continuous()+
  geom_sf(data=pnts1, color="yellow")+
  geom_sf(data=pnts2, color="orange")+
  geom_sf(data=pnts3, color="pink")+
  geom_sf(data=pnts4, color="green")+
  geom_sf(data=pnts5, color="purple")

#convert to lat lon coords
p1<-st_transform(pnts1, crs = 4326)%>%st_coordinates()%>% as.data.frame()
p2<-st_transform(pnts2, crs = 4326)%>%st_coordinates()%>% as.data.frame()
p3<-st_transform(pnts3, crs = 4326)%>%st_coordinates()%>% as.data.frame()
p4<-st_transform(pnts4, crs = 4326)%>%st_coordinates()%>% as.data.frame()
p5<-st_transform(pnts5, crs = 4326)%>%st_coordinates()%>% as.data.frame()

smpl_pnts<-rbind(p1,p2,p3,p4,p5)

R_grid$dens_dex<-as.factor(R_grid$dens_dex)
ggplot() +
  geom_sf(data = R_grid, aes(fill = dens_dex, color= dens_dex)) +
  scale_fill_manual(values = c("white","#ffe5ec","#ffc2cd", "#ff93ac","#ff6289","#ff6289","#fc3468","#ff084a","#ff084a"))+
  scale_color_manual(values = c("white","#ffe5ec","#ffc2cd", "#ff93ac","#ff6289","#ff6289","#fc3468","#ff084a","#ff084a"))+
  #geom_sf(data=rad, fill="transparent", linewidth=1.2)+
  theme_void()

