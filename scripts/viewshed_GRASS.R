#################################################################
#           Stage 3A - GMN 2019-2020 - AgroParisTech            #
#                                                               #
#    Caractéristiques biophysiques et usages des milieux        #
# au cours des saisons, dans l'alpage du Chardonnet (Névache 05)#
#                                                               #
#      Ninon FONTAINE - encadrante : Isabelle BOULANGEAT        #
#################################################################
#                                                               #
#     Construction de la base de données pixels x traits        #
#                pour les données de viewsheds                  #
#                                                               #
#################################################################


######################################################################
# La fonction "viewsheds" du package viewshed3d renvoie des résultats 
# étranges (points visibles alors qu'ils ne devraient pas l'être). En
# passant par GRASS via le package rgrass7 et la fonction r.viewshed, 
# les viewsheds calculés sont plus cohérents.
#
# /!\ il faut lancer rstudio depuis la console OSGeo4W64
#
######################################################################


#+++++++++++++++++++++++++++++++++++++++++++++++++
#    Chargement des librairies nécessaires    ####
#+++++++++++++++++++++++++++++++++++++++++++++++++

# # Installation des librairies si nécessaire
# install.packages("rgrass7")

# Chargement des librairies
library(rgrass7)
library(sp)
library(raster)
library(vegan)

#+++++++++++++++++++++++++++++++
#   Lecture des fichiers    ####
#+++++++++++++++++++++++++++++++

wd = getwd()
output_path <- paste0(wd,"/output/")

#*----- MNT -----
#*# MNT 25m CALé sur le grille de REFERENCE
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
chemin_mnt2 <- paste0(dos_var_sp ,"/Milieux/IGN/MNT_25m_50km_cale.tif")

# chemin_mnt <- paste0(wd,"/input/mnt_25m_belledonne_cale.tif")
# chemin_mnt2 <- paste0(wd,"/input/MNT_25m_50km_cale.tif")


MNT25 = raster(chemin_mnt2) 

#*----- Limites  -----
#emprise = readOGR(path_emprise)
# path_N2000 <- paste0(wd,"/input/cembraie_N2000_limites.gpkg")

path_N2000 <- paste0(dos_var_sp,"/limites_etude/cembraie_N2000_limites.gpkg")
N2000 <- rgdal::readOGR(path_N2000)
lim = N2000@polygons[[1]]@Polygons[[1]]@coords

#*----- OSO -----
# path_raster_habitat_OCS <-paste0(wd,"/input/habitats_OCS.tif")
path_raster_habitat_OCS <-paste0(output_path,"/var_intermediaire/habitats_OCS.tif")
OCS_raster = raster(path_raster_habitat_OCS )
print("RASTER HABITAT OK.\n")

#*----- Masque =pixel hors forêt -----
# path_foret_rast <- paste0(wd,"/input/foret_raster_IGN.tif")
path_foret_rast <- paste0(output_path,"/var_intermediaire/foret_raster_IGN.tif")
rast_foret = raster::raster(path_foret_rast)
print("RASTER FORET OK.\n")
masque <- mask(MNT25,rast_foret,inverse=TRUE)

# #*----- Table correspondance OCS <-> note ethétique -----

#table_esthe <- fread(tbl_esth_path)

#*----- Projection voulue -----
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "
proj_l93 =  EPSG_2154


#+++++++++++++++++++++++++++++++++++
#    Initialisation de GRASS    ####
#+++++++++++++++++++++++++++++++++++

# Pour résoudre les problèmes GRASS (initialisation notamment) :
# http://r-sig-geo.2731867.n2.nabble.com/rgrass7-Error-in-parseGRASS-td7590598.html
#

# setup a temp workingdir
working.dir<- "~/tmp/"

# georeference the data 
# proj4string(MNT5)<- CRS(proj_l93)
proj4string(MNT25)<- CRS(proj_l93)
proj4string(masque) <- CRS(proj_l93)
proj4string(OCS_raster) <- CRS(proj_l93)

# get extent for GRASS
xmax<-MNT25@extent@xmax
xmin<-MNT25@extent@xmin
ymax<-MNT25@extent@ymax
ymin<-MNT25@extent@ymin

# get a first cellsize/pixel resolution for GRASS
resolution <- (xmax - xmin) / MNT25@ncols

# create and set working directory
if (!file.exists(file.path(working.dir,"run"))){
  dir.create(file.path(working.dir,"run"),recursive = TRUE)
}
setwd(file.path( working.dir,"run"))


########### SETUP OSGEO4W enviroment settings manually
# setup the OSGEO4W environ manually
# assuming a osgeow4w default "deskop fastinstall
# using the default installation directory "C:\OSGeo4W64"
  
# set OSGE4W base directory
osgeo4w.root="C:/OSGeo4W"
# osgeo4w.root="~/OSGeo4W" # TODO : remplacer avec getwd() pour remplacer C:/
Sys.setenv(OSGEO4W_ROOT=osgeo4w.root)
# define GISBASE
grass.gis.base<-paste0(osgeo4w.root,"/apps/grass/grass78")
Sys.setenv(GISBASE=grass.gis.base)

Sys.setenv(GRASS_PYTHON=paste0(Sys.getenv("OSGEO4W_ROOT"),"/bin/python.exe"))
Sys.setenv(PYTHONHOME=paste0(Sys.getenv("OSGEO4W_ROOT"),"/apps/Python39"))
Sys.setenv(PYTHONPATH=paste0(Sys.getenv("OSGEO4W_ROOT"),"/apps/grass/grass78/etc/python"))
Sys.setenv(GRASS_PROJSHARE=paste0(Sys.getenv("OSGEO4W_ROOT"),"/share/proj"))
Sys.setenv(PROJ_LIB=paste0(Sys.getenv("OSGEO4W_ROOT"),"/share/proj"))
Sys.setenv(GDAL_DATA=paste0(Sys.getenv("OSGEO4W_ROOT"),"/share/gdal"))

Sys.setenv(GEOTIFF_CSV=paste0(Sys.getenv("OSGEO4W_ROOT"),"/share/epsg_csv"))
Sys.setenv(FONTCONFIG_FILE=paste0(Sys.getenv("OSGEO4W_ROOT"),"/etc/fonts.conf"))

# call all OSGEO4W settings
system("C:/OSGeo4W/bin/o-help.bat")
# system("~/OSGeo4W/bin/o-help.bat")

# create PATH variable
Sys.setenv(PATH=paste0(grass.gis.base,";",
                       "C:/OSGEO4~1/apps/Python39/Lib/site-packages/numpy/core",";",
                       "C:/OSGeo4W/apps/grass/grass78/bin",";",
                       "C:/OSGeo4W/apps/grass/grass78/lib",";",
                       "C:/OSGeo4W/apps/grass/grass78/etc",";",
                       "C:/OSGeo4W/apps/grass/grass78/etc/python",";",
                       "C:/OSGeo4W/apps/Python39/Scripts",";",
                       "C:/OSGeo4W/bin",";",
                       "c:/OSGeo4W/apps",";",
                       "C:/OSGEO4~1/apps/saga",";",
                       paste0(Sys.getenv("WINDIR"),"/WBem"),";",
                       Sys.getenv("PATH")))

# Sys.setenv(PATH=paste0(grass.gis.base,";",
#                        "~/OSGEO4~1/apps/Python39/Lib/site-packages/numpy/core",";",
#                        "~/OSGeo4W/apps/grass/grass78/bin",";",
#                        "~/OSGeo4W/apps/grass/grass78/lib",";",
#                        "~/OSGeo4W/apps/grass/grass78/etc",";",
#                        "~/OSGeo4W/apps/grass/grass78/etc/python",";",
#                        "~/OSGeo4W/apps/Python39/Scripts",";",
#                        "~/OSGeo4W/bin",";",
#                        "~/OSGeo4W/apps",";",
#                        "~/OSGEO4~1/apps/saga",";",
#                        paste0(Sys.getenv("WINDIR"),"/WBem"),";",
#                        Sys.getenv("PATH")))


#################### start with GRASS setup
rgrass7::initGRASS(gisBase=grass.gis.base,
                     home=tempdir(),
                     mapset='PERMANENT',
                     override=TRUE
  )

# assign GRASS projection according to data set
rgrass7::execGRASS('g.proj',
                   flags=c('c','quiet'),
                   proj4=proj_l93
)

# assign GRASS extent and resolution
rgrass7::execGRASS('g.region',
                   flags=c('quiet'),
                   n=as.character(ymax),
                   s=as.character(ymin),
                   e=as.character(xmax),
                   w=as.character(xmin),
                   res=as.character(resolution)
)

#############   now do GRASS STUFF


#+++++++++++++++++++++++++++++
#    Calculs via GRASS    ####
#+++++++++++++++++++++++++++++

#*----- Initialisation : lecture du MNT dans GRASS -----
use_sp() # en réponse à l'erreur Error in rgrass7::writeRAST(MNT25, "MNT25") : no stars support yet
rgrass7::write_RAST(as(MNT25, "SpatialGridDataFrame"), "MNT25") # writeRAST fonctionne avec des spatialgriddataframes

#*----- Premier test -----
i=40000
rgrass7::execGRASS("r.viewshed", 
        input="MNT25", 
        output = 'viewshed_GRASS', 
        coordinates = c(coordinates(masque)[!is.na(values(masque)),1][i],coordinates(masque)[!is.na(values(masque)),2][i]),
        flags = c('quiet',"overwrite","b"),
        max_distance = 10000)
viewshed_GRASS <- readRAST("viewshed_GRASS")

# Visualisation
image(viewshed_GRASS)
points(coordinates(masque)[!is.na(values(masque)),1][i],coordinates(masque)[!is.na(values(masque)),2][i], pch=15)
lines(lim)

#*----- Boucle pour faire le calcul sur tout l'alpage -----

calculVarsViewshed <- function(i){
  
  print(i)
  # TEST
  # i=50000
  
  
  # AJOUT DE LA BOUCLE IF : si le pixel est en forêt, on considère que la visibilité est nulle
  # if (hab_dom$type_hab[which(hab_dom$num_pixel == i)] != "forêt"){}
  
  # 
  # X <- as.numeric(vector_ixy[2])
  # Y <- as.numeric(vector_ixy[3])
  #i <- df_points$num_pixel
  
  
  # Calcul du viewshed
  rgrass7::execGRASS("r.viewshed", 
                     input="MNT25", 
                     output = 'viewshed_GRASS', 
                     coordinates = c(raster::coordinates(masque)[!is.na(values(masque)),1][i],
                                     raster::coordinates(masque)[!is.na(values(masque)),2][i]),
                     # coordinates = c(X,Y),
                     flags = c('quiet',"overwrite","b"),
                     max_distance = 5000
  )
  viewshed_test <- read_RAST("viewshed_GRASS")
  
  # Calcul des variables
  
  # Diversité habitats visibles (= physionomie)
  OSO_viewshed = OCS_raster[which(viewshed_test@data$viewshed_GRASS == 1)]
  OSO_viewshed_NAomit = na.omit(OSO_viewshed)
  div_OSO_i <- vegan::diversity(table(OSO_viewshed_NAomit), index = "shannon")
  # recap_viewsheds$div_OSO[i] <- div_OSO_i
  
  # # Qualité esthétique moyenne des habitats visibles
  # df <- data.frame("classe"=OSO_viewshed_NAomit)
  # df_vis <- merge(df, table_esthe,by="classe")
  # note_quali_scenique_moy_OSO_i <- mean(df_vis$note_esthétique_B)
  # note_quali_scenique_med_OSO_i <- median(df_vis$note_esthétique_B)
  # recap_viewsheds$note_quali_scenique_moy_OSO[i] <- note_quali_scenique_moy_OSO_i
  # recap_viewsheds$note_quali_scenique_med_OSO[i] <- note_quali_scenique_med_OSO_i
  
  # # Proportion (%) de lacs visibles
  # pourcent_lac_i <- round(( dim(df_vis[df_vis$classe == 23,])[1] / dim(df_vis)[1] ) *100,2) 
  # recap_viewsheds$pourcent_lac[i] <- pourcent_lac_i
  
  
  # Distance mean, medianne, max des éléments visibles
  point_vue = c(raster::coordinates(masque)[!is.na(values(masque)),1][i],
                raster::coordinates(masque)[!is.na(values(masque)),2][i])

  #raster_visible = raster(viewshed_GRASS)
  raster_visible = raster(viewshed_test)
  
  raster_distance = distanceFromPoints(raster_visible,point_vue )
  raster_distance_visible = raster_visible * raster_distance
  distance_visible = values(raster_distance_visible)
  distance_visible = distance_visible[distance_visible>0]
  
  maxVis <- max(distance_visible)
  medVis <- median(distance_visible)
  meanVis <- mean(distance_visible)
  
  # les outputs : div_OSO ; note_quali_scenique_moy_OSO ; note_quali_scenique_med_OSO ; pourcent_lac
  outputs <- c(as.integer(i),div_OSO_i,
               # note_quali_scenique_moy_OSO_i,note_quali_scenique_med_OSO_i,
               # pourcent_lac_i,
               maxVis, medVis, meanVis)
  return(outputs)
  }

nb_pix <- length(masque[!is.na(masque)])
recap_viewsheds = data.frame(num_pixel = 1:length(masque[!is.na(masque)]), 
                             x = coordinates(masque)[!is.na(values(masque)),1],
                             y = coordinates(masque)[!is.na(values(masque)),2]) #99 857 pixels



#list_inter <- lapply(1:99857,calculVarsViewshed)
list_inter <- lapply(50000:99857,calculVarsViewshed)
df_inter <- as.data.frame(do.call(rbind, list_inter))
names(df_inter) <- c("num_pixel","div_OSO",
                     # "note_quali_scenique_moy_OSO",
                     #      "note_quali_scenique_med_OSO",
                     # "pourcent_lac",
                     "maxVis","medVis","meanVis")
df_VS <- merge(df_inter,recap_viewsheds, by="num_pixel")

write.csv2(df_VS, "C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractMilieu/output/var_intermediaire/viewshed/viewshed_50000_fin.csv")


# Assemblage des tables

liste_csv = list.files("C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractMilieu/output/var_intermediaire/viewshed", '.csv', full.names = T)
library(data.table)
table_viewshed = do.call(rbind,lapply(liste_csv, function(x) fread(x, drop="V1")))



table_viewshed2 = table_viewshed[!(duplicated(table_viewshed))]

write.csv(table_viewshed2, "C:/Users/perle.charlot/Documents/PhD/DATA/R_git/CaractMilieu/output/var_intermediaire/viewshed.csv")

# # Spatialisation
# raster_diversite_hbt_visible <- rasterFromXYZ(data.frame(df_VS$x, df_VS$y, df_VS$div_OSO))
# raster_max_visibilite <- rasterFromXYZ(data.frame(df_VS$x, df_VS$y, df_VS$maxVis))
# raster_med_visibilite <- rasterFromXYZ(data.frame(df_VS$x, df_VS$y, df_VS$medVis))
# raster_mean_visibilite <- rasterFromXYZ(data.frame(df_VS$x, df_VS$y, df_VS$meanVis))
# plot(raster_mean_visibilite, colNA='black')
# plot(raster_diversite_hbt_visible)
# plot(raster_max_visibilite, colNA="black")#pixel en foret = aveugle
# plot(raster_diversite_hbt_visible,colNA='black')
# 
# stack_run1 <- stack(raster_diversite_hbt_visible,
#       raster_max_visibilite,
#       raster_med_visibilite,
#       raster_mean_visibilite)
# writeRaster(stack_run1)
