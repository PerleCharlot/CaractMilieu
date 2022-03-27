### Titre -------------------------------------
# Nom : Calcul des variables de la dimension CONTEXTE ABIOTIQUE
# Auteure : Perle Charlot
# Date de création : 17-11-2021
# Dates de modification : 27-03-2022

### Librairies -------------------------------------
library(raster)
library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(vegan)

# library(data.table)
# library(fasterize)
# library(rgdal)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des outputs (dans le git)
output_path <- paste0(wd,"/output/")
# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

#### Données spatiales ####
# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# Vars sp
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
path_LS_factor <- paste0(dos_var_sp,"/Milieux/LS_factor/ls_factor_Belledonne.tif")
path_landform5m <- paste0(output_path,"/var_intermediaire/landform_5m_crs.tif")

#### Tables ####

### Programme -------------------------------------

#### Conditions topographiques ####

MNT25m <- raster(chemin_mnt)

## VAR : pente ##
pente25m <- terrain(MNT25m, opt="slope",unit="degrees",
                    filename=paste0(output_path,"/var_CA/pente_25m.tif"))
## VAR : exposition ##
# Pour éviter discontinuité, transfo en northing et easting
# /!\ sin(x) et cos(x) où x en radian, donc calcule exposition en RAD
exposition25m <- terrain(MNT25m, opt="aspect",unit="radians")
northing25m <- calc(exposition25m, fun = cos, 
                    filename= paste0(output_path,"/var_CA/northing_25m.tif")) 
easting25m <- calc(exposition25m, sin,
                   filename= paste0(output_path,"/var_CA/easting_25m.tif"))
# Sauvegarde de l'exposition en degrés
exposition25m_deg <- terrain(MNT25m, opt="aspect",unit="degrees",
                             filename=paste0(output_path,"/pas_utilisé/exposition_25m.tif"))
## VAR : érosion (LS factor) ##
rast_LS <- raster(path_LS_factor)
rast_LS <- projectRaster(rast_LS, MNT25m)# CROP en même temps!
writeRaster(rast_LS, paste0(output_path,"/var_CA/LS_factor.tif"))

## VAR : rayonnement ##

## VAR : diversité de reliefs ##
LF5m <- raster(path_landform5m)
LF5m_crop <- crop(LF5m, MNT25m)

# créer un raster où chaque pixel a une valeur différentes
rast_zonal = MNT25m; nb_cel <- dim(MNT25m)[1]*dim(MNT25m)[2]
rast_zonal[] <- 1:nb_cel
# créer une grille taille pixel 25m
grille2 <- rasterToPolygons(rast_zonal)
names(grille2) <-"pixel25"
datavals4 <- do.call(rbind,exact_extract(LF5m_crop, grille2, include_cols="pixel25"))
# calculer indices de diversité par pixel de 25m
diversite_lf <- datavals4 %>% 
  group_by(pixel25) %>%
  summarise(nb_lf = n_distinct(value))
tableDiversity <- table(datavals4$pixel25, datavals4$value)
diversite_lf$shannon <- diversity(tableDiversity, index = "shannon")
diversite_lf$simpson <- diversity(tableDiversity, index = "simpson")
# /!\ j'ai fait fi des % de couverture des pixels...
# créer couches rasters depuis dataframe
shannon_lf = rast_zonal
shannon_lf[] <- diversite_lf$shannon
simpson_lf = rast_zonal
simpson_lf[] <- diversite_lf$simpson
ndis_lf = rast_zonal
ndis_lf[] <- diversite_lf$nb_lf
# par(mfrow=c(1,3))
# plot(shannon_lf,main="Shannon")
# plot(simpson_lf, main ="Simpson")
# plot(ndis_lf, main ="Nb lf")
writeRaster(shannon_lf, paste0(output_path,"/var_CA/shannon_landform.tif"))
writeRaster(simpson_lf, paste0(output_path,"/var_CA/simpson_landform.tif"))
writeRaster(ndis_lf, paste0(output_path,"/var_CA/nb_distinct_landform.tif"))

#### Eau ####

## VAR : TWI ##
source("C:/Users/perle.charlot/Documents/R/win-library/dynatopmodel_1.2.1.tar/dynatopmodel_1.2.1/dynatopmodel/R/upslope_area.R")
TWI25m <- upslope.area(MNT25m, atb = TRUE)$atb
writeRaster(TWI25m, filename=paste0(output_path,"/var_CA/TWI_25m.tif"))

## VAR : présence d'eaux libres dans le pixel ##