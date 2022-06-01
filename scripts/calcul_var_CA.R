### Titre -------------------------------------
# Nom : Calcul des variables de la dimension CONTEXTE ABIOTIQUE
# Auteure : Perle Charlot
# Date de création : 17-11-2021
# Dates de modification : 31-03-2022

### Librairies -------------------------------------
library(raster)
library(terra)
library(exactextractr)
library(sf)
library(dplyr)
library(vegan)
library(data.table)
library(fasterize)
# library(rgdal)

### Fonctions -------------------------------------

mean_ete <- function(i){
  index_col_var <- which(names(table_NoPs) %in% paste0(liste_var[i],"_",été))
  df_var <- as.data.frame(table_NoPs)[,index_col_var]
  mean_var <- apply(df_var,1,mean)
  df_var <- data.frame( mean_var)
  names(df_var)[1] <- paste0(liste_var[i],"_mean_ete")
  return(df_var)
}
mean_hiver <- function(i){
  index_col_var <- which(names(table_NoPs) %in% paste0(liste_var[i],"_",hiver))
  df_var <- as.data.frame(table_NoPs)[,index_col_var]
  mean_var <- apply(df_var,1,mean)
  df_var <- data.frame( mean_var)
  names(df_var)[1] <- paste0(liste_var[i],"_mean_hiver")
  return(df_var)
}

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
path_eau_vect <- paste0(output_path,"/var_intermediaire/eaux_libres.gpkg")
# Chemin raster NoP SAFRAN
path_raster_NoP <- paste0(input_path,"/raster_NoP.tif")

#### Tables ####
path_table_NoPs <- paste0(output_path,"/tables/table_NoPs_climat.csv")

### Programme -------------------------------------

#### Conditions climatiques ####

table_NoPs <- fread(path_table_NoPs)

# Définition mois d'été et mois d'hiver
été = c("06","07","08","09") # de début juin à fin septembre ?
hiver = c("12","01","02","03") # de décembre à mars ?

liste_var = c("tmin","tmax","tmean","GDD","t10","t90",
              "nbJgel","nbJssdegel",
              "prmean","prsum","rain0",
              "nebmean","nbJneb10","nbJneb90",
              "windmean","wind10","wind90",
              "htNeigmean","raymean") # 19 variables climatiques

## VAR : Température ##
#(décile des T journallières puis moyenne mensuelle pour discerner extrême chaud et extrême froid)
## VAR : Précipitations ##
# nombre de jours sans pluie
## VAR : Neige ##
# épaisseur de neige moyenne
## VAR : Gel ##
# nombre de jours sans dégel
# nombre de jours de gel
## VAR : Nébulosité ##
# nb jour avec forte ou faible nébulosité
## VAR : Vent ##
# vent (décile sur vent moyen journalier pour discerner grand vent)

df_mean_ete <- do.call(cbind,lapply(1:length(liste_var),mean_ete)) 
df_mean_ete$NoPs = table_NoPs$V1

df_mean_hiver <- do.call(cbind,lapply(1:length(liste_var),mean_hiver)) 
df_mean_hiver$NoPs = table_NoPs$V1

# Création et remplissage des rasters climatiques
rast_NoPs <- raster(paste0(input_path,"/raster_NoP.tif"))
names(rast_NoPs) = "NoPs"

rast_climato_ete <- subs(rast_NoPs, df_mean_ete, by="NoPs", which=1:(dim(df_mean_ete)[2]-1))
writeRaster(rast_climato_ete,
            paste0(output_path,"/var_CA/", liste_var,"_ete.tif"),
            bylayer=TRUE)

rast_climato_hiver <- subs(rast_NoPs, df_mean_hiver, by="NoPs", which=1:(dim(df_mean_hiver)[2]-1))
writeRaster(rast_climato_hiver,
            paste0(output_path,"/var_CA/", liste_var,"_hiver.tif"),
            bylayer=TRUE)
# Transférer GDD de var_CA à var_B
CA <- list.files(paste0(output_path,"/var_CA/"), full.names = TRUE)
from <- CA[grep("GDD",CA)]
to <-  paste0(output_path,"/var_B/GDD_",c("ete","hiver"),".tif")
to
file.copy(from,to)

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
# cf conditions climatiques

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
eau_vect <- st_read(path_eau_vect)
vect_obj_rast <- fasterize(eau_vect,raster(chemin_mnt), background = 0)
plot(vect_obj_rast, colNA='black')
writeRaster(vect_obj_rast, paste0(output_path,"/var_CA/presence_eau.tif"))
