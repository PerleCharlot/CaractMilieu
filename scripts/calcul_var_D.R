### Titre -------------------------------------
# Nom : Calcul des variables de la dimension DYNAMIQUE
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 10-03-2022

### Librairies -------------------------------------

library(data.table)


library(raster)
library(sf)
library(fasterize)
library(rgdal)

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
# chemin du dossier contenant les différentes couches shape du CLPA
path_doss_CLPA <- paste0(dos_var_sp,"Milieux/CLPA_Alp_L93_20191213_PCharlot/")

CLPA_Alp_linpi_L93_20191213 # photo-interprétation, linéaire
CLPA_Alp_lint_L93_20191213 # témoignage, linéaire
CLPA_Alp_zonpi_L93_20191213 # photo-interprétation, polygone
CLPA_Alp_zont_L93_20191213 # témoignage, polygone

# Emprise carré autour N2000
path_emprise <- paste0(dos_var_sp,"/limites_etude/emprise.gpkg")

chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####

### Programme -------------------------------------

#### Régime de perturbation ####

## VAR : présence couloir d’avalanche (/âge depuis dernière perturbation) ##
liste_couche_CLPA <- list.files(path_doss_CLPA, "*.shp$", full.names=TRUE)
# do.call(readOGR, as.list(liste_couche_CLPA))
CLPA1 <- readOGR(liste_couche_CLPA[[1]])
CLPA2 <- readOGR(liste_couche_CLPA[[2]])
CLPA3 <- readOGR(liste_couche_CLPA[[3]])
CLPA4 <- readOGR(liste_couche_CLPA[[4]])

# Transformer de ligne à polygone + singlepolygon
CLPA1_buf <- buffer(CLPA1,10)
CLPA2_buf <- buffer(CLPA2,10)

# Regrouper toutes les infrastructures
CLPA <- raster::bind(CLPA1_buf,CLPA2_buf,CLPA3,CLPA4)

# rasteriser les couches
CLPA_raster <- fasterize(st_as_sf(CLPA),raster(chemin_mnt), background = 0)
plot(CLPA_raster, colNA="black")
writeRaster(CLPA_raster, paste0(output_path,"/var_D/presence_avalanche.TIF"),overwrite=TRUE)
st_write(st_as_sf(CLPA), paste0(output_path,"/var_intermediaire/couches_CLPA_regroupe.gpkg"))

#### Stade de succession ####

## VAR : vitesse greening ##


