### Titre -------------------------------------
# Nom : Calcul des variables de la dimension PHYSIONOMIE DE VEGETATION
# Auteure : Perle Charlot
# Date de création : 25-03-2022
# Dates de modification : -2022

### Librairies -------------------------------------

library(data.table)
library(raster)

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
# chemin du dossier contenant les différentes couches spatiales iées aux habitats
path_poly_hab <- paste0(dos_var_sp ,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
path_rast_hab <- paste0(output_path,"/habitat_raster_25m.TIF")

#### Tables ####
path_table_hbt_PV <- paste0(output_path,"/tables/table_hbt_PV.csv")

### Programme -------------------------------------

#### Structure ####

# Import tables
table_hbt <- fread(path_table_hbt_PV)

# Import raster habitat
rast_hab <- raster(path_rast_hab)

## VAR : hauteur physionomie max ##
rast_htphysio_ete <- raster::subs(rast_hab,table_hbt,by='code',which='ht_physio_max_été')
rast_htphysio_hiv <- raster::subs(rast_hab,table_hbt,by='code',which='ht_physio_max_hiv')
writeRaster(rast_htphysio_ete, paste0(output_path,"/var_PV/ht_physio_max_ete.tif"))
writeRaster(rast_htphysio_hiv, paste0(output_path,"/var_PV/ht_physio_max_hiv.tif"))

## VAR : nombre de strates de végétation ##
rast_nbstrate <- raster::subs(rast_hab,table_hbt,by='code',which='nb_strates')
writeRaster(rast_nbstrate, paste0(output_path,"/var_PV/nb_strates.tif"))

## VAR : pénétrabilité de la strate basse ##
rast_penetrab <- raster::subs(rast_hab,table_hbt,by='code',which='pénétrabilité_strate_basse')
writeRaster(rast_penetrab, paste0(output_path,"/var_PV/penetrabilite.tif"))


plot(rast_penetrab,colNA='black')
#### Qualité scénique locale ####

## VAR : intensité floraison ##

