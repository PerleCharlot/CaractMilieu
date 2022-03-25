### Titre -------------------------------------
# Nom : Calcul des variables de la dimension BIOMASSE
# Auteure : Perle Charlot
# Date de création : 25-03-2022
# Dates de modification : -2022

### Librairies -------------------------------------

library(data.table)
library(raster)
library(dplyr)

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

# Emprise carré autour N2000
path_emprise <- paste0(dos_var_sp,"/limites_etude/emprise.gpkg")

#### Tables ####
path_table_hbt_PV <- paste0(output_path,"/tables/table_hbt_PV.csv")

### Programme -------------------------------------

#### Quantité ####

## VAR : GDD ##

## VAR : NDVI ##

## VAR : Bois sur pied ##

## VAR : Abondance feullage ##
rast_hab <- raster(path_rast_hab)
table_hbt <- fread(path_table_hbt_PV)

# passer de catégoriel (néant, rare, moyen, abaondant) à ordinal (0,1,2,3)
table_hbt  <- table_hbt %>% 
  mutate(abondance_feuillage_été = recode(feuillage_dispo_été,
                                                             `néant`=0,
                                                             `rare`=1,
                                                             `moyen`=2,
                                                             `abondant`=3
                                                             ),
                     abondance_feuillage_hiv = recode(feuillage_dispo_hiv,
                                                             `néant`=0,
                                                             `rare`=1,
                                                             `moyen`=2,
                                                             `abondant`=3)
                     )
# créer les rasters
rast_feuillage_ete <- raster::subs(rast_hab,table_hbt,by='code',which='abondance_feuillage_été')
rast_feuillage_hiv <- raster::subs(rast_hab,table_hbt,by='code',which='abondance_feuillage_hiv')
writeRaster(rast_feuillage_ete, paste0(output_path,"/var_B/abondance_feuillage_été.tif"))
writeRaster(rast_feuillage_hiv, paste0(output_path,"/var_B/abondance_feuillage_hiv.tif"))

#### Qualité ####

## VAR : Digestibilité ##

#### Productivité ####

## VAR : P-ETP ##