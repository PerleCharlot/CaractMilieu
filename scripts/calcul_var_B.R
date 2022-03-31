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
# Dossier contenant les NDVI
path_dos_ndvi <- paste0(dos_var_sp,"/Milieux/NDVI/")

# Emprise carré autour N2000
path_emprise <- paste0(dos_var_sp,"/limites_etude/emprise.gpkg")
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####
path_table_hbt_PV <- paste0(output_path,"/tables/table_hbt_PV.csv")

### Programme -------------------------------------

#### Quantité ####

## VAR : GDD ##
# calculé dans script "calcul_var_CA"  dans ** conditions climatiques ***

## VAR : NDVI ##
liste_ndvi <- list.files(path_dos_ndvi,".tif$",full.names = TRUE)
liste_ndvi_été <- liste_ndvi[grep("_ete", liste_ndvi)]
liste_ndvi_hiv <- liste_ndvi[grep("_hiv", liste_ndvi)]

stack_ndvi_hiv <- stack(liste_ndvi_hiv)
stack_ndvi_ete <- stack(liste_ndvi_été)

# Moyenne sur les X années
rast_ndvi_mean_été <- calc(stack_ndvi_ete, mean)
rast_ndvi_mean_hiv <- calc(stack_ndvi_hiv, mean)
plot(rast_ndvi_mean_hiv, colNA='black')

# recaler sur raster de ref
rast_ref <- raster(chemin_mnt)
rast_ndvi_mean_été <- projectRaster(rast_ndvi_mean_été,rast_ref)
rast_ndvi_mean_été_cale <- resample(rast_ndvi_mean_été, rast_ref)
rast_ndvi_mean_hiv <- projectRaster(rast_ndvi_mean_hiv,rast_ref)
rast_ndvi_mean_hiv_cale <- resample(rast_ndvi_mean_hiv, rast_ref)

# crop à l'emprise
emprise <- st_read(path_emprise)
rast_ndvi_mean_été_cale_crop <- raster::crop(rast_ndvi_mean_été_cale,emprise)
rast_ndvi_mean_hiv_cale_crop <- raster::crop(rast_ndvi_mean_hiv_cale,emprise)

par(mfrow=c(1,2))
plot(rast_ndvi_mean_été_cale_crop, main="NDVI été")
plot(rast_ndvi_mean_hiv_cale_crop, main ="NDVI hiver")

writeRaster(rast_ndvi_mean_été_cale_crop, paste0(output_path,"/var_B/NDVI_été.tif"))
writeRaster(rast_ndvi_mean_hiv_cale_crop, paste0(output_path,"/var_B/NDVI_hiv.tif"))

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