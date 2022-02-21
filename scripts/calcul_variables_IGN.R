### Titre -------------------------------------
# Nom : Calcul variables issues IGN
# Auteure : Perle Charlot
# Date de création : 08-01-2022
# Dates de modification : 18-01-2022

### Librairies -------------------------------------

library(raster)
library(sf)
library(fasterize)
library(data.table)
library(rgdal)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des outputs (dans le git)
output_path <- paste0(wd,"/output/")

# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Chemin du MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

# Chemin données spatiales
surface_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/surface_vecteur.gpkg")
tronçon_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
foret_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
#TODO : vecteur à enregistrer
bati_path <- paste0(dos_var_sp,"/Milieux/IGN/batiment_emprise.gpkg")
transp_path <- paste0(dos_var_sp,"/Milieux/IGN/equipement_de_transport_emprise.gpkg")
cable_path <- paste0(dos_var_sp,"/Milieux/IGN/transport_par_cable_emprise.gpkg")

# Chemin vecteur habitat
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
path_raster_habitat <- paste0(output_path,"/habitat_raster_25m.tif")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

resolution_etude = 25

# Table correspondance habitat et degré d'artificialisation (0 à 3, pas à très artificialisé)
corresp_hbt_artif_path <- paste0(dos_var_sp,"/Milieux/IGN/habitat_artificialisation.csv")
corresp_hbt_code_path <-paste0(output_path,"/correspondance_habitat_raster.csv")

### Programme -------------------------------------

### Eaux libres (CS > Viewshed & CA > eau) ####

# Chargement des vecteurs de tronçons et surface en eau (issu BD TOPO IGN)
vect_surf_eau <- st_read(surface_eau_path)
vect_tron_eau <- st_read(tronçon_eau_path)

# Rasteriser les vecteurs
rast_ref <- raster(chemin_mnt)
rast_surf_eau <-fasterize(vect_surf_eau,rast_ref)
rast_tron_eau <-rasterize(as(vect_tron_eau,"Spatial"),rast_ref, field=1)

# Assemblage des rasters
rast_eau <- merge(rast_surf_eau,rast_tron_eau)
names(rast_eau) <- "eaux_libres"
plot(rast_eau, colNA="black")

# Calcul des variables topo directement issues du MNT 25m
if(!dir.exists(paste0(output_path,"/var_IGN"))){
  dir.create(paste0(output_path,"/var_IGN"))
}

# Sauvegarde fichier raster
writeRaster(rast_eau, file= paste0(output_path,"/var_IGN/eaux_libres.TIF"))


### Infrastructures > artificialisation ####

# Assemblage des différentes couches d'infrastructures
# Charger les vecteurs infrastructures (batiment, linéaire cable, transport)
bati_vect <- readOGR(bati_path)
cable_line <- readOGR(cable_path)
transp_vect <- readOGR(transp_path)

# Transformer de ligne à polygone + singlepolygon
cable_line_buf <- buffer(cable_line,10)
multiP <- st_cast(st_as_sf(cable_line_buf), "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")
cable_line_buf <- as_Spatial(singleP)
# !! après examen sous QGIS, le refuge de la Pra disparait lors de la rastérisation
bati_vect_buf <- buffer(bati_vect, 5)
multiP <- st_cast(st_as_sf(bati_vect_buf), "MULTIPOLYGON")
singleP <- st_cast(multiP, "POLYGON")
bati_vect_buf <- as_Spatial(singleP)
# Regrouper toutes les infrastructures
infrastructures_vect <- bind(bati_vect_buf,transp_vect,cable_line_buf)
st_write(st_as_sf(infrastructures_vect), paste0(output_path,"/var_IGN/infrastructures.gpkg"),append=F)

## Degré d'artificialisation/d'aménagement du pixel : ordinale, 0, 1, 2, 3 (pas à très aménagé) ##

# passer par l'habitat déjà rastérisé afin d'éviter les trous au milieu
# Chargement table de correspondance habitat <-> degré artif
corresp_habitat_artif <- fread(corresp_hbt_artif_path)
# Chargement table correspondance habitat <-> code
corresp_habitat_code <- fread(corresp_hbt_code_path,drop="V1")
corresp_code_artif <- merge(corresp_habitat_artif,corresp_habitat_code, by=c("lbhab","cdhab"))
# Chargement raster hbt
rast_habitat <- raster(path_raster_habitat)
habitat_artif_rast <- subs(rast_habitat, corresp_code_artif, by="code",which="degre_artif")
plot(habitat_artif_rast,colNA="black")

# Sauvegarde du vecteur et du raster
st_write(habitat_artif_vect)
writeRaster(habitat_artif_rast, paste0(output_path,"/var_habitat/degre_artif.tif"))

## Pourcentage de sol artificialisé dans un pixel

# Chargement couche bati
vect_bati <- st_read(paste0(output_path,"/var_IGN/infrastructures.gpkg"))
vect_bati$infra <- 1
vect_bati <- vect_bati %>% select(infra)

library(tidyverse)
library(exactextractr)

grille <- raster(paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne_cale.tif"))
#rast_habitat <- raster(path_raster_habitat)

datavals <- do.call(rbind,exact_extract(grille, vect_bati,
                                        include_xy = TRUE,progress = FALSE))
datavals$value[!is.na(datavals$value)] <- 1
datavals$surf <- datavals$value * datavals$coverage_fraction * 100

xyz <- as.data.frame(cbind(x = datavals$x, y = datavals$y, z = datavals$surf))
xyz$z[is.na(xyz$z)] = 0

pourcent_bati <- rasterFromXYZ(xyz,
                      crs=crs(grille))
pourcent_bati[is.na(pourcent_bati)] = 0
plot(pourcent_bati, colNA='black')
writeRaster(pourcent_bati, paste0(output_path,"/var_IGN/pourcentage_infrastructures.tif"))

# Typologie d'aménagement


## Proximité à du bati



# raster ref
rast_ref <- raster(path_raster_habitat)
# Rastériser infrastructures
bati_rast <- fasterize(st_as_sf(infrastructures_vect),rast_ref)
plot(bati_rast)

# Calculer distance aux infrastructures
dist_bati_rast <- terra::distance(bati_rast)
plot(dist_bati_rast)

# Sauvegarde des rasters
writeRaster(bati_rast, paste0(output_path,"/var_IGN/infrastructures_25m.tif"),overwrite=TRUE)
writeRaster(dist_bati_rast, paste0(output_path,"/var_IGN/distance_infrastructures.tif"),overwrite=TRUE)
