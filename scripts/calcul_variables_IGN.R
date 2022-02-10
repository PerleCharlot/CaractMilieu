### Titre -------------------------------------
# Nom : Calcul variables issues IGN
# Auteure : Perle Charlot
# Date de création : 08-01-2022
# Dates de modification : 18-01-2022

### Librairies -------------------------------------

library(raster)
library(sf)
library(fasterize)
#library(data.table)

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

# Chemin raster eaux
surface_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/surface_vecteur.gpkg")
tronçon_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
foret_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

### Programme -------------------------------------

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
