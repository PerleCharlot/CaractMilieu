### Titre -------------------------------------
# Nom : Constructions des variables intermédiaires
# Auteure : Perle Charlot
# Date de création : 01-03-2022
# Dates de modification : 

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

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")
# Surface en eaux libres et tronçons rivières IGN
surface_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/surface_vecteur.gpkg")
tronçon_eau_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
# Forêts IGN
foret_path <- paste0(dos_var_sp,"/Milieux/IGN/tronçon_vecteur.gpkg")
# Bâti IGN (bâtiments, infrastructures de transport et transport par cable)
bati_path <- paste0(dos_var_sp,"/Milieux/IGN/batiment_emprise.gpkg")
transp_path <- paste0(dos_var_sp,"/Milieux/IGN/equipement_de_transport_emprise.gpkg")
cable_path <- paste0(dos_var_sp,"/Milieux/IGN/transport_par_cable_emprise.gpkg")

# Habitats expertisés
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")
path_vecteur_habitat_simplifié <- paste0(dos_var_sp,"/Milieux/Natura_2000/polygones_habitat_simplifié.gpkg")
path_raster_habitat <- paste0(output_path,"/habitat_raster_25m.tif")

#### Tables ####

### Programme -------------------------------------

# Création des dossiers de sauvegarde
if(!dir.exists(paste0(output_path,"/var_IGN"))){
  dir.create(paste0(output_path,"/var_IGN"))
}

#### COUCHE VECTORIELLE INFRASTRUCTURES ####

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
infrastructures_vect <- raster::bind(bati_vect_buf,transp_vect,cable_line_buf)
st_write(st_as_sf(infrastructures_vect), paste0(output_path,"/var_IGN/infrastructures.gpkg"),append=F)

#### COUCHE RASTER EAUX LIBRES ####

# Chargement des vecteurs de tronçons et surface en eau (issu BD TOPO IGN)
vect_surf_eau <- st_read(surface_eau_path)
vect_surf_eau <- st_cast(vect_surf_eau, "POLYGON")

vect_tron_eau <- st_read(tronçon_eau_path)
vect_tron_eau <- buffer(as_Spatial(vect_tron_eau), 5)
vect_tron_eau <- st_cast(st_as_sf(vect_tron_eau), "POLYGON")
# Assembkage des vecteurs et sauvegarde
vect_eaux_libres <- raster::bind(as_Spatial(vect_surf_eau) ,as_Spatial(vect_tron_eau))
plot(vect_eaux_libres)
st_write(st_as_sf(vect_eaux_libres), paste0(output_path,"/var_IGN/eaux_libres.gpkg"),append=F)

# Rasteriser les vecteurs
rast_ref <- raster(chemin_mnt)
rast_surf_eau <- fasterize(vect_surf_eau,rast_ref)
rast_tron_eau <- rasterize(as(vect_tron_eau,"Spatial"),rast_ref, field=1)
# Assemblage des rasters
rast_eau <- merge(rast_surf_eau,rast_tron_eau)
names(rast_eau) <- "eaux_libres"
plot(rast_eau, colNA="black")
# Sauvegarde fichier raster
writeRaster(rast_eau, file= paste0(output_path,"/var_IGN/eaux_libres.TIF"))