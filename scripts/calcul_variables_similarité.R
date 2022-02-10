### Titre -------------------------------------
# Nom : Calcul variables Contexte Spatiael (CS) > Similarité
# Auteure : Perle Charlot
# Date de création : 10-02-2022
# Dates de modification : 

### Librairies -------------------------------------

library(sf)
library()
library()
library(raster)
library(data.table)
library(tictoc)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs
input_path <- paste0(wd,"/input/")
output_path <- paste0(wd,"/output/")

# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Chemin vecteur habitat
path_vecteur_habitat <- paste0(dos_var_sp,"/Milieux/Natura_2000/n_hab_dominants_n2000_s_r84_jointure.gpkg")

# Chemin d'un raster calé
raster_ref <- paste0(dos_var_sp,"/Milieux/IGN/mnt_25m_belledonne_cale.TIF")

# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "

### Programme -------------------------------------

# Habitat similaire adjacent
# % de même habitat adjacent dans un rayon de 500m (tester plusieurs rayons)


# taille du patch d’habitat
