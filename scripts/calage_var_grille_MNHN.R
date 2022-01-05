### Titre -------------------------------------
# Nom : Calage des variables sur grille MNHN
# Auteure : Perle Charlot
# Date de création : 04-01-2022
# Dates de modification : 05-01-2022

### Librairies -------------------------------------

library(raster)

### Fonctions -------------------------------------

### Constantes -------------------------------------

# Espace de travail
wd <- "C:/Users/perle.charlot/Documents/PhD/DATA/"

# Dossier où l'on souhaite sauvegarder les variables calculées
dossier_sauv <- paste0(wd,"/Variables_spatiales_Belledonne/Milieux/variables_calculees_mnt/")

# Chemin de la grille de référence
path_grill_ref <- paste0(wd,"Variables_spatiales_Belledonne/zonage_MNHN/zonage_raster_25m.TIF")
### Programme -------------------------------------

# Espace de travail
setwd(wd)

# Chargement de la grille de référence
grille_calee <- raster(path_grill_ref)

# Chargement des raster à recaler
TPi <- raster(paste0(dossier_sauv,"TPI_25m.tif"))

# Recalage des variables selon la grille
TPI_cale <- resample(TPi, grille_calee)

# SAuvegarde des rasters recalés
writeRaster(TPI_cale,paste0(dossier_sauv,"TPI_25m_cale.tif"))
