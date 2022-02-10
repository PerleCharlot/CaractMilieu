### Titre -------------------------------------
# Nom : Calage des variables sur grille MNHN
# Auteure : Perle Charlot
# Date de création : 04-01-2022
# Dates de modification : 05-01-2022

### Librairies -------------------------------------

library(raster)

### Fonctions -------------------------------------

# Fonction qui permet de lire un raster, le rééchantilloner selon une référence et l'enregistrer
recalageRaster <- function(r, grille_raster_reference, path_dos_sauv){
  r <- raster(r)
  fn <- paste0(path_dos_sauv,"/",names(r),"_cale.tif")
  r_cale <- resample(r, grille_raster_reference, filename=fn, progress="text")
}

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des variables spatiales
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Dossier de sauvegarde des couches finales (avant recalage)
dos_sauv_1 <- paste0(dos_var_sp,"COUCHES_MILIEU_AVANT_RECALAGE")
# Dossier de sauvegarde des couches finales (après recalage)
dos_sauv_2 <- paste0(dos_var_sp,"COUCHES_MILIEU_FINALES")

# Chemin de la grille de référence
path_grill_ref <- paste0(dos_var_sp,"/zonage_MNHN/zonage_raster_25m.TIF")
### Programme -------------------------------------

# Espace de travail
setwd(wd)

# Chargement de la grille de référence
grille_calee <- raster(path_grill_ref)

# Chargement des raster à recaler
LR <- list.files(dos_sauv_1,full.names = TRUE)


# Utilisatinon recalage pour une liste de rasters
lapply(LR, recalageRaster,
       grille_raster_reference = grille_calee,
       path_dos_sauv = dos_sauv_2)

recalageRaster(paste0(dos_var_sp,"Milieux/IGN/mnt_25m_belledonne.tif"),
               grille_raster_reference = grille_calee,
               path_dos_sauv = dos_sauv_2)


