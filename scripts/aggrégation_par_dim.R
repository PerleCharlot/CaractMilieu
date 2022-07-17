### Titre -------------------------------------
# Nom : Regroupement des variables à travers chaque dimension
# Auteure : Perle Charlot
# Date de création : 31-03-2022
# Dates de modification : 17-07-2022

### Librairies -------------------------------------

library(raster)
library(terra)

### Fonctions -------------------------------------

# Fonction qui vérifie que le raster ait le bon CRS et extent, et le modifie si besoin
AjustExtCRS <- function(path.raster.to.check, path.raster.ref=chemin_mnt){
  
  # # TEST
  # path.raster.to.check = r_hiv[2]
  # raster.ref = MNT
  
  raster.to.check <- raster(path.raster.to.check)
  raster.ref <- raster(path.raster.ref)
  
  ext.to.check <- extent(raster.to.check)
  bon.extent <- extent( raster.ref)
  
  sameCRS <- compareCRS(raster.to.check,EPSG_2154)
  sameExtent <- (ext.to.check == bon.extent)
  
  if(any(!sameCRS,!sameExtent)) {
    raster.to.check <- projectRaster(raster.to.check, raster.ref)
    writeRaster(raster.to.check, path.raster.to.check, overwrite=TRUE)
    cat("\nRaster ", names(raster.to.check)," a été modifié et sauvegarde.")
  }
  
}

# Fonction qui crée une stack de variables par dimension et par saison
CreateStackDimSeason <- function(nom_dim){
  
  # # #TEST
  # nom_dim = "CS"
  
  nom_dim = as.character(nom_dim)
  
  # Création d'une stack par mois
  periode = list.dirs(paste0(output_path,"/var_", nom_dim,"/par_periode//"), recursive = F, full.names = F)
  for(i in periode){
    liste_rasters <- list.files(paste0(output_path,"/var_",nom_dim,"/par_periode/",i),'.tif$|.TIF',full.names = TRUE)
    # S'assurer de la conformité des variables
    lapply(liste_rasters, AjustExtCRS)
    stack <- stack(liste_rasters)
    # Utiliser terra pour garder les noms de chaque variables dans la stack
    stack <- rast(stack)
    
    noms_vars =paste(names(stack), collapse = "\n- ")
    cat(paste0("\nLa stack de la variable ", nom_dim," pour le mois de ",i," contient les variables : \n- ",noms_vars))
    terra::writeRaster(stack,
                       paste0(output_path,"/stack_dim/",nom_dim,"/",i,"/stack_vars_",nom_dim,"_",i,".tif"),
                       overwrite=TRUE)
  }
  
  # 
  # liste_rasters <- list.files(paste0(output_path,"/var_",nom_dim,"/"),'.tif$',full.names = TRUE)
  # if(length(liste_rasters) == 0){
  #   liste_rasters <- list.files(paste0(output_path,"/var_",nom_dim,"/"),'.TIF$',full.names = TRUE)
  # }
  # # S'assurer de la conformité des variables
  # lapply(liste_rasters, AjustExtCRS)
  # 
  # # Séparer hiver/été si existent
  # r_hiv <- liste_rasters[grep("hiv",liste_rasters)]
  # r_ete <- liste_rasters[grep("ete|été",liste_rasters)]
  # r_both <- liste_rasters[which(!liste_rasters %in% r_hiv & !liste_rasters %in% r_ete)]
  # 
  # # Condition
  # if(!sum(length(r_hiv), length(r_ete), length(r_both)) == length(liste_rasters)){
  #   stop("\nProblème entre été et hiver.")
  # }
  # 
  # # Création de la stack par saison
  # stack_hiv <- stack(c(r_hiv, r_both))
  # stack_ete <- stack(c(r_ete, r_both))
  # # Utiliser terra pour garder les noms de chaque variables dans la stack
  # stack_hiv <- rast(stack_hiv)
  # stack_ete <- rast(stack_ete)
  # terra::writeRaster(stack_hiv,
  #                    paste0(output_path,"/stack_dim/",nom_dim,"_hiv.tif"),
  #                    overwrite=TRUE)
  # terra::writeRaster(stack_ete,
  #                    paste0(output_path,"/stack_dim/",nom_dim,"_ete.tif"),
  #                    overwrite=TRUE)
  # # Sauvegarde de la stack par saison par dimension
  # writeRaster(stack_hiv, paste0(output_path,"/stack_dim/",nom_dim,"_hiv.tif"), overwrite=TRUE)
  # writeRaster(stack_ete, paste0(output_path,"/stack_dim/",nom_dim,"_ete.tif"), overwrite=TRUE)
  
  cat("\nDimension ", nom_dim, "terminée. \n")
  
}

### Constantes -------------------------------------

# Espace de travail
wd <- getwd()
# Dossier des inputs (dans le git)
input_path <- paste0(wd,"/input/")
# Dossier des outputs (dans le git)
output_path <- paste0(wd,"/output/")
# Projection Lambert 93 (EPSG : 2154)
EPSG_2154 =  "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs "
# Liste dimensions
liste_dimensions =  c("CA","B","PV","CS","D","I")

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"
# MNT 25m CALé sur le grille de REFERENCE
chemin_mnt <- paste0(dos_var_sp ,"/Milieux/IGN/mnt_25m_belledonne_cale.tif")

#### Tables ####

### Programme -------------------------------------

# Stack des variables par dimension et par saison
lapply(liste_dimensions,CreateStackDimSeason)
