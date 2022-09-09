### Titre -------------------------------------
# Nom : ACP variables climatiques
# Auteure : Perle Charlot
# Date de création : 09-08-2022
# Dates de modification : -2022

### Librairies -------------------------------------

library(raster)
library(RStoolbox)
library(data.table)

### Fonctions -------------------------------------

# ACP sur les variables climatiques, par mois
ACP_varclim <- function(mois){
  # # TEST
  # mois = liste.mois[1]
  
  list_var = list.files(paste0(output_path,"/var_CA/par_periode/",mois), ".tif$|.TIF", full.names=TRUE)
  stack_vars = stack(list_var)
  
  var_clim = table_variables$Nom[which(table_variables$Caractéristique == "Conditions climatiques")]
  stack_vars_clim = dropLayer(stack_vars,which(!names(stack_vars) %in% var_clim))
  
  PCA_var_clim = rasterPCA(stack_vars_clim, spca=TRUE, nComp=2)
  
  print(summary(PCA_var_clim$model))
  # Se limiter à 2 axes (min 87% | max 97% variance cumulée expliquée)
  if(!dir.exists(paste0(output_path,"/var_CA/par_periode/",mois,"/ACP_climat/"))){
    dir.create(paste0(output_path,"/var_CA/par_periode/",mois,"/ACP_climat/"))}
  
  writeRaster(PCA_var_clim$map$PC1, paste0(output_path,"/var_CA/par_periode/",mois,"/ACP_climat/ACP1_clim.tif"), overwrite=T)
  writeRaster(PCA_var_clim$map$PC2, paste0(output_path,"/var_CA/par_periode/",mois,"/ACP_climat/ACP2_clim.tif"), overwrite=T)
  
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

#### Données spatiales ####

# Dossier des variables spatiales & chemins des fichiers
dos_var_sp <- "C:/Users/perle.charlot/Documents/PhD/DATA/Variables_spatiales_Belledonne/"

# Dossier des rasters en stack de dimensions
path_dos_stack <- paste0(output_path,"/stack_dim/")

#### Tables ####
path_table_variables <- paste0(input_path,"/liste_variables.csv")

#### Autre ####
# Liste mois étudiés
liste.mois = c("mai","juin","juillet","aout","septembre")


### Programme -------------------------------------

table_variables <- fread(path_table_variables)

lapply(liste.mois, ACP_varclim)

# MAI
# Importance of components:
#                         Comp.1     Comp.2     Comp.3     Comp.4      Comp.5       Comp.6       Comp.7     Comp.8       Comp.9      Comp.10
# Standard deviation     3.0226303 0.75299630 0.38825692 0.32989806 0.153798368 0.0791858414 0.0670668063 0.04595541 1.922214e-02 1.493380e-02
# Proportion of Variance 0.9136294 0.05670034 0.01507434 0.01088327 0.002365394 0.0006270397 0.0004497957 0.00021119 3.694908e-05 2.230183e-05
# Cumulative Proportion  0.9136294 0.97032971 0.98540406 0.99628733 0.998652724 0.9992797634 0.9997295591 0.99994075 9.999777e-01 1.000000e+00

# JUIN
# Importance of components:
#                         Comp.1    Comp.2     Comp.3     Comp.4      Comp.5      Comp.6       Comp.7       Comp.8       Comp.9      Comp.10
# Standard deviation     2.8929420 1.0887722 0.49698059 0.36598810 0.216774488 0.101278984 0.0600020378 0.0409170066 0.0363358768 2.609813e-02
# Proportion of Variance 0.8369114 0.1185425 0.02469897 0.01339473 0.004699118 0.001025743 0.0003600245 0.0001674201 0.0001320296 6.811125e-05
# Cumulative Proportion  0.8369114 0.9554539 0.98015282 0.99354755 0.998246671 0.999272415 0.9996324390 0.9997998592 0.9999318887 1.000000e+00

# JUILLET
# Importance of components:
#                         Comp.1    Comp.2     Comp.3     Comp.4     Comp.5      Comp.6      Comp.7       Comp.8       Comp.9      Comp.10
# Standard deviation     2.6006197 1.5137537 0.70951994 0.50204056 0.38811952 0.154016103 0.107645540 0.0425975040 0.0341160113 3.065470e-02
# Proportion of Variance 0.6763223 0.2291450 0.05034186 0.02520447 0.01506368 0.002372096 0.001158756 0.0001814547 0.0001163902 9.397108e-05
# Cumulative Proportion  0.6763223 0.9054673 0.95580918 0.98101366 0.99607733 0.998449428 0.999608184 0.9997896387 0.9999060289 1.000000e+00

# AOUT
# Importance of components:
#                          Comp.1    Comp.2     Comp.3     Comp.4     Comp.5     Comp.6      Comp.7      Comp.8       Comp.9      Comp.10
# Standard deviation     2.6275224 1.3670560 0.79376670 0.53071580 0.43330409 0.32153665 0.120764722 0.079426380 0.0512796132 2.997847e-02
# Proportion of Variance 0.6903874 0.1868842 0.06300656 0.02816593 0.01877524 0.01033858 0.001458412 0.000630855 0.0002629599 8.987089e-05
# Cumulative Proportion  0.6903874 0.8772716 0.94027815 0.96844408 0.98721932 0.99755790 0.999016314 0.999647169 0.9999101291 1.000000e+00

# SEPTEMBRE
# Importance of components:
#                         Comp.1    Comp.2     Comp.3     Comp.4      Comp.5      Comp.6      Comp.7       Comp.8       Comp.9      Comp.10
# Standard deviation     2.8984128 1.0652662 0.50067410 0.36458429 0.205506735 0.154587701 0.103202641 0.0485743235 0.0320758634 2.540777e-02
# Proportion of Variance 0.8400797 0.1134792 0.02506746 0.01329217 0.004223302 0.002389736 0.001065079 0.0002359465 0.0001028861 6.455547e-05
# Cumulative Proportion  0.8400797 0.9535589 0.97862633 0.99191850 0.996141798 0.998531533 0.999596612 0.9998325584 0.9999354445 1.000000e+00
