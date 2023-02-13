### Titre -------------------------------------
# Nom : Regroupement des variables à travers chaque dimension
# Auteure : Perle Charlot
# Date de création : 31-03-2022
# Dates de modification : 13-02-2023

### Librairies -------------------------------------

library(raster)
library(terra)
library(data.table)
library(stringr)
library(tidyverse)

### Fonctions -------------------------------------

# Permet de nettoyer les noms de colonnes (= variables) d'un dt
cleanVarName <- function(liste_nom_var_ref, dt_to_clean){
  # # TEST
  # liste_nom_var_ref = table_variables$Nom
  # dt_to_clean = test_dt
  
  noms_variables = names(dt_to_clean)[! names(dt_to_clean) %in% c("x","y")]
  if(any(!noms_variables %in% liste_nom_var_ref)){
    noms_bug = noms_variables[!noms_variables %in% liste_nom_var_ref]}
  
  for(i in liste_nom_var_ref){
    if(any(grepl(i, noms_bug))){
      names(dt_to_clean)[grepl(i, names(dt_to_clean))] = i
    }}
  return(dt_to_clean)
}

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
  }else{cat("\nRaster conforme.")}
}

# Fonction qui crée une stack de variables par dimension et par saison
CreateStackDimSeason <- function(nom_dim, periode=c("mai","juin","juillet","aout","septembre")){
  # # #TEST
  # nom_dim = "CA"
  cat(paste0("\nDimension ",nom_dim, " en cours."))
  nom_dim = as.character(nom_dim)
  # 
  # # Création d'une stack par mois
  # periode = list.dirs(paste0(output_path,"/var_", nom_dim,"/par_periode//"), recursive = F, full.names = F)
  for(i in periode){
    cat(paste0("\nMois ",i, " en cours."))
    liste_rasters <- list.files(paste0(output_path,"/var_",nom_dim,"/par_periode/",i),'.tif$|.TIF',full.names = TRUE)
    # S'assurer de la conformité des variables
    lapply(liste_rasters, AjustExtCRS)
    
    stack <- stack(liste_rasters)
    
    # # Utiliser ACP1_clim au lieu des 10 variables climatiques
    # if(nom_dim == "CA"){
    #   var_a_retirer = c("htNeigmean","nbJgel","nbJneb10","nbJneb90","nbJssdegel","rain0","t10","t90","wind10","wind90")
    #   stack = dropLayer(stack,which(names(stack) %in% var_a_retirer))
    # 
    #   stack_ACPclimat = stack(list.files(paste0(output_path,"/var_",nom_dim,"/par_periode/",i,"/ACP_climat/"), full.names = T, ".tif"))
    #   stack = stack(stack, stack_ACPclimat)
    # }
    
    # Utiliser terra pour garder les noms de chaque variables dans la stack
    stack <- rast(stack)
    
    noms_vars = paste(names(stack), collapse = "\n- ")
    cat(paste0("\nLa stack de la variable ", nom_dim," pour le mois de ",i," contient les variables : \n- ",noms_vars))
    # terra::writeRaster(stack,
    #                    paste0(output_path,"/stack_dim/",nom_dim,"/",i,"/stack_vars_",nom_dim,"_",i,"_sansACP.tif"),
    #                    overwrite=TRUE)
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

# Fonction qui crée une stack de variables (tous les mois confondus et dim confondues)
# nécéssite d'avoir au préalable fait tourner CreateStackDimSeason
CreateGlobalStack <- function(list_dims, periode){
  # # TEST
  # list_dims = c("CA","B","PV","CS","D","I")
  # periode = c("mai","juin","juillet","aout","septembre")
  
# vérifier que CreateStackDimSeason a bien tourné
  length_dirs <- length(list.dirs(paste0(output_path,"/stack_dim/"),recursive=F))
  length_dims <- length(list_dims)
  
  try(if(length_dims != length_dirs) stop("You must run CreateStackDimSeason function before.\n"))
  
  # Créer espace de stockage
  if(!dir.exists(paste0(output_path,"/stack_dim_global/")))
  {dir.create(paste0(output_path,"/stack_dim_global/"),recursive = T)}
  
  # 1 - rbind de chaque stack mensuelle, par dimension
  rbindDim <- function(dim_to_rbind, periode=periode){
    # #TEST
    # dim_to_rbind = "B"
    
    # Charger des stacks de la dimension en cours
    path = paste0(output_path, "/stack_dim/",dim_to_rbind,"/")
    path_months = lapply(periode,function(x) paste0(path,x))
    path_stack_months = list.files(unlist(path_months),recursive=T, ".tif$|.TIF", full.names = T)
    # S'assurer de la conformité des variables
    lapply(path_stack_months, AjustExtCRS)
    liste_stacks = lapply(path_stack_months, stack)
    liste_dt_stacks = lapply(liste_stacks, function(x) as.data.frame(data.table(as.data.frame(x))))
    coords = coordinates(liste_stacks[[1]])
    liste_dt_stacks = lapply(liste_dt_stacks, function(x) cbind(coords, x))
    # Ajouter une colonne mois
    periode_reord = periode[order(periode)]
    for(i in 1:length(liste_dt_stacks)){
      liste_dt_stacks[[i]] = cbind(liste_dt_stacks[[i]],month=rep(periode_reord[i],dim(liste_dt_stacks[[i]])[1]))
    }
    # S'assurer qu'il y ait les bons noms de colonnes
    liste_dt_stacks = lapply(liste_dt_stacks, function(x) cleanVarName(liste_nom_var_ref = table_variables$Nom, 
                                                     dt_to_clean = x))
    # rbind sur tous les mois
    dt_stacks =  do.call(rbind, liste_dt_stacks)
    cat(paste0("Dimension ", dim_to_rbind," traitée.\n"))
    return(dt_stacks)
  }
  dfs_dim_rbind = lapply(list_dims, function(x) rbindDim(x, periode))
  # 2 - merge de toutes les stacks, by x,y,month
  dt_env_periode = dfs_dim_rbind  %>% reduce(left_join, by = c("x","y","month"))
  # Nan <- Na
  dt_env_periode[is.na(dt_env_periode)] = NA
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  extr_tb = table_variables[table_variables$Nom %in% names(dt_env_periode), ]
  liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
  dt_env_periode[liste_nom_var_quali] <- lapply(dt_env_periode[liste_nom_var_quali] , factor)
  # Passer la variable mois en facteur
  dt_env_periode$month <- as.factor(dt_env_periode$month)
  # sauvegarde df
  write.csv(dt_env_periode, paste0(output_path,"/stack_dim_global/data_env_5months.csv"))
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
path_table_variables <- paste0(input_path,"/liste_variables.csv")

### Programme -------------------------------------
table_variables <- fread(path_table_variables)

# Stack des variables par dimension et par saison
lapply(liste_dimensions,CreateStackDimSeason)

# une stack globale (à travers toutes les dimensions et les mois)
CreateGlobalStack(list_dims = liste_dimensions,
                  periode = c("mai","juin","juillet","aout","septembre"))
