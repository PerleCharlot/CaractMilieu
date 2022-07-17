### Titre -------------------------------------
# Nom : ACP du milieu 
# Auteure : Perle Charlot
# Date de création : 04-06-2022
# Dates de modification : 17-07-2022

### Librairies -------------------------------------

library(raster)
library(data.table)
library(FactoMineR)
library(factoextra)
library(patchwork)

### Fonctions -------------------------------------

# Fonction qui calcule une FAMD, pour une dimension, pour une saison
makeFAMD <- function(table_donnees, saison, dimension){
  # # TEST
  # table_donnees = dt_stack
  # saison = i
  
  # Retirer x et y 
  tbl_data = subset(table_donnees,select=-c(x,y))
  
  t = try(FAMD(tbl_data , graph = FALSE, ncp = 3))
  if(inherits(t, "try-error")) {
    # PCA si seulement quanti
    res.famd <- PCA(tbl_data , graph = FALSE, ncp = 3)
  } else{  # FAMD si miste quali/quanti
    res.famd <- FAMD(tbl_data , graph = FALSE, ncp = 3)}
  
  # % variance expliquée par axe
  graph_var_expl <- fviz_screeplot(res.famd, main ="éboulis des valeurs")
  # Graphique des variables
  options(ggrepel.max.overlaps = Inf)
  graph_var <- fviz_famd_var(res.famd, "var", col.var = "cos2",
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                             repel = TRUE)
  # fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
  #               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  #               repel = FALSE)
  
  # Contribution aux axes 1 et 2, par variable
  graph_contrib_var_axe1 <- fviz_contrib(res.famd, "var", axes = 1)
  graph_contrib_var_axe2 <- fviz_contrib(res.famd, "var", axes = 2)
  # Graphique des individus
  
  t2 = try(fviz_famd_ind(res.famd,  alpha.ind = 0.05,
                         geom=c("point"),
                         repel = FALSE))
  if(inherits(t2, "try-error")) {
    graph_ind <- fviz_pca_ind(res.famd,  alpha.ind = 0.05,
                              geom=c("point"),
                              repel = FALSE)
  } else{  
    graph_ind <- fviz_famd_ind(res.famd,  alpha.ind = 0.05,
                               geom=c("point"),
                               repel = FALSE)}
  
  # Création et Sauvegarde des graphiques
  p1 = plot(graph_var_expl) 
  p2 = plot(graph_contrib_var_axe1) 
  p3 = plot(graph_contrib_var_axe2)
  p4 = p1 / (p2 | p3)
  png(file=paste0(output_path,"/ACP/",dimension,"/",saison,"/contrib_",dimension,"_",saison,".png"), 
      width=1400, height=800)
  print(p4)
  dev.off()
  
  p5 = graph_var
  p6 = graph_ind
  p7 = p5 | p6
  png(file=paste0(output_path,"/ACP/",dimension,"/",saison,"/graph_",dimension,"_",saison,".png"), 
      width=1400, height=800)
  print(p7)
  dev.off()
  
  # valeurs des axes, par pixels
  # FAMD_tbl <-as.data.frame(res.famd$svd$U)
  FAMD_tbl <-as.data.frame(res.famd$ind$coord)
  names(FAMD_tbl) = paste0("axe",seq(1:length(colnames(FAMD_tbl))))
  table_all <- cbind(table_donnees,FAMD_tbl)
  write.csv(table_all, paste0(output_path,"/ACP/",dimension,"/",saison,"/tblFAMD_",dimension,"_",saison,".csv"))
  # Création des rasters des axes 1, 2 et 3 de la FAMD
  rast_axe1 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe1))
  rast_axe2 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe2))
  rast_axe3 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe3))
  writeRaster(rast_axe1, paste0(output_path,"/ACP/",dimension,"/",saison,"/axe1_",dimension,"_",saison,".tif"), overwrite=T)
  writeRaster(rast_axe2, paste0(output_path,"/ACP/",dimension,"/",saison,"/axe2_",dimension,"_",saison,".tif"), overwrite=T)
  writeRaster(rast_axe3, paste0(output_path,"/ACP/",dimension,"/",saison,"/axe3_",dimension,"_",saison,".tif"), overwrite=T)
  
}


# fonction qui fait tout ... 
fct_FAMD <- function(dimension, periode=c("mai",'juin','juillet','aout','septembre')){
  
  # # TEST
  dimension = liste.dim[1]
  periode = c("mai",'juin','juillet','aout','septembre')
  
  # Fonctionnement par période
  for(i in periode){
    # TEST
    i = periode[1]
     
    stack_dim <- stack(list.files(paste0(path_dos_stack,dimension,"/",i),".tif", full.names = T))
    # Transformation en table
    dt_stack <- as.data.frame(data.table(as.data.frame(stack_dim)))
    dt_stack <- cbind(dt_stack,coordinates(stack_dim))
    # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
    dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
    # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
    
    # Si au moins un nom de variable de la stack n'est pas trouvé dans la liste des variables
    noms_variables = names(dt_stack)[! names(dt_stack) %in% c("x","y")]
    if(any(!noms_variables %in% table_variables$Nom)){
      noms_bug = noms_variables[!noms_variables %in% table_variables$Nom]
      cat(paste0("Bug(s) sur le(s) nom(s) : \n- ",paste(noms_bug, collapse ="\n- ")))
      # A la mano
      if(any(grepl("temps_acces", noms_bug))){
        names(dt_stack)[grepl("temps_acces", names(dt_stack))] = "temps_acces"
      }
      if(any(grepl("abondance_feuillage", noms_bug))){
        names(dt_stack)[grepl("abondance_feuillage", names(dt_stack))] = "abondance_feuillage"
      }
      if(any(grepl("NDVI", noms_bug))){
        names(dt_stack)[grepl("NDVI", names(dt_stack))] = "NDVI"
      }
      if(any(grepl("P_ETP", noms_bug))){
        names(dt_stack)[grepl("P_ETP", names(dt_stack))] = "P_ETP"
      }
      if(any(grepl("ht_physio_max", noms_bug))){
        names(dt_stack)[grepl("ht_physio_max", names(dt_stack))] = "ht_physio_max"
      }
      if(any(grepl("diffT__dif_tmean", noms_bug))){
        names(dt_stack)[grepl("diffT", names(dt_stack))] = " diffT"
      }
    }

    # Vérifier la nature des variables (si qualitative, coder en facteur)
    extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
    liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
    dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
    cat(paste0("\nPréparation table des variables pour dimension ",dimension,
               " pour le mois de ",i," effectuée.\n"))
    # str(dt_stack)
    
    makeFAMD(dt_stack,i, dimension)
    
    #
  }
  
  # # Chargement d'une stack d'une dimension pour une saison
  # dim_ete <- paste0(path_dos_stack,dimension,"_ete.tif")
  # dim_hiv <- paste0(path_dos_stack,dimension,"_hiv.tif")
  # # Création des stacks
  # stack_dim_ete <- stack(dim_ete)
  # #plot(stack_dim_ete)
  # stack_dim_hiv <- stack(dim_hiv)
  # # Transformation en table
  # dt_stack_ete <- as.data.frame(data.table(as.data.frame(stack_dim_ete)))
  # dt_stack_ete <- cbind(dt_stack_ete,coordinates(stack_dim_ete))
  # dt_stack_hiv <- as.data.frame(data.table(as.data.frame(stack_dim_hiv)))
  # dt_stack_hiv <- cbind(dt_stack_hiv,coordinates(stack_dim_hiv))
  # # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
  # dt_stack_ete <- dt_stack_ete[complete.cases(dt_stack_ete),]
  # dt_stack_hiv <- dt_stack_hiv[complete.cases(dt_stack_hiv),]
  # names(dt_stack_ete) <- sub("_ete","",names(dt_stack_ete))
  # names(dt_stack_hiv) <- sub("_hiv|_hiver","",names(dt_stack_hiv))
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  # extr_tb = table_variables[table_variables$Nom %in% sub("_ete","",names(dt_stack_ete)), ]
  # liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
  # dt_stack_ete[liste_nom_var_quali] <- lapply(dt_stack_ete[liste_nom_var_quali] , factor)
  # dt_stack_hiv[liste_nom_var_quali] <- lapply(dt_stack_hiv[liste_nom_var_quali] , factor)
  # str(dt_stack_ete)
  # str(dt_stack_hiv)
  # makeFAMD(dt_stack_ete,"ete")
  # makeFAMD(dt_stack_hiv,"hiv")
  
}

# fonction FAMD sur toutes les variables en meme temps
fct_FAMD_all <- function(periode=c("mai",'juin','juillet','aout','septembre')){
  
  # # # TEST
  # periode = c("mai",'juin','juillet','aout','septembre')
  
  # Fonctionnement par période
  for(i in periode){
    # # TEST
    # i = periode[1]
    
    dirs_mois = list.dirs(paste0(path_dos_stack))[grep(i, list.dirs(paste0(path_dos_stack)))] 
    stack_mois <- stack(lapply(list.files(dirs_mois,".tif", full.names = T),stack))
    # Transformation en table
    dt_stack <- as.data.frame(data.table(as.data.frame(stack_mois)))
    dt_stack <- cbind(dt_stack,coordinates(stack))
    # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
    dt_stack <- dt_stack[complete.cases(dt_stack),]     # 211 200 pixels --> 98 967
    # Ré-écrire correctement le nom des variables (si jamais du superflu traine)
    
    # Si au moins un nom de variable de la stack n'est pas trouvé dans la liste des variables
    noms_variables = names(dt_stack)[! names(dt_stack) %in% c("x","y")]
    if(any(!noms_variables %in% table_variables$Nom)){
      noms_bug = noms_variables[!noms_variables %in% table_variables$Nom]
      cat(paste0("Bug(s) sur le(s) nom(s) : \n- ",paste(noms_bug, collapse ="\n- ")))
      # A la mano
      if(any(grepl("temps_acces", noms_bug))){
        names(dt_stack)[grepl("temps_acces", names(dt_stack))] = "temps_acces"
      }
      if(any(grepl("abondance_feuillage", noms_bug))){
        names(dt_stack)[grepl("abondance_feuillage", names(dt_stack))] = "abondance_feuillage"
      }
      if(any(grepl("NDVI", noms_bug))){
        names(dt_stack)[grepl("NDVI", names(dt_stack))] = "NDVI"
      }
      if(any(grepl("P_ETP", noms_bug))){
        names(dt_stack)[grepl("P_ETP", names(dt_stack))] = "P_ETP"
      }
      if(any(grepl("ht_physio_max", noms_bug))){
        names(dt_stack)[grepl("ht_physio_max", names(dt_stack))] = "ht_physio_max"
      }
      if(any(grepl("diffT__dif_tmean", noms_bug))){
        names(dt_stack)[grepl("diffT", names(dt_stack))] = " diffT"
      }
    }
    
    # Vérifier la nature des variables (si qualitative, coder en facteur)
    extr_tb = table_variables[table_variables$Nom %in% names(dt_stack), ]
    liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
    dt_stack[liste_nom_var_quali] <- lapply(dt_stack[liste_nom_var_quali] , factor)
    cat(paste0("\nPréparation table des variables pour le mois de ",i," effectuée.\n"))
    #str(dt_stack)
    
    # # ajouter la dimension d'appartenance
    # merge(dt_stack, table_variables)
    
    makeFAMD(dt_stack,i, dimension = "toutes")
    
  }
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
liste.dim=  c("CA","B","PV","CS","D","I")

#### Données spatiales ####

path_dos_stack <- paste0(output_path,"/stack_dim/")

#### Tables ####
path_table_variables <- paste0(input_path,"/liste_variables.csv")

### Programme -------------------------------------

##### FAMD/ACP par dimension par saison ####

table_variables <- fread(path_table_variables)

lapply(liste.dim, fct_FAMD)

##### FAMD sur toutes les dimensions simultanément, par saison ####
fct_FAMD_all()

##### t-SNE par dimension par saison ####
library(Rtsne)

# stack_matrix <- as.matrix(dt_stack[,1:21])
# Set a seed if you want reproducible results
set.seed(42)
tsne_out <- Rtsne(dt_stack[,1:21],partial_pca=T) # Run TSNE
# Show the objects in the 2D tsne representation
plot(tsne_out$Y, asp=0,alpha=0.1)

library(ggplot2)
DATA = as.data.frame(tsne_out$Y)
ggplot(DATA, aes(x=V1,y=V2))+
  geom_point(alpha=0.05)



# Tips Natéo pour visualiser ACP
library(Factoshiny)
Factoshiny(dt_stack_ete)


# inspiration rpz d'un espèce écologique et individus sur ACP
# https://iboulangeat.github.io/pdfs/Boulangeat2012_EcolLet_published.pdf


# TODO : dudi.mix par rapport à FAMD ???
# https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.mix

