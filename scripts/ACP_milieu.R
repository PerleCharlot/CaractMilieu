### Titre -------------------------------------
# Nom : ACP du milieu 
# Auteure : Perle Charlot
# Date de création : 04-06-2022
# Dates de modification : 20-06-2022

### Librairies -------------------------------------

library(raster)
library(data.table)
library(FactoMineR)
library(factoextra)
library(patchwork)

### Fonctions -------------------------------------

# Fonction qui calcule une FAMD, pour une dimension, pour une saison
makeFAMD <- function(table_donnees, saison){
  # # TEST
  # table_donnees = dt_stack_ete
  # saison = "ete"
  
  # Retirer x et y 
  tbl_data = subset(table_donnees,select=-c(x,y))
  
  # FAMD
  res.famd <- FAMD(tbl_data , graph = FALSE, ncp = 3)
  # % variance expliquée par axe
  graph_var_expl <- fviz_screeplot(res.famd, main ="éboulis des valeurs")
  # Graphique des variables
  graph_var <- fviz_famd_var(res.famd, "var", col.var = "cos2",
                             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                             repel = FALSE)
  # fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
  #               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
  #               repel = FALSE)
  
  # Contribution aux axes 1 et 2, par variable
  graph_contrib_var_axe1 <- fviz_contrib(res.famd, "var", axes = 1)
  graph_contrib_var_axe2 <- fviz_contrib(res.famd, "var", axes = 2)
  # Graphique des individus
  graph_ind <- fviz_famd_ind(res.famd,  alpha.ind = 0.1,
                             repel = FALSE)
  
  # Création et Sauvegarde des graphiques
  p1 = plot(graph_var_expl) 
  p2 = plot(graph_contrib_var_axe1) 
  p3 = plot(graph_contrib_var_axe2)
  p4 = p1 / (p2 | p2)
  png(file=paste0(output_path,"/ACP/",dimension,"/",dimension,"_contrib_",saison,".png"), 
      width=1400, height=800)
  print(p4)
  dev.off()
  p5 = graph_var
  p6 = graph_ind
  p7 = p5 | p6
  png(file=paste0(output_path,"/ACP/",dimension,"/",dimension,"_graph_",saison,".png"), 
      width=1400, height=800)
  print(p7)
  dev.off()
  
  # valeurs des axes, par pixels
  FAMD_tbl <-as.data.frame(res.famd$svd$U)
  names(FAMD_tbl) = c("axe1","axe2","axe3")
  table_all <- cbind(table_donnees,FAMD_tbl)
  write.csv(table_all, paste0(output_path,"/ACP/",dimension,"/",dimension,"_tblFAMD_",saison,".csv"))
  # Création des rasters des axes 1, 2 et 3 de la FAMD
  rast_axe1 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe1))
  rast_axe2 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe2))
  rast_axe3 <- rasterFromXYZ(cbind(table_all$x, table_all$y, table_all$axe3))
  writeRaster(rast_axe1, paste0(output_path,"/ACP/",dimension,"/",dimension,"_axe1_",saison,".tif"))
  writeRaster(rast_axe2, paste0(output_path,"/ACP/",dimension,"/",dimension,"_axe2_",saison,".tif"))
  writeRaster(rast_axe3, paste0(output_path,"/ACP/",dimension,"/",dimension,"_axe3_",saison,".tif"))
  
}


# fonction qui fait tout ... 
fct_FAMD <- function(dimension){
  
  # # TEST
  dimension = liste.dim[1]
  
  # Chargement d'une stack d'une dimension pour une saison
  dim_ete <- paste0(path_dos_stack,dimension,"_ete.tif")
  dim_hiv <- paste0(path_dos_stack,dimension,"_hiv.tif")
  # Création des stacks
  stack_dim_ete <- stack(dim_ete)
  #plot(stack_dim_ete)
  stack_dim_hiv <- stack(dim_hiv)
  # Transformation en table
  dt_stack_ete <- as.data.frame(data.table(as.data.frame(stack_dim_ete)))
  dt_stack_ete <- cbind(dt_stack_ete,coordinates(stack_dim_ete))
  dt_stack_hiv <- as.data.frame(data.table(as.data.frame(stack_dim_hiv)))
  dt_stack_hiv <- cbind(dt_stack_hiv,coordinates(stack_dim_hiv))
  
  # Retirer les NA (quand calculé sur N2000 et pas emrpise carrée) : 211 200 pixels --> 50 320
  dt_stack_ete <- dt_stack_ete[complete.cases(dt_stack_ete),]
  dt_stack_hiv <- dt_stack_hiv[complete.cases(dt_stack_hiv),]
  names(dt_stack_ete) <- sub("_ete","",names(dt_stack_ete))
  names(dt_stack_hiv) <- sub("_hiv|_hiver","",names(dt_stack_hiv))
  
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  extr_tb = table_variables[table_variables$Nom %in% sub("_ete","",names(dt_stack_ete)), ]
  liste_nom_var_quali = extr_tb$Nom[which(extr_tb$Nature == "qualitative")]
  dt_stack_ete[liste_nom_var_quali] <- lapply(dt_stack_ete[liste_nom_var_quali] , factor)
  dt_stack_hiv[liste_nom_var_quali] <- lapply(dt_stack_hiv[liste_nom_var_quali] , factor)
  
  # str(dt_stack_ete)
  # str(dt_stack_hiv)
  
  makeFAMD(dt_stack_ete,"ete")
  makeFAMD(dt_stack_hiv,"hiv")
  
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

##### ACP par dimension par saison ####

table_variables <- fread(path_table_variables)



lapply(liste.dim, fct_FAMD)


# Tips Natéo pour visualiser ACP
library(Factoshiny)
Factoshiny(dt_stack_ete)

# # PCA
# res.pca <- PCA(dt_stack_ete, graph = FALSE)
# fviz_screeplot(res.pca) 
# fviz_famd_var(res.pca, repel = TRUE)
# fviz_pca_var(res.pca,col.var = "cos2",main="été")
# fviz_pca_var(res.pca,
#              col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
#              repel=TRUE)



# inspiration rpz d'un espèce écologique et individus sur ACP
# https://iboulangeat.github.io/pdfs/Boulangeat2012_EcolLet_published.pdf


# TODO : dudi.mix par rapport à FAMD ???
# https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.mix


#####  Analyse multifactorielles sur toutes les variables par saison ####
