### Titre -------------------------------------
# Nom : ACP du milieu 
# Auteure : Perle Charlot
# Date de création : 04-06-2022
# Dates de modification : 07-06-2022

### Librairies -------------------------------------

library(raster)
library(data.table)
library(FactoMineR)
library(factoextra)

### Fonctions -------------------------------------


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

function(liste_dimensions){
  
  # TEST
  liste_dimensions = liste.dim[2]
  
  
  # Chargement d'une stack d'une dimension pour une saison
  dim_ete <- paste0(path_dos_stack,liste_dimensions,"_ete.tif")
  dim_hiv <- paste0(path_dos_stack,liste_dimensions,"_hiv.tif")
  # Création des stacks
  stack_dim_ete <- stack(dim_ete)
  stack_dim_hiv <- stack(dim_hiv)
  # Transformation en table
  dt_stack_ete <- data.table(as.data.frame(stack_dim_ete))
  dt_stack_hiv <- data.table(as.data.frame(stack_dim_hiv))
  dt_stack_ete <- dt_stack_ete[complete.cases(dt_stack_ete),]
  dt_stack_hiv <- dt_stack_hiv[complete.cases(dt_stack_hiv),]
  
  # Vérifier la nature des variables (si qualitative, coder en facteur)
  str(dt_stack)
  
  which(table_variables$Nature[table_variables])
  if(){
    
  }

  dt_stack$abondance_feuillage_ete = as.factor(dt_stack$abondance_feuillage_ete)
  
  
}


# TODO : sortir une liste avec la nom des variables et leur nature (quantitative ou qualitative)
# pour pouvoir vérifier et recoder une variable en facteur


str(dt_stack)
# FAMD
res.famd <- FAMD(dt_stack, graph = FALSE)
fviz_screeplot(res.famd)
# Graphique des variables
fviz_famd_var (res.famd, repel = TRUE)
# Contribution à la première dimension
fviz_contrib (res.famd, "var", axes = 1)
fviz_contrib (res.famd, "var", axes = 2)


quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var 

# Couleur par valeurs cos2: qualité sur le plan des facteurs
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)

fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_famd_var(res.famd, "quali.var", col.var = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

ind <- get_famd_ind(res.famd)
ind

fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)

# inspiration rpz d'un espèce écologique et individus sur ACP
# https://iboulangeat.github.io/pdfs/Boulangeat2012_EcolLet_published.pdf


# TODO : dudi.mix par rapport à FAMD ???
# https://www.rdocumentation.org/packages/ade4/versions/1.7-19/topics/dudi.mix


#####  Analyse multifactorielles sur toutes les variables par saison ####
