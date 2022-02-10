
#++++++++++++++++++++++++++++++++++++++++++++++
#   Fonction d'extraction   ####
#++++++++++++++++++++++++++++++++++++++++++++++


extract_meteo <- function(path_data_allslopes, years, variables, 
                                     safran_classes_table=NULL,
                                     # crocus_alti=NULL, 
                                     # crocus_aspect=NULL, 
                                     # crocus_slope=NULL,
                                     # cellsIndex=NULL, 
                                     liste_combinaisons,
                                     jours_hiver, jours_ete,
                                     num_massif){
  # path_data_allslopes = chemin vers le dossier contenant les fichiers netcdf
  # years = années sur lesquelles est lancé le calcul
  # variables = variables climatiques modélisées disponibles dans les netcdf, et nécessaires pour les calculs de variables voulues (ex : température moyenne saisonnière)
  # safran_classes_table = safran_classe : tableau des classes altitude (zs) - aspect - pente - massif, avec le numéro de code correspondant (Number_of_points)
  # crocus_alti = raster des altitudes, en classes correspondant aux netcdf
  # crocus_aspect = raster des aspects, en classes correspondant aux netcdf
  # crocus_slope = raster des pentes, en classes correspondant aux netcdf
  # cellsIndex = index issu du masque
  # jours_hiver = dates limites de la période dite hivernale (ex : pour le 1 janvier-31 mars on écrira c("2017-01-01","2017-03-01") (mettre n'importe quelle année non bisextile et faire en sorte que les durées été-hiver soient identiques))
  # jours_ete = dates limites de la période dite estivale (ex : pour le 15 juin-15 septembre on écrira c("2017-06-15","2017-09-15") (mettre n'importe quelle année non bisextile et faire en sorte que les durées été-hiver soient identiques))

  
  # TEST
  path_data_allslopes = chemin_fichiersncdf
  years = years_to_use
  variables = var_to_use
  safran_classes_table = safran_classes_complet
  liste_combinaisons = liste_combinaisons_to_use
  jours_ete = jours_ete_to_use
  jours_hiver = jours_hiver_to_use
  
  
  # Liste des libraries requises
  require(tidync)
  require(ncmeta)
  require(tidyr)
  require(lubridate)

  # Vérification de la présence des données netcdf dans le dossier indiqué ----
  if(!(length(list.files(path_data_allslopes)) > 0) ) stop("not able to connect to data path or empty path")


  # Sélection spatiale ----
  if(is.null(safran_classes_table)) stop ("missing safran massif table")

  selected_points <- as.numeric(liste_combinaisons)


  # Boucle temporelle ----

  pt_var_annee = array(data=NA, dim=c(length(unique(selected_points)),20,length(years))) # Mettre à jour le nombre de variables calculées (nombre de colonnes du dataframe : 3 pour le test)

  # initialisation et récupération de la première année
  init_annee = 0
  pro_file = paste0(path_data_allslopes, "/PRO_",as.character(years[1]-1),"080106_",as.character(years[1]),"080106.nc")
  src <- tidync(pro_file)
  data_y1 <- src %>% activate("D0,D3") %>%  hyper_tibble(select_var = variables)
  data_y1$TG4 <- (src %>% activate("D0,D2,D3") %>%  hyper_tibble(select_var = "TG4"))$TG4
  data_y1 = data_y1[data_y1$Number_of_points %in% selected_points,]

  # initialisation pour sélectionner les bons n° de jours correspondant aux périodes hivernales et estivales
  deb_hiver = time_length(interval(start = ymd("2016-08-01"), end = jours_hiver[1]), unit = "days")
  fin_hiver = time_length(interval(start = ymd("2016-08-01"), end = jours_hiver[2]), unit = "days")
  deb_ete = time_length(interval(start = ymd("2016-08-01"), end = jours_ete[1]), unit = "days")
  fin_ete = time_length(interval(start = ymd("2017-08-01"), end = jours_ete[2]), unit = "days")


  for (y in years){
    
    # # TEST
    # y = years[1]
    
    
    cat(paste0("\nExtraction en cours de l'année ",y))

    # lecture des données et extraction pour les années y et y+1 (on a besoin de 2 années consécutives pour pouvoir calculer des valeurs moyennes et cumulées estivales, puisque l'année est coupée au 1er août)
    pro_file = paste0(path_data_allslopes, "/PRO_",as.character(y),"080106_",as.character(y+1),"080106.nc")
    src <- tidync(pro_file)
    data_y2 <- src %>% activate("D0,D3") %>%  hyper_tibble(select_var = variables)
    data_y2$TG4 <- (src %>% activate("D0,D2,D3") %>%  hyper_tibble(select_var = "TG4"))$TG4
    data_y2 = data_y2[data_y2$Number_of_points %in% selected_points,]

    # sur les 2 tables ainsi obtenues, on sélectionne l'hiver et le début d'été (jusqu'au 31 juillet = jour 365) de la première
    #                                                 la fin d'été (1 août - 14 septembre = jour 44) de la seconde table

    data_hiver = data_y1[which(data_y1$time >= deb_hiver & data_y1$time<=fin_hiver),]
    data_ete = rbind(data_y1[which(data_y1$time >= deb_ete),],data_y2[which(data_y2$time <= fin_ete),])

    init_annee = init_annee + 1
    for(no_point in 1:length(unique(selected_points))){
      
      # #TEST
      # no_point = 1

      #*---- colonne 1 : Température moyenne hivernale de l'air sur la période ----
      # (la température correspond à la couche d'air au-dessus du sol ou de la neige)
      pt_var_annee[no_point,1,init_annee] = mean(data_hiver$TS_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point])])

      #*---- colonne 2 : Température moyenne estivale de l'air sur la période ----
      # (la température correspond à la couche d'air au-dessus du sol ou de la neige)
      pt_var_annee[no_point,2,init_annee] = mean(data_ete$TS_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point])])

      #*---- colonne 3 : Enneigement moyen hivernal sur la période ----
      # /!\ prendre plutôt l'enneigement cumulé ?? NON on n'a pas les chutes de neige... DSN_T_ISBA prend déjà en compte les phénomènes de fonte, tassement ----
      pt_var_annee[no_point,3,init_annee] = mean(data_hiver$DSN_T_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point])])

      #*---- colonne 4 : Enneigement moyen estival sur la période ----
      # /!\ prendre plutôt l'enneigement cumulé ?? NON on n'a pas les chutes de neige... DSN_T_ISBA prend déjà en compte les phénomènes de fonte, tassement ----
      pt_var_annee[no_point,4,init_annee] = mean(data_ete$DSN_T_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point])])

      #*---- colonne 5 : date de déneigement après la période d'enneigement continu ----
      # /!\ Hypothèse forte : il y a toujours de la neige au 200ème jour !! OK d'après vérif ----
      date_deneig = min(data_y1$time[data_y1$DSN_T_ISBA==0 & data_y1$time >200 & data_y1$Number_of_points==unique(selected_points)[no_point]])
      pt_var_annee[no_point,5,init_annee] = date_deneig

      #*---- colonne 6 : durée d'enneigement continu ----
      # /!\ Hypothèse forte : il y a toujours de la neige au 200ème jour !! OK d'après vérif ----
      date_enneig = max(data_y1$time[data_y1$DSN_T_ISBA==0 & data_y1$time <200 & data_y1$Number_of_points==unique(selected_points)[no_point]])
      pt_var_annee[no_point,6,init_annee] = date_deneig - date_enneig

      #*---- colonne 7 : nombre de jours de gel sévère (température de l'air inférieure à -5°C, impactant la productivité du milieu) sur la période déneigée  -----
      # On prend donc les dates de fin d'enneigement et de début d'enneigement
      pt_var_annee[no_point,7,init_annee] = length(data_y1$TS_ISBA[which(data_y1$time > date_deneig & data_y1$TS_ISBA < 273.15 - 5 &
                                                                           data_y1$Number_of_points == unique(selected_points)[no_point] )]) +
        length(data_y2$TS_ISBA[which(data_y2$time < date_enneig & data_y2$TS_ISBA < 273.15 - 5 &
                                       data_y2$Number_of_points == unique(selected_points)[no_point] )])

      #*---- colonne 8 : nombre de jours de gel (température du sol inférieure à 0°C) sur la période hivernale de 90j ----
      pt_var_annee[no_point,8,init_annee] = length(data_hiver$TG4[which(data_hiver$Number_of_points == unique(selected_points)[no_point] & data_hiver$TG4 < 273.15 )])

      #*---- colonne 9 : nombre de jours de gel (température du sol inférieure à 0°C) sur la période estivale de 90j ----
      pt_var_annee[no_point,9,init_annee] = length(data_ete$TG4[which(data_ete$Number_of_points == unique(selected_points)[no_point] & data_ete$TG4 < 273.15 )])


      #*---- colonne 10 : GDD sans neige ----
      # On additionne les températures du sol à 8cm de profondeur à partir de la date de fin de l'enneigement continu, et uniquement pour les jours où il n'y a pas de neige et où la température est supérieure à 5.5 °C
      # /!\ on doit aussi considérer la période 1er août - début enneigement
      pt_var_annee[no_point,10,init_annee] = sum(data_y1$TG4[which(data_y1$time > date_deneig & data_y1$DSN_T_ISBA == 0 &
                                                                    data_y1$TG4 > 273.15+5.5 & data_y1$Number_of_points==unique(selected_points)[no_point])] - (273.15+5.5)) +
        sum(data_y2$TG4[which(data_y2$time < date_enneig & data_y2$DSN_T_ISBA == 0 &
                                data_y2$TG4 > 273.15+5.5 & data_y2$Number_of_points==unique(selected_points)[no_point])] - (273.15+5.5))

      #*----- colonne 11 : radiation moyenne hivernale -----
      pt_var_annee[no_point,11,init_annee] = mean(data_hiver$RN_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point])])

      #*----- colonne 12 : radiation moyenne estivale -----
      pt_var_annee[no_point,12,init_annee] = mean(data_ete$RN_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point])])

      #*----- colonne 13 : précipitations cumulées sur la période de végétation (de la fin de l'enneigement au début de l'enneigement) -----
      pt_var_annee[no_point,13,init_annee] = sum(data_y1$RAINF_ISBA[which(data_y1$time > date_deneig & data_y1$Number_of_points==unique(selected_points)[no_point])] ) +
        sum(data_y2$RAINF_ISBA[which(data_y2$time < date_enneig & data_y2$Number_of_points==unique(selected_points)[no_point])] )

      #*----- colonne 14 : nombre de jours de précipitations en hiver -----
      pt_var_annee[no_point,14,init_annee] = length(data_hiver$RAINF_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point] & data_hiver$RAINF_ISBA > 0 )])

      #*----- colonne 15 : nombre de jours de précipitations en été -----
      pt_var_annee[no_point,15,init_annee] = length(data_ete$RAINF_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point] & data_ete$RAINF_ISBA > 0)])

      #*---- AJOUTER DES COLONNES / VARIABLES CLIMATIQUES SUPPLEMENTAIRES ----
      
      #*----- colonne 16 : quantile 25% température de l'air en été -----
      pt_var_annee[no_point,16,init_annee] = unname(quantile(data_ete$TS_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point])])[2])
      
      #*----- colonne 17 : quantile 75% température de l'air en été -----
      pt_var_annee[no_point,17,init_annee] = unname(quantile(data_ete$TS_ISBA[which(data_ete$Number_of_points == unique(selected_points)[no_point])])[4])
      
      #*----- colonne 18 : quantile 25% température de l'air en hiver -----
      pt_var_annee[no_point,18,init_annee] = unname(quantile(data_hiver$TS_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point])])[2])
      
      #*----- colonne 19 : quantile 75% température de l'air en hiver -----
      pt_var_annee[no_point,19,init_annee] = unname(quantile(data_hiver$TS_ISBA[which(data_hiver$Number_of_points == unique(selected_points)[no_point])])[4])

      
      #*----- colonne 20 : NoP -----
      pt_var_annee[no_point,20,init_annee] =  data_hiver$Number_of_points[no_point]
      
      
    } # fin de la boucle de calcul & remplissage de l'array par point unique alti-aspect-slope

    # on conserve la 2ème table pour y récupérer l'hiver et le début d'été
    data_y1 = data_y2
    saveRDS(data_y1, file = paste0(output_path,"/data_y1.rds"))
    saveRDS(pt_var_annee, file = paste0(output_path,"/pt_var_annee.rds"))

  } # fin de la boucle de calcul & remplissage de l'array par année
  dimnames(pt_var_annee) = list(unique(selected_points),
                                c("Tmoy_hiver","Tmoy_ete",
                                  "Enneigmoy_hiver","Enneigmoy_ete","date_deneig","duree_neige_continue",
                                  "nb_jr_gel_moins5_air_periodeveg","nb_jr_gel_0_sol_hiver","nb_jr_gel_0_sol_ete",
                                  "GDD_5.5_sansneige",
                                  "radiation_nette_moy_hiver","radiation_nette_moy_ete"
                                  ,"precipitation_cumulees_saison_veg","nb_jr_precip_hiver","nb_jr_precip_ete",
                                  "Tquant25_ete","Tquant75_ete","Tquant25_hiver","Tquant75_hiver",
                                  "NoP"
                                  ), # /!\ Liste à ajuster
                                years)


  # Construction de l'output : moyennes interannuelles ----
  output1 = apply(pt_var_annee,c(1,2),mean) # calcul des variables interannuelles
  output = as.data.frame(output1)

  return(output)

}
