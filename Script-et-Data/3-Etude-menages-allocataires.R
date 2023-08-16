library(tidyverse)
library(lubridate)
library(dplyr)
library(zoo)
library(data.table)

setwd("C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/Insertion/Programme_et_data_BRSA")

# Créer un objet vide pour stocker les données
Data <- data.frame()

# Boucle pour itérer sur les années de 2013 à 2023
for (annee in 2013:2023) {
  # Construire le nom du fichier CSV
  fichier <- paste0("DataBRSA/requete_", annee, ".csv")
  
  # Importer les données du fichier CSV
  requete <- read.csv2(file = fichier)
  
  # Ajouter les données à l'objet Data en effectuant une union
  Data <- bind_rows(Data, requete)
}

####MODIF 
Data <- Data %>%
  filter(DROIT_DEVOIR_FOY == "O")

#Liste individu erreur (à expliqué pourquoi , des allocatires qui changes de dossiers)
Liste_individu_erreur <- Data %>%
  filter(DATE_FIN_PRESENCE_RSA < DATE_DEBUT_PRESENCE_RSA)

#Supprimer les observations ou DATE_FIN_P_RSA < DATE_DEBUT_P_RSA
Data <- Data %>%
  filter(DATE_FIN_PRESENCE_RSA >= DATE_DEBUT_PRESENCE_RSA | is.na(DATE_FIN_PRESENCE_RSA))

#Liste individu erreur 2
Liste_individu_erreur2 <- Data %>%
  filter(difftime(as.Date(DATE_FIN_PRESENCE_RSA), as.Date(DATE_DEBUT_PRESENCE_RSA), units = "days") < 35)
         
#Supprimer les observations ou DATE_DEBUT_PRESENCE_RSA - DATE_FIN_PRESENCE_RSA < 1 mois
Data <- Data %>%
  filter(difftime(as.Date(DATE_FIN_PRESENCE_RSA), as.Date(DATE_DEBUT_PRESENCE_RSA), units = "days") >= 35 | is.na(DATE_FIN_PRESENCE_RSA))

#Liste individu erreur 3
Liste_individu_erreur3 <- Data %>%
  group_by(INDEX_INDIVIDU) %>%
  filter(n() == 1)

#Supprimer les lignes qui ont un seule enregistrement par individu 
Data <- Data %>%
  group_by(INDEX_INDIVIDU) %>%
  filter(n() > 1)

#Le ratio du montant d?pens? par le d?partement par rapport au revenu minimum garanti pour un foyer
Data <- Data %>%
  mutate(Ratio_montant.departement_revenu.minimum = ifelse(MTT_RMG_RSA != 0,(MTT_TOTAL_RSA /MTT_RMG_RSA) * 100,NA))

# Le ratio du montant totale dépensé par le département plus le montant totale MTT_RESS_MENS_RSA
#par rapport à la somme des revenu mensuel minimum garanti pour un foyer
#ca represente combien le depatement arrive à remplire le besoin minimum 
Data <- Data %>%
  mutate(Ratio_montant_complement = ifelse(MTT_RMG_RSA != 0, ((MTT_TOTAL_RSA + MTT_RESS_MENS_RSA) / MTT_RMG_RSA) * 100, NA))


#convertir NB_PERS_CHARGE en numerique
Data$NB_PERS_CHARGE <- as.numeric(Data$NB_PERS_CHARGE)

# Trier les donn?es selon INDEX_INDIVIDU et DATE_REFERENCE_FLUX en ordre d?croissant
Data <- Data %>%
  arrange(desc(INDEX_INDIVIDU), desc(DATE_REFERENCE_FLUX))

# Cr?er la nouvelle colonne NB_PERS_CHARGE_derniere_situation
Data <- Data %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(NB_PERS_CHARGE_derniere_situation = first(NB_PERS_CHARGE)) %>%
  ungroup()

#Filter la modalité 3 dans CODE_ETAT_DROIT_RSA
Data <- Data %>% filter(CODE_ETAT_DROIT_RSA %in% c(3))

# Créer la nouvelle variable Allocatiare_Conjoint
Data <- Data %>%
  mutate(Allocataire_Conjoint = case_when(
    INDEX_ALLOCATAIRE == INDEX_INDIVIDU ~ "Allocataire",
    INDEX_CONJOINT == INDEX_INDIVIDU ~ "Conjoint",
    TRUE ~ "Non_identifier"
  ))

# 
Data <- Data %>%
  filter(Allocataire_Conjoint == "Allocataire")

#Créer la variable DROIT_DEVOIR_FOY_ALL
Data <- Data %>%
  mutate(DROIT_DEVOIR_FOY_ALL = if_else(DROIT_DEVOIR_FOY == 'O' & ALL_DROIT_DEV == 1, 1, 0)) 

#premier individu par dossier 
Data_durees <- Data %>%   ##Data_dates
  group_by(INDEX_INDIVIDU) %>%
  mutate(Duree_Beneficiaire = n(),
         Duree_DD_Oui = sum(DROIT_DEVOIR_FOY_ALL == 1),  #sum(DROIT_DEVOIR_FOY_ALL == "O")
                                                                            #sum(DROIT_DEVOIR_FOY_ALL == "N")
         Duree_DD_Non = sum(DROIT_DEVOIR_FOY_ALL == 0),
         Duree_perception_RSA = sum(MTT_TOTAL_RSA != 0), 
         Duree_DD_Oui_An_mois = paste(floor(Duree_DD_Oui/12), "an(s)", Duree_DD_Oui %% 12, "mois"),
         Duree_DD_Non_An_mois = paste(floor(Duree_DD_Non/12), "an(s)", Duree_DD_Non %% 12, "mois"),
         Duree_perception_RSA_An_mois = paste(floor(Duree_perception_RSA/12), "an(s)", Duree_perception_RSA %% 12, "mois"),
         Montant_total_percu = sum(MTT_TOTAL_RSA),
         Montant_moyen_mensuel_percu = sum(MTT_TOTAL_RSA)/sum(MTT_TOTAL_RSA != 0))



#### 1 cette partie predn beaucoup de temps à s'executer
#Data_durees <- Data_durees %>%
#  group_by(INDEX_INDIVIDU) %>%
#  arrange(INDEX_INDIVIDU, DATE_REFERENCE_FLUX) %>%
#  mutate(
#    interruption = difftime(DATE_REFERENCE_FLUX, lag(DATE_REFERENCE_FLUX), units = "days"),
#    interruption = na.fill(interruption, fill = 0),
#    groupe = cumsum(ifelse(interruption > 6 * 30, 1, 0)),
#    groupe = groupe + cumsum(ifelse(interruption > 6 * 30, 1, 0)) * (interruption == 0)
#  ) %>%
#  ungroup()

# 2 cette logique s'execute dans quelques minutes 
# Convertir le dataframe en data.table
setDT(Data_durees)

# Indexer les colonnes utilisées pour le tri
setkey(Data_durees, INDEX_INDIVIDU, DATE_REFERENCE_FLUX)

# Conversion de la colonne DATE_REFERENCE_FLUX en date
#Data_durees[, DATE_REFERENCE_FLUX := as.Date(DATE_REFERENCE_FLUX)]

# Calculer l'interruption et le groupe
#Data_durees[, interruption := difftime(DATE_REFERENCE_FLUX, shift(DATE_REFERENCE_FLUX), units = "days")]
Data_durees[, interruption := difftime(DATE_REFERENCE_FLUX, lag(DATE_REFERENCE_FLUX), units = "days")]
Data_durees[, interruption := na.fill(interruption, fill = 0)]
Data_durees[, groupe := cumsum(ifelse(interruption > 6 * 30, 1, 0))]
Data_durees[, groupe := groupe + cumsum(ifelse(interruption > 6 * 30, 1, 0)) * (interruption == 0)]

# Supprimer les colonnes temporaires
#Data_durees[, c("interruption") := NULL]

# Rétablir l'ordre initial
setkey(Data_durees, INDEX_INDIVIDU, DATE_REFERENCE_FLUX)

####
Data_durees <- Data_durees  %>%
  group_by(INDEX_INDIVIDU, groupe) %>%
  mutate(DATE_DEBUT_P_RSA = min(DATE_REFERENCE_FLUX) ,
         DATE_FIN_P_RSA = max(DATE_REFERENCE_FLUX))

#Data_durees <- Data_durees %>%  mutate(
#    I_verif = difftime(DATE_DEBUT_P_RSA, lag(DATE_FIN_P_RSA), units = "days")
#  ) %>%
#  ungroup()

## Convertir le dataframe en data.table
#setDT(Data_durees)
#
## Trier le data.table par INDEX_INDIVIDU et DATE_REFERENCE_FLUX
#setkey(Data_durees, INDEX_INDIVIDU, DATE_REFERENCE_FLUX)
#
## Créer une variable pour marquer les premières lignes du groupe pour DATE_DEBUT_P_RSA
#Data_durees[, isFirstRow := c(1, diff(groupe) != 0), by = INDEX_INDIVIDU]
#
## Créer une variable pour marquer les dernières lignes du groupe précédent pour lag(DATE_FIN_P_RSA)
#Data_durees[, isLastRow := c(diff(groupe) != 0, 1), by = INDEX_INDIVIDU]
#
## Calculer la colonne I_verif uniquement pour les premières lignes du groupe pour DATE_DEBUT_P_RSA
#Data_durees[Data_durees$isFirstRow, I_verif := difftime(DATE_DEBUT_P_RSA, lag(DATE_FIN_P_RSA), units = "days")]
#
## Supprimer les variables temporaires
#Data_durees[, c("isFirstRow", "isLastRow") := NULL]


#Base_de_donnees 

Data_duree_1 <- Data_durees %>%
  group_by(INDEX_INDIVIDU) %>%
  #filter(INDEX_INDIVIDU == first(INDEX_INDIVIDU)) %>% 
  arrange(INDEX_DOSSIER, INDEX_INDIVIDU, DATE_REFERENCE_FLUX) %>%
  distinct(DATE_DEBUT_P_RSA, .keep_all = TRUE)

#MODIF

# INTERRUPTION ,DUREE_PRESENCE 
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  arrange(INDEX_DOSSIER, INDEX_INDIVIDU, DATE_DEBUT_P_RSA) %>%
  mutate(DATE_FIN_P_RSA = as.character(DATE_FIN_P_RSA)) %>%
  mutate(DATE_FIN_P_RSA = ifelse(is.na(DATE_FIN_P_RSA), as.character(ceiling_date(Sys.Date(), "month") - days(1)), DATE_FIN_P_RSA)) %>%
  mutate(DATE_FIN_P_RSA = as.Date(DATE_FIN_P_RSA))%>%
  mutate(INTERRUPTION = as.numeric(difftime(DATE_DEBUT_P_RSA, lag(DATE_FIN_P_RSA), units = "days")))

#INTERRUPTION_plus_grand_6_mois
Data_duree_1 <- Data_duree_1 %>%
  mutate(INTERRUPTION_plus_grand_6_mois = ifelse(INTERRUPTION > 6*30, 1, 0),
         INTERRUPTION_negative = ifelse(INTERRUPTION < 0, 1, 0))


#Base des beneficiaires avec de chevauchement de periode de presence
base_erreur_chevauchement <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  filter(any(INTERRUPTION_negative == 1))

#Supprimer les individus particuliers de la dataframe
Data_duree_1 <- anti_join(Data_duree_1, base_erreur_chevauchement, by = "INDEX_INDIVIDU")

#Traitement des cas particuliers erreur_chevauchement
#filtrer les individus erreur dans Data_durees 
Data_durees_filtered <- Data_durees %>%
  filter(INDEX_INDIVIDU %in% unique(base_erreur_chevauchement$INDEX_INDIVIDU))

#supprimer les anciennes colonnes 
Data_durees_filtered <- Data_durees_filtered  %>%
  dplyr::select(-interruption, -groupe, -DATE_DEBUT_P_RSA, -DATE_FIN_P_RSA, -Duree_perception_RSA,- Montant_total_percu, -Montant_moyen_mensuel_percu)

#filtrer les suivant les dates references flux avec le montant (MTT_TOTAL_RSA) minimal 
Data_durees_filtered <- Data_durees_filtered %>%
  group_by(INDEX_INDIVIDU, DATE_REFERENCE_FLUX) %>%
  slice_min(MTT_TOTAL_RSA)

#corriger les variables Duree_perception_RSA,Montant_total_percu, Montant_moyen_mensuel_percu
Data_durees_filtered <- Data_durees_filtered %>%   
  group_by(INDEX_INDIVIDU) %>%
  mutate(Duree_Beneficiaire = n(),
         Duree_perception_RSA = sum(MTT_TOTAL_RSA != 0), 
         Montant_total_percu = sum(MTT_TOTAL_RSA),
         Montant_moyen_mensuel_percu = sum(MTT_TOTAL_RSA)/sum(MTT_TOTAL_RSA != 0))


#creer DATE_DEBUT_P_RSA, DATE_FIN_P_RSA 
# Convertir le dataframe en data.table
setDT(Data_durees_filtered)

# Indexer les colonnes utilisées pour le tri
setkey(Data_durees_filtered, INDEX_INDIVIDU, DATE_REFERENCE_FLUX)

# Conversion de la colonne DATE_REFERENCE_FLUX en date
#Data_durees[, DATE_REFERENCE_FLUX := as.Date(DATE_REFERENCE_FLUX)]

# Calculer l'interruption et le groupe
#Data_durees[, interruption := difftime(DATE_REFERENCE_FLUX, shift(DATE_REFERENCE_FLUX), units = "days")]
Data_durees_filtered[, interruption := difftime(DATE_REFERENCE_FLUX, lag(DATE_REFERENCE_FLUX), units = "days")]
Data_durees_filtered[, interruption := na.fill(interruption, fill = 0)]
Data_durees_filtered[, groupe := cumsum(ifelse(interruption > 6 * 30, 1, 0))]
Data_durees_filtered[, groupe := groupe + cumsum(ifelse(interruption > 6 * 30, 1, 0)) * (interruption == 0)]

# Supprimer les colonnes temporaires
#Data_durees[, c("interruption") := NULL]

# Rétablir l'ordre initial
setkey(Data_durees_filtered, INDEX_INDIVIDU, DATE_REFERENCE_FLUX)

####
Data_durees_filtered <- Data_durees_filtered  %>%
  group_by(INDEX_INDIVIDU, groupe) %>%
  mutate(DATE_DEBUT_P_RSA = min(DATE_REFERENCE_FLUX) ,
         DATE_FIN_P_RSA = max(DATE_REFERENCE_FLUX))

#
Data_duree_filtred_1 <- Data_durees_filtered %>%
  group_by(INDEX_INDIVIDU) %>%
  #filter(INDEX_INDIVIDU == first(INDEX_INDIVIDU)) %>% 
  arrange(INDEX_DOSSIER, INDEX_INDIVIDU, DATE_REFERENCE_FLUX) %>%
  distinct(DATE_DEBUT_P_RSA, .keep_all = TRUE)

#MODIF

# INTERRUPTION ,DUREE_PRESENCE 
Data_duree_filtred_1 <- Data_duree_filtred_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  arrange(INDEX_DOSSIER, INDEX_INDIVIDU, DATE_DEBUT_P_RSA) %>%
  mutate(DATE_FIN_P_RSA = as.character(DATE_FIN_P_RSA)) %>%
  mutate(DATE_FIN_P_RSA = ifelse(is.na(DATE_FIN_P_RSA), as.character(ceiling_date(Sys.Date(), "month") - days(1)), DATE_FIN_P_RSA)) %>%
  mutate(DATE_FIN_P_RSA = as.Date(DATE_FIN_P_RSA))%>%
  mutate(INTERRUPTION = as.numeric(difftime(DATE_DEBUT_P_RSA, lag(DATE_FIN_P_RSA), units = "days")))

#INTERRUPTION_plus_grand_6_mois
Data_duree_filtred_1 <- Data_duree_filtred_1 %>%
  mutate(INTERRUPTION_plus_grand_6_mois = ifelse(INTERRUPTION > 6*30, 1, 0),
         INTERRUPTION_negative = ifelse(INTERRUPTION < 0, 1, 0))

#jointure des cas particulier au base totale aprés traitement
Data_duree_1 <- bind_rows(Data_duree_1, Data_duree_filtred_1)

#Entrees
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(Entrees = ifelse(row_number() == 1, 1, ifelse(INTERRUPTION_plus_grand_6_mois == 1, 1, 0))) %>%
  ungroup()

#Sorties (a ajouter la definition dans le logique ,dans le rapport)
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(Sorties = ifelse(lead(Entrees) == 1 | 
                            (is.na(lead(Entrees)) & difftime(Sys.Date(), DATE_FIN_P_RSA, units = "days") > 6*30),
                          1, 0))

#Annee_Entrees
Data_duree_1 <- Data_duree_1 %>%
  mutate(Date_Entrees = ifelse(Entrees == 1, DATE_DEBUT_P_RSA, NA))


#Annee_Sorties
Data_duree_1 <- Data_duree_1 %>%
  mutate(Date_Sorties = ifelse(Sorties == 1, as.character(DATE_FIN_P_RSA), NA))

#884056 
#DUREE_PERIODE_PRESENCE 
#Data_duree_1 <- Data_duree_1 %>%
#  mutate(DUREE_PERIODE_PRESENCE = as.numeric(difftime(DATE_FIN_P_RSA, DATE_DEBUT_P_RSA, units = "days")),
#         DUREE_PERIODE__PRESENCE_An_mois = paste(floor(DUREE_PERIODE_PRESENCE / 365), "an(s)", floor((DUREE_PERIODE_PRESENCE %% 365) / 30), "mois"))

Data_duree_1 <- Data_duree_1 %>%
  mutate(DUREE_PERIODE_PRESENCE = ifelse(as.numeric(difftime(DATE_FIN_P_RSA, DATE_DEBUT_P_RSA, units = "days")) < 0,
                                         as.numeric(difftime(DATE_FIN_PRESENCE_RSA, DATE_DEBUT_PRESENCE_RSA, units = "days")),
                                         as.numeric(difftime(DATE_FIN_P_RSA, DATE_DEBUT_P_RSA, units = "days"))),
         DUREE_PERIODE__PRESENCE_An_mois = paste(floor(DUREE_PERIODE_PRESENCE / 365), "an(s)", floor((DUREE_PERIODE_PRESENCE %% 365) / 30), "mois"))


#DUREE_PRESENCE 
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(
    DUREE_PRESENCE = sum(DUREE_PERIODE_PRESENCE, na.rm = TRUE) + sum(INTERRUPTION[INTERRUPTION < 0], na.rm = TRUE),
    DUREE_PRESENCE_An_mois = paste(floor(DUREE_PRESENCE / 365), "an(s)", floor((DUREE_PRESENCE %% 365) / 30), "mois")
  )

######



#duree_categorie
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(
    categorie_duree = ifelse(!is.na(Date_Sorties), paste0("d", cumsum(!is.na(Date_Sorties))), NA),
    categorie_duree = ifelse(row_number() == n() & is.na(categorie_duree), "dc", categorie_duree)
  ) %>%
  fill(categorie_duree, .direction = "up")


#Nb_presence
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(
    Nb_presence = n_distinct(categorie_duree)
  )

#duree_par_presence
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU, categorie_duree) %>%
  mutate(
    duree_par_presence = sum(DUREE_PERIODE_PRESENCE) + sum(ifelse(!is.na(INTERRUPTION) & INTERRUPTION < 0, INTERRUPTION, 0))
  )
    
    
#categorie_interruption    
Data_duree_1 <- Data_duree_1 %>%
  group_by(INDEX_INDIVIDU) %>%
  mutate(
    categorie_interruption = ifelse(!is.na(INTERRUPTION_plus_grand_6_mois) & INTERRUPTION_plus_grand_6_mois == 1,
                                    paste0("i", cumsum(!is.na(INTERRUPTION_plus_grand_6_mois) & INTERRUPTION_plus_grand_6_mois == 1)), NA)
  )

##Data_Parcours_Duree
#Data_Parcours_Duree <- Data_duree_1 %>%
#  pivot_wider(names_from = categorie_duree, values_from = duree_par_presence, values_fill = NA) %>%
#  group_by(INDEX_INDIVIDU) %>%
#  select(INDEX_INDIVIDU, Nb_presence, d1, d2, d3, d4, dc) %>%
#  arrange(INDEX_INDIVIDU, Nb_presence) %>%
#  distinct(d1, d2, d3, d4, dc, .keep_all = TRUE)

Data_Parcours_Duree <- Data_duree_1 %>%
  pivot_wider(names_from = categorie_duree, values_from = duree_par_presence, values_fill = NA) %>%
  group_by(INDEX_INDIVIDU) %>%
  summarize(Nb_presence, d1, d2, d3, d4, dc) %>%
  arrange(INDEX_INDIVIDU, Nb_presence) %>%
  distinct(d1, d2, d3, d4, dc, .keep_all = TRUE)

  
Data_Parcours_Duree <- Data_Parcours_Duree %>%
  group_by(INDEX_INDIVIDU) %>%
  summarize(Nb_presence = max(Nb_presence),
            d1 = coalesce(na.omit(d1)[1], NA),
            d2 = coalesce(na.omit(d2)[1], NA),
            d3 = coalesce(na.omit(d3)[1], NA),
            d4 = coalesce(na.omit(d4)[1], NA),
            dc = coalesce(na.omit(dc)[1], NA))

##Data_Parcours_Interruption
#Data_Parcours_Interruption <- Data_duree_1 %>%
#  pivot_wider(names_from = categorie_interruption, values_from = INTERRUPTION, values_fill = NA) %>%
#  group_by(INDEX_INDIVIDU) %>%
#  select(INDEX_INDIVIDU, Nb_presence, i1, i2, i3) %>%
#  arrange(INDEX_INDIVIDU) %>%
#  distinct(i1, i2, i3, .keep_all = TRUE)

Data_Parcours_Interruption <- Data_duree_1 %>%
  pivot_wider(names_from = categorie_interruption, values_from = INTERRUPTION, values_fill = NA) %>%
  group_by(INDEX_INDIVIDU) %>%
  summarize(Nb_presence, i1, i2, i3) %>%
  arrange(INDEX_INDIVIDU) %>%
  distinct(i1, i2, i3, .keep_all = TRUE)


Data_Parcours_Interruption <- Data_Parcours_Interruption %>%
  group_by(INDEX_INDIVIDU) %>%
  summarize(
    Nb_presence = max(Nb_presence),
    i1 = ifelse(Nb_presence == 1, NA, coalesce(na.omit(i1)[1], NA)),
    i2 = coalesce(na.omit(i2)[1], NA),
    i3 = coalesce(na.omit(i3)[1], NA)
  )

######Data_Parcours_Duree_Interruption 
Data_Parcours_Duree_Interruption <- inner_join(Data_Parcours_Duree, Data_Parcours_Interruption, by = c("Nb_presence", "INDEX_INDIVIDU"))

#Data_Parcours_encore_dans_dispositif
Data_Parcours_encore_dans_dispositif <- filter(Data_Parcours_Duree_Interruption, !is.na(dc))
Data_Parcours_encore_dans_dispositif <- Data_Parcours_encore_dans_dispositif %>%
  mutate(d1 = ifelse(Nb_presence == 1, dc, d1),
         d2 = ifelse(Nb_presence == 2, dc, d2),
         d3 = ifelse(Nb_presence == 3, dc, d3),
         d4 = ifelse(Nb_presence == 4, dc, d4))

Data_Parcours_encore_dans_dispositif <- Data_Parcours_encore_dans_dispositif[,-c(7)]

#Data_Parcours_hors_dispositif
Data_Parcours_hors_dispositif <- filter(Data_Parcours_Duree_Interruption, is.na(dc))
Data_Parcours_hors_dispositif <- Data_Parcours_hors_dispositif[,-c(7)]

#
Data_classification <- rbind(Data_Parcours_encore_dans_dispositif, Data_Parcours_hors_dispositif)

#
Data_durees_colonne_age_montant <- Data_durees[,c("INDEX_INDIVIDU","DATE_NAISSANCE_INDIVIDU","NB_PERS_CHARGE_derniere_situation","Montant_moyen_mensuel_percu","Montant_total_percu","Duree_perception_RSA")] 
Data_durees_colonne_age_montant <- distinct(Data_durees_colonne_age_montant, INDEX_INDIVIDU, .keep_all = TRUE)

# Convertir la colonne "DATE_NAISSANCE_INDIVIDU" en format de date
Data_durees_colonne_age_montant$DATE_NAISSANCE_INDIVIDU <- as.Date(Data_durees_colonne_age_montant$DATE_NAISSANCE_INDIVIDU)

# Calculer l'âge jusqu'au 1er juin de cette année
Data_durees_colonne_age_montant$Age <- floor(time_length(interval(Data_durees_colonne_age_montant$DATE_NAISSANCE_INDIVIDU, as.Date("2023-06-01")), "years"))

# Supprimer la colonne "DATE_NAISSANCE_INDIVIDU"
Data_durees_colonne_age_montant$DATE_NAISSANCE_INDIVIDU <- NULL

#
classification_parcours <- Data_classification %>%
  left_join(Data_durees_colonne_age_montant, by = "INDEX_INDIVIDU")

classification_parcours$Nb_presence <- as.factor(classification_parcours$Nb_presence)

#Ratio_Montant_totale_Percu_Revenu_minimum_impose_pour_chaque_foyer

Liste_entrees <- Data_duree_1 %>%
  dplyr::filter(Entrees == 1) %>%
  dplyr::arrange(Date_Entrees, INDEX_INDIVIDU) %>%
  dplyr::select(Date_Entrees, INDEX_DOSSIER, INDEX_INDIVIDU, DATE_NAISSANCE_INDIVIDU, SEXE_INDIVIDU, NB_PERS_CHARGE_derniere_situation,NB_PERS_CHARGE, duree_par_presence,
                DUREE_PRESENCE, DUREE_PRESENCE_An_mois, Montant_total_percu, Montant_moyen_mensuel_percu,
                CODE_POSTAL, NOM_COM, CODE_SECTEUR, NOM_SECTEUR, Allocataire_Conjoint) %>%
  dplyr::group_by(INDEX_INDIVIDU) %>%
  dplyr::mutate(Age = as.integer(difftime(as.Date("2023-05-31"), DATE_NAISSANCE_INDIVIDU, units = "days") / 365),
                Age_rep = cut(Age, breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, Inf),
                              labels = c("<= 25", "]25,30]", "]30,35]", "]35,40]", "]40,45]", "]45,50]", "]50,55]", "]55,60]", ">60")),
                NB_PERS_CHARGE_rep = cut(NB_PERS_CHARGE, breaks = c(-Inf, 0, 2, 4, Inf),
                                         labels = c("0", "[1,2]", "[3,4]", ">4")))




Liste_entrees_Moyenne_par_annee <- Liste_entrees %>%
  group_by(annee = year(Date_Entrees)) %>%
  summarize(
    Moyenne_duree_par_presence = mean(duree_par_presence),
    Moyenne_duree_par_presence_mois_e = floor(mean(duree_par_presence) / 30),
    Moyenne_duree_par_presence_An_mois = paste(
      floor(mean(duree_par_presence) / 365), "an(s)",
      floor((mean(duree_par_presence) %% 365) / 30), "mois"
    )
  )

#Liste_entrees_Somme_par_individu <- Liste_entrees %>%
#  group_by(INDEX_INDIVIDU) %>%
#  summarize(
#    Somme_DUREE_PRESENCE = sum(DUREE_PRESENCE),
#    Somme_DUREE_PRESENCE_An_mois = paste(
#      floor(sum(DUREE_PRESENCE) / 365), "an(s)",
#      floor((sum(DUREE_PRESENCE) %% 365) / 30), "mois"
#    )
#  )


#Liste-sorties
#Liste_sorties <- Data_duree_1 %>%
#  filter(Sorties == 1) %>%
#  arrange(Date_Sorties, INDEX_INDIVIDU)%>%
# select(Date_Sorties, INDEX_DOSSIER, INDEX_INDIVIDU, DATE_NAISSANCE_INDIVIDU, SEXE_INDIVIDU,NB_PERS_CHARGE_derniere_situation,duree_par_presence,
#       DUREE_PRESENCE, DUREE_PRESENCE_An_mois, Montant_total_percu, Montant_moyen_mensuel_percu,
#       #Ratio_montant.departement_revenu.minimum,Ratio_montant_complement,
#       CODE_POSTAL, NOM_COM, CODE_SECTEUR, NOM_SECTEUR)


Liste_sorties <- Data_duree_1 %>%
  dplyr::filter(Sorties == 1) %>%
  dplyr::arrange(Date_Sorties, INDEX_INDIVIDU) %>%
  dplyr::select(Date_Sorties, INDEX_DOSSIER, INDEX_INDIVIDU, DATE_NAISSANCE_INDIVIDU, SEXE_INDIVIDU, NB_PERS_CHARGE, NB_PERS_CHARGE_derniere_situation, duree_par_presence,
                DUREE_PRESENCE, DUREE_PRESENCE_An_mois, Montant_total_percu, Montant_moyen_mensuel_percu,
                CODE_POSTAL, NOM_COM, CODE_SECTEUR, NOM_SECTEUR, Allocataire_Conjoint) %>%
  dplyr::group_by(INDEX_INDIVIDU) %>%
  dplyr::mutate(Age = as.integer(difftime(as.Date("2023-05-31"), DATE_NAISSANCE_INDIVIDU, units = "days") / 365),
                Age_rep = cut(Age, breaks = c(-Inf, 25, 30, 35, 40, 45, 50, 55, 60, Inf),
                              labels = c("<= 25", "]25,30]", "]30,35]", "]35,40]", "]40,45]", "]45,50]", "]50,55]", "]55,60]", ">60")),
                NB_PERS_CHARGE_rep = cut(NB_PERS_CHARGE, breaks = c(-Inf, 0, 2, 4, Inf),
                                         labels = c("0", "[1,2]", "[3,4]", ">4")))



Liste_sorties_Moyenne_par_annee <- Liste_sorties %>%
  group_by(annee = year(Date_Sorties)) %>%
  summarize(
    Moyenne_duree_par_presence = mean(duree_par_presence),
    Moyenne_duree_par_presence_mois_s = floor(mean(duree_par_presence) / 30),
    Moyenne_duree_par_presence_An_mois = paste(
      floor(mean(duree_par_presence) / 365), "an(s)",
      floor((mean(duree_par_presence) %% 365) / 30), "mois"
    )
  )


#on peut utiliser cette logique pour la data des parcours   ajouter d1,d2 = somme_duree_presence_par_individu ,i1 +i2 =i1 ,ca sert à expliqué la
#catégorisation 

#Liste_sorties_Somme_par_individu <- Liste_sorties %>%
#  group_by(INDEX_INDIVIDU) %>%
#  summarize(
#    Somme_DUREE_PRESENCE = sum(DUREE_PRESENCE),
#    Somme_DUREE_PRESENCE_An_mois = paste(
#      floor(sum(DUREE_PRESENCE) / 365), "an(s)",
#      floor((sum(DUREE_PRESENCE) %% 365) / 30), "mois"
#    )
#  )

#Resultat1: Recapitulatif annuel d'entrees et de sorties
Recapitulatif_entrees <- Liste_entrees %>%
  group_by(annee = year(Date_Entrees)) %>%
  summarize(Nombre_entrees = n())

Recapitulatif_sorties <- Liste_sorties %>%
  group_by(annee = year(Date_Sorties)) %>%
  summarize(Nombre_sorties = n())

Recapitulatif_total <- left_join(Recapitulatif_entrees, Recapitulatif_sorties, by = "annee")

Data <- Data %>%
  mutate(annee = format(as.Date(DATE_REFERENCE_FLUX), "%Y"))

#valeur_réelle_du_stock
Valeur_reelle_stock <- data.frame(Stock = c(16875, 16886, 17399, 19395, 18587, 18909),
                    annee = c(2017, 2018, 2019, 2020, 2021, 2022))

# Joindre les résultats avec Recapitulatif_total
Valeur_reelle_stock$annee <- as.double(Valeur_reelle_stock$annee) # Conversion du type de données de "annee" en double
Recapitulatif_total <- left_join(Recapitulatif_total, Valeur_reelle_stock, by = "annee")

# Convertir la variable "annee" en format de date
Recapitulatif_total$Date <- as.Date(paste(Recapitulatif_total$annee, "01-01", sep = "-"))
Recapitulatif_total$Date[nrow(Recapitulatif_total)] <- Sys.Date()

# 
## Calculer la variable somme_stock_entrees_sorties
Recapitulatif_total$stock_plus_entrees_moins_sorties <- lag(Recapitulatif_total$Stock) + (Recapitulatif_total$Nombre_entrees - Recapitulatif_total$Nombre_sorties)


#write.csv2(Data_durees ,file = "C:/Users/Hannaoui Youness/OneDrive/Bureau/Data_durees.csv")
#write.csv2(Liste_sorties,file = "resultats_entree_sortie_BRSA/Liste des sorties.csv")
#write.csv2(Liste_sorties_Moyenne_par_annee,file = "resultats_entree_sortie_BRSA/Liste_sorties_Moyenne_par_annee.csv")
##write.csv2(Liste_sorties_Somme_par_individu,file = "resultats_entree_sortie_BRSA/Liste_sorties_Somme_par_individu.csv")
#write.csv2(Liste_entrees,file = "resultats_entree_sortie_BRSA/Liste des entrees.csv")
#write.csv2(Liste_entrees_Moyenne_par_annee,file = "resultats_entree_sortie_BRSA/Liste_entrees_Moyenne_par_annee.csv")
#write.csv2(Data_Parcours_encore_dans_dispositif,file = "resultats_entree_sortie_BRSA/Data_Parcours_encore_dans_dispositif.csv")
#write.csv2(Data_Parcours_hors_dispositif,file = "resultats_entree_sortie_BRSA/Data_Parcours_hors_dispositif.csv")
#write.csv2(Data,file = "Verification_stock_entrees_sorties/Data_Totale.csv")
#write.csv2(Recapitulatif_total,file = "Verification_stock_entrees_sorties/Recapitulatif_total_2.csv")

