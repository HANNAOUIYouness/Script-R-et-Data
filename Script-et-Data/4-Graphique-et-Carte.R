library(ggplot2)
library(RColorBrewer)
library(sf)
library(raster)
library(dplyr)
library(tidyr)
library(knitr)
library(stringdist)
library(readxl)
library(tidyverse)
library(webshot)
library(kableExtra)
library(tmap)

# Spécifier le chemin du fichier Excel
chemin_fichier <- "------/Population_Ille-et-Vilaine_2020.xlsx"

# Importer la feuille "Population_commune"
population_commune <- read_excel(chemin_fichier, sheet = "Feuil1")

# Importer la feuille "Population_secteur"
population_secteur <- read_excel(chemin_fichier, sheet = "Feuil2")



#supprimer les valeurs manquantes de Liste_entrees et Liste_sorties
Liste_entrees <- Liste_entrees %>%
  filter(!is.na(DATE_NAISSANCE_INDIVIDU))
Liste_sorties <- Liste_sorties %>%
  filter(!is.na(DATE_NAISSANCE_INDIVIDU))

#  Évolution du nombre d'entrées et de sorties

# graphique 1 : Evolution de flux (Entrees-Sorties) des allocatires de RSA dansle dispositif de departement d'Ille-et-Vilaine
ggplot(Recapitulatif_total, aes(x = Date)) +
  geom_line(aes(y = Nombre_entrees, color = "Nombre d'entrées"), size = 1) +
  geom_line(aes(y = Nombre_sorties, color = "Nombre de sorties"), size = 1) +
  geom_line(aes(y = stock_plus_entrees_moins_sorties,color = "Lag Stock + Entrées - Sorties"), vjust = -0.5, size = 2.5, linetype = "dashed") +
  geom_line(aes(y = Stock, color = "Stock"), size = 1) +
  geom_text(aes(y = Nombre_entrees, label = Nombre_entrees, color = "Nombre d'entrées"), vjust = -0.5, size = 4) +
  geom_text(aes(y = Nombre_sorties, label = Nombre_sorties, color = "Nombre de sorties"), vjust = 1, size = 4) +
  geom_text(aes(y = Stock, label = Stock, color = "Stock"), vjust = -0.5, size = 4) +
  labs(x = "Date", y = "Nombre", color = "Variables", title = "Graphique 2.1.2.1") +
  theme_minimal() +
  xlim(as.Date("2013-01-01"), as.Date("2022-12-31")) +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("Nombre d'entrées" = "red", "Nombre de sorties" = "green", "Stock" = "black", "Lag Stock + Entrées - Sorties" = "purple"))

##Sexe
#Sorties
#Comparaison de l'évolution des sorties entre les hommes et les femmes
Liste_sorties$SEXE_INDIVIDU <- as.factor(Liste_sorties$SEXE_INDIVIDU)

Liste_sorties$Annee <- year(Liste_sorties$Date_Sorties)

Liste_sorties_filtered <- filter(Liste_sorties, Date_Sorties >= as.Date("2012-12-31"))

# Agréger les données par année et sexe pour obtenir le nombre total d'individus
Liste_sorties_aggregated <- Liste_sorties_filtered %>%
  arrange(Annee) %>%
  group_by(Annee, SEXE_INDIVIDU) %>%
  summarise(Count = n()) %>%
  filter(Annee != 2012)


# Filtrer les données pour inclure uniquement les années entre 2013 et 2022
Liste_sorties_filtered <- Liste_sorties_aggregated[as.numeric(as.character(Liste_sorties_aggregated$Annee)) %in% 2013:2022, ]

# Tracer le graphique en utilisant les données filtrées
ggplot(Liste_sorties_filtered, aes(x = as.factor(Annee), y = Count)) +
  #geom_vline(xintercept = "2018", linetype = "dashed", color = "orange", size = 1) +
  geom_line(aes(group = SEXE_INDIVIDU, color = SEXE_INDIVIDU), size = 1.2) +
  geom_point(aes(color = SEXE_INDIVIDU), size = 2) +
  geom_text(aes(label = Count, color = SEXE_INDIVIDU), vjust = -0.5, size = 4.5) +
  scale_x_discrete(limits = as.character(2013:2022)) +
  labs(x = "Année de sortie", y = "", title = "Graphique 4.1.3.1.2",
       color = "Sexe") +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 25)
  ) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"),
    labels = c("Femme", "Homme")
  )

#Entrees
#Comparaison de l'évolution des entrees entre les hommes et les femmes
Liste_entrees$SEXE_INDIVIDU <- as.factor(Liste_entrees$SEXE_INDIVIDU)

Liste_entrees$Annee <- year(Liste_entrees$Date_Entrees)

Liste_entrees_filtered <- filter(Liste_entrees, Date_Entrees >= as.Date("2012-12-31"))

# Agréger les données par année et sexe pour obtenir le nombre total d'individus
Liste_entrees_aggregated <- Liste_entrees_filtered %>%
  arrange(Annee) %>%
  group_by(Annee, SEXE_INDIVIDU) %>%
  summarise(Count = n()) %>%
  filter(Annee != c(2012,2023))


# Filtrer les données d'entrées pour inclure uniquement les années entre 2013 et 2022
Liste_entrees_filtered <- Liste_entrees_aggregated[as.numeric(as.character(Liste_entrees_aggregated$Annee)) %in% 2013:2022, ]

# Tracer le graphique en utilisant les données filtrées d'entrées
ggplot(Liste_entrees_filtered, aes(x = as.factor(Annee), y = Count)) +
  #geom_vline(xintercept = "2018", linetype = "dashed", color = "orange", size = 1) +
  geom_line(aes(group = SEXE_INDIVIDU, color = SEXE_INDIVIDU), size = 1.2) +
  geom_point(aes(color = SEXE_INDIVIDU), size = 2) +
  geom_text(aes(label = Count, color = SEXE_INDIVIDU), vjust = -0.5, size = 4.5) +
  scale_x_discrete(limits = c(as.character(unique(Liste_entrees_aggregated$Annee)))) +
  scale_x_discrete(limits = as.character(2013:2022)) +
  labs(x = "Année d'entrée", y = "", title = "Graphique 4.1.3.1.1",color = "Sexe") +
  theme_void() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 25)
  ) +
  scale_color_manual(
    values = c("0" = "blue", "1" = "red"),
    labels = c("Femme", "Homme")
  )

###panel NOM_SECTEUR
#exclure "hors departement "
Liste_entrees_secteur <- Liste_entrees %>%
  dplyr::filter(NOM_SECTEUR != "HORS DEPARTEMENT")

Liste_sorties_secteur <- Liste_sorties  %>%
  dplyr::filter(NOM_SECTEUR != "HORS DEPARTEMENT")

#Sorties
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_sorties_annee <- Liste_sorties_secteur %>%
    filter(year(Date_Sorties) == annee)
  
  # Calculer le nombre de sorties par modalité de NOM_SECTEUR, SEXE_INDIVIDU et Age_rep
  sorties_counts <- Liste_sorties_annee %>%
    group_by(NOM_SECTEUR, SEXE_INDIVIDU, Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NOM_SECTEUR
  NOM_SECTEUR_percentages <- sorties_counts %>%
    group_by(NOM_SECTEUR) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee[[as.character(annee)]] <- NOM_SECTEUR_percentages
}

# Combiner les résultats pour toutes les années
combined_table_NOM_SECTEUR_sorties <- bind_rows(pourcentages_par_annee, .id = "Année") %>%
  dplyr::select(Année, NOM_SECTEUR, count, percentage)

#####
# Créer une liste des modalités de NOM_COM1
modalites_NOM_COM1 <- unique(population_secteur$`Étiquettes de lignes`)

# Fonction pour trouver la modalité la plus proche
find_closest_match <- function(name, modalites) {
  name_lower <- tolower(name)
  modalites_lower <- tolower(modalites)
  
  if (name_lower == "vitre communaute") {
    return("PAYS DE VITRE")
  } else {
    distances <- stringdist::stringdist(name_lower, modalites_lower)
    closest_index <- which.min(distances)
    closest_match <- modalites[closest_index]
    
    return(closest_match)
  }
}


# Parcourir les modalités de NOM_COM dans combined_table_NOM_COM_entrees
for (i in 1:nrow(combined_table_NOM_SECTEUR_sorties)) {
  nom_com <- combined_table_NOM_SECTEUR_sorties[i, "NOM_SECTEUR"]
  
  # Trouver la modalité la plus proche dans la liste des modalités de NOM_COM1
  closest_matches <- find_closest_match(nom_com, modalites_NOM_COM1)
  
  # Affecter la modalité la plus proche à NOM_COM1 dans combined_table_NOM_COM_entrees
  combined_table_NOM_SECTEUR_sorties[i, "NOM_SECTEUR1"] <- closest_matches
}

# Effectuer la jointure à gauche entre combined_table_NOM_COM_entrees et population_commune
combined_table_NOM_SECTEUR_sorties <- left_join(combined_table_NOM_SECTEUR_sorties, population_secteur, by = c("NOM_SECTEUR1" = "Étiquettes de lignes"))

# Sélectionner les colonnes souhaitées
combined_table_NOM_SECTEUR_sorties <- combined_table_NOM_SECTEUR_sorties %>%
  dplyr::select(Année, NOM_SECTEUR, NOM_SECTEUR1 ,count, percentage, 'Somme de Population municipale')

combined_table_NOM_SECTEUR_sorties <- combined_table_NOM_SECTEUR_sorties %>%
  mutate(Ration_nombre_sortant = round((count / `Somme de Population municipale`)*100,2))


#entrees
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee_entrees <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_entrees_annee <- Liste_entrees_secteur %>%
    filter(year(Date_Entrees) == annee)
  
  # Calculer le nombre d'entrées par modalité de NOM_SECTEUR, SEXE_INDIVIDU et Age_rep
  entrees_counts <- Liste_entrees_annee %>%
    group_by(NOM_SECTEUR, SEXE_INDIVIDU, Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NOM_SECTEUR
  NOM_SECTEUR_percentages_entrees <- entrees_counts %>%
    group_by(NOM_SECTEUR) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste pour les entrées
  pourcentages_par_annee_entrees[[as.character(annee)]] <- NOM_SECTEUR_percentages_entrees
}

# Combiner les résultats pour toutes les années pour les entrées
combined_table_NOM_SECTEUR_entrees <- bind_rows(pourcentages_par_annee_entrees, .id = "Année") %>%
  dplyr::select(Année, NOM_SECTEUR, count, percentage)

# Créer une liste des modalités de NOM_COM1
modalites_NOM_COM1 <- unique(population_secteur$`Étiquettes de lignes`)

# Fonction pour trouver la modalité la plus proche
find_closest_match <- function(name, modalites) {
  name_lower <- tolower(name)
  modalites_lower <- tolower(modalites)
  
  if (name_lower == "vitre communaute") {
    return("PAYS DE VITRE")
  } else {
    distances <- stringdist::stringdist(name_lower, modalites_lower)
    closest_index <- which.min(distances)
    closest_match <- modalites[closest_index]
    
    return(closest_match)
  }
}


# Parcourir les modalités de NOM_COM dans combined_table_NOM_COM_entrees
for (i in 1:nrow(combined_table_NOM_SECTEUR_entrees)) {
  nom_com <- combined_table_NOM_SECTEUR_entrees[i, "NOM_SECTEUR"]
  
  # Trouver la modalité la plus proche dans la liste des modalités de NOM_COM1
  closest_matches <- find_closest_match(nom_com, modalites_NOM_COM1)
  
  # Affecter la modalité la plus proche à NOM_COM1 dans combined_table_NOM_COM_entrees
  combined_table_NOM_SECTEUR_entrees[i, "NOM_SECTEUR1"] <- closest_matches
}

# Effectuer la jointure à gauche entre combined_table_NOM_COM_entrees et population_commune
combined_table_NOM_SECTEUR_entrees <- left_join(combined_table_NOM_SECTEUR_entrees, population_secteur, by = c("NOM_SECTEUR1" = "Étiquettes de lignes"))

# Sélectionner les colonnes souhaitées
combined_table_NOM_SECTEUR_entrees <- combined_table_NOM_SECTEUR_entrees %>%
  dplyr::select(Année, NOM_SECTEUR, NOM_SECTEUR1 ,count, percentage, 'Somme de Population municipale')

combined_table_NOM_SECTEUR_entrees <- combined_table_NOM_SECTEUR_entrees %>%
  mutate(Ration_nombre_entrant = round((count / `Somme de Population municipale`)*100,2))


####
###panel NOM_COM
#exclure "hors departement "
Liste_entrees_carte <- Liste_entrees %>%
  dplyr::filter(NOM_SECTEUR != "HORS DEPARTEMENT")

Liste_sorties_carte <- Liste_sorties  %>%
  dplyr::filter(NOM_SECTEUR != "HORS DEPARTEMENT")

#Sorties
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_sorties_annee <- Liste_sorties_carte %>%
    filter(year(Date_Sorties) == annee)
  
  # Calculer le nombre de sorties par modalité de NOM_COM, SEXE_INDIVIDU et Age_rep
  sorties_counts <- Liste_sorties_annee %>%
    group_by(NOM_COM,SEXE_INDIVIDU , Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NOM_COM
  NOM_COM_percentages <- sorties_counts %>%
    group_by(NOM_COM) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee[[as.character(annee)]] <- NOM_COM_percentages
}

# Combiner les résultats pour toutes les années
combined_table_NOM_COM_sorties <- bind_rows(pourcentages_par_annee, .id = "Année") %>%
  dplyr::select(Année, NOM_COM,count, percentage)


# Créer une liste des modalités de NOM_COM1
modalites_NOM_COM1 <- unique(population_commune$`Nom de la commune`)

# Fonction pour trouver la modalité la plus proche
find_closest_match <- function(name, modalites) {
  name_lower <- tolower(name)
  modalites_lower <- tolower(modalites)
  
  distances <- stringdist::stringdist(name_lower, modalites_lower)
  closest_index <- which.min(distances)
  closest_match <- modalites[closest_index]
  
  return(closest_match)
}

# Parcourir les modalités de NOM_COM dans combined_table_NOM_COM_entrees
for (i in 1:nrow(combined_table_NOM_COM_sorties)) {
  nom_com <- combined_table_NOM_COM_sorties[i, "NOM_COM"]
  
  # Trouver la modalité la plus proche dans la liste des modalités de NOM_COM1
  closest_matches <- find_closest_match(nom_com, modalites_NOM_COM1)
  
  # Affecter la modalité la plus proche à NOM_COM1 dans combined_table_NOM_COM_entrees
  combined_table_NOM_COM_sorties[i, "NOM_COM1"] <- closest_matches
}

# Effectuer la jointure à gauche entre combined_table_NOM_COM_entrees et population_commune
combined_table_NOM_COM_sorties <- left_join(combined_table_NOM_COM_sorties, population_commune, by = c("NOM_COM1" = "Nom de la commune"))

# Sélectionner les colonnes souhaitées
combined_table_NOM_COM_sorties <- combined_table_NOM_COM_sorties %>%
  dplyr::select(Année, NOM_COM ,NOM_COM1 ,count, percentage, 'Population municipale')

combined_table_NOM_COM_sorties <- combined_table_NOM_COM_sorties %>%
  mutate(Ration_nombre_sortant = round((count / `Population municipale`)*100,2))


#entrees
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee_entrees <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_entrees_annee <- Liste_entrees_carte %>%
    filter(year(Date_Entrees) == annee)
  
  # Calculer le nombre d'entrées par modalité de NOM_COM, SEXE_INDIVIDU et Age_rep
  entrees_counts <- Liste_entrees_annee %>%
    group_by(NOM_COM, SEXE_INDIVIDU, Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NOM_COM
  NOM_COM_percentages_entrees <- entrees_counts %>%
    group_by(NOM_COM ) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste pour les entrées
  pourcentages_par_annee_entrees[[as.character(annee)]] <- NOM_COM_percentages_entrees
}

# Combiner les résultats pour toutes les années pour les entrées
combined_table_NOM_COM_entrees <- bind_rows(pourcentages_par_annee_entrees, .id = "Année") %>%
  dplyr::select(Année, NOM_COM, count, percentage)


# Créer une liste des modalités de NOM_COM1
modalites_NOM_COM1 <- unique(population_commune$`Nom de la commune`)

# Fonction pour trouver la modalité la plus proche
find_closest_match <- function(name, modalites) {
  name_lower <- tolower(name)
  modalites_lower <- tolower(modalites)
  
  distances <- stringdist::stringdist(name_lower, modalites_lower)
  closest_index <- which.min(distances)
  closest_match <- modalites[closest_index]
  
  return(closest_match)
}

# Parcourir les modalités de NOM_COM dans combined_table_NOM_COM_entrees
for (i in 1:nrow(combined_table_NOM_COM_entrees)) {
  nom_com <- combined_table_NOM_COM_entrees[i, "NOM_COM"]
  
  # Trouver la modalité la plus proche dans la liste des modalités de NOM_COM1
  closest_matches <- find_closest_match(nom_com, modalites_NOM_COM1)
  
  # Affecter la modalité la plus proche à NOM_COM1 dans combined_table_NOM_COM_entrees
  combined_table_NOM_COM_entrees[i, "NOM_COM1"] <- closest_matches
}

# Effectuer la jointure à gauche entre combined_table_NOM_COM_entrees et population_commune
combined_table_NOM_COM_entrees <- left_join(combined_table_NOM_COM_entrees, population_commune, by = c("NOM_COM1" = "Nom de la commune"))

# Sélectionner les colonnes souhaitées
combined_table_NOM_COM_entrees <- combined_table_NOM_COM_entrees %>%
 dplyr::select(Année, NOM_COM ,NOM_COM1 ,count, percentage, 'Population municipale')

combined_table_NOM_COM_entrees <- combined_table_NOM_COM_entrees %>%
  mutate(Ration_nombre_entrant = round((count / `Population municipale`)*100,2))

#PANEL Age_rep
#sorties
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_sorties_annee <- Liste_sorties %>%
    filter(year(Date_Sorties) == annee)
  
  # Calculer le nombre de sorties par modalité de Age_rep
  sorties_counts <- Liste_sorties_annee %>%
    group_by(Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de Age_rep
  Age_rep_percentages <- sorties_counts %>%
    group_by(Age_rep) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee[[as.character(annee)]] <- Age_rep_percentages
}

# Combiner les résultats pour toutes les années
combined_table_Age_rep_sorties <- bind_rows(pourcentages_par_annee, .id = "Année") %>%
  dplyr::select(Année, Age_rep, count, percentage)

#entrees
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee_entrees <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_entrees_annee <- Liste_entrees %>%
    filter(year(Date_Entrees) == annee)
  
  # Calculer le nombre d'entrées par modalité de Age_rep
  entrees_counts <- Liste_entrees_annee %>%
    group_by(Age_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de Age_rep
  Age_rep_percentages_entrees <- entrees_counts %>%
    group_by(Age_rep) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee_entrees[[as.character(annee)]] <- Age_rep_percentages_entrees
}

# Combiner les résultats pour toutes les années pour les entrées
combined_table_Age_rep_entrees <- bind_rows(pourcentages_par_annee_entrees, .id = "Année") %>%
  dplyr::select(Année, Age_rep, count, percentage)


#panel SEX_INDIVIDU
#sorties
pourcentages_par_annee <- list()

for (annee in 2018:2022) {
  Liste_sorties_annee <- Liste_sorties %>%
    filter(year(Date_Sorties) == annee)
  
  sorties_counts <- Liste_sorties_annee %>%
    group_by(SEXE_INDIVIDU) %>%
    summarise(count = n()) %>%
    ungroup()
  
  SEXE_INDIVIDU_percentages <- sorties_counts %>%
    group_by(SEXE_INDIVIDU) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  pourcentages_par_annee[[as.character(annee)]] <- SEXE_INDIVIDU_percentages
}

combined_table_SEXE_INDIVIDU_sorties <- bind_rows(pourcentages_par_annee, .id = "Année") %>%
  dplyr::select(Année, SEXE_INDIVIDU, count, percentage)

#entrees
# Créer une liste vide pour stocker les résultats par année
pourcentages_par_annee_entrees <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_entrees_annee <- Liste_entrees %>%
    filter(year(Date_Entrees) == annee)
  
  # Calculer le nombre d'entrées par modalité de SEXE_INDIVIDU
  entrees_counts <- Liste_entrees_annee %>%
    group_by(SEXE_INDIVIDU) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de SEXE_INDIVIDU
  SEXE_INDIVIDU_percentages_entrees <- entrees_counts %>%
    group_by(SEXE_INDIVIDU) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee_entrees[[as.character(annee)]] <- SEXE_INDIVIDU_percentages_entrees
}

# Combiner les résultats pour toutes les années pour les entrées
combined_table_SEXE_INDIVIDU_entrees <- bind_rows(pourcentages_par_annee_entrees, .id = "Année") %>%
  dplyr::select(Année, SEXE_INDIVIDU, count, percentage)

#Nombre de personne en charge 
#sorties
pourcentages_par_annee_sorties <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_sorties_annee <- Liste_sorties %>%
    filter(year(Date_Sorties) == annee)
  
  # Calculer le nombre de sorties par modalité de NB_PERS_CHARGE_rep
  sorties_counts <- Liste_sorties_annee %>%
    group_by(NB_PERS_CHARGE_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NB_PERS_CHARGE_rep
  NB_PERS_CHARGE_rep_percentages_sorties <- sorties_counts %>%
    group_by(NB_PERS_CHARGE_rep) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee_sorties[[as.character(annee)]] <- NB_PERS_CHARGE_rep_percentages_sorties
}

# Combiner les résultats pour toutes les années pour les sorties
combined_table_NB_PERS_CHARGE_rep_sorties <- bind_rows(pourcentages_par_annee_sorties, .id = "Année") %>%
  dplyr::select(Année, NB_PERS_CHARGE_rep, count, percentage)


#entrees
pourcentages_par_annee_entrees <- list()

# Boucle pour calculer les pourcentages pour chaque année
for (annee in 2018:2022) {
  # Filtrer les données pour l'année en cours
  Liste_entrees_annee <- Liste_entrees %>%
    filter(year(Date_Entrees) == annee)
  
  # Calculer le nombre d'entrées par modalité de NB_PERS_CHARGE_rep
  entrees_counts <- Liste_entrees_annee %>%
    group_by(NB_PERS_CHARGE_rep) %>%
    summarise(count = n()) %>%
    ungroup()
  
  # Calculer les pourcentages par modalité de NB_PERS_CHARGE_rep
  NB_PERS_CHARGE_rep_percentages_entrees <- entrees_counts %>%
    group_by(NB_PERS_CHARGE_rep) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    ungroup()
  
  # Ajouter les pourcentages de l'année à la liste
  pourcentages_par_annee_entrees[[as.character(annee)]] <- NB_PERS_CHARGE_rep_percentages_entrees
}

# Combiner les résultats pour toutes les années pour les entrées
combined_table_NB_PERS_CHARGE_rep_entrees <- bind_rows(pourcentages_par_annee_entrees, .id = "Année") %>%
  dplyr::select(Année, NB_PERS_CHARGE_rep, count, percentage)


###Visualisation 
# Convertir la variable Année en facteur pour assurer l'ordre chronologique dans le graphique
combined_table_NB_PERS_CHARGE_rep_entrees$Année <- factor(combined_table_NB_PERS_CHARGE_rep_entrees$Année, levels = as.character(2018:2022))

#Evolution de Nombre de personnes à charge des allocataires de RSA
# Créer le graphique en barres empilées
graphique <- ggplot(combined_table_NB_PERS_CHARGE_rep_entrees, aes(x = Année, y = count, fill = NB_PERS_CHARGE_rep)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "Nombre d'entrées", fill = "Graphique 4.1.3.2") +
  scale_fill_discrete(name = "Nombre de personnes à charge") +
  theme_minimal() +
  ggtitle("Graphique 4.1.3.2.1") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Ajouter les étiquettes de pourcentage au-dessus de chaque barre
graphique <- graphique +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 3)

# Afficher le graphique
print(graphique)

# Convertir la variable Année en facteur pour assurer l'ordre chronologique dans le graphique
combined_table_NB_PERS_CHARGE_rep_sorties$Année <- factor(combined_table_NB_PERS_CHARGE_rep_sorties$Année, levels = as.character(2018:2022))

# Évolution du nombre de personnes à charge des allocataires de RSA
# Créer le graphique en barres empilées
graphique <- ggplot(combined_table_NB_PERS_CHARGE_rep_sorties, aes(x = Année, y = count, fill = NB_PERS_CHARGE_rep)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "Nombre de sorties", fill = "Graphique 4.1.3.2") +
  scale_fill_discrete(name = "Nombre de personnes à charge") +
  theme_minimal() +
  ggtitle("Graphique 4.1.3.2.2") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Ajouter les étiquettes de pourcentage au-dessus de chaque barre
graphique <- graphique +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 3)

# Afficher le graphique
print(graphique)

##Age_rep

# Convertir la variable Année en facteur pour assurer l'ordre chronologique dans le graphique
combined_table_Age_rep_entrees$Année <- factor(combined_table_Age_rep_entrees$Année, levels = as.character(2018:2022))

# Évolution du Tranche d'âge des entrees
# Créer le graphique en barres empilées
graphique_entrees <- ggplot(combined_table_Age_rep_entrees, aes(x = Année, y = count, fill = Age_rep)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "Nombre d'entrées", fill = "Tranche d'âge") +
  scale_fill_discrete(name = "Tranche d'âge") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("Graphique 4.1.3.3.1")


# Ajouter les étiquettes de pourcentage au-dessus de chaque barre
graphique_entrees <- graphique_entrees +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 3)

# Afficher le graphique
print(graphique_entrees)

# Convertir la variable Année en facteur pour assurer l'ordre chronologique dans le graphique
combined_table_Age_rep_sorties$Année <- factor(combined_table_Age_rep_sorties$Année, levels = as.character(2018:2022))

# Évolution du Tranche d'âge des sorties
# Créer le graphique en barres empilées
graphique_sorties <- ggplot(combined_table_Age_rep_sorties, aes(x = Année, y = count, fill = Age_rep)) +
  geom_bar(stat = "identity") +
  labs(x = "Année", y = "Nombre de sorties", fill = "Tranche d'âge") +
  scale_fill_discrete(name = "Tranche d'âge") +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("Graphique 4.1.3.3.2")


# Ajouter les étiquettes de pourcentage au-dessus de chaque barre
graphique_sorties <- graphique_sorties +
  geom_text(aes(label = paste0(round(percentage), "%")), position = position_stack(vjust = 0.5), size = 3)

# Afficher le graphique
print(graphique_sorties)

##Carte1

#TELECHARGER LE FICHIER 
# Spécifier le chemin vers le fichier GeoJSON
chemin_fichier <- "C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/Insertion/Programme_et_data_BRSA/DataBRSA/communes-france.geojson"

# Lire le fichier GeoJSON
communes <- st_read(chemin_fichier)

#créer la data.frame d'evolution d'effectif entre 2018 et 2022
filtered_data <- combined_table_NOM_COM_entrees[(combined_table_NOM_COM_entrees$Année == 2018 | combined_table_NOM_COM_entrees$Année == 2019 | combined_table_NOM_COM_entrees$Année == 2022
                                                 | combined_table_NOM_COM_entrees$Année == 2021 ), ]
spread_data <- filtered_data %>%
  dplyr::select(NOM_COM, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data$evolution_2018_2022 <- ifelse(spread_data$`2018` == 0, 
                                          ((spread_data$`2022` - spread_data$`2018`) / spread_data$`2019`) * 100,
                                          ((spread_data$`2022` - spread_data$`2018`) / spread_data$`2018`) * 100)

spread_data_1 <- filtered_data %>%
  dplyr::select(NOM_COM, Année, Ration_nombre_entrant) %>%
  spread(Année, Ration_nombre_entrant) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data_1$evolution_ratio_2018_2022 <- ifelse(spread_data_1$`2018` == 0, 
                                                  ((spread_data_1$`2022` - spread_data_1$`2018`) / spread_data_1$`2019`) * 100,
                                                  ((spread_data_1$`2022` - spread_data_1$`2018`) / spread_data_1$`2018`) * 100)




# Créer une liste des modalités de NOM_COM1
modalites_NOM_COM1 <- unique(communes$com_name)

# Fonction pour trouver la modalité la plus proche
find_closest_match <- function(name, modalites) {
  name_lower <- tolower(name)
  modalites_lower <- tolower(modalites)
  
  # Correspondances spécifiques pour certains noms de commune
  if (name_lower == "st uniac") {
    return("Epiniac")
  } else if (name_lower == "brece") {
    return("Brécé")
  } else if (name_lower == "st gilles") {
    return("Saint-Gilles")
  } else if (name_lower == "st peran") {
    return("Saint-Péran")
  } else if (name_lower == "st pere") {
    return("Saint-Père-Marc-en-Poulet")
  }
  
  # Correspondance générale en utilisant la logique de distance minimale
  distances <- stringdist::stringdist(name_lower, modalites_lower)
  closest_index <- which.min(distances)
  closest_match <- modalites[closest_index]
  
  return(closest_match)
}

## Ajouter la colonne "com_name" à spread_data
spread_data$com_name <- sapply(spread_data$NOM_COM, function(nom_com) {
  closest_match <- find_closest_match(nom_com, modalites_NOM_COM1)
  return(closest_match)
})

# Fusionner les données des communes avec les données
communes <- merge(communes, spread_data, by.x = "com_name", by.y = "com_name", all.x = TRUE)

## Ajouter la colonne "com_name" à spread_data
spread_data_1$com_name <- sapply(spread_data_1$NOM_COM, function(nom_com) {
  closest_match <- find_closest_match(nom_com, modalites_NOM_COM1)
  return(closest_match)
})

# Fusionner les données des communes avec les données
communes1 <- merge(communes, spread_data_1, by.x = "com_name", by.y = "com_name", all.x = TRUE)


#affichage interactive 

#COMMUNE
#ENRTREES

# Créer les intervalles pour evolution_2018_2022
intervals_evolution <- c(-Inf, -75, -50, -25, 0, 25, 50, 75, 100, 200, 400, 600, Inf)

# Créer la palette de couleurs pour evolution_2018_2022
colors_evolution <- colorRampPalette(c("blue", "white", "red"))(length(intervals_evolution) - 1)

# Déterminer les limites pour les intervalles de la deuxième carte
min_value_2018 <- min(communes$`2018`, na.rm = TRUE)
max_value_2018 <- max(communes$`2018`, na.rm = TRUE)
interval_length_2018 <- (max_value_2018 - min_value_2018) / 20
intervals_2018 <- seq(min_value_2018, max_value_2018, by = interval_length_2018)

# Définir les regroupements de modalités pour la deuxième carte avec 8 catégories
groupings_2018 <- c(0, 5, 10, 25, 50, 100, 200, 300, 500, 800, 1000, max_value_2018)

# Créer la palette de couleurs pour 2018
colors_2018 <- colorRampPalette(c("green", "red"))(length(groupings_2018) - 1)

# Créer la carte pour evolution_2018_2022 ("Evolution du nombre d'entrees par habitant entre 2018 et 2022")
map_evolution <- tm_shape(communes) +
  tm_polygons(col = "evolution_2018_2022", palette = colors_evolution, breaks = intervals_evolution, midpoint = NA, title = "Evolution du nombre d'entrees") +
  #tm_text(text = "com_code", size = 0.6, col = "black") +  # Ajouter les noms des communes
  tm_layout(legend.outside = TRUE)

# Créer la carte pour 2018 avec 8 catégories
map_2018 <- tm_shape(communes) +
  tm_fill(col = "2018", palette = colors_2018, breaks = groupings_2018, labels = c("0-5", "5-10", "10-25", "25-50", "50-100", "100-200", "200-300", "300-500", "500-800", "800-1000", paste("1000-", max_value_2018)), 
          title = "Nombre d'entrées en 2018") +
  #tm_text(text = "com_code", size = 0.6, col = "black") +
  tm_layout(legend.outside = TRUE)

## Creer la carte pour evolution_ratio_2018_2022 ("Evolution du ratio d'entree par habitant entre 2018 et 2022")
#map_evolution_ratio_2018_2022 <- tm_shape(communes1) +
#  tm_polygons(col = "evolution_ratio_2018_2022", palette = colors_evolution, breaks = intervals_evolution, midpoint = NA, title = "Evolution du ratio d'entree") +
#  #tm_text(text = "com_name", size = 0.8, col = "black") +  # Ajouter les noms des communes
#  tm_layout(legend.outside = TRUE)

# Afficher les deux cartes
map_evolution
map_2018
#map_evolution_ratio_2018_2022

## Exporter la carte d'évolution
#tmap_save(map_evolution, filename = "map_evolution_entrees.png", width = 800, height = 600)
#
## Exporter la carte de 2018
#tmap_save(map_2018, filename = "map_2018_etrees.png", width = 800, height = 600)


#SORTIES

#carte 2

# Créer la data.frame d'évolution d'effectif entre 2018 et 2022 pour les sorties
filtered_data1 <- combined_table_NOM_COM_sorties[(combined_table_NOM_COM_sorties$Année == 2018 | combined_table_NOM_COM_sorties$Année == 2019 | combined_table_NOM_COM_sorties$Année == 2022
                                                  | combined_table_NOM_COM_sorties$Année == 2021), ]
spread_data1 <- filtered_data1 %>%
  dplyr::select(NOM_COM, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data1$evolution_2018_2022 <- ifelse(spread_data1$`2018` == 0, 
                                           ((spread_data1$`2022` - spread_data1$`2018`) / spread_data1$`2019`) * 100,
                                           ((spread_data1$`2022` - spread_data1$`2018`) / spread_data1$`2018`) * 100)

spread_data_1 <- filtered_data1 %>%
  dplyr::select(NOM_COM, Année, Ration_nombre_sortant) %>%
  spread(Année, Ration_nombre_sortant) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data_1$evolution_ratio_2018_2022 <- ifelse(spread_data_1$`2018` == 0, 
                                                  ((spread_data_1$`2022` - spread_data_1$`2018`) / spread_data_1$`2019`) * 100,
                                                  ((spread_data_1$`2022` - spread_data_1$`2018`) / spread_data_1$`2018`) * 100)

#
# Lire le fichier GeoJSON
communes <- st_read(chemin_fichier)

## Ajouter la colonne "com_name" à spread_data1
spread_data1$com_name <- sapply(spread_data1$NOM_COM, function(nom_com) {
  closest_match <- find_closest_match(nom_com, modalites_NOM_COM1)
  return(closest_match)
})

# Fusionner les données des communes avec les données d'évolution pour les sorties
communes <- merge(communes, spread_data1, by.x = "com_name", by.y = "com_name", all.x = TRUE)

## Ajouter la colonne "com_name" à spread_data
spread_data_1$com_name <- sapply(spread_data_1$NOM_COM, function(nom_com) {
  closest_match <- find_closest_match(nom_com, modalites_NOM_COM1)
  return(closest_match)
})

# Fusionner les données des communes avec les données
communes1 <- merge(communes, spread_data_1, by.x = "com_name", by.y = "com_name", all.x = TRUE)

#SORTIES
# Créer les intervalles pour evolution_2018_2022
intervals_evolution_sorties <- c(-Inf, -75, -50, -25, 0, 25, 50, 75, 100, 200, 400, 800, Inf)

# Créer la palette de couleurs pour evolution_2018_2022
colors_evolution_sorties <- colorRampPalette(c("blue", "white", "red"))(length(intervals_evolution_sorties) - 1)

# Déterminer les limites pour les intervalles de la deuxième carte
min_value_2018_sorties <- min(communes$`2018`, na.rm = TRUE)
max_value_2018_sorties <- max(communes$`2018`, na.rm = TRUE)
interval_length_2018_sorties <- (max_value_2018_sorties - min_value_2018_sorties) / 20
intervals_2018_sorties <- seq(min_value_2018_sorties, max_value_2018_sorties, by = interval_length_2018_sorties)

# Définir les regroupements de modalités pour la deuxième carte avec 8 catégories
groupings_2018_sorties <-  c(0, 5, 10, 25, 50, 100, 200, 300, 400, max_value_2018)

# Créer la palette de couleurs pour 2018
colors_2018_sorties <- colorRampPalette(c("green", "red"))(length(groupings_2018_sorties) - 1)

# Créer la carte pour evolution_2018_2022 pour les sorties ("Evolution du nombre de sorties par habitant entre 2018 et 2022")
map_evolution_sorties <- tm_shape(communes) +
  tm_polygons(col = "evolution_2018_2022", palette = colors_evolution_sorties, breaks = intervals_evolution_sorties, midpoint = NA, title = "Evolution du nombre de sorties") +
  #tm_text(text = "com_code", size = 0.6, col = "black") + 
  tm_layout(legend.outside = TRUE)

# Créer la carte pour 2018 avec 8 catégories pour les sorties
map_2018_sorties <- tm_shape(communes) +
  tm_fill(col = "2018", palette = colors_2018_sorties, breaks = groupings_2018_sorties, labels = c("0-5", "5-10","10-25", "25-50","50-100", "100-200", "200-400", paste("400-", max_value_2018_sorties)), 
          title = "Nombre de sorties en 2018") +
  #tm_text(text = "com_code", size = 0.6, col = "black") + 
  tm_layout(legend.outside = TRUE)

## Creer la carte pour evolution_ratio_2018_2022  ("Evolution du ratio de sorties par habitant entre 2018 et 2022")
#map_evolution_ratio_2018_2022_sorties <- tm_shape(communes1) +
#  tm_polygons(col = "evolution_ratio_2018_2022", palette = colors_evolution, breaks = intervals_evolution, midpoint = NA, title = "Evolution du ratio de sorties") +
#  #tm_text(text = "com_code", size = 0.8, col = "black") + 
#  tm_layout(legend.outside = TRUE)

# Afficher les deux cartes pour les sorties
map_evolution_sorties
map_2018_sorties
#map_evolution_ratio_2018_2022_sorties

#setwd("C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/RAPPORT_Alternance/Capture")
## Exporter la carte d'évolution
#tmap_save(map_evolution_sorties, filename = "map_evolution_sorties.png", width = 800, height = 600)
#
## Exporter la carte de 2018
#tmap_save(map_2018_sorties, filename = "map_2018_sorties.png", width = 800, height = 600)

#Tableau HTML 1
#entrees
filtered_data <- combined_table_NOM_SECTEUR_entrees[(combined_table_NOM_SECTEUR_entrees$Année == 2018 | combined_table_NOM_SECTEUR_entrees$Année == 2019 | combined_table_NOM_SECTEUR_entrees$Année == 2022
                                                     | combined_table_NOM_SECTEUR_entrees$Année == 2021) & combined_table_NOM_SECTEUR_entrees$NOM_SECTEUR != "HORS DEPARTEMENT", ]
spread_data <- filtered_data %>%
  dplyr::select(NOM_SECTEUR, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

#spread_data$evolution_2018_2022 <- ifelse(spread_data$`2018` == 0, 
#                                          round(((spread_data$`2022` - spread_data$`2018`) / spread_data$`2019`) * 100, 1),
#                                          round(((spread_data$`2022` - spread_data$`2018`) / spread_data$`2018`) * 100, 1))

spread_data_1 <- filtered_data %>%
  dplyr::select(NOM_SECTEUR, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data_1 <- spread_data_1 %>%
  mutate(evolution_ratio_2018_2022 = ifelse(`2018` == 0, 
                                            ((`2022` - `2018`) / `2019`) * 100,
                                            ((`2022` - `2018`) / `2018`) * 100)) %>%
  dplyr::select(NOM_SECTEUR, evolution_ratio_2018_2022) %>%
  mutate(evolution_ratio_2018_2022 = round(evolution_ratio_2018_2022, 1))


spread_data <- merge(spread_data, spread_data_1, by = "NOM_SECTEUR")


spread_data <- spread_data %>%
  arrange(desc(evolution_ratio_2018_2022))

# Réorganiser les colonnes
spread_data <- spread_data %>%
  dplyr::select(NOM_SECTEUR, `2018`, `2022`, `evolution_ratio_2018_2022`)

#renommer les colonnes
spread_data <- spread_data %>%
  rename("Nombre d'entrees en 2018" = `2018`,
         "Nombre d'entrees en 2022" = `2022`,
         "Evolution du nombre d'entree entre 2018 et 2022" = evolution_ratio_2018_2022)


# Générer le tableau HTML avec le titre
html_table <- spread_data %>%
  kable(format = "html", caption = "Graphique 4.1.3.4.1") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


print(html_table)
####exporter 
## Enregistrer la capture d'écran du tableau entrees au format PNG#
## Spécifier la taille de la page Word en pixels
##largeur_page_word <- 800  # Largeur de la page Word en pixels
##hauteur_page_word <- 600  # Hauteur de la page Word en pixels
#
## Enregistrer le code HTML dans un fichier temporaire
#temp_file <- tempfile(fileext = ".html")
#writeLines(html_table, temp_file)
#
## Capturer une capture d'écran du fichier HTML avec les dimensions spécifiées
#output_file <- "tableau.png"
#webshot(temp_file, output_file)#, cliprect = c(0, 0, largeur_page_word, hauteur_page_word)
#
## Supprimer le fichier temporaire HTML
#file.remove(temp_file)

#sorties
filtered_data <- combined_table_NOM_SECTEUR_sorties[(combined_table_NOM_SECTEUR_sorties$Année == 2018 | combined_table_NOM_SECTEUR_sorties$Année == 2019 | combined_table_NOM_SECTEUR_sorties$Année == 2022
                                                     | combined_table_NOM_SECTEUR_sorties$Année == 2020) & combined_table_NOM_SECTEUR_sorties$NOM_SECTEUR != "HORS DEPARTEMENT", ]
spread_data <- filtered_data %>%
  dplyr::select(NOM_SECTEUR, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2021` = 0))

#spread_data$evolution_2018_2021 <- ifelse(spread_data$`2018` == 0, 
#                                          round(((spread_data$`2021` - spread_data$`2018`) / spread_data$`2019`) * 100, 1),
#                                          round(((spread_data$`2021` - spread_data$`2018`) / spread_data$`2018`) * 100, 1))
spread_data_1 <- filtered_data %>%
  dplyr::select(NOM_SECTEUR, Année, count) %>%
  spread(Année, count) %>%
  replace_na(list(`2018` = 0, `2022` = 0))

spread_data_1 <- spread_data_1 %>%
  mutate(evolution_ratio_2018_2022 = ifelse(`2018` == 0, 
                                            ((`2022` - `2018`) / `2019`) * 100,
                                            ((`2022` - `2018`) / `2018`) * 100)) %>%
  dplyr::select(NOM_SECTEUR, evolution_ratio_2018_2022) %>%
  mutate(evolution_ratio_2018_2022 = round(evolution_ratio_2018_2022, 1))


spread_data <- merge(spread_data, spread_data_1, by = "NOM_SECTEUR")

#spread_data <- spread_data %>%
#  dplyr::arrange(evolution_ratio_2018_2022)

spread_data <- spread_data %>%
  arrange(desc(evolution_ratio_2018_2022))

# Réorganiser les colonnes
spread_data <- spread_data %>%
  dplyr::select(NOM_SECTEUR, `2018`, `2022`, `evolution_ratio_2018_2022`)

#renommer les colonnes
spread_data <- spread_data %>%
  rename("Nombre de sorties en 2018" = `2018`,
         "Nombre de sorties en 2022" = `2022`,
         "Evolution du nombre de sortie entre 2018 et 2022" = evolution_ratio_2018_2022)

# Générer le tableau HTML avec le titre
html_table1 <- spread_data %>%
  kable(format = "html", caption = "Graphique 4.1.3.4.2") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

print(html_table1)

##exporter 
## Enregistrer le code HTML dans un fichier temporaire
#temp_file <- tempfile(fileext = ".html")
#writeLines(html_table1, temp_file)
#
## Capturer une capture d'écran du fichier HTML et l'enregistrer sous forme d'image
#output_file <- "tableauPNG.png"
#webshot(temp_file, output_file)
#
## Supprimer le fichier temporaire HTML
#file.remove(temp_file)
