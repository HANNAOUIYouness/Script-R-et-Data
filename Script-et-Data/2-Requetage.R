library(RODBC)
library(DBI)
library(tidyverse)
library(RMySQL)

#ODBC Database Connectivity

# Se connecter ? la source de donn?es Oracle
con <- odbcConnect("----------", uid = "-------", pwd = "--------")

# Fermer la connexion
#odbcClose(con)

#Fonction renvoie une liste de tables disponible dans la base de donn?es Oracle connect? ? 'con'
Liste_table <- sqlTables(con)


#Liste colonnes de la table INDIVIDU 
liste_colonnes_INDIVIDU <-  sqlColumns(con,"INDIVIDU")
liste_colonnes_MENAGE_RSA <-  sqlColumns(con,"MENAGE_RSA")
liste_colonnes_DOSSIER <-  sqlColumns(con,"DOSSIER")
liste_colonnes_PRESENCE_RSA <-  sqlColumns(con,"PRESENCE_RSA")
liste_colonnes_INTEGRATION_CAF_RSA <-  sqlColumns(con,"INTEGRATION_CAF_RSA")
liste_colonnes_ETAT_DROIT_RSA <-  sqlColumns(con,"ETAT_DROIT_RSA")
liste_colonnes_CONTEXTE_FAMILIAL <-  sqlColumns(con,"CONTEXTE_FAMILIAL")
liste_colonnes_ADRESSE_INDIVIDU <-  sqlColumns(con,"ADRESSE_INDIVIDU")
liste_colonnes_UNITE_TERRITORIALE <-  sqlColumns(con,"UNITE_TERRITORIALE")
liste_colonnes_ADRESSE_CAF <-  sqlColumns(con,"ADRESSE_CAF")
liste_colonnes_RECHERCHE_ADRESSE <-  sqlColumns(con,"RECHERCHE_ADRESSE")
liste_colonnes_LIEU <-  sqlColumns(con,"LIEU")
liste_colonnes_COMMUNE <-  sqlColumns(con,"COMMUNE")
liste_colonnes_ORGANISME <-  sqlColumns(con,"ORGANISME")
liste_colonnes_ACTION_INSERTION_RSA <-  sqlColumns(con,"ACTION_INSERTION_RSA")
liste_colonnes_SECTEUR <-  sqlColumns(con,"SECTEUR")



# Cr?er une liste d'ann?es entre 2014 et 2023
annees <- 2013:2023
# D?finir la fonction Requete pour ex?cuter la requ?te SQL pour une ann?e donn?e
Requete <- function(annee) {
  # Modifier la requ?te pour filtrer les r?sultats pour l'ann?e en cours
  query <- paste0("SELECT INDIVIDU.INDEX_DOSSIER , INDIVIDU.INDEX_INDIVIDU, MENAGE_RSA.INDEX_ALLOCATAIRE , MENAGE_RSA.INDEX_CONJOINT ,
                   INDIVIDU.DATE_NAISSANCE_INDIVIDU, INDIVIDU.SEXE_INDIVIDU,
                   PRESENCE_RSA.DATE_DEBUT_PRESENCE_RSA, PRESENCE_RSA.DATE_FIN_PRESENCE_RSA, 
                   PRESENCE_RSA.DERNIERE_PRESENCE_RSA,
                   CONTEXTE_FAMILIAL.LC_CONTEXTE_FAMILIAL,
                   INTEGRATION_CAF_RSA.NB_PERS_CHARGE,
                   INTEGRATION_CAF_RSA.MTT_TOTAL_RSA, INTEGRATION_CAF_RSA.MTT_RMG_RSA, INTEGRATION_CAF_RSA.MTT_RESS_MENS_RSA,
                   INTEGRATION_CAF_RSA.CODE_ETAT_DROIT_RSA ,
                   INTEGRATION_CAF_RSA.DROIT_DEVOIR_FOY , INTEGRATION_CAF_RSA.ALL_DROIT_DEV , INTEGRATION_CAF_RSA.CONJ_DROIT_DEV,
                   INTEGRATION_CAF_RSA.DATE_REFERENCE_FLUX,
                   LIEU.CODE_POSTAL , COMMUNE.NOM_COM, LIEU.CODE_SECTEUR, SECTEUR.NOM_SECTEUR
                   FROM INDIVIDU 
                   INNER JOIN DOSSIER ON INDIVIDU.INDEX_DOSSIER = DOSSIER.INDEX_DOSSIER
                   INNER JOIN MENAGE_RSA ON DOSSIER.INDEX_DOSSIER = MENAGE_RSA.INDEX_DOSSIER
                   INNER JOIN PRESENCE_RSA ON MENAGE_RSA.INDEX_MENAGE_RSA = PRESENCE_RSA.INDEX_MENAGE_RSA
                   INNER JOIN CONTEXTE_FAMILIAL ON DOSSIER.CODE_CONTEXTE_FAMILIAL = CONTEXTE_FAMILIAL.CODE_CONTEXTE_FAMILIAL
                   INNER JOIN INTEGRATION_CAF_RSA ON PRESENCE_RSA.INDEX_PRESENCE_RSA = INTEGRATION_CAF_RSA.INDEX_PRESENCE_RSA
                   LEFT JOIN COMMUNE ON DOSSIER.CODE_DEP = COMMUNE.CODE_DEP AND DOSSIER.CODE_A_COM = COMMUNE.CODE_A_COM
                   INNER JOIN lieu ON lieu.code_dep = dossier.code_dep
                                     AND lieu.code_a_com = dossier.code_a_com
                                     AND lieu.code_lieu = dossier.code_lieu
                   INNER JOIN secteur ON lieu.code_secteur = secteur.code_secteur
                   WHERE EXTRACT(YEAR FROM INTEGRATION_CAF_RSA.DATE_REFERENCE_FLUX) = ", annee,"
AND CODE_ETAT_DROIT_RSA = 3;")
  
  #", annee,"
  # Ex?cuter la requ?te
  resultats <- sqlQuery(con, query)
  
  # Retourner les r?sultats
  return(resultats)
}



# Boucle sur chaque ann?e de la liste
for (annee in annees) {
  
  # Appliquer la fonction Requete pour l'ann?e en cours
  resultats <- Requete(annee)
  
  # Filtrer les r?sultats pour le mois le plus r?cent de chaque individu
  resultats <- resultats %>%
    arrange(INDEX_INDIVIDU, desc(DATE_REFERENCE_FLUX)) %>%
    mutate(MOIS_DATE_REFERENCE_FLUX = as.integer(substr(DATE_REFERENCE_FLUX, 6, 7))) %>%
    group_by(MOIS_DATE_REFERENCE_FLUX, INDEX_INDIVIDU) %>%
    filter(DATE_REFERENCE_FLUX == max(DATE_REFERENCE_FLUX))
  #suppression des doublons
  resultats <- resultats[!duplicated(resultats), ]
  
  # Exporter les r?sultats dans un fichier CSV
  nom_fichier <- paste0("requete_", annee, ".csv")
  chemin_fichier <- file.path("D:/ProfilDomaine/yhannaoui1/Bureau/Insertion/requete20142023", nom_fichier)
  write.csv2(resultats, file = chemin_fichier, row.names = FALSE)
}

query <- "SELECT INDIVIDU.INDEX_DOSSIER , INDIVIDU.INDEX_INDIVIDU, MENAGE_RSA.INDEX_ALLOCATAIRE , MENAGE_RSA.INDEX_CONJOINT ,
                   INDIVIDU.DATE_NAISSANCE_INDIVIDU, INDIVIDU.SEXE_INDIVIDU,
                   PRESENCE_RSA.DATE_DEBUT_PRESENCE_RSA, PRESENCE_RSA.DATE_FIN_PRESENCE_RSA, 
                   PRESENCE_RSA.DERNIERE_PRESENCE_RSA,
                   CONTEXTE_FAMILIAL.LC_CONTEXTE_FAMILIAL,
                   INTEGRATION_CAF_RSA.NB_PERS_CHARGE,
                   INTEGRATION_CAF_RSA.MTT_TOTAL_RSA, INTEGRATION_CAF_RSA.MTT_RMG_RSA, INTEGRATION_CAF_RSA.MTT_RESS_MENS_RSA,
                   INTEGRATION_CAF_RSA.CODE_ETAT_DROIT_RSA ,
                   INTEGRATION_CAF_RSA.DROIT_DEVOIR_FOY , INTEGRATION_CAF_RSA.ALL_DROIT_DEV , INTEGRATION_CAF_RSA.CONJ_DROIT_DEV,
                   INTEGRATION_CAF_RSA.DATE_REFERENCE_FLUX,
                   LIEU.CODE_POSTAL , COMMUNE.NOM_COM, LIEU.CODE_SECTEUR, SECTEUR.NOM_SECTEUR
                   FROM INDIVIDU 
                   INNER JOIN DOSSIER ON INDIVIDU.INDEX_DOSSIER = DOSSIER.INDEX_DOSSIER
                   INNER JOIN MENAGE_RSA ON DOSSIER.INDEX_DOSSIER = MENAGE_RSA.INDEX_DOSSIER
                   INNER JOIN PRESENCE_RSA ON MENAGE_RSA.INDEX_MENAGE_RSA = PRESENCE_RSA.INDEX_MENAGE_RSA
                   INNER JOIN CONTEXTE_FAMILIAL ON DOSSIER.CODE_CONTEXTE_FAMILIAL = CONTEXTE_FAMILIAL.CODE_CONTEXTE_FAMILIAL
                   INNER JOIN INTEGRATION_CAF_RSA ON PRESENCE_RSA.INDEX_PRESENCE_RSA = INTEGRATION_CAF_RSA.INDEX_PRESENCE_RSA
                   LEFT JOIN COMMUNE ON DOSSIER.CODE_DEP = COMMUNE.CODE_DEP AND DOSSIER.CODE_A_COM = COMMUNE.CODE_A_COM
                   INNER JOIN lieu ON lieu.code_dep = dossier.code_dep
                                     AND lieu.code_a_com = dossier.code_a_com
                                     AND lieu.code_lieu = dossier.code_lieu
                   INNER JOIN secteur ON lieu.code_secteur = secteur.code_secteur
                   WHERE EXTRACT(YEAR FROM INTEGRATION_CAF_RSA.DATE_REFERENCE_FLUX) >= 2013
                   AND EXTRACT(YEAR FROM INTEGRATION_CAF_RSA.DATE_REFERENCE_FLUX) < 2019
AND CODE_ETAT_DROIT_RSA = 3;"

result <- sqlQuery(con, query)

result <- result %>%
  arrange(INDEX_INDIVIDU, desc(DATE_REFERENCE_FLUX)) %>%
  mutate(MOIS_DATE_REFERENCE_FLUX = as.integer(substr(DATE_REFERENCE_FLUX, 6, 7))) %>%
  mutate(ANNEE_DATE_REFERENCE_FLUX = as.integer(substr(DATE_REFERENCE_FLUX, 1, 4))) %>%
  group_by(ANNEE_DATE_REFERENCE_FLUX, MOIS_DATE_REFERENCE_FLUX, INDEX_INDIVIDU) %>%
  filter(DATE_REFERENCE_FLUX == max(DATE_REFERENCE_FLUX))

## suppression des doublons (ex : 886030, 35269, 37687, 38253)
result <- result[!duplicated(result), ]

