#Packages et bibliotheques
install.packages("tidyverse")
install.packages("tuple")
library("readr")
library("tidyverse")
library("dplyr")
library("readxl")
library("lubridate")
library("tuple")
library("ggplot2")

#nstall.packages("devtools")
#nstall.packages("usethis")
#ibrary(devtools)
#nstall_github("easyGgplot2")
#library(easyGgplot2)

#library(rAmCharts)

#Repertoire de travail 
#setwd()


#Importation des donnees
BRSA_01_21 <- read_csv2("Stats_Etude_Actu_01_21.csv", locale =locale(encoding="latin1"))
BRSA_01_21 <- BRSA_01_21 %>% add_column(Date = "01/01/2021", .before = "Index Individu")

BRSA_02_21 <- read_csv2("Stats_Etude_Actu_02_21.csv", locale =locale(encoding="latin1"))
BRSA_02_21 <- BRSA_02_21 %>% add_column(Date = "01/02/2021", .before = "Index Individu")

BRSA_03_21 <- read_csv2("Stats_Etude_Actu_03_21.csv", locale =locale(encoding="latin1"))
BRSA_03_21 <- BRSA_03_21 %>% add_column(Date = "01/03/2021", .before = "Index Individu")

BRSA_04_21 <- read_csv2("Stats_Etude_Actu_04_21.csv", locale =locale(encoding="latin1"))
BRSA_04_21 <- BRSA_04_21 %>% add_column(Date = "01/04/2021", .before = "Index Individu")

BRSA_05_21 <- read_csv2("Stats_Etude_Actu_05_21.csv", locale =locale(encoding="latin1"))
BRSA_05_21 <- BRSA_05_21 %>% add_column(Date = "01/05/2021", .before = "Index Individu")

BRSA_06_21 <- read_csv2("Stats_Etude_Actu_06_21.csv", locale =locale(encoding="latin1"))
BRSA_06_21 <- BRSA_06_21 %>% add_column(Date = "01/06/2021", .before = "Index Individu")

BRSA_07_21 <- read_csv2("Stats_Etude_Actu_07_21.csv", locale =locale(encoding="latin1"))
BRSA_07_21 <- BRSA_07_21 %>% add_column(Date = "01/07/2021", .before = "Index Individu")

BRSA_08_21 <- read_csv2("Stats_Etude_Actu_08_21.csv", locale =locale(encoding="latin1"))
BRSA_08_21 <- BRSA_08_21 %>% add_column(Date = "01/08/2021", .before = "Index Individu")

BRSA_09_21 <- read_csv2("Stats_Etude_Actu_09_21.csv", locale =locale(encoding="latin1"))
BRSA_09_21 <- BRSA_09_21 %>% add_column(Date = "01/09/2021", .before = "Index Individu") 

BRSA_10_21 <- read_csv2("Stats_Etude_Actu_10_21.csv", locale =locale(encoding="latin1"))
BRSA_10_21 <- BRSA_10_21 %>% add_column(Date = "01/10/2021", .before = "Index Individu")

BRSA_11_21 <- read_csv2("Stats_Etude_Actu_11_21.csv", locale =locale(encoding="latin1"))
BRSA_11_21 <- BRSA_11_21 %>% add_column(Date = "01/11/2021", .before = "Index Individu")

BRSA_12_21 <- read_csv2("Stats_Etude_Actu_12_21.csv", locale =locale(encoding="latin1"))
BRSA_12_21 <- BRSA_12_21 %>% add_column(Date = "01/12/2021", .before = "Index Individu")

BRSA_01_22 <- read_csv2("Stats_Etude_Actu_01_22.csv", locale =locale(encoding="latin1"))
BRSA_01_22 <- BRSA_01_22 %>% add_column(Date = "01/01/2022", .before = "Index Individu")

BRSA_02_22 <- read_csv2("Stats_Etude_Actu_02_22.csv", locale =locale(encoding="latin1"))
BRSA_02_22 <- BRSA_02_22 %>% add_column(Date = "01/02/2022", .before = "Index Individu")

BRSA_03_22 <- read_csv2("Stats_Etude_Actu_03_22.csv", locale =locale(encoding="latin1"))
BRSA_03_22 <- BRSA_03_22 %>% add_column(Date = "01/03/2022", .before = "Index Individu")

BRSA_04_22 <- read_csv2("Stats_Etude_Actu_04_22.csv", locale =locale(encoding="latin1"))
BRSA_04_22 <- BRSA_04_22 %>% add_column(Date = "01/04/2022", .before = "Index Individu")

BRSA_05_22 <- read_csv2("Stats_Etude_Actu_05_22.csv", locale =locale(encoding="latin1"))
BRSA_05_22 <- BRSA_05_22 %>% add_column(Date = "01/05/2022", .before = "Index Individu")

BRSA_06_22 <- read_csv2("Stats_Etude_Actu_06_22.csv", locale =locale(encoding="latin1"))
BRSA_06_22 <- BRSA_06_22 %>% add_column(Date = "01/06/2022", .before = "Index Individu")

BRSA_07_22 <- read_csv2("Stats_Etude_Actu_07_22.csv", locale =locale(encoding="latin1"))
BRSA_07_22 <- BRSA_07_22 %>% add_column(Date = "01/07/2022", .before = "Index Individu")

BRSA_08_22 <- read_csv2("Stats_Etude_Actu_08_22.csv", locale =locale(encoding="latin1"))
BRSA_08_22 <- BRSA_08_22 %>% add_column(Date = "01/08/2022", .before = "Index Individu")

BRSA_09_22 <- read_csv2("Stats_Etude_Actu_09_22.csv", locale =locale(encoding="latin1"))
BRSA_09_22 <- BRSA_09_22 %>% add_column(Date = "01/09/2022", .before = "Index Individu") 

BRSA_10_22 <- read_csv2("Stats_Etude_Actu_10_22.csv", locale =locale(encoding="latin1"))
BRSA_10_22 <- BRSA_10_22 %>% add_column(Date = "01/10/2022", .before = "Index Individu")

BRSA_11_22 <- read_csv2("Stats_Etude_Actu_11_22.csv", locale =locale(encoding="latin1"))
BRSA_11_22 <- BRSA_11_22 %>% add_column(Date = "01/11/2022", .before = "Index Individu")

BRSA_12_22 <- read_csv2("Stats_Etude_Actu_12_22.csv", locale =locale(encoding="latin1"))
BRSA_12_22 <- BRSA_12_22 %>% add_column(Date = "01/12/2022", .before = "Index Individu")

# Récupérer les noms de colonnes de BRSA_06_21
col_names <- colnames(BRSA_06_21)

# Affecter les mêmes noms de colonnes aux autres objets
colnames(BRSA_01_21) <- col_names
colnames(BRSA_02_21) <- col_names
colnames(BRSA_03_21) <- col_names
colnames(BRSA_04_21) <- col_names
colnames(BRSA_05_21) <- col_names
colnames(BRSA_07_22) <- col_names
colnames(BRSA_08_22) <- col_names
colnames(BRSA_09_22) <- col_names
colnames(BRSA_10_22) <- col_names
colnames(BRSA_11_22) <- col_names
colnames(BRSA_12_22) <- col_names



BRSA_0621_0622 <- rbind(BRSA_01_21,BRSA_02_21,BRSA_03_21,BRSA_04_21,BRSA_05_21,BRSA_06_21,BRSA_07_21,BRSA_08_21,BRSA_09_21,BRSA_10_21,BRSA_11_21,BRSA_12_21,
                        BRSA_01_22,BRSA_02_22,BRSA_03_22,BRSA_04_22,BRSA_05_22,BRSA_06_22,BRSA_07_22,BRSA_08_22,BRSA_09_22,BRSA_10_22,BRSA_11_22,BRSA_12_22)

BRSA_0621_0622$`Index Individu` <- as.character(BRSA_0621_0622$`Index Individu`)

# Remplacer les NA par 0 dans la colonne 'OP  - Mtt Total Rsa'
BRSA_0621_0622$`OP  - Mtt Total Rsa` <- replace(BRSA_0621_0622$`OP  - Mtt Total Rsa`, is.na(BRSA_0621_0622$`OP  - Mtt Total Rsa`), 0)

#BRSA_0621_0622$`OP  - Mtt Total Rsa` <- as.numeric(BRSA_0621_0622$`OP  - Mtt Total Rsa`)

#Suppression des variables sans utilité dans notre etude
BRSA_0621_0622 <- BRSA_0621_0622[,-c(19,20,23)]

#Suppression des lignes qui contient que des valeurs manquantes
BRSA_0621_0622 <- na.omit(BRSA_0621_0622)

Liste_date <- c("01/01/2021","01/02/2021", "01/03/2021", "01/04/2021", "01/05/2021",
  "01/06/2021","01/07/2021","01/08/2021","01/09/2021","01/10/2021","01/11/2021","01/12/2021",
                "01/01/2022","01/02/2022","01/03/2022","01/04/2022","01/05/2022","01/06/2022",
  "01/07/2022", "01/08/2022", "01/09/2022", "01/10/2022", "01/11/2022", "01/12/2022")

#Création des variables "Age" et "Rennais"
BRSA_0621_0622 <- BRSA_0621_0622 %>% mutate(Age = as.numeric(difftime(Sys.Date(), as.POSIXct(BRSA_0621_0622$`Date Naissance Individu`, format = "%d/%m/%Y"), units = "days")) / 365.25)

secteur_rennes_metropole = c("RENNES VILLE","COURONNE RENNAISE SUD","COURONNE RENNAISE NORD OUEST","COURONNE RENNAISE EST")  #par secteur ou par commune

#secteur_hors_rennes_metropole = c("PAYS DE BROCELIANDE","SAINT AUBIN D'AUBIGNE","PAYS DE GUICHEN","VITRE COMMUNAUTE",
#                                  "SAINT MALO","LE SEMNON","PAYS DE COMBOURG","LA BAIE","PAYS MALOUIN","HORS DEPARTEMENT",
#                                  "PAYS DE FOUGERES","PAYS DE LA ROCHE AUX FEES","PAYS DE REDON","MARCHES DE BRETAGNE")  #par secteur ou par commune


BRSA_0621_0622 <- BRSA_0621_0622 %>% mutate(Rennais = ifelse(BRSA_0621_0622$Secteur %in% secteur_rennes_metropole,"Rennais","Non Rennais"))

#Nombre de sorties definitives:
#Pour une date x ,un individu est sorti definitivement si on le trouve plus sur les dates [x+1 , xfinal]

Nb_sorties_def <- function(){
  Nb_sorties_def = c(rep(0, length = length(Liste_date)))
  s=1
  while (s < length(Liste_date)){
    Date_actuel <- Liste_date[s]
    Date_suivant <- Liste_date[(s+1):length(Liste_date)]
    #Date_actuel <- list(A)
    Base_Date_actuel <- filter(BRSA_0621_0622,Date %in% Date_actuel)
    unique_individu_Date_actuel = unique(Base_Date_actuel$`Index Individu`) #je prend les individus pour le premier mois
    Base_Date_suivant <- filter(BRSA_0621_0622,Date %in% Date_suivant)
    unique_individu_Date_suivant = unique(Base_Date_suivant$`Index Individu`)
    i = 1 
    while (!is.na.data.frame(unique_individu_Date_actuel[i])){
      if(!(unique_individu_Date_actuel[i] %in% unique_individu_Date_suivant)){
        Nb_sorties_def[s] <- Nb_sorties_def[s] + 1
        }
      i=i+1
      }
    s=s+1
    }
  return(Nb_sorties_def)
  }

Nb_sorties_def <- Nb_sorties_def()
Nb_sorties_def_annee <- rbind(Liste_date,Nb_sorties_def)

dfNb_sorties_def_annee <- data.frame(Nb_sorties_def_annee)

#Liste des individus sortant definitivement(jusqu'au "01/06/2022" )
Liste_indiv_sort_def <- function(){
  Liste_indiv_sor_def_01_01_2021 <- list()
  Liste_indiv_sor_def_01_02_2021 <- list()
  Liste_indiv_sor_def_01_03_2021 <- list()
  Liste_indiv_sor_def_01_04_2021 <- list()
  Liste_indiv_sor_def_01_05_2021 <- list()
  Liste_indiv_sor_def_01_06_2021 <- list()
  Liste_indiv_sor_def_01_07_2021 <- list()
  Liste_indiv_sor_def_01_08_2021 <- list()
  Liste_indiv_sor_def_01_09_2021 <- list()
  Liste_indiv_sor_def_01_10_2021 <- list()
  Liste_indiv_sor_def_01_11_2021 <- list()
  Liste_indiv_sor_def_01_12_2021 <- list()
  Liste_indiv_sor_def_01_01_2022 <- list()
  Liste_indiv_sor_def_01_02_2022 <- list()
  Liste_indiv_sor_def_01_03_2022 <- list()
  Liste_indiv_sor_def_01_04_2022 <- list()
  Liste_indiv_sor_def_01_05_2022 <- list()
  Liste_indiv_sor_def_01_06_2022 <- list()
  Liste_indiv_sor_def_01_07_2022 <- list()
  Liste_indiv_sor_def_01_08_2022 <- list()
  Liste_indiv_sor_def_01_09_2022 <- list()
  Liste_indiv_sor_def_01_10_2022 <- list()
  Liste_indiv_sor_def_01_11_2022 <- list()
  Liste_indiv_sor_def_01_12_2022 <- list()
  Liste_indiv_sor_def_total <- list(Liste_indiv_sor_def_01_01_2021,
                                    Liste_indiv_sor_def_01_02_2021,
                                    Liste_indiv_sor_def_01_03_2021,
                                    Liste_indiv_sor_def_01_04_2021,
                                    Liste_indiv_sor_def_01_05_2021,
                                    Liste_indiv_sor_def_01_06_2021,
                                    Liste_indiv_sor_def_01_07_2021,
                                    Liste_indiv_sor_def_01_08_2021,
                                    Liste_indiv_sor_def_01_09_2021,
                                    Liste_indiv_sor_def_01_10_2021,
                                    Liste_indiv_sor_def_01_11_2021,
                                    Liste_indiv_sor_def_01_12_2021,
                                    Liste_indiv_sor_def_01_01_2022,
                                    Liste_indiv_sor_def_01_02_2022,
                                    Liste_indiv_sor_def_01_03_2022,
                                    Liste_indiv_sor_def_01_04_2022,
                                    Liste_indiv_sor_def_01_05_2022,
                                    Liste_indiv_sor_def_01_06_2022,
                                    Liste_indiv_sor_def_01_07_2022,
                                    Liste_indiv_sor_def_01_08_2022,
                                    Liste_indiv_sor_def_01_09_2022,
                                    Liste_indiv_sor_def_01_10_2022,
                                    Liste_indiv_sor_def_01_11_2022,
                                    Liste_indiv_sor_def_01_12_2022)
  s=1
  while (s < length(Liste_date)){
    Date_actuel <- Liste_date[s]
    Date_suivant <- Liste_date[(s+1):length(Liste_date)]
    #Date_actuel <- list(A)
    Base_Date_actuel <- filter(BRSA_0621_0622,Date %in% Date_actuel)
    unique_individu_Date_actuel = unique(Base_Date_actuel$`Index Individu`) #je prend les individus pour le premier mois
    Base_Date_suivant <- filter(BRSA_0621_0622,Date %in% Date_suivant)
    unique_individu_Date_suivant = unique(Base_Date_suivant$`Index Individu`)
    i = 1 
    while (!is.na.data.frame(unique_individu_Date_actuel[i])){
      if(!(unique_individu_Date_actuel[i] %in% unique_individu_Date_suivant)){
        Liste_indiv_sor_def_total[[s]][[i]] <- unique_individu_Date_actuel[[i]]
      }
      i=i+1
    }
    
    if(length(Liste_indiv_sor_def_total[[s]]) != 0){
    Liste_indiv_sor_def_total[[s]] <-  Liste_indiv_sor_def_total[[s]][-which(sapply(Liste_indiv_sor_def_total[[s]], is.null))]
    }
    
    s=s+1
  }
  return(Liste_indiv_sor_def_total)
}

Liste_indiv_sor_def_total <- Liste_indiv_sort_def()

#les bases d'individus sortants definitivement + trajectoire jusqu'a la sortie
#Base_indiv_sor_def <- function(M){
#  D <- Liste_indiv_sor_def_total[[M]]
#  d <- unlist(D)
#  A <- as.vector(d)
#  Base_indiv_sor_def <- filter(BRSA_0621_0622,(BRSA_0621_0622$`Index Individu`) %in% A)
#  return(Base_indiv_sor_def)
#}
#
#Base_indiv_sor_def_01_06_2021 <- Base_indiv_sor_def(1)
#Base_indiv_sor_def_01_07_2021 <- Base_indiv_sor_def(2)
#Base_indiv_sor_def_01_08_2021 <- Base_indiv_sor_def(3)
#Base_indiv_sor_def_01_09_2021 <- Base_indiv_sor_def(4)
#Base_indiv_sor_def_01_10_2021 <- Base_indiv_sor_def(5)
#Base_indiv_sor_def_01_11_2021 <- Base_indiv_sor_def(6)
#Base_indiv_sor_def_01_12_2021 <- Base_indiv_sor_def(7)
#Base_indiv_sor_def_01_01_2022 <- Base_indiv_sor_def(8)
#Base_indiv_sor_def_01_02_2022 <- Base_indiv_sor_def(9)
#Base_indiv_sor_def_01_03_2022 <- Base_indiv_sor_def(10)
#Base_indiv_sor_def_01_04_2022 <- Base_indiv_sor_def(11)
#Base_indiv_sor_def_01_05_2022 <- Base_indiv_sor_def(12)
#Base_indiv_sor_def_01_06_2022 <- Base_indiv_sor_def(13)

#les bases d'individus sortant definitivement
Base_indiv_sor_def <- function(M,date){
  D <- Liste_indiv_sor_def_total[[M]]
  d <- unlist(D)
  A <- as.vector(d)
  Base_indiv_sor_def <- filter(BRSA_0621_0622,(BRSA_0621_0622$`Index Individu`) %in% A & BRSA_0621_0622$Date == date)
  return(Base_indiv_sor_def)
}

Base_indiv_sor_def_01_01_2021 <- Base_indiv_sor_def(1,"01/01/2021")
Base_indiv_sor_def_01_02_2021 <- Base_indiv_sor_def(2,"01/02/2021")
Base_indiv_sor_def_01_03_2021 <- Base_indiv_sor_def(3,"01/03/2021")
Base_indiv_sor_def_01_04_2021 <- Base_indiv_sor_def(4,"01/04/2021")
Base_indiv_sor_def_01_05_2021 <- Base_indiv_sor_def(5,"01/05/2021")
Base_indiv_sor_def_01_06_2021 <- Base_indiv_sor_def(6,"01/06/2021")
Base_indiv_sor_def_01_07_2021 <- Base_indiv_sor_def(7,"01/07/2021")
Base_indiv_sor_def_01_08_2021 <- Base_indiv_sor_def(8,"01/08/2021")
Base_indiv_sor_def_01_09_2021 <- Base_indiv_sor_def(9,"01/09/2021")
Base_indiv_sor_def_01_10_2021 <- Base_indiv_sor_def(10,"01/10/2021")
Base_indiv_sor_def_01_11_2021 <- Base_indiv_sor_def(11,"01/11/2021")
Base_indiv_sor_def_01_12_2021 <- Base_indiv_sor_def(12,"01/12/2021")
Base_indiv_sor_def_01_01_2022 <- Base_indiv_sor_def(13,"01/01/2022")
Base_indiv_sor_def_01_02_2022 <- Base_indiv_sor_def(14,"01/02/2022")
Base_indiv_sor_def_01_03_2022 <- Base_indiv_sor_def(15,"01/03/2022")
Base_indiv_sor_def_01_04_2022 <- Base_indiv_sor_def(16,"01/04/2022")
Base_indiv_sor_def_01_05_2022 <- Base_indiv_sor_def(17,"01/05/2022")
Base_indiv_sor_def_01_06_2022 <- Base_indiv_sor_def(18,"01/06/2022")
Base_indiv_sor_def_01_07_2022 <- Base_indiv_sor_def(19,"01/07/2022")
Base_indiv_sor_def_01_08_2022 <- Base_indiv_sor_def(20,"01/08/2022")
Base_indiv_sor_def_01_09_2022 <- Base_indiv_sor_def(21,"01/09/2022")
Base_indiv_sor_def_01_10_2022 <- Base_indiv_sor_def(22,"01/10/2022")
Base_indiv_sor_def_01_11_2022 <- Base_indiv_sor_def(23,"01/11/2022")
Base_indiv_sor_def_01_12_2022 <- Base_indiv_sor_def(24,"01/12/2022")


#http://www.sthda.com/english/wiki/ggplot2-barplot-easy-bar-graphs-in-r-software-using-ggplot2
#Barplot sur les bénéficiers (demandeurs ou conjoint) contexte familiale
#base qui rassemble tout les bénificiers
Base_indiv_sor_def_rbind <- rbind(Base_indiv_sor_def_01_01_2021,
                                  Base_indiv_sor_def_01_02_2021,
                                  Base_indiv_sor_def_01_03_2021,
                                  Base_indiv_sor_def_01_04_2021,
                                  Base_indiv_sor_def_01_05_2021,
                                  Base_indiv_sor_def_01_06_2021,
                                  Base_indiv_sor_def_01_07_2021,
                                  Base_indiv_sor_def_01_08_2021,
                                  Base_indiv_sor_def_01_09_2021,
                                  Base_indiv_sor_def_01_10_2021,
                                  Base_indiv_sor_def_01_11_2021,
                                  Base_indiv_sor_def_01_12_2021,
                                  Base_indiv_sor_def_01_01_2022,
                                  Base_indiv_sor_def_01_02_2022,
                                  Base_indiv_sor_def_01_03_2022,
                                  Base_indiv_sor_def_01_04_2022,
                                  Base_indiv_sor_def_01_05_2022,
                                  Base_indiv_sor_def_01_06_2022,
                                  Base_indiv_sor_def_01_07_2022,
                                  Base_indiv_sor_def_01_08_2022,
                                  Base_indiv_sor_def_01_09_2022,
                                  Base_indiv_sor_def_01_10_2022,
                                  Base_indiv_sor_def_01_11_2022,
                                  Base_indiv_sor_def_01_12_2022)


#Base_indiv_sor_def_rbind$`Contexte Familial` <- as.factor(Base_indiv_sor_def_rbind$`Contexte Familial`)
#Base_indiv_sor_def_rbind$`OP - Role CAF` <- as.factor(Base_indiv_sor_def_rbind$`OP - Role CAF`)

#summary(Base_indiv_sor_def_rbind)

#barplot
#demandeur sortant en foction du context familiale
#Base_indiv_sor_def_rbind <- data.frame(Base_indiv_sor_def_rbind)
#Base_indiv_sor_def_rbind_dem <- Base_indiv_sor_def_rbind  %>% dplyr::select(Base_indiv_sor_def_rbind,`OP - Role CAF`,`Contexte Familial`)
#
#
#gplot2.barplot(data=Base_indiv_sor_def_rbind_dem, xName="time", yName='total_bill')
# 
##conjoint sortant  en foction du context familiale
#Base_indiv_sor_def_rbind_con <- filter(Base_indiv_sor_def_rbind,Base_indiv_sor_def_rbind$`OP - Role CAF` == "Conjoint") 

#Nombre d'entrees
#Pour une date x , un individu est entree si on ne le trouve pas sur x-1 et on le trouve sur x.

Nb_entree <- function(){
  Nb_entree = c(rep(0, length = length(Liste_date)))
  s=1
  while (!is.na.data.frame(Liste_date[s])){
    Mois_precedent <-  Liste_date[s]
    Mois_suivant <-  Liste_date[s+1]
    Base_Mois_precedent <- filter(BRSA_0621_0622,Date %in% Mois_precedent)
    unique_individu_Mois_precedent <- unique(Base_Mois_precedent$`Index Individu`)
    Base_Mois_suivant <- filter(BRSA_0621_0622,Date %in% Mois_suivant)
    unique_individu_Mois_suivant <- unique(Base_Mois_suivant$`Index Individu`)
    i=1
    while(!is.na.data.frame(unique_individu_Mois_suivant[i])){
      if(!(unique_individu_Mois_suivant[i]  %in% unique_individu_Mois_precedent)){
        Nb_entree[s] <- Nb_entree[s] + 1 
      }
      i=i+1
    }
    s=s+1
  }
  return(Nb_entree)
}

Nb_entree <- Nb_entree()
Nb_entree_annee <- rbind(Liste_date,Nb_entree)

dfNb_entree_annee <- data.frame(Nb_entree_annee)

#Liste des individus entrants pour chaque mois entre  "01/06/2021" et "01/06/2022"
Liste_indiv_ent <- function(){
  Liste_indiv_ent_01_01_2021 <- list()
  Liste_indiv_ent_01_02_2021 <- list()
  Liste_indiv_ent_01_03_2021 <- list()
  Liste_indiv_ent_01_04_2021 <- list()
  Liste_indiv_ent_01_05_2021 <- list()
  Liste_indiv_ent_01_06_2021 <- list()
  Liste_indiv_ent_01_07_2021 <- list()
  Liste_indiv_ent_01_08_2021 <- list()
  Liste_indiv_ent_01_09_2021 <- list()
  Liste_indiv_ent_01_10_2021 <- list()
  Liste_indiv_ent_01_11_2021 <- list()
  Liste_indiv_ent_01_12_2021 <- list()
  Liste_indiv_ent_01_01_2022 <- list()
  Liste_indiv_ent_01_02_2022 <- list()
  Liste_indiv_ent_01_03_2022 <- list()
  Liste_indiv_ent_01_04_2022 <- list()
  Liste_indiv_ent_01_05_2022 <- list()
  Liste_indiv_ent_01_06_2022 <- list()
  Liste_indiv_ent_01_07_2022 <- list()
  Liste_indiv_ent_01_08_2022 <- list()
  Liste_indiv_ent_01_09_2022 <- list()
  Liste_indiv_ent_01_10_2022 <- list()
  Liste_indiv_ent_01_11_2022 <- list()
  Liste_indiv_ent_01_12_2022 <- list()
  Liste_indiv_ent_total <- list(Liste_indiv_ent_01_01_2021,
                                Liste_indiv_ent_01_02_2021,
                                Liste_indiv_ent_01_03_2021,
                                Liste_indiv_ent_01_04_2021,
                                Liste_indiv_ent_01_05_2021,
                                Liste_indiv_ent_01_06_2021,
                                Liste_indiv_ent_01_07_2021,
                                Liste_indiv_ent_01_08_2021,
                                Liste_indiv_ent_01_09_2021,
                                Liste_indiv_ent_01_10_2021,
                                Liste_indiv_ent_01_11_2021,
                                Liste_indiv_ent_01_12_2021,
                                Liste_indiv_ent_01_01_2022,
                                Liste_indiv_ent_01_02_2022,
                                Liste_indiv_ent_01_03_2022,
                                Liste_indiv_ent_01_04_2022,
                                Liste_indiv_ent_01_05_2022,
                                Liste_indiv_ent_01_06_2022,
                                Liste_indiv_ent_01_07_2022,
                                Liste_indiv_ent_01_08_2022,
                                Liste_indiv_ent_01_09_2022,
                                Liste_indiv_ent_01_10_2022,
                                Liste_indiv_ent_01_11_2022,
                                Liste_indiv_ent_01_12_2022)
  s=1
  while (s < length(Liste_date)){
    Mois_precedent <- Liste_date[s]
    Mois_suivant <- Liste_date[s+1]
    #Mois_precedent <- list(A)
    Base_Mois_precedent <- filter(BRSA_0621_0622,Date %in% Mois_precedent)
    unique_individu_Mois_precedent = unique(Base_Mois_precedent$`Index Individu`) #je prend les individus pour le premier mois
    Base_Mois_suivant <- filter(BRSA_0621_0622,Date %in% Mois_suivant)
    unique_individu_Mois_suivant = unique(Base_Mois_suivant$`Index Individu`)
    i = 1 
    while (!is.na.data.frame(unique_individu_Mois_suivant[i])){
      if(!(unique_individu_Mois_suivant[i]  %in% unique_individu_Mois_precedent)){
        Liste_indiv_ent_total[[s]][[i]] <- unique_individu_Mois_suivant[[i]]
      }
      i=i+1
    }
    
    if(length(Liste_indiv_ent_total[[s]]) != 0){
      Liste_indiv_ent_total[[s]] <-  Liste_indiv_ent_total[[s]][-which(sapply(Liste_indiv_ent_total[[s]], is.null))]
    }
    
    s=s+1
  }
  return(Liste_indiv_ent_total)
}

Liste_indiv_ent_total <- Liste_indiv_ent()

#les bases d'individus entrants
Base_indiv_ent <- function(M,date){
  D <- Liste_indiv_ent_total[[M]]
  d <- unlist(D)
  A <- as.vector(d)
  Base_indiv_entr <- filter(BRSA_0621_0622,(BRSA_0621_0622$`Index Individu`) %in% A & BRSA_0621_0622$Date == date)
  return(Base_indiv_entr)
}

Base_indiv_ent_01_01_2021 <- Base_indiv_ent(1,"01/02/2021")
Base_indiv_ent_01_02_2021 <- Base_indiv_ent(2,"01/03/2021")
Base_indiv_ent_01_03_2021 <- Base_indiv_ent(3,"01/04/2021")
Base_indiv_ent_01_04_2021 <- Base_indiv_ent(4,"01/05/2021")
Base_indiv_ent_01_05_2021 <- Base_indiv_ent(5,"01/06/2021")
Base_indiv_ent_01_06_2021 <- Base_indiv_ent(6,"01/07/2021")
Base_indiv_ent_01_07_2021 <- Base_indiv_ent(7,"01/08/2021")
Base_indiv_ent_01_08_2021 <- Base_indiv_ent(8,"01/09/2021")
Base_indiv_ent_01_09_2021 <- Base_indiv_ent(9,"01/10/2021")
Base_indiv_ent_01_10_2021 <- Base_indiv_ent(10,"01/11/2021")
Base_indiv_ent_01_11_2021 <- Base_indiv_ent(11,"01/12/2021")
Base_indiv_ent_01_12_2021 <- Base_indiv_ent(12,"01/01/2022")
Base_indiv_ent_01_01_2022 <- Base_indiv_ent(1,"01/02/2022")
Base_indiv_ent_01_02_2022 <- Base_indiv_ent(2,"01/03/2022")
Base_indiv_ent_01_03_2022 <- Base_indiv_ent(3,"01/04/2022")
Base_indiv_ent_01_04_2022 <- Base_indiv_ent(4,"01/05/2022")
Base_indiv_ent_01_05_2022 <- Base_indiv_ent(5,"01/06/2022")
Base_indiv_ent_01_06_2022 <- Base_indiv_ent(6,"01/07/2022")   #faut trouver une une solution 13 est improviser 
Base_indiv_ent_01_07_2022 <- Base_indiv_ent(7,"01/08/2022")
Base_indiv_ent_01_08_2022 <- Base_indiv_ent(8,"01/09/2022")
Base_indiv_ent_01_09_2022 <- Base_indiv_ent(9,"01/10/2022")
Base_indiv_ent_01_10_2022 <- Base_indiv_ent(10,"01/11/2022")
Base_indiv_ent_01_11_2022 <- Base_indiv_ent(11,"01/12/2022")
Base_indiv_ent_01_12_2022 <- Base_indiv_ent(12,"01/13/2022") #!!!!! #faut trouver une une solution 13 est improviser
Base_indiv_ent_rbind <- rbind(Base_indiv_ent_01_01_2021,
                              Base_indiv_ent_01_02_2021,
                              Base_indiv_ent_01_03_2021,
                              Base_indiv_ent_01_04_2021,
                              Base_indiv_ent_01_05_2021,
                              Base_indiv_ent_01_06_2021,
                              Base_indiv_ent_01_07_2021,
                              Base_indiv_ent_01_08_2021,
                              Base_indiv_ent_01_09_2021,
                              Base_indiv_ent_01_10_2021,
                              Base_indiv_ent_01_11_2021,
                              Base_indiv_ent_01_12_2021,
                              Base_indiv_ent_01_01_2022,
                              Base_indiv_ent_01_02_2022,
                              Base_indiv_ent_01_03_2022,
                              Base_indiv_ent_01_04_2022,
                              Base_indiv_ent_01_05_2022,
                              Base_indiv_ent_01_06_2022,
                              Base_indiv_ent_01_07_2022,
                              Base_indiv_ent_01_08_2022,
                              Base_indiv_ent_01_09_2022,
                              Base_indiv_ent_01_10_2022,
                              Base_indiv_ent_01_11_2022,
                              Base_indiv_ent_01_12_2022)


#Nombre de sorties temporaire

#Pour une date X entre Juin 2021 et Juin 2022.
# On considéré un individu sortant temporairement si on le trouve pas sur l’intervalle (X+1 , X+j) ,
# puis on le trouve sur l’intervalle  (X+j ,Juin 2022).  (tels que j dans (1,12)).

Nb_sorties_temp <- function(){
  Nb_sorties_temp = c(rep(0, length = length(Liste_date)))
  s=1
  while (s < length(Liste_date) ){
    Date1 <- Liste_date[s]
    Date2 <- Liste_date[(s+1)]
    Date3 <- Liste_date[(s+2):length(Liste_date)]
    Base_Date1 <- filter(BRSA_0621_0622,Date %in% Date1)
    unique_individu_Date1 = unique(Base_Date1$`Index Individu`) 
    Base_Date2 <- filter(BRSA_0621_0622,Date %in% Date2)
    unique_individu_Date2 = unique(Base_Date2$`Index Individu`)
    Base_Date3 <- filter(BRSA_0621_0622,Date %in% Date3)
    unique_individu_Date3 = unique(Base_Date3$`Index Individu`)
    i = 1 
    while (!is.na.data.frame(unique_individu_Date1[i])){
      if(!(unique_individu_Date1[i] %in% unique_individu_Date2) & (unique_individu_Date1[i] %in% unique_individu_Date3)){
        Nb_sorties_temp[s] <- Nb_sorties_temp[s] + 1
      }
      i=i+1
    }
    s=s+1
  }
  return(Nb_sorties_temp)
}

Nb_sorties_temp <- Nb_sorties_temp()
Nb_sorties_temp_annee <- rbind(Liste_date,Nb_sorties_temp)

dfNb_sorties_temp_annee <- data.frame(Nb_sorties_temp_annee)

#Liste des individus sortants temprairements (jusqu'au "01/06/2022" )
Liste_indiv_sort_temp <- function(){
  Liste_indiv_sor_temp_01_01_2021 <- list()
  Liste_indiv_sor_temp_01_02_2021 <- list()
  Liste_indiv_sor_temp_01_03_2021 <- list()
  Liste_indiv_sor_temp_01_04_2021 <- list()
  Liste_indiv_sor_temp_01_05_2021 <- list()
  Liste_indiv_sor_temp_01_06_2021 <- list()
  Liste_indiv_sor_temp_01_07_2021 <- list()
  Liste_indiv_sor_temp_01_08_2021 <- list()
  Liste_indiv_sor_temp_01_09_2021 <- list()
  Liste_indiv_sor_temp_01_10_2021 <- list()
  Liste_indiv_sor_temp_01_11_2021 <- list()
  Liste_indiv_sor_temp_01_12_2021 <- list()
  Liste_indiv_sor_temp_01_01_2022 <- list()
  Liste_indiv_sor_temp_01_02_2022 <- list()
  Liste_indiv_sor_temp_01_03_2022 <- list()
  Liste_indiv_sor_temp_01_04_2022 <- list()
  Liste_indiv_sor_temp_01_05_2022 <- list()
  Liste_indiv_sor_temp_01_06_2022 <- list()
  Liste_indiv_sor_temp_01_07_2022 <- list()
  Liste_indiv_sor_temp_01_08_2022 <- list()
  Liste_indiv_sor_temp_01_09_2022 <- list()
  Liste_indiv_sor_temp_01_10_2022 <- list()
  Liste_indiv_sor_temp_01_11_2022 <- list()
  Liste_indiv_sor_temp_01_12_2022 <- list()
  Liste_indiv_sor_temp_total <- list(Liste_indiv_sor_temp_01_01_2021,
                                     Liste_indiv_sor_temp_01_02_2021,
                                     Liste_indiv_sor_temp_01_03_2021,
                                     Liste_indiv_sor_temp_01_04_2021,
                                     Liste_indiv_sor_temp_01_05_2021,
                                     Liste_indiv_sor_temp_01_06_2021,
                                     Liste_indiv_sor_temp_01_07_2021,
                                     Liste_indiv_sor_temp_01_08_2021,
                                     Liste_indiv_sor_temp_01_09_2021,
                                     Liste_indiv_sor_temp_01_10_2021,
                                     Liste_indiv_sor_temp_01_11_2021,
                                     Liste_indiv_sor_temp_01_12_2021,
                                     Liste_indiv_sor_temp_01_01_2022,
                                     Liste_indiv_sor_temp_01_02_2022,
                                     Liste_indiv_sor_temp_01_03_2022,
                                     Liste_indiv_sor_temp_01_04_2022,
                                     Liste_indiv_sor_temp_01_05_2022,
                                     Liste_indiv_sor_temp_01_06_2022,
                                     Liste_indiv_sor_temp_01_07_2022,
                                     Liste_indiv_sor_temp_01_08_2022,
                                     Liste_indiv_sor_temp_01_09_2022,
                                     Liste_indiv_sor_temp_01_10_2022,
                                     Liste_indiv_sor_temp_01_11_2022,
                                     Liste_indiv_sor_temp_01_12_2022)
  s=1
  while (s < length(Liste_date)){
    Date1 <- Liste_date[s]
    Date2 <- Liste_date[(s+1)]
    Date3 <- Liste_date[(s+2):length(Liste_date)]
    Base_Date1 <- filter(BRSA_0621_0622,Date %in% Date1)
    unique_individu_Date1 = unique(Base_Date1$`Index Individu`) 
    Base_Date2 <- filter(BRSA_0621_0622,Date %in% Date2)
    unique_individu_Date2 = unique(Base_Date2$`Index Individu`)
    Base_Date3 <- filter(BRSA_0621_0622,Date %in% Date3)
    unique_individu_Date3 = unique(Base_Date3$`Index Individu`)
    i = 1 
    while (!is.na.data.frame(unique_individu_Date1[i])){
      if(!(unique_individu_Date1[i] %in% unique_individu_Date2) & (unique_individu_Date1[i] %in% unique_individu_Date3)){
        Liste_indiv_sor_temp_total[[s]][[i]] <- unique_individu_Date1[[i]]
      }
      i=i+1
    }
    
    if(length(Liste_indiv_sor_temp_total[[s]]) != 0){
      Liste_indiv_sor_temp_total[[s]] <-  Liste_indiv_sor_temp_total[[s]][-which(sapply(Liste_indiv_sor_temp_total[[s]], is.null))]
    }
    
    s=s+1
  }
  return(Liste_indiv_sor_temp_total)
}

Liste_indiv_sor_temp_total <- Liste_indiv_sort_temp()

#les bases d'individus sortant temporairement
Base_indiv_sor_temp <- function(M,date){
  D <- Liste_indiv_sor_temp_total[[M]]
  d <- unlist(D)
  A <- as.vector(d)
  Base_indiv_sor_temp <- filter(BRSA_0621_0622,(BRSA_0621_0622$`Index Individu`) %in% A & BRSA_0621_0622$Date == date)
  return(Base_indiv_sor_temp)
}

Base_indiv_sor_temp_01_01_2021 <- Base_indiv_sor_temp(1,"01/01/2021")
Base_indiv_sor_temp_01_02_2021 <- Base_indiv_sor_temp(2,"01/02/2021")
Base_indiv_sor_temp_01_03_2021 <- Base_indiv_sor_temp(3,"01/03/2021")
Base_indiv_sor_temp_01_04_2021 <- Base_indiv_sor_temp(4,"01/04/2021")
Base_indiv_sor_temp_01_05_2021 <- Base_indiv_sor_temp(5,"01/05/2021")
Base_indiv_sor_temp_01_06_2021 <- Base_indiv_sor_temp(6,"01/06/2021")
Base_indiv_sor_temp_01_07_2021 <- Base_indiv_sor_temp(7,"01/07/2021")
Base_indiv_sor_temp_01_08_2021 <- Base_indiv_sor_temp(8,"01/08/2021")
Base_indiv_sor_temp_01_09_2021 <- Base_indiv_sor_temp(9,"01/09/2021")
Base_indiv_sor_temp_01_10_2021 <- Base_indiv_sor_temp(10,"01/10/2021")
Base_indiv_sor_temp_01_11_2021 <- Base_indiv_sor_temp(11,"01/11/2021")
Base_indiv_sor_temp_01_12_2021 <- Base_indiv_sor_temp(12,"01/12/2021")
Base_indiv_sor_temp_01_01_2022 <- Base_indiv_sor_temp(13,"01/01/2022")
Base_indiv_sor_temp_01_02_2022 <- Base_indiv_sor_temp(14,"01/02/2022")
Base_indiv_sor_temp_01_03_2022 <- Base_indiv_sor_temp(15,"01/03/2022")
Base_indiv_sor_temp_01_04_2022 <- Base_indiv_sor_temp(16,"01/04/2022")
Base_indiv_sor_temp_01_05_2022 <- Base_indiv_sor_temp(17,"01/05/2022")
Base_indiv_sor_temp_01_06_2022 <- Base_indiv_sor_temp(18,"01/06/2022")
Base_indiv_sor_temp_01_07_2022 <- Base_indiv_sor_temp(19,"01/07/2022")
Base_indiv_sor_temp_01_08_2022 <- Base_indiv_sor_temp(20,"01/08/2022")
Base_indiv_sor_temp_01_09_2022 <- Base_indiv_sor_temp(21,"01/09/2022")
Base_indiv_sor_temp_01_10_2022 <- Base_indiv_sor_temp(22,"01/10/2022")
Base_indiv_sor_temp_01_11_2022 <- Base_indiv_sor_temp(23,"01/11/2022")
Base_indiv_sor_temp_01_12_2022 <- Base_indiv_sor_temp(24,"01/12/2022")
Base_indiv_sor_temp_rbind <- rbind(Base_indiv_sor_temp_01_01_2021,
                                   Base_indiv_sor_temp_01_02_2021,
                                   Base_indiv_sor_temp_01_03_2021,
                                   Base_indiv_sor_temp_01_04_2021,
                                   Base_indiv_sor_temp_01_05_2021,
                                   Base_indiv_sor_temp_01_06_2021,
                                   Base_indiv_sor_temp_01_07_2021,
                                   Base_indiv_sor_temp_01_08_2021,
                                   Base_indiv_sor_temp_01_09_2021,
                                   Base_indiv_sor_temp_01_10_2021,
                                   Base_indiv_sor_temp_01_11_2021,
                                   Base_indiv_sor_temp_01_12_2021,
                                   Base_indiv_sor_temp_01_01_2022,
                                   Base_indiv_sor_temp_01_02_2022,
                                   Base_indiv_sor_temp_01_03_2022,
                                   Base_indiv_sor_temp_01_04_2022,
                                   Base_indiv_sor_temp_01_05_2022,
                                   Base_indiv_sor_temp_01_06_2022,
                                   Base_indiv_sor_temp_01_07_2022,
                                   Base_indiv_sor_temp_01_08_2022,
                                   Base_indiv_sor_temp_01_09_2022,
                                   Base_indiv_sor_temp_01_10_2022,
                                   Base_indiv_sor_temp_01_11_2022,
                                   Base_indiv_sor_temp_01_12_2022)

##rbind
sortie_temp_def_entree <- rbind(Nb_sorties_def_annee, Nb_sorties_temp, Nb_entree)
sortie_temp_def_entree <- data.frame(sortie_temp_def_entree)

# Transposer le dataframe sortie_temp_def_entree
sortie_temp_def_entree <- t(sortie_temp_def_entree)
sortie_temp_def_entree <- data.frame(sortie_temp_def_entree)

# Convertir la colonne "Liste_date" en format de date
sortie_temp_def_entree$Liste_date <- as.Date(sortie_temp_def_entree$Liste_date, format = "%d/%m/%Y")

# Convertir les colonnes en type numérique
sortie_temp_def_entree$Nb_sorties_def <- as.numeric(sortie_temp_def_entree$Nb_sorties_def)
sortie_temp_def_entree$Nb_sorties_temp <- as.numeric(sortie_temp_def_entree$Nb_sorties_temp)
sortie_temp_def_entree$Nb_entree <- as.numeric(sortie_temp_def_entree$Nb_entree)

# Créer la nouvelle colonne "sortie" contenant la somme des sorties temporaires et des sorties définitives
sortie_temp_def_entree$sortie <- rowSums(select(sortie_temp_def_entree, Nb_sorties_def, Nb_sorties_temp), na.rm = TRUE)

# Tracer les courbes
ggplot(sortie_temp_def_entree) +
  geom_line(aes(x = Liste_date, y = Nb_entree, color = "Nombre d'entrées", group = 1), size = 1) +
  geom_line(aes(x = Liste_date, y = Nb_sorties_def, color = "Nombre de sorties définitives", group = 1), size = 1) +
  geom_line(aes(x = Liste_date, y = Nb_sorties_temp, color = "Nombre de sorties temporaires", group = 1), size = 1) +
  geom_line(aes(x = Liste_date, y = sortie, color = "Sortie (Temp. + Def.)", group = 1), size = 1) +
  geom_text(aes(x = Liste_date, y = Nb_entree, label = Nb_entree, color = "Nombre d'entrées"), vjust = -0.5, size = 4) +
  geom_text(aes(x = Liste_date, y = Nb_sorties_def, label = Nb_sorties_def, color = "Nombre de sorties définitives"), vjust = 1, size = 4) +
  geom_text(aes(x = Liste_date, y = Nb_sorties_temp, label = Nb_sorties_temp, color = "Nombre de sorties temporaires"), vjust = 1, size = 4) +
  geom_text(aes(x = Liste_date, y = sortie, label = sortie, color = "Sortie (Temp. + Def.)"), vjust = 1, size = 4) +
  labs(x = "Date", y = "Nombre", color = "Variables", title = "Graphique 3.2") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 10),
        panel.grid = element_blank()) +
  scale_color_manual(values = c("Nombre d'entrées" = "red", "Nombre de sorties définitives" = "darkgreen", "Nombre de sorties temporaires" = "green", "Sortie (Temp. + Def.)" = "blue")) +
  scale_linetype_manual(values = c("Nombre d'entrées" = "solid", "Nombre de sorties définitives" = "solid", "Nombre de sorties temporaires" = "solid", "Sortie (Temp. + Def.)" = "dashed")) +
  scale_y_continuous(breaks = seq(0, max(sortie_temp_def_entree$Nb_entree, sortie_temp_def_entree$Nb_sorties_def, sortie_temp_def_entree$Nb_sorties_temp, sortie_temp_def_entree$sortie), 5000), expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 month") +
  xlim(as.Date("2021-06-01"), as.Date("2022-06-01"))

#ggsave("C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/RAPPORT_Alternance/Capture/graphique_ 3.2.png", width = 8, height = 10, dpi = 300)

# Tracer les courbes ,stock et verification
# importer les données du stock
path <- "C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/Stats_Etude_Actualisation_BRSA_Juin21_22"
stock <- read_excel(file.path(path, "Evolution_Effectif_Droit_et_devoir.xls"))
stock$BRS_DATE_REFERENCE_FLUX <- as.Date(stock$BRS_DATE_REFERENCE_FLUX) 

# Perform the join using merge()
merged_data <- merge(sortie_temp_def_entree, stock, by.x = "Liste_date", by.y = "BRS_DATE_REFERENCE_FLUX", all = TRUE)

#Créer la variable stock_plus_entrees_moins_sorties
merged_data$stock_plus_entrees_moins_sorties <- lag(merged_data$`Nbre individus`) + merged_data$Nb_entree - merged_data$sortie

ggplot(merged_data) +
  geom_line(aes(x = Liste_date, y = `Nbre individus`, color = "Stock", group = 1), size = 1) +
  geom_line(aes(x = Liste_date, y = stock_plus_entrees_moins_sorties, color = "Lag Stock + entree - sortie", group = 1), size = 1, linetype = "dashed") +
  geom_text(aes(x = Liste_date, y = `Nbre individus`, label = `Nbre individus`, color = "Stock"), vjust = -0.5, size = 4) +
  geom_text(aes(x = Liste_date, y = stock_plus_entrees_moins_sorties, label = stock_plus_entrees_moins_sorties, color = "Lag Stock + entree - sortie"), vjust = 1, size = 4) +
  labs(x = "Date", y = "Nombre", color = "Variables", title = "Graphique 3.3") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("Stock" = "blue", "Lag Stock + entree - sortie" = "darkgreen")) +
  scale_linetype_manual(values = c("Stock" = "solid", "Lag Stock + entree - sortie" = "dashed")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 month") +
  xlim(as.Date("2021-06-01"), as.Date("2022-06-01"))

#ggsave("C:/Users/Hannaoui Youness/OneDrive/Bureau/CD35/RAPPORT_Alternance/Capture/graphique_ 3.3.png", width = 8, height = 10, dpi = 300)

