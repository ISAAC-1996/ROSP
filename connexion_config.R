################# Connexion #############
#Library
library(mailR)
library(jsonlite)
library(DBI)
library(odbc)
library(plotly)
library(tidyverse)
library(ggplot2)
library(mailR)
library(openxlsx)
library(gganimate)
library(ggforce)
library(lubridate)
library(taskscheduleR)
library(writexl)
library(httr)
library(purrr)
library(magrittr)
library(rmarkdown)
library(pagedown)
library(jsonlite)
library(base64enc)
library(readxl)
library(dplyr)
library(tidyverse)

#CONNEXION A LA BASE DE DONNEE SQL Server
con <- DBI::dbConnect(odbc::odbc(), Database = "OSP_DATASTAT", 
                      Driver = Sys.getenv("sql_driver"), Server = Sys.getenv("sql_server"), 
                      UID = Sys.getenv("sql_uid"), PWD = Sys.getenv("sql_pwd"), 
                      Port = Sys.getenv("sql_port"))


              ############ Date R ##########
mois_precedent <- function(format = "texte") {
  date_courante <- Sys.Date()
  date_lt <- as.POSIXlt(date_courante)
  date_lt$mon <- date_lt$mon - 1
  date_mois_prec <- as.Date(date_lt)
  
  mois_fr <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin",
               "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
  
  annee_prec <- format(date_mois_prec, "%Y")
  mois_prec <- format(date_mois_prec, "%m")  # Format numérique 01-12
  
  if (format == "texte") {
    return(paste0(mois_fr[as.integer(mois_prec)], " ", annee_prec))  # Exemple: "Janvier 2025"
  } else if (format == "sql") {
    return(paste0(annee_prec, mois_prec))  # Exemple: "202501"
  } else {
    stop("Format non reconnu. Utilisez 'texte' ou 'sql'.")
  }
}
mois_precedent()
        ############ Fonction Nom et adresse des pharmacies ################
getPharmacieInfo <- function(n_auto_adhpha) {
  query <- "
    SELECT a.n_auto_adhpha,
           a.rs_adhpha,
           a.cp_ville,
           a.nom_ville
    FROM vuProdAdhpha a
    LEFT JOIN os_ville v 
      ON v.n_auto_ville = a.ville_adhpha
    WHERE a.n_auto_adhpha = ?
  "
  df <- dbGetQuery(con, query, params = list(n_auto_adhpha))
  if (nrow(df) == 0) {
    warning("Aucune pharmacie trouvée pour n_auto_adhpha = ", n_auto_adhpha)
    return(NA_character_)
  }
  rs_adhpha_clean <- trimws(df$rs_adhpha)
  cp_clean        <- trimws(df$cp_ville)
  ville_clean     <- trimws(df$nom_ville)
  result <- paste0(
    "Pharmacie ", rs_adhpha_clean,
    " - ", cp_clean, "  ", ville_clean
  )
  return(result)
}

########## Top 10 fuites géneriques ############
top_10_gener <- function(n_auto_adhpha_artic, con) {
  date_previous <- mois_precedent("sql")  
  periode <- date_previous  
  
  query <- sprintf("
select top 10 rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as Présentation, sum(qt_vendu_artic) as Quantité, sum(qt_vendu_artic*pfht) as PFHT
from (select lien as liensandoz, max(remise) as remsandoz
from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien  
left join os_adhfour ad on ad.n_auto_adhfour=os_labogener_artic.n_auto_adhfour where n_auto_adhpha_artic=%d
         and ((an_artic*100)+mois_artic) = %s
         and os_labogener_artic.n_auto_adhfour not in (401884) AND groupe_gener IN (select groupe_gener from os_gener inner join os_artic on n_auto_artic=n_auto_artic_gener where type_artic='G' and arret_artic is null) 
		 group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour, remise order by PFHT desc",
                   n_auto_adhpha_artic,
                   periode)
  result <- dbGetQuery(con, query)
  return(result)
}

########## Top 5 princeps non substitués ############

top_5 <- function(n_auto_adhpha_artic, con) {
  date_previous <- mois_precedent("sql")  
  periode <- date_previous  
  query <- sprintf("
 select top 5 rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as Présentation, nom_adhfour as Laboratoire, sum(qt_vendu_artic) as Quantité, sum(qt_vendu_artic*pfht_groupe) as PFHT
 from (select lien as liensandoz, max(remise) as remsandoz from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
 inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien inner join os_artic on os_artic.n_auto_artic=n_auto_artic_artic inner join os_adhfour on os_adhfour.n_auto_adhfour=os_artic.adhfour 
 where n_auto_adhpha_artic= %d
         and ((an_artic*100)+mois_artic) = %s
         and os_labogener_artic.type_artic='P' group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour
		 order by PFHT desc",
                   n_auto_adhpha_artic,
                   periode)
  result <- dbGetQuery(con, query)
  return(result)
}

##################### Lancement sur les 12 derniers mois #####################
get_period_range <- function() {
  end_period <- mois_precedent("sql")
  # Convertir en date pour calculer le début de période
  end_year <- as.integer(substr(end_period, 1, 4))
  end_month <- as.integer(substr(end_period, 5, 6))
  # Calculer l'année et le mois de début (12 mois avant)
  start_year <- end_year - 1
  start_month <- end_month + 1
  # Ajuster si le mois de début dépasse 12
  if (start_month > 12) {
    start_month <- 1
    start_year <- start_year + 1
  }
  # Formater le mois de début au format YYYYMM
  start_period <- sprintf("%d%02d", start_year, start_month)
  return(list(
    start = start_period,
    end = end_period
  ))}
get_period_range()

# Fonction principale pour calculer les lancements
calculer_lancements <- function(n_auto_adhpha_artic, con) {
  # Obtenir la période des 12 derniers mois
  period <- get_period_range()
  query <- sprintf("
    SELECT top 10
      dci as Lancements, 
    COALESCE(SANDOZ * 10000 / NULLIF(TOTAL_GX, 0) * 0.01, 0) AS 'PDM Gx Sandoz',
    COALESCE(SANDOZ, 0) AS Sandoz,
    COALESCE(CONCURRENT, 0) AS 'Autres génériqueurs',
    COALESCE(PRINCEPS, 0) AS Princeps
    FROM (
      SELECT DISTINCT lib_dci as dci 
      FROM os_dci 
      WHERE groupe_dci IN (
        SELECT groupe_gener 
        FROM os_gener 
        WHERE n_auto_artic_gener IN (
          SELECT n_auto_artic_ean 
          FROM os_ean 
          WHERE code_ean IN (SELECT ean FROM sandoz_lancements)
        )
      )
    ) t1 
    LEFT JOIN (
      SELECT 
        lib_dci,
        SUM(CASE WHEN adhfour=401884 THEN qt_vendu_artic ELSE 0 END) as SANDOZ,
        SUM(CASE WHEN adhfour NOT IN (401884) AND type_artic IN ('G','A') THEN qt_vendu_artic ELSE 0 END) as CONCURRENT,
        NULLIF(SUM(CASE WHEN type_artic IN ('G', 'A') THEN qt_vendu_artic ELSE 0 END), 0) as TOTAL_GX,
        SUM(CASE WHEN type_artic='P' THEN qt_vendu_artic ELSE 0 END) as PRINCEPS
      FROM os_stat_artic
      INNER JOIN os_gener ON n_auto_artic_artic=n_auto_artic_gener
      INNER JOIN os_dci ON groupe_gener=groupe_dci
      INNER JOIN os_artic ON n_auto_artic_artic=n_auto_artic
      WHERE n_auto_adhpha_artic=%d
      AND n_auto_artic_artic IN (
        SELECT n_auto_artic_ean 
        FROM os_ean 
        WHERE code_ean IN (SELECT ean FROM sandoz_lancements)
      )
      AND periode BETWEEN %s AND %s
      GROUP BY lib_dci
    ) t2 ON dci=t2.lib_dci
    ORDER BY 'PDM Gx Sandoz' DESC",
                   n_auto_adhpha_artic,
                   period$start,
                   period$end
  )
  result <- dbGetQuery(con, query)
  return(result)
}

########## Sommes des switch ###############
# Fonction pour calculer la somme des switchs
Somme_switch <- function(n_auto_adhpha_artic, con) {
  # Obtenir le mois précédent au format YYYYMM
  date_previous <- mois_precedent("sql")
  
  # Construction de la requête SQL
  query <- sprintf("
    SELECT COALESCE(SUM(sellout), 0) as total_switch
    FROM (
      SELECT SUM(qt_vendu_artic*pfht) as sellout
      FROM (
        SELECT lien as liensandoz, MAX(remise) as remsandoz 
        FROM os_labogener_artic 
        WHERE n_auto_adhfour=401884 
        AND date_fin_application > GETDATE() 
        GROUP BY lien
      ) t1 
      INNER JOIN os_labogener_artic ON liensandoz = lien 
      INNER JOIN os_stat_artic ON n_auto_artic = n_auto_artic_artic 
      INNER JOIN os_grpgener_artic ON os_labogener_artic.lien = os_grpgener_artic.lien  
      LEFT JOIN os_adhfour ad ON ad.n_auto_adhfour = os_labogener_artic.n_auto_adhfour 
      WHERE n_auto_adhpha_artic = %d
      AND ((an_artic*100) + mois_artic) = %s
      AND os_labogener_artic.n_auto_adhfour NOT IN (401884) 
      AND groupe_gener IN (
        SELECT groupe_gener 
        FROM os_gener 
        INNER JOIN os_artic ON n_auto_artic = n_auto_artic_gener 
        WHERE type_artic = 'G' 
        AND arret_artic IS NULL
      )
      GROUP BY liensandoz, 
               RTRIM(CONCAT(RTRIM(libelle_court_groupe), ' ', 
                           RTRIM(conditionnement), ' ',
                           RTRIM(volume), ' ',
                           RTRIM(unite))), 
               remsandoz, 
               nom_adhfour, 
               remise
    ) subquery",
                   n_auto_adhpha_artic,
                   date_previous
  )
  
  # Exécution de la requête
  result <- dbGetQuery(con, query)
  
  # Ajout d'attributs pour la traçabilité
  attr(result, "periode") <- mois_precedent("texte")
  attr(result, "pharmacie_id") <- n_auto_adhpha_artic
  
  return(result$total_switch)
}
#somme <- Somme_switch(12885, con)
#somme

############## Sommes des princeps #################

# Fonction pour calculer la somme des princeps
Somme_princeps <- function(n_auto_adhpha_artic, con) {
  date_previous <- mois_precedent("sql")
  query <- sprintf("
    SELECT COALESCE(SUM(sellout), 0) as total_princeps
    FROM (
      SELECT 
        liensandoz,
        RTRIM(CONCAT(RTRIM(libelle_court_groupe),' ',
                    RTRIM(conditionnement),' ',
                    RTRIM(volume),' ',
                    RTRIM(unite))) as name,
        SUM(qt_vendu_artic * pfht_groupe) as sellout,
        SUM(qt_vendu_artic) as quantity,
        remsandoz,
        nom_adhfour as manufacturer
      FROM (
        SELECT 
          lien as liensandoz,
          MAX(remise) as remsandoz 
        FROM os_labogener_artic 
        WHERE n_auto_adhfour = 401884 
        AND date_fin_application > GETDATE() 
        GROUP BY lien
      ) t1 
      INNER JOIN os_labogener_artic ON liensandoz = lien 
      INNER JOIN os_stat_artic ON n_auto_artic = n_auto_artic_artic 
      INNER JOIN os_grpgener_artic ON os_labogener_artic.lien = os_grpgener_artic.lien 
      INNER JOIN os_artic ON os_artic.n_auto_artic = n_auto_artic_artic 
      INNER JOIN os_adhfour ON os_adhfour.n_auto_adhfour = os_artic.adhfour 
      WHERE n_auto_adhpha_artic = %d
      AND ((an_artic*100) + mois_artic) = %s
      AND os_labogener_artic.type_artic = 'P'
      GROUP BY 
        liensandoz,
        RTRIM(CONCAT(RTRIM(libelle_court_groupe),' ',
                    RTRIM(conditionnement),' ',
                    RTRIM(volume),' ',
                    RTRIM(unite))),
        remsandoz,
        nom_adhfour
    ) subquery",
                   n_auto_adhpha_artic,
                   date_previous
  )
  
  result <- dbGetQuery(con, query)
  attr(result, "periode") <- mois_precedent("texte")
  attr(result, "pharmacie_id") <- n_auto_adhpha_artic
  
  return(result$total_princeps)
}

# Exemple d'utilisation :
#somme_p <- Somme_princeps(12885, con)
#somme_p






################### Biosimillaire #####################
Bisomilaire <- function(n_auto_adhpha_ecollect, con) {
  date_previous <- mois_precedent("sql")  # Génération automatique de la période
  periode <- date_previous  # Assigne la période pour éviter l'erreur
  
  query <- sprintf("WITH ProduitsMolecules AS (
    SELECT 'ZARZIO (FILGRASTIM)' AS PRESENTATION, 'SANDOZ' AS LABO, 'FILGRASTIM' AS BASE_MOLECULE
    UNION ALL
    SELECT 'FILGRASTIM', 'AUTRES', 'FILGRASTIM'
    UNION ALL
    SELECT 'ZIEXTENZO (PEGFILGRASTIM)', 'SANDOZ', 'PEGFILGRASTIM'
    UNION ALL
    SELECT 'PEGFILGRASTIM', 'AUTRES', 'PEGFILGRASTIM'
    UNION ALL
    SELECT 'EPOETINE', 'SANDOZ', 'EPOETINE'
    UNION ALL
    SELECT 'EPOETINE', 'AUTRES', 'EPOETINE'
    UNION ALL
    SELECT 'ETANERCEPT', 'SANDOZ', 'ETANERCEPT'
    UNION ALL
    SELECT 'ETANERCEPT', 'AUTRES', 'ETANERCEPT'
    UNION ALL
    SELECT 'ADALIMUMAB', 'SANDOZ', 'ADALIMUMAB'
    UNION ALL
    SELECT 'ADALIMUMAB', 'AUTRES', 'ADALIMUMAB'
    UNION ALL
    SELECT 'ENOXAPARINE', 'SANDOZ', 'ENOXAPARINE'
    UNION ALL
    SELECT 'ENOXAPARINE', 'AUTRES', 'ENOXAPARINE'
),
Ventes AS (
    SELECT CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%' 
                AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZARZIO (FILGRASTIM)' ELSE 'FILGRASTIM' END
           WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZIEXTENZO (PEGFILGRASTIM)' ELSE 'PEGFILGRASTIM' END 
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%EPOETINE%' 
                THEN 'EPOETINE'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ETANERCEPT%' 
                THEN 'ETANERCEPT'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ADALIMUMAB%' 
                THEN 'ADALIMUMAB'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ENOXAPARINE%' 
                THEN 'ENOXAPARINE' END AS PRESENTATION,
        CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END AS LABO,
        SUM(qt_vendu_lcollect) AS QTE,
        SUM(qt_vendu_lcollect * P_Fab_HT) AS PFHT
    FROM v_el_collect_ospharm
    INNER JOIN os_gener ON n_auto_artic_lcollect=n_auto_artic_gener
    INNER JOIN os_dci ON groupe_gener=groupe_dci
    INNER JOIN os_artic ON n_auto_artic_lcollect=n_auto_artic
    INNER JOIN os_adhfour ON adhfour=n_auto_adhfour
    LEFT JOIN OSP_PROD.dbo.CEPS_Prix ON n_auto_artic_lcollect = id_sql AND actif = 1
    WHERE n_auto_adhpha_ecollect=%d
    AND type_artic IN ('R','B')
    AND groupe_dci IN (
        SELECT groupe_gener 
        FROM os_grpgener_artic 
        WHERE libelle_court_groupe LIKE 'filgrastim%' OR libelle_court_groupe LIKE 'pegfilgrastim%' OR libelle_court_groupe LIKE 'epoetine%'OR libelle_court_groupe LIKE 'etanercept%'
		OR libelle_court_groupe LIKE 'adalimumab%'OR libelle_court_groupe LIKE 'enoxaparine%') AND periode = '%s'
    GROUP BY CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%' 
                AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZARZIO (FILGRASTIM)' ELSE 'FILGRASTIM' END
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZIEXTENZO (PEGFILGRASTIM)' ELSE 'PEGFILGRASTIM' END 
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%EPOETINE%' 
                THEN 'EPOETINE'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ETANERCEPT%' 
                THEN 'ETANERCEPT'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ADALIMUMAB%' 
                THEN 'ADALIMUMAB'
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ENOXAPARINE%' 
                THEN 'ENOXAPARINE' END, CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END)
SELECT pm.PRESENTATION, pm.LABO,
    COALESCE(v.QTE, 0) AS QTE,
    COALESCE(v.PFHT, 0) AS PFHT,
    CAST(CASE WHEN SUM(COALESCE(v.PFHT, 0)) OVER (PARTITION BY pm.BASE_MOLECULE) = 0 
            THEN 0 ELSE ROUND(COALESCE(v.PFHT, 0) * 100.0 / SUM(COALESCE(v.PFHT, 0)) OVER (PARTITION BY pm.BASE_MOLECULE), 2)
         END AS DECIMAL(10,2)) AS PDM_PFHT
FROM ProduitsMolecules pm LEFT JOIN Ventes v ON pm.PRESENTATION = v.PRESENTATION AND pm.LABO = v.LABO
ORDER BY CASE WHEN pm.PRESENTATION LIKE '%FILGRASTIM%' AND pm.PRESENTATION NOT LIKE '%PEG%' THEN 1
        WHEN pm.PRESENTATION LIKE '%PEGFILGRASTIM%' THEN 2
        WHEN pm.PRESENTATION LIKE '%EPOETINE%' THEN 3
        WHEN pm.PRESENTATION LIKE '%ETANERCEPT%' THEN 4
        WHEN pm.PRESENTATION LIKE '%ADALIMUMAB%' THEN 5
        WHEN pm.PRESENTATION LIKE '%ENOXAPARINE%' THEN 6 END, pm.PRESENTATION, pm.LABO DESC;",
                   n_auto_adhpha_ecollect,
                   periode)
    result <- dbGetQuery(con, query)
  return(result)
}

# Exécuter la fonction
#res <- Bisomilaire(178, con)
#print(res)

 ############## TRONQUER DES TEXTE  ###########
truncate_string <- function(txt, max_len = 50) {
  if (nchar(txt) > max_len) {
    paste0(substr(txt, 1, max_len), "...")
  } else {
    txt
  }
}


### Premiere lettre en Majuscule  ########

capitalize_first <- function(txt) {
  if (nchar(txt) == 0) return(txt)
  txt_lower <- tolower(txt)
  first_char <- toupper(substr(txt_lower, 1, 1))
  rest_chars <- substr(txt_lower, 2, nchar(txt_lower))
  paste0(first_char, rest_chars)
}

