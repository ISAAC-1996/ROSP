############## PROJET ARTICLE ROSP #################
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
library(readxl)
library(sf)
library(leaflet)
library(htmlwidgets)

#CONNEXION A LA BASE DE DONNEE SQL Server
con <- DBI::dbConnect(odbc::odbc(), Database = "OSP_DATASTAT", 
                      Driver = Sys.getenv("sql_driver"), Server = Sys.getenv("sql_server"), 
                      UID = Sys.getenv("sql_uid"), PWD = Sys.getenv("sql_pwd"), 
                      Port = Sys.getenv("sql_port"))

dpt <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/Extrapolation_Projet_Ospharm/Input/Les departements de France.xlsx")
str(dpt)

####### Novembre #############
Substitution_11 <- dbGetQuery(con, "select cip, count(*) as SUBSTITU
from vuProdAdhpha where syndic_adhpha = 1 and dextinct_adhpha is null
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202412 and moisok_completudepha in (0,8))
and n_auto_adhpha in (
select n_auto_adhpha_artic
from os_stat_artic where periode between 202401 and 202412
and n_auto_artic_artic in (select n_auto_artic From OSP_STAT_BU.dbo.os_hybride_artic where type_artic = 'H')
group by n_auto_adhpha_artic
having sum(qt_vendu_artic) > 0)
and n_auto_adhpha in (
select n_auto_adhpha_artic
  from os_stat_artic
  inner join os_labogener_artic on n_auto_artic_artic=n_auto_artic
  inner join os_adhfour on os_labogener_artic.n_auto_adhfour=os_adhfour.n_auto_adhfour
  inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien
  where periode between 202401 and 202412
  and type_artic='B'
  and (os_grpgener_artic.libelle_court_groupe like 'FILGRASTIM%' OR os_grpgener_artic.libelle_court_groupe like 'PEGFILGRASTIM%' or  os_grpgener_artic.libelle_court_groupe like 'RANIBIZUMAB%')
  group by n_auto_adhpha_artic
having sum(qt_vendu_artic) > 0)
group by cip")
Substitution_11 <- as_tibble(Substitution_11)

Mail_pha_11 <- dbGetQuery(con, "select cip, n_auto_adhpha as id, rs_adhpha, mail ,  case when SUBSTRING(cp_ville, 1, 3) = 200 then '2A'
 when SUBSTRING(cp_ville, 1, 3) = 201 or SUBSTRING(cp_ville, 1, 3) = 202 or SUBSTRING(cp_ville, 1, 3) = 206 then '2B'
 when SUBSTRING(cp_ville, 1, 3) in (971, 972, 973, 974, 976, 988, 987, 980) then SUBSTRING(cp_ville, 1, 3)
 else SUBSTRING(cp_ville, 1, 2) end as Departement
from vuProdAdhpha left join ospharea_adherents on finess_adhpha = finess 
where syndic_adhpha = 1 and dextinct_adhpha is null 
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202412 and moisok_completudepha in (0,8))")
colnames(Mail_pha_11)
Mail_pha_11 <- as_tibble(Mail_pha_11)%>%
  left_join(dpt, by = c("Departement" = "code"))%>%
  select(cip, id, mail, rs_adhpha, Departement, nom)
#view(Mail_pha_11)
ACTES_PHA_11 <- dbGetQuery(con, "
WITH PHARMACIE AS (
    select cip, n_auto_adhpha from vuProdAdhpha a
where syndic_adhpha in (1)
AND dextinct_adhpha IS NULL
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202412 and moisok_completudepha in (0,8))
), ACTU as (
select n_auto_adhpha_B2,
coalesce(sum( case when acteB2 in ('AC1', 'AC2', 'AC3', 'AC4', 'ASI', 'ASS', 'BMI', 'BMS', 'BMT') and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'ENTRETIENS',
coalesce(sum( case when acteB2 = 'EFE' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'EFE',
coalesce(sum( case when acteB2 = 'TRD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'TRD',
coalesce(sum( case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
from os_stat_B2
where periode between 202401 and 202412
and os_stat_B2.n_auto_adhpha_B2 in (select n_auto_adhpha from vuProdAdhpha a where syndic_adhpha in (1) AND dextinct_adhpha IS NULL)
and os_stat_B2.n_auto_adhpha_B2 not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202412 and moisok_completudepha in (0,8))
group by n_auto_adhpha_B2
),
PRECEDENT as (
select n_auto_adhpha_B2,
coalesce(sum( case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
from os_stat_B2
where periode between 202301 and 202312
and os_stat_B2.n_auto_adhpha_B2 in (select n_auto_adhpha from vuProdAdhpha a where syndic_adhpha in (1) AND dextinct_adhpha IS NULL)
and os_stat_B2.n_auto_adhpha_B2 not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202301 and 202312 and moisok_completudepha in (0,8))
group by n_auto_adhpha_B2)
select ACTU.*, PRECEDENT.RKD as RKD_2023, coalesce(case when  PRECEDENT.RKD !=0  then (ACTU.RKD - PRECEDENT.RKD) * 100/PRECEDENT.RKD else case when ACTU.RKD > 0 THEN 10 else null end end, 0) AS Evolution_RKD
from PHARMACIE
left join ACTU on PHARMACIE.n_auto_adhpha = ACTU.n_auto_adhpha_B2
left join PRECEDENT on PHARMACIE.n_auto_adhpha = PRECEDENT.n_auto_adhpha_B2")
ACTES_PHA_11 <- as_tibble(ACTES_PHA_11)

# Fusion des tables 
pharmaciens_data_11 <- ACTES_PHA_11%>%
  left_join(Mail_pha_11, by=c("n_auto_adhpha_B2" = "id"))%>%
  left_join(Substitution_11, by = c("cip"= "cip"))%>%
  mutate(SUBSTITU = replace_na(SUBSTITU, 0),Toilette = 1,
         Remuneration = 
           ifelse(ENTRETIENS > 0, 400, 0) +
           ifelse(EFE > 0, 50, 0) +
           ifelse(TRD > 0, 50, 0) +
           ifelse(Evolution_RKD >= 10, 250, 0) +
           ifelse(SUBSTITU > 0, 100, 0) +
           ifelse(Toilette > 0, 100, 0) )%>%
  select(mail, n_auto_adhpha_B2, rs_adhpha, ENTRETIENS, EFE, TRD, Evolution_RKD, SUBSTITU, Toilette, Departement, nom, Remuneration)%>%
  arrange(mail, n_auto_adhpha_B2, rs_adhpha)
str(pharmaciens_data_11)
view(pharmaciens_data_11)


write.xlsx(pharmaciens_data_11, "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/ROSP_2024/table_carte.xlsx")

# Regrouper les données par département
data_aggregated <- pharmaciens_data_11 %>%
  group_by(Departement) %>%
  summarise(
    Remuneration_moyenne = mean(Remuneration, na.rm = TRUE),
    Nombre_clients = n_distinct(n_auto_adhpha_B2))
#view(data_aggregated)

# Charger les données géographiques des départements (GeoJSON ou shapefile)
geo_departements <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson") 
#view(geo_departements)

# Calcul des pourcentages des missions réalisées par département
missions_filtered <- pharmaciens_data_11 %>%
  group_by(Departement, n_auto_adhpha_B2) %>% # Grouper par département et ID client
  summarise(
    ENTRETIENS = max(ENTRETIENS > 0),
    EFE = max(EFE > 0),
    TRD = max(TRD > 0),
    Evolution_RKD = ifelse(max(Evolution_RKD) >=10, max(Evolution_RKD > 0), 0), # Condition Evolution_RKD
    SUBSTITU = max(SUBSTITU > 0),
    .groups = "drop") %>%
  rowwise()%>%
  mutate(Missions_realisees = sum(c_across(c(ENTRETIENS, EFE, TRD, Evolution_RKD, SUBSTITU)))) %>%
  ungroup() %>%
  group_by(Departement) %>%
  summarise(
    Total_clients = n(), # Nombre total de clients uniques
    mission_non_realisee = sum(Missions_realisees == 0),
    Missions_realisees_1 = sum(Missions_realisees == 1),
    Missions_realisees_2 = sum(Missions_realisees == 2),
    Missions_realisees_3 = sum(Missions_realisees == 3),
    Missions_realisees_4 = sum(Missions_realisees == 4),
    Missions_realisees_5 = sum(Missions_realisees == 5)) %>%
  mutate(
    Perc_non_realisee = round((mission_non_realisee / Total_clients) * 100, 1),
    Perc_mission_1 = round((Missions_realisees_1 / Total_clients) * 100, 1),
    Perc_mission_2 = round((Missions_realisees_2 / Total_clients) * 100, 1),
    Perc_mission_3 = round((Missions_realisees_3 / Total_clients) * 100, 1),
    Perc_mission_4 = round((Missions_realisees_4 / Total_clients) * 100, 1),
    Perc_mission_5 = round((Missions_realisees_5 / Total_clients) * 100, 1))
# Voir les résultats
view(missions_filtered)

# Joindre les données avec le fichier géographique
geo_departements <- geo_departements %>%
  left_join(
    pharmaciens_data_11 %>%
      group_by(Departement) %>%
      summarise(
        Remuneration_moyenne = mean(Remuneration, na.rm = TRUE),
        Nombre_pharmaciens = n()
      ),
    by = c("code" = "Departement")
  ) %>%
  left_join(missions_filtered, by = c("code" = "Departement"))

# Ajouter des coordonnées pour les étiquettes si elles ne sont pas déjà présentes
if (!"longitude" %in% colnames(geo_departements) || !"latitude" %in% colnames(geo_departements)) {
  geo_departements <- geo_departements %>%
    mutate(
      longitude = st_coordinates(st_centroid(geometry))[, 1],
      latitude = st_coordinates(st_centroid(geometry))[, 2]
    )
}

# Définir une palette de couleurs pour la rémunération moyenne
palette_remuneration <- colorNumeric(
  palette = "YlOrRd", 
  domain = geo_departements$Remuneration_moyenne
)

# Créer la carte interactive
Carte <- leaflet(geo_departements) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette_remuneration(Remuneration_moyenne),
    weight = 1,
    color = "white",
    fillOpacity = 0.7,
    label = ~lapply(
      paste0(
        "<div style='font-size:12px;'>",
        "<strong>Département :</strong> ", ifelse(!is.na(nom), nom, "Non disponible"), "<br>",
        "<strong>Rémunération moyenne :</strong> ", 
        ifelse(!is.na(Remuneration_moyenne), paste0(round(Remuneration_moyenne, 2), " €"), "Non disponible"), "<br>",
        "<strong>Nombre de Pharmacies :</strong> ", 
        ifelse(!is.na(Nombre_pharmaciens), Nombre_pharmaciens, "Non disponible"), "<br>",
        "<strong>Missions réalisées (%):</strong><br>",
        " - 0/5 : ", ifelse(!is.na(Perc_non_realisee), paste0(Perc_non_realisee, "%"), "Non disponible"), "<br>",
        " - 1/5 : ", ifelse(!is.na(Perc_mission_1), paste0(Perc_mission_1, "%"), "Non disponible"), "<br>",
        " - 2/5 : ", ifelse(!is.na(Perc_mission_2), paste0(Perc_mission_2, "%"), "Non disponible"), "<br>",
        " - 3/5 : ", ifelse(!is.na(Perc_mission_3), paste0(Perc_mission_3, "%"), "Non disponible"), "<br>",
        " - 4/5 : ", ifelse(!is.na(Perc_mission_4), paste0(Perc_mission_4, "%"), "Non disponible"), "<br>",
        " - 5/5 : ", ifelse(!is.na(Perc_mission_5), paste0(Perc_mission_5, "%"), "Non disponible"),
        "</div>"
      ), htmltools::HTML),
    highlight = highlightOptions(
      weight = 3,
      color = "blue",
      fillOpacity = 0.9,
      bringToFront = TRUE))%>%
  addLabelOnlyMarkers(
    lng = ~longitude,
    lat = ~latitude,
    label = ~paste0(round(Remuneration_moyenne, 2), " €"),
    labelOptions = labelOptions(
      noHide = TRUE, 
      direction = 'center',
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "8px",
        "font-weight" = "bold",
        "background-color" = "rgba(255,255,255,0.7)",
        "padding" = "2px")))%>%
  addLegend(
    pal = palette_remuneration, 
    values = geo_departements$Remuneration_moyenne, 
    title = "Rémunération Moyenne (€)", 
    position = "bottomright"
  )

Carte
########EXPORT######
saveWidget(Carte, file = "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/ROSP_2024/carte_article.html", selfcontained = TRUE)

# Afficher la carte dans R
browseURL("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/ROSP_2024/carte_article.html")
