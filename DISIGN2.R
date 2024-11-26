##################EXO###################
########### EMAILING ROSP 2024 ########################
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


glob <- list(
  log = TRUE,
  ymdhms = "%Y-%m-%d_%H-%M-%S",
  month = NULL, # If need to force something != than prev month
  customer_to = "biama@ospharea.com",
  customer_cc = "adouchin@ospharea.com",
  ospharm_from = "Coopérative OSPHARM<solutions@ospharea.com>",
  ospharm_cc = "datastat@ospharea.com"
)

#CONNEXION A LA BASE DE DONNEE SQL Server
con <- DBI::dbConnect(odbc::odbc(), Database = "OSP_DATASTAT", 
                      Driver = Sys.getenv("sql_driver"), Server = Sys.getenv("sql_server"), 
                      UID = Sys.getenv("sql_uid"), PWD = Sys.getenv("sql_pwd"), 
                      Port = Sys.getenv("sql_port"))

#IMPORTATION DES DATA
Substitution <- dbGetQuery(con, "select cip, count(*) as SUBSTITU
from vuProdAdhpha where syndic_adhpha = 1 and dextinct_adhpha is null
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202409 and moisok_completudepha in (0,8))
and n_auto_adhpha in (
select n_auto_adhpha_artic
from os_stat_artic where periode between 202401 and 202409
and n_auto_artic_artic in (select n_auto_artic From OSP_STAT_BU.dbo.os_hybride_artic where type_artic = 'H')
group by n_auto_adhpha_artic
having sum(qt_vendu_artic) > 0)
and n_auto_adhpha in (
select n_auto_adhpha_artic
  from os_stat_artic
  inner join os_labogener_artic on n_auto_artic_artic=n_auto_artic
  inner join os_adhfour on os_labogener_artic.n_auto_adhfour=os_adhfour.n_auto_adhfour
  inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien
  where periode between 202401 and 202409
  and type_artic='B'
  and (os_grpgener_artic.libelle_court_groupe like 'FILGRASTIM%' OR os_grpgener_artic.libelle_court_groupe like 'PEGFILGRASTIM%')
  group by n_auto_adhpha_artic
having sum(qt_vendu_artic) > 0)
group by cip
")
Substitution <- as_tibble(Substitution)
#str(Substitution)

Mail_pha <- dbGetQuery(con, "select cip, n_auto_adhpha as id, rs_adhpha, mail from vuProdAdhpha left join ospharea_adherents on finess_adhpha = finess 
where syndic_adhpha = 1 and dextinct_adhpha is null 
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202409 and moisok_completudepha in (0,8))
")
Mail_pha <- as_tibble(Mail_pha)
#view(Mail_pha)

ACTES_PHA <- dbGetQuery(con, "
WITH PHARMACIE AS (
    select cip, n_auto_adhpha from vuProdAdhpha a
where syndic_adhpha in (1)
AND dextinct_adhpha IS NULL
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202409 and moisok_completudepha in (0,8))
), ACTU as (
select n_auto_adhpha_B2,
coalesce(sum( case when acteB2 in ('AC1', 'AC2', 'AC3', 'AC4', 'ASI', 'ASS', 'BMI', 'BMS', 'BMT') and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'ENTRETIENS',
coalesce(sum( case when acteB2 = 'EFE' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'EFE',
coalesce(sum( case when acteB2 = 'TRD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as 'TRD',
coalesce(sum( case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
from os_stat_B2
where periode between 202401 and 202409
and os_stat_B2.n_auto_adhpha_B2 in (select n_auto_adhpha from vuProdAdhpha a where syndic_adhpha in (1) AND dextinct_adhpha IS NULL)
and os_stat_B2.n_auto_adhpha_B2 not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202409 and moisok_completudepha in (0,8))
group by n_auto_adhpha_B2
),
PRECEDENT as (
select n_auto_adhpha_B2,
coalesce(sum( case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
from os_stat_B2
where periode between 202301 and 202309
and os_stat_B2.n_auto_adhpha_B2 in (select n_auto_adhpha from vuProdAdhpha a where syndic_adhpha in (1) AND dextinct_adhpha IS NULL)
and os_stat_B2.n_auto_adhpha_B2 not in (select n_auto_adhpha from os_completudepha where periode_completudepha between  202401 and 202409 and moisok_completudepha in (0,8))
group by n_auto_adhpha_B2)
select ACTU.*, PRECEDENT.RKD as RKD_2023, coalesce(case when  PRECEDENT.RKD !=0  then (ACTU.RKD - PRECEDENT.RKD) * 100/PRECEDENT.RKD else case when ACTU.RKD > 0 THEN 10 else null end end, 0) AS Evolution_RKD
from PHARMACIE
left join ACTU on PHARMACIE.n_auto_adhpha = ACTU.n_auto_adhpha_B2
left join PRECEDENT on PHARMACIE.n_auto_adhpha = PRECEDENT.n_auto_adhpha_B2")
ACTES_PHA <- as_tibble(ACTES_PHA)


#FUISON DES TABLES
pharmaciens_data <- ACTES_PHA%>%
  left_join(Mail_pha, by=c("n_auto_adhpha_B2" = "id"))%>%
  left_join(Substitution, by = c("cip"= "cip"))%>%
  mutate(SUBSTITU = replace_na(SUBSTITU, 0), Toilette = 1)%>%
  select(mail, n_auto_adhpha_B2, rs_adhpha, ENTRETIENS, EFE, TRD, Evolution_RKD, SUBSTITU, Toilette)
#pharmaciens_data$mail <- c("biama@ospharea.com", "mprudhomme@ospharea.com", "adouchin@ospharea.com", "odufut@ospharea.com", "rborios@ospharea.com", "djanvier@ospharea.com")

view(pharmaciens_data)

# Fonction pour générer une coche si un pharmacien a réalisé au moins un acte
generate_checkmark <- function(value, is_evolution = FALSE) {
  if (is_evolution) {
    if (value >= 10) {
      return('<td style="text-align:center; color:green; font-size:30px; border: 1px solid black;">✔️</td>')
    } else {
      return('<td style="border: 1px solid black;"></td>')
    }
  } else {
    if (value > 0) {
      return('<td style="text-align:center; color:green; font-size:30px; border: 1px solid black;">✔️</td>')
    } else {
      return('<td style="border: 1px solid black;"></td>')
    }
  }
}

# Fonction pour calculer le montant attendu en fonction des actes réalisés
calculate_montant_attendu <- function(pharmacien_row) {
  montant_total <- 0
  montants <- c(50, 100, 400, 50, 250, 100)  # Montants associés aux actes TRD, Toilette, ENTRETIENS, EFE, Evolution_RKD, SUBSTITU
  actes <- c(pharmacien_row$TRD, pharmacien_row$Toilette, pharmacien_row$ENTRETIENS, pharmacien_row$EFE, pharmacien_row$Evolution_RKD, pharmacien_row$SUBSTITU)
  
  for (i in 1:length(actes)) {
    if (actes[i] > 0) {
      montant_total <- montant_total + montants[i]
    }
  }
  
  return(montant_total)
}

# Générer les lignes HTML spécifiques pour le tableau du pharmacien avec condition sur Evolution_RKD
generate_pharmacien_rows <- function(pharmacien_row) {
  rows <- paste0('
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">100 €</span> Pour la substitution d’un hybride et d’un biosimilaire en 2024.</td>', generate_checkmark(pharmacien_row$SUBSTITU), '</tr>
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">50 €</span> Pour la réalisation d’au moins un Trod angine au sein de l’officine.</td>', generate_checkmark(pharmacien_row$TRD), '</tr>
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">250 €</span> Si le nombre de kits de dépistage du cancer colorectal remis en officine en 2024 a augmenté d’au moins 10 % par rapport à 2023. (Sous réserve de la validation de l\'Assurance Maladie)</td>', generate_checkmark(pharmacien_row$Evolution_RKD, is_evolution = TRUE), '</tr>
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">50 €</span> Pour la réalisation en 2024 d’au moins un entretien femmes enceintes.</td>', generate_checkmark(pharmacien_row$EFE), '</tr>
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">400 €</span> Pour la réalisation en 2024, d’au moins un entretien auprès de patients atteints de maladies chroniques (asthme, anticancéreux, AOK, AOD, bilan de médication).</td>', generate_checkmark(pharmacien_row$ENTRETIENS), '</tr>
    <tr>
      <td style="border: 1px solid black;"><span style="color:blue; font-weight:bold">100 €</span> Au titre de l’aménagement des locaux ou la disposition de locaux adaptés au public en 2024 en vue de dépister une infection urinaire. Ces adaptations feront l’objet d’une déclaration au moyen d’un téléservice accessible depuis le portail internet de l’assurance maladie dédié aux professionnels de santé **.</td>', generate_checkmark(pharmacien_row$Toilette), '</tr>')
  
  return(rows)
}


# Générer et envoyer un email avec les résultats de chaque pharmacien
send_email_to_pharmacien <- function(pharmacien_row, email, nom) {
  # Calculer le montant attendu
  montant_attendu <- calculate_montant_attendu(pharmacien_row)
  
  # Générer le contenu de l'email pour ce pharmacien avec la raison sociale
  email_body <- paste0('
  <div style="background-color:rgba(255, 255, 255, 0.9); padding:50px; position:relative;">
      <div style="position:relative; background-color:rgba(255, 255, 255, 0.9); padding:30px; border-radius:10px;">
          <div style="text-align:center; display:flex; justify-content:center; align-items:center; padding:40px 0;">
              <div style="background-color:#29a745; color:white; font-weight:bold; padding:25px 35px; border-radius:50px; font-size:24px; width:90%;">
                  Votre étude personnalisée sur les rémunérations exceptionnelles 2024 
              </div>
          </div>
          <p>Bonjour,</p>
          
          <p><a href="https://www.ameli.fr/pharmacien/actualites/signature-de-l-avenant-1-la-convention-qui-favorise-la-lutte-contre-les-deserts-pharmaceutiques" style="color:blue;">L’avenant 1 à la convention pharmaceutique</a> signé le 10 juin dernier prévoit de <strong>nouvelles rémunérations forfaitaires à compter de 2024</strong>. Cette ROSP sera versée au 1er semestre 2025.</p>
          <p>Cumulées, elles peuvent conduire à un versement total de 950 € pour votre officine.</p>

          <p><span style="color:green; font-weight:bold; font-size:20px;">Pourquoi ce suivi ?</span> Evaluer l\'appropriation des nouvelles missions de santé publique en officine et saisir l\'opportunité de valoriser votre expertise en tant qu\'acteur central du système de santé.</p>
          <p><span style="color:blue; font-weight:bold; font-size:20px"> Pharmacie ', pharmacien_row$rs_adhpha, '</span> : où en êtes-vous à fin septembre?</p>

          <table border="1" cellpadding="5" cellspacing="0" style="margin-left:5%; margin-right:5%; border-collapse:separate; border-radius:20px; border: 1px solid black; overflow:hidden;">
              <tr style="background-color:#29a745; border-radius: 15px;">
                  <th style="text-align:center; color:white; border: 1px solid black; border-top-left-radius: 15px;">CIBLE ET RÉMUNÉRATION</th>
                  <th style="text-align:center; color:white; border: 1px solid black; border-top-right-radius: 15px;">RÉALISÉ À FIN SEPTEMBRE 2024</th>
              </tr>
              ', generate_pharmacien_rows(pharmacien_row), '
              <tr>
                  <td style="text-align:right; font-size:25px; color:green; font-weight:bold; font-style:italic; border: 1px solid black; border-bottom-left-radius: 15px;">Votre rémunération attendue* </td>
                  <td style="text-align:center; font-size:25px; color:green; font-weight:bold; border: 1px solid black; border-bottom-right-radius: 15px;">', montant_attendu, ' €</td>
              </tr>
          </table>

          <p><span style="font-weight:bold;">* </span> Les chiffres que nous communiquons sont issus des informations transmises depuis votre LGO à OSPHARM. Si des informations vous semblent erronées, n\'hésitez pas à prendre contact avec nos équipes.</p>
          
          <p><span style="font-weight:bold;">** </span> Par défaut, cet objectif est coché sous réserve de sa réalisation et de sa déclaration sur le site d\' Ameli Pro.</p>
        
          <p>Pour plus d’informations, rendez-vous sur la plateforme <a href="https://datastat.ospharm.org" style="color:blue;">OSPHARM DATASTAT</a> (votre identifiant : <span style="color:blue;">', email, '</span>).</p>
          
          <p style = "font-weight:bold; margin-top: 20px;">Une nouvelle étude personnalisée vous sera communiquée avec vos données actualisées début décembre.</p>
          <p style="color:black; font-style:italic; margin-top: 10px;">Ospharm, votre allié pour construire la pharmacie de demain.</p>

          <div style="text-align:left; margin-top: 20px;">
              <img src="https://www.ospharm.com/wp-content/uploads/2024/10/logo_site.png" alt="Logo OSPHARM" style="width:150px; height:auto;">
              <p style="margin-top: 10px; color:green;">
                  2 Avenue du Gulf Stream<br>
                  44380 Pornichet<br>
                  <a href="mailto:solutions@ospharea.com" style="color:blue;">solutions@ospharea.com</a><br>
                  <a href="tel:+33(0)2 40 53 63 44" style="color:blue;">+33(0)2 40 53 63 44</a><br>
                  <a href="https://www.ospharm.com" style="color:blue;">www.ospharm.com</a>
              </p>
          </div>
      </div>
  </div>
  ')
  # Informations de l'API Mailjet et envoi de l'email ici
  
  mailjet_api_key <- Sys.getenv("mailjet_user")
  mailjet_secret_key <- Sys.getenv("mailjet_pass")
  
  # Construction de la requête JSON pour l'API Mailjet
  mailjet_payload <- toJSON(list(
    Messages = list(
      list(
        From = list(
          Email = glob$ospharm_from,  # Remplacez par votre email d'expéditeur
          Name = "OSPHARM DATASTAT"
        ),
        To = list(
          list(
            Email = email,  # Email du pharmacien
            Name = nom
          )
        ),
        Subject = "OSPHARM vous accompagne : Votre étude personnalisée sur les rémunérations exceptionnelles 2024",
        HTMLPart = email_body
      )
    )
  ), auto_unbox = TRUE)
  
  # Envoi de la requête POST à l'API Mailjet pour envoyer l'email
  response <- POST(
    url = "https://api.mailjet.com/v3.1/send",
    authenticate(mailjet_api_key, mailjet_secret_key),
    add_headers("Content-Type" = "application/json"),
    body = mailjet_payload,
    encode = "json"
  )
  
  # Vérification de la réponse
  if (status_code(response) == 200) {
    print(paste("Email envoyé avec succès à", nom, "(", email, ")"))
  } else {
    print(paste("Erreur lors de l'envoi de l'email à", nom, ":"))
    print(content(response, as = "text"))
  }
}

# Boucle pour envoyer un email à chaque pharmacien
for (i in 1:nrow(pharmaciens_data)) {
  pharmacien_row <- pharmaciens_data[i, ]
  send_email_to_pharmacien(pharmacien_row, pharmacien_row$mail, paste("Pharmacien", i))
}
