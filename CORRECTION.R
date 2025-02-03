##################exo###################
########### emailing rosp 2024##########
# library
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
  month = NULL, 
  customer_to = "biama@ospharea.com",
  customer_cc = "adouchin@ospharea.com",
  ospharm_from = "Coopérative OSPHARM<solutions@ospharea.com>",
  ospharm_cc = "datastat@ospharea.com"
)

###############################################
### 1. connexion à la base de données sql server
###############################################
con <- DBI::dbConnect(
  odbc::odbc(),
  Database = "OSP_DATASTAT", 
  Driver   = Sys.getenv("sql_driver"),
  Server   = Sys.getenv("sql_server"), 
  UID      = Sys.getenv("sql_uid"),
  PWD      = Sys.getenv("sql_pwd"),
  Port     = Sys.getenv("sql_port")
)

############################################################
### 2. importation des tables pour la période septembre
###    (202401–202409) : pharmaciens_data_09
############################################################
substitution <- DBI::dbGetQuery(con, "
select cip, count(*) as SUBSTITU
from vuProdAdhpha 
where syndic_adhpha = 1 and dextinct_adhpha is null
  and n_auto_adhpha not in (
    select n_auto_adhpha 
    from os_completudepha 
    where periode_completudepha between 202401 and 202409
      and moisok_completudepha in (0,8)
  )
  and n_auto_adhpha in (
    select n_auto_adhpha_artic
    from os_stat_artic
    where periode between 202401 and 202409
      and n_auto_artic_artic in (
        select n_auto_artic 
        from OSP_STAT_BU.dbo.os_hybride_artic
        where type_artic = 'H'
      )
    group by n_auto_adhpha_artic
    having sum(qt_vendu_artic) > 0
  )
  and n_auto_adhpha in (
    select n_auto_adhpha_artic
    from os_stat_artic
    inner join os_labogener_artic on n_auto_artic_artic = n_auto_artic
    inner join os_adhfour        on os_labogener_artic.n_auto_adhfour = os_adhfour.n_auto_adhfour
    inner join os_grpgener_artic on os_labogener_artic.lien = os_grpgener_artic.lien
    where periode between 202401 and 202409
      and type_artic = 'B'
      and (
        os_grpgener_artic.libelle_court_groupe like 'FILGRASTIM%'
        or os_grpgener_artic.libelle_court_groupe like 'PEGFILGRASTIM%'
      )
    group by n_auto_adhpha_artic
    having sum(qt_vendu_artic) > 0
  )
group by cip
")

mail_pha <- DBI::dbGetQuery(con, "
select cip, n_auto_adhpha as id, rs_adhpha, mail, case
when SUBSTRING(cp_ville, 1, 3) in (971, 972, 973, 974, 976, 988, 980, 987)  then SUBSTRING(cp_ville, 1, 3)
when SUBSTRING(cp_ville, 1, 3) = 200 then '2A'
 when SUBSTRING(cp_ville, 1, 3) in (201, 202, 203, 206, 204, 205) then '2B' else
 SUBSTRING(cp_ville, 1, 2) end as Departement
from vuProdAdhpha 
left join ospharea_adherents on finess_adhpha = finess
where dextinct_adhpha is null
  and n_auto_adhpha not in (
    select n_auto_adhpha 
    from os_completudepha 
    where periode_completudepha between 202401 and 202409
      and moisok_completudepha in (0,8) )
")
#view(mail_pha)

actes_pha <- DBI::dbGetQuery(con, "
with PHARMACIE as (
  select cip, n_auto_adhpha
  from vuProdAdhpha a
  where syndic_adhpha = 1
    and dextinct_adhpha is null
    and n_auto_adhpha not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202401 and 202409
        and moisok_completudepha in (0,8)
    )
),
ACTU as (
  select 
    n_auto_adhpha_B2,
    coalesce(sum(case when acteB2 in ('AC1','AC2','AC3','AC4','ASI','ASS','BMI','BMS','BMT') and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as ENTRETIENS,
    coalesce(sum(case when acteB2 = 'EFE' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as EFE,
    coalesce(sum(case when acteB2 = 'TRD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as TRD,
    coalesce(sum(case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
  from os_stat_B2
  where periode between 202401 and 202409
    and n_auto_adhpha_B2 in (
      select n_auto_adhpha
      from vuProdAdhpha a
      where syndic_adhpha = 1
        and dextinct_adhpha is null
    )
    and n_auto_adhpha_B2 not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202401 and 202409
        and moisok_completudepha in (0,8)
    )
  group by n_auto_adhpha_B2
),
PRECEDENT as (
  select
    n_auto_adhpha_B2,
    coalesce(sum(case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
  from os_stat_B2
  where periode between 202301 and 202309
    and n_auto_adhpha_B2 in (
      select n_auto_adhpha
      from vuProdAdhpha a
      where syndic_adhpha = 1
        and dextinct_adhpha is null
    )
    and n_auto_adhpha_B2 not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202301 and 202309
        and moisok_completudepha in (0,8)
    )
  group by n_auto_adhpha_B2
)
select
  ACTU.*,
  PRECEDENT.RKD as RKD_2023,
  coalesce(
    case when PRECEDENT.RKD != 0
      then (ACTU.RKD - PRECEDENT.RKD)*100.0/PRECEDENT.RKD
      else case when ACTU.RKD > 0 then 10 else null end
    end
  , 0) as Evolution_RKD
from PHARMACIE
left join ACTU on PHARMACIE.n_auto_adhpha = ACTU.n_auto_adhpha_B2
left join PRECEDENT on PHARMACIE.n_auto_adhpha = PRECEDENT.n_auto_adhpha_B2
")

# On fusionne pour la période sept
pharmaciens_data_09 <- actes_pha %>%
  left_join(mail_pha,     by=c("n_auto_adhpha_B2"="id")) %>%
  left_join(substitution, by="cip") %>%
  mutate(
    SUBSTITU = replace_na(SUBSTITU, 0),
    Toilette = 1
  ) %>%
  select(
    cip, mail, n_auto_adhpha_B2, rs_adhpha,
    ENTRETIENS, EFE, TRD, Evolution_RKD, SUBSTITU, Toilette, Departement
  )

                      ############################################################
                      ### 3. importation des tables pour la période décembre
                      ###    (202401–202412) : pharmaciens_data_11
                      ############################################################
substitution_11 <- DBI::dbGetQuery(con, "
select cip, count(*) as SUBSTITU_12
from vuProdAdhpha
where syndic_adhpha = 1
  and dextinct_adhpha is null
  and n_auto_adhpha not in (
    select n_auto_adhpha
    from os_completudepha
    where periode_completudepha between 202401 and 202409
      and moisok_completudepha in (0,8)
  )
  and n_auto_adhpha in (
    select n_auto_adhpha_artic
    from os_stat_artic
    where periode between 202401 and 202412
      and n_auto_artic_artic in (
        select n_auto_artic
        from OSP_STAT_BU.dbo.os_hybride_artic
        where type_artic = 'H'
      )
    group by n_auto_adhpha_artic
    having sum(qt_vendu_artic) > 0
  )
  and n_auto_adhpha in (
    select n_auto_adhpha_artic
    from os_stat_artic
    inner join os_labogener_artic on n_auto_artic_artic = n_auto_artic
    inner join os_adhfour        on os_labogener_artic.n_auto_adhfour = os_adhfour.n_auto_adhfour
    inner join os_grpgener_artic on os_labogener_artic.lien = os_grpgener_artic.lien
    where periode between 202401 and 202412
      and type_artic = 'B'
      and (
        os_grpgener_artic.libelle_court_groupe like 'FILGRASTIM%'
        or os_grpgener_artic.libelle_court_groupe like 'PEGFILGRASTIM%'
        or os_grpgener_artic.libelle_court_groupe like 'RANIBIZUMAB%'
      )
    group by n_auto_adhpha_artic
    having sum(qt_vendu_artic) > 0
  )
group by cip
")

mail_pha_11 <- DBI::dbGetQuery(con, "
select 
  a.cip,
  a.n_auto_adhpha as id,
  rs_adhpha,
  mail,
  case when ssii_adhpha = 'EVERYS' then 1 else 0 end as everys,
  convert(nvarchar, b.date_dernier_vente, 103) as Vente
from vuProdAdhpha a
left join ospharea_adherents on finess_adhpha = finess
left join ex_adhpha b        on a.n_auto_adhpha = b.n_auto_adhpha
where dextinct_adhpha is null
  and a.n_auto_adhpha not in (
    select n_auto_adhpha
    from os_completudepha
    where periode_completudepha between 202401 and 202409
      and moisok_completudepha in (0,8)
  )
")

actes_pha_11 <- DBI::dbGetQuery(con, "
with PHARMACIE as (
  select cip, n_auto_adhpha
  from vuProdAdhpha a
  where syndic_adhpha = 1
    and dextinct_adhpha is null
    and n_auto_adhpha not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202401 and 202409
        and moisok_completudepha in (0,8)
    )
),
ACTU as (
  select
    n_auto_adhpha_B2,
    coalesce(sum(case when acteB2 in ('AC1','AC2','AC3','AC4','ASI','ASS','BMI','BMS','BMT') and qt_vendu_B2>0 then qt_vendu_B2 end),0) as ENTRETIENS_12,
    coalesce(sum(case when acteB2 = 'EFE' and qt_vendu_B2>0 then qt_vendu_B2 end),0) as EFE_12,
    coalesce(sum(case when acteB2 = 'TRD' and qt_vendu_B2>0 then qt_vendu_B2 end),0) as TRD_12,
    coalesce(sum(case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end),0) as RKD_12
  from os_stat_B2
  where periode between 202401 and 202412
    and n_auto_adhpha_B2 in (
      select n_auto_adhpha
      from vuProdAdhpha a
      where syndic_adhpha = 1
        and dextinct_adhpha is null
    )
    and n_auto_adhpha_B2 not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202401 and 202409
        and moisok_completudepha in (0,8)
    )
  group by n_auto_adhpha_B2
),
PRECEDENT as (
  select
    n_auto_adhpha_B2,
    coalesce(sum(case when acteB2 = 'RKD' and qt_vendu_B2>0 then qt_vendu_B2 end), 0) as RKD
  from os_stat_B2
  where periode between 202301 and 202312
    and n_auto_adhpha_B2 in (
      select n_auto_adhpha
      from vuProdAdhpha a
      where syndic_adhpha = 1
        and dextinct_adhpha is null
    )
    and n_auto_adhpha_B2 not in (
      select n_auto_adhpha
      from os_completudepha
      where periode_completudepha between 202301 and 202309
        and moisok_completudepha in (0,8)
    )
  group by n_auto_adhpha_B2
)
select
  ACTU.*,
  PRECEDENT.RKD as RKD_2023,
  coalesce(
    case when PRECEDENT.RKD != 0
      then (ACTU.RKD_12 - PRECEDENT.RKD)*100.0/PRECEDENT.RKD
      else case when ACTU.RKD_12 > 0 then 10 else null end
    end
  ,0) as Evolution_RKD_12
from PHARMACIE
left join ACTU on PHARMACIE.n_auto_adhpha = ACTU.n_auto_adhpha_B2
left join PRECEDENT on PHARMACIE.n_auto_adhpha = PRECEDENT.n_auto_adhpha_B2
")

pharmaciens_data_11 <- actes_pha_11 %>%
  left_join(mail_pha_11,  by=c("n_auto_adhpha_B2"="id")) %>%
  left_join(substitution_11, by="cip") %>%
  mutate(
    SUBSTITU_12 = replace_na(SUBSTITU_12, 0),
    Toilette_12 = 1
  ) %>%
  rename(
    mail_12          = mail,
    n_auto_adhpha_B2_12 = n_auto_adhpha_B2,
    rs_adhpha_12     = rs_adhpha
  ) %>%
  distinct(cip, .keep_all=TRUE) %>%
  select(
    cip, mail_12, n_auto_adhpha_B2_12, rs_adhpha_12,
    ENTRETIENS_12, EFE_12, TRD_12, Evolution_RKD_12,
    SUBSTITU_12, Toilette_12, everys, Vente
  )


                    ##################################################################
                    ### 4. construction de la table panel_data (pour calculer moyennes)
                    ###    on joint pharmaciens_data_09 et pharmaciens_data_11 par cip
                    ##################################################################

panel_data <- pharmaciens_data_09 %>%
  left_join(pharmaciens_data_11, by="cip") %>%
  select(-mail_12, -n_auto_adhpha_B2_12, -rs_adhpha_12)

# remplacer na par 0
#panel_data[is.na(panel_data)] <- 0

                        ############################################
                        ### 5. fonction pour calculer la rémunération
                        ############################################

calculate_montant_attendu_par_mois <- function(pharmacien_row, mois="novembre") {
  # montants dans l'ordre: TRD=50,Toilette=100,ENTRETIENS=400,EFE=50,Evolution_RKD=250,SUBSTITU=100
  montants <- c(50, 100, 400, 50, 250, 100)
  if(mois=="novembre"){
    actes <- c(
      pharmacien_row$TRD_12,
      pharmacien_row$Toilette_12,
      pharmacien_row$ENTRETIENS_12,
      pharmacien_row$EFE_12,
      pharmacien_row$Evolution_RKD_12,
      pharmacien_row$SUBSTITU_12
    )
  } else {
    stop("Mois invalide : choisissez 'novembre' si on veut la campagne de décembre.")
  }
  total <- 0
  for(i in seq_along(actes)) {
    if(i==5) { 
      # i=5 => Evolution_RKD => valider si >=10
      if(actes[i]>=10) {
        total <- total + montants[i]
      }
    } else {
      if(actes[i]>0) {
        total <- total + montants[i]
      }
    }
  }
  return(total)
}

                          #############################################
                          ### 6. calcul des moyennes sur tout le panel
                          #############################################

panel_data <- panel_data %>%
  rowwise() %>%
  mutate(montant_novembre = calculate_montant_attendu_par_mois(cur_data(),"novembre")) %>%
  ungroup()

panel_moyenne_remu <- mean(panel_data$montant_novembre)
nb_panel <- nrow(panel_data)

# calcul du pourcentage de pharmacies >= moyenne
count_above_avg <- sum(panel_data$montant_novembre >= panel_moyenne_remu)
percent_above_avg <- 100 * count_above_avg / nb_panel


panel_data <- panel_data %>%
  rowwise() %>%
  mutate(
    nb_items_valides = sum(
      TRD_12>0,
      EFE_12>0,
      ENTRETIENS_12>0,
      Toilette_12>0,
      SUBSTITU_12>0,
      Evolution_RKD_12>=10)) %>%
  ungroup()
view(panel_data)
percent_item_validated <- 100 * sum(panel_data$nb_items_valides>0)/nb_panel
percent_item_validated
# pourcentages d'objectifs validés
panel_substitu  <- 100 * sum(panel_data$SUBSTITU_12   > 0)          / nb_panel
panel_trd       <- 100 * sum(panel_data$TRD_12        > 0)          / nb_panel
panel_evolrkd   <- 100 * sum(panel_data$Evolution_RKD_12 >= 10)     / nb_panel
panel_efe       <- 100 * sum(panel_data$EFE_12        > 0)          / nb_panel
panel_entretien <- 100 * sum(panel_data$ENTRETIENS_12 > 0)          / nb_panel
panel_toilette  <- 100 * sum(panel_data$Toilette_12   > 0)          / nb_panel

                          ###############################################
                          ### 7. table pour l'envoi : pharmaciens_data
                          ###############################################

pharmaciens_data <- pharmaciens_data_09 %>%
  left_join(pharmaciens_data_11, by="cip") %>%
  select(-mail_12, -n_auto_adhpha_B2_12, -rs_adhpha_12)#%>%
  #slice(1:1)
#pharmaciens_data$mail <- c("mprudhomme@ospharea.com")
#view(pharmaciens_data)
#write.xlsx(pharmaciens_data, file = "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/ROSP_2024/ta_rosp.xlsx")
                          ##################################################
                          ### 8. fonction pour afficher les cases à cocher
                          ##################################################

generate_checkmark <- function(value, is_evolution=FALSE){
  if(is_evolution){
    if(value >= 10){
      return('<td style="text-align:center; color:green; font-size:30px; border:1px solid black;">✔️</td>')
    } else {
      return('<td style="border:1px solid black;"></td>')
    }
  } else {
    if(value>0){
      return('<td style="text-align:center; color:green; font-size:30px; border:1px solid black;">✔️</td>')
    } else {
      return('<td style="border:1px solid black;"></td>')
    }
  }
}

##############################################
### 9. génération du corps du tableau html
##############################################
generate_pharmacien_rows <- function(pharmacien_row){
  # dans la dernière colonne, on met le style font-weight:bold
  rows <- paste0(
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">100 €</span> 
        Pour la substitution d’un hybride et d’un biosimilaire en 2024
        <i>(ajout du ranibizumab – arrêté du 31 octobre 2024 sur la substitution des biosimilaires).</i>
      </td>',
    generate_checkmark(pharmacien_row$SUBSTITU_12),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">', 
    round(panel_substitu,0),'%</td>
    </tr>',
    
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">50 €</span> 
        Pour la réalisation d’au moins un trod angine au sein de l’officine.
      </td>',
    generate_checkmark(pharmacien_row$TRD_12),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">',
    round(panel_trd,0),'%</td>
    </tr>',
    
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">250 €</span> 
        Si le nombre de kits de dépistage du cancer colorectal remis en officine en 2024 
        a augmenté d’au moins 10% versus 2023 sur la même période. 
        (sous réserve de la validation de l\'assurance maladie)
      </td>',
    generate_checkmark(pharmacien_row$Evolution_RKD_12,TRUE),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">',
    round(panel_evolrkd,0),'%</td>
    </tr>',
    
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">50 €</span> 
        Pour la réalisation en 2024 d’au moins un entretien femme enceinte.
      </td>',
    generate_checkmark(pharmacien_row$EFE_12),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">',
    round(panel_efe,0),'%</td>
    </tr>',
    
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">400 €</span> 
        Pour la réalisation en 2024, d’au moins un entretien auprès de patients atteints de maladies chroniques
        (asthme, anticancéreux, AOK, AOD, bilan de médication).
      </td>',
    generate_checkmark(pharmacien_row$ENTRETIENS_12),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">',
    round(panel_entretien,0),'%</td>
    </tr>',
    
    '<tr>
      <td style="border:1px solid black;">
        <span style="color:blue; font-weight:bold">100 €</span> 
        Au titre de l’aménagement des locaux ou la disposition de locaux adaptés au public en 2024
        en vue de dépister une infection urinaire.
        Ces adaptations feront l’objet d’une déclaration 
        au moyen d’un téléservice accessible depuis le portail internet de l’assurance maladie 
        dédié aux professionnels de santé **.
      </td>',
    generate_checkmark(pharmacien_row$Toilette_12),
    '<td style="text-align:center; border:1px solid black; font-weight:bold;">__</td>
    </tr>'
  )
  return(rows)
}

###############################################
### 10. envoi du mail pour chaque pharmacien
###############################################

# Créer deux data frames pour stocker les tentatives
mails_sent     <- data.frame(Email=character(), Nom=character(), stringsAsFactors=FALSE)
mails_not_sent <- data.frame(Email=character(), Nom=character(), Raison=character(), stringsAsFactors=FALSE)

send_email_to_pharmacien <- function(pharmacien_row, email, nom){
  # rémunération du pharmacien, arrondie sans virgule
  montant_attendu_novembre <- round(
    calculate_montant_attendu_par_mois(pharmacien_row,"novembre"),
    0
  )
  
  
  everys_paragraph <- if(pharmacien_row$everys==1){
    '<p style="color:black; font-weight:bold; margin-top:15px;">
      Nous attirons votre attention sur le fait que certaines données, 
      dans le cadre de cette étude, ne nous sont pas remontées par le logiciel winpharma. 
      Si des informations vous semblent erronées, 
      n\'hésitez pas à prendre contact avec nos équipes.
    </p>'
  } else {''}
  
  Carte_part <- if (pharmacien_row$Departement %in% c(971, 972, 973, 974, 976, 988, 980, 987))
  {''} else {
    '<p>
      Retrouvez la rémunération moyenne par département en cliquant sur la carte ci-dessous.
    </p>
    <div style="display: flex; justify-content:center; align-items:center;">
      <a href="https://datastat.ospharm.org/carte_article.html" target="_blank">
        <img 
          src="https://isaac-1996.github.io/Localisation/carte_2-overlay.png"
        alt="logo ospharm"
        style="width:150px; height:auto;"
        />
      </a>
    </div>' 
    
  }
  
  # arrondir la moyenne & le pourcentage
  # Taux item validé
  percent_item_val <- round(percent_item_validated,1)
  panel_moyenne_arr <- round(panel_moyenne_remu,0)
  panel_percent_arr <- round(percent_above_avg,0)
  
email_body <- paste0('
<div style="background-color:rgba(255,255,255,0.9); padding:50px; position:relative;">
  <div style="position:relative; background-color:rgba(255,255,255,0.9); padding:30px; border-radius:10px;">
    <div style="text-align:center; display:flex; justify-content:center; align-items:center; padding:40px 0;">
      <div style="background-color:#29a745; color:white; font-weight:bold; padding:25px 35px; border-radius:50px; font-size:24px; width:90%;">
        ROSP : Evaluation finale sur les rémunérations exceptionnelles 2024
      </div>
    </div>
<p>
    Chers coopérateurs,
</p>
    <p>
      <a href="https://www.ameli.fr/pharmacien/actualites/signature-de-l-avenant-1-la-convention-qui-favorise-la-lutte-contre-les-deserts-pharmaceutiques" style="color:blue;">L’avenant 1 à la convention pharmaceutique</a> signé le 10 juin dernier prévoit de 
      <strong>nouvelles rémunérations forfaitaires exceptionnelles sur l’exercice 2024</strong>.<br> 
      Cette ROSP sera versée au 1er semestre 2025 par l\'assurance maladie, 
      au regard de la facturation automatique via votre LGO.<br>
      <span style="font-weight:bold;">OSPHARM</span> vous a accompagné dans l\'atteinte du montant maximal de cette campagne <span style="font-weight:bold;">(950 €)</span>, en vous communiquant un suivi personnalisé en <span style="font-weight:bold;">octobre</span> et <span style="font-weight:bold;">décembre</span>.<br> 
      Cette campagne étant terminée, vous trouverez ci-dessous <span style="font-weight:bold;">votre récapitulatif</span> ainsi que <span style="font-weight:bold;">votre rémunération finale estimée</span>.</p>
    <p>
      <span style="color:blue; font-weight:bold; font-size:20px;">
        Pharmacie ', pharmacien_row$rs_adhpha, '
      </span>
    </p>
    <table border="1" cellpadding="5" cellspacing="0"
           style="margin-left:5%; margin-right:5%; border-collapse:separate; border-radius:20px; border:1px solid black; overflow:hidden;">
      <tr style="background-color:#29a745; border-radius:15px;">
        <th style="text-align:center; color:white; border:1px solid black; border-top-left-radius:15px;">
          Cible et rémunération
        </th>
        <th style="text-align:center; color:white; border:1px solid black;">
          Evaluation finale 2024*
        </th>
        <th style="text-align:center; color:white; border:1px solid black; border-top-right-radius:15px;">
          Pharmacies ayant validé l\'item (% du panel Ospharm)
        </th>
      </tr>',
                       generate_pharmacien_rows(pharmacien_row),
                       '</table>               
    <p style="margin-top:10px; font-size:18px; color:green; font-weight:bold;">
      Au regard de votre récapitulatif, votre rémunération estimée est de* : ', montant_attendu_novembre, '€.
    </p>
      <p style="margin-top:5px; font-size:18px; color:green; font-weight:bold;">
        99,8% des pharmacies du panel Ospharm (près de 11500 pharmacies) 
        ont eu au moins 1 item validé et la rémunération moyenne est de ', panel_moyenne_arr, ' €.
      </p>
    ', Carte_part,'
    ', everys_paragraph, '
    <p>
      <span style="font-weight:bold;">* </span>
      Les chiffres que nous communiquons sont issus des informations transmises
      depuis votre LGO à Ospharm dont la dernière transmission date du :
      <span style="font-weight:bold;">', pharmacien_row$Vente, '</span>.
      </p>
    <p>
      <span style="font-weight:bold;">** </span>
      Par défaut, cet objectif est coché sous réserve de sa réalisation 
      et de votre déclaration sur le site d\'ameli pro avant le 28 février 2025.
    </p>
    <p>
      L’ensemble de ces données ainsi que le <span style="font-weight:bold;">bilan de vos honoraires 2024</span> sont consultables directement sur votre plateforme <a href="https://datastat.ospharm.org" style="color:blue;">Ospharm Datastat</a>
      (votre identifiant :<span style="color:blue;">', email, '</span>). <br>
      Rendez-vous sur le menu "Analyse des ventes", sous-menu "Honoraires", en choisissant la période "01/01/2024 au 31/12/2024".<br>
Ces données sont également disponibles en dernière page de votre rapport mensuel de décembre adressé par mail le 21/01/2025. </p>
    <p style="color:black; font-weight:bold; font-style:italic; margin-top:20px;">
      OSPHARM, votre allié pour construire la pharmacie de demain.
    </p>
    <div style="text-align:left; margin-top:20px;">
      <img 
        src="https://isaac-1996.github.io/Localisation/03_LOGO_OSPHARM.png"
        alt="logo ospharm"
        style="width:150px; height:auto;"
      />
      <p style="margin-top:10px; color:green;">
        2 Avenue du gulf stream<br>
        44380 Pornichet<br>
        <a href="mailto:solutions@ospharea.com" style="color:blue;">solutions@ospharea.com</a><br>
        <a href="tel:+33(0)2 40 53 63 44" style="color:blue;">+33(0)2 40 53 63 44</a><br>
        <a href="https://www.ospharm.com" style="color:blue;">www.ospharm.com</a>
      </p>
    </div>
  </div>
</div>
')
  
############################################################
### 11. mail
############################################################

mailjet_api_key    <- Sys.getenv("mailjet_user")
mailjet_secret_key <- Sys.getenv("mailjet_pass")

mailjet_payload <- toJSON(
  list(
    Messages = list(
      list(
        From = list(
          Email = glob$ospharm_from,
          Name  = "OSPHARM DATASTAT"
        ),
        To = list(
          list(
            Email = email,
            Name  = nom
          )
        ),
        Subject = "OSPHARM vous accompagne - ROSP : Évaluation finale sur les rémunérations exceptionnelles 2024",
        HTMLPart = email_body
      )
    )
  ),
  auto_unbox=TRUE
)

response <- POST(
  url = "https://api.mailjet.com/v3.1/send",
  authenticate(mailjet_api_key, mailjet_secret_key),
  add_headers("Content-Type"="application/json"),
  body=mailjet_payload,
  encode="json"
)

# On va vérifier le statut pour déterminer succès ou échec
if(status_code(response)==200){
  print(paste("Email envoyé avec succès à", nom, "(", email, ")"))
  # on enregistre dans mails_sent
  mails_sent <<- rbind(
    mails_sent,
    data.frame(Email=email, Nom=nom, stringsAsFactors=FALSE)
  )
} else {
  print(paste("Erreur lors de l'envoi de l'email à", nom, ":"))
  print(content(response, as="text"))
  # on enregistre dans mails_not_sent
  mails_not_sent <<- rbind(
    mails_not_sent,
    data.frame(
      Email  = email,
      Nom    = nom,
      Raison = paste("Envoi échoué. Code:", status_code(response)),
      stringsAsFactors=FALSE
    )
  )
}
}

############################################################
### 12. BOUCLE POUR L'ENVOI À CHAQUE PHARMACIEN
############################################################
mails_total <- nrow(pharmaciens_data)

for(i in seq_len(mails_total)){
  row_pharma <- pharmaciens_data[i, ]
  email <- row_pharma$mail
  nom   <- row_pharma$rs_adhpha
  
  if(!is.na(email) && email!=""){
    send_email_to_pharmacien(row_pharma, email, nom)
  } else {
    # Email non valide
    message("Email non valide pour la ligne ", i)
    mails_not_sent <<- rbind(
      mails_not_sent,
      data.frame(
        Email  = ifelse(is.na(email),"",email),
        Nom    = nom,
        Raison = "Adresse email manquante ou vide",
        stringsAsFactors=FALSE
      )
    )
  }
}

# Une fois la boucle terminée, on affiche un récap :
cat("\n----- BILAN D'ENVOI -----\n")
cat("Nombre total de mails tentés :", mails_total, "\n")
cat("Nombre de mails envoyés avec succès :", nrow(mails_sent), "\n")
cat("Nombre de mails non envoyés :", nrow(mails_not_sent), "\n")

if(nrow(mails_not_sent) > 0){
  cat("\nDétails des mails non envoyés :\n")
  print(mails_not_sent)
}