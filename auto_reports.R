########################## Corps du mail et automatisation via R ###################
# Définir le chemin vers Pandoc - AJOUTER CES LIGNES ICI
#Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/quarto/bin/tools")
# Si la ligne ci-dessus ne fonctionne pas, essayez plutôt celle-ci :
 Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools")

source("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/reports/connexion_config.R")

################################################################################
# 2) Définir le calcul du jour d'envoi : le 10 (ou ici 18 pour test) 
#    + gestion du week-end et jours fériés
################################################################################

jours_feries_2025 <- as.Date(c(
  "2025-01-01",
  "2025-04-21",
  "2025-05-01",
  "2025-05-08",
  "2025-05-29",
  "2025-07-14",
  "2025-08-15",
  "2025-11-01",
  "2025-11-11",
  "2025-12-25"
))

jour_envoi_mensuel <- function(an = year(Sys.Date()), mo = month(Sys.Date())) {
  # ICI, on a mis 18 pour tester, remettez 10 pour votre version finale
  date_theorique <- as.Date(sprintf("%04d-%02d-20", an, mo))
  
  while (weekdays(date_theorique) %in% c("samedi", "dimanche") ||
         date_theorique %in% jours_feries_2025) {
    date_theorique <- date_theorique + 1
  }
  return(date_theorique)
}

jour_envoi <- jour_envoi_mensuel()
message("Le jour d'envoi mensuel calculé est : ", as.character(jour_envoi))

################################################################################
# 3) Vérifier si nous sommes bien ce jour d'envoi
################################################################################

if (Sys.Date() == jour_envoi) {
  message("==> Nous sommes le bon jour d'envoi ! Début du traitement...")
  
  # Définition du répertoire de travail
  setwd("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/reports/")
  
  options(scipen = 999)  
  
  # (Optionnel) charger d'autres scripts si besoin
  # Par ex. un dossier "Environment" :
  list.files(
    path       = "../../Utils/sandbox/Environment",
    pattern    = "*.R",
    full.names = TRUE
  ) |>
    lapply(source, encoding = "UTF-8") |>
    invisible()
  
  ##############################################################################
  # 3) Récupération de la période & requête SQL pour la liste des pharmacies
  ##############################################################################
  
  # Ici, `mois_precedent()` est déjà chargé depuis connexion_config.R
  date_   <- mois_precedent("sql")
  periode <- date_
  message("Période calculée (SQL) : ", periode)
  
  sql_req <- sprintf("
    SELECT n_auto_adhpha, mail
    FROM vuProdAdhpha 
    LEFT JOIN ospharea_adherents ON finess_adhpha = finess
    WHERE dextinct_adhpha IS NULL
      AND n_auto_adhpha NOT IN (
        SELECT n_auto_adhpha
        FROM os_completudepha
        WHERE periode_completudepha = %s
          AND moisok_completudepha IN (0,8)
      )
      AND n_auto_adhpha IN (
        SELECT n_auto_adhpha
        FROM os_grp_adhpha
        WHERE n_auto_adhgrp = 406
      )
    ORDER BY mail;
  ", periode)
  
  df_pharmacies <- dbGetQuery(con, sql_req)%>%
    filter(n_auto_adhpha == 12222)%>%
    view()
  
  # Pour test, on modifie l'email
  df_pharmacies$mail <-  "biama@ospharea.com" #,c("biama@ospharea.com", "adouchin@ospharea.com", "mprudhomme@ospharea.com") 
  df_pharmacies
  message("Pharmacies à traiter : ", nrow(df_pharmacies))
  ##############################################################################
  # 4) Fonction de génération et envoi de rapport
  ##############################################################################
  
  genererEtEnvoyerRapport <- function(id_pharma, email_pharma, con) {
    # -- 1) Calculs / extraction de données
    maPharma    <- getPharmacieInfo(id_pharma)
    monMois     <- mois_precedent() 
    top_10_data <- top_10_gener(id_pharma, con)
    Lancement   <- calculer_lancements(id_pharma, con)
    top_5_data  <- top_5(id_pharma, con)
    bio         <- Bisomilaire(id_pharma, con)
    s_switch    <- Somme_switch(id_pharma, con)
    s_princeps  <- Somme_princeps(id_pharma, con)
    marge       <- s_switch + s_princeps
    
    # -- 2) Rendre le RMarkdown en HTML puis convertir en PDF
    html_file <- paste0("Rapport_", id_pharma, "_", monMois, ".html")
    pdf_file  <- paste0("Rapport_", id_pharma, "_", monMois, ".pdf")
    
    rmarkdown::render(
      input       = "test.Rmd",
      output_file = html_file,
      params      = list(
        pharmacie      = maPharma,
        mois_annee     = monMois,
        switch_total   = s_switch,
        princeps_total = s_princeps,
        marge          = marge,
        top_10         = top_10_data,
        top_5          = top_5_data,
        lancement      = Lancement,
        bio            = bio
      )
    )
    
    pagedown::chrome_print(
      input   = html_file,
      output  = pdf_file,
      options = list(
        format = "A4",
        scale = 0.95,
        marginTop           = 0, 
        marginBottom        = 0,
        marginRight         = 0,
        marginLeft          = 0,
        printBackground     = TRUE,
        preferCSSPageSize   = TRUE, 
        displayHeaderFooter = FALSE
      ),
      timeout = 60
    )
    
  ########## Email body #############
email_body <- paste0('
          <p>Bonjour,</p>
          <p> Veuillez trouver en pièce jointe votre rapport de ' ,monMois ,'. </p>
          <p style="color:black; font-weight:bold; font-style:italic; margin-top: 20px;">Ospharm, votre allié pour construire la pharmacie de demain.</p>
 
<div style="text-align:left; margin-top: 20px;">
      <a href="https://www.ospharm.com/" target="_blank" style="text-decoration: none;"><img 
        src="https://isaac-1996.github.io/Localisation/03_LOGO_OSPHARM.png"
        alt="logo ospharm"
        style="width:150px; height:auto;"
      /></a>
       
  <p style="margin-top: 10px; color:green;">
    2 Avenue du Gulf Stream<br>
    44380 Pornichet<br>
    <a href="mailto:solutions@ospharea.com" style="color:blue;">solutions@ospharea.com</a><br>
    <a href="tel:+33(0)2 40 53 63 44" style="color:blue;">+33(0)2 40 53 63 44</a><br>
    <a href="https://www.ospharm.com" style="color:blue;">www.ospharm.com</a>
  </p>
</div>
  ')
    
    # -- 3) Envoi de l'email avec la PJ PDF
    send.mail(
      from         = "Coopérative OSPHARM<solutions@ospharea.com>",
      to           = email_pharma,
      subject      = paste("Votre rapport mensuel du groupement Sandoz", maPharma),
      body         = email_body,
      html         = TRUE,  # indispensable pour que le body soit traité comme HTML
      smtp         = list(
        host.name = Sys.getenv("mailjet_host"),
        port      = Sys.getenv("mailjet_port"),
        user.name = Sys.getenv("mailjet_user"),
        passwd    = Sys.getenv("mailjet_pass"),
        ssl       = TRUE
      ),
      attach.files  = pdf_file,
      authenticate  = TRUE,
      send          = TRUE
    )
    
    return(TRUE)
  }
  
  ##############################################################################
  # 5) Boucle d’envoi + capture des statuts
  ##############################################################################
  
  report_results <- data.frame(
    id_pharma    = integer(),
    email_pharma = character(),
    status       = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(nrow(df_pharmacies))) {
    id_pharma    <- df_pharmacies$n_auto_adhpha[i]
    email_pharma <- df_pharmacies$mail[i]
    
    cat("Traitement en cours pour ID =", id_pharma, "| Email =", email_pharma, "\n")
    
    status_result <- tryCatch(
      {
        genererEtEnvoyerRapport(id_pharma, email_pharma, con)
        cat("OK - Rapport généré et envoyé.\n")
        "OK"
      },
      error = function(e) {
        cat("ERREUR pour ID =", id_pharma, ":", e$message, "\n")
        paste("ERREUR:", e$message)
      }
    )
    
    report_results <- rbind(
      report_results,
      data.frame(
        id_pharma    = id_pharma,
        email_pharma = email_pharma,
        status       = status_result,
        stringsAsFactors = FALSE
      )
    )
  }
  
  ##############################################################################
  # 6) Envoyer un bilan à l'administrateur
  ##############################################################################
  
  num_ok  <- sum(report_results$status == "OK")
  num_err <- sum(grepl("ERREUR", report_results$status))
  errors_only <- subset(report_results, grepl("ERREUR", status))
  
  bilan_text <- paste0(
    "Bonjour,\n\n",
    "Bilan de l'envoi du ", Sys.Date(), " :\n",
    " - Envoyés OK : ", num_ok, "\n",
    " - En erreurs : ", num_err, "\n\n",
    "Détails des erreurs :\n"
  )
 
  if (nrow(errors_only) == 0) {
    bilan_text <- paste0(bilan_text, "Aucune erreur rencontrée.\n")
  } else {
    # Sinon, on liste les erreurs
    for (j in seq_len(nrow(errors_only))) {
      bilan_text <- paste0(
        bilan_text,
        "ID=", errors_only$id_pharma[j], " | ",
        "Email=", errors_only$email_pharma[j], " | ",
        "Statut=", errors_only$status[j], "\n"
      )
    }
  }
  
  # Envoi du mail de bilan
  send.mail(
    from         = "Coopérative OSPHARM<solutions@ospharea.com>",
    to           = "biama@ospharea.com",  # Admin
    subject      = paste("Bilan d'envoi -", Sys.Date()),
    body         = bilan_text,
    smtp         = list(
      host.name = Sys.getenv("mailjet_host"),
      port      = Sys.getenv("mailjet_port"),
      user.name = Sys.getenv("mailjet_user"),
      passwd    = Sys.getenv("mailjet_pass"),
      ssl       = TRUE
    ),
    authenticate = TRUE,
    send         = TRUE
  )
  
  message("==> Traitement terminé, bilan envoyé à l'administrateur.")
  
} else {
  # 7) Si nous ne sommes pas le bon jour
  message("Nous ne sommes PAS le bon jour d'envoi (le 10 ou jour ouvrable suivant). Fin du script.")
}
