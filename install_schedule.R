library(taskscheduleR)

script_path <- "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Projet/reports/auto_reports.R"
taskscheduler_create(
  taskname   = "TestEnvoiRapportsOnce", 
  rscript    = script_path,
  schedule   = "MONTHLY",
  starttime  = "12:13",
  startdate  = format(Sys.Date(), "%d/%m/%Y"),
  days = 20
)


# Pour supprimer la tache si on ne veut plus automatiser 
#taskscheduler_delete("TestEnvoiRapportsOnce")
#taskscheduler_ls()  


#ONCE : Exécute le script une seule fois à la date et l'heure spécifiées
#DAILY : Exécute le script tous les jours à l'heure spécifiée
#WEEKLY : Exécute le script une fois par semaine (vous devrez spécifier le jour)
#MONTHLY : Exécute le script une fois par mois (vous pouvez spécifier le jour du mois)
#YEARLY : Exécute le script une fois par an (vous devrez spécifier le jour et le mois)