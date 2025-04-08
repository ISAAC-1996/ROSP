
------------------- Requêtes Sandoz ---------------------
use [OSP_DATASTAT]

-- sommes Switch (sum(sellout))
select liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as name, sum(qt_vendu_artic*pfht) as sellout, sum(qt_vendu_artic) as quantity, remsandoz, remise, 
nom_adhfour as manufacturer from (select lien as liensandoz, max(remise) as remsandoz from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien  
left join os_adhfour ad on ad.n_auto_adhfour=os_labogener_artic.n_auto_adhfour where n_auto_adhpha_artic=12885
         and ((an_artic*100)+mois_artic) between 202406 and 202406
         and os_labogener_artic.n_auto_adhfour not in (401884) AND groupe_gener IN (select groupe_gener from os_gener inner join os_artic on n_auto_artic=n_auto_artic_gener where type_artic='G' and arret_artic is null) 
		 group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour, remise order by sellout desc
-- Top 10 fuites generique
select top 10 rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as Présentation, sum(qt_vendu_artic) as Quantité, sum(qt_vendu_artic*pfht) as PFHT
from (select lien as liensandoz, max(remise) as remsandoz
from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien  
left join os_adhfour ad on ad.n_auto_adhfour=os_labogener_artic.n_auto_adhfour where n_auto_adhpha_artic=12885
         and ((an_artic*100)+mois_artic) = 202501
         and os_labogener_artic.n_auto_adhfour not in (401884) AND groupe_gener IN (select groupe_gener from os_gener inner join os_artic on n_auto_artic=n_auto_artic_gener where type_artic='G' and arret_artic is null) 
		 group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour, remise order by PFHT desc

-- princeps (sum(sellout))
 select liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as name, sum(qt_vendu_artic*pfht_groupe) as sellout, sum(qt_vendu_artic) as quantity, remsandoz, nom_adhfour as manufacturer
 from (select lien as liensandoz, max(remise) as remsandoz from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
 inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien inner join os_artic on os_artic.n_auto_artic=n_auto_artic_artic inner join os_adhfour on os_adhfour.n_auto_adhfour=os_artic.adhfour 
 where n_auto_adhpha_artic=12885
         and ((an_artic*100)+mois_artic) between 202406 and 202406
         and os_labogener_artic.type_artic='P' group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour
		 order by sellout desc
-- Top 5 des princeps non substitués
 select top 5 rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))) as Présentation, nom_adhfour as Laboratoire, sum(qt_vendu_artic) as Quantité, sum(qt_vendu_artic*pfht_groupe) as PFHT
 from (select lien as liensandoz, max(remise) as remsandoz from os_labogener_artic where n_auto_adhfour=401884 and date_fin_application>getdate() group by lien)t1 
 inner join os_labogener_artic on liensandoz=lien inner join os_stat_artic on n_auto_artic=n_auto_artic_artic inner join os_grpgener_artic on os_labogener_artic.lien=os_grpgener_artic.lien inner join os_artic on os_artic.n_auto_artic=n_auto_artic_artic inner join os_adhfour on os_adhfour.n_auto_adhfour=os_artic.adhfour 
 where n_auto_adhpha_artic=12885
         and ((an_artic*100)+mois_artic) = 202501
         and os_labogener_artic.type_artic='P' group by liensandoz, rtrim(concat(rtrim(libelle_court_groupe),' ',rtrim(conditionnement),' ',rtrim(volume),' ',rtrim(unite))), remsandoz, nom_adhfour
		 order by PFHT desc

 

 ---- Biosimilaire 
 select PRESENTATION, LABO, QTE, PFHT from
(select concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci))  as PRESENTATION,case when adhfour=401884 then 'SANDOZ'
else 'Autres' end as LABO, sum(qt_vendu_lcollect) as QTE,
SUM(qt_vendu_lcollect * P_Fab_HT) as PFHT
from v_el_collect_ospharm
inner join os_gener on n_auto_artic_lcollect=n_auto_artic_gener
inner join os_dci on groupe_gener=groupe_dci
inner join os_artic on n_auto_artic_lcollect=n_auto_artic
inner join os_adhfour on adhfour=n_auto_adhfour
left join OSP_PROD.dbo.CEPS_Prix on n_auto_artic_lcollect = id_sql  and actif = 1
where n_auto_adhpha_ecollect=178
and type_artic in ('R','B')
and groupe_dci in (select groupe_gener from os_grpgener_artic where libelle_court_groupe like 'filgrastim%' or libelle_court_groupe like 'pegfilgrastim%')
and periode = 202501 
group by concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)),case when adhfour=401884 then 'SANDOZ'
else 'Autres' end) t1
left join
(select concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) as P, max(code_ean) as EAN from os_ean
inner join os_gener on n_auto_artic_ean=n_auto_artic_gener
inner join os_dci on groupe_gener=groupe_dci
where type_code='1'
and n_auto_artic_ean in (select n_auto_artic from os_artic where adhfour=401884 and type_artic in ('R','B') and arret_artic is null) group by concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci))) t2
on t1.PRESENTATION=t2.P
order by 1 asc

-- Biosimilaire
WITH ProduitsMolecules AS (SELECT 'ZARZIO (FILGRASTIM)' as PRESENTATION, 'SANDOZ' as LABO, 'FILGRASTIM' as BASE_MOLECULE
    UNION ALL SELECT 'FILGRASTIM', 'AUTRES', 'FILGRASTIM'
    UNION ALL SELECT 'ZIEXTENZO (PEGFILGRASTIM)', 'SANDOZ', 'PEGFILGRASTIM'
    UNION ALL SELECT 'PEGFILGRASTIM', 'AUTRES', 'PEGFILGRASTIM'),
Ventes AS (SELECT CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%' AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZARZIO (FILGRASTIM)' ELSE 'FILGRASTIM' END
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZIEXTENZO (PEGFILGRASTIM)'ELSE 'PEGFILGRASTIM' END END as PRESENTATION,
        CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END as LABO,
        SUM(qt_vendu_lcollect) as QTE,
        SUM(qt_vendu_lcollect * P_Fab_HT) as PFHT
    FROM v_el_collect_ospharm
    INNER JOIN os_gener ON n_auto_artic_lcollect=n_auto_artic_gener
    INNER JOIN os_dci ON groupe_gener=groupe_dci
    INNER JOIN os_artic ON n_auto_artic_lcollect=n_auto_artic
    INNER JOIN os_adhfour ON adhfour=n_auto_adhfour
    LEFT JOIN OSP_PROD.dbo.CEPS_Prix ON n_auto_artic_lcollect = id_sql AND actif = 1
    WHERE n_auto_adhpha_ecollect=178
    AND type_artic IN ('R','B')
    AND groupe_dci IN (
        SELECT groupe_gener 
        FROM os_grpgener_artic 
        WHERE libelle_court_groupe LIKE 'filgrastim%' 
        OR libelle_court_groupe LIKE 'pegfilgrastim%') AND periode = 202501
    GROUP BY CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%' AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZARZIO (FILGRASTIM)' ELSE 'FILGRASTIM' END
            WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' 
                THEN CASE WHEN adhfour = 401884 THEN 'ZIEXTENZO (PEGFILGRASTIM)' ELSE 'PEGFILGRASTIM' END END,
        CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END)
SELECT pm.PRESENTATION, pm.LABO,
    COALESCE(v.QTE, 0) as QTE,
    COALESCE(v.PFHT, 0) as PFHT,
    CAST(CASE WHEN SUM(COALESCE(v.PFHT, 0)) OVER (PARTITION BY pm.BASE_MOLECULE) = 0 THEN 0 ELSE ROUND(COALESCE(v.PFHT, 0) * 100.0 / SUM(COALESCE(v.PFHT, 0)) OVER (PARTITION BY pm.BASE_MOLECULE), 2)END AS DECIMAL(10,2)) as PDM_PFHT
FROM ProduitsMolecules pm
	LEFT JOIN Ventes v ON pm.PRESENTATION = v.PRESENTATION AND pm.LABO = v.LABO
ORDER BY CASE WHEN pm.PRESENTATION LIKE '%FILGRASTIM%' AND pm.PRESENTATION NOT LIKE '%PEG%' THEN 1
        WHEN pm.PRESENTATION LIKE '%PEGFILGRASTIM%' THEN 2 END, pm.PRESENTATION, pm.LABO DESC


-- Sandoz 406
select n_auto_adhpha, mail from vuProdAdhpha 
left join ospharea_adherents on finess_adhpha = finess 
where dextinct_adhpha is null 
and n_auto_adhpha not in (select n_auto_adhpha from os_completudepha where periode_completudepha = 202501 and moisok_completudepha in (0,8))
and n_auto_adhpha in (select n_auto_adhpha from os_grp_adhpha where n_auto_adhgrp = 406)



---- Rajout des autres biosimilaires (Epoetine Etanercept Adalimumab Enoxaparine)
WITH 
BaseMols AS (
  SELECT 'FILGRASTIM'    AS base_mol
  UNION ALL SELECT 'PEGFILGRASTIM'
  UNION ALL SELECT 'EPOETINE'
  UNION ALL SELECT 'ETANERCEPT'
  UNION ALL SELECT 'ADALIMUMAB'
  UNION ALL SELECT 'ENOXAPARINE'
),
Ventes AS (
  SELECT
    CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%FILGRASTIM%%'
           AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%%PEG%%'
        THEN 'FILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%PEGFILGRASTIM%%'
        THEN 'PEGFILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%EPOETINE%%'
        THEN 'EPOETINE'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ETANERCEPT%%'
        THEN 'ETANERCEPT'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ADALIMUMAB%%'
        THEN 'ADALIMUMAB'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ENOXAPARINE%%'
        THEN 'ENOXAPARINE'
      ELSE NULL
    END AS base_mol,
    CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END AS labo,
    SUM(qt_vendu_lcollect)              AS Qte,
    SUM(qt_vendu_lcollect * P_Fab_HT)   AS PFHT
  FROM v_el_collect_ospharm
  INNER JOIN os_gener   ON n_auto_artic_lcollect = n_auto_artic_gener
  INNER JOIN os_dci     ON groupe_gener = groupe_dci
  INNER JOIN os_artic   ON n_auto_artic_lcollect = n_auto_artic
  INNER JOIN os_adhfour ON adhfour = n_auto_adhfour
  LEFT JOIN OSP_PROD.dbo.CEPS_Prix 
         ON n_auto_artic_lcollect = id_sql 
        AND actif = 1
  WHERE n_auto_adhpha_ecollect = 178
    AND type_artic IN ('R','B')
    AND groupe_dci IN (
      SELECT groupe_gener 
      FROM os_grpgener_artic 
      WHERE libelle_court_groupe LIKE 'filgrastim%%'
         OR libelle_court_groupe LIKE 'pegfilgrastim%%'
         OR libelle_court_groupe LIKE 'epoetine%%'
         OR libelle_court_groupe LIKE 'etanercept%%'
         OR libelle_court_groupe LIKE 'adalimumab%%'
         OR libelle_court_groupe LIKE 'enoxaparine%%') AND periode = 202502
  GROUP BY
    CASE
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%FILGRASTIM%%'
           AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%%PEG%%'
        THEN 'FILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%PEGFILGRASTIM%%'
        THEN 'PEGFILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%EPOETINE%%'
        THEN 'EPOETINE'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ETANERCEPT%%'
        THEN 'ETANERCEPT'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ADALIMUMAB%%'
        THEN 'ADALIMUMAB'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%%ENOXAPARINE%%'
        THEN 'ENOXAPARINE'
      ELSE NULL
    END,
    CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END
),
Pivoted AS (
  SELECT
    v.base_mol,
    SUM(CASE WHEN v.labo='SANDOZ' THEN v.Qte  ELSE 0 END) AS QteSandoz,
    SUM(CASE WHEN v.labo='SANDOZ' THEN v.PFHT ELSE 0 END)AS PFHTSandoz,
    SUM(CASE WHEN v.labo='AUTRES' THEN v.Qte  ELSE 0 END) AS QteAutres,
    SUM(CASE WHEN v.labo='AUTRES' THEN v.PFHT ELSE 0 END)AS PFHTAutres
  FROM Ventes v
  WHERE v.base_mol IS NOT NULL  -- on ignore tout ce qui n'est pas dans nos 6 molécules
  GROUP BY v.base_mol),
ResultsWithOrder AS (
  SELECT
    CASE b.base_mol
      WHEN 'FILGRASTIM'    THEN 'FILGRASTIM(ZARZIO)'
      WHEN 'PEGFILGRASTIM' THEN 'PEGFILGRASTIM(ZIEXTENZO)'
      WHEN 'EPOETINE'      THEN 'EPOETINE(BINOCRIT)'
      WHEN 'ETANERCEPT'    THEN 'ETANERCEPT(ERELZI)'
      WHEN 'ADALIMUMAB'    THEN 'ADALIMUMAB(AMGEVITA)'
      WHEN 'ENOXAPARINE'   THEN 'ENOXAPARINE(BECAT)'
      ELSE 'INCONNU'
    END AS [Molécules],
    COALESCE(p.QteSandoz,   0) AS [Quantité Sandoz],
    COALESCE(p.QteAutres,   0) AS [Quantité autres laboratoires],
    COALESCE(p.PFHTSandoz,  0) AS [PFHT Sandoz],
    COALESCE(p.PFHTAutres,  0) AS [PFHT autres laboratoires],
    CAST(CASE WHEN COALESCE(p.PFHTSandoz,0) + COALESCE(p.PFHTAutres,0) = 0 THEN 0
        ELSE ROUND(
          100.0 * COALESCE(p.PFHTSandoz,0)/ (COALESCE(p.PFHTSandoz,0)+COALESCE(p.PFHTAutres,0)),2)END AS DECIMAL(10,2)) AS [PDM PFHT],
    0 AS [Order],
    CASE 
      WHEN b.base_mol='FILGRASTIM'     THEN 1
      WHEN b.base_mol='PEGFILGRASTIM'  THEN 2
      WHEN b.base_mol='EPOETINE'       THEN 3
      WHEN b.base_mol='ETANERCEPT'     THEN 4
      WHEN b.base_mol='ADALIMUMAB'     THEN 5
      WHEN b.base_mol='ENOXAPARINE'    THEN 6
      ELSE 999 
    END AS [SortOrder]
  FROM BaseMols b
  LEFT JOIN Pivoted p ON p.base_mol = b.base_mol
  UNION ALL
  SELECT
    'TOTAL' AS [Molécules],
    SUM(COALESCE(p.QteSandoz, 0)) AS [Quantité Sandoz],
    SUM(COALESCE(p.QteAutres, 0)) AS [Quantité autres laboratoires],
    SUM(COALESCE(p.PFHTSandoz, 0)) AS [PFHT Sandoz],
    SUM(COALESCE(p.PFHTAutres, 0)) AS [PFHT autres laboratoires],
    CAST(CASE WHEN SUM(COALESCE(p.PFHTSandoz,0)) + SUM(COALESCE(p.PFHTAutres,0)) = 0 THEN 0
        ELSE ROUND(
          100.0 * SUM(COALESCE(p.PFHTSandoz,0))/ (SUM(COALESCE(p.PFHTSandoz,0))+SUM(COALESCE(p.PFHTAutres,0))),2)END AS DECIMAL(10,2)) AS [PDM PFHT],
    1 AS [Order],
    999 AS [SortOrder]
  FROM BaseMols b
  LEFT JOIN Pivoted p ON p.base_mol = b.base_mol
)
SELECT 
  [Molécules],
  [Quantité Sandoz],
  [Quantité autres laboratoires],
  [PFHT Sandoz],
  [PFHT autres laboratoires],
  [PDM PFHT]
FROM ResultsWithOrder
ORDER BY [Order], [SortOrder]


















-- Marger biosdimliaire 
DECLARE @TauxMarge DECIMAL(10,2) = 25.0; -- Je dois remplacé cette partie par la remise personalisée que Sandoz applique sur chaque vente de molécule au pharmacien.
WITH BaseMols AS (
  SELECT 'FILGRASTIM'    AS base_mol
  UNION ALL SELECT 'PEGFILGRASTIM'
  UNION ALL SELECT 'EPOETINE'
  UNION ALL SELECT 'ETANERCEPT'
  UNION ALL SELECT 'ADALIMUMAB'
  UNION ALL SELECT 'ENOXAPARINE'),
Ventes AS (SELECT CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%'
AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' THEN 'FILGRASTIM' 
		WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' THEN 'PEGFILGRASTIM'
        WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%EPOETINE%' THEN 'EPOETINE'
		WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ETANERCEPT%' THEN 'ETANERCEPT'
		WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ADALIMUMAB%' THEN 'ADALIMUMAB'
		WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ENOXAPARINE%' THEN 'ENOXAPARINE'
		ELSE NULL END AS base_mol,
    CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END AS labo,
    SUM(qt_vendu_lcollect)              AS Qte,
    SUM(qt_vendu_lcollect * P_Fab_HT)   AS PFHT
  FROM v_el_collect_ospharm
  INNER JOIN os_gener   ON n_auto_artic_lcollect = n_auto_artic_gener
  INNER JOIN os_dci     ON groupe_gener = groupe_dci
  INNER JOIN os_artic   ON n_auto_artic_lcollect = n_auto_artic
  INNER JOIN os_adhfour ON adhfour = n_auto_adhfour
  LEFT JOIN OSP_PROD.dbo.CEPS_Prix 
         ON n_auto_artic_lcollect = id_sql 
        AND actif = 1
  WHERE n_auto_adhpha_ecollect = 178
    AND type_artic IN ('R','B')
    AND groupe_dci IN (
      SELECT groupe_gener 
      FROM os_grpgener_artic 
      WHERE libelle_court_groupe LIKE 'filgrastim%'
         OR libelle_court_groupe LIKE 'pegfilgrastim%'
         OR libelle_court_groupe LIKE 'epoetine%'
         OR libelle_court_groupe LIKE 'etanercept%'
         OR libelle_court_groupe LIKE 'adalimumab%'
         OR libelle_court_groupe LIKE 'enoxaparine%') 
    AND periode = 202502
  GROUP BY CASE WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%FILGRASTIM%' AND concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) NOT LIKE '%PEG%' THEN 'FILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%PEGFILGRASTIM%' THEN 'PEGFILGRASTIM'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%EPOETINE%' THEN 'EPOETINE'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ETANERCEPT%' THEN 'ETANERCEPT'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ADALIMUMAB%' THEN 'ADALIMUMAB'
      WHEN concat(rtrim(lib_dci),' ',rtrim(dosage_dci),' ',rtrim(forme_dci)) LIKE '%ENOXAPARINE%'THEN 'ENOXAPARINE' ELSE NULL END,
    CASE WHEN adhfour=401884 THEN 'SANDOZ' ELSE 'AUTRES' END),

Calcul_Marge AS (SELECT SUM(CASE WHEN labo='SANDOZ' THEN PFHT ELSE 0 END) AS PFHT_Sandoz_Actuel,
    SUM(PFHT) AS PFHT_Total,
    @TauxMarge AS Taux_Marge_Pct
  FROM Ventes
  WHERE base_mol IS NOT NULL)
SELECT PFHT_Total * 0.5 AS PFHT_Objectif_50Pct,
  PFHT_Sandoz_Actuel AS PFHT_Sandoz_Actuel,
  CASE WHEN PFHT_Total * 0.5 > PFHT_Sandoz_Actuel
    THEN PFHT_Total * 0.5 - PFHT_Sandoz_Actuel ELSE 0 END AS PFHT_Supplementaire_Necessaire,
  -- Calcul de la marge supplémentaire pour atteindre les 50% de PDM PFHT
  CASE WHEN PFHT_Total * 0.5 > PFHT_Sandoz_Actuel THEN (PFHT_Total * 0.5 - PFHT_Sandoz_Actuel) * (Taux_Marge_Pct/100)
    ELSE 0 END AS Marge_Supplementaire,
  -- les infos de mon pharmacien
  CAST(CASE WHEN PFHT_Total = 0 THEN 0 ELSE ROUND(100.0 * PFHT_Sandoz_Actuel / PFHT_Total, 2)
  END AS DECIMAL(10,2)) AS PDM_Actuelle_Pct,
  '202502' AS Periode,
  '178' AS ID_Client
FROM Calcul_Marge



