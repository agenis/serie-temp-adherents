
#################################################################
# Series temporelles : dynamique d’adhesions a un parti politique
#################################################################


# import et preparation des donnees
###################################

# dates election en france 
# premier tour seulement, la derniere etant americaine. on met aussi le brexit
elections = as.Date(c("2009-06-07", "2010-03-14", "2012-04-22", "2012-06-10", "2014-05-25", "2015-12-06", "2016-06-23", "2016-12-08"))

# donnees journalieres
dfj <- import("upr_adherents_journalier_modifie_31012017.csv", stringsAsFactors=FALSE)
dfj %<>% dplyr::select(date=jour, cum=nb_total) %>%
  mutate(date=as.Date(date, format="%d/%m/%Y"), nb=c(0, diff(cum))) %>%
  CreateCalendarVariables(id_column=1, detailed=T) %>%
  mutate(jour.t0=seq_along(cum)) %>%
  mutate(ete=.mois %in% 6:8)

# donnees horaires 
dfh = import("upr_adherents_horaire.csv") %>% dplyr::select(-1)
names(dfh) = c("cumul", "datetime")
dfh$datetime <- dfh$datetime %>% strptime(format="%d/%m/%Y %H:%M") %>% as.POSIXct()
# les donnees sont vraimet horaires a partir de fin 2015.
dfh <- dfh %>% arrange(datetime) %>% filter(datetime >= "2015-11-28 00:00:00")
# On egalement exclut la periode electorale qui est irreguliere
dfh <- dfh %>% filter(datetime > "2016-01-01 00:00:00")
# les dates sans valeurs sont manquantes. On les creer et on imputation par le precedent
dfh <- seq.POSIXt(from=dfh$datetime[1], to=dfh$datetime[NROW(dfh)], by="hour") %>%
  data.frame('datetime'=.) %>% left_join(dfh)
dfh$cumul <- na.locf(dfh$cumul)
# creation de la serie temporelle
dfh$cumul <- ts(dfh$cumul, start=1, freq=24)
dfh$adherents <- c(0, diff(dfh$cumul))
dfh %<>% CreateCalendarVariables(1, detailed=TRUE)
# creation d'une variable de comptage des jours a partir du debut
temp <- (diff(dfh$.heure_num)<0) %>% cumsum %>% '+'(1)
dfh['jour.t0'] = c(temp, tail(temp, 1))
# outliers - ne pas le faire sauf pour le calage de distribution
dfh$adherents[dfh$adherents>10] <- 0
dfh$adherents[dfh$adherents< 0] <- 0
dfh$adherents[is.na(dfh$adherents)] <- 0
# homogeneiser les noms
names(dfh)[1:3] <- c("date", "cum", "nb")

# statistiques google trends
# on peut rejouer le webscrapping avec le script suivant:
# source("script_webscrapping_trends.R")
# attention: a cause de pb de quotas il faut le jouer pas a pas.
# donnees deja preparees:
load(file="trend.asselineau.Rdata")
load(file="trend.upr.Rdata")
# les deux series Gtrends sont calees en minmax 1-100, il faut les mettre a l'echelle:
# ecart global de popularite entre les deux termes
browseURL(url="https://www.google.fr/trends/explore?date=2007-10-01%202017-02-10&geo=FR&q=asselineau,upr")
# ratio 52/37 en faveur de upr pour janvier 2017. Correction:
dfj['score.upr']        <- res.upr$score.upr*52/57
dfj['score.asselineau'] <- res.asselineau$hits*37/92

