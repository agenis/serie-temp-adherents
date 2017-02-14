
#######################################
# FONCTIONS A SOURCER

# Ces fonctions sont egalement disponibles dans le projet "mes_fonctions_utiles" ici:
# browseURL(url="https://gitlab.com/agenis/mes-fonctions-utiles/blob/master/mes_fonctions_utiles.R")


# fonction specifique ici pour estimer les quantiles
QuantileFunc = function(seuil) dfj %>% dplyr::select(date, cum, nb) %>% arrange(desc(date)) %>% mutate(prop=cum/head(cum, 1)) %>%
  filter(almost.equal(prop, seuil, tolerance=0.001))


# Fonction pour importer un csv classique point virgule, decimale (systeme francais). Peut importer automatiquement du bureau.
import = function(file, desktop=FALSE, ...) { # entre guillemets
  path <- file
  if (desktop) {
    path <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop/", file)
  }
  return(read.csv2(path, sep=";", dec=",", header=TRUE, ...)) 
}

# fonction qui ajoute a un tableau des colonnes de variables calendaires pratiques pour la modelisation
# temporelle. Detailed FALSE donne les formats pratiques par defaut, numerique ou facteur selon le cas,
# detailed TRUE donne l'un et l'autre.
CreateCalendarVariables = function(df, id_column=NULL, detailed=FALSE) {
  if (  !("data.frame" %in% class(df))  ){
    message("la donnee en entree a ete convertie en data.frame")
    df <- data.frame(df)
  }
  if (is.null(id_column)) stop("L'argument id_column est obligatoire")
  temp <- df[, id_column]
  if (  !(class(temp)[1] %in% c("Date", "POSIXct", "POSIXt", "POSIXlt"))  ){
    stop("la colonne de dates n'est pas de classe appropriee")
  }
  df['.annee']      <- year(temp)
  df['.trimestre']  <- quarter(temp)
  df['.mois']       <- month(temp)
  df['.semaine']    <- week(temp)
  df['.JMA']        <- as.Date(temp)
  df['.jouran']     <- yday(temp)
  df['.jourmois']   <- mday(temp)
  df['.joursem']    <- wday(temp, label=T, abbr=FALSE) %>% factor(., levels=levels(.)[c(2,3,4,5,6,7,1)])
  df['.is_we']      <- df$.joursem %in% c("Saturday", "Sunday")
  if(class(temp)[1] != "Date"){
    df['.heure']    <- factor(hour(temp))
  }
  if(detailed==TRUE){
    df['.annee_fact']      <- factor(df$.annee)
    df['.trimestre_fact']  <- factor(df$.trimestre)
    df['.mois_fact']       <- factor(df$.mois)
    df['.semaine_fact']    <- factor(df$.semaine)
    df['.jouran_fact']     <- factor(df$.jouran)
    df['.jourmois_fact']   <- factor(df$.jourmois)
    df['.joursem_num']     <- as.numeric(df$.joursem)
    if(class(temp)[1] != "Date"){
      df['.heure_num']    <- as.numeric(df$.heure)
    }
  }
  return(df)
}

# Fonction pour multiplier par 100 et garder DEUX decimales (pour les resultats en pourcentage dnas les rapports)
pc2 = function(vector) {
  return(round(vector*100, 2))
}


# Fonction pour afficher une matrice sous forme d'image, **dans le bon sens!**
Show = function(df, ...) {image(t(df[nrow(df):1,]), ...)}


# fonction pour verifier si deux nombres sont quasiument egaux (a la precision machine pres)
# typiquement pour verifier l'egalite entre un nombre et le meme nombre issu d'un format charactere
almost.equal <- function (x, y, tolerance=.Machine$double.eps^0.5,
                          na.value=TRUE)
{
  answer <- rep(na.value, length(x))
  test <- !is.na(x)
  answer[test] <- abs(x[test] - y) < tolerance
  answer
}


# calcule plusieurs types d'erreur de prediction
PredError = function(actual, predicted, type="RMSE", season, ...){
  temp <- na.omit(data.frame(actual, predicted))
  # use the {...} to specify na.rm=TRUE or weightings of means and sums
  # sometimes we force to NA the observations where only one of the vectors has NA.
  # measures return single value except squared error
  if (type %in% c("SE", "SD")){
    # Squared Error
    output <- (actual - predicted)^2
  } else if (type %in% c("MSE", "MSD")){
    # Mean Squared Error
    output <- mean( (actual - predicted)^2, ... )
  } else if (type=="RMSE"){
    # Root Mean Squared Error
    output <- sqrt(   mean( (actual - predicted)^2, ... )   )
  } else if (type=="CV-RMSE"){
    # Coef Variaiton of the RMSE
    output <- sqrt(   mean( (actual - predicted)^2, ... )   )/mean(actual, ...)
  } else if (type=="MBE"){
    # Mean Biased Error
    output <- mean( (predicted - actual), ... )
  } else if (type %in% c("MAE", "MAD")){
    # Mean Absolute Error
    output <- mean( abs(actual - predicted), ... )
  } else if (type=="MCE"){
    # Mean Cubic Error
    output <- mean( (actual - predicted)^3, ... )
  } else if (type=="MACE"){
    # Mean Absolute Cubic Error
    output <- mean(   abs( (actual - predicted)^3 ), ...   )
  } else if (type=="MPE"){
    # Mean Percentage error
    output <- mean( (actual - predicted)/actual, ... )
  } else if (type=="MAPE"){
    # Mean Absolute Percentage error
    output <- mean(   abs( (actual - predicted)/actual ), ...   )
  } else if (type %in% c("sMAPE", "SMAPE", "sMAPE1", "SMAPE1")){
    # Symetric Mean Absolute Percentage Error 1
    output <- 2*mean(   abs( actual - predicted )/( abs(actual)+abs(predicted) )   )
  } else if (type %in% c("sMAPE2", "SMAPE2")){
    # Symetric Mean Absolute Percentage Error 2 (with bias direction)
    actual <- temp$actual; predicted <- temp$predicted
    output <- 2*sum( abs(actual - predicted), ... )/sum(actual + predicted, ...)
  } else if (type %in% c("MAD/MEAN", "MAD-MEAN ratio", "MADMEAN")){
    # Mean Absolute Deviation / Mean
    actual <- temp$actual; predicted <- temp$predicted
    output <- sum( abs(actual - predicted), ... )/sum(actual, ...)
  } else if (type %in% c("MASE", "MASE1")){
    # Mean Absolute Scaled Error (NON SEASONNAL)
    if (as.logical(sum(is.na(c(actual, predicted))))) stop("this method does not allow missing values")
    output <- (NROW(actual)-1)/NROW(actual)*sum(abs(actual-predicted))/sum(abs(diff(actual)) )
  } else if (type %in% c("MASE2", "seasonnal MASE", "sMASE", "SMASE")){
    # Mean Absolute Scaled Error (SEASONNAL)
    if (as.logical(sum(is.na(c(actual, predicted))))) stop("this method does not allow missing values")
    output <- (NROW(actual)-season)/NROW(actual)*sum(abs(actual-predicted))/sum(abs(diff(actual, season)) )
  } else if (type=="GMRAE"){
    # Geometric Mean Relative Absolute Error
    stop("not yet implemented")
  } else if (type %in% c("MdAE", "MDAE")){
    # Median Absolute Error
    output <- median( abs(actual - predicted), ... )
  } else if (type %in% c("MdAPE", "MDAPE", "MdRAE", "MDRAE")){
    # Median Absolute Percentage Error
    output <- median(   abs( (actual - predicted)/actual ), ...   )
  } else if (type=="RMSLE"){
    # Root Mean Squared Logarithmic Error
    output <- sqrt(      mean(   ( log(actual+1) - log(predicted+1) )^2, ...   )      )
  } else {
    stop("unknown type of error asked")
  }
  print(paste("type of error choosen: ", type))
  return(output)
}


# Graphes ACP et PACF
ggACF = function(x){
  require(ggplot2)
  require(gridExtra)
  TS <- x
  bacf    <- acf(TS, plot = FALSE)
  bacfdf <<- with(bacf, data.frame(lag, acf, n.used))
  bacfdf  <- with(bacf, data.frame(lag, acf, n.used))
  q1      <- ggplot(data = bacfdf, aes(x = lag, y = acf)) + 
    geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_ribbon(aes(x=lag, ymin=-qnorm(0.975)/sqrt(bacfdf[1,3]), ymax=qnorm(0.975)/sqrt(bacfdf[1,3])), fill="red", alpha=0.2) +
    ggtitle("Auto-correlogramme") + xlab("decalage temporel (rouge: IC95%)") + ylab("correlation") + ylim(-1,1)
  bacf    <- pacf(TS, plot = FALSE)
  bacfdf <<- with(bacf, data.frame(lag, acf, n.used))
  bacfdf  <- with(bacf, data.frame(lag, acf, n.used))
  q2      <- ggplot(data = bacfdf, aes(x = lag, y = acf)) + 
    geom_hline(aes(yintercept = 0)) + geom_segment(mapping = aes(xend = lag, yend = 0)) +
    geom_ribbon(aes(x=lag, ymin=-qnorm(0.975)/sqrt(bacfdf[1,3]), ymax=qnorm(0.975)/sqrt(bacfdf[1,3])), fill="red", alpha=0.2) +
    ggtitle("Auto-correlogramme PARTIEL") + xlab("decalage temporel (rouge: IC95%)") + ylab("correlation") + ylim(-1,1)
  grid.arrange(q1, q2, ncol=2)
}


# fonction qui cree un vecteur retarde (ou differencie) dans un sens ou dans l'autre
# fill argument pour remplir les trous crees avec la premiere ou derniere valeur
shift = function(x, lag, fill=FALSE) {
  require(dplyr)
  switch(sign(lag)/2+1.5, 
         lead( x, n=abs(lag), default=switch(fill+1, NA, tail(x, 1))  ), 
         lag(  x, n=abs(lag), default=switch(fill+1, NA, head(x, 1))  )
  )
}
