
# Function: xml_find_attr
# funzione modificata dal pacchetto xml2 che permette di estrarre direttamente gli elementi aventi un determinato 
# valore in un attributo (class, id, etc), è possibile selezionare un solo attributo ma piu nomi di classi possibili 

xml_find_attr <- function(x, xpath, attrnames = "", attribute = "class", ns = xml_ns(x), exact_match = TRUE){
  
  all_elements = xml2::xml_find_all(x = x, xpath = xpath, ns = ns)
  
  if(exact_match){
    class_index = which(xml2::xml_attr(all_elements, attribute) %in% attrnames)
  } else {
    class_index = which(str_detect(xml2::xml_attr(all_elements, attribute), attrnames[1]))
  }

  all_elements[class_index]
}

# esempio: estrae da html tutti i paragrafi div con classe "smalllinks"
# 
# xml_find_attr(html, ".//div", attrnames = "smalllinks", attribute = "class")



# FUNCTION: tr_to_row 
## input:
#### tr: riga della tabella dell'archivio storico meteo.it

tr_to_row <- function(tr){
  
  null_df = dplyr::tibble(
    date = NA_character_,   # data in formato %YYYY/%MM/%DD
    city = NA_character_,   # nome citta
    station = NA_character_,# stazione di importazione
    n.day = NA_character_,  # numero giorno del mese 
    month = NA_character_,  # numero del mese 
    year = NA_character_,   # anno 
    
    mean_temp = NA_integer_,   # temperatura media 
    min_temp = NA_integer_,    # temperatura minima 
    max_temp = NA_integer_,    # temperatura massima 
    
    dew_point = NA_character_, # punto di rugiada
    precip = NA_character_,    # precipitazioni 
    mean_humid = NA_character_,# umidita media 
    min_humid = NA_character_, # umidita minima 
    max_humid = NA_character_, # umidita massima 
    visibility = NA_character_,# visibilita
    mean_wind = NA_character_, # velocita vento media 
    max_wind = NA_character_,  # velocita vento massima 
    burst = NA_character_,     # raffiche di vento 
    mean_pressure = NA_character_,     # pressione media 
    sealevel_pressure = NA_character_, # pressione media sul livello del mare
    
    rain = NA_character_,              # pioggia 
    phenomena = NA_character_,         # fenomeni metereologici
    weather_condition = NA_character_, # condizioni meteo 
    url_info = NA_character_           # url infomazioni giornaliere 
  ) 
  
  # divisione tr in td
  all_td = xml2::xml_find_all(tr, "./td")
  
  # posizione td da controllare (potrebbe cambiare)
  n.day = xml2::xml_text(all_td[1])
  mean_temp = xml2::xml_text(all_td[2])
  min_temp = xml2::xml_text(all_td[3])
  max_temp = xml2::xml_text(all_td[4])
  precip = xml2::xml_text(all_td[5])
  max_vento = xml2::xml_text(all_td[7])
  url_info = xml2::xml_attr(xml2::xml_find_all(all_td[10], "./a"), "href")
  
  # controllo per valori mancanti 
  null_df$n.day = ifelse(is.null(n.day), NA_character_, n.day)
  null_df$mean_temp = ifelse(is.null(mean_temp), NA_character_, mean_temp)
  null_df$min_temp = ifelse(is.null(min_temp), NA_character_, min_temp)
  null_df$max_temp = ifelse(is.null(max_temp), NA_character_, max_temp)
  null_df$precip = ifelse(is.null(precip), NA_character_, precip)
  null_df$max_wind = ifelse(is.null(max_vento), NA_character_, max_vento)
  null_df$url_info = ifelse(is.null(url_info), NA_character_, paste0("https://www.ilmeteo.it/portale/", url_info))
  
  return(null_df)
}

# FUNCTION: Meteo_city_month_year 
## input:
#### city: stringa, città importazione 
#### month: stringa, mese importazione 
#### imp.year: stringa, anno importazione 
#### verbose: logical, if true display messages during importation 


city = "Viterbo"
month = "2"
imp.year = "2022"


Meteo_city_month_year <- function(city = "", month = "", imp.year = "", verbose = TRUE){
  
  # funzione per rendere maiuscola prima lettera di una parola 
  FirstToupper <- function(word){
    
    word = stringr::str_split(word, "")
    
    word[[1]][1] = toupper(word[[1]][1])
    
    word = paste0(word[[1]], collapse = "")
    
    return(word)
    
  }
  
  # URL di base 
  base_url = "https://www.ilmeteo.it/portale/archivio-meteo"
  
  # 1) match argomento anni
  anni_validi = seq(1973, lubridate::year(Sys.Date()), 1)
  imp.year = match.arg(imp.year, anni_validi)
  
  # 2) la citta deve essere scritta sempre con la lettera maiuscola
  #### le citta con un nome composto vengono separate dal "+": es Ascoli+Piceno
  city = stringr::str_split(city, " ")[[1]]     # separo le parole (se sono piu di una)
  city = purrr::map_chr(city, FirstToupper)     # trasformo tutte le lettere in maiuscole 
  city_name = paste0(city, collapse = " ")      # salvo il nome della citta 
  city = paste0(city, collapse = "+")           # ricreo la stringa 
  
  # 3) match argomento mesi
  mesi_validi = c(Gennaio = 1, Febbraio = 2, Marzo = 3, 
                  Aprile = 4, Maggio = 5, Giugno = 6, Luglio = 7, 
                  Agosto = 8, Settembre = 9, Ottobre = 10, Novembre = 11, Dicembre = 12)
  
  # indice per il numero del mese 
  month_index = match.arg(month, mesi_validi)
  month = names(month_index) # nome del mese per comporre url 
  
  # 4) creazione url per l'importazione 
  url = paste0(base_url, "/", city, "/", imp.year, "/", month )
  
  # 5) lettura html 
  html = xml2::read_html(url) 
  
  # 6) tabella con informazioni di interesse n4
  table = xml2::xml_find_all(html, "//table")[4]
  
  # 7) separo tutte le righe per avere un importazione controllata (ma piu lunga) 
  all_tr = xml2::xml_find_all(table, "./tr")
  
  # 8) Mapping tr_to_row
  df_importazione = purrr::map_df(all_tr[-1], tr_to_row)
  
  # importazione stazione 
  stazione = xml_text(xml_find_attr(html, ".//div", attrnames = "smalllinks")[1])
  stazione = str_remove_all(stazione, "Dati registrati dalla stazione meteo di  ")
  stazione = tm::removePunctuation(stazione)
  
  # aggiunta informazioni sulla citta, mese e anno 
  df_importazione = dplyr::mutate(df_importazione, 
                                  city = city_name, 
                                  month = month_index,
                                  year = imp.year, 
                                  station = stazione)

  
  # creazione della data
  df_importazione$date = as.Date(paste0(imp.year,"/", month_index, "/", df_importazione$n.day))
  
  if(verbose) message("Importazione: ", city_name, " (Anno: ", imp.year, ", Mese: ", month, ")")
  
  return(df_importazione)
  
}


# FUNCTION: Meteo_city_year 
Meteo_city_year <- function(city = "", imp.year = "", verbose = TRUE){
  
  # match argomento anni
  anni_validi = seq(1973, lubridate::year(Sys.Date()), 1)
  
  imp.year = as.character(match.arg(imp.year, anni_validi))
  
  if(imp.year == lubridate::year(Sys.Date())){
    
    max_month = lubridate::month(Sys.Date())
    months = as.character(1:max_month)
    
  } else {
    months = as.character(1:12)
    
  }
  
  purrr::map_df(months, ~Meteo_city_month_year(city = city, 
                                               month = .x, 
                                               imp.year = imp.year, 
                                               verbose = verbose))
}


# FUNCTION: MeteoIT 
MeteoIT <- function(city = "", start_year = "2021", end_year = "2022", verbose = TRUE, complete_info = FALSE, filename = NULL){
  
  seq_years = as.numeric(start_year):as.numeric(end_year)
  seq_years = as.character(seq_years)
  
  df_importazione = purrr::map_df(seq_years, ~Meteo_city_year(city, imp.year = .x, verbose = verbose))
  
  if(verbose){message("Importazione Parziale Completata!")}
  
  # se il salvataggio è attivo salviamo dopo la prima importazione 
  if(!is.null(filename)){
    
    data = Sys.Date()
    data = str_replace_all(data, "-", "_")
    filename = paste0(filename, "_", data, "_",  ".RData")
    save(df_importazione, file = filename)
    
    if(verbose){message("Importazione Parziale Salvata con il nome di: ", filename)}
  }
  
  
  if(complete_info){
    
    if(verbose){message("Importazione con informazioni dettagliate in corso...")}
    
    # aggiunta informazioni complete 
    df_completo = AddDailyInfo(df_importazione, verbose = verbose)
    df_completo = CleanMeteoIT(df_completo)
    
    # se il salvataggio è attivo salviamo tutto dopo la seconda importazione 
    if(!is.null(filename)){
      
      data = Sys.Date()
      data = str_replace_all(data, "-", "_")
      filename = paste0(filename, "_", data, "_",  ".RData")
      
      save(df_importazione, df_completo, file = filename)
      
      if(verbose){message("Importazione Completa Salvata con il nome di: ", filename)}
    }
    
    return(df_completo)
  }
  
  return(df_importazione)
  
}


# FUNCTION: AddDailyInfo 
# aggiunta informazioni dettagliate (procedura piu lunga )
AddDailyInfo <- function(df, verbose = TRUE ){
  
  # inizializzo output 
  df_output = NULL
  
  # funzione per importare le informazioni di un singolo url di una singola riga 
  DailyInfo <- function(df_row){
    
    url = df_row$url_info[1]
    
    html = xml2::read_html(url)
    
    table = xml2::xml_find_all(html, ".//table")[4]
    table = rvest::html_table(table)[[1]]
    
    
    df_row$mean_temp = table[1,][[2]]
    df_row$min_temp = table[2,][[2]]
    df_row$max_temp = table[3,][[2]]
    df_row$dew_point = table[4,][[2]]
    df_row$mean_humid = table[5,][[2]]
    df_row$min_humid = table[6,][[2]]
    df_row$max_humid = table[7,][[2]]
    df_row$visibility = table[8,][[2]]
    df_row$mean_wind = table[9,][[2]]
    df_row$max_wind = table[10,][[2]]
    df_row$burst = table[11,][[2]]
    df_row$mean_pressure = table[12,][[2]]
    df_row$sealevel_pressure = table[13,][[2]]
    df_row$rain = table[14,][[2]]
    df_row$phenomena = table[15,][[2]]
    df_row$weather_condition = table[16,][[2]]
    
    return(df_row)
    
  }
  
  n_iter <- nrow(df) # Number of iterations of the loop
  
  if(verbose) message("Tempo Stimato: ", round(n_iter), " Secondi")
  
  start_time = Sys.time()
  
  if(verbose){
    # Initializes the progress bar
    pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                         max = n_iter, # Maximum value of the progress bar
                         style = 3,    # Progress bar style (also available style = 1 and style = 2)
                         width = 50,   # Progress bar width. Defaults to getOption("width")
                         char = "=")   # Character used to create the bar
  }
  
  
  all_df_info = list()
  safe_import = purrr::safely(DailyInfo)
  
  for(i in 1:n_iter) {
    
    result = safe_import(df[i,])$result
    # controllo per eventuali pagine mancanti 
    if(is.null(result)){
      all_df_info[[i]] = df[i,]
    } else {
      all_df_info[[i]] = result 
    }
    
    if(verbose){setTxtProgressBar(pb, i)} # settaggio progress bar 
    
  }
  
  if(verbose){close(pb)} # chiusura connesione 
  
  end_time = Sys.time()
  
  if(verbose) message("Tempo Impiegato: ", 
                      round(end_time-start_time, 3), 
                      " Minuti...")
  
  # dataset finale 
  df_output = bind_rows(all_df_info)
  return(df_output)
  
}

# FUNCTION: CleanMeteoIT 
# pulizia standard dei dati importati 

CleanMeteoIT <- function(df){

  # clean temperature data 
  
  df = suppressWarnings(dplyr::mutate(df, 
              
    # clean temperature data           
    mean_temp = stringr::str_remove_all(mean_temp, "°C"),
    mean_temp = stringr::str_trim(mean_temp), 
    mean_temp = as.numeric(mean_temp),
    
    min_temp = stringr::str_remove_all(min_temp, "°C"),
    min_temp = stringr::str_trim(min_temp), 
    min_temp = as.numeric(min_temp),
    
    
    max_temp = stringr::str_remove_all(max_temp, "°C"),
    max_temp = stringr::str_trim(max_temp), 
    max_temp = as.numeric(max_temp),
    
    dew_point = stringr::str_remove_all(dew_point, "°C"),
    dew_point = stringr::str_trim(dew_point), 
    dew_point = as.numeric(dew_point),
    
    precip = ifelse(precip == "n/d" | precip == "-", NA_character_, precip),
    burst = ifelse(burst == "n/d" | burst == "-", NA_character_, burst),
    rain = ifelse(rain == "n/d" | rain == "-", NA_character_, rain),
    sealevel_pressure = ifelse(sealevel_pressure == "n/d" | sealevel_pressure == "-", NA_character_, sealevel_pressure),
    
    # humidity
    mean_humid = stringr::str_remove_all(mean_humid, "%"),
    mean_humid = stringr::str_trim(mean_humid), 
    mean_humid = as.numeric(mean_humid),
    
    min_humid = stringr::str_remove_all(min_humid, "%"),
    min_humid = stringr::str_trim(min_humid), 
    min_humid = as.numeric(min_humid),
    
    
    max_humid = stringr::str_remove_all(max_humid, "%"),
    max_humid = stringr::str_trim(max_humid), 
    max_humid = as.numeric(max_humid),
    
    # visibility 
    visibility = stringr::str_remove_all(visibility, "km"),
    visibility = stringr::str_trim(visibility), 
    visibility = as.numeric(visibility),
    
    # wind
    mean_wind = stringr::str_remove_all(mean_wind, "km/h"),
    mean_wind = stringr::str_trim(mean_wind), 
    mean_wind = as.numeric(mean_wind),
    
    max_wind = stringr::str_remove_all(max_wind, "km/h"),
    max_wind = stringr::str_trim(max_wind), 
    max_wind = as.numeric(max_wind),
    
    burst = stringr::str_remove_all(burst, "km/h"),
    burst = stringr::str_trim(burst), 
    burst = as.numeric(burst),
    
    # se le raffiche sono nulle le sostituiamo con il vento massimo riportato
    burst = ifelse(is.na(burst), max_wind, burst),
    
    # pressure 
    mean_pressure = stringr::str_remove_all(mean_pressure, "mb"),
    mean_pressure = stringr::str_trim(mean_pressure), 
    mean_pressure = as.numeric(mean_pressure),
    
    sealevel_pressure = stringr::str_remove_all(sealevel_pressure, "mb"),
    sealevel_pressure = stringr::str_trim(sealevel_pressure), 
    sealevel_pressure = as.numeric(sealevel_pressure),
    
    # rain 
    rain = stringr::str_remove_all(rain, "mb"),
    rain = stringr::str_trim(rain), 
    rain = as.numeric(rain),
    # se NA no piogge quindi 0
    rain = ifelse(is.na(rain), 0, rain),
    
    # riduco etichetta ridondante
    phenomena = ifelse(phenomena == "Pioggia - Temporale Pioggia - Temporale - Nebbia", "Pioggia - Temporale - Nebbia", phenomena),
    
  
    # ricalcolo la mean temp come (massimo+minimo)/2
    mean_temp = (max_temp + min_temp)/2
    
  ))  %>%
    dplyr::select(-url_info, -precip)
  
  df$month =  months.Date(df$date)
  df$n.day =  weekdays(df$date)
  
 return(df)


  
}





