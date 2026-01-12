# ===============================
# Téléchargement Flight List OPDI
# Période : Mai – Juillet 2023
# ===============================

# Packages nécessaires
library(fs)
library(httr)
library(lubridate)

# -------------------------------------------------
# Générer les URLs OPDI Flight List (mensuelles)
# -------------------------------------------------
generate_urls <- function(data_type, start_date, end_date) {
  
  base_url <- paste0(
    "https://www.eurocontrol.int/performance/data/download/OPDI/v002/",
    data_type, "/", data_type, "_"
  )
  
  urls <- c()
  
  start_dt <- ymd(paste0(start_date, "01"))
  end_dt   <- ymd(paste0(end_date, "01"))
  
  current_dt <- start_dt
  while (current_dt <= end_dt) {
    urls <- c(
      urls,
      paste0(base_url, format(current_dt, "%Y%m"), ".parquet")
    )
    current_dt <- current_dt %m+% months(1)
  }
  
  return(urls)
}

# -------------------------------------------------
# Télécharger les fichiers (avec sécurité)
# -------------------------------------------------
download_files <- function(urls, save_folder) {
  
  if (!dir_exists(save_folder)) {
    dir_create(save_folder, recurse = TRUE)
  }
  
  for (url in urls) {
    
    file_name <- basename(url)
    save_path <- path(save_folder, file_name)
    
    if (file_exists(save_path)) {
      message("✓ Skipping ", file_name, " (already exists)")
      next
    }
    
    message("⬇ Downloading ", file_name)
    
    tryCatch({
      response <- GET(
        url,
        write_disk(save_path, overwrite = TRUE),
        timeout(300)
      )
      
      if (http_error(response)) {
        warning("HTTP error ", status_code(response), " for ", file_name)
        file_delete(save_path)
      } else {
        message("✓ Saved to ", save_path)
      }
      
    }, error = function(e) {
      warning("Failed: ", file_name, " → ", e$message)
      if (file_exists(save_path)) file_delete(save_path)
    })
    
    # Pause de courtoisie
    Sys.sleep(2)
  }
}

# -------------------------------------------------
# UTILISATION : 3 mois consécutifs
# -------------------------------------------------

# Dataset OPDI à télécharger
data_type <- "flight_list"

# Période choisie (intéressante pour congestion)
start_period <- "202305"  # Mai 2023
end_period   <- "202307"  # Juillet 2023

# Génération des URLs
urls <- generate_urls(
  data_type = data_type,
  start_date = start_period,
  end_date   = end_period
)

# Téléchargement
download_files(
  urls = urls,
  save_folder = "./data/opdi/flight_list"
)
