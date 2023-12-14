check_spotify_keys <- function() {
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SECRET_KEY")
  if (is.na(client_id) || is.na(client_secret) || client_id == "" || client_secret == "") {
    error_message <- "Spotify anahtarlarına erişim sağlanamadı. .Renviron dosyasını kontrol edin ve gerekli anahtarları eklediğinizden emin olun."
    stop(error_message)
  } else {
    cat("Spotify anahtarları başarıyla yüklendi.\n")
  }
}

use_data(check_spotify_keys)
