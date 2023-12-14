
spotify_token <- function(){
  response <- httr::POST(
    "https://accounts.spotify.com/api/token",
    config = httr::authenticate(user = Sys.getenv("SPOTIFY_ID"),
                          password = Sys.getenv("SECRET_KEY")),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  getToken <-  httr::content(response)
  bearer.Token = paste(getToken$token_type, getToken$access_token)
  return(list(status_code = response$status_code, token=bearer.Token))
}



spotify_search_artist <- function(artist_name){
  if(is.character(artist_name) == F){
    return(list(status_code=NULL, search_results=NULL))
  }

  auth <- httr::POST(
    "https://accounts.spotify.com/api/token",
    config = httr::authenticate(user = Sys.getenv("SPOTIFY_ID"),
                          password = Sys.getenv("SECRET_KEY")),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  getToken <-  httr::content(auth)
  bearer.Token = paste(getToken$token_type, getToken$access_token)

  response <- httr::GET(
    "https://api.spotify.com/v1/search",
    config = httr::add_headers(Authorization = bearer.Token),
    query = list(q = artist_name, type = "artist")
  )
  content <- httr::content(response)
  search_result <- data.frame(
    artist = sapply(content$artists$items, function(artist_info){
      artist_info$name
    }),
    id = sapply(content$artists$items, function(artist_info){
      artist_info$id
    })
  )
  return(list(status_code=response$status_code, search_results=search_result))
}




spotify_artist_top_track <- function(artistId){


  # Get the Bearer token of the user using SPOTIFY_ID and SPOTIFY_SECRET
  auth <- httr::POST(
    "https://accounts.spotify.com/api/token",
    config = httr::authenticate(user = Sys.getenv("SPOTIFY_ID"),
                          password = Sys.getenv("SECRET_KEY")),
    body = list(grant_type = "client_credentials"),
    encode = "form"
  )
  getToken <-  httr::content(auth)
  bearer.Token = paste(getToken$token_type, getToken$access_token)


  # Endpoint to get Spotify catalog information about an artist's
  api_url <- paste0("https://api.spotify.com/v1/artists/", artistId, "/top-tracks")
  res <- httr::GET(
    url = api_url,
    config = httr::add_headers(Authorization = bearer.Token),
    query = list(market="ES")
  )
  status_code = httr::status_code(res)


  if(status_code == 200) {

    response <- httr::content(res)
    # Creating list with every element of the returned JSON
    tracks_list <- lapply(response$tracks, function(track){
      list(
        id = track$id,
        name = track$name,
        artist = track$artists[[1]]$name,
        album = track$album$name,
        year = substr(track$album$release_date, 1, 10)

      )
    })
    final_list <- as.data.frame(do.call(rbind, tracks_list))
    return(list(
      status_code = as.numeric(auth$status_code),
      resultdf = final_list
    ))
  } else {
    content <- httr::content(res, "parsed");
    return(content)
  }

}


spotify_top_tracks <- function(artist_ids) {

  tracks_list <- lapply(artist_ids, spotify_artist_top_track)
  alltracks <- do.call(rbind, lapply(tracks_list, function(track_data) {
    if (!is.null(track_data$resultdf)) {
      track_data$resultdf
    } else {
      data.frame(name = character(0), artist = character(0), album = character(0), year = character(0), stringsAsFactors = FALSE)
    }
  }))

  return(alltracks)


}





create_package("spotify.200401114")

use_mit_license("Erlindi Isaj")

use_readme_md()

use_package("httr")

use_data(spotify_token, overwrite = TRUE)
use_data(spotify_search_artist, overwrite = TRUE)
use_data(spotify_artist_top_track, overwrite = TRUE)
