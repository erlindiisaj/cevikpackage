

test_that("Global Workspace’de spotify_token adlı bir değişken var.", {
  expect_true(exists("spotify_token"))
})


test_that("spotify_token adlı değişkenin tipi “function” olmalı.", {
  expect_true(is.function(spotify_token))
})

test_that("spotify_token() çağrıldığında döndürdüğü çıktı bir liste olmalı", {
  expect_true(typeof(spotify_token()) == "list")
})


test_that("spotify_token() çağrıldığında döndürdüğü listenin iki elementi olmalı.", {
  expect_true(length(spotify_token()) == 2)
})


test_that("listenin ilk elementinin ismi status_code olmalı", {
  expect_true(names(spotify_token()[1]) == "status_code")
})


test_that("döndürdüğü listenin ilk elementinin class’ı numeric olmalı", {
  expect_true(is.numeric(spotify_token()[[1]]))
})


test_that("döndürdüğü listenin status_code adlı elementinin değeri 200’e eşit olmalı", {
  expect_true(spotify_token()[[1]] == 200)
})




test_that("listenin ikinci elementinin ismi token olmalı", {
  expect_true(names(spotify_token()[2]) == "token")
})

test_that("döndürdüğü listenin ilk elementinin class’ı numeric olmalı", {
  expect_true(is.character(spotify_token()[[2]]))
})

test_that("döndürdüğü listenin ikinci elementi ’Bearer ’ ile başlamalı", {
  expect_true(grepl("^Bearer ",spotify_token()[[2]]))
})


test_that("döndürdüğü listenin ikinci elementi ’Bearer ’ ile başlamalı", {
  expect_true(nchar(spotify_token()[[2]]) == 122)
})






test_that("Global Workspace’de spotify_search_artist adlı bir değişken olmalı.", {
  expect_true(exists("spotify_search_artist"))
})


test_that("spotify_search_artist adlı değişkenin tipi “function” olmalı.", {
  expect_true(is.function(spotify_search_artist))
})


test_that("spotify_search_artist() herhangi bir artist ismi ile çağrıldığında döndürdüğü çıktı bir liste olmalı.", {
  res <- spotify_search_artist("The Doors")
  expect_true(typeof(res) == "list")
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin iki elementi olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(length(res) == 2)
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ilk elementinin ismi status_code olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(names(res)[1] == "status_code")
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ilk elementinin class’ı numeric olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(is.numeric(res[[1]]))
})



test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin status_code adlı elementinin değeri 200’e eşit olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(res[[1]] == 200)
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin ismi search_results olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(names(res)[2] == "search_results")
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin class’ı data.frame olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(is.data.frame(res[[2]]))
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin iki sütun barındırmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(ncol(res[[2]]) == 2)
})


test_that("spotify_search_artist() çağrıldığında döndürdüğü listenin ikinci elementinin sütun isimleri c(“artist”, “id”) olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_identical(names(res[[2]]) == c("artist", "id"), c(T,T))
})


test_that("spotify_search_artist(\"The Doors\") çağrıldığında döndürdüğü listenin ikinci elementinin birinci satırının \"id\" adlı sütunu \"22WZ7M8sxp5THdruNY3gXt\" olmalı", {
  res <- spotify_search_artist("The Doors")
  expect_true(res$search_results$id[1] == "22WZ7M8sxp5THdruNY3gXt")
})





test_that("Global Workspace’de spotify_artist_top_track adlı bir değişken var.", {
  expect_true(exists("spotify_artist_top_track"))
})

test_that("Spotify_artist_top_track adlı değişkenin tipi function'dur.", {
  expect_true(is.function(spotify_artist_top_track))
})

test_that("Döndürdüğü function'un status_code adlı elementinin değeri 200’e eşit degildir", {
  artist_id <- "testingwrongid"
  expect_true(spotify_artist_top_track(artist_id)$error$status != 200)
})

test_that("Error iki eleman içerir.", {
  artist_id <- "testingwrongid"
  expect_true(length(spotify_artist_top_track(artist_id)$error) == 2)
})









test_that("Global Workspace’de spotify_artist_top_track adlı bir değişken var.", {
  expect_true(exists("spotify_artist_top_track"))
})

test_that("Spotify_artist_top_track adlı değişkenin tipi function'dur.", {
  expect_true(is.function(spotify_artist_top_track))
})

test_that("Döndürdüğü function'un status_code adlı elementinin değeri 200’e eşit olmalı", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(spotify_artist_top_track(artist_id)[[1]] == 200)
})

test_that("Fonksiyonun çıktısı 2 elementli bir liste ", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(length(spotify_artist_top_track(artist_id)) == 2)
})

test_that("Listenin birinci elementinin ismi “status_code” olmalı", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(names(spotify_artist_top_track(artist_id)[1]) == "status_code")
})

test_that("status_code numeric olmalı", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(is.numeric(spotify_artist_top_track(artist_id)[[1]]))
})

test_that("Listenin ikinci elementinin adı resultdf olmalı ", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(names(spotify_artist_top_track(artist_id)[2]) == "resultdf")
})


test_that("resultdf data.frame olmalı ", {
  artist_id <- "36QJpDe2go2KgaRleHCDTp"
  expect_true(is.data.frame(spotify_artist_top_track(artist_id)[[2]]))
})



test_that("Global Workspace’de spotify_top_tracks adlı bir değişken var.", {
  expect_true(exists("spotify_top_tracks"))
})

test_that("Spotify_artist_top_track adlı değişkenin tipi function'dur.", {
  expect_true(is.function(spotify_top_tracks))
})


test_that("Fonksiyonun çıktısı 5 sutunlu bir date.frame olmali", {
  spotify_artist_ids <- c(
    "0TnOYISbd1XYRBk9myaseg",
    "1vCWHaC5f2uS3yhpwWbIA6",
    "6vWDO969PvNqNYHIOW5v0m"
  )
  expect_true(ncol(spotify_top_tracks(spotify_artist_ids)) == 5)
})


test_that("Spotify_artist_top_track functionun ciktisi data.frame olmali.", {
  spotify_artist_ids <- c(
    "0TnOYISbd1XYRBk9myaseg",
    "1vCWHaC5f2uS3yhpwWbIA6",
    "6vWDO969PvNqNYHIOW5v0m"
  )
  expect_true(is.data.frame(spotify_top_tracks(spotify_artist_ids)))
})
