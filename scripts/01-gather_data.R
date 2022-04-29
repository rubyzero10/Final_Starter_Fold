#### Preamble ####
# Purpose: Clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander [CHANGE THIS TO YOUR NAME!!!!]
# Data: 3 January 2021
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!
# - Change these to yours
# Any other information needed?



install.packages("spotifyr")
install.packages("tidyverse")
library(spotifyr)
library(tidyverse)
library(usethis)
edit_r_environ()

#select top 20
kpop <- get_genre_artists('k-pop')

#remove non 3rd and 4th gen idols and soloists and add reasured groups to total 10 
# 3rd gen and 10 4th gen 

#add mamamo 
mamamoo <- get_artist("0XATRDCYuuGhk0oE7C0o5G")
#add gfriend 
g_friend <- get_artist("0qlWcS66ohOIi0M8JZwPft")
#add treasure 
treasure<- get_artist("3KonOYiLsU53m4yT7gNotP")
#add n_mixx / aespa
aespa <- get_artist("6YVMFz59CuY7ngCxTxjpxE")

add<- 
  bind_rows(
  c(name = g_friend$name, popularity = g_friend$popularity,followers.total = g_friend$followers$total, gender = "GG", generation = "3rd Gen"),
  c(name = mamamoo$name, popularity = mamamoo$popularity,followers.total = mamamoo$followers$total, gender = "GG", generation = "3rd Gen" ),
  c(name = treasure$name, popularity = treasure$popularity,followers.total = treasure$followers$total, gender = "BG", generation = "4th Gen" ),
  c(name = aespa$name, popularity = aespa$popularity,followers.total = aespa$followers$total, gender = "GG", generation = "4th Gen" ),
  )
  


kpop_mid <- 
kpop |>
  select(name, popularity, followers.total) |>
  mutate(name = case_when(
    name == "SEVENTEEN" ~ "SVT", 
    name == "TOMORROW X TOGETHER"  ~ "TXT",
    name == "BTS" ~ "BTS",
    name == "TWICE" ~ "TWICE",
    name == "Stray Kids" ~ "SKZ",
    name == "BLACKPINK" ~ "BLACKPINK",
    name == "Red Velvet" ~ "Red Velvet",
    name == "(G)I-DLE" ~ "(G)I-DLE",
    name == "ENHYPEN" ~ "ENHYPEN",
    name == "ATEEZ" ~ "ATEEZ",
    name == "NCT DREAM" ~ "NCT DREAM",
    name == "STAYC" ~ "STAYC",
    name == "EXO" ~ "EXO",
    name == "NCT 127" ~ "NCT 127",
    name == "ITZY" ~ "ITZY",
    name == "IVE" ~ "IVE")) %>%
  mutate(gender = case_when(
    name == "SVT" ~ "BG", 
    name == "TXT"  ~ "BG",
    name == "BTS" ~ "BG",
    name == "TWICE" ~ "GG",
    name == "SKZ" ~ "BG",
    name == "BLACKPINK" ~ "GG",
    name == "Red Velvet" ~ "GG",
    name == "(G)I-DLE" ~ "GG",
    name == "ENHYPEN" ~ "BG",
    name == "ATEEZ" ~ "BG",
    name == "NCT DREAM" ~ "BG",
    name == "STAYC" ~ "GG",
    name == "EXO" ~ "BG",
    name == "NCT 127" ~ "BG",
    name == "ITZY" ~ "GG",
    name == "IVE" ~ "GG")) %>%
  mutate(generation = case_when(
    name == "SVT" ~ "3rd Gen", 
    name == "TXT"  ~ "4th Gen",
    name == "BTS" ~ "3rd Gen",
    name == "TWICE" ~ "3rd Gen",
    name == "SKZ" ~ "4th Gen",
    name == "BLACKPINK" ~ "3rd Gen",
    name == "Red Velvet" ~ "3rd Gen",
    name == "(G)I-DLE" ~ "4th Gen",
    name == "ENHYPEN" ~ "4th Gen",
    name == "ATEEZ" ~ "4th Gen",
    name == "NCT DREAM" ~ "3rd Gen",
    name == "STAYC" ~ "4th Gen",
    name == "EXO" ~ "3rd Gen",
    name == "NCT 127" ~ "3rd Gen",
    name == "ITZY" ~ "4th Gen",
    name == "IVE" ~ "4th Gen")) %>%
  drop_na()
  
popular_data <- rbind(kpop_mid, add)  


write.csv(popular_data, "inputs/data/raw_data_popular.csv", row.names=FALSE)

#Now find top tracks for each artists 

BTS <- get_artist_top_tracks('3Nrfpe0tUJi4K4DXYWgMUX')
TWICE <- get_artist_top_tracks('7n2Ycct7Beij7Dj7meI4X0')
SKZ <- get_artist_top_tracks("2dIgFjalVxs4ThymZ67YCE")
BLACKPINK <- get_artist_top_tracks('41MozSoPIsD1dJM0CLPjZF')
TXT <- get_artist_top_tracks('0ghlgldX5Dd6720Q3qFyQB')
RED_VELVET <- get_artist_top_tracks('1z4g3DjTBBZKhvAroFlhOM')
SVT <- get_artist_top_tracks('7nqOGRxlXj7N2JYbgNEjYH')
GIDLE <- get_artist_top_tracks('2AfmfGFbe0A0WsTYm0SDTx')
ENHYPHEN <- get_artist_top_tracks('5t5FqBwTcgKTaWmfEbwQY9')
ATEEZ <- get_artist_top_tracks('68KmkJeZGfwe1OUaivBa2L')
DREAM <- get_artist_top_tracks('1gBUSTR3TyDdTVFIaQnc02')
STAYC <- get_artist_top_tracks('01XYiBYaoMJcNhPokrg0l0')
ITZY <- get_artist_top_tracks('2KC9Qb60EaY0kW4eH68vr3')
EXO <- get_artist_top_tracks('3cjEqqelV9zb4BYE3qDQ4O')
NCT_1 <- get_artist_top_tracks('7f4ignuCJhLXfZ9giKT7rH')
IVE <- get_artist_top_tracks('6RHTUrRF63xao58xh9FXYJ')
G_F <- get_artist_top_tracks(g_friend$id)
MAMA <- get_artist_top_tracks(mamamoo$id)
AESP <- get_artist_top_tracks(aespa$id)
TREASURE <- get_artist_top_tracks(treasure$id)

kpop_audio1 <- rbind(BTS, TWICE, SKZ, BLACKPINK, TXT, RED_VELVET, SVT, GIDLE, ENHYPHEN, ATEEZ)
get_track_audio_features(kpop_audio1$id)

kpop_audio2 <- rbind(DREAM, STAYC,ITZY, EXO, NCT_1, IVE, G_F, MAMA, TREASURE,AESP) 

get_track_audio_features(kpop_audio2$id)

#get name of group fro audio tracks 
t_10 <- 
  popular_data %>% 
  select(name, gender, generation) |>
  slice(1:10)|>
  rename(
    artist_name = name
  )
  
x <- data.frame(t_10,
                nb_times = c(10,10,10,10,10,10,10,10,10,10))


kpop_name_1 <- data.frame(lapply(x, rep, x$nb_times))[1:3]

t_20 <- 
  popular_data %>% 
  select(name, gender, generation) |>
  slice(11:20)|>
  rename(
    artist_name = name
  )


y <- data.frame(t_20,
                nb_times = c(10,10,10,10,10,4,10,10,10,10))

kpop_name_2 <- data.frame(lapply(y, rep, y$nb_times))[1:3]

#combine all data to get audio data for each artist 
song_data_check <- cbind(get_track_audio_features(kpop_audio1$id),kpop_name_1, kpop_audio1 %>% select(name, popularity, album.release_date))


song_data_1 <- cbind(get_track_audio_features(kpop_audio1$id),kpop_name_1, kpop_audio1 %>% select(name, popularity, album.release_date))
song_data_2 <- cbind(get_track_audio_features(kpop_audio2$id),kpop_name_2, kpop_audio2 %>% select(name, popularity, album.release_date))

audio_data <- rbind(song_data_1, song_data_2) 

audio_data <- 
audio_data %>% 
  rename(
    song_name = name
  )

audio_data <- 
  audio_data %>% 
  select(danceability, energy, key, mode, loudness, speechiness, acousticness, 
         instrumentalness, liveness, valence, tempo, artist_name, song_name, 
         gender, generation, popularity, album.release_date )

write.csv(audio_data, "inputs/data/raw_data_audio.csv", row.names=FALSE)











