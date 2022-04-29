


kpop <- read_csv("inputs/data/raw_data_popular.csv")

kpop_b <- read_csv("inputs/data/raw_data_audio.csv")
#make table from this data 

#Popularity 

kpop |>
  #select(name, popularity, followers.total) |> 
  ggplot(mapping = aes(x = reorder(name, -popularity), y = popularity, fill = generation)) +
  geom_bar(stat = "identity")+ 
  theme_bw()+
  labs(x = "Kpop Groups",
       y = "Spotify Popularity",
       fill = "Gen") 


#follwers 
follow<- 
  kpop |>
  #slice(1:10) |> 
  #select(name, popularity, followers.total) |> 
  ggplot(mapping = aes(x = reorder(name, -followers.total), y = followers.total, fill = generation)) +
  geom_bar(stat = "identity")+
  theme_bw()+
  labs(x = "Kpop Groups",
       y = "Spotify Followers",
       fill = "Gen") 

#Table 

#smaller table 
ggsave("font_test2_10x10_300.png", plot = follow, width = 20, height = 20, units = "in", dpi = 300)


#use this data shows how 4th gen in popular 
kpop_b |>
  ggplot(aes(x = album.release_date,
             y = popularity,
             color = generation)) +
  geom_point(alpha = 0.5) +
  geom_smooth() +
  theme_minimal() +
  labs(x = "Album release date",
       y = "Popularity",
       color = "Artist") +
  scale_color_brewer(palette = "Set1")

kpop_b |>
  #filter(popularity < 20) |> 
  ggplot(aes(x = popularity, fill = generation)) + 
  geom_bar() +
  labs(x = "Song popularity ",
       caption = "Distribution of song popularity in generation")


#for shiny 
kpop_b |>
  #filter(popularity < 20) |> 
  ggplot(aes(x = popularity, fill = generation)) + 
  geom_bar(position = "dodge")+
  theme_minimal()+
  scale_fill_brewer(palette = "Set2")

#table 1 
#instead of table 1 in 
# These included musical notational predictors such as time signature, mode, 
#key, along with audio differentiators like speechiness, intrustmentalsness, and acousticness,
#that measure the presence of spoken words, vocals and use of  instrumentals, accordingly. 
#Other quantifiers includes a songs “danceability”, “energy”, “tempo”,”liveness”, “loudness”, “valence”, or
#the musical positiveness conveyed by a track, “tempo” , “track duration”, and ”explicitly of a song” measured in true or false. 

kpop_a |> 
  datasummary_skim(type = "numeric",
                   title = "Summary of audio features ditrubution ")

tab <- matrix(rep(1, times=22), ncol=2, byrow=TRUE)
colnames(tab) <- c('name', 'dis')
rownames(tab) <- c('danceability','energy','key', 'mode','loudness','speechiness','acousticness','instrumentalness','liveness',
                   'valence','tempo')
r <- data.frame(tab)
df |> 
  mutate(dis = case_when(name == "danceability" ~ "SVT"))
tab <- as.table(tab)

df <- data.frame(name=c('danceability','energy','key', 'mode','loudness','speechiness','acousticness','instrumentalness','liveness',
                           'valence','tempo'), 
                 discribtion=c('Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable.','Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy',
                               'The key the track is in. Integers map to pitches using standard Pitch Class notation. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on. If no key was detected, the value is -1.',
                               'Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0.',
                               'The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typically range between -60 and 0 db.',
                               'Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks.',
                               'A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.',
                               'Predicts whether a track contains no vocals. "Ooh" and "aah" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly "vocal". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0.','Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.',
                               'A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry).','The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration.'))


df |> 
  kable(
    caption = "Discribtion of Audio Features",
    digits = 1,
    booktabs = TRUE, 
    linesep = "")



# eq log 


Pr(yi=1)=logit-1(β0+β1xi)

ggg <- data.frame('.'=rep(c('Prediction', '3rd Gen', '4th Gen'), each=1),
                  Truth=rep(c('3rd Gen', '15', '4' ), times=1),
                 ".."=(c("4th Gen", 6, 14)))

