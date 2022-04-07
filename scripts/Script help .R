install.packages("spotifyr")
install.packages("tidyverse")
library(spotifyr)
library(tidyverse)
library(usethis)
edit_r_environ()

#select top 20
kpop <- get_genre_artists('k-pop')
#add bg andgg column 
kpop |>
  select(name, popularity, followers.total) |>
  mutate(gender = case_when(
    name == "SEVENTEEN" ~ "bg"))%>%
#add 4th gen and 3rd gen identifiers 
# add txt and svt along with other short hand 
  #add debut date 
  

  
  dataf <- kpop %>% 
  mutate(gender = case_when(
    name == "SEVENTEEN" ~ "bg"))
head()


kpop <- 
  kpop %>% 
  #mutate(age = replace_na(age, 0))%>%
  mutate(name = case_when(
    name == "SEVENTEEN" ~ "SVT"))
#Popularity 

kpop |>
  select(name, popularity, followers.total) |> 
  ggplot(mapping = aes(x = reorder(name, -popularity), y = popularity)) +
  geom_bar(stat = "identity")
 

#follwers 
follow<- 
  kpop |>
  #slice(1:10) |> 
  select(name, popularity, followers.total) |> 
  ggplot(mapping = aes(x = reorder(name, -followers.total), y = followers.total)) +
  geom_bar(stat = "identity")


#smaller table 
ggsave("font_test2_10x10_300.png", plot = follow, width = 20, height = 20, units = "in", dpi = 300)




