#initialize from rds files
library(tidyverse)
library(scales)

all_movies <- read_rds("allmovies.rds")
genres <- read_rds("genres.rds")
movies <- filter(all_movies, !is.na(title), year < 2022, !is.na(runtime), !is.na(genres))
sumgenres <- genres %>% 
  summarize(action = sum(action), adult = sum(adult), adventure = sum(adventure), animation = sum(animation), biography = sum(biography),
            comedy = sum(comedy), crime = sum(crime), documentary = sum(documentary), drama = sum(drama), family = sum(family),
            fantasy = sum(fantasy), filmnoir = sum(filmnoir), gameshow = sum(gameshow), history = sum(history), horror = sum(horror),
            music = sum(music), musical = sum(musical), mystery = sum(mystery), news = sum(news), romance = sum(romance),
            scifi = sum(scifi), short = sum(short), sport = sum(sport), thriller = sum(thriller), realitytv = sum(realitytv),
            war = sum(war), western = sum(western)) %>% 
  pivot_longer(action:western, names_to = "genre", values_to = "n")


#initialize rds files
titles <- read_tsv("movies.tsv", col_types = cols_only(
  tconst = "c", titleType = "f", primaryTitle = "c", startYear = "i", runtimeMinutes = "i", genres = "c"), na = "\\N")
ratings <- read_tsv("ratings.tsv", col_types = "cdi")

all_movies <- titles %>% 
  inner_join(ratings, by = c("tconst")) %>% 
  filter(titleType == "movie") %>%
  select(-titleType, -tconst) %>% 
  rename(title = primaryTitle, year = startYear, runtime = runtimeMinutes) %>% 
  mutate(decade = year %/% 10 * 10)
write_rds(all_movies, "allmovies.rds")

genres <- movies %>% 
  mutate(action = str_detect(genres, regex("action", ignore_case = TRUE)),
         adult = str_detect(genres, regex("adult", ignore_case = TRUE)),
         adventure = str_detect(genres, regex("adventure", ignore_case = TRUE)),
         animation = str_detect(genres, regex("animation", ignore_case = TRUE)),
         biography = str_detect(genres, regex("biography", ignore_case = TRUE)),
         comedy = str_detect(genres, regex("comedy", ignore_case = TRUE)),
         crime = str_detect(genres, regex("crime", ignore_case = TRUE)),
         documentary = str_detect(genres, regex("documentary", ignore_case = TRUE)),
         drama = str_detect(genres, regex("drama", ignore_case = TRUE)),
         family = str_detect(genres, regex("family", ignore_case = TRUE)),
         fantasy = str_detect(genres, regex("fantasy", ignore_case = TRUE)),
         filmnoir = str_detect(genres, regex("film-noir", ignore_case = TRUE)),
         gameshow  = str_detect(genres, regex("game-show", ignore_case = TRUE)),
         history = str_detect(genres, regex("history", ignore_case = TRUE)),
         horror = str_detect(genres, regex("horror", ignore_case = TRUE)),
         music = str_detect(genres, regex("music", ignore_case = TRUE)),
         musical = str_detect(genres, regex("musical", ignore_case = TRUE)),
         mystery = str_detect(genres, regex("mystery", ignore_case = TRUE)),
         news = str_detect(genres, regex("news", ignore_case = TRUE)),
         romance = str_detect(genres, regex("romance", ignore_case = TRUE)),
         scifi = str_detect(genres, regex("sci-fi", ignore_case = TRUE)),
         short = str_detect(genres, regex("short", ignore_case = TRUE)),
         sport = str_detect(genres, regex("sport", ignore_case = TRUE)),
         thriller = str_detect(genres, regex("thriller", ignore_case = TRUE)),
         realitytv = str_detect(genres, regex("reality-tv", ignore_case = TRUE)),
         war = str_detect(genres, regex("war", ignore_case = TRUE)),
         western = str_detect(genres, regex("western", ignore_case = TRUE)))
write_rds(genres, "genres.rds")
