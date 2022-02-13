#highest rated movies
movies %>% 
  arrange(desc(averageRating)) %>% 
  slice_head(n = 10)

movies %>% 
  filter(numVotes >= 1000) %>% 
  arrange(desc(averageRating) )%>% 
  slice_head(n = 10)

movies %>% 
  filter(numVotes >= 10000) %>% 
  arrange(desc(averageRating)) %>% 
  slice_head(n = 10)

movies %>% 
  filter(numVotes >= 100000) %>% 
  arrange(desc(averageRating)) %>% 
  slice_head(n = 10)

movies %>% 
  filter(numVotes >= 500000) %>% 
  arrange(desc(averageRating)) %>% 
  slice_head(n = 10)

movies %>% 
  filter(numVotes >= 1000000) %>% 
  arrange(desc(averageRating)) %>% 
  slice_head(n = 10)

movies %>%
  mutate(total_votes = numVotes * averageRating) %>% 
  arrange(desc(total_votes)) %>% 
  slice_head(n = 10)
  

#average rating by decade
movies %>% 
  filter(between(decade, 1910, 2010)) %>% 
  ggplot() +
  geom_boxplot(aes(decade, averageRating, group = decade), varwidth = FALSE, outlier.alpha = .1) +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  xlab("Decade") +
  ylab("Rating")

#highest rating by genre
genres %>% 
  pivot_longer(action:western, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(genre) %>% 
  mutate(total_ratings = numVotes * averageRating) %>% 
  arrange(desc(total_ratings)) %>% 
  select(genre, title, year, runtime, averageRating, numVotes) %>% 
  slice_head(n = 1) %>% 
  print(n = Inf)

genres %>% 
  pivot_longer(action:western, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(genre) %>% 
  filter(numVotes >= 100000) %>% 
  arrange(desc(averageRating)) %>% 
  select(genre, title, year, runtime, averageRating, numVotes) %>% 
  slice_head(n = 1) %>% 
  print(n = Inf)

genres %>% 
  pivot_longer(action:western, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(genre) %>% 
  filter(numVotes >= 500000) %>% 
  arrange(desc(averageRating)) %>% 
  select(genre, title, year, runtime, averageRating, numVotes) %>% 
  slice_head(n = 1) %>% 
  print(n = Inf)


genres %>% 
  pivot_longer(action:western, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(genre) %>% 
  filter(numVotes >= 1000000) %>% 
  arrange(desc(averageRating)) %>% 
  select(genre, title, year, runtime, averageRating, numVotes) %>% 
  slice_head(n = 1) %>% 
  print(n = Inf)


#average rating by genre
genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary, romance, action, crime, thriller, horror, adventure, music) %>% 
  pivot_longer(drama:music, names_to = "genre") %>% 
  filter(value) %>% 
  ggplot() +
  geom_boxplot(aes(reorder(genre, averageRating, FUN = median), averageRating), outlier.alpha = .1) +
  xlab("Genre") +
  ylab("Rating")

genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary, romance, action, crime, thriller, horror, adventure, music) %>% 
  filter(numVotes >= 1000) %>% 
  pivot_longer(drama:music, names_to = "genre") %>% 
  filter(value) %>% 
  ggplot() +
  geom_boxplot(aes(reorder(genre, averageRating, FUN = median), averageRating), outlier.alpha = .1) +
  xlab("Genre") +
  ylab("Rating") +
  ggtitle("Rating Distribution Grouped by Genre", "filtered on number votes at least 1000")

#average rating by genre and decade
genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary, romance, action, crime, thriller, horror, adventure, music) %>% 
  filter(between(decade, 1920, 2010)) %>% 
  pivot_longer(drama:music, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(decade, genre) %>% 
  summarise(meanRating = mean(averageRating)) %>% 
  ggplot() +
  geom_tile(aes(decade, reorder(genre, meanRating, FUN = mean), fill = meanRating)) +
  scale_x_continuous(breaks = seq(1920, 2010, 10)) +
  xlab("Decade") +
  ylab("Genre")

genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary, romance, action, crime, thriller, horror, adventure, music) %>% 
  filter(numVotes >= 1000, between(decade, 1920, 2010)) %>% 
  pivot_longer(drama:music, names_to = "genre") %>% 
  filter(value) %>% 
  group_by(decade, genre) %>% 
  summarise(averageRating = mean(averageRating)) %>% 
  ggplot() +
  geom_tile(aes(decade, reorder(genre, averageRating, FUN = mean), fill = averageRating)) +
  scale_x_continuous(breaks = seq(1920, 2010, 10)) +
  xlab("Decade") +
  ylab("Genre") +
  ggtitle("Mean Rating by Genre and Decade", "filtered on number votes at least 1000")

#3 most popular genres rating distributions over time
genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary) %>% 
  pivot_longer(drama:documentary, names_to = "genre") %>% 
  filter(value, between(decade, 1910, 2010)) %>% 
  ggplot() +
  geom_boxplot(aes(x = decade, y = averageRating, group = decade), outlier.alpha = .1) +
  facet_wrap(~ reorder(genre, averageRating, FUN = median)) +
  scale_x_continuous(breaks = seq(1910, 2010, 20)) +
  xlab("Decade") +
  ylab("Rating")

#with factors you could reorder the genres
genres %>% 
  select(title, year, decade, numVotes, averageRating, drama, comedy, documentary) %>% 
  pivot_longer(drama:documentary, names_to = "genre") %>% 
  filter(value, between(decade, 1910, 2010)) %>% 
  ggplot() +
  geom_boxplot(aes(x = decade, y = averageRating, group = interaction(decade, genre), color = genre), outlier.alpha = .2) +
  xlab("Decade") +
  ylab("Rating") +
  scale_x_continuous(breaks = seq(1910, 2010, 10))
