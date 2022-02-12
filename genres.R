#most popular genres
sumgenres %>%
  arrange(desc(n)) %>% 
  slice_head(n = 10)

sumgenres %>% 
  ggplot() +
  geom_col(aes(reorder(genre, -n), n)) +
  scale_y_continuous(breaks = seq(0, 100000, 25000)) +
  coord_flip() +
  xlab("Genre") +
  ylab("# of Movies")

#genre popularity facet
genres %>%
  filter(year < 2022) %>% 
  group_by(year) %>% 
  summarize(drama = sum(drama), comedy = sum(comedy), documentary = sum(documentary), romance = sum(romance), action = sum(action),
         crime = sum(crime), thriller = sum(thriller), horror = sum(horror), adventure = sum(adventure), music = sum(music)) %>% 
  pivot_longer(drama:music, names_to = "genre", values_to = "n") %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  facet_wrap(~ genre, nrow = 2) +
  xlab("Year") +
  ylab("# of Movies")

genres %>%
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  summarize(drama = mean(drama), comedy = mean(comedy), documentary = mean(documentary), romance = mean(romance), action = mean(action),
            crime = mean(crime), thriller = mean(thriller), horror = mean(horror), adventure = mean(adventure), music = mean(music)) %>% 
  pivot_longer(drama:music, names_to = "genre", values_to = "prop") %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  facet_wrap(~ genre, nrow = 2) +
  scale_x_continuous(breaks = seq(1920, 2000, 40)) +
  xlab("Year") +
  ylab("Yearly Proportion of Movies")

#genre popularity geom tile
genres %>% 
  filter(year < 2020) %>% 
  group_by(decade) %>% 
  summarize(drama = sum(drama), comedy = sum(comedy), documentary = sum(documentary), romance = sum(romance), action = sum(action),
            crime = sum(crime), thriller = sum(thriller), horror = sum(horror), adventure = sum(adventure), music = sum(music)) %>% 
  pivot_longer(drama:music, names_to = "genre", values_to = "n") %>% 
  ggplot() +
  geom_tile(aes(decade, reorder(genre, n, FUN = mean), fill = n)) +
  scale_x_continuous(breaks = seq(1890, 2010, 10)) +
  xlab("Decade") +
  ylab("Genre")

genres %>% 
  filter(year < 2020) %>% 
  group_by(decade) %>% 
  filter(n() >= 10) %>% 
  summarize(drama = mean(drama), comedy = mean(comedy), documentary = mean(documentary), romance = mean(romance), action = mean(action),
            crime = mean(crime), thriller = mean(thriller), horror = mean(horror), adventure = mean(adventure), music = mean(music)) %>% 
  pivot_longer(drama:music, names_to = "genre", values_to = "prop") %>% 
  ggplot() +
  geom_tile(aes(decade, reorder(genre, prop, FUN = mean), fill = prop)) +
  scale_x_continuous(breaks = seq(1900, 2010, 10))


#specific genres' popularity over time, absolute and relative
#war
genres %>% 
  filter(year < 2022, war) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(war)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#scifi
genres %>% 
  filter(year < 2022, scifi) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(scifi)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#fantasy
genres %>% 
  filter(year < 2022, fantasy) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(fantasy)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#family
genres %>% 
  filter(year < 2022, family) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(family)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#western
genres %>% 
  filter(year < 2022, western) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(western)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#history
genres %>% 
  filter(year < 2022, history) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(history)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

#film noir
genres %>% 
  filter(year < 2022, filmnoir) %>% 
  group_by(year) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))

genres %>% 
  filter(year < 2022) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  mutate(prop = mean(filmnoir)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1920, 2020, 10))
