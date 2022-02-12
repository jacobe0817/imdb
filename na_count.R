#movies per year
movies %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1890, 2020, 10)) +
  xlab("Year") +
  ylab("# of Movies")

#movies with missing information
all_movies %>% 
  summarize(title = sum(is.na(title)), year = sum(is.na(year)), runtime = sum(is.na(runtime)), genres = sum(is.na(genres))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "count") %>% 
  mutate(variable = reorder(variable, count)) %>% 
  ggplot() +
  geom_col(aes(variable, count)) +
  xlab("Information Missing") +
  ylab("# of Movies")

all_movies %>% 
  summarize(title = sum(is.na(title)), year = sum(is.na(year)), runtime = sum(is.na(runtime)), genres = sum(is.na(genres))) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "count") %>% 
  mutate(variable = reorder(variable, count)) %>% 
  ggplot() +
  geom_col(aes(variable, count)) +
  coord_cartesian(ylim = c(0, 500)) +
  xlab("Information Missing") +
  ylab("# of Movies")


#movies missing runtime or genre vs year
all_movies %>% 
  filter(is.na(runtime) | is.na(genres), year < 2022) %>% 
  count(year) %>% 
  ggplot() +
  geom_line(aes(year, n)) +
  scale_x_continuous(breaks = seq(1890, 2020, 10)) +
  xlab("Year") +
  ylab("# of Movies Missing Runtime or Year Info")


all_movies %>% 
  filter(year < 2022) %>% 
  mutate(missing = is.na(runtime) | is.na(genres)) %>% 
  group_by(year) %>% 
  filter(n() >= 10) %>% 
  summarize(prop = mean(missing)) %>% 
  ggplot() +
  geom_line(aes(year, prop)) +
  scale_x_continuous(breaks = seq(1890, 2020, 10)) +
  xlab("Year") +
  ylab("Yearly Proportion of Movies Missing Runtime or Year Info")
