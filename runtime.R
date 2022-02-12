#longest movies
movies %>% 
  select(runtime, title, genres, year) %>% 
  arrange(desc(runtime)) %>% 
  slice_head(n = 10)

#runtime distribution
ggplot(movies) +
  geom_boxplot(aes(runtime)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Runtime (minutes)") +
  ggtitle("Runtime Distribution of All Movies")

ggplot(movies) +
  geom_boxplot(aes(runtime), outlier.alpha = .1) +
  coord_cartesian(xlim = c(0, 300)) +
  scale_x_continuous(breaks = seq(0, 300, 30)) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  xlab("Runtime (minutes)") +
  ggtitle("Runtime Distribution of All Movies", subtitle = "Zoomed in on runtimes of 5 hours or less")

#median runtime vs year
movies %>%
  group_by(year) %>%
  summarize(medianrun = median(runtime), n = n()) %>% 
  ggplot() +
  geom_point(aes(year, medianrun, alpha = n)) +
  xlab("Year") +
  ylab("Median Runtime (minutes)") +
  ggtitle("Median Runtime vs Year") +
  scale_x_continuous(breaks = seq(1890, 2020, 10)) +
  scale_y_continuous(breaks = seq(0, 120, 30))

#runtime distributions grouped by decades
movies %>%
  filter(year < 2020, year >= 1910) %>% 
  ggplot() +
  geom_boxplot(aes(decade, runtime, group = decade), varwidth = TRUE, outlier.alpha = .1) +
  xlab("Decade") +
  ylab("Runtime (minutes)") +
  coord_cartesian(ylim = c(0, 300)) +
  ggtitle("Runtime vs Release Decade", "Zoomed in on runtimes of 5 hours or less") +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 300, 60))

movies %>%
  filter(year < 2020, year >= 1910, runtime <= 300) %>% 
  ggplot() +
  geom_violin(aes(decade, runtime, group = decade)) +
  xlab("Decade") +
  ylab("Runtime (minutes)") +
  coord_cartesian(ylim = c(0, 300)) +
  ggtitle("Runtime vs Decade", "Filtered on runtimes of 5 hours or less") +
  scale_x_continuous(breaks = seq(1910, 2010, 10)) +
  scale_y_continuous(breaks = seq(0, 300, 60))
