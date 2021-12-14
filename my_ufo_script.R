library(tidyverse) # collection of packages for data wrangling/visualisation
library(patchwork)

# read in our data # using a different function can read in txt. files etc
ufo_sightings <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-25/ufo_sightings.csv") # csv allows rectangular format (columns & rows)

# plot of top 10 US states with number of sightings in each state
plot1 <- ufo_sightings %>% # %>% called the pipe and is read as "and then"
  filter(!is.na(state)) %>% # excluding missing data points
  mutate(state = str_to_upper(state)) %>% # mutating means changing one of the columns
  group_by(state) %>%
  tally() %>%
  top_n(10) %>%
  ggplot(aes(x = reorder(state, n), y = n , fill = state)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(title = "Top 10 States for UFO Sightings",
       x = NULL,
       y = NULL) +
  ylim(0, 11000) +
  theme_minimal() + # keeps theme minimal e.g. plain background and no dashes on axis
  theme(text = element_text(size = 15))

# work out states within lat and long limits (i.e., exclude Alaska)
tidied_ufo <- ufo_sightings %>% # <- = says map everything on the right hand side of the expression to the name on the left
  filter(country == "us") %>%
  filter(latitude > 24 & latitude < 50)

# plot all sightings on a map of the US
plot2 <- tidied_ufo %>%
  ggplot(aes(x = longitude, y = latitude)) + # ggplot grammar of graphics - build a basic skeleton of the visulisation then build on layers of the plot
  geom_point(size = .5, alpha = .25) + # then add geom, the geometry can be points, crosses or columns etc.
  theme_void() + #removes certain aesthetics - alpha means translucency of the points
  coord_cartesian() +
  labs(title = "Site of UFO sightings in the US") +
  theme(text = element_text(size = 15)) # ensure we have spaces when using operators (+, =, > etc)

# plot of top 10 UFO shapes spotted in California
plot3 <- tidied_ufo %>%
  filter(state == "ca") %>%
  filter(ufo_shape != "other") %>%
  filter(ufo_shape != "unknown") %>%
  group_by(ufo_shape) %>%
  tally() %>%
  top_n(10) %>%
  mutate(ufo_shape = str_to_title(ufo_shape)) %>%
  ggplot(aes(x = reorder(ufo_shape, n), y = n, fill = ufo_shape)) +
  geom_col() +
  coord_flip() +
  guides(fill = "none") +
  labs(title = "Top 10 UFO shapes spotted in California",
       x = NULL,
       y = NULL) +
  theme_minimal() +
  theme(text = element_text(size = 15))

# Put plots together
my_plot <- (plot1 + plot3) / (plot2)

# saved to form a image can be seen in the ufo_sightings_project
ggsave("ufo_plot.jpg", plot = my_plot, width = 12, height = 10)


       
       