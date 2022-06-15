# Data processing
library(readxl)
library(tidyverse)
library(gganimate)
library(gifski)
library(png)

list <- read_excel("listeOfSids.xlsx")
gdp <- read_excel("interactive map/gdpPerCapitaInternationUSD.xls")

sidsGdp <- list %>%
  left_join (gdp, by=c("country_name"="Country Name")) %>%
  select(1,2,45:65) %>%
  na.omit() %>%
  gather(year,gdp_per_capita,"2000":"2020") %>%
  mutate(year=as.integer(year), gdp_per_capita = round(gdp_per_capita))

# African SIDS animated line

data2 %>% filter(year == 2012) %>%
  ggplot() +
  geom_col(aes(ranking,gdp_per_capita,fill=country_name),width = 0.7) +
  geom_text(aes(ranking,gdp_per_capita,label = as.character(gdp_per_capita)),hjust = -0.1, alpha = 0.5, size = 3) +
  geom_text (aes(ranking,y = 0, label = paste(country_name," "),hjust = 1)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(x=20, y=60000 , label = as.factor(year)), vjust = 1, alpha = 0.5, col = "gray", size = 15) +
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
    theme_minimal() + theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(3, 2, 2, 6, "cm")) +
  labs (title = "Small Islands Developping States",
        subtitle = "GDP per Capita 2000-2020",
        caption = "Data Source: World Bank - GDP per capita, PPP (constant 2017 international $)") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 10, size = 30),
        plot.subtitle = element_text(hjust = 0.5,vjust = 9,size=25))

# 'Data Source: World Bank'




data2 <- sidsGdp %>%
  group_by(year) %>%
  arrange(year,desc(gdp_per_capita))%>%
  mutate(ranking = row_number())





graph2 <- data2 %>%
  ggplot() +
  geom_col(aes(ranking,gdp_per_capita,fill=country_name),width = 0.7) +
  geom_text(aes(ranking,gdp_per_capita,label = as.character(gdp_per_capita)),hjust = -0.1, alpha = 0.5, size = 3) +
  geom_text (aes(ranking,y = 0, label = paste(country_name," "),hjust = 1)) +
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(x=20, y=60000 , label = as.factor(year)), vjust = 1, alpha = 0.5,  col = "gray", size = 15) +
  coord_flip(clip = "off", expand = FALSE) + scale_x_reverse() +
  theme_minimal() + theme(
    panel.grid = element_blank(),
    legend.position = "none",
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    plot.margin = margin(3, 2, 2, 6, "cm")) +
  labs (title = "Small Islands Developping States",
        subtitle = "GDP per Capita 2000-2020",
        caption = "Data Source: World Bank - GDP per capita, PPP (constant 2017 international $)") +
  theme(plot.title = element_text(hjust = 0.5,vjust = 10, size = 30),
        plot.subtitle = element_text(hjust = 0.5,vjust = 9, size=25)) +
  transition_states(year, state_length = 0, transition_length = 2) +
  enter_fade() +
  exit_fade() +
  ease_aes('quadratic-in-out')




anim_sids <- animate(graph2,width = 1745, height = 910, fps = 25, duration = 15, rewind = FALSE)
anim_save("gdp_sids.gif",animation = anim_sids)
