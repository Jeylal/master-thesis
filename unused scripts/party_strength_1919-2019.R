library(tidyverse)

force_parti_ofs2 <- 
  force_parti_ofs %>% 
  rename(year = "...1") %>% 
  pivot_longer(cols = 2:25, names_to = "party")

force_parti_ofs2 %>% 
  filter(party %in% c("UDC", "PS", "PLR (PRD) 3)", "PDC", "PES", "PVL")) %>% 
  ggplot()+
  aes(x = year, y = value, color = party, shape = party)+
  geom_line(linewidth = 1)+
  geom_point(size = 3)+
  theme_minimal()+
  scale_color_brewer(palette = "Set1")+
  scale_x_continuous(breaks = seq(from = 1919, to = 2019, by = 4))+
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
    text = element_text(size = 17),
    legend.position = "bottom"
  )+
  labs(title = "Party Strength",
       subtitle = "Swiss Federal Election, 1919-2019",
       x = "", y = "")

names(force_parti_ofs)

force_parti_ofs2 <- 
  force_parti_ofs2 %>% 
  mutate(
    bourgeois_parties = PDC+PVL+PBD
  )
