olympics %>% filter(medal %in% c("🥈") &
                      team %in% "India") %>% select(sport) %>% unique()
unique(olympics$sport[olympics$team %in% input$cou &
                        olympics$medal %in% c("🥇", "🥈", "🥉")])

glimpse(olympics)
labels <- c(medal(1), medal(2), medal(3))

# olympics <- olympics %>% mutate(medal = case_when(
#   medal == "Gold" ~ medal(1),
#   medal == "Silver" ~ "🥈",
#   medal == "Bronze" ~ "🥉",
# ))
# 
# olympics <- olympics %>% mutate(label=case_when(
#                     medal == "\U0001f947" ~ "Gold",
#                     medal == "\U0001f949" ~ "Bronze",
#                     medal == "\U0001f948" ~ "Silver",
#                       ))

olympics %>% filter(medal %in% c("Gold", "Silver", "Bronze") &
                      team == "Australia" & year == 2016) %>%
  count(medal) %>% ggplot(aes(x = medal,
                              y = n,
                              fill = medal)) + geom_col() + geom_text(
                                aes(label = n),
                                vjust = 0.5,
                                color = "black",
                                position = position_stack(0.5),
                                size = 4.0
                              ) + scale_fill_manual(name="Medal Type",breaks = c("Bronze", "Gold", "Silver"),
                                                     values = c("#E69F00", "gold", "#999999")) +
  labs(title ="All Athletes Indivudual Medal Count per Country",x="Medal Type",y="Medal Count")+  scale_y_continuous(limits = c(0, 50))+coord_flip()+theme_classic()+theme(
  plot.background = element_rect(fill = NA, color = "cyan", size = 4),
  legend.background = element_rect(linetype = "dashed"),
  legend.text = element_text(size = 15),
  legend.title.align = 0.5,
  legend.box="vertical"
)+annotation_custom(img,ymin = 45,ymax = 50)
