tabla <- eph %>% 
  group_by(ano_trim) %>% 
  mutate(imputados = ifelse(P47T == -9, 1, 0)) %>% 
  summarise(imputados = mean(imputados, na.rm = T))

tabla %>% 
  ggplot(aes(x = ano_trim, y = imputados, group = 1)) +
  geom_line() +
  labs(title = "Proporción de imputados por trimestre en el ingreso individual",
       x = "Año-trimestre",
       y = "Proporción de imputados") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, size = 8, hjust = 1)) +
  scale_y_continuous(labels = scales::percent)

ggsave("salidas/prop_imputados_ingreso_individual.png", width = 10, height = 6, dpi = 300)
