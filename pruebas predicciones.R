# Cargo modelos----------

tree_rf <- readRDS("modelos entrenados/tree_rf.rds")

test <- augment(tree_rf, new_data = test) 

test <- test %>%
  mutate(pobreza_predicha = ifelse(.pred_Pobre >= optimal_threshold, "Pobre", "No pobre"))

tabla <- test %>%
  group_by(ano_trim, pobreza_dic) %>% 
  tally() %>% 
  group_by(ano_trim) %>%
  mutate(prop.real = n / sum(n)) %>% 
  filter(!is.na(pobreza_dic)) %>% 
  select(-n)

tabla2 <- test %>%
  group_by(ano_trim, pobreza_predicha) %>% 
  tally() %>% 
  group_by(ano_trim) %>%
  mutate(prop.pred = n / sum(n)) %>% 
  rename(pobreza_dic = pobreza_predicha) %>% 
  select(-n)

tabla3 <- test %>% 
  group_by(ano_trim, .pred_class) %>% 
  tally() %>% 
  group_by(ano_trim) %>%
  mutate(prop.pred2 = n / sum(n)) %>% 
  rename(pobreza_dic = .pred_class) %>% 
  select(-n)

tabla <- tabla %>% 
  left_join(tabla2, by = c("ano_trim", "pobreza_dic")) %>% 
  left_join(tabla3, by = c("ano_trim", "pobreza_dic"))

tabla <- tabla %>% 
  pivot_longer(cols = c(prop.real, prop.pred, prop.pred2), names_to = "tipo", values_to = "prop")

tabla <- tabla %>% 
  filter(pobreza_dic != "No pobre")

tabla %>% 
  ggplot(aes(x = ano_trim, y = prop, fill = pobreza_dic, group = tipo)) +
  geom_line(aes(linetype = tipo), size = 1)
