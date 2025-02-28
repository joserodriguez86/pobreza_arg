# Cargo modelos----------

tree_rf1 <- readRDS("modelos entrenados/tree_rf1.rds")

test_20173 <- augment(tree_rf1, new_data = test_20173) 


test_20173 %>%
  group_by(pobreza_dic) %>% 
  tally(PONDIH) %>% 
  janitor::adorn_percentages("col")

test_20173 %>%
  group_by(.pred_class) %>% 
  tally(PONDIH) %>% 
  janitor::adorn_percentages("col")
