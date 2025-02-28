metricas_log %>% 
  filter(tipo == "Entrenamiento") %>% 
  summarise(precision_mean = mean(precision), recall_mean = mean(recall), f1_mean = mean(f_meas), accuracy_mean = mean(accuracy), auc_mean = mean(roc_auc))

metricas_rf %>% 
  filter(tipo == "Entrenamiento") %>% 
  summarise(precision_mean = mean(precision), recall_mean = mean(recall), f1_mean = mean(f_meas), accuracy_mean = mean(accuracy), auc_mean = mean(roc_auc))
