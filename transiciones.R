# Pares de transición
transiciones <- list(
  "2023 T3",
  "2023 T4",
  "2024 T1",
  "2024 T2",
  "2024 T3",
  "2024 T4"
)

# Función corregida
tabla_transicion <- function(periodo_actual) {
  pool %>% 
    filter(Periodo == periodo_actual, 
           !is.na(pobreza_f)) %>%
    group_by(pobreza_f_t1, pobreza_f) %>%
    summarise(n = sum(pondih_sin_elevar_t1), .groups = "drop") %>%
    group_by(pobreza_f) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    pivot_wider(names_from = pobreza_f_t1, values_from = c(n, pct), values_fill = 0)
}

# Aplicar a cada período (T) → transición T-1 a T
tablas_transicion <- map(transiciones, tabla_transicion)

# Acceder a cada tabla
tabla1 <- tablas_transicion[[1]]  # 2023 T3 → T4
tabla2 <- tablas_transicion[[2]]  # 2023 T4 → 2024 T1
tabla3 <- tablas_transicion[[3]]  # 2024 T1 → T2
tabla4 <- tablas_transicion[[4]]  # 2024 T2 → T3
tabla5 <- tablas_transicion[[5]]  # 2024 T3 → T4




