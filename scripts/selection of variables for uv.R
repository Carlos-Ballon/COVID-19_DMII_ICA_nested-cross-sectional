var_selec_uv <- function(data, valor_filtro) {
  data |>
    dplyr::filter(t2dm == valor_filtro) |>
    dplyr::select(
      # Demographic characteristics and clinical history
      edad.c,
      sex,
      hta,
      obesity,
      
      # Signs and symptoms
      fever,
      tos,
      taquipnea,
      disnea,
      
      # Vital signs
      frecuencia_cardiaca.c,
      frecuencia_respiratoria.c,
      p_a_sistolica.c,
      p_a_diastolica.c,
      
      # Laboratory findings
      wbc.c,
      neutrofilos.c,
      linfocitos.c,
      nlr.c,
      platelets.c,
      saturacion_de_oxigeno.c,
      fio2.c,
      pafi.c,
      pao2.c,
      
      # Treatment
      corticoides,
      anticoagulantes,
      antipaludicos,
      pronacion,
      
      # outcomes
      outcome
    ) |>
    
    na.omit()
}