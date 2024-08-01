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
      frec_cardiaca.c,
      frec_respiratoria.c,
      SBP.c,
      DBP.c,
      
      # Laboratory findings
      WBC.c,
      neutrofilos.c,
      linfocitos.c,
      NLR.c,
      platelets.c,
      SaO2.c,
      FiO2.c,
      PaO2_FiO2.c,
      PaO2.c,
      
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