# Patients with T2DM
var_selec_sens <- function(data, var, valor_filter) {
  data |>
    dplyr::filter({{ var }} == valor_filter) |>
    dplyr::select(
      # Demographic characteristics and clinical history
      edad,
      hta,
      
      # Signs and symptoms
      disnea,
      
      # Vital signs
      frec_cardiaca,
      frec_respiratoria,
      SBP,
      DBP,
      
      # Laboratory findings
      WBC,
      neutrofilos,
      linfocitos,
      NLR,
      platelets,
      SaO2,
      FiO2,
      PaO2_FiO2,
      PaO2,
      
      # outcomes
      outcome
    ) |>
    
    na.omit()
}