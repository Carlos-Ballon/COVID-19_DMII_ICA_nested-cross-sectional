# Patients with T2DM
var_selec_sens <- function(data, var, valor_filter) {
  data |>
    dplyr::filter({
      {
        var
      }
    } == valor_filter) |>
    dplyr::select(
      # Demographic characteristics and clinical history
      edad,
      hta,
      
      # Signs and symptoms
      disnea,
      
      # Vital signs
      frecuencia_cardiaca,
      frecuencia_respiratoria,
      p_a_sistolica,
      p_a_diastolica,
      
      # Laboratory findings
      wbc,
      neutrofilos,
      linfocitos,
      nlr,
      platelets,
      saturacion_de_oxigeno,
      fio2,
      pafi,
      pao2,
      
      # outcomes
      outcome
    ) |>
    
    na.omit()
}