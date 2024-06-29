PCA_data_select <- function(data, var, valor_filter) {
  # Variable selection
  data <- data |>
    dplyr::filter({
      {
        var
      }
    } == valor_filter) |>
    dplyr::select(
      edad,
      frecuencia_respiratoria,
      frecuencia_cardiaca,
      p_a_sistolica,
      p_a_diastolica,
      wbc,
      neutrofilos,
      linfocitos,
      nlr,
      platelets,
      saturacion_de_oxigeno,
      fio2,
      pafi,
      pao2,
      outcome,
      t2dm
    ) |>
    stats::na.omit()
  
  # Rename variables
  data <- data |> 
    rename("Age" = edad,
    "Respiratory rate" = frecuencia_respiratoria,
    "Heart rate" = frecuencia_cardiaca,
    "SBP" = p_a_sistolica,
    "DBP" = p_a_diastolica,
    "White-cells" = wbc,
    "Neutrophils" = neutrofilos,
    "Lymphocytes" = linfocitos,
    "NLR" = nlr,
    "Platelets" = platelets,
    "SaO2" = saturacion_de_oxigeno,
    "FiO2" = fio2,
    "PaO2:FiO2 ratio" = pafi,
    "PaO2" = pao2
  )
  return(data)
}
