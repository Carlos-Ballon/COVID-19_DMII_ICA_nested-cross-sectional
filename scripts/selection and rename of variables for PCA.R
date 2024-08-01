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
      frec_respiratoria,
      frec_cardiaca,
      SBP,
      DBP,
      WBC,
      neutrofilos,
      linfocitos,
      NLR,
      platelets,
      SaO2,
      FiO2,
      PaO2_FiO2,
      PaO2,
      outcome,
      t2dm
    ) |>
    stats::na.omit()
  
  # Rename variables
  data <- data |> 
    rename("Age" = edad,
    "Respiratory rate" = frec_respiratoria,
    "Heart rate" = frec_cardiaca,
    "SBP" = SBP,
    "DBP" = DBP,
    "WBC" = WBC,
    "Neutrophils" = neutrofilos,
    "Lymphocytes" = linfocitos,
    "NLR" = NLR,
    "Platelets" = platelets,
    "SaO2" = SaO2,
    "FiO2" = FiO2,
    "PaO2:FiO2 ratio" = PaO2_FiO2,
    "PaO2" = PaO2
  )
  return(data)
}
