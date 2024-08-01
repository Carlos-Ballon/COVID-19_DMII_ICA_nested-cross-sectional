var_selec_analysis <- function(data) {
  data |>
    dplyr::select(
      # Demographic characteristics and clinical history
      code,
      edad,
      edad.c,
      sex,
      obesity,
      t2dm,
      hta,
      len_hosp_stay,
      
      # Signs and symptoms
      fever,
      tos,
      dolor_de_garganta,
      malestar_general,
      headache,
      taquipnea,
      disnea,
      anosmia,
      disgeusia,
      estertores_pulmonares,
      diarrea,
      emesis,
      astenia,
      dolor_abdominal,
      
      # Vital signs
      frec_respiratoria,
      frec_respiratoria.c,
      frec_cardiaca,
      frec_cardiaca.c,
      SBP,
      SBP.c,
      DBP,
      DBP.c,
      
      # Laboratory findings
      WBC,
      WBC.c,
      neutrofilos,
      neutrofilos.c,
      linfocitos,
      linfocitos.c,
      NLR,
      NLR.c,
      platelets,
      platelets.c,
      
      # Blood gas findings
      SaO2,
      SaO2.c,
      FiO2,
      FiO2.c,
      PaO2_FiO2,
      PaO2_FiO2.c,
      pafi_cal,
      pafi_cal.c,
      PaO2,
      PaO2.c,
      
      # Treatment
      corticoides,
      anticoagulantes,
      antipaludicos,
      pronacion,
      
      # outcomes
      outcome
    )
}
