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
      frecuencia_respiratoria,
      frecuencia_respiratoria.c,
      frecuencia_cardiaca,
      frecuencia_cardiaca.c,
      p_a_sistolica,
      p_a_sistolica.c,
      p_a_diastolica,
      p_a_diastolica.c,
      
      # Laboratory findings
      wbc,
      wbc.c,
      neutrofilos,
      neutrofilos.c,
      linfocitos,
      linfocitos.c,
      nlr,
      nlr.c,
      platelets,
      platelets.c,
      
      # Blood gas findings
      saturacion_de_oxigeno,
      saturacion_de_oxigeno.c,
      fio2,
      fio2.c,
      pafi,
      pafi.c,
      pafi_cal,
      pafi_cal.c,
      pao2,
      pao2.c,
      
      # Treatment
      corticoides,
      anticoagulantes,
      antipaludicos,
      pronacion,
      
      # outcomes
      outcome
    )
}
