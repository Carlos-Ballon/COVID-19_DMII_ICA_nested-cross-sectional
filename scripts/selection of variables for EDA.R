var_selection <- function(data) {
  data |>
    dplyr::select(
      # Demographic characteristics and clinical history
      matched_id,
      weights,
      code,
      edad,
      edad.c,
      sex,
      obesity,
      t2dm,
      hta,
      ecv,
      cancer,
      hiv,
      immunsupressive_dis,
      erc,
      hemodialisis,
      asma_bronquial,
      epc,
      len_hosp_stay,
      shock_septico,
      sepsis,
      disf_multiorg,
      ac_renal_failure,
      f_hepatica_aguda,
      f_multiorganica,
      
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
      perdida_de_peso,
      poliuria,
      polidipysia,
      polifagia,
      sensorio,
      
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
      antibiotics,
      corticoides,
      anticoagulantes,
      antipaludicos,
      pronacion,
      
      # outcomes
      outcome
    )
}