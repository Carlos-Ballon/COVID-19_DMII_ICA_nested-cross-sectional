var_selec_eda <- function(data) {
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
      fio2,
      fio2.c,
      
      PaO2_FiO2,
      PaO2_FiO2.c,
      pafi_cal,
      pafi_cal.c,
      PaO2,
      PaO2.c,
      
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