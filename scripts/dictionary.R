dictionary <- function(data) {
  data |>
    mutate(
      len_hosp_stay = ff_label(len_hosp_stay, "Follow-up time (days)"),
      
      edad = ff_label(edad, "Age (years)"),
      ###
      
      edad.c = case_when(edad <= 60 ~ "< 61",
                         edad > 60 ~ ">= 61") |>
        fct_relevel("< 61", ">= 61") |>
        ff_label("Age (years)"),
      
      sex = factor(sex) |> ###
        fct_recode("Female" = "female",
                   "Male" = "male") |>
        fct_relevel("Female", "Male") |>
        ff_label("Sex"),
      
      t2dm = factor(t2dm) |> ###
        ff_label("T2DM"),
      
      smoker = factor(smoker) |> ####
        fct_recode(
          "No" = "N0",
          "No" = "no",
          "Yes" = "yes"
        ) |>
        fct_relevel("No", "Yes") |>
        ff_label("Smoking"),
      
      alcoholism = factor(alcoholism) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Alcoholism"),
      
      obesity = factor(obesity) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Obesity"),
      
      asma_bronquial = factor(asma_bronquial) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Asthma"),
      
      hta = factor(hta) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Hypertension"),
      
      dislip = factor(dislip) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Dyslipidemia"),
      
      ecv = factor(ecv) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Cerebrovascular disease"),
      
      cancer = factor(cancer) |> ####
        fct_recode(
          "No" = "N",
          "No" = "no",
          "Yes" = "yes"
        ) |>
        fct_relevel("No", "Yes") |>
        ff_label("Cancer"),
      
      hiv = factor(hiv) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("HIV"),
      
      immunsupressive_dis = case_when(immunsupressive_dis == "no" ~ "No",
                                      TRUE ~ "Yes") |> ####
        fct_relevel("No", "Yes") |>
        ff_label("Immunesupressive disease"),
      
      erc = factor(erc) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("CDK"),
      
      hemodialisis = factor(hemodialisis) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Hemodialysis"),
      
      epc = factor(epc) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("EPOC"),
      
      disf_multiorg = factor(disf_multiorg) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Multiple organ failure"),
      
      ac_renal_failure = factor(ac_renal_failure) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Acute kidney injury"),
      
      f_hepatica_aguda = factor(f_hepatica_aguda) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("ACLF"),
      
      f_multiorganica = factor(f_multiorganica) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes"),
      
      fever = factor(fever) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Fever"),
      
      tos = factor(tos) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Dry cought"),
      
      dolor_de_garganta = factor(dolor_de_garganta) |> #
        fct_recode(
          "No" = "no",
          "Yes" = "yes",
          "Yes" = "S"
        ) |>
        fct_relevel("No", "Yes") |>
        ff_label("Sore throat"),
      
      malestar_general = factor(malestar_general) |> #
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("General malaise"),
      
      headache = factor(headache) |> #
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Headache"),
      
      astenia = factor(astenia) |> #
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Asthenia"),
      
      anosmia = factor(anosmia) |> #
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Anosmia"),
      
      disgeusia = factor(disgeusia) |> #
        fct_recode(
          "No" = "N",
          "No" = "no",
          "Yes" = "yes",
        ) |>
        fct_relevel("No", "Yes") |>
        ff_label("Dysgeusia"),
      
      disnea = factor(disnea) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Dyspnea"),
      
      perdida_de_peso = factor(perdida_de_peso) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Weight loss"),
      
      estertores_pulmonares = factor(estertores_pulmonares) |> ##
        fct_recode(
          "No" = "no",
          "Yes" = "yes",
          "Yes" = "yes0"
        ) |>
        fct_relevel("No", "Yes") |>
        ff_label("Lung crackles"),
      
      diarrea = factor(diarrrea) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Diarrhea"),
      
      emesis = factor(emesis) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Vomiting"),
      
      dolor_abdominal = factor(dolor_abdominal) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Abdominal pain"),
      
      poliuria = factor(poliuria) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("Yes", "No") |>
        ff_label("Polyuria"),
      
      polidipysia = factor(polidipysia) |> ####
        fct_recode(
          "No" = "no",
          "Yes" = "yes",
          "No" = "ON"
        ) |>
        fct_relevel("Yes", "No") |>
        ff_label("Polidipsia"),
      
      polifagia = factor(polifagia) |> ####
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("Yes", "No") |>
        ff_label("Poliphagia"),
      
      disgeusia = factor(disgeusia) |> ####
        fct_recode(
          "No" = "no",
          "Yes" = "yes",
          "No" = "N"
        ) |>
        fct_relevel("Yes", "No") |>
        ff_label("Dysgeusia"),
      
      taquipnea = factor(taquipnea) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Tachypnea"),
      
      sensorio = factor(sensorio) |> #
        fct_recode(
          "Awake" = "despierto",
          "Sleepy" = "somnoliento",
          "Drowsy" = "soporoso"
        ) |>
        fct_relevel("Awake",
                    "Sleepy",
                    "Drowsy") |>
        ff_label("Sensory"),
      
      corticoides = case_when(corticoides == "no" ~ "No",
                              TRUE ~ "Yes") |> ###
        fct_relevel("No", "Yes") |>
        ff_label("Corticosteroids"),
      
      anticoagulantes = case_when(anticoagulantes == "no" ~ "No",
                                  TRUE ~ "Yes") |> ###
        fct_relevel("No", "Yes") |>
        ff_label("Anticoagulants"),
      
      antipaludicos = case_when(antipaludicos == "no" ~ "No",
                                TRUE ~ "Yes") |> ###
        fct_relevel("No", "Yes") |>
        ff_label("Antimalarials"),
      
      antibiotics = case_when(antibiotics == "no" ~ "No",
                              TRUE ~ "Yes") |> ###
        fct_relevel("No", "Yes") |>
        ff_label("Antibiotics"),
      
      pronacion = factor(pronacion) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("Yes", "No") |>
        ff_label("Pronation"),
      
      sepsis = factor(sepsis) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Sepsis"),
      
      shock_septico = factor(shock_septico) |> ###
        fct_recode("No" = "no",
                   "Yes" = "yes") |>
        fct_relevel("No", "Yes") |>
        ff_label("Septic shock"),
      
      SBP = ff_label(SBP, "SBP (mmHg)"),
      ###
      
      SBP.c = case_when(SBP < 140 ~ "< 140",
                                  TRUE ~ ">= 140") |>
        fct_relevel("< 140", ">= 140") |>
        ff_label("SBP (mmHg)"),
      
      DBP = ff_label(DBP, "DBP (mmHg)"),
      ###
      
      DBP.c = case_when(DBP < 90 ~ "< 90",
                                   TRUE ~ ">= 90") |>
        fct_relevel("< 90", ">= 90") |>
        ff_label("DBP (mmHg)"),
      
      WBC = ff_label(WBC, "WBC (×10^9^/L)"),
      ###
      
      WBC.c = case_when(WBC < 4 ~ "< 4",
                        WBC >= 4 & WBC <= 10 ~ "4-10",
                        WBC > 10 ~ "> 10") |>
        fct_relevel("4-10", "< 4", "> 10") |>
        ff_label("WBC (×10^9^/L)"),
      
      linfocitos = ff_label(linfocitos, "Lymphocytes (×10^9^/L)"),
      ###
      
      linfocitos.c = case_when(linfocitos < 1 ~ "< 1",
                               TRUE ~ ">= 1") |>
        fct_relevel(">= 1", "< 1") |>
        ff_label("Lymphocytes (×10^9^/L)"),
      
      neutrofilos = ff_label(neutrofilos, "Neutrophils (×10^9^/L)"),
      ###
      
      neutrofilos.c = case_when(neutrofilos > 6.3 ~ "> 6.3",
                                TRUE ~ "<= 6.3") |>
        fct_relevel("<= 6.3", "> 6.3") |>
        ff_label("Neutrophils (×10^9^/L)"),
      
      NLR = neutrofilos / linfocitos,
      NLR = ff_label(NLR, "NLR"),
      ###
      
      NLR.c = case_when(NLR < 5.86 ~ "< 5.86",
                        NLR >= 5.86 ~ ">= 5.86") |>
        fct_relevel("< 5.86", ">= 5.86") |>
        ff_label("NLR"),
      
      platelets = ff_label(platelets, "Platelets (×10^9^/L)"),
      ###
      
      platelets.c = case_when(platelets < 125 ~ "< 125",
                              TRUE ~ ">= 125") |>
        fct_relevel(">= 125", "< 125") |>
        ff_label("Platelets (×10^9^/L)"),
      
      frec_cardiaca = ff_label(frec_cardiaca, "Heart rate (BPM)"),
      ##
      
      frec_cardiaca.c = case_when(frec_cardiaca < 100 ~ "< 100",
                                        TRUE ~ ">= 100") |>
        fct_relevel("< 100", ">= 100") |>
        ff_label("Heart rate (BPM)"),
      
      frec_respiratoria = ff_label(frec_respiratoria,
                                         "Respiratory rate (BPM)"),
      ##
      
      frec_respiratoria.c =
        case_when(
          frec_respiratoria < 24 ~ "< 24",
          frec_respiratoria >= 24 & frec_respiratoria <= 30 ~ "24 - 30",
          frec_respiratoria > 30 ~ "> 30"
        ) |> ##
        fct_relevel("24 - 30", "< 24", "> 30") |>
        ff_label("Respiratory rate (BPM)"),
      
      SaO2 = ff_label(SaO2, "SaO~2~ (%)"),
      ###
      
      SaO2.c = case_when(SaO2 < 90 ~ "< 90",
                                          TRUE ~ ">= 90") |>
        fct_relevel(">= 90", "< 90") |>
        ff_label("SaO~2~"),
      
      FiO2 = ff_label(FiO2, "FiO~2~ (%)"),
      ###
      
      FiO2.c = case_when(FiO2 > 21 ~ "> 21 (O~2~ therapy)",
                         TRUE ~ "21") |>
        fct_relevel("> 21 (O~2~ therapy)", "21") |>
        ff_label("FiO~2~ (%)"),
      
      PaO2 = round(as.numeric(PaO2), 2),
      PaO2 = ff_label(PaO2, "PaO2 (mmHg)"),
      ##
      
      PaO2.c = case_when(PaO2 < 60 ~ "< 60",
                         TRUE ~ ">= 60") |>
        fct_relevel(">= 60", "< 60") |>
        ff_label("PaO~2~"),
      
      pafi_cal = round(PaO2 / FiO2, 2),
      pafi_cal = ff_label(pafi_cal, "Calculated PaO~2~:FiO~2~ ratio"),
      ###
      
      pafi_cal.c = case_when(pafi_cal <= 200 ~ "<= 200",
                             TRUE ~ "> 200") |>
        fct_relevel("> 200", "<= 200") |>
        ff_label("Calculated PaO~2~:FiO~2~ ratio"),
      
      PaO2_FiO2 = ff_label(PaO2_FiO2, "PaO~2~:FiO~2~ ratio"),
      ###
      
      PaO2_FiO2.c = case_when(PaO2_FiO2 <= 200 ~ "<= 200",
                         TRUE ~ "> 200") |>
        fct_relevel("> 200", "<= 200") |>
        ff_label("PaO~2~:FiO~2~ ratio"),
      
      outcome = factor(outcome) |>
        fct_recode("Non-survivor" = "non.surv",
                   "Survivor" = "survivor") |>
        fct_relevel("Survivor", "Non-survivor"),
      
      cc = case_when(t2dm == "yes" ~ "case",
                     t2dm == "no" ~ "control")
    )
}
