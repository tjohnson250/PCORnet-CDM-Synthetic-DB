# Profile Generator Functions for Clinical Profiles
# Assigns profiles to patients and generates clinically coherent data

#' Assign clinical profiles to patients based on age and prevalence rates
#'
#' @param patient_df Data frame with patient demographics (must include birth_date column)
#' @param current_date Reference date for age calculation
#' @param profiles List of clinical profile definitions (default: CLINICAL_PROFILES)
#' @return Data frame with clinical_profile column added
assign_clinical_profiles <- function(patient_df, current_date, profiles = CLINICAL_PROFILES) {
  n_patients <- nrow(patient_df)

  # Calculate age for each patient
  ages <- as.numeric(difftime(current_date, patient_df$birth_date, units = "days")) / 365.25

  # Calculate adjusted prevalence for each profile based on age
  profile_names <- names(profiles)
  profile_probs <- matrix(0, nrow = n_patients, ncol = length(profile_names))
  colnames(profile_probs) <- profile_names

  for (i in seq_along(profile_names)) {
    profile <- profiles[[profile_names[i]]]
    base_prev <- profile$prevalence_base
    age_mods <- sapply(ages, profile$age_modifier)
    profile_probs[, i] <- base_prev * age_mods
  }

  # Normalize probabilities to sum to 1 for each patient
  row_sums <- rowSums(profile_probs)
  profile_probs <- profile_probs / row_sums

  # Assign profiles
  assigned_profiles <- apply(profile_probs, 1, function(probs) {
    sample(profile_names, 1, prob = probs)
  })

  patient_df$clinical_profile <- assigned_profiles
  patient_df$age <- ages

  return(patient_df)
}

#' Generate number of encounters based on profile
#'
#' @param profile_name Name of the clinical profile
#' @param profiles List of clinical profile definitions
#' @return Integer number of encounters
generate_encounter_count <- function(profile_name, profiles = CLINICAL_PROFILES) {
  enc_range <- profiles[[profile_name]]$encounter_range
  sample(enc_range[1]:enc_range[2], 1)
}

#' Generate diagnoses for an encounter based on clinical profile
#'
#' @param profile Profile definition list
#' @param is_first_encounter Whether this is the patient's first encounter
#' @return Data frame of diagnosis rows
generate_profile_diagnoses <- function(profile, is_first_encounter = FALSE) {
  diagnoses <- list()


  # Always assign primary diagnosis
  primary_list <- profile$diagnoses$primary
  if (length(primary_list) > 1) {
    # Multiple possible primaries - pick one based on probability
    probs <- sapply(primary_list, function(x) x$probability)
    probs <- probs / sum(probs)
    primary <- primary_list[[sample(length(primary_list), 1, prob = probs)]]
  } else {
    primary <- primary_list[[1]]
  }

  diagnoses[[1]] <- data.frame(
    code = primary$code,
    description = primary$description,
    is_primary = TRUE,
    stringsAsFactors = FALSE
  )

  # Assign secondary diagnoses based on probability
  for (secondary in profile$diagnoses$secondary) {
    if (runif(1) < secondary$probability) {
      diagnoses[[length(diagnoses) + 1]] <- data.frame(
        code = secondary$code,
        description = secondary$description,
        is_primary = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(diagnoses) > 0) {
    return(do.call(rbind, diagnoses))
  } else {
    return(NULL)
  }
}

#' Generate lab results for an encounter based on clinical profile
#'
#' @param profile Profile definition list
#' @param encounter_type Type of encounter (IP, ED, AV, etc.)
#' @return Data frame of lab result rows
generate_profile_labs <- function(profile, encounter_type) {
  labs <- list()

  # Higher probability of labs for inpatient/ED encounters
  lab_prob_modifier <- switch(encounter_type,
    "IP" = 1.0,
    "ED" = 0.9,
    "IS" = 0.8,
    "AV" = 0.4,
    "OA" = 0.3,
    0.4
  )

  for (lab in profile$labs) {
    # Decide if this lab is ordered based on frequency and encounter type
    if (runif(1) < lab_prob_modifier * min(1, lab$frequency_per_year / 2)) {
      # Determine if result is abnormal
      is_abnormal <- runif(1) < lab$abnormal_probability

      if (is_abnormal) {
        value <- runif(1, lab$abnormal_range[1], lab$abnormal_range[2])
        # Determine if high or low
        if (value < lab$normal_range[1]) {
          abn_ind <- "AL"  # Abnormal low
        } else {
          abn_ind <- "AH"  # Abnormal high
        }
      } else {
        value <- runif(1, lab$normal_range[1], lab$normal_range[2])
        abn_ind <- "NI"  # Normal
      }

      labs[[length(labs) + 1]] <- data.frame(
        loinc = lab$loinc,
        name = lab$name,
        value = round(value, 2),
        unit = lab$unit,
        abn_ind = abn_ind,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(labs) > 0) {
    return(do.call(rbind, labs))
  } else {
    return(NULL)
  }
}

#' Generate medications for a patient based on clinical profile
#'
#' @param profile Profile definition list
#' @return Data frame of medication rows
generate_profile_medications <- function(profile) {
  medications <- list()

  for (med in profile$medications) {
    if (runif(1) < med$probability) {
      freq_info <- FREQUENCY_CODES[[med$frequency]]
      if (is.null(freq_info)) {
        freq_info <- list(code = "NI", raw = med$frequency)
      }

      medications[[length(medications) + 1]] <- data.frame(
        rxnorm = med$rxnorm,
        name = med$name,
        dose = med$dose,
        unit = med$unit,
        frequency_code = freq_info$code,
        frequency_raw = freq_info$raw,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(medications) > 0) {
    return(do.call(rbind, medications))
  } else {
    return(NULL)
  }
}

#' Generate procedures for an encounter based on clinical profile
#'
#' @param profile Profile definition list
#' @param encounter_type Type of encounter (IP, ED, AV, etc.)
#' @return Data frame of procedure rows
generate_profile_procedures <- function(profile, encounter_type) {
  procedures <- list()

  # Modifier based on encounter type
  proc_prob_modifier <- switch(encounter_type,
    "IP" = 1.2,
    "ED" = 1.0,
    "IS" = 0.9,
    "AV" = 0.8,
    "OA" = 0.6,
    0.7
  )

  for (proc in profile$procedures) {
    adjusted_prob <- min(1, proc$probability * proc_prob_modifier)
    if (runif(1) < adjusted_prob) {
      procedures[[length(procedures) + 1]] <- data.frame(
        cpt = proc$cpt,
        name = proc$name,
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(procedures) > 0) {
    return(do.call(rbind, procedures))
  } else {
    return(NULL)
  }
}

#' Generate vital signs for an encounter based on clinical profile
#'
#' @param profile Profile definition list
#' @return Data frame with single row of vital signs
generate_profile_vitals <- function(profile) {
  vitals_def <- profile$vitals

  # Generate height (consistent per patient, but we generate per encounter for simplicity)
  height_cm <- rnorm(1, mean = 170, sd = 10)
  height_cm <- max(140, min(200, height_cm))  # Clamp to reasonable range

  # Generate BMI from profile, then calculate weight
  bmi <- rnorm(1, mean = vitals_def$bmi_mean, sd = vitals_def$bmi_sd)
  bmi <- max(16, min(50, bmi))  # Clamp to reasonable range

  # Weight (kg) = BMI * height(m)^2
  weight_kg <- bmi * (height_cm / 100)^2

  # Blood pressure
  systolic <- round(rnorm(1, mean = vitals_def$systolic_mean, sd = vitals_def$systolic_sd))
  systolic <- max(80, min(200, systolic))

  diastolic <- round(rnorm(1, mean = vitals_def$diastolic_mean, sd = vitals_def$diastolic_sd))
  diastolic <- max(50, min(120, diastolic))

  # Ensure diastolic < systolic
  if (diastolic >= systolic) {
    diastolic <- systolic - 20
  }

  return(data.frame(
    ht = round(height_cm, 1),
    wt = round(weight_kg, 1),
    bmi = round(bmi, 1),
    systolic = systolic,
    diastolic = diastolic,
    stringsAsFactors = FALSE
  ))
}

#' Select encounter type based on clinical profile
#'
#' @param profile_name Name of the clinical profile
#' @return Character encounter type code (IP, ED, AV, OA, IS)
select_encounter_type <- function(profile_name) {
  # Different profiles have different encounter type distributions
  enc_probs <- switch(profile_name,
    "healthy" = c(IP = 0.02, ED = 0.03, AV = 0.70, OA = 0.20, IS = 0.05),
    "diabetic" = c(IP = 0.08, ED = 0.07, AV = 0.60, OA = 0.20, IS = 0.05),
    "cardiac" = c(IP = 0.15, ED = 0.12, AV = 0.50, OA = 0.15, IS = 0.08),
    "respiratory" = c(IP = 0.10, ED = 0.15, AV = 0.50, OA = 0.15, IS = 0.10),
    "mental_health" = c(IP = 0.05, ED = 0.08, AV = 0.60, OA = 0.22, IS = 0.05),
    "multimorbid" = c(IP = 0.18, ED = 0.12, AV = 0.45, OA = 0.15, IS = 0.10),
    c(IP = 0.10, ED = 0.08, AV = 0.55, OA = 0.17, IS = 0.10)  # default
  )

  sample(names(enc_probs), 1, prob = enc_probs)
}

#' Calculate length of stay based on encounter type
#'
#' @param enc_type Encounter type code
#' @return Integer length of stay in days
calculate_length_of_stay <- function(enc_type) {
  switch(enc_type,
    "IP" = sample(1:14, 1, prob = c(rep(0.15, 3), rep(0.1, 4), rep(0.05, 7))),
    "ED" = sample(0:1, 1, prob = c(0.7, 0.3)),
    "IS" = sample(1:7, 1, prob = c(0.3, 0.25, 0.2, 0.1, 0.08, 0.05, 0.02)),
    0  # AV and OA are same-day
  )
}
