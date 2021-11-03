#' Generate APACHE II score
#'
#' Generate APACHE II score
#' @param data data.frame object
#' @param age Age
#' @param bt Body temperature
#' @param map Mean arterial pressure
#' @param ph Blood pH
#' @param pr Pulse rate
#' @param rr Respiratory rate
#' @param na Serum sodium
#' @param kal Serum potassium
#' @param cr Serum Creatinine
#' @param hct Hematocrit
#' @param wbc White blood cells count
#' @param gcs Glasgow Coma Scale
#' @param po2 Blood pO2
#' @keywords apache2_gen
#' @export
apache2_gen = function(data, age, bt, map, ph, pr, rr, na, kal, cr, hct, wbc, gcs, po2){
  di_data = data %>%
    mutate(
      AGE = case_when(
        !!as.name(age) <= 44 ~ 0,
        !!as.name(age) >= 45 & !!as.name(age) < 54 ~ 2,
        !!as.name(age) >= 55 & !!as.name(age) < 64 ~ 3,
        !!as.name(age) >= 65 & !!as.name(age) < 74 ~ 5,
        TRUE ~ 6
      ),
      BT = case_when(
        !!as.name(bt) >= 36 & !!as.name(bt) < 38.5 ~ 0,
        !!as.name(bt) >= 38.5 & !!as.name(bt) < 39 ~ 1,
        !!as.name(bt) >= 34 & !!as.name(bt) < 36 ~ 1,
        !!as.name(bt) >= 32 & !!as.name(bt) < 34 ~ 2,
        !!as.name(bt) >= 39 & !!as.name(bt) < 41 ~ 3,
        !!as.name(bt) >= 30 & !!as.name(bt) < 32 ~ 3,
        TRUE ~ 4
      ),
      MAP = case_when(
        !!as.name(map) >= 69 & !!as.name(map) < 109 ~ 0,
        !!as.name(map) >= 109 & !!as.name(map) < 129 ~ 2,
        !!as.name(map) >= 49 & !!as.name(map) < 69 ~ 2,
        !!as.name(map) >= 129 & !!as.name(map) < 159 ~ 3,
        TRUE ~ 4
      ),
      PH = case_when(
        !!as.name(ph) >= 7.33 & !!as.name(ph) < 7.50 ~ 0,
        !!as.name(ph) >= 7.50 & !!as.name(ph) < 7.60 ~ 1,
        !!as.name(ph) >= 7.25 & !!as.name(ph) < 7.33 ~ 2,
        !!as.name(ph) >= 7.60 & !!as.name(ph) < 7.70 ~ 3,
        !!as.name(ph) >= 7.15 & !!as.name(ph) < 7.25 ~ 3,
        TRUE ~ 4
      ),
      PR = case_when(
        !!as.name(pr) >= 70 & !!as.name(pr) < 110 ~ 0,
        !!as.name(pr) >= 110 & !!as.name(pr) < 140 ~ 2,
        !!as.name(pr) >= 55 & !!as.name(pr) < 70 ~ 2,
        !!as.name(pr) >= 140 & !!as.name(pr) < 180 ~ 3,
        !!as.name(pr) >= 40 & !!as.name(pr) < 55 ~ 3,
        TRUE ~ 4
      ),
      RR = case_when(
        !!as.name(rr) >= 12 & !!as.name(rr) < 25 ~ 0,
        !!as.name(rr) >= 25 & !!as.name(rr) < 35 ~ 1,
        !!as.name(rr) >= 10 & !!as.name(rr) < 12 ~ 1,
        !!as.name(rr) >= 6 & !!as.name(rr) < 10 ~ 2,
        !!as.name(rr) >= 35 & !!as.name(rr) < 50 ~ 3,
        TRUE ~ 4
      ),
      Na = case_when(
        !!as.name(na) >= 130 & !!as.name(na) < 150 ~ 0,
        !!as.name(na) >= 150 & !!as.name(na) < 155 ~ 1,
        !!as.name(na) >= 155 & !!as.name(na) < 160 ~ 2,
        !!as.name(na) >= 120 & !!as.name(na) < 130 ~ 2,
        !!as.name(na) >= 160 & !!as.name(na) < 180 ~ 3,
        !!as.name(na) >= 111 & !!as.name(na) < 120 ~ 3,
        TRUE ~ 4
      ),
      KAL = case_when(
        !!as.name(kal) >= 3.5 & !!as.name(kal) < 5.5 ~ 0,
        !!as.name(kal) >= 5.5 & !!as.name(kal) < 6.0 ~ 1,
        !!as.name(kal) >= 3.0 & !!as.name(kal) < 3.5 ~ 1,
        !!as.name(kal) >= 2.5 & !!as.name(kal) < 3.0 ~ 2,
        !!as.name(kal) >= 6.0 & !!as.name(kal) < 7.0 ~ 3,
        TRUE ~ 4
      ),
      CR = case_when(
        !!as.name(cr) >= 0.6 & !!as.name(cr) < 1.5 ~ 0,
        !!as.name(cr) >= 1.5 & !!as.name(cr) < 2.0 ~ 2,
        !!as.name(cr) < 0.6 ~ 2,
        !!as.name(cr) >= 2.0 & !!as.name(cr) < 3.5 ~ 3,
        TRUE ~ 4
      ),
      HCT = case_when(
        !!as.name(hct) >= 30 & !!as.name(hct) < 46 ~ 0,
        !!as.name(hct) >= 46 & !!as.name(hct) < 50 ~ 1,
        !!as.name(hct) >= 50 & !!as.name(hct) < 60 ~ 2,
        !!as.name(hct) >= 20 & !!as.name(hct) < 30 ~ 2,
        TRUE ~ 4
      ),
      WBC = case_when(
        !!as.name(wbc) >= 3 & !!as.name(wbc) < 15 ~ 0,
        !!as.name(wbc) >= 15 & !!as.name(wbc) < 20 ~ 1,
        !!as.name(wbc) >= 20 & !!as.name(wbc) < 40 ~ 2,
        !!as.name(wbc) >= 1 & !!as.name(wbc) < 3 ~ 2,
        TRUE ~ 4
      ),
      GCS = 15 - !!as.name(gcs),
      PO2 = case_when(
        !!as.name(po2) > 70 ~ 0,
        !!as.name(po2) >= 61 & !!as.name(po2) <= 70 ~ 1,
        !!as.name(po2) >= 55 & !!as.name(po2) <= 60 ~ 3,
        TRUE ~ 4
      ),
      apache2_score = AGE + BT + MAP + PH + PR + RR + Na + KAL + CR + HCT + WBC + GCS + PO2

    ) %>%
    select(
      -AGE, -BT, -MAP, -PH, -PR, -RR, -Na, -KAL, -CR, -HCT, -WBC, -GCS, -PO2
    )

  return (di_data)
}
