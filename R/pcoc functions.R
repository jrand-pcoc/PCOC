# require(tidyverse)
#library(devtools)
# Create the pcoc ggplot package
theme_pcoc_png <- function(){
  theme(
    # text font, colour and size
    plot.title = element_text(family = "montserrat", colour = '#001641', size = 35),
    plot.subtitle = element_text(family = "montserrat", colour = '#001641', size = 25),
    axis.text = element_text(family = 'montserrat', colour = '#001641', size = 20),
    axis.title.x = element_text(family = 'montserrat', colour = '#001641', size = 25),
    axis.title.y = element_text(family = 'montserrat', colour = '#001641', size = 25),
    legend.text = element_text(family = 'montserrat', colour = '#001641', size = 25),
    legend.title = element_text(family = 'montserrat', colour = '#001641', size = 25),
    strip.text = element_text(family = 'montserrat', colour = '#001641', size = 25),
    plot.caption = element_text(family = 'montserrat', colour = '#001641', size = 20),
    # Legend settings
    legend.position = 'bottom',
    # Panel settings
    panel.grid.major = element_line(colour = '#cad2d2', size = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background =  element_rect(colour = 'white', fill = 'white'),
    panel.border = element_rect(color = "grey50", fill = NA,  linewidth = 0.5),

  )
}

theme_pcoc_pdf <- function(){
  theme(
    # text font, colour and size
    plot.title = element_text(family = "montserrat", colour = '#001641', size = 12),
    plot.subtitle = element_text(family = "montserrat", colour = '#001641', size = 10),
    axis.text = element_text(family = 'montserrat', colour = '#001641', size = 7),
    axis.title.x = element_text(family = 'montserrat', colour = '#001641', size = 7),
    axis.title.y = element_text(family = 'montserrat', colour = '#001641', size = 7),
    legend.text = element_text(family = 'montserrat', colour = '#001641', size = 7),
    legend.title = element_text(family = 'montserrat', colour = '#001641', size = 7),
    strip.text = element_text(family = 'montserrat', colour = '#001641', size = 7),
    plot.caption = element_text(family = 'montserrat', colour = '#001641', size = 7),
    # Legend settings
    legend.position = 'bottom',
    # Panel settings
    panel.grid.major = element_line(colour = '#cad2d2', size = 0.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background =  element_rect(colour = 'white', fill = 'white'),
    panel.border = element_rect(color = "grey50", fill = NA,  linewidth = 0.5),

  )
}


theme_pcoc_watermelon <- function(){
  theme(strip.text = element_text(family = 'montserrat', colour = '#001641', size = 15),
        axis.title.x = element_text(family = 'montserrat', colour = '#001641', size = 15),
        axis.title.y = element_text(family = 'montserrat', colour = '#001641', size = 15),
        plot.caption = element_text(family = 'montserrat', colour = '#001641', size = 15),
        axis.text = element_text(family = 'montserrat', colour = '#001641', size = 10),
        panel.grid.major = element_line(colour = '#cad2d2', size = 0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background =  element_rect(colour = 'white', fill = 'white'),
        panel.border = element_rect(color = "grey50", fill = NA,  linewidth = 0.5))
}

palette_pcoc <- c('#AEBD37','#0D96D4','#4C318F','#ED8D2B','#001641')

saveplot <- function(plot = p1, filename = 'Plot1.png', width = 7, height = 7){
  ggsave(plot = plot, filename = filename, width = width, height = height)
}

report_label <- function(report = 37){
  period <- case_when(report %% 2 == 1 ~ paste0('January - June ', 2000 + (report + 11) %/% 2),
                      report %% 2 == 0 ~ paste0('July - December ', 2000 + (report + 11) %/% 2),
                      TRUE ~ 'Other')
  return(period)
}

# preferred_language <- function(preferred_language = 3904){
#
#   # group indigenous langauges
#   df_pref <- read.csv('Preferred Language.csv')
#
#   df_pref <- df_pref %>%
#     mutate(PreferredLanguageDesc = case_when(substr(preferred_language,1,2) %in% c('81', '82', '83', '84', '85', '86', '87', '88', '89') ~ 'Australian Indigenous Languages',
#                                              TRUE ~ PreferredLanguageDesc)) %>%
#     bind_rows(tibble(preferred_language = c(-1,-2,-3,-4,-5),
#                      PreferredLanguageDesc = c('Not Stated','Not Stated','Not Stated','Not Stated','Not Stated')))
#
#   prefdesc <- df_pref %>%
#     filter(preferred_language == preferred_language) %>%
#     pull(PreferredLanguageDesc)
#
#   return(prefdesc)
# }

pcoc_dd <- function(variable = 'sex'){
  sex <- tibble(sex = c(1,2,3,9, 1,2,3,9),
                sex_desc = c(
                  'Male',
                  'Female',
                  'Indeterminate',
                  'Not Stated/Inadequately Described',
                  "Male",
                  "Female",
                  "Other",
                  "Not Recorded/Invalid"),
                data_version = c(rep(3,4),rep(4,4)))

  state <- tibble(state = c(1:10,99),
                  state_abb = c('NSW',
                               'VIC',
                               'QLD',
                               'SA',
                               'WA',
                               'TAS',
                               'NT',
                               'ACT',
                               'Other Australian Territories',
                               'Not Australia',
                               'Unknown/Not Recorded'),
                  state_desc = c(
                    'New South Wales',
                    'Victoria',
                    'Queensland',
                    'South Australia',
                    'Western Australia',
                    'Tasmania',
                    'Northern Territory',
                    'Australian Capital Territory',
                    'Other Australian Territories',
                    'Not Australia',
                    'Unknown/Not Recorded'))

  indigenous_status <- tibble(indigenous_status = c(1:4,9),
                              indigenous_status_desc = c(
                                'Aboriginal but not Torres Strait Islander Origin',
                                'Torres Strait Islander but not Aboriginal Origin',
                                'Both Aboriginal and Torres Strait Islander Origin',
                                'Neither Aboriginal nor Torres Strait Islander origin',
                                'Not Stated /Inadequately Described'))

  episode_setting_group <- tibble(episode_setting_group = c(1:3),
                            episode_setting_group_desc = c(
                              'Inpatient',
                              'Ambulatory',
                              'Community'))

  country_of_birth <- tibble(country_of_birth = c(1101, 2102, 7103, 6101, 1201, 5204, 5105, 9225,
                                                5203, 3104, 7107, 2105, 7105, 8104, 2304, 99),
                             country_of_birth_desc = c('Australia', 'England', 'India', 'China', 'New Zealand', 'Philippines',
                                                    'Vietnam', 'South Africa', 'Malaysia', 'Italy', 'Sri Lanka',
                                                    'Scotland', 'Nepal', 'United States of America', 'Germany',
                                                    'All other countries')
                             )

  preferred_language <- tibble(preferred_language = c(12, 71, 52, 42, 63, 65, 51, 23, 22, 24, 35, 92, 73,
                                                93, 80),
                               preferred_language_desc = c(
                               'English', 'Chinese', 'Hindi', 'Arabic', 'Vietnamese', 'Filipino/Indonesian',
                               'Tamil/Malayam', 'Spanish', 'Greek', 'Italian', 'Macedonian/Croatian',
                               'African languages', 'Korean', 'Samoan/Tongan', 'Australian Indigenous languages'))

  broad_diagnosis <- tibble(broad_diagnosis = c(1:2),
                            broad_diagnosis_desc = c(
                              'Malignant',
                              'Non-Malignant'))

  diagnosis <- tibble(diagnosis = c(100:114, 180, 200:213, 280, 999,
                                    101:115,180,201:212,280,999),
                      diagnosis_desc = c(
                        'Malignant - not further defined',
                        'Bone and soft tissue',
                        'Breast',
                        'CNS',
                        'Colorectal',
                        'Other GIT',
                        'Haematological',
                        'Head and neck',
                        'Lung',
                        'Pancreas',
                        'Prostate',
                        'Other urological',
                        'Gynaecological',
                        'Skin',
                        'Unknown Primary',
                        'Other primary malignancy',
                        'Non Malignant - not further defined',
                        'Cardiovascular disease',
                        'HIV/AIDS',
                        'End stage kidney disease',
                        'Stroke','Motor Neurone Disease',
                        'Alzheimer’s dementia',
                        'Other dementia',
                        'Other neurological disease',
                        'Respiratory failure',
                        'End stage liver disease',
                        'Diabetes and its complications',
                        'Sepsis',
                        'Multiple organ failure',
                        'Other non-malignancy',
                        'Unknown',

                        'Bone and Soft Tissue',
                        "Breast",
                        "Central Nervous System",
                        "Colorectal",
                        "Pancreas",
                        "Liver",
                        "Other Gastrointestinal Tract",
                        "Haematological",
                        "Head and Neck",
                        "Lung",
                        "Prostate",
                        "Other Urological",
                        "Gynaecological",
                        "Skin",
                        "Unknown Primary",
                        "Other Malignancy",
                        "Cardiovascular Disease",
                        "HIV/AIDS",
                        "End Stage Kidney Disease",
                        "Cerebrovascular Disease",
                        "Motor Neuron Disease",
                        "Dementia",
                        "Other Neurological Disease",
                        "Respiratory Failure",
                        "End Stage Liver Disease",
                        "Diabetes and its complications",
                        "Sepsis",
                        "Multiple organ failure",
                        "Other Non-Malignancy",
                        "Unknown"),
                      data_version = c(rep(3,32),rep(4,30)))

  referral_source <- tibble(referral_source = c(10:15, 20:25, 30, 40, 50, 60, 61, 70, 80, 90, 99),
                            referral_source_desc = c(
                              'Public hospital - not further defined',
                              'Public hospital - palliative care unit/team',
                              'Public hospital - oncology unit/team',
                              'Public hospital - medical unit/team',
                              'Public hospital - surgical unit/team',
                              'Public hospital - emergency department',
                              'Private hospital - not further defined',
                              'Private hospital - palliative care unit/team',
                              'Private hospital - oncology unit/team',
                              'Private hospital - medical unit/team',
                              'Private hospital - surgical unit/team',
                              'Private hospital - emergency department',
                              'Outpatient clinic',
                              'General Practitioner',
                              'Specialist Practitioner',
                              'Community Palliative Care Service',
                              'Community Generalist Service',
                              'Residential Aged Care Facility',
                              'Self, carer(s), family, friends',
                              'Other',
                              'Unknown/inadequately described'))

  episode_setting <- tibble(episode_setting = c(10:12, 20:22, 30:32,
                                                10,11,20,21,30:32),
                            episode_setting_desc = c(
                               'Overnight Admitted - Not Further Specified',
                               'Overnight Admitted - Designated Palliative Care Bed',
                               'Overnight Admitted - Non-designated Palliative Care Bed',
                               'Hospital Ambulatory - Not Further Specified',
                               'Same Day Admitted',
                               'Outpatient',
                               'Community - Not Further Specified',
                               'Private Residence',
                               'Residential Aged Care Facility',

                               'Overnight Admitted - Designated Palliative Care Bed',
                               'Overnight Admitted - Non-Designated Palliative Care Bed',
                               'Same Day Admitted',
                               'Outpatient',
                               'Private Residence',
                               'Residential Aged Care Facility',
                               'Community - Other'),
                            data_version = c(rep(3,9), rep(4,7)))

  episode_start_mode <- tibble(episode_start_mode = c(11:16,19,21,22,99),
                               episode_start_mode_desc = c(
                                 'Admitted from usual accommodation',
                                 'Admitted from other than usual accommodation',
                                 'Admitted (transferred) from another hospital',
                                 'Admitted (transferred) from acute care in another ward',
                                 'Change from acute care to palliative care while remaining on same ward',
                                 'Change of sub-acute/non-acute care type',
                                 'Other',
                                 'Patient transferred from being an overnight admitted palliative care patient',
                                 'Patient was not transferred from being an overnight palliative care patient',
                                 'Not recorded'
                               ))

  episode_start_accommodation <- tibble(episode_start_accommodation = c(1,2,3,7,9),
                                        episode_start_accommodation_desc = c(
                                          'Private residence (including unit in retirement village)',
                                          'Residential aged care - low level care (hostel)',
                                          'Residential aged care - high level care (nursing home)',
                                          'Other',
                                          'Unknown'
                                        ))

  episode_end_mode <- tibble(episode_end_mode = c(11:19, 21:26, 29, 99),
                             episode_end_mode_desc = c(
                               'Discharged to usual accommodation',
                               'Discharged to other than usual accommodation',
                               'Death',
                               'Discharged to another hospital',
                               'Change from palliative care to acute care - different ward',
                               'Change from palliative care to acute care - same ward',
                               'Change in sub-acute care type',
                               'End of consultative episode - inpatient episode ongoing',
                               'Other',
                               'Discharge/case closure',
                               'Death',
                               'Discharged for inpatient palliative care',
                               'Discharged for inpatient acute care',
                               'Discharged to another palliative care service',
                               'Discharged to primary health care (e.g. GP)',
                               'Other',
                               'Not recorded'
                             ))

  episode_end_accommodation <- tibble(episode_end_accommodation = c(1,2,3,7,9),
                                      episode_end_accommodation_desc = c(
                                        'Private residence (including unit in retirement village)',
                                        'Residential aged care - low level care (hostel)',
                                        'Residential aged care - high level care (nursing home)',
                                        'Other',
                                        'Unknown'))

  place_of_death <- tibble(place_of_death = c(1,2,3,9, 1:7,9),
                           place_of_death_desc = c(
                             'Home',
                             'Residential Aged Care Facility',
                             'Hospital',
                             'Unknown',

                             "Private Residence",
                             "Residential Aged Care Facility",
                             "Group Home",
                             "Hospital",
                             "Correctional Facility",
                             "Other",
                             "Unknown",
                             "Not Recorded/Invalid"
                             ),
                           data_version = c(rep(3,4), rep(4,8)))


  phase_type <- tibble(phase_type = c(1:5),
                       phase_type_desc = c(
                         'Stable',
                         'Unstable',
                         'Deteriorating',
                         'Terminal',
                         'Bereavement/Post death support'
                       ))

  rug_bed <- tibble(rug_bed = c(1,3:5,9),
                    rug_bed_desc = c(
                       'Independent or supervision only',
                       'Limited physical assistance',
                       'Other than two persons physical assist',
                       'Two-person (or more) physical assist',
                       'Not assessed'
                     ))

  rug_toilet <- tibble(rug_toilet = c(1,3:5,9),
                       rug_toilet_desc = c(
                               'Independent or supervision only',
                               'Limited physical assistance',
                               'Other than two persons physical assist',
                               'Two-person (or more) physical assist',
                               'Not assessed'
                             ))

  rug_transfer <- tibble(rug_transfer = c(1,3:5,9),
                         rug_transfer_desc = c(
                                 'Independent or supervision only',
                                 'Limited physical assistance',
                                 'Other than two persons physical assist',
                                 'Two-person (or more) physical assist',
                                 'Not assessed'
                               ))

  rug_eating <- tibble(rug_eating = c(1:3,9),
                             rug_eating_desc = c(
                               'Independent or supervision only',
                               'Limited assistance',
                               'Extensive assistance/total dependence/tube fed',
                               'Not assessed'
                             ))

  pcpss <- tibble(pcpss = c(0:3,9),
                  pcpss_desc = c(
                    'Absent',
                    'Mild',
                    'Moderate',
                    'Severe',
                    'Not assessed'))

  sas <- tibble(sas = c(0:10),
                sas_desc = c(
                    'Absent',
                    rep('Mild', 3),
                    rep('Moderate', 4),
                    rep('Severe', 3) )
                )

  esas <- tibble(esas = c(0:10),
                 esas_desc = c(
                  'Absent',
                  rep('Mild', 3),
                  rep('Moderate', 4),
                  rep('Severe', 3) )
  )

  akps <- tibble(akps = c(sort(seq(10,100,by = 10), decreasing = TRUE),999),
                 akps_desc = c(
                   'Normal; no complaints; no evidence of disease',
                   'Able to carry on normal activity; minor signs or symptoms',
                   'Normal activity with effort; some signs or symptoms of disease',
                   'Cares for self; unable to carry on normal activity or to do active work',
                   'Requires occasional assistance but is able to care for most of his needs',
                   'Requires considerable assistance and frequent medical care',
                   'In bed more than 50% of the time',
                   'Almost completely bedfast',
                   'Totally bedfast and requiring extensive nursing care by professionals and/or family',
                   'Comatose or barely rousable',
                   'Not assessed'))

  phase_end_reason <- tibble(phase_end_reason = c(10, 20, 30, 40, 50, 60, 70, 99),
                             phase_end_reason_desc = c(
                               'Phase changed to Stable',
                               'Phase changed to Unstable',
                               'Phase changed to Deteriorating',
                               'Phase changed to Terminal',
                               'Death',
                               'End Bereavement Phase (Post Death Support)',
                               'Discharge/Case Closure',
                               'Not Recorded'))

  ## Version 4

  assessment_type <- tibble(assessment_type = c(1:3),
                            assessment_type_desc = c(
                             "Comprehensive Assessment",
                             "Ongoing Assessment",
                             "Separation Assessment"))

  referral_provider_type <- tibble(referral_provider_type = c(1:9,99),
                                   referral_provider_type_desc = c(
                                     "Public Hospital",
                                     "Private Hospital",
                                     "Outpatient Clinic",
                                     "Specialist Medical Practice",
                                     "General Medicine Practice",
                                     "Community Service",
                                     "Residential Aged Care Facility",
                                     "Self, Carer(s), Family, Friends",
                                     "Other",
                                     "Not Recorded/Invalid"
                                   ))

  referral_speciality <- tibble(referral_speciality = c(1:17,99),
                                   referral_speciality_desc = c(
                                     "Palliative Care",
                                     "Medical Oncology",
                                     "Radiation Oncology",
                                     "Respiratory Medicine",
                                     "Cardiology",
                                     "Haematology",
                                     "Geriatric Medicine",
                                     "Emergency Medicine",
                                     "Nephrology",
                                     "Neurology",
                                     "Rehabilitation Medicine",
                                     "Surgical",
                                     "General Medicine",
                                     "General Practice",
                                     "Other Primary Care",
                                     "Not Applicable - Self, Carer(s), Family, Friend",
                                     "Other",
                                     "Not Recorded/Invalid"
                                   ))

  setting_of_care <- tibble(setting_of_care = c(10,11,20,21,30:32),
                            setting_of_care_desc = c(
                              "Overnight Admitted - Designated Palliative Care Bed",
                              "Overnight Admitted - Non-Designated Palliative Care Bed",
                              "Same Day Admitted",
                              "Outpatient",
                              "Private Residence",
                              "Residential Aged Care Facility",
                              "Community - Other"
                            ))

  setting_of_care_group <- tibble(setting_of_care_group = c(1,2,3),
                            setting_of_care_group_desc = c(
                              "Inpatient",
                              "Ambulatory",
                              "Community"
                            ))

  patients_usual_accommodation <- tibble(patients_usual_accommodation = c(10,20,30,40,50,99),
                                        patients_usual_accommodation_desc = c(
                                          "Private Residence",
                                          "Residential Aged Care Facility",
                                          "Group Home",
                                          "Correctional Facility",
                                          "Other",
                                          "Not Recorded/Invalid"
                                        ))

  patient_location_after_assessment <- tibble(patient_location_after_assessment = c(1:9,99),
                                              patient_location_after_assessment_desc = c(
                                                "Inpatient - Different Hospital",
                                                "Inpatient - Different Ward in the Same Hospital",
                                                "Inpatient - Same Ward in the Same Hospital",
                                                "Inpatient",
                                                "Community-Usual Accommodation",
                                                "Community-Not Usual Accommodation, Private Residence",
                                                "Community-Not Usual Accommodation, Residential Aged Care",
                                                "Other",
                                                "Not Applicable - Patient Died",
                                                "Not Recorded/Invalid"
                                              ))

  carer_availability <- tibble(carer_availability = c(0,1,2,9),
                               carer_availability_desc = c(
                                 "No",
                                 "Yes",
                                 "Not Assessed",
                                 "Not Recorded/Invalid"
                               ))

  mode_of_assessment <- tibble(mode_of_assessment = c(1:3,9),
                               mode_of_assessment_desc = c(
                                 "In Person",
                                 "Videoconference",
                                 "Telephone",
                                 "Not Recorded/Invalid"
                               ))

  esas_delirium_rated_by <- tibble(esas_delirium_rated_by = c(1:4,9),
                                   esas_delirium_rated_by_desc = c(
                                     "Patient",
                                     "Family/Carer",
                                     "Clinician",
                                     "Other",
                                     "Not Recorded/Invalid Score"
                                   ))

  esas_other_symtpom <- tibble(esas_other_symtpom = c(1:5,8,9),
                               esas_other_symtpom_desc = c(
                                 "Dry or Sore Mouth",
                                 "Itch",
                                 "Swallowing",
                                 "Other",
                                 "No Other Symptoms Present",
                                 "Not Assessed",
                                 "Not Recorded/Invalid"
                                   ))

  ## create list
  list_dd <- list(sex = sex,
                  state = state,
                  indigenous_status = indigenous_status,
                  episode_setting = episode_setting,
                  country_of_birth = country_of_birth,
                  preferred_language = preferred_language,
                  broad_diagnosis = broad_diagnosis,
                  diagnosis = diagnosis,
                  referral_source = referral_source,
                  episode_start_mode = episode_start_mode,
                  episode_start_accommodation = episode_start_accommodation,
                  episode_end_mode = episode_end_mode,
                  episode_end_accommodation = episode_end_accommodation,
                  place_of_death = place_of_death,
                  phase_type = phase_type,
                  rug_bed = rug_bed,
                  rug_toilet = rug_toilet,
                  rug_transfer = rug_transfer,
                  rug_eating = rug_eating,
                  pcpss = pcpss,
                  sas = sas,
                  esas = esas,
                  akps = akps,
                  phase_end_reason = phase_end_reason,
                  assessment_type = assessment_type,
                  referral_provider_type = referral_provider_type,
                  referral_speciality = referral_speciality,
                  setting_of_care = setting_of_care,
                  patients_usual_accommodation = patients_usual_accommodation,
                  patient_location_after_assessment = patient_location_after_assessment,
                  carer_availability = carer_availability,
                  mode_of_assessment = mode_of_assessment,
                  esas_delirium_rated_by = esas_delirium_rated_by,
                  esas_other_symtpom = esas_other_symtpom

  )

  return_table <- list_dd[[variable]]

  if(length(return_table)==0){
    return_table <- paste('The variable selected does not exist in this function. Included in this function are ',paste(attributes(list_dd)$names, collapse = ', ' ))
  }
  return(return_table)
}

# highchart theme
# require(highcharter)
pcoc_theme_hc <- highcharter::hc_theme(
  chart = list(
    style = list(
      fontFamily = "Montserrat, sans-serif",
      fontSize = "14px"
    )
  ),
  title = list(
    style = list(
      fontWeight = "bold",
      fontSize = "18px",
      fontFamily = "Montserrat, sans-serif"
    )
  ),
  subtitle = list(
    style = list(
      fontSize = "14px",
      fontFamily = "Montserrat, sans-serif"
    )
  ),
  xAxis = list(
    labels = list(
      style = list(
        fontSize = "12px",
        fontFamily = "Montserrat, sans-serif"
      )
    )
  ),
  yAxis = list(
    labels = list(
      style = list(
        fontSize = "12px",
        fontFamily = "Montserrat, sans-serif"
      )
    )
  ),
  legend = list(
    itemStyle = list(
      fontSize = "13px",
      fontFamily = "Montserrat, sans-serif"
    )
  )
)

# pre filter df_report_all for add filter elements
# require(rlang)

benchmarks <- function(data = df_report_all, report_ids = c('P291'), benchmarks = 'all', group_by = c('report', 'report_id',"episode_setting_group"), rank_within = c('report','episode_setting_group'), current_report = 38, Report_ID_split = 'N'){

  curr_report_num <- current_report

  result <- list()

  if(benchmarks %in% c('1','all')){

    if(length(report_ids)==1){

      tbl_bm1 <- data %>%
        filter(!is.na(first_phase),
               ready_for_care_to_episode_start >= 0,
               flag_start_in_scope == 1,
               flag_first_phase == 1,
               !is.na(report_id),
               episode_setting_group %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ready_for_care_to_episode_start <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){

      tbl_bm1 <- data %>%
        filter(!is.na(first_phase),
               ready_for_care_to_episode_start >= 0,
               flag_start_in_scope == 1,
               flag_first_phase == 1,
               !is.na(report_id),
               episode_setting_group %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ready_for_care_to_episode_start <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        mutate(comb_reportid = ifelse(report_id %in% c(report_ids), 'sel',report_id)) %>%
        group_by(report, comb_reportid) %>%
        mutate(report_id = ifelse(sel_serv == 'Y', 'sel',report_id),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){

      tbl_bm1 <- data %>%
        filter(!is.na(first_phase),
               ready_for_care_to_episode_start >= 0,
               flag_start_in_scope == 1,
               flag_first_phase == 1,
               !is.na(report_id),
               episode_setting_group %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ready_for_care_to_episode_start <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    tbl_bm1_all_curr <- tbl_bm1 %>%
      filter(report == curr_report_num)

    tbl_bm1_sel_ot <- tbl_bm1 %>%
      filter(sel_serv == 'Y')

    tbl_bm1_sel_curr <- tbl_bm1 %>%
      filter(sel_serv == 'Y',
             report == curr_report_num)


    result$tbl_bm1 <- tbl_bm1
    result$tbl_bm1_all_curr <- tbl_bm1_all_curr
    result$tbl_bm1_sel_ot <- tbl_bm1_sel_ot
    result$tbl_bm1_sel_curr <- tbl_bm1_sel_curr
  }

  if(benchmarks %in% c('2','all')){

    if(length(report_ids)==1){
      tbl_bm2 <- data %>%
        filter(phase_type == 2,
               flag_less_than_4 %in% c(0,1),
               !is.na(report_id),
               episode_setting_group %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(phase_length <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){
      tbl_bm2 <- data %>%
        filter(phase_type == 2,
               flag_less_than_4 %in% c(0,1),
               !is.na(report_id),
               episode_setting_group %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(phase_length <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        mutate(comb_reportid = ifelse(report_id %in% c(report_ids), 'sel',report_id)) %>%
        group_by(report, comb_reportid) %>%
        mutate(report_id = ifelse(sel_serv == 'Y', 'sel',report_id),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){
      tbl_bm2 <- data %>%
        filter(phase_type == 2,
               flag_less_than_4 %in% c(0,1),
               !is.na(report_id),
               episode_setting_group %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(phase_length <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    tbl_bm2_all_curr <- tbl_bm2 %>%
      filter(report == curr_report_num)

    tbl_bm2_sel_ot <- tbl_bm2 %>%
      filter(sel_serv == 'Y')

    tbl_bm2_sel_curr <- tbl_bm2 %>%
      filter(sel_serv == 'Y',
             report == curr_report_num)

      result$tbl_bm2 <- tbl_bm2
      result$tbl_bm2_all_curr <- tbl_bm2_all_curr
      result$tbl_bm2_sel_ot <- tbl_bm2_sel_ot
      result$tbl_bm2_sel_curr <- tbl_bm2_sel_curr
  }

  if(benchmarks %in% c('3','all')){

    df_bm3 <- data %>%
      filter(!is.na(report_id),
             episode_setting_group %in% c(1:3)) %>%
      mutate(bm31_phases = case_when(pcpss_pain_start_group == 0 & pcpss_pain_end_group == 0 & phase_end_reason != 50 ~ 'met',
                                     pcpss_pain_start_group == 0 & pcpss_pain_end_group == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm32_phases = case_when(pcpss_pain_start_group == 1 & pcpss_pain_end_group == 0 & phase_end_reason != 50 ~ 'met',
                                     pcpss_pain_start_group == 1 & pcpss_pain_end_group == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm33_phases = case_when(esas_pain_group_start == 0 & esas_pain_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_pain_group_start == 0 & esas_pain_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm34_phases = case_when(esas_pain_group_start == 1 & esas_pain_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_pain_group_start == 1 & esas_pain_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm35_phases = case_when(esas_tiredness_group_start == 0 & esas_tiredness_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_tiredness_group_start == 0 & esas_tiredness_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm36_phases = case_when(esas_tiredness_group_start == 1 & esas_tiredness_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_tiredness_group_start == 1 & esas_tiredness_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm37_phases = case_when(esas_shortness_of_breath_group_start == 0 & esas_shortness_of_breath_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_shortness_of_breath_group_start == 0 & esas_shortness_of_breath_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm38_phases = case_when(esas_shortness_of_breath_group_start == 1 & esas_shortness_of_breath_group_end == 0 & phase_end_reason != 50 ~ 'met',
                                     esas_shortness_of_breath_group_start == 1 & esas_shortness_of_breath_group_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm39_phases = case_when(pcpss_family_carer_start_group == 0 & pcpss_family_carer_end_group == 0 & phase_end_reason != 50 ~ 'met',
                                     pcpss_family_carer_start_group == 0 & pcpss_family_carer_end_group == 1 & phase_end_reason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm310_phases = case_when(pcpss_family_carer_start_group == 1 & pcpss_family_carer_end_group == 0 & phase_end_reason != 50 ~ 'met',
                                      pcpss_family_carer_start_group == 1 & pcpss_family_carer_end_group == 1 & phase_end_reason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'),
             bm311_phases = case_when(pcpss_psychological_spiritual_start == 0 & pcpss_psychological_spiritual_end == 0 & phase_end_reason != 50 ~ 'met',
                                      pcpss_psychological_spiritual_start == 0 & pcpss_psychological_spiritual_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'),
             bm312_phases = case_when(pcpss_psychological_spiritual_start == 1 & pcpss_psychological_spiritual_end == 0 & phase_end_reason != 50 ~ 'met',
                                      pcpss_psychological_spiritual_start == 1 & pcpss_psychological_spiritual_end == 1 & phase_end_reason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'))

    df_bm3 <- df_bm3 %>%
      select(report, service_id, report_id, patient_id, episode_id, episode_setting_group, phase_type, phase_id, starts_with('bm3')) %>%
      filter(bm31_phases %in% c('met','nmet')|
               bm32_phases %in% c('met','nmet')|
               bm33_phases %in% c('met','nmet')|
               bm34_phases %in% c('met','nmet')|
               bm35_phases %in% c('met','nmet')|
               bm36_phases %in% c('met','nmet')|
               bm37_phases %in% c('met','nmet')|
               bm38_phases %in% c('met','nmet')|
               bm39_phases %in% c('met','nmet')|
               bm310_phases %in% c('met','nmet')|
               bm311_phases %in% c('met','nmet')|
               bm312_phases %in% c('met','nmet'))

    #
    process_phases <- function(data = df_bm3, phase_col = "bm32_phases") {

      dd <- data %>%
        filter(.data[[phase_col]] %in% c('met', 'nmet'),
        ) %>%
        group_by(!!!syms(group_by)) %>%
        count(.data[[phase_col]]) %>%
        ungroup() %>%
        pivot_wider(id_cols = c(!!!syms(group_by)),
                    names_from = .data[[phase_col]],
                    values_from = n)

      if (!"nmet" %in% colnames(dd)) {
        dd$`nmet` <- 0
      }

      if (!"met" %in% colnames(dd)) {
        dd$met <- 0
      }

      dd <- dd %>%
        mutate(met = replace_na(met,0),
               `nmet` = replace_na(`nmet`, 0))

      dd <- dd %>%
        mutate(perc_met = met / (`nmet` + met) * 100,
               bm = phase_col)
      return(dd)
    }

    # bm3.1
    tbl_bm3 <- process_phases(df_bm3, "bm31_phases")

    # bm3.2
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm32_phases"))

    #bm3.3
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm33_phases"))

    #bm3.4
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm34_phases"))

    #bm3.5
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm35_phases"))

    #bm3.6
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm36_phases"))

    #bm3.7
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm37_phases"))

    #bm3.8
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm38_phases"))

    #bm3.9
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm39_phases"))

    #bm3.10
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm310_phases"))

    #bm3.11
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm311_phases"))

    #bm3.12
    tbl_bm3 <- bind_rows(tbl_bm3,process_phases(df_bm3, "bm312_phases"))

    if(length(report_ids)==1){
      tbl_bm3 <- tbl_bm3 %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){
      tbl_bm3 <- tbl_bm3 %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        mutate(comb_reportid = ifelse(report_id %in% c(report_ids), 'sel',report_id)) %>%
        group_by(report, comb_reportid, bm) %>%
        mutate(report_id = ifelse(sel_serv == 'Y', 'sel',report_id),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               nmet = ifelse(sel_serv == 'Y', sum(nmet),nmet),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){
      tbl_bm3 <- tbl_bm3 %>%
        mutate(sel_serv = ifelse(report_id %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (report_id %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }


    tbl_bm3_all_curr <- tbl_bm3 %>%
      filter(report == curr_report_num)

    tbl_bm3_sel_ot <- tbl_bm3 %>%
      filter(sel_serv == 'Y')

    tbl_bm3_sel_curr <- tbl_bm3 %>%
      filter(sel_serv == 'Y',
             report == curr_report_num)

      result$tbl_bm3 <- tbl_bm3
      result$tbl_bm3_all_curr <- tbl_bm3_all_curr
      result$tbl_bm3_sel_ot <- tbl_bm3_sel_ot
      result$tbl_bm3_sel_curr <- tbl_bm3_sel_curr

  }

  if(benchmarks %in% c('4','all')){

      tbl_bm4 <- data %>%
        filter(phase_end_reason != 50,
               !is.na(report_id),
               episode_setting_group %in% c(1:3),
               report_id %in% c(report_ids)) %>%
        mutate(report_id = ifelse(report_id %in% c(report_ids), 'sel',report_id)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(
          across(
            c(pcpss_pain_xcas_2014, pcpss_other_symptoms_xcas_2014, pcpss_family_xcas_2014,
              pcpss_psychological_xcas_2014, esas_pain_xcas_2014, esas_nausea_xcas_2014,
              esas_shortness_of_breath_xcas_2014, esas_constipation_xcas_2014),
            list(mean = ~ mean(., na.rm = TRUE))
          )
        ) %>%
        ungroup() %>%
        pivot_longer(cols = ends_with('mean'),
                     names_to = 'name',
                     values_to = 'XCAS') %>%
        mutate(name = gsub('_XCAS_2014_mean','',name)) %>%
        mutate(bm = case_when(grepl('PSSPain',name) ~ 'bm41_phases',
                              grepl('PSSOtherSymptoms',name) ~ 'bm42_phases',
                              grepl('PSSFamily',name) ~ 'bm43_phases',
                              grepl('PSSPsychological',name) ~ 'bm44_phases',
                              grepl('SASPain',name) ~ 'bm45_phases',
                              grepl('SASNausea',name) ~ 'bm46_phases',
                              grepl('SASBreathing',name) ~ 'bm47_phases',
                              grepl('SASBowels',name) ~ 'bm48_phases',
                              TRUE ~ '-'))

    tbl_bm4_sel_curr <- tbl_bm4 %>%
      filter(report == curr_report_num)

    result$tbl_bm4_sel_curr <- tbl_bm4_sel_curr

  }

  return(result)

}
# benchmarks(data = df_report_all, report_ids = c('P291'), benchmarks = 'all')

# df_preferred_language <- read.csv('Preferred Language.csv')
# df_country_of_birth <- read.csv('Country Of Birth.csv')
# usethis::use_data(df_preferred_language, overwrite = TRUE)
# usethis::use_data(df_country_of_birth, overwrite = TRUE)
# pref <- c('1201','9403','1201')

preferred_language <- function(language_codes = pref){
   data(df_preferred_language)
   df_lookup <- df_preferred_language %>%
     mutate(preferred_language = as.character(preferred_language))
   require(tidyverse)
   df_codes <- tibble(preferred_language = as.character(language_codes))
   df_codes <- df_codes %>%
     left_join(df_lookup,
               by = 'preferred_language') %>%
     pull(PreferredLanguageDesc)

   return(df_codes)
}
# preferred_language(pref)

country_of_birth <- function(cob_codes = cob_codes){
  data(df_country_of_birth)
  df_lookup <- df_country_of_birth %>%
    mutate(country_of_birth = as.character(country_of_birth))
  require(tidyverse)
  df_codes <- tibble(country_of_birth = as.character(cob_codes))
  df_codes <- df_codes %>%
    left_join(df_lookup,
              by = 'country_of_birth') %>%
    pull(CountryOfBirthDesc)

  return(df_codes)
}

# # X-CAS calculations
# require(xlsx)
# df_xcas_pcpss <- read.xlsx("S:/CHSD PCOC/10 Processed Data/X-CAS Baselines/2014 Baseline/SAS and PCPSS XCAS Baselines.xlsx",sheetIndex = 1)
# df_xcas_esas <- read.xlsx("S:/CHSD PCOC/10 Processed Data/X-CAS Baselines/2014 Baseline/SAS and PCPSS XCAS Baselines.xlsx",sheetIndex = 2)
#
# df_xcas_pcpss <- df_xcas_pcpss %>%
#   rename(phase_type = Phase,
#          start_score = StartScore,
#          pain = Pain,
#          other_symptom = OtherSymptoms,
#          pyschological_spiritual = Psychological,
#          family_carer = Family) %>%
#   mutate(phase_type = as.character(phase_type),
#          start_score = as.character(start_score))
#
# df_xcas_esas <- df_xcas_esas %>%
#   rename(phase_type = Phase,
#          start_score = StartScore,
#          pain = Pain,
#          nausea = Nausea,
#          constipation = Bowels,
#          shortness_of_breath = Breathing) %>%
#   mutate(phase_type = as.character(phase_type),
#          start_score = as.character(start_score))

# usethis::use_data(df_xcas_esas, overwrite = TRUE)
# usethis::use_data(df_xcas_pcpss, overwrite = TRUE)

add_xcas_esas <- function(df = df_bm, symptom = 'pain', xcas_data = df_xcas_esas) {
  # Create column names dynamically
  symptom_col <- paste0("esas_", symptom, "_xcas_baseline")
  start_col <- paste0("esas_", symptom,"_start")

  df <- df %>%
    rename(start_score = as.name(start_col)) %>%
    left_join(
      xcas_data %>% select(phase_type, start_score, !!sym(symptom)),
      by = c("phase_type", "start_score")
    ) %>%
    rename_with(~ symptom_col, !!sym(symptom)) %>%
    rename(!!sym(start_col) := start_score)

  return(df)
}

# Define the function to add PCPSS XCAS baseline values
add_xcas_pss <- function(df=df_bm, symptom = 'pain', xcas_data = df_xcas_pcpss) {
  # Create column names dynamically
  symptom_col <- paste0("pcpss_", symptom, "_xcas_baseline")
  start_col <- paste0("pcpss_", symptom,"_start")

  df <- df %>%
    rename(start_score = as.name(start_col)) %>%
    left_join(
      xcas_data %>% select(phase_type, start_score, !!sym(symptom)),
      by = c("phase_type", "start_score")
    ) %>%
    rename_with(~ symptom_col, !!sym(symptom)) %>%
    rename(!!sym(start_col) := start_score)

  return(df)
}
