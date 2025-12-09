#require(tidyverse)
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

report_label <- function(Report = 37){
  period <- case_when(Report %% 2 == 1 ~ paste0('January - June ', 2000 + (Report + 11) %/% 2),
                      Report %% 2 == 0 ~ paste0('July - December ', 2000 + (Report + 11) %/% 2),
                      TRUE ~ 'Other')
  return(period)
}

# preferred_language <- function(preferred_language = 3904){
#
#   # group indigenous langauges
#   df_pref <- read.csv('Preferred Language.csv')
#
#   df_pref <- df_pref %>%
#     mutate(PreferredLanguageDesc = case_when(substr(PreferredLanguage,1,2) %in% c('81', '82', '83', '84', '85', '86', '87', '88', '89') ~ 'Australian Indigenous Languages',
#                                              TRUE ~ PreferredLanguageDesc)) %>%
#     bind_rows(tibble(PreferredLanguage = c(-1,-2,-3,-4,-5),
#                      PreferredLanguageDesc = c('Not Stated','Not Stated','Not Stated','Not Stated','Not Stated')))
#
#   prefdesc <- df_pref %>%
#     filter(PreferredLanguage == preferred_language) %>%
#     pull(PreferredLanguageDesc)
#
#   return(prefdesc)
# }

pcoc_dd <- function(variable = 'sex'){
  sex <- tibble(Sex = c(1,2,3,9),
                SexDesc = c(
                  'Male',
                  'Female',
                  'Indeterminate',
                  'Not Stated/Inadequately Described'))

  state <- tibble(State = c(1:10,99),
                  StateAbb = c('NSW',
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
                  StateDesc = c(
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

  indigenous_status <- tibble(IndigenousStatus = c(1:4,9),
                              IndigenousStatusDesc = c(
                                'Aboriginal but not Torres Strait Islander Origin',
                                'Torres Strait Islander but not Aboriginal Origin',
                                'Both Aboriginal and Torres Strait Islander Origin',
                                'Neither Aboriginal nor Torres Strait Islander origin',
                                'Not Stated /Inadequately Described'))

  episode_setting <- tibble(EpisodeSetting = c(1:3),
                            EpisodeSettingDesc = c(
                              'Inpatient',
                              'Ambulatory',
                              'Community'))

  country_of_birth <- tibble(CountryOfBirth = c(1101, 2102, 7103, 6101, 1201, 5204, 5105, 9225,
                                                5203, 3104, 7107, 2105, 7105, 8104, 2304, 99),
                             CountryOfBirthDesc = c('Australia', 'England', 'India', 'China', 'New Zealand', 'Philippines',
                                                    'Vietnam', 'South Africa', 'Malaysia', 'Italy', 'Sri Lanka',
                                                    'Scotland', 'Nepal', 'United States of America', 'Germany',
                                                    'All other countries')
                             )

  preferred_language <- tibble(PreferredLanguage = c(12, 71, 52, 42, 63, 65, 51, 23, 22, 24, 35, 92, 73,
                                                93, 80),
                               PreferredLanguageDesc = c(
                               'English', 'Chinese', 'Hindi', 'Arabic', 'Vietnamese', 'Filipino/Indonesian',
                               'Tamil/Malayam', 'Spanish', 'Greek', 'Italian', 'Macedonian/Croatian',
                               'African languages', 'Korean', 'Samoan/Tongan', 'Australian Indigenous languages'))

  broad_diagnosis <- tibble(BroadDiagnosis = c(1:2),
                            BroadDiagnosisDesc = c(
                              'Malignant',
                              'Non-Malignant'))

  diagnosis <- tibble(Diagnosis = c(100:114, 180, 200:213, 280, 999),
                      DiagnosisDesc = c(
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
                        'Gynecological',
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
                        'Unknown'))

  referral_source <- tibble(ReferralSource = c(10:15, 20:25, 30, 40, 50, 60, 61, 70, 80, 90, 99),
                            ReferralSourceDesc = c(
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

  episode_type <- tibble(EpisodeType = c(10:12, 20:22, 30:32),
                         EpisodeTypeDesc = c(
                           'Overnight Admitted - Not Further Specified',
                           'Overnight Admitted - Designated Palliative Care Bed',
                           'Overnight Admitted - Non-designated Palliative Care Bed',
                           'Hospital Ambulatory - Not Further Specified',
                           'Same Day Admitted',
                           'Outpatient',
                           'Community - Not Further Specified',
                           'Private Residence',
                           'Residential Aged Care Facility'))

  episode_start_mode <- tibble(EpisodeStartMode = c(11:16,19,21,22,99),
                               EpisodeStartModeDesc = c(
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

  episode_start_accommodation <- tibble(EpisodeStartAccommodation = c(1,2,3,7,9),
                                        EpisodeStartAccommodationDesc = c(
                                          'Private residence (including unit in retirement village)',
                                          'Residential aged care - low level care (hostel)',
                                          'Residential aged care - high level care (nursing home)',
                                          'Other',
                                          'Unknown'
                                        ))

  episode_end_mode <- tibble(EpisodeEndMode = c(11:19, 21:26, 29, 99),
                             EpisodeEndModeDesc = c(
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

  episode_end_accommodation <- tibble(EpisodeEndAccommodation = c(1,2,3,7,9),
                                      EpisodeEndAccommodationDesc = c(
                                        'Private residence (including unit in retirement village)',
                                        'Residential aged care - low level care (hostel)',
                                        'Residential aged care - high level care (nursing home)',
                                        'Other',
                                        'Unknown'))

  place_of_death <- tibble(PlaceOfDeath = c(1,2,3,9),
                           PlaceOfDeathDesc = c(
                             'Home',
                             'Residential Aged Care Facility',
                             'Hospital',
                             'Unknown'))

  phase_type <- tibble(PhaseType = c(1:5),
                       PhaseTypeDesc = c(
                         'Stable',
                         'Unstable',
                         'Deteriorating',
                         'Terminal',
                         'Bereavement/Post death support'
                       ))

  rug_start_mobility <- tibble(RugStartMobility = c(1,3:5,9),
                               RugStartMobilityDesc = c(
                                 'Independent or supervision only',
                                 'Limited physical assistance',
                                 'Other than two persons physical assist',
                                 'Two-person (or more) physical assist',
                                 'Not assessed'
                               ))

  rug_start_toilet <- tibble(RugStartToileting = c(1,3:5,9),
                             RugStartToiletingDesc = c(
                               'Independent or supervision only',
                               'Limited physical assistance',
                               'Other than two persons physical assist',
                               'Two-person (or more) physical assist',
                               'Not assessed'
                             ))

  rug_start_transfer <- tibble(RugStartTransfer = c(1,3:5,9),
                               RugStartTransferDesc = c(
                                 'Independent or supervision only',
                                 'Limited physical assistance',
                                 'Other than two persons physical assist',
                                 'Two-person (or more) physical assist',
                                 'Not assessed'
                               ))

  rug_start_eating <- tibble(RugStartEating = c(1:3,9),
                             RugStartEatingDesc = c(
                               'Independent or supervision only',
                               'Limited assistance',
                               'Extensive assistance/total dependence/tube fed',
                               'Not assessed'
                             ))

  pcpss <- tibble(PCPSS = c(0:3,9),
                  PCPSSDesc = c(
                    'Absent',
                    'Mild',
                    'Moderate',
                    'Severe',
                    'Not assessed'))

  sas <- tibble(SAS = c(0:10),
                SASDesc = c(
                    'Absent',
                    rep('Mild', 3),
                    rep('Moderate', 4),
                    rep('Severe', 3) )
                )

  akps <- tibble(AKPS = c(sort(seq(10,100,by = 10), decreasing = TRUE),999),
                 AKPSDesc = c(
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

  phase_end_reason <- tibble(PhaseEndReason = c(10, 20, 30, 40, 50, 60, 70, 99),
                             PhaseEndReasonDesc = c(
                               'Phase changed to Stable',
                               'Phase changed to Unstable',
                               'Phase changed to Deteriorating',
                               'Phase changed to Terminal',
                               'Death',
                               'End Bereavement Phase (Post Death Support)',
                               'Discharge/Case Closure',
                               'Not Recorded'))

  list_dd <- list(sex = sex,
                  state = state,
                  indigenous_status = indigenous_status,
                  episode_setting = episode_setting,
                  country_of_birth = country_of_birth,
                  preferred_language = preferred_language,
                  broad_diagnosis = broad_diagnosis,
                  diagnosis = diagnosis,
                  referral_source = referral_source,
                  episode_type = episode_type,
                  episode_start_mode = episode_start_mode,
                  episode_start_accommodation = episode_start_accommodation,
                  episode_end_mode = episode_end_mode,
                  episode_end_accommodation = episode_end_accommodation,
                  place_of_death = place_of_death,
                  phase_type = phase_type,
                  rug_start_mobility = rug_start_mobility,
                  rug_start_toilet = rug_start_toilet,
                  rug_start_transfer = rug_start_transfer,
                  rug_start_eating = rug_start_eating,
                  pcpss = pcpss,
                  sas = sas,
                  akps = akps,
                  phase_end_reason = phase_end_reason
  )

  return_table <- list_dd[[variable]]

  if(length(return_table)==0){
    return_table <- paste('The variable selected does not exist in this function. Included in this function are ',paste(attributes(list_dd)$names, collapse = ', ' ))
  }
  return(return_table)
}

# highchart theme
require(highcharter)
pcoc_theme_hc <- hc_theme(
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
require(rlang)

benchmarks <- function(data = df_report_all, report_ids = c('P291'), benchmarks = 'all', group_by = c('Report', 'Report_ID',"EpisodeSetting"), rank_within = c('Report','EpisodeSetting'), current_report = 38, Report_ID_split = 'N'){

  curr_report_num <- current_report

  result <- list()

  if(benchmarks %in% c('1','all')){

    if(length(report_ids)==1){

      tbl_bm1 <- data %>%
        filter(!is.na(FirstPhase_),
               ReadyForCareToEpisodeStart >= 0,
               Flag_StartInScope == 1,
               Flag_FirstPhase == 1,
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ReadyForCareToEpisodeStart <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){

      tbl_bm1 <- data %>%
        filter(!is.na(FirstPhase_),
               ReadyForCareToEpisodeStart >= 0,
               Flag_StartInScope == 1,
               Flag_FirstPhase == 1,
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ReadyForCareToEpisodeStart <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        mutate(comb_reportid = ifelse(Report_ID %in% c(report_ids), 'sel',Report_ID)) %>%
        group_by(Report, comb_reportid) %>%
        mutate(Report_ID = ifelse(sel_serv == 'Y', 'sel',Report_ID),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){

      tbl_bm1 <- data %>%
        filter(!is.na(FirstPhase_),
               ReadyForCareToEpisodeStart >= 0,
               Flag_StartInScope == 1,
               Flag_FirstPhase == 1,
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)) %>%
        ungroup() %>%
        mutate(bm1_met = ifelse(ReadyForCareToEpisodeStart <= 1, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm1_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    tbl_bm1_all_curr <- tbl_bm1 %>%
      filter(Report == curr_report_num)

    tbl_bm1_sel_ot <- tbl_bm1 %>%
      filter(sel_serv == 'Y')

    tbl_bm1_sel_curr <- tbl_bm1 %>%
      filter(sel_serv == 'Y',
             Report == curr_report_num)


    result$tbl_bm1 <- tbl_bm1
    result$tbl_bm1_all_curr <- tbl_bm1_all_curr
    result$tbl_bm1_sel_ot <- tbl_bm1_sel_ot
    result$tbl_bm1_sel_curr <- tbl_bm1_sel_curr
  }

  if(benchmarks %in% c('2','all')){

    if(length(report_ids)==1){
      tbl_bm2 <- data %>%
        filter(PhaseType == 2,
               Flag_LessThan4 %in% c(0,1),
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(PhaseLength <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){
      tbl_bm2 <- data %>%
        filter(PhaseType == 2,
               Flag_LessThan4 %in% c(0,1),
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(PhaseLength <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        mutate(comb_reportid = ifelse(Report_ID %in% c(report_ids), 'sel',Report_ID)) %>%
        group_by(Report, comb_reportid) %>%
        mutate(Report_ID = ifelse(sel_serv == 'Y', 'sel',Report_ID),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){
      tbl_bm2 <- data %>%
        filter(PhaseType == 2,
               Flag_LessThan4 %in% c(0,1),
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3)
        ) %>%
        mutate(bm2_met = ifelse(PhaseLength <= 3, 1,0)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(met = sum(bm2_met, na.rm = T),
                  denom = n()) %>%
        ungroup() %>%
        unique() %>%
        filter(denom != 0) %>%
        mutate(perc_met = met/denom*100) %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N')) %>%
        ungroup() %>%
        group_by(!!!syms(rank_within)) %>%
        arrange(!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    tbl_bm2_all_curr <- tbl_bm2 %>%
      filter(Report == curr_report_num)

    tbl_bm2_sel_ot <- tbl_bm2 %>%
      filter(sel_serv == 'Y')

    tbl_bm2_sel_curr <- tbl_bm2 %>%
      filter(sel_serv == 'Y',
             Report == curr_report_num)

      result$tbl_bm2 <- tbl_bm2
      result$tbl_bm2_all_curr <- tbl_bm2_all_curr
      result$tbl_bm2_sel_ot <- tbl_bm2_sel_ot
      result$tbl_bm2_sel_curr <- tbl_bm2_sel_curr
  }

  if(benchmarks %in% c('3','all')){

    df_bm3 <- data %>%
      filter(!is.na(Report_ID),
             EpisodeSetting %in% c(1:3)) %>%
      mutate(bm31_phases = case_when(PSSStartPainGroup == 0 & PSSEndPainGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     PSSStartPainGroup == 0 & PSSEndPainGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm32_phases = case_when(PSSStartPainGroup == 1 & PSSEndPainGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     PSSStartPainGroup == 1 & PSSEndPainGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm33_phases = case_when(SASStartPainGroup == 0 & SASEndPainGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartPainGroup == 0 & SASEndPainGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm34_phases = case_when(SASStartPainGroup == 1 & SASEndPainGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartPainGroup == 1 & SASEndPainGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm35_phases = case_when(SASStartFatigueGroup == 0 & SASEndFatigueGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartFatigueGroup == 0 & SASEndFatigueGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm36_phases = case_when(SASStartFatigueGroup == 1 & SASEndFatigueGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartFatigueGroup == 1 & SASEndFatigueGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm37_phases = case_when(SASStartBreathingGroup == 0 & SASEndBreathingGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartBreathingGroup == 0 & SASEndBreathingGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm38_phases = case_when(SASStartBreathingGroup == 1 & SASEndBreathingGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     SASStartBreathingGroup == 1 & SASEndBreathingGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm39_phases = case_when(PSSStartFamilyGroup == 0 & PSSEndFamilyGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                     PSSStartFamilyGroup == 0 & PSSEndFamilyGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                     TRUE ~ 'Not recorded'),
             bm310_phases = case_when(PSSStartFamilyGroup == 1 & PSSEndFamilyGroup == 0 & PhaseEndReason != 50 ~ 'met',
                                      PSSStartFamilyGroup == 1 & PSSEndFamilyGroup == 1 & PhaseEndReason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'),
             bm311_phases = case_when(PSSStartPsychological == 0 & PSSEndPsychological == 0 & PhaseEndReason != 50 ~ 'met',
                                      PSSStartPsychological == 0 & PSSEndPsychological == 1 & PhaseEndReason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'),
             bm312_phases = case_when(PSSStartPsychological == 1 & PSSEndPsychological == 0 & PhaseEndReason != 50 ~ 'met',
                                      PSSStartPsychological == 1 & PSSEndPsychological == 1 & PhaseEndReason != 50 ~ 'nmet',
                                      TRUE ~ 'Not recorded'))

    df_bm3 <- df_bm3 %>%
      select(Report, Service_ID, Report_ID, Patient_ID, Episode_ID, EpisodeSetting, PhaseType, Phase_ID, starts_with('bm3')) %>%
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
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()
    }

    if(length(report_ids)>1 & Report_ID_split == 'N'){
      tbl_bm3 <- tbl_bm3 %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        mutate(comb_reportid = ifelse(Report_ID %in% c(report_ids), 'sel',Report_ID)) %>%
        group_by(Report, comb_reportid, bm) %>%
        mutate(Report_ID = ifelse(sel_serv == 'Y', 'sel',Report_ID),
               met = ifelse(sel_serv == 'Y', sum(met),met),
               nmet = ifelse(sel_serv == 'Y', sum(nmet),nmet),
               denom = ifelse(sel_serv == 'Y', sum(denom),denom)) %>%
        select(-perc_met) %>%
        unique() %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }

    if(length(report_ids)>1 & Report_ID_split == 'Y'){
      tbl_bm3 <- tbl_bm3 %>%
        mutate(sel_serv = ifelse(Report_ID %in% c(report_ids), 'Y','N'),
               denom = met + nmet) %>%
        mutate(perc_met = met/denom*100) %>%
        ungroup() %>%
        group_by(bm,!!!syms(rank_within)) %>%
        arrange(bm,!!!lapply(rank_within, function(col) call2(desc, sym(col))), desc(perc_met)) %>%
        filter(denom >= 10 | (Report_ID %in% report_ids & denom != 0)) %>%
        mutate(rank = row_number()) %>%
        mutate(cdf = 100 - (rank - 1) / (max(rank) - 1) * 100) %>%
        ungroup()

    }


    tbl_bm3_all_curr <- tbl_bm3 %>%
      filter(Report == curr_report_num)

    tbl_bm3_sel_ot <- tbl_bm3 %>%
      filter(sel_serv == 'Y')

    tbl_bm3_sel_curr <- tbl_bm3 %>%
      filter(sel_serv == 'Y',
             Report == curr_report_num)

      result$tbl_bm3 <- tbl_bm3
      result$tbl_bm3_all_curr <- tbl_bm3_all_curr
      result$tbl_bm3_sel_ot <- tbl_bm3_sel_ot
      result$tbl_bm3_sel_curr <- tbl_bm3_sel_curr

  }

  if(benchmarks %in% c('4','all')){

      tbl_bm4 <- data %>%
        filter(PhaseEndReason != 50,
               !is.na(Report_ID),
               EpisodeSetting %in% c(1:3),
               Report_ID %in% c(report_ids)) %>%
        mutate(Report_ID = ifelse(Report_ID %in% c(report_ids), 'sel',Report_ID)) %>%
        group_by(!!!syms(group_by)) %>%
        summarise(
          across(
            c(PSSPain_XCAS_2014, PSSOtherSymptoms_XCAS_2014, PSSFamily_XCAS_2014,
              PSSPsychological_XCAS_2014, SASPain_XCAS_2014, SASNausea_XCAS_2014,
              SASBreathing_XCAS_2014, SASBowels_XCAS_2014),
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
      filter(Report == curr_report_num)

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
     mutate(PreferredLanguage = as.character(PreferredLanguage))
   require(tidyverse)
   df_codes <- tibble(PreferredLanguage = as.character(language_codes))
   df_codes <- df_codes %>%
     left_join(df_lookup,
               by = 'PreferredLanguage') %>%
     pull(PreferredLanguageDesc)

   return(df_codes)
}
# preferred_language(pref)

country_of_birth <- function(cob_codes = cob_codes){
  data(df_country_of_birth)
  df_lookup <- df_country_of_birth %>%
    mutate(CountryOfBirth = as.character(CountryOfBirth))
  require(tidyverse)
  df_codes <- tibble(CountryOfBirth = as.character(cob_codes))
  df_codes <- df_codes %>%
    left_join(df_lookup,
              by = 'CountryOfBirth') %>%
    pull(CountryOfBirthDesc)

  return(df_codes)
}
