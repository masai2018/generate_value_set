################################################################################
### new provider with Taxonomy1
### version controlled by git
################################################################################


## setup tools and packages
require(data.table)
library(tidyverse)
taxo_info <- rbind(data.table('taxo' = '207Q00000X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine'),
                   data.table('taxo' = '207QA0000X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Adolescent Medicine'),
                   data.table('taxo' = '207QA0505X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Adult Medicine'),
                   data.table('taxo' = '207QG0300X', 'spec1' = 'FP', 'reference' = 'CMS08',
                              'description' = 'Allopathic & Osteopathic Physicians/Family Medicine, Geriatric Medicine'),
                   data.table('taxo' = '208D00000X', 'spec1' = 'GP', 'reference' = 'CMS01',
                              'description' = 'Allopathic & Osteopathic Physicians/General Practice'),
                   data.table('taxo' = '207R00000X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine'),
                   data.table('taxo' = '207RA0000X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine, Adolescent Medicine'),
                   data.table('taxo' = '207RG0300X', 'spec1' = 'IM', 'reference' = 'CMS11',
                              'description' = 'Allopathic & Osteopathic Physicians/Internal Medicine, Geriatric Medicine'),
                   data.table('taxo' = '208000000X', 'spec1' = 'PedM', 'reference' = 'CMS37',
                              'description' = 'Allopathic & Osteopathic Physicians/Pediatrics'),
                   data.table('taxo' = '2080A0000X', 'spec1' = 'PedM', 'reference' = 'CMS37',
                              'description' = 'Allopathic & Osteopathic Physicians/Pediatrics, Adolescent Medicine'),
                   data.table('taxo' = '363L00000X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner'),
                   data.table('taxo' = '363LA2200X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Adult Health'),
                   data.table('taxo' = '363LC1500X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Community Health'),
                   data.table('taxo' = '363LF0000X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Family'),
                   data.table('taxo' = '363LG0600X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Gerontology'),
                   data.table('taxo' = '363LP0200X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Pediatrics'),
                   data.table('taxo' = '363LP2300X', 'spec1' = 'NP', 'reference' = 'CMS50',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Primary Care'),
                   data.table('taxo' = '364S00000X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist'),
                   data.table('taxo' = '364SA2200X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Adult Health'),
                   data.table('taxo' = '364SC1501X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Community Health/Public Health'),
                   data.table('taxo' = '364SF0001X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Family Health'),
                   data.table('taxo' = '364SG0600X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Gerontology'),
                   data.table('taxo' = '364SP0200X', 'spec1' = 'CCNS', 'reference' = 'CMS89',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Pediatrics'),
                   data.table('taxo' = '363A00000X', 'spec1' = 'PA', 'reference' = 'CMS97',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Physician Assistant'),
                   data.table('taxo' = '363AM0700X', 'spec1' = 'PA', 'reference' = 'CMS97',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Physician Assistant, Medical'),
                   data.table('taxo' = '207V00000X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology'),
                   data.table('taxo' = '207VG0400X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology, Gynecology'),
                   data.table('taxo' = '207VX0000X', 'spec1' = 'OBGYN', 'reference' = 'CMS16',
                              'description' = 'Allopathic & Osteopathic Physicians/Obstetrics & Gynecology, Obstetrics'),
                   data.table('taxo' = '363LX0001X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Obstetrics & Gynecology'),
                   data.table('taxo' = '363LW0102X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Nurse Practitioner, Women’s Health'),
                   data.table('taxo' = '364SW0102X', 'spec1' = 'OBGYN', 'reference' = 'sup',
                              'description' = 'Physician Assistants & Advanced Practice Nursing Providers/Clinical Nurse Specialist, Women’s Health'),
                   NULL # placeholder for comments
)
taxo_info[spec1 %in% c('FP',
                       'GP',
                       'IM',
                       'PedM',
                       'NP',
                       'CCNS',
                       'PA'),
          spec0 := 'PCP'][spec1 %in% c('OBGYN'),
                          spec0 := 'OBGYN']

## read data
nppes0 <- fread('E:/CT_APCD/shared/intermediate_data/APCD_modified/provider/nppes0.csv',
                ## 'E:/CT APCD/Hongfei/files/nppes0.csv',
                ## 'E:/CT APCD/shared/intermediate_data/APCD_modified/nppes0_shrink_12to17.csv',
                header = TRUE,
                colClasses = 'character',
                encoding = 'UTF-8',
                select = c('NPI', 'Taxonomy1'))
provider <- fread("E:/CT_APCD/LDS_12-5-19/ct_provider.txt", # needs update
                  header = TRUE,
                  select = c('PROVIDER_ID', 'ORIG_NPI'),
                  colClasses = 'character',
                  encoding = 'UTF-8') %>% unique(use.key = FALSE)

## target data
provider.new <- nppes0[provider, on = c('NPI' = 'ORIG_NPI')]
provider.new <- taxo_info[provider.new, on = c("taxo" = "Taxonomy1")]

## save the imported data
provider.out.dir <- "E:/CT_APCD/Sai/medicaid/medicaid_files/modified_data/provider/"
fwrite(provider.new,
       paste0(provider.out.dir, 'provider_new.csv'))

# another approach
provider2 <- fread("E:/CT_APCD/LDS_12-5-19/ct_provider.txt", # needs update
                  header = TRUE,
                  select = c('PROVIDER_ID'),
                  colClasses = 'character',
                  encoding = 'UTF-8') %>% unique(use.key = FALSE)
mc <- data.table()
for(yy in 2015:2018){
  tmp <- fread(paste0("E:/CT_APCD/LDS_12-5-19/ct_medical_split_", 
                      yy, ".txt"), colClasses = "character",
               nrow = 100)
  
}