
# change factor values
woe_table <-
  work_data %>%
  mutate(Species = recode(job_17, 
                          A171 = "HIRED_EMPLOYEE",
                          A172 = "OWN_BUSINESS",
                          A173 = "NOT_OFFICIAL",
                          A174 = "PENSIONER"))
