# RA_Task_GBL
# Data Associate Test Solution in R
# This script is designed to be simple, efficient, and easy to follow.

#==============================================================================
# S E T U P
#==============================================================================

# -- 0. Set the correct working directory -- #
# This is the corrected path, pointing one level up.
setwd("C:/Users/Admin/Downloads/RA Interview and Data Tests resources/DA_Test_Set_A")


# -- 1. Install and load necessary libraries -- #
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, haven, lubridate, fixest, modelsummary, readr)


#==============================================================================
# S E C T I O N  1 :  D A T A  W R A N G L I N G
#==============================================================================

# -- 2. Process Attendance Data (EAS) -- #
# Read the asterisk-separated file.
attendance_summary <- read_delim(
    "eas_oct_2016_cleaned.txt",
    delim = "*",
    col_types = cols(.default = "c")
  ) %>%
  rename(factory_unit = UNIT, worker_id = TKN_NO, line = BATCH, sub_department = SUBDEPT) %>%
  filter(factory_unit == "1", sub_department == "SEWING") %>%
  distinct(worker_id, .keep_all = TRUE) %>%
  pivot_longer(
    cols = matches("A\\d{2}_STATUS"),
    names_to = "day",
    values_to = "status"
  ) %>%
  mutate(
    present = if_else(status == "P", 1, 0, missing = 0),
    day = as.integer(str_extract(day, "\\d{2}"))
  ) %>%
  group_by(factory_unit, line, day) %>%
  summarise(attendance_rate = mean(present, na.rm = TRUE), .groups = "drop")

# -- 3. Process Production Data (Sipmon) -- #
production_summary <- read_csv("SipmonOct16Feb17.xlsx - Sheet1.csv") %>%
  rename(factory_unit = `UNIT_CODE`, line = `LINE_NUMBER`, date = `SCHEDULE_DATE`) %>%
  mutate(date = dmy(date)) %>%
  filter(month(date) == 10, year(date) == 2016, factory_unit == "UNIT-1") %>%
  mutate(day = day(date)) %>%
  group_by(factory_unit, line, day) %>%
  summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)), .groups = "drop")

# -- 4. Merge and Save Final Dataset -- #
line_day_data <- left_join(attendance_summary, production_summary, by = c("factory_unit", "line", "day"))
write_dta(line_day_data, "line_day_production_oct2016.dta")

print("--- Section 1: Summary of Merged Data ---")
summary(line_day_data)

#==============================================================================
# S E C T I O N  2 :  D I F F E R E N C E - I N - D I F F E R E N C E S
#==============================================================================

# -- 5. Load and Prepare DiD Data -- #
did_data <- read_csv("read_did_data.csv") %>%
  mutate(
    post_treat = if_else(time >= training_end, 1, 0, missing = 0),
    time_to_treat = time - training_end
  )

# -- 6. Run DiD Regression -- #
did_model <- feols(
  production ~ trained * post_treat | factory + time,
  data = did_data,
  cluster = ~factory
)
print("--- Section 2: DiD Regression Results ---")
modelsummary(did_model, stars = TRUE, title = "Effect of Training on Production")

# -- 7. Create and Save an Event Study Plot -- #
event_study_model <- feols(
  production ~ i(time_to_treat, trained, ref = -1) | factory + time,
  data = did_data,
  cluster = ~factory
)
did_plot <- coefplot(event_study_model, keep = "time_to_treat::") +
  labs(
    title = "Event Study: Effect of Training on Production",
    x = "Time Relative to Treatment Start (Months)",
    y = "Change in Production"
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal()

ggsave("did_es_plot.png", plot = did_plot, width = 8, height = 5)

print("--- Script Finished ---")
