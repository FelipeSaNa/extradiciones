# Author: FSN
# Maintainers: FSN
# Copyright:   2021, FSN GPL v2 or later
# Data Import
# =============================================

#loading packages
pacman::p_load(tidyverse, here, janitor, readxl, writexl, forcats, lubridate, hrbrthemes)

#importing rechazos databases with clean names
extradiciones = read_xlsx(here("data", "raw", "extradiciones_mex_eua.xlsx")) %>%
    clean_names() %>%
    mutate(delito = as.character(delito))
#END