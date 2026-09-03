# Module 5 wrapper for the SI_IMF25 robustness run.
# Module 5 reads "Subsidy scenarios_price and CO2 response_v5.Rdata" from pathout2
# and writes its outputs to pathout3, so this wrapper temporarily redirects both
# paths to the SI_IMF25 folders, runs Module 5 unchanged, and then restores them.

# Save the original paths (currently pointing to the main-run folders)
pathout2_main_backup <- pathout2

# Redirect pathout2 to the IMF25 folder for Module 5's load() call
pathout2 <- pathout2_imf25

# Module 5 expects the "_v5.Rdata" filename, while Module 3 (IMF25) saved
# "_v5_IMF25.Rdata"; keep a copy under the expected name as well.
if (file.exists(str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5_IMF25.Rdata"))) {
  file.copy(
    str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5_IMF25.Rdata"),
    str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5.Rdata"),
    overwrite = TRUE
  )
}

# Redirect pathout3 to the IMF25 folder for Module 5's save() calls
pathout3_main_backup <- pathout3
pathout3 <- pathout3_imf25

source(str_c(pathcode, "/Module 5_Effects_Subsidy+SA_v5_recyshares_v6.R"))

# Restore the main-run paths
pathout2 <- pathout2_main_backup
pathout3 <- pathout3_main_backup
