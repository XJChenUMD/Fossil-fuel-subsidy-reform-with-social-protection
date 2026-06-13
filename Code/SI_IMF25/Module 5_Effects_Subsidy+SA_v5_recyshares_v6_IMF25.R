# Module 5 SI_IMF25 wrapper
#
# Module 5 line 12 loads: load(str_c(pathout2, "/Subsidy scenarios_price and CO2 response_v5.Rdata"))
# For IMF25 run, we need pathout2 to point to pathout2_imf25 AND the filename has _IMF25 suffix.
# Strategy: temporarily override pathout2 AND the filename via RDATA_M3_OVERRIDE mechanism.
#
# The simplest robust fix: copy/symlink is not portable.
# Instead: temporarily set pathout2 <- pathout2_imf25 so Module 5's load() finds the right folder.
# Then restore pathout2 after Module 5 runs.
# The filename difference (_IMF25 vs _v5): handle via RDATA_SUFFIX_OVERRIDE.

# Save original pathout2 (currently pointing to main run folder)
pathout2_main_backup <- pathout2

# Redirect pathout2 to IMF25 folder for Module 5's load() call
pathout2 <- pathout2_imf25

# We also need Module 5's load() to find the _IMF25 filename.
# Since the filename is hardcoded in Module 5 as "_v5.Rdata", create a
# temporary symlink-equivalent by saving under both names:
# (Already saved as _IMF25.Rdata by Module 3; save a copy under _v5.Rdata name too)
if (file.exists(str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5_IMF25.Rdata"))) {
  file.copy(
    str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5_IMF25.Rdata"),
    str_c(pathout2_imf25, "/Subsidy scenarios_price and CO2 response_v5.Rdata"),
    overwrite = TRUE
  )
}

# Also redirect pathout3 -> pathout3_imf25 for Module 5's save() calls
pathout3_main_backup <- pathout3
pathout3 <- pathout3_imf25

source(str_c(pathcode, "/Module 5_Effects_Subsidy+SA_v5_recyshares_v6.R"))

# Restore
pathout2 <- pathout2_main_backup
pathout3 <- pathout3_main_backup
