# Module 7 wrapper for the SI_IMF25 robustness run.
# Calls the standalone IMF25-specific Module 7 directly; no path redirection is
# needed because the standalone file uses pathout2_imf25 and pathout5_imf25
# for all load/save/ggsave calls.
source(str_c(pathcode_imf25, "/Module 7_Visualization_v9_recyshares100_IMF25_standalone.R"))
