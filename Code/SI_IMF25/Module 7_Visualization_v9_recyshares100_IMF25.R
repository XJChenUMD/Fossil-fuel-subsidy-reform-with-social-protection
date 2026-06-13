# Module 7 SI_IMF25 wrapper
# Calls the STANDALONE IMF25-specific Module 7 directly.
# No path redirection needed -- the standalone file hardcodes
# pathout2_imf25 and pathout5_imf25 for all load/save/ggsave calls.
source(str_c(pathcode_imf25, "/Module 7_Visualization_v9_recyshares100_IMF25_standalone.R"))
