dist_pkgdata_DATA = climaprec.v7d 

if ORSIM

dist_bin_SCRIPTS = libsim_gen_ana_ora.sh libsim_gen_var_ora.sh

dist_pkgdata_DATA += varmap.csv polipciv4.dat estra_temp.dat in.grb

dist_pkgdata_DATA += \
  net_001.simana  net_011.simana  net_021.simana  net_030.simana  net_041.simana \
  net_002.simana  net_012.simana  net_022.simana  net_031.simana  net_042.simana \
  net_003.simana  net_013.simana  net_023.simana  net_032.simana  net_043.simana \
  net_004.simana  net_014.simana  net_024.simana  net_035.simana  net_044.simana \
  net_005.simana  net_015.simana  net_025.simana  net_036.simana  net_045.simana \
  net_006.simana  net_017.simana  net_026.simana  net_037.simana \
  net_007.simana  net_018.simana  net_027.simana  net_038.simana \
  net_009.simana  net_019.simana  net_028.simana  net_039.simana \
  net_010.simana  net_020.simana  net_029.simana  net_040.simana

# Per rigenerare l'anagrafica dal DB nel ramo di sviluppo:
# rm -f *.simana
# make
%.simana:
	./libsim_gen_ana_ora.sh $@

# Decommentare la seguente se l'anagrafica non deve stare in SVN
#CLEANFILES = *.simana

endif

