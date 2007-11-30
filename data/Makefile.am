if ORSIM

dist_bin_SCRIPTS = libsim_gen_ana_ora.sh

dist_pkgdata_DATA = varmap.csv polipciv4.dat estra_temp.dat

dist_pkgdata_DATA += net_001.simana net_002.simana net_010.simana \
 net_011.simana net_012.simana net_013.simana net_015.simana \
 net_017.simana net_018.simana net_019.simana net_020.simana net_021.simana \
 net_022.simana 


# Per rigenerare l'anagrafica dal DB nel ramo di sviluppo:
# rm -f *.simana
# make
%.simana:
	./libsim_gen_ana_ora.sh $@

# Decommentare la seguente se l'anagrafica non deve stare in SVN
#CLEANFILES = *.simana

endif

