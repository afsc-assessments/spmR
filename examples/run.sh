test -f data/$1_spcat.dat || echo "Missing file..." data/$1 
test -f data/$1_spcat.dat || exit
echo $1
\cp  data/$1_spcat.dat spp_catch.dat
../src/main -nohess 
test -d $1_out || mkdir $1_out
\mv *.out $1_out
rm eigv.rpt variance admodel.* *.r0? *.p0? fmin.log *.b0? 

	
