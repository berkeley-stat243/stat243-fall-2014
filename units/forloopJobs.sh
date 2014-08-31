# example of bash for loop for starting jobs
# usage: ./forloopJobs.sh
# Author: Chris Paciorek
# Date: July 28, 2011


n=100 # if I want to be able to vary n from outside the R program
for(( it=1; it<=100; it++));
do
    echo "n=$n; it=$it; source('base.R')" > tmp-$n-$it.R
    R CMD BATCH --no-save tmp-$n-$it.R sim-n$n-it$it.Rout
done
# note that base.q should NOT set either 'n' or 'it'