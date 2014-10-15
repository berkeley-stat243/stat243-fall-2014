############################################################
### Demo code for Unit 9 of Stat243, "Databases and Big Data"
### Chris Paciorek, October 2014
############################################################

#####################################################
# 2: Databases
#####################################################

### 2.2 Accessing databases in R

## @knitr chunk1
library(RSQLite)

fileName <- "/mirror/data/pub/html/scf/cis.db"
drv <- dbDriver("SQLite")
db <- dbConnect(drv, dbname = fileName) # using a connection once again!
# con <- dbConnect(SQLite(), dbname = fileName) # alternative

# get information on the database schema
dbListTables(db)
dbListFields(db, "articles")
dbListFields(db, "authors")
dbListFields(db, "authorships")


## @knitr chunk2

auth <- dbSendQuery(db, "select * from authorships")
fetch(auth, 5)
dbClearResult(auth)

query <- "select id from authors where name like 'Breiman%'"
a_ids <- dbGetQuery(db, query)


a_ids <- as.list(unlist(a_ids))
query <- paste("select id_title from authorships where author_id in (",
               paste(rep("?", length(a_ids)), collapse = ","), ")")
query
a_ids
t_ids <- dbGetQuery(db, query, a_ids)
t_ids$id_title[1:5]

t_ids <- as.list(unlist(t_ids))
query <- paste("select * from articles where id_title in (",
               paste(rep("?", length(t_ids)), collapse = ","), ")")
titles <- dbGetQuery(db, query, t_ids)
head(titles)
# do a google scholar check to see that things seem to be ok

## @knitr chunk3

# alternatively, we can do a query that involves multiple tables
info <- dbGetQuery(db, "select * from articles, authors, authorships where
   authors.name like 'Breiman%' and authors.id = authorships.author_id and
   authorships.id_title = articles.id_title")
# "select * from articles, authors, authorships where authors.name
#  like 'Breiman%' and authors.id = authorships.author_id and
#  authorships.id_title = articles.id_title"
head(info)

## @knitr chunk4
# that db is read-only; to create a view we need to be able to modify it
system(paste0('cp ', fileName, ' /tmp/.'))
dbDisconnect(db)
db <- dbConnect(drv, dbname = '/tmp/cis.db') 

# finally, we can create a view that amounts to joining the tables
fullAuthorInfo <- dbSendQuery(db, 'create view fullAuthorInfo as select *
     from authors join authorships on authorships.author_id = authors.id')
# 'create view fullAuthorInfo as select * from authors join
#  authorships on authorships.author_id = authors.id'

partialArticleInfo <- dbSendQuery(db, 'create view partialArticleInfo as
     select * from articles join fullAuthorInfo on
     articles.id_title=fullAuthorInfo.id_title')
# 'create view partialArticleInfo as select * from articles join
#  fullAuthorInfo on articles.id_title=fullAuthorInfo.id_title'

fullInfo <- dbSendQuery(db, 'select * from journals join partialArticleInfo
   on journals.id = partialArticleInfo.journal_id')
# 'select * from journals join partialArticleInfo on
#  journals.id = partialArticleInfo.journal_id')
subData <- fetch(fullInfo, 3)
subData
dbClearResult(fullInfo)

## @knitr extra

# demo that cross and inner joins can be the same
tmp <- dbSendQuery(db, 'select * from journals join partialArticleInfo on journals.id = partialArticleInfo.journal_id order by id_entity')
sub1 <- fetch(tmp, 100)
dbClearResult(tmp)
tmp <- dbSendQuery(db, 'select * from journals cross join partialArticleInfo where journals.id = partialArticleInfo.journal_id order by id_entity')
sub2 <- fetch(tmp, 100)
dbClearResult(tmp)
identical(sub1, sub2)

dbDisconnect(db)

## @knitr extra-end

#####################################################
# 3: R and big data
#####################################################

## @knitr airline-prep, engine='bash'

for yr in {1987..2008}; do
 curl http://stat-computing.org/dataexpo/2009/${yr}.csv.bz2 -o /scratch/users/paciorek/243/AirlineData/${yr}.csv.bz2
done

cd /scratch/users/paciorek/243/AirlineData/
cp 1987.csv.bz2 AirlineDataAll.csv.bz2
bunzip2 AirlineDataAll.csv.bz2
for yr in {1988..2008}; do
  bunzip2 ${yr}.csv.bz2 -c | tail -n +2 >> AirlineDataAll.csv
done

# try to determine types and values of fields...
cut -d',' -f11 AirlineDataAll.csv | sort | uniq | less
cut -d',' -f29 AirlineDataAll.csv | sort | uniq | less

cp /scratch/users/paciorek/AirlineDataAll.csv /tmp/.

# create a small test file for testing our code
head -n 10000 AirlineDataAll.csv > test.csv


## @knitr bigmemory-prep, engine='bash'

## I think this script converts all values to numerics
## creates airline.csv
# python AirlineFormatter.py


### 3.1 Working with big datasets in memory: data.table

## @knitr data.table-read

require(data.table)
fileName <- '/tmp/AirlineDataAll.csv'

dt <- fread(fileName, colClasses=c(rep("numeric", 8), "factor",
                            "numeric", "factor", rep("numeric", 5),
                            rep("factor", 2), rep("numeric", 4),
                            "factor", rep("numeric", 6)))
#Read 123534969 rows and 29 (of 29) columns from
#    11.203 GB file in 00:05:16


class(dt)
# [1] "data.table" "data.frame"

## @knitr data.table-subset

system.time(sfo <- subset(dt, Origin == "SFO"))
## 8.8 seconds 
system.time(sfoShort <- subset(dt, Origin == "SFO" & Distance < 1000))
## 12.7 seconds

system.time(setkey(dt, Origin, Distance))
## 33 seconds:
## takes some time, but will speed up later operations
tables()
##     NAME            NROW    MB
##[1,] dt       123,534,969 27334
##[2,] sfo        2,733,910   606
##[3,] sfoShort   1,707,171   379
##     COLS                                                                            
##[1,] Year,Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarr
##[2,] Year,Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarr
##[3,] Year,Month,DayofMonth,DayOfWeek,DepTime,CRSDepTime,ArrTime,CRSArrTime,UniqueCarr
##     KEY            
##[1,] Origin,Distance
##[2,]                
##[3,]                
##Total: 28,319MB

## vector scan
system.time(sfo <- subset(dt, Origin == "SFO"))
## 8.5 seconds
system.time(sfoShort <- subset(dt, Origin == "SFO" & Distance < 1000 ))
## 12.4 seconds

## binary search
system.time(sfo <- dt[.('SFO'), ])
## 0.8 seconds

## @knitr dummy1

### 3.2 Working with big datasets on disk: ff


## @knitr ff

require(ff)
require(ffbase)

# I put the data file on local disk on the machine I am using
# (/tmp on radagast)
# it's good to test with a small subset before
# doing the full operations
fileName <- '/tmp/test.csv'
dat <- read.csv.ffdf(file = fileName, header = TRUE,
     colClasses = c('integer', rep('factor', 3),
     rep('integer', 4), 'factor', 'integer', 'factor',
     rep('integer', 5), 'factor','factor', rep('integer', 4),
     'factor', rep('integer', 6)))


fileName <- '/tmp/AirlineDataAll.csv'
system.time(  dat <- read.csv.ffdf(file = fileName, header = TRUE,
    colClasses = c('integer', rep('factor', 3), rep('integer', 4),
    'factor', 'integer', 'factor', rep('integer', 5), 'factor',
    'factor', rep('integer', 4), 'factor', rep('integer', 6))) )
## takes about 22 minutes

system.time(ffsave(dat, file = '/tmp/AirlineDataAll'))
## takes 11 minutes
## file is saved (in a binary format) as AirlineDataAll.ffData
## with metadata in AirlineDataAll.RData

rm(dat) # pretend we are in a new R session

system.time(ffload('/tmp/AirlineDataAll'))
# this is much quicker:
# 107 seconds

## @knitr tableInfo

# load again as previous chunk not run w/in pdf compilation

ffload('/tmp/AirlineDataAll')
# [1] "tmp/RtmpU5Uw6z/ffdf4e684aecd7c4.ff" "tmp/RtmpU5Uw6z/ffdf4e687fb73a88.ff"
# [3] "tmp/RtmpU5Uw6z/ffdf4e6862b1033f.ff" "tmp/RtmpU5Uw6z/ffdf4e6820053932.ff"
# [5] "tmp/RtmpU5Uw6z/ffdf4e681e7d2235.ff" "tmp/RtmpU5Uw6z/ffdf4e686aa01c8.ff"
# ...

dat$Dest
# ff (closed) integer length=123534969 (123534969) levels: BUR LAS LAX OAK PDX RNO SAN SFO SJC SNA
# ABE ABQ ACV ALB ALO AMA ANC ATL AUS AVP AZO BDL BFL BGR BHM BIL BLI BNA BOI BOS BTV BUF BWI CAE
# CAK CCR CHS CID CLE CLT CMH CMI COS CPR CRP CRW CVG DAB DAL DAY DCA DEN DFW DLH DRO DSM DTW ELP
# EUG EVV EWR FAI FAR FAT FLG FLL FOE FSD GCN GEG GJT GRR GSO GSP GTF HNL HOU HPN HRL HSV IAD IAH
# ICT ILG ILM IND ISP JAN JAX JFK KOA LBB LEX LGA LGB LIH LIT LMT LNK MAF MBS MCI MCO MDT MDW MEM
# MFR MHT MIA MKE MLB MLI MOB MRY MSN MSP MSY OGG OKC OMA ONT ORD ORF PBI PHL PHX PIA PIT PNS PSC
# ...

# let's do some basic tabulation
DestTable <- sort(table.ff(dat$Dest), decreasing = TRUE)
# why do I need to call table.ff() and not table()?

# takes a while

#    ORD     ATL     DFW     LAX     PHX     DEN     DTW     IAH     MSP     SFO

# 6638035 6094186 5745593 4086930 3497764 3335222 2997138 2889971 2765191 2725676

#    STL     EWR     LAS     CLT     LGA     BOS     PHL     PIT     SLC     SEA

#  2720250 2708414 2629198 2553157 2292800 2287186 2162968 2079567 2004414 1983464 

# looks right - the busiest airports are ORD (O'Hare in Chicago) and ATL (Atlanta)

dat$DepDelay[1:50]
#opening ff /tmp/RtmpU5Uw6z/ffdf4e682d8cd893.ff
#  [1] 11 -1 11 -1 19 -2 -2  1 14 -1  5 16 17  1 21  3 13 -1 87 19 31 17 32  0  1
# [26] 29 26 15  5 54  0 25 -2  0 12 14 -1  2  1 16 15 44 20 15  3 21 -1  0  7 23

min.ff(dat$DepDelay, na.rm = TRUE)
# [1] -1410
max.ff(dat$DepDelay, na.rm = TRUE)
# [1] 2601

# tmp <- clone(dat$DepDelay) # make a deep copy

## @knitr dummy2

### 3.2.3 sqldf

## @knitr sqldf
require(sqldf)
# read in file, with temporary database in memory
system.time(sfo <- read.csv.sql(fn,
      sql = "select * from file where Origin = 'SFO'",
      dbname=NULL, header = TRUE))
# read in file, with temporary database on disk
system.time(sfo <- read.csv.sql(fn,
      sql = "select * from file where Origin = 'SFO'",
      dbname=tempfile(), header = TRUE))


## @knitr airline-model

require(ffbase)
require(biglm)

datUse <- subset(dat, DepDelay < 60*12 & DepDelay > (-30) &
                 !is.na(DepDelay))

# any concern about my model?
system.time(mod <- bigglm(DepDelay ~ Distance + DayOfWeek, data = datUse))
# 542.149  11.248 550.779
summary(mod)


## @knitr significance-prep

n <- 150000000  # n*4*8/1e6 Mb of RAM (~5 Gb)
# but turns out to be 11 Gb as a text file
nChunks <- 100
chunkSize <- n/nChunks

set.seed(0)

for(p in 1:nChunks) {
  x1 <- runif(chunkSize)
  x2 <- runif(chunkSize)
  x3 <- runif(chunkSize)
  y <- rnorm(chunkSize, .001*x1, 1)
  write.table(cbind(y,x1,x2,x3), file = '/tmp/signif.csv',
     sep = ',', col.names = FALSE,  row.names = FALSE,
     append = TRUE, quote = FALSE)
}


fileName <- '/tmp/signif.csv'
system.time(  dat <- read.csv.ffdf(file = fileName,
   header = FALSE, colClasses = rep('numeric', 4)))
# 922.213  18.265 951.204 -- timing is on an older machine than radagast

names(dat) <- c('y', 'x1','x2', 'x3')
ffsave(dat, file = '/tmp/signif')

## @knitr significance-model
system.time(ffload('/tmp/signif'))
# 52.323   7.856  60.802  -- timing is on an older machine

system.time(mod <- bigglm(y ~ x1 + x2 + x3, data = dat))
#  1957.358    8.900 1966.644  -- timing is on an older machine

options(digits = 12)
summary(mod)


# R^2 on a subset (why can it be negative?)
coefs <- summary(mod)$mat[,1]
wh <- 1:1000000
1 - sum((dat$y[wh] - coefs[1] + coefs[2]*dat$x1[wh] +
  coefs[3]*dat$x2[wh] + coefs[4]*dat$x3[wh])^2) /
  sum((dat$y[wh] - mean(dat$y[wh]))^2)

## @knitr endchunk

#####################################################
# 4: Sparsity
#####################################################


## @knitr spam
require(spam)
mat = matrix(rnorm(1e8), 1e4)
mat[mat > (-2)] <- 0
sMat <- as.spam(mat)
print(object.size(mat), units = 'Mb')
print(object.size(sMat), units = 'Mb')

vec <- rnorm(1e4)
system.time(mat %*% vec)
system.time(sMat %*% vec)

## @knitr dummy3

#####################################################
# 6: Hadoop, MapReduce, and Spark
#####################################################

### 6.2 MapReduce and RHadoop

## @knitr mr-example

library(rmr)

mymap <- function(k, v) {
   record <- my_readline(v)
   key <- record[['state']]
   value <- record[['income']]
   keyval(key, value)
}

myreduce <- function(k, v){
   keyval(k, c(length(v), mean(v), sd(v)))
}

incomeResults <- mapreduce(
   input = "incomeData",
   map = mymap,
   reduce = myreduce,
   combine = NULL,
   input.format = 'csv',
   output.format = 'csv')

from.dfs(incomeResults, format = 'csv', structured = TRUE)

## @knitr dummy4

### 6.3 Spark

### 6.3.1 Getting set up

## @knitr spark-setup

export SPARK_VERSION=1.1.0 
export CLUSTER_SIZE=12  # number of slave nodes

cd /usr/local/src/pd/spark-${SPARK_VERSION}/spark-${SPARK_VERSION}/ec2

# set Amazon secret keys (manually or in my case by querying them elsewhere)
#AWS_ACCESS_KEY_ID=blah
AWS_ACCESS_KEY_ID=$(grep "^AWS_ACCESS_KEY_ID" ~/.starcluster/config | cut -d' ' -f3)
#AWS_SECRET_ACCESS_KEY=blahhhh
AWS_SECRET_ACCESS_KEY=$(grep "^AWS_SECRET_ACCESS_KEY" ~/.starcluster/config | cut -d' ' -f3)

# start cluster
./spark-ec2 -k ec2star -i ~/.ssh/ec2star.rsa --region=us-west-2 \
 -s ${CLUSTER_SIZE} -w 200 -v ${SPARK_VERSION} launch sparkvm

# login to cluster
# as root
./spark-ec2 -k ec2star -i ~/.ssh/ec2star.rsa --region=us-west-2 \
   login sparkvm

# you can check your nodes via the EC2 management console

# to logon to one of the slaves, look at /root/ephemeral-hdfs/conf/slaves
# and ssh to that address
ssh `head -n 1 /root/ephemeral-hdfs/conf/slaves`

# We can view system status through a web browser interface

# on master node of the EC2 cluster, do:
MASTER_IP=`cat /root/ephemeral-hdfs/conf/masters`
echo ${MASTER_IP}
# Point a browser on your own machine to the result of the next command
# you'll see info about the "Spark Master", i.e., the cluster overall
echo "http://${MASTER_IP}:8080/"
# Point a browser on your own machine to the result of the next command
# you'll see info about the "Spark Stages", i.e., the status of Spark tasks
echo "http://${MASTER_IP}:4040/"
# Point a browser on your own machine to the result of the next command
# you'll see info about the HDFS"
echo "http://${MASTER_IP}:50070/"


# to shutdown cluster - IMPORTANT to avoid extra charges!!!
./spark-ec2 --region=us-west-2 destroy sparkvm

## @knitr spark-hdfs


export PATH=$PATH:/root/ephemeral-hdfs/bin/

hadoop fs -mkdir /data
hadoop fs -mkdir /data/airline

df -h
mkdir /mnt/airline
scp paciorek@saruman.berkeley.edu:/scratch/users/paciorek/243/AirlineData/*bz2 /mnt/airline
hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline

# check files on the HDFS, e.g.:
hadoop fs -ls /data/airline

# pyspark is in /root/spark/bin
export PATH=${PATH}:/root/spark/bin
# start Spark's Python interface as interactive session
pyspark

## @knitr spark-data

from operator import add

lines = sc.textFile('/data/airline').cache()
numLines = lines.count()

# count flights by departure airport
lines = sc.textFile('/data/airline')

# mapper
def stratify(line):
    vals = line.split(',')
    return(vals[16], 1)

result = lines.map(stratify).reduceByKey(add).collect()
# reducer is simply the addition function

# this counting by key could have been done
# more easily using countByKey()

vals = [x[1] for x in result]
sum(vals)  # check this matches numLines
[x[1] for x in result if x[0] == "SFO"]  # SFO result

# if don't collect, can grab result one by one
output = lines.map(stratify).reduceByKey(add)
output.take(1)

# also, you can have interim results stored as objects
mapped = lines.map(stratify)
result = mapped.reduceByKey(add).collect()


lines.filter(lambda line: "SFO" in line.split(',')[16]).saveAsTextFile('/data/airline/SFO')

## make sure it's all in one chunk for easier manipulation on master
lines.filter(lambda line: "SFO" in line.split(',')[16]).repartition(1).saveAsTextFile('/data/airline/SFO2')

## @knitr spark-nonstandard

def computeKeyValue(line):
    vals = line.split(',')
    # key is carrier-month-origin-destination
    keyVals = '-'.join([vals[x] for x in [8,1,16,17]])
    if vals[0] == 'Year':
        return('0', [0,0,1,1])
    cnt1 = 1
    cnt2 = 1
    # 14 and 15 are arrival and departure delays
    if vals[14] == 'NA':
        vals[14] = '0'
        cnt1 = 0
    if vals[15] == 'NA':
        vals[15] = '0'
        cnt2 = 0
    return(keyVals, [int(vals[14]), int(vals[15]), cnt1, cnt2])


def medianFun(input):
    import numpy as np
    if len(input) == 2:
        if len(input[1]) > 0:
            m1 = np.median([val[0] for val in input[1] if val[2] == 1])
            m2 = np.median([val[1] for val in input[1] if val[3] == 1])
            return((input[0], m1, m2)) # m1, m2))
        else:
            return((input[0], -999, -999))
    else:
        return((input[0], -9999, -9999))


output = lines.map(computeKeyValue).groupByKey().cache()
medianResults = output.map(medianFun).collect()

## @knitr spark-fit1

lines = sc.textFile('/data/airline').cache()

def screen(vals):
    vals = vals.split(',')
    return(vals[0] != 'Year' and vals[14] != 'NA' and vals[18] != 'NA' and vals[3] != 'NA'
           and float(vals[14]) < 720 and float(vals[14]) > (-30) )

lines = lines.filter(screen).repartition(96).cache()

import numpy as np
from operator import add

P = 8
bc = sc.broadcast(P)

#######################
# calc xtx and xty
#######################
def crossprod(line):
    vals = line.split(',')
    y = float(vals[14])
    dist = float(vals[18])
    dayOfWeek = int(vals[3])
    xVec = np.array([0.0] * P)
    xVec[0] = 1.0
    xVec[1] = float(dist)/1000
    if dayOfWeek > 1:
        xVec[dayOfWeek] = 1.0
    xtx = np.outer(xVec, xVec)
    xty = xVec * y
    return(np.c_[xtx, xty])

summs = lines.map(crossprod).reduce(add)
# 6 mins w/ 12 partitions
# 

# now just solve system of linear equations!!

#######################
# calc xtx and xty w/ mapPartitions
#######################

# dealing with x matrix via mapPartitions

def readPointBatch(iterator):
    strs = list(iterator)
    matrix = np.zeros((len(strs), P+1))
    for i in xrange(len(strs)):
        vals = strs[i].split(',')
        dist = float(vals[18])
        dayOfWeek = int(vals[3])
        xVec = np.array([0.0] * (P+1))
        xVec[8] = float(vals[14]) # y
        xVec[0] = 1.0  # int
        xVec[1] = float(dist) / 1000
        if(dayOfWeek > 1):
            xVec[dayOfWeek] = 1.0
        matrix[i] = xVec
    return([matrix.T.dot(matrix)])

def myAdd(l1, l2):
    return(l1 + l2)

xtxy = lines.mapPartitions(readPointBatch).reduce(myAdd)


## @knitr spark-fit2

def readPointPartition(iterator):
    strs = list(iterator)
    matrix = np.zeros((len(strs), P+1))
    print(len(strs))
    for i in xrange(len(strs)):
        vals = strs[i].split(',')
        dist = float(vals[18])
        dayOfWeek = int(vals[3])
        xVec = np.array([0.0] * (P+1))
        xVec[8] = float(vals[14]) # y
        xVec[0] = 1.0  # int
        xVec[1] = float(dist) / 1000
        if(dayOfWeek < 7):
            xVec[dayOfWeek+1] = 1.0
        matrix[i] = xVec
    return([matrix])

batches = lines.mapPartitions(readPointPartition).cache()

def denomSumSqPartition(mat):
    return((mat*mat).sum(axis=0))

def getNumPartition(mat):
    beta[p] = 0
    sumXb = mat[:, 0:P].dot(beta)
    return(sum((mat[:,P] - sumXb)*mat[:,p]))

sumx2 = batches.map(denomSumSqPartition).reduce(add)

beta = np.array([0.0] * P)
p = 0

oldBeta = beta.copy() # otherwise a shallow (i.e., pointer) copy!

it = 0

tol = .001
maxIts = 10

while crit > tol and its < maxIts:
#for it in range(1,6):
    for p in xrange(P):
        # distribute current beta and current coordinate
        bc = sc.broadcast(beta)
        bc = sc.broadcast(p)
        # get numerator as product of residual and X for coordinate
        sumNum = batches.map(getNumPartition).reduce(add)
        beta[p] = sumNum / sumx2[p]   
        print("Updated var " + str(p) + " in iteration ", str(it), ".")
    crit = sum(abs(beta - oldBeta))
    oldBeta = beta.copy()  
    print("-"*100)
    print(beta)
    print(crit)
    print("-"*100)
    it = it+1

## @knitr pyspark-script

import sys
from pyspark import SparkContext
if __name__ == "__main__":
    sc = SparkContext()
    # use sys.argv to get arguments
    # for example:
    total_samples = int(sys.argv[1]) if len(sys.argv) > 1 else 1000000
    num_slices = int(sys.argv[2]) if len(sys.argv) > 2 else 2
    samples_per_slice = round(total_samples / num_slices)
    def sample(p):
        rand.seed(p)
        x, y = rand.random(samples_per_slice), rand.random(samples_per_slice)
        return sum(x*x + y*y < 1)

    count = sc.parallelize(xrange(0, num_slices), num_slices).map(sample).reduce(lambda a, b: a + b)
    print "Pi is roughly %f" % (4.0 * count / (num_slices*samples_per_slice))

