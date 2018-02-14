# Author: Alexander Kowarik
# Read the test data from estonian smart meters and there aux. information
# 1000 business and 1000 households
library(data.table);library(lubridate);
datapath <- "X:/Projektid/AUSTRIA_ELERING/Alexander_Kowarikule"
list.files(datapath)

dl <- fread(file.path(datapath,"data_legal_at.csv"))
pl <- fread(file.path(datapath,"points_legal_at.csv"))

dp <- fread(file.path(datapath,"data_private_at.csv"))
pp <- fread(file.path(datapath,"points_private_at.csv"))

colnames(dp) <- colnames(dl)<- c("ID","time","value")
colnames(pl) <- c("turnover","emp","nace","reg","ID")
colnames(pp) <- c("type","rooms","const_year","size_m2","hh_size","urban","ID")

