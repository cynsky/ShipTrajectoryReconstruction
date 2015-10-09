
#----------------some words for this project---------------------
# this project will first use the ais data from chunxun.com.
# the main purpose is to check the quality of the their data
# and compare the data quality with the data from bomao.com
# in addition, an demo application to reconstruct uncertain
# ship trajectories will be developed based on this data from
# scratch.

# the input should be a large dataset of uncertain trajectories T.uncertain
# T includes points from different ships(mmsis) ordered by time.
# individual point of T include the following columns: mmsi,time,status,lon,lat,sog
# then,T will be grided into trajectories of grids. here grids 
# would be taken in several levels: level0 has the same scale with lon,lat
# level1: 10*lon,level2:100*lon,level3:1000*lon. 
# the output is a set of reconstructed trajectories T.complete
# 
# the main processes should include following steps:
# 1. prepare the data: 
# 1.1 T=get.raw.trajs(dirpath,mmsis,lon1,lat1,lon2,lat2)
# 1.2 T.point
# 1.3 T.line
# 1.4 T.grid
# 2. identify ports or other stay area based on density-based-cluster
# 3. break trajectories base on clusters
# 4. get points of each port pairs with dirction
# 5. for traj in each port pairs:
# 6. construct regions 
# 7. construct edges
# 8. get the whole graph
# 9. interpote uncertain trajectory at level1,continue grids
# 10.refine the trajectories
# 11.evaluate performance
# 12.calculate emissions 
# 13.actuall,i can calculate a raw emission without do any thing with the raw data
# 14.in other words, we assume that all of the trajectory are perfect.
# 15.this can publish a pretty good article which are probable be indexed by IE



















# 