library(data.table)
library(dplyr)
library(dbscan)
library(igraph)
#----------prepare data-------------------------
shipfile='D:/share/Rprojects/inputs/ships.txt';
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi);
container.mmsis=ships[type_en=='Container',list(mmsi=as.numeric(mmsi))]
dir='D:/share/AIS/AIS_chuanxun_201409/csvdata'
t=get.raw.traj(dir,108,18,126,42)
t.container=data.table(inner_join(t,container.mmsis,by='mmsi'));head(t.container);setkey(t.container,mmsi,time)
t.container=t.container[sog<250];
t.points=t.container[,pid:=1:nrow(t.container)]
t.grids=t.points[,level3lon:=floor(lon*1000)]
t.grids=t.grids[,level3lat:=floor(lat*1000)];setkey(t.grids,level3lon,level3lat)
t.grids=t.points[,level2lon:=floor(lon*100)]
t.grids=t.grids[,level2lat:=floor(lat*100)];setkey(t.grids,level2lon,level2lat)
t.grids=t.points[,level1lon:=floor(lon*10)]
t.grids=t.grids[,level1lat:=floor(lat*10)];setkey(t.grids,level1lon,level1lat)
#mean is much faster than median

#-----------get clusters for each points---------
#input trajectories and out put trajectories with cluster number
t.grids.level3=t.grids[sog<10,list(lon=median(lon),lat=median(lat)),by=list(level3lon,level3lat)]
dim(t.grids.level3)
t.cluster=getclusters(t.grids.level3,0.25,20);t.cluster[,.N,by=cluster]
#each point with a cluster, cluster of points not in t.cluster is set to 0 as well.
#now the class is at a quite high level.
t.grids.cluster=left_join(t.grids,t.cluster[cluster>0,list(cluster),by=c('level3lon','level3lat')])
t.grids.cluster=t.grids.cluster[is.na(cluster),cluster:=0];setkey(t.grids.cluster,mmsi,time)
drawClst(t.cluster[cluster>0])
write.csv(t.cluster,file='D:/share/AIS/AIS_chuanxun_201409/containers/cluster3.csv',sep=',')

#------------beark trajectories of each ship with an start and end cluster---------------
# pay attention to some segments which may be on the way to an port 
# the start and end cluster is not include in the trip

t.trips=get.trips(t.grids.cluster)

#------------plot original-destination network---------------------
# only for non-zero start and end clusters

t.network=t.trips[cluster.start>0&cluster.end>0&cluster.start!=cluster.end,.N,by=list(cluster.start,cluster.end,trip)][,.N,by=list(cluster.start,cluster.end)]
g <- graph.data.frame(t.network[,list(cluster.start,cluster.end)], directed=TRUE)
tkplot(g,layout=layout_with_fr, vertex.size=4,vertex.label.dist=1, vertex.color="red", edge.arrow.size=1,edge.label=t.network$N)

#------------construct route between ports(clusters)--------------------
# select trips from gaoxiong to xiamen as an example.
t.route=t.trips[cluster.start==14&cluster.end==5];dim(t.route);head(t.route)
write.csv(t.route,file='D:/share/AIS/AIS_chuanxun_201409/containers/GaoXiongToXiaMen.csv',sep=',')
#end stand the trips
plot.route(t.route)
plot.each.trip(dt=t.route,columns = 5)
trip.timespan=t.route[,list(timespan=max(.SD$time)-min(.SD$time)),by=list(mmsi,trip)]
plot.trip.grid(t.route,1)
plot.trip.grid(t.route,2)

#----------construct trajectories------------------------
#get the boundary of t.route

grids.full=get.full.grids(t.route,2)
edges=get.full.edges(grids.full)
g2 <- graph.data.frame(edges[,list(gid1,gid2)], directed=TRUE)


