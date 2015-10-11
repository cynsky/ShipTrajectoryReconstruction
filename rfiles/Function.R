
getships<-function(shipfile){
  
  #ships=fread('D:/Rprojects/ships/ships.txt',sep=',') # build year should be include
  ships=fread(shipfile,sep=',')
  ships=ships[mmsi!='null']
  # ships$mmsi<-as.character(ships$mmsi)
  setkey(ships,mmsi)
  return (ships)
  
}
# column names of chuanxun data:
# unique_ID;acquisition_time;target_type;data_source;status;longitude;latitude;
# area_ID;speed;conversion;cog;true_head;power;ext;extend



get.raw.traj<-function(dir,lon1,lat1,lon2,lat2){
  
  
  t=data.table(mmsi=0,time=0,status=0,sog=0,lon=0,lat=0)[mmsi==-1]
  filenames=list.files(dir)
  
  for (filename in filenames){
    
    path.in=paste(dir,'/',filename,sep='')
    print(Sys.time())
    tempdt=fread(path.in)
    # depend on the structure of the data
    tempdt=tempdt[,list(mmsi=unique_ID,time=acquisition_time,status,sog=speed,lon=longitude,lat=latitude)]
    setkey(tempdt,lon,lat)
    tempdt=tempdt[lon<=lon2&lon>=lon1&lat>=lat1&lat<=lat2,list(mmsi,time,status,sog,lon,lat)]
    t=rbind(t,tempdt)
    
  }
  t=t[,sog:=round(sog*3600/1000/1852*10)]
  return(t)
}

#get clusters on different levelwhich is controlled by 
#e:the distance of the density circle
#MinPts:min points required at each circle when the center point is within the cluster

getclusters<-function(stops,e,MinPts){
  
  model <- dbscan(as.matrix(stops[,list(lon,lat)]),eps=e,minPts=MinPts)  # 50000 for archor places5
  stops=cbind(stops,cluster=model$cluster)
  return(stops)
  
}

drawClst<-function(dt){
  library(ggplot2)
  p <-ggplot(dt,aes(x=lon,y=lat))
  # p=getMap(x[cluster>0],5)
  p=p+geom_point(data=dt,size=1, aes(colour=factor(cluster)),shape=3)
  p=p+geom_text(data=dt[,.SD[1],by=cluster],size=3,col='black',aes(x=lon+0.05,y=lat+0.05,label=cluster))
  p
}
#break the trajectores with start and end clusters
#the trip does not include points of start and end clusters
#this function may be changed to set two or three level original and destination trips

get.trips<-function(dt){
  
  dt[,cluster.start:=0];dt[,cluster.end:=0];dt[,trip:=0]
  dt$cluster.start=as.integer(dt$cluster.start);dt$cluster.end=as.integer(dt$cluster.end)
  #get mmsis with 2 points at least
  mmsis=unique(dt[,.N,by=mmsi][N>=2]$mmsi);mmsis.len=length(mmsis);
  x=dt[mmsi==0]
  print(mmsis.len)
  print(Sys.time())
  for (i in(1: mmsis.len)){
    if(i%%100==0){
      print(i)
    }
    aship=dt[mmsi==mmsis[i]];setkey(aship,time);len=nrow(aship);
    tmp1=aship[1:(len-1)]$cluster
    tmp2=aship[2:len]$cluster
    tmp=as.data.table(tmp2-tmp1)
    idx.n=tmp[V1<0,which=TRUE]
    idx.p=tmp[V1>0,which=TRUE]
    idx=as.data.table(c(1,len,idx.n,idx.p))
    idx=unique(idx)
    setkey(idx,V1)
    l=nrow(idx)
    
    for (j in (1:(l-1))){
      
      #first row in index.
      if(j==1){
        #idx[1] means the first number(the index of dt) in idx
        range=idx[j]$V1:idx[j+1]$V1
        sc=aship[idx[j]$V1]$cluster
        aship[range,trip:=j]
      }else{
        range=(idx[j]$V1+1):(idx[j+1]$V1)
        sc=aship[idx[j]$V1]$cluster
        
      }
      aship[range,trip:=j]
      
      #last row with different cluster
      if(j==(l-1)){
        ec=aship[idx[j+1]$V1]$cluster
      }else{
        ec=aship[idx[j+1]$V1+1]$cluster
      }
      
      aship[range,cluster.start:=sc]
      aship[range,cluster.end:=ec]
      #for cluster>0 its start and end cluster is the same one
      #before is mainly for cluster==0
      aship[cluster>0,cluster.start:=cluster]
      aship[cluster>0,cluster.end:=cluster]
      
    }
    
    x=rbind(x,aship)
  }
  setkey(x,mmsi,time)
  return(x)
  print(Sys.time())
}

plot.route<-function(dt){
  dev.new()
  #p<-getMap(dt,8)
  p=ggplot()
  p=p+geom_point(data=dt,alpha=0.5,aes(x=lon,y=lat,color=factor(paste(mmsi,trip))))
  p=p+geom_path(data=dt,alpha=0.5,aes(x=lon,y=lat,color=factor(paste(mmsi,trip))))
  p
  
}

getMap<-function(dt,zoomsize){
  library('ggmap')
  lon=dt$lon
  lat=dt$lat
  centerX=0.5*(max(lon)+min(lon))
  centerY=0.5*(max(lat)+min(lat))
  p<-ggmap(get_map(location=c(centerX,centerY),zoom=zoomsize,source='google'))
  return(p)

}
#number of columns of the plot
plot.each.trip<-function(dt,columns){
  dev.new()
  trips=dt[,.N,by=list(mmsi,trip)]

  plist=vector("list",nrow(trips))
  for (i in 1:nrow(trips)){
    
    plist[[i]]=dtplot(dt[mmsi==trips[i]$mmsi&trip==trips[i]$trip])
    
  }
  multiplot(plotlist=plist,cols=columns)

}


dtplot<-function(dt){
  library('ggplot2')
  p=ggplot(data=dt,aes(x=lon,y=lat))
  p<-p+geom_point(data=dt,aes(x=lon,y=lat),size=1,color="red",alpha=0.7)
  p<-p+geom_path(data=dt,aes(x=lon,y=lat),size=0.5,color="green",alpha=0.7)
  return(p) 
}

#---------------------multiplot--------------------------------------

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot.trip.speed<-function(dt){
  
  dev.new()
  dt=dt[,tt:=time-.SD[1]$time,by=list(mmsi,trip)]
  
  p<-ggplot(data=dt,aes(x=tt,y=sog))
  p=p+geom_point(aes(color=factor(paste(mmsi,trip))),alpha=0.5)
  p=p+geom_path(aes(color=factor(paste(mmsi,trip))),alpha=0.5)
  
  p
  
  
}
#plot the grid the trips have passed by
#the parameter scale is for the level of grids
#for example 1 for 0.1*0.1 grids,2 for 0.01*0.01 grids

plot.trip.grid<-function(dt,scale){
  
  factor=10^scale
  grd=dt
  grd=grd[,g.lon:=floor(lon*factor)]
  grd=grd[,g.lat:=floor(lat*factor)]
  grd=grd[,.N,list(g.lon,g.lat)]
  grd=grd[,gid:=(1:nrow(grd))]
  setkey(grd,gid)
  #get real position of the four cornor points fo each grid
  grd=grd[,g.lon1:=g.lon/factor]
  grd=grd[,g.lon2:=g.lon/factor]
  grd=grd[,g.lon3:=(g.lon+1)/factor]
  grd=grd[,g.lon4:=(g.lon+1)/factor]
  
  grd=grd[,g.lat1:=g.lat/factor]
  grd=grd[,g.lat2:=(g.lat+1)/factor]
  grd=grd[,g.lat3:=(g.lat+1)/factor]
  grd=grd[,g.lat4:=g.lat/factor]
  
  temp=grd[gid==0,list(g.lon,g.lat,N,gid)]
  setnames(temp,c('x','y','N','gid'))
  # each grid points should group into its gid
  for (i in (1:nrow(grd))){
    
    x=matrix(grd[i,list(g.lon1,g.lon2,g.lon3,g.lon4)])
    y=matrix(grd[i,list(g.lat1,g.lat2,g.lat3,g.lat4)])
    gid=rep(grd[i]$gid,4)
    N=rep(grd[i]$N,4)
    
    tmp=data.table(cbind(x,y,gid,N))
    setnames(tmp,c('x','y','gid','N'))
    temp=rbind(temp,tmp)
    
  }
  
  tmp=data.table(cbind(x=unlist(temp$x),y=unlist(temp$y),gid=unlist(temp$gid),N=unlist(temp$N)))
  
  dev.new()
  p <- ggplot(tmp, aes(x=x, y=y)) 
  p=p+geom_polygon(aes(fill=N, group=gid,color=factor(gid)))
  #p=p+geom_text(data=grd,aes(x=g.lon1+0.005,y=g.lat1+0.005,label=gid),size=3)
  p
  
}


get.full.grids<-function(dt,scale){
  
  factor=10^scale
  grd=t.route[,list(lon,lat)]
  grd=grd[,levellon:=floor(lon*factor)]
  grd=grd[,levellat:=floor(lat*factor)]
  
  maxlon=max(grd$levellon)
  minlon=min(grd$levellon)
  maxlat=min(grd$levellat)
  minlat=max(grd$levellat)
  
  lons=data.table(lon=minlon:maxlon)
  lats=data.table(lat=minlat:maxlat)
  lons2=lons[,list(rep(lon,nrow(lats))),by=lon];lons2$V1=NULL;head(lons2);dim(lons2)
  lats2=lats[,list(lat=rep(lat,nrow(lons)))];head(lats2);dim(lats2)
  temp=cbind(lons2,lats2)
  
  setkey(temp,lon)
  return(temp)
}

get.full.edges<-function(dt){
  
  #lon,lat is the position of grids
  
  temp=dt[,list(lon,lat)]
  temp.idx=cbind(temp,gid=(1:nrow(temp)))
  
  lonmax=max(temp$lon)
  lonmin=min(temp$lon)
  latmax=max(temp$lat)
  latmin=min(temp$lat)
  
  #first,repeat each row or grids four times
  
  temp=temp[,list(rep(lat,4),id=seq(1:4)),by=list(lon,lat)]
  temp$V1=NULL
  # add another point of the each ages
  # only the edge from the point to its neighbor are produced.
  # all points will get a double direct network finally
  temp=temp[id==1,lon2:=lon]
  temp=temp[id==1,lat2:=as.integer(lat+1)]
  
  temp=temp[id==2,lon2:=lon]
  temp=temp[id==2,lat2:=as.integer(lat-1)]
  
  temp=temp[id==3,lat2:=lat]
  temp=temp[id==3,lon2:=as.integer(lon+1)]
  
  temp=temp[id==4,lat2:=lat]
  temp=temp[id==4,lon2:=as.integer(lon-1)]
  
  #remove the grids which out the boundary
  temp=temp[lon2<=lonmax&lon2>=lonmin&lat2<=latmax&lat2>=latmin]
  temp$id=NULL
  setnames(temp,c('lon1','lat1','lon2','lat2'))
  setnames(temp.idx,c('lon1','lat1','gid1'))
  temp1=inner_join(temp,temp.idx,by=c('lon1','lat1'))
  setnames(temp.idx,c('lon2','lat2','gid2'))
  temp2=inner_join(temp1,temp.idx,by=(c('lon2','lat2')))
  return(temp2)

}


