
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
  p=p+geom_point(data=dt,aes(x=lon,y=lat,color=factor(trip)))
  p=p+geom_path(data=dt,aes(x=lon,y=lat,color=factor(trip)))
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