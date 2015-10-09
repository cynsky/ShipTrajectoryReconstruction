
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