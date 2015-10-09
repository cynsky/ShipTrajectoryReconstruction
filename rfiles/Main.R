library(data.table)
#----------prepare data-------------------------
shipfile='D:/share/Rprojects/inputs/ships.txt';
ships=getships(shipfile);dim(ships);head(ships);setkey(ships,mmsi);
container.mmsis=ships[type_en=='Container',list(mmsi=as.numeric(mmsi))]
dir='D:/share/AIS/AIS_chuanxun_201409/csvdata'
t=get.raw.traj(dir,108,18,126,42)
t.container=data.table(inner_join(t,container.mmsis,by='mmsi'));head(t.container);setkey(t.container,mmsi,time)
#-----------get clusters------------------------
