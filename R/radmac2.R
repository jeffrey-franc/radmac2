los<-function(x,...){
    UseMethod('los');
}



los<-function(protocol,sim,db='medstat1_surgesim'){

# This Sim
    sqlstriage<-"SELECT RegTime-Time_Runtime AS triage FROM runtimes where RegTime-Time_Runtime < 3600 and RegTime IS NOT NULL";
    striage_df <- mydf(sql=sqlstriage,db=sim);
    triage<-median(striage_df$triage);

     sqlsroom<-"SELECT RoomTime-Time_Runtime AS room FROM runtimes where RoomTime-Time_Runtime < 3600 and RoomTime IS NOT NULL";
    sroom_df <- mydf(sql=sqlsroom,db=sim);
    room<-median(sroom_df$room);

sqlsmd<-"SELECT MDTime-Time_Runtime AS md FROM runtimes where MDTime-Time_Runtime < 3600 and MDTime IS NOT NULL";
    smd_df <- mydf(sql=sqlsmd,db=sim);
    md<-median(smd_df$md);

    sqlsdispo<-"SELECT DispoTime-Time_Runtime AS dispo FROM runtimes where DispoTime-Time_Runtime < 3600 and DispoTime IS NOT NULL";
    sdispo_df <- mydf(sql=sqlsdispo,db=sim);
    dispo<-median(sdispo_df$dispo);

    values=c(triage=triage,room=room,md=md,dispo=dispo);




# ++++++++++CONTROLS+++++++++++++++

    #triage
    sqltriage<-"select Simulation,Protocol,RegTime-Time_Runtime AS triage FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where RegTime IS NOT NULL && RegTime-Time_Runtime < 3600 && Protocol = '";
    endquote<-"'";
    sqltriage<-paste(sqltriage,protocol,endquote,sep='');
    triage_df<-mydf(sql=sqltriage,db=db);
    triage_med <-median(triage_df$triage);
    triage_q75<-as.numeric(quantile(triage_df$triage)[4]);

    #Room
     sqlroom<-"select Simulation,Protocol,RoomTime-Time_Runtime AS room FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where RoomTime IS NOT NULL && RoomTime-Time_Runtime < 3600 && Protocol = '";
    endquote<-"'";
    sqlroom<-paste(sqlroom,protocol,endquote,sep='');
    room_df <- mydf(sql=sqlroom, db=db);
    room_med <- median(room_df$room);
    room_q75<-as.numeric(quantile(room_df$room)[4]);


    #MD
sqlmd<-"select Simulation,Protocol,MDTime-Time_Runtime AS md FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where MDTime IS NOT NULL && MDTime-Time_Runtime < 3600 && Protocol = '";
    endquote<-"'";
    sqlmd<-paste(sqlmd,protocol,endquote,sep='');
    md_df <- mydf(sql=sqlmd, db=db);
    md_med <- median(md_df$md);
    md_q75<-as.numeric(quantile(md_df$md)[4]);


    #dispo
sqldispo<-"select Simulation,Protocol,DispoTime-Time_Runtime AS dispo FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where DispoTime IS NOT NULL && DispoTime-Time_Runtime < 3600 && Protocol = '";
    endquote<-"'";
    sqldispo<-paste(sqldispo,protocol,endquote,sep='');
    dispo_df <- mydf(sql=sqldispo, db=db);
    dispo_med <- median(dispo_df$dispo);
    dispo_q75<-as.numeric(quantile(dispo_df$dispo)[4]);


    q75<-c(triage=triage_q75,room=room_q75,md=md_q75,dispo=dispo_q75);
    medians<-c(triage=triage_med,room=room_med,md=md_med,dispo=dispo_med);




    z<-(list(simulation=sim,
             medians=medians,
             q75=q75,
             values=values
             ));
    class(z)<-'los';
    return(z);


}

plot.los <- function(z,color=TRUE){
    maintitle<-paste('SurgeSim Length of Stay Markers for',z$simulation);

    if(color){
         plot_colors<-c('blue','green','red');

    }else{
       plot_colors<-c('grey','black','black');
    }

    ymax<-(max(c(z$medians,z$values,z$q75))/60)+2;


    q<-barplot(z$values/60,main=maintitle,ylab='Median Time to Benchmark / min',ylim=c(0,ymax),col=plot_colors[1],sub='Values above the 75%ile may warrent investigation');
    lines(x=q,y=z$medians/60,type='o',lty=1,col=plot_colors[2]);
    lines(x=q,y=z$q75/60,type='o',lty=3,col=plot_colors[3]);
    legend(x=q[1],y=ymax,legend=c('Median','75%ile'),lty=c(1,3),col=c(plot_colors[2],plot_colors[3]));

}



function(sql,db='medstat1_surgesim') {
  library(RMySQL);
  con<-dbConnect(MySQL(),user=***REMOVED***,password=***REMOVED***,dbname=db,host='localhost');
  x<-dbGetQuery(con,sql);

  y<-data.frame(x);
  dbDisconnect(con);
  return(y);
}
