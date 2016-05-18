#' radmac2: Calculate and plot surge capacity metrics from SurgeSim
#'
#' The package provides four types of benchmark plots
#'
#' The package requires a configuration object mydf2config which can be written
#' to the environment with the function mydf2config_make
#'
#' @references
#' Jeffrey Michael Franc, Pier Luigi Ingrassia, Manuela Verde, Davide Colombo
#' and Francesco Della Corte (2015). A Simple Graphical Method for Quantification
#' of Disaster Management Surge Capacity Using Computer Simulation and Process-
#' control Tools. Prehospital and Disaster Medicine, 30, pp 9-15.
#'
#' @docType package
#' @name radmac2
NULL



los<-function(x,...){
    UseMethod('los');
}

pv<-function(x,...){
    UseMethod('pv');
}

bench<-function(x,...){
    UseMethod('bench');
}



   #' Calculate length of stay for SurgeSim Simulation Group
    #'
    #' @param db Simulation database containing datamine
    #' @param sim Simulation group database
    #' @param protocol Simulation protocol for comparison
    #' @examples
  #' z<-los(protocol='Geyserville',sim='emdm2016a');
  #' @export
los<-function(db='medstat1_surgesim',sim,protocol){

    surgesimcalc(db=db,sim=sim,protocol=protocol,rtype='los');
}


 #' Calculate patient volume benchmarks for SurgeSim
    #'
    #' @param db Simulation database containing datamine
    #' @param sim Simulation group database
    #' @param protocol Simulation protocol for comparison
    #' @examples
  #' z<-pv(protocol='Geyserville',sim='emdm2016a');
  #' @export
pv<-function(db='medstat1_surgesim',sim,protocol){
    surgesimcalc(db=db,sim=sim,protocol=protocol,rtype='pv');
}

#' Calculate patient volume stay benchmarks for SurgeSim
    #'
    #' @param db Simulation database containing datamine
    #' @param protocol Simulation protocol for benchmarks
    #' @examples
  #' z<-bench(protocol='Geyserville');
  #' @export
bench<-function(db='medstat1_surgesim',protocol){
    surgesimcalc(db=db,sim=NULL,protocol=protocol,rtype='bench');
}


surgesimcalc<-function(db='medstat1_surgesim',sim,protocol,rtype){


if(!(is.null(sim))) {
# This Sim
    sqlstriage<-"SELECT RegTime-Time_Runtime AS triage FROM runtimes where RegTime-Time_Runtime < 3600 and RegTime-Time_Runtime > 0 && Time_Min > 0 && RegTime IS NOT NULL";
    striage_df <- mydf2(sql=sqlstriage,db=sim);
    triage<-median(striage_df$triage);
    ntriage<-length(striage_df$triage);


     sqlsroom<-"SELECT RoomTime-Time_Runtime AS room FROM runtimes where RoomTime-Time_Runtime < 3600 && RoomTime-Time_Runtime > 0 && Time_Min > 0 && RoomTime IS NOT NULL";
    sroom_df <- mydf2(sql=sqlsroom,db=sim);
    room<-median(sroom_df$room);
    nroom<-length(sroom_df$room);

sqlsmd<-"SELECT MDTime-Time_Runtime AS md FROM runtimes where MDTime-Time_Runtime < 3600 && MDTime-Time_Runtime > 0 && Time_Min > 0 && MDTime IS NOT NULL";
    smd_df <- mydf2(sql=sqlsmd,db=sim);
    md<-median(smd_df$md);
    nmd<-length(smd_df$md);

    sqlsdispo<-"SELECT DispoTime-Time_Runtime AS dispo FROM runtimes where DispoTime-Time_Runtime < 3600 && DispoTime-Time_Runtime > 0 && Time_Min > 0 && DispoTime IS NOT NULL";
    sdispo_df <- mydf2(sql=sqlsdispo,db=sim);
    dispo<-median(sdispo_df$dispo);
    ndispo<-length(sdispo_df$dispo);


    values<-c(triage=triage,room=room,md=md,dispo=dispo);
    nvalues<-c(triage=ntriage,room=nroom,md=nmd,dispo=ndispo);

}

# ++++++++++CONTROLS+++++++++++++++

    #triage
    sqltriage<-"select Simulation,Protocol,RegTime-Time_Runtime AS triage FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where RegTime IS NOT NULL && RegTime-Time_Runtime < 3600 && RegTime-Time_Runtime >0 && Protocol = '";
    endquote<-"'";
    sqltriage<-paste(sqltriage,protocol,endquote,sep='');
    triage_df<-mydf2(sql=sqltriage,db=db);
    triage_med <-median(triage_df$triage);
    triage_q75<-as.numeric(quantile(triage_df$triage)[4]);
    ntriage<-(with(triage_df,aggregate(triage,by=list(Simulation),FUN=length))[2])[,1];
    ntriage_med<-median(ntriage);
    ntriage_q25 <- as.numeric(quantile(ntriage))[2];

    #Room
     sqlroom<-"select Simulation,Protocol,RoomTime-Time_Runtime AS room FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where RoomTime IS NOT NULL && RoomTime-Time_Runtime < 3600 && RoomTime-Time_Runtime>0 && Time_Min > 0 && Protocol = '";
    endquote<-"'";
    sqlroom<-paste(sqlroom,protocol,endquote,sep='');
    room_df <- mydf2(sql=sqlroom, db=db);
    room_med <- median(room_df$room);
    room_q75<-as.numeric(quantile(room_df$room)[4]);
    nroom<-(with(room_df,aggregate(room,by=list(Simulation),FUN=length))[2])[,1];
    nroom_med<-median(nroom);
    nroom_q25 <- as.numeric(quantile(nroom))[2];

    #MD
sqlmd<-"select Simulation,Protocol,MDTime-Time_Runtime AS md FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where MDTime IS NOT NULL && MDTime-Time_Runtime < 3600 && MDTime-Time_Runtime > 0 && Time_Min > 0 && Protocol = '";
    endquote<-"'";
    sqlmd<-paste(sqlmd,protocol,endquote,sep='');
    md_df <- mydf2(sql=sqlmd, db=db);
    md_med <- median(md_df$md);
    md_q75<-as.numeric(quantile(md_df$md)[4]);
    nmd<-(with(md_df,aggregate(md,by=list(Simulation),FUN=length))[2])[,1];
    nmd_med<-median(nmd);
    nmd_q25 <- as.numeric(quantile(nmd))[2];


    #dispo
sqldispo<-"select Simulation,Protocol,DispoTime-Time_Runtime AS dispo FROM datamine RIGHT join stat_TOC ON datamine.Simulation=stat_toc.DBName where DispoTime IS NOT NULL && DispoTime-Time_Runtime < 3600 && DispoTime-Time_Runtime > 0 && Time_Min > 0 && Protocol = '";
    endquote<-"'";
    sqldispo<-paste(sqldispo,protocol,endquote,sep='');
    dispo_df <- mydf2(sql=sqldispo, db=db);
    dispo_med <- median(dispo_df$dispo);
    dispo_q75<-as.numeric(quantile(dispo_df$dispo)[4]);
    ndispo<-(with(dispo_df,aggregate(dispo,by=list(Simulation),FUN=length))[2])[,1];
    ndispo_med<-median(ndispo);
    ndispo_q25 <- as.numeric(quantile(ndispo))[2];



    q75<-c(triage=triage_q75,room=room_q75,md=md_q75,dispo=dispo_q75);
    medians<-c(triage=triage_med,room=room_med,md=md_med,dispo=dispo_med);
    nmedians<-c(triage=ntriage_med,room=nroom_med,md=nmd_med,dispo=ndispo_med);
    nq25<-c(triage=ntriage_q25,room=nroom_q25,md=nmd_q25,dispo=ndispo_q25);

    benchmarks<-list(triage=triage_df$triage,room=room_df$room,md=md_df$md,dispo=dispo_df$dispo);

#returns

  if(rtype=='los') {
     z<-(list(simulation=sim,
             medians=medians,
             q75=q75,
             values=values
              ));
     class(z)<-'los';
     return(z);

  } else if (rtype=='pv') {
      z2<-(list(simulation=sim,
              medians=nmedians,
              q25=nq25,
              values=nvalues
                ));
       class(z2)<-'pv';
       return(z2);
  } else {
     z3<-list(protocol=protocol,
             benchmarks=benchmarks
             );
     class(z3)<-'bench';
     return(z3);
    }

}

#' @export
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

#' @export
plot.pv <- function(z,color=TRUE){
    maintitle<-paste('SurgeSim Patient Volume Markers for',z$simulation);

    if(color){
         plot_colors<-c('blue','green','red');

    }else{
       plot_colors<-c('grey','black','black');
    }

    ymax<-(max(c(z$medians,z$values,z$q25)))+2;


    q<-barplot(z$values,main=maintitle,ylab='Number of Patients to Reach Benchmark',ylim=c(0,ymax),col=plot_colors[1],sub='Values below the 25%ile may warrent investigation');
    lines(x=q,y=z$medians,type='o',lty=1,col=plot_colors[2]);
    lines(x=q,y=z$q25,type='o',lty=3,col=plot_colors[3]);
    legend(x=q[3],y=ymax,legend=c('Median','25%ile'),lty=c(1,3),col=c(plot_colors[2],plot_colors[3]));

}

#' @export
plot.bench <-function(z,color=TRUE) {
  maintitle<-paste("SurgeSim Benchmarks for Protocol:",z$protocol);
 if(color){
         plot_color<-'blue';

    }else{
       plot_color<-'grey';
    }
  boxplot(lapply(z$benchmarks,function(x) x/60),main=maintitle,ylab='Time to Benchmark / min',col=plot_color);

}

#' @export
print.bench <-function(z){
    print(paste('Protocol:',z$protocol));
    print('Values');
    lapply(z$benchmarks,quantile);

}

