
triage_accuracy<-function(x,...){
    UseMethod('triage_accuracy');
}

#' Calculate Triage Accuracy for SurgeSim
#'
#' @param sim Simulation database
#' @param model Triage method used: 'start','ctas', or 'nato'
#' @examples
#' z<-triage_accuracy(sim='emdm2016a',model='start');
#' @export
triage_accuracy<-function(sim,model){

 sql<-'SELECT Acuity as acuity,TriageCode_NATO as nato, Triage_CTAS as ctas, Triage_START as start FROM runtimes RIGHT JOIN patient_data on runtimes.PatientNumber = patient_data.PatientNumber WHERE RegTime IS NOT NULL';
    tc<-mydf(sql=sql,db=sim);

 #Convert all triage codes to NATO
 tc$acnato<-tc$acuity; #create corrected acuity
 if(model=='ctas'){
     tc$acnato[tc$acnato==4 | tc$acnato==5]<-3;
 }

 tc$acc<-tc$acnato - tc$nato;

 ta<-list(simulation=sim,
           model=model,
           tdata=tc
       );
 class(ta)<-'triage_accuracy';
 return(ta);


}

#' @export
print.triage_accuracy<-function(ta){

  cat(paste('Simulation:',ta$simulation));
  cat(paste('\nTriage Model:',ta$model,'\n'));
  cat('\nConfusion Matrix: (Corrected to NATO)\n');
  table(Assigned=ta$tdata$acnato,True=ta$tdata$nato);

}

#' @export
plot.triage_accuracy <-function(ta,color=TRUE) {
    if(color){
        bar_color='blue';
    }else{
        bar_color='grey';
    }
    mtitle<-paste('SurgeSim Triage Accuracy for ',ta$simulation);

   barplot(table(-ta$tdata$acc),sub='Positive Numbers Indicate OverTriage', xlab='Triage Over / Under',ylab='Frequency',col=bar_color,main=mtitle);
}

