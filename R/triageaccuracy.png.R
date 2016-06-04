run <- function(sim,model) {
  #link should be /server/cgi-bin/R/triageaccuracy.png?sim=foo&model=bar
  #http://localhost/cgi-bin/R/triageaccuracy.png?sim=emdm2016a&model=start
  #requires mydfconfig.Rd in the /var/FastRWeb/tmp directory
    library('radmac2');
    z<-radmac2::triage_accuracy(sim=sim,model=model,db=db);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}
