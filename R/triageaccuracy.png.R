run <- function(sim,model) {
  #link should be /server/cgi-bin/R/triageaccuracy.png?sim=foo&model=bar
  #http://localhost/cgi-bin/R/losbench.png?protocol=Geyserville&sim=emdm2016a
    library('radmac2');
    z<-radmac2::triage_accuracy(sim=sim,model=model);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}
