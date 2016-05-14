run <- function(protocol,sim) {
  #link should be /server/cgi-bin/R/losbench.png?protocol=foo&sim=bar
  #http://localhost/cgi-bin/R/pvbench.png?protocol=Geyserville&sim=emdm2016a
    library('radmac2');
    z<-radmac2::pv(protocol=protocol,sim=sim);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}