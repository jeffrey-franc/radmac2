run <- function(protocol,sim,db='medstat1_surgesim') {
  #link should be /server/cgi-bin/R/losbench.png?protocol=foo&sim=bar@db=baz
  #http://localhost/cgi-bin/R/pvbench.png?protocol=Geyserville&sim=emdm2016a&db=emdm2016a
  #requires mydfconfig.Rd in the /var/FastRWeb/tmp directory

    library('radmac2');
    z<-radmac2::pv(protocol=protocol,sim=sim);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}
