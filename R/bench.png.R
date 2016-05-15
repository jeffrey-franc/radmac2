run <- function(protocol,db='medstat1_surgesim') {
  #link should be /server/cgi-bin/R/losbench.png?protocol=foo&sim=bar
  #http://localhost/cgi-bin/R/bench.png?protocol=Geyserville&db=emdm2016a
    library('radmac2');
    z<-radmac2::bench(protocol=protocol,db=db);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}
