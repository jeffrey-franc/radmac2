run <- function(protocol, sim,db="medstat1_surgesim") {
  #link should be /server/cgi-bin/R/losbench.png?protocol=foo&sim=bar&db=baz
  #http://localhost/cgi-bin/R/losbench.png?protocol=Geyserville&sim=emdm2016a&db=emdm2016a
    library("radmac2");
    z <- radmac2::los(protocol = protocol, sim = sim, db = db);
    p <- WebPlot(400, 400)
    plot(z);
    return(p);
}
