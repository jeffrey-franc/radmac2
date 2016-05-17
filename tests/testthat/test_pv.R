library(radmac2);
context('Patient Volumes');

z<-pv(db='emdm2016a',sim='emdm2016a',protocol='Geyserville');


test_that('q75 greater than mean', {
              expect_lt(z$q25[1],z$medians[1])
              expect_lt(z$q25[2],z$medians[2])
              expect_lt(z$q25[3],z$medians[3])
              expect_lt(z$q25[4],z$medians[4])
          })


