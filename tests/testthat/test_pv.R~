library(radmac2);
context('Length of Stay');

z<-los(db='emdm2016a',sim='emdm2016a',protocol='Geyserville');

test_that('q75 greater than mean', {
              expect_gt(z$q75[1],z$medians[1])
              expect_gt(z$q75[2],z$medians[2])
              expect_gt(z$q75[3],z$medians[3])
              expect_gt(z$q75[4],z$medians[4])
          })
