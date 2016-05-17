library(radmac2);
context("Triage Accuracy");

ta<-triage_accuracy(sim='emdm2016a',model='start');

test_that("Correct Calculation of Accuracy", {
              expect_equal(ta$tdata$acc,(ta$tdata$acnato-ta$tdata$nato))
          });
