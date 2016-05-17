library(radmac2);
context('Benchmarks');

bm<-bench(db='emdm2016a',protocol='Geyserville');

test_that('Protocol is correct', {
              expect_true(bm$protocol=='Geyserville')
          })
test_that('All values are positive',{
              expect_true(all(bm$benchmarks$triage>0))
              expect_true(all(bm$benchmarks$room>0))
              expect_true(all(bm$benchmarks$md>0))
              expect_true(all(bm$benchmarks$dispo>0))
           })


