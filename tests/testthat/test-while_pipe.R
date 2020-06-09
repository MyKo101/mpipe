

x <- runif(20)

res_x <- x
while(length(res_x) > 1)
{
  res_x <- res_x %>%
    diff %>%
    `/`(2)
}



test_that("cond works",{
    expect_equal(x %>%
                   while_pipe(length(.) > 1,
                              . %>%
                                diff %>%
                                `/`(2)),
                 res_x)
})


test_that(".counter can be used in cond",{

  expect_equal(x %>%
                 while_pipe(.counter <= 19,
                            . %>%
                              diff %>%
                              `/`(2)),
               res_x)

})








