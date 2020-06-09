

test_that("function name evaluation happens with implicit vectors",{
  expect_equal(eval_expr(c(4,8,15,16,23,42),mean),
               mean(c(4,8,15,16,23,42)))
  expect_equal(eval_expr(c(4,8,15,16,23,42,NA),sd),NA_real_)
})

test_that("dot functions evaluated with implicit vectors",{
  expect_equal(eval_expr(c(4,8,15,16,23,42,NA),sd(.,na.rm=T)),
               sd(c(4,8,15,16,23,42)))
  expect_equal(eval_expr("hello",substr(.,1,1)),
               substr("hello",1,1))
})

test_that("pipes evaluation hapens with implicit vectors",{
  expect_equal(eval_expr(c(4,8,15,16,23,42),. %>% mean),
               mean(c(4,8,15,16,23,42)))
  expect_equal(eval_expr(c(4,8,15,16,23,42,NA),. %>% sd(na.rm=T)),
               sd(c(4,8,15,16,23,42)))
})





x <- c(4,8,15,16,23,42)
x_NA <- c(4,8,15,16,23,42,NA)

hello_world <- c("hello","world")


test_that("function name evaluation happens with explicit vectors",{
  expect_equal(eval_expr(x,mean),mean(x))
  expect_equal(eval_expr(x_NA,sd),NA_real_)
})

test_that("dot functions evaluated with explicit vectors",{
  expect_equal(eval_expr(x_NA,sd(.,na.rm=T)),sd(x))
  expect_equal(eval_expr(hello_world,substr(.,1,1)),
               substr(hello_world,1,1))
})

test_that("pipes evaluation hapens with explicit vectors",{
  expect_equal(eval_expr(x,. %>% mean),mean(x))
  expect_equal(eval_expr(x,. %>% sd(na.rm=T)),sd(x,na.rm=T))
})







tbl <- data.frame(x=runif(10),y=sample(letters,10,replace=T))

test_that("function name evaluation happens on data.frames",{
  expect_equal(eval_expr(tbl,nrow),nrow(tbl))
})

test_that("dot functions evaluation happens on data.frames",{
  expect_equal(eval_expr(tbl,vapply(.,class,character(1))),
               vapply(tbl,class,character(1)))
})

test_that("names functions evaluation happens on data.frames",{
  expect_equal(eval_expr(tbl,mean(x)),
               mean(tbl$x))
  expect_equal(eval_expr(tbl,substring(y,1,1)),
               substring(tbl$y,1,1))
})

test_that("pipes evaluation hapens on data.frames",{
  expect_equal(eval_expr(tbl,. %>% nrow),nrow(tbl))
  expect_equal(eval_expr(tbl,. %>% vapply(class,character(1))),
               vapply(tbl,class,character(1)))
})







lst <- list(x=runif(10),y="hello world")

test_that("function name evaluation happens on list",{
  expect_equal(eval_expr(lst,length),length(lst))
})

test_that("dot functions evaluation happens on list",{
  expect_equal(eval_expr(lst,vapply(.,class,character(1))),
               vapply(lst,class,character(1)))
})

test_that("names functions evaluation happens on list",{
  expect_equal(eval_expr(lst,mean(x)),
               mean(lst$x))
  expect_equal(eval_expr(lst,substring(y,1,1)),
               substring(lst$y,1,1))
})

test_that("pipes evaluation hapens on data.frames",{
  expect_equal(eval_expr(lst,. %>% length),length(lst))
  expect_equal(eval_expr(lst,. %>% vapply(class,character(1))),
               vapply(lst,class,character(1)))
})






test_that("function name evaluation happens on data.frame with extra variable",{
  expect_equal(eval_expr(tbl,length,z=1),length(tbl))
})

test_that("dot functions evaluation happens on data.frame with extra variable",{
  expect_equal(eval_expr(tbl,mean(.$x + z),z=1),
               mean(tbl$x+1))
})

test_that("names functions evaluation happens on data.frame with extra variable",{
  expect_equal(eval_expr(tbl,mean(x+z),z=1),
               mean(tbl$x+1))
})

test_that("pipes evaluation hapens on data.frames with extra variable",{
  expect_equal(eval_expr(tbl,. %>% mutate(w=x+z),z=1),
               mutate(tbl,w=x+1))

})













e1 <- new.env()
e1$z  <- 2


test_that("function name evaluation happens on data.frame with extra variable & environment",{
  expect_equal(eval_expr(tbl,length,z=1,env=e1),length(tbl))
})

test_that("dot functions evaluation happens on data.frame with extra variable & environment",{
  expect_equal(eval_expr(tbl,mean(.$x + z),z=1,env=e1),
               mean(tbl$x+1))
})

test_that("names functions evaluation happens on data.frame with extra variable & environment",{
  expect_equal(eval_expr(tbl,mean(x+z),z=1,env=e1),
               mean(tbl$x+1))
})

test_that("pipes evaluation hapens on data.frames with extra variable & environment",{
  expect_equal(eval_expr(tbl,. %>% mutate(w=x+z),z=1,env=e1),
               mutate(tbl,w=x+1))

})


lst <- list(x=c(4,8,15,16,23,42),
            my_fun = mean)

test_that("functions defined within data work",{
  expect_equal(eval_expr(lst,my_fun(x)),
               mean(c(4,8,15,16,23,42)))

})




test_that("value precedent works correctly",{
  lst <- list(x=1:3,y=1)
  y <- 4

  e1 <- new.env(parent=emptyenv())
  e1$y <- 3

  expect_equal(eval_expr(lst,y,y=2,env=e1),1)

  lst$y <- NULL

  expect_equal(eval_expr(lst,y,y=2,env=e1),2)

  expect_equal(eval_expr(lst,y,env=e1),3)

  expect_equal(eval_expr(lst,y),4)

})



test_that("NULL outputs are flagged as errors (when needed)",{
  expect_error(eval_expr(1,NULL))
  expect_null(eval_expr(1,NULL,allow_NULL = T))

})













