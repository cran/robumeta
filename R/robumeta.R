forest.robu <- function(x, es.lab, study.lab, ...){
  if(paste(x$ml[3]) != 1)  
      stop("Requires an intercept-only model.")
  output        <- x$output
  dat           <- as.data.frame(x$data)
  data.full     <- as.data.frame(x$data.full)
  dat$weights   <- data.full$r.weights
  dat$r.weights <- data.full$r.weights
  dat$es        <- data.full$effect.size
  dat$var       <- data.full$var.eff.size
  dat           <- droplevels(dat)
  elipses       <- list(...) 
  extracols     <- length(elipses)

  # prepare additional columns
  
  if (extracols > 0){
  add.col.lab <- c()
  add.col.title <- c()
  
  
  for (i in 1:extracols) {
    if(is.numeric(dat[,elipses[[i]]]))
      dat[,elipses[[i]]] <- format(dat[,elipses[[i]]], 
                                   digits = 3, justify = "left")
    
    add.col.lab[[i]]   <- dat[,elipses[[i]]]
    add.col.lab[[i] ]  <- as.character(add.col.lab[[i]])
    add.col.title[[i]] <- names(elipses[i])
  }
  }
  grand.ES      <- x$robust.coefficients
  grand.CI.L    <- x$CI.L
  grand.CI.U    <- x$CI.U
  dat$study.num <- data.full$study
  dat$es.num    <- ave(dat$study.num, dat$study.num, FUN = seq_along)
  dat$es.lab    <- dat[,es.lab]
  dat$es.lab    <- as.character(dat$es.lab)
  dat$study.lab <- dat[,study.lab]
  total.rows    <- length(dat$study.num) + 2*max(dat$study.num) + 2
  total.studies <- max(dat$study.num)

  # Effect size rows and labels
  es.rows <-  c()
  textcol.labels <- c()

  for(i in 1:total.studies) {
    w       <- which(dat$study.num == i)
    temp    <- (2*i) + w
    es.rows <- c(es.rows, temp)
  }

  for (i in 1:length(dat$study.num)) {
    textcol.labels[[i]] <- textGrob(paste(dat[i,]$es.lab), 
                                    x = 0, 
                                    just = "left")
  }

  labels  <- list(labels = textcol.labels)
  rows    <- list(rows = es.rows )
  textcol <- c(labels, rows)

  # Additional Columns
  
  add.col <- c()
  extra.cols <- list()
  add.rows <- c(es.rows, (min(es.rows)-1))
  j.pos <- (length(dat$study.num)+1)
  
  if (extracols > 0){
  
  for (i in 1:extracols) {
    extra.cols[[i]] <- list()
    
    for (j in 1:length(dat$study.num)) {
      extra.cols[[i]][[j]] <- list()
      extra.cols[[i]][[j]] <- textGrob(paste(add.col.lab[[i]][j]), 
                                    x = 0, 
                                    just = "left")
    }

    extra.cols[[i]][[j.pos]] <- textGrob(paste(add.col.title[[i]]), 
                                  x=0, 
                                  just="left", 
                                  gp=gpar(fontface="bold"))
    
    labels       <- list(labels = extra.cols[[i]])
    rows         <- list(rows = add.rows )
    add.col[[i]] <- c(labels, rows)
  }
  }

  # Study rows and labels
  study.rows   <- c()
  study.labels <- c()
  total.label  <- c()

  for(i in 1:total.studies) {
    w          <- which(dat$study.num == i)
    temp       <- min((2*i + w)) - 1
    study.rows <- c(study.rows, temp)
  }

  study.rows <- c(study.rows, max(total.rows))
  orig.study.rows <- which(dat$es.num == 1)

  for (i in 1:length(orig.study.rows)) {
    study.labels[[i]] <- textGrob(paste(dat[(orig.study.rows[i]),]$study.lab), 
                                  x=0, 
                                  just="left", 
                                  gp=gpar(fontface="bold"))
  }

  study.labels[[length(orig.study.rows)+1]] <- textGrob(paste("Total"), 
                                                        x = 0, 
                                                        just = "left", 
                                                        gp = gpar(fontface = 
                                                                    "bold"))
  textcol$labels <- c(textcol$labels, study.labels)
  textcol$rows   <- c(textcol$rows, study.rows)

  # Data column

  type          <- c()
  effect.size   <- c()
  size          <- c()
  CI.L          <- c()
  CI.U          <- c()

  for (i in 1:length(dat$study.num)) {
    effect.size[[i]] <- dat[i,]$es
    CI.U[[i]] <- dat[i,]$es + (1.96*sqrt(dat[i,]$var))
    CI.L[[i]] <- dat[i,]$es - (1.96*sqrt(dat[i,]$var))
    size[[i]] <- dat[i,]$weights 
    type[[i]] <- "n"
  }

  type  <- c(type, "s")
  rows  <- c(es.rows, max(es.rows)+2)
  size  <- size/max(size)
  ES    <- c(effect.size, grand.ES)
  CI.L  <- c(CI.L, grand.CI.L)
  CI.U  <- c(CI.U, grand.CI.U)

  datacol       <- list(type = type, rows = rows, size = size, CI.L = CI.L, 
                        CI.U = CI.U, ES = ES)
  datacolwidth  <- unit(3, "inches")
  min           <- floor(as.numeric(min(datacol$CI.L)))
  max           <- ceiling(as.numeric(max(datacol$CI.U)))
  datacol$range <- c(min, max)

  # Draw Label Column
  drawLabelCol <- function(col, j) {
    for (i in 1:length(col$rows)) {
      pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j))
      grid.draw(col$labels[[i]])
      popViewport()
    }
  }

  # Draw Confidence Intervals
  drawNormalCI <- function(CI.L, ES, CI.U, size) {
    grid.rect(x=unit(ES, "native"),
              width=unit(size, "snpc"), height=unit(size, "snpc"),
              gp=gpar(fill="black"))

    if (convertX(unit(CI.U, "native"), "npc", valueOnly=TRUE) > 1)
      grid.lines(x=unit(c(CI.L, 1), c("native", "npc")), y=.5,
                arrow=arrow(length=unit(0.05, "inches")))
      else { 
      lineCol <- "black"
      grid.lines(x=unit(c(CI.L, CI.U), "native"), y=0.5,
                gp=gpar(col=lineCol))
      }
    }

  # Draw Summary Effect Size Diamond
  drawSummaryCI <- function(CI.L, ES, CI.U) {

    grid.polygon(x=unit(c(CI.L, ES, CI.U, ES), "native"),
                y=unit(0.5 + c(0, 0.25, 0, -0.25), "npc"))
  }
  
  # Draw Data Column
  drawDataCol <- function(col, j) {
    pushViewport(viewport(layout.pos.col=j, xscale=col$range))
    grid.lines(x=unit(col$ES[length(col$ES)], "native"),
              y=unit(0:(total.rows-2), "lines"), gp=gpar(lty="dashed"))
    grid.xaxis(gp=gpar(cex=1))
    grid.text("Effect Size", y=unit(-3, "lines"), x = unit(0.5, "npc"), 
            just = "centre", gp=gpar(fontface="bold"))
    popViewport()
    x = unit(0.5, "npc")
    for (i in 1:length(col$rows)) {
      pushViewport(viewport(layout.pos.row=col$rows[i], layout.pos.col=j,
                            xscale=col$range))
      if (col$type[i] == "n")
        drawNormalCI(col$CI.L[i], col$ES[i], col$CI.U[i], col$size[i])
      else
        drawSummaryCI(col$CI.L[i], col$ES[i], col$CI.U[i])
      popViewport()
    }
  }

  # Prepare widths
  textcolwidth<- max(unit(rep(1, length(textcol$labels)), 
                                            "grobwidth", textcol$labels))
  colgap         <- unit(10, "mm")
  cols           <- unit.c( textcolwidth, colgap, datacolwidth, colgap)
  add.col.widths <- c()
  if (extracols > 0){
  for (i in 1:extracols) {
    add.col.widths[[i]] <-  max(unit(rep(1, length(add.col[[i]]$labels)), 
                                    "grobwidth", add.col[[i]]$labels))
    cols <- unit.c(cols, add.col.widths[[i]])
    cols <- unit.c(cols, colgap)
  }
  }

  # Layout and Columns
  pushViewport(viewport(layout=grid.layout(total.rows, (4 + (2*extracols)),
                          widths= cols,
                          heights=unit(c(1, rep(1, total.rows)), "lines"))))

  # Draw Title and Columns
  pushViewport(viewport(layout.pos.row=1))
  grid.text("Forest Plot", 
            y=unit(+3, "lines"),
            just = "center", 
            gp=gpar(fontface="bold"))
  grid.text(paste(x$model.lab1), 
            y=unit(+2, "lines"),
            just = "center", 
            gp=gpar(fontface="italic"))
  popViewport()

  # Draw columns
  drawLabelCol(textcol, 1)
  
  if (extracols > 0){
  for (i in 1:extracols) {
    drawLabelCol(add.col[[i]], ((i*2)+3))
  }
  }
  drawDataCol(datacol, 3)
  popViewport()
}

sensitivity <- function(x) UseMethod("sensitivity")

  sensitivity.robu <- function(x){
    
    modelweights   <- x$modelweights
    user.weighting <- x$user.weighting
    
    if(modelweights == "HIER")  
      stop("Sensitivity analysis is not available for hierarchical effects.")
    if(user.weighting == TRUE)  
      stop("Sensitivity analysis is not available for user specified weights.")
    
    switch(modelweights,
           
      
      HIER = {
        
      },
           
      CORR = {
        plotvalsALL <- 0
        plotvalls <- 0
        p         <- x$p
        N         <- x$n
        Xreg      <- x$Xreg
        y         <- x$y
        X         <- x$X
        data.full <- x$data.full
        X.full    <- x$X.full
        k         <- data.full$k
        kl        <- x$kl
        term1     <- x$term1
        term2     <- x$term2
        adjusted  <- x$adjusted
        rho.test  <- seq(0, 1, .2)
        type      <- c("Estimate", rep("-", p), "Std. Err.", rep("-", p),
                       "Tau.Sq")
        label     <- c(x$labels, x$labels, "-")
        df.sen    <- data.frame(Type = type,  Variable = label)
                              
        for (i in (1: length(rho.test))){
          tau.sq1 <- term1 + rho.test[i]*term2 
          tau.sq  <- ifelse(tau.sq1 < 0, 0, tau.sq1)
          data.full$r.weights <- 1/(data.full$k * 
                                 (data.full$avg.var.eff.size + tau.sq))
          W.r              <- by(data.full$r.weights, data.full$study, 
                                function(x) diag(x, nrow = length(x)))
          sumXWX.r         <- mapply(function(X,W) t(X) %*% W %*% X, 
                                    X=X, W=W.r, 
                                    SIMPLIFY = FALSE)
          sumXWy.r         <- mapply(function(X,W,y) t(X) %*% W %*% y, 
                                     X=X, W=W.r, y=y, 
                                     SIMPLIFY = FALSE)
          sumXWX.r         <- Reduce("+", sumXWX.r)
          sumXWy.r         <- Reduce("+", sumXWy.r)
          b.r              <- solve(sumXWX.r)%*%sumXWy.r # Robust betas
    
          data.full$pred.r <- Xreg%*%b.r
          data.full$e.r    <- cbind(data.full$effect.size) - data.full$pred.r
          data.full$e.r    <- as.numeric(data.full$e.r)
          sigma.hat.r      <- by(data.full$e.r, data.full$study, 
                                function(x) tcrossprod(x))
        
       if (adjusted != 1) { 

          sumXWeeWX.r      <- mapply(function(X,W,V) t(X) %*% W %*% V %*% W %*% X, 
                                              X = X, W = W.r, V = sigma.hat.r, 
                                             SIMPLIFY = FALSE)
          sumXWeeWX.r      <- Reduce("+", sumXWeeWX.r)   
          VR.r             <- solve(sumXWX.r) %*% sumXWeeWX.r %*% solve(sumXWX.r)  
          SE               <- sqrt(diag(VR.r)) * sqrt(N/(N-(p+1)))

        } else { 
          
          Q        <- solve(sumXWX.r)
          W.r.des  <- diag(data.full$r.weights)
          ImH      <- diag(c(1), dim(Xreg)[1], dim(Xreg)[1]) - 
                      Xreg %*% Q %*% t(Xreg) %*% W.r.des
          dfS      <- c(rep(0,p+1))
          diagOnes <- by(rep(1,nrow(X.full)), X.full$study, 
                         function(x) diag(x, nrow = length(x)))
          Q.list   <- rep(list(Q), N)
          ImHii    <- mapply(function(X, Q, W, D) D - X %*% Q %*% t(X) %*% W,
                                      X = X, Q = Q.list, W = W.r, D = diagOnes, 
                                      SIMPLIFY = FALSE)
          eigenvec <- lapply(ImHii, function(x) eigen(x)$vectors) 
          eigenval <- lapply(ImHii, function(x) eigen(x)$values)
          I        <- ImHii
          A.MBB    <- mapply(function (eigenvec, eigenval, kl) 
                       eigenvec %*% diag(1/sqrt(eigenval), kl, kl) %*% 
                       t(eigenvec),
                       eigenvec = eigenvec, eigenval = eigenval, kl = kl, 
                       SIMPLIFY = FALSE)
          A.MBB1   <- mapply(function(K, A, I) if(K > 1) A 
                      else matrix(sqrt(solve(I))), 
                      K = kl, I = I, A = A.MBB, 
                      SIMPLIFY = FALSE)
          
          A.MBB2                <- A.MBB 
          sumXWA.MBBeeA.MBBWX.r <- mapply(function(X,W,A,S) 
                                          t(X) %*% W %*% A %*% S %*% A %*% W %*%X, 
                                          X = X, W = W.r, A = A.MBB1, 
                                          S = sigma.hat.r, 
                                          SIMPLIFY = FALSE)
          sumXWA.MBBeeA.MBBWX.r <- Reduce("+", sumXWA.MBBeeA.MBBWX.r) 
          data.full$ImH         <- ImH
          ImH                   <- lapply(split(data.full$ImH, data.full$study), 
                                    matrix, ncol=nrow(data.full))

          giTemp                <- mapply(function(I, A, W, X, Q)
                                          t(I) %*% A %*% W %*% X %*% Q, 
                                          I = ImH, A = A.MBB2, W = W.r, X = X, 
                                          Q = Q.list, 
                                          SIMPLIFY = FALSE) 
          dfs <- c(rep(0,p+1))
          
          for (i in 1:(p+1)) { 
            L      <- c(rep(0,p+1))
            L[i]   <- 1
            Ll     <- rep(list(L), N)
            G      <- 0
            gi     <- mapply(function(G,L) G %*% cbind(L), 
                             G = giTemp, L = Ll, SIMPLIFY = FALSE)
            G      <- lapply(gi, function(x) tcrossprod(x))
            G      <- Reduce("+", G)
            B      <- solve(sqrt(W.r.des))%*% G %*% solve(sqrt(W.r.des))
            e.val2 <- eigen(B)
            dfs[i] <- sum(e.val2$values)^2/sum(e.val2$values^2)
          }
    
          VR.MBB1 <- solve(sumXWX.r)%*%sumXWA.MBBeeA.MBBWX.r%*%solve(sumXWX.r)
          VR.r    <- VR.MBB1
          SE      <- sqrt(diag(VR.r))         
        }       
        vals<- c(b.r, SE, tau.sq)
        vals <- format(vals, digits=3, justify="centre")
        df.sen <- cbind(df.sen, vals)
        }
      }
    )
    rho.names <- paste("rho=", seq(0,1,.2), sep="")
    colnames(df.sen)[3:(length(rho.test)+2)] <- rho.names
    return(df.sen)

  }
          
group.mean <- function(var, grp) {
  grp <- as.factor(grp)
  grp <- as.numeric(grp)
  var <- as.numeric(var)
  return(tapply(var, grp, mean, na.rm = TRUE)[grp])
}

group.center <- function(var, grp) {
  grp <- as.factor(grp)
  grp <- as.numeric(grp)
  var <- as.numeric(var)
  return(var - tapply(var, grp, mean, na.rm = TRUE)[grp])
}

print.robu <- function(x, digits = 3,...){

user.weighting <- x$user.weighting
modelweights <- x$modelweights

if(!user.weighting){
  
  switch(modelweights,
         
    HIER = { # Begin HIER
          
      cat(x$model.lab, "\n")
      cat("\nModel:",paste(x$ml[2]), paste(x$ml[[1]]), paste(x$ml[3]),"\n\n")
      cat(paste("Number of clusters ="), x$n, "\n")
      cat(paste("Number of outcomes ="), x$k, paste("(min ="), x$min.k,
          paste(", mean ="), x$mean.k, paste(", median ="), x$median.k, 
          paste(", max ="), x$max.k,")\n")
      cat(paste("Omega.sq ="), x$omega.sq, "\n")
      cat(paste("Tau.Sq ="), x$tau.sq, "\n\n")
      print(x$output)
      cat("---\n")
      cat("Signif. codes: < .01 *** < .05 ** < .10 *\n")
      cat("---\n")
      cat(x$notice)
           
    }, # End HIER
         
    CORR = { # Begin CORR
           
      cat(x$model.lab, "\n")
      cat("\nModel:",paste(x$ml[2]), paste(x$ml[[1]]), paste(x$ml[3]),"\n\n")
      cat(paste("Number of studies ="), x$n, "\n")
      cat(paste("Number of outcomes ="), x$k, paste("(min ="), x$min.k,
          paste(", mean ="), x$mean.k, paste(", median ="), x$median.k, 
          paste(", max ="), x$max.k,")\n")
      cat(paste("Rho ="), x$rho, "\n")
      cat(paste("I2 ="), x$I.2, "\n")
      cat(paste("Tau.Sq ="), x$tau.sq, "\n\n")
      print(x$output)
      cat("---\n")
      cat("Signif. codes: < .01 *** < .05 ** < .10 *\n")
      cat("---\n")
      cat(x$notice)      
             
    } # End CORR      
  )
  } else {
      cat(x$model.lab, "\n")
      cat("\nModel:",paste(x$ml[2]), paste(x$ml[[1]]), paste(x$ml[3]),"\n\n")
      cat(paste("Number of studies ="), x$n, "\n")
      cat(paste("Number of outcomes ="), x$k, paste("(min ="), x$min.k,
          paste(", mean ="), x$mean.k, paste(", median ="), x$median.k, 
          paste(", max ="), x$max.k,")\n\n")
      print(x$output)
      cat("---\n")
      cat("Signif. codes: < .01 *** < .05 ** < .10 *\n")
      cat("---\n")
      cat(x$notice) 
    
  } 
}

robu     <-function(formula, data, studynum,var.eff.size, userweights,
                    modelweights = c("CORR", "HIER"), rho = 0.8, 
                    small = TRUE, ...) {
  #, na.action = na.omit
  # Evaluate model weighting scheme.
  modelweights <- match.arg(modelweights)

  if(modelweights == "CORR" && rho > 1 | rho < 0)  
       stop("Rho must be a value between 0 and 1.")
  
  if (missing(userweights)){
    user.weighting = FALSE
  } else {
    user.weighting = TRUE
  }

  cl                       <- match.call() # Full model call
  mf                       <- match.call(expand.dots = FALSE)
  ml                       <- mf[[2]] # Extract formula 
  m                        <- match(c("formula", "data", "studynum", 
                                     "var.eff.size", "userweights"), names(mf)) 
  mf                       <- mf[c(1L, m)] 
  mf$drop.unused.levels    <- TRUE
  mf[[1L]]                 <- as.name("model.frame") 
  # test
  #mf$na.action             <- substitute(na.action)
  
  #
  mf                       <- eval(mf, parent.frame()) 
  
  if(!user.weighting){ 
    dframe                 <- data.frame(effect.size = mf[,1],
                                        model.matrix(formula, mf), 
                                        studynum = mf[["(studynum)"]],
                                        var.eff.size = mf[["(var.eff.size)"]])
    X.full.names           <-  names(dframe)[-match(c("effect.size", "studynum", 
                               "var.eff.size"), names(dframe))] 
  } else {
    
    dframe                 <- data.frame(effect.size = mf[,1],
                                        model.matrix(formula, mf), 
                                        studynum = mf[["(studynum)"]], 
                                        var.eff.size = mf[["(var.eff.size)"]],
                                        userweights = mf[["(userweights)"]])
      
    X.full.names           <-  names(dframe)[-match(c("effect.size", "studynum", 
                                "userweights", "var.eff.size"), names(dframe))] 
  }

  dframe$study             <- as.factor(dframe$studynum)
  dframe$study             <- as.numeric(dframe$study)
  dframe                   <- dframe[order(dframe$study),]
  k                        <- as.data.frame(unclass(rle(sort(dframe$study))))
  dframe$k                 <- k[[1]][ match(dframe$study, k[[2]])]
  dframe$avg.var.eff.size  <- ave(dframe$var.eff.size, dframe$studynum)
  dframe$sd.eff.size       <- sqrt(dframe$var.eff.size)
  
  switch(modelweights,
    HIER = {dframe$weights <- 1/dframe$var.eff.size},
    CORR = {dframe$weights <- 1/(dframe$k*dframe$avg.var.eff.size)}
  )
  
  X.full                   <- dframe[c("study", X.full.names)]
  data.full.names          <- names(dframe)[-match(c("studynum",X.full.names), 
                                          names(dframe))] 
  data.full                <- dframe[c(data.full.names)]
  
  clusters         <- data.full[ !duplicated(data.full$study), ] 
  k                <- clusters$k
  kl               <- as.list(k)
  p                <- ncol(X.full)-2 
  N                <- max(data.full$study) 
  W                <- by(data.full$weights, data.full$study, 
                         function(x) diag(x, nrow = length(x)))
  W                <- matrix(W)
  X                <- data.matrix(X.full)
  X                <- lapply(split(X[,2:(p+2)], X[,1]), matrix, ncol=p+1)
  y                <- by(data.full$effect.size, data.full$study, 
                         function(x) matrix(x))
  J                <- by(rep(1,nrow(X.full)), X.full$study, 
                         function(x) matrix(x, nrow = length(x), 
                                               ncol = length(x)))
  sigma            <- by(data.full$sd.eff.size, data.full$study, 
                         function(x) tcrossprod(x))
  vee              <- by(data.full$var.eff.size, data.full$study, 
                         function(x) diag(x, nrow = length(x)))
  SigmV            <- mapply(function(x, y) x - y,
                             x = sigma, y = vee, SIMPLIFY = FALSE)
  sumXWX           <- mapply(function(X, W) t(X) %*% W %*% X, 
                             X = X, W = W, SIMPLIFY = FALSE)
  sumXWy           <- mapply(function(X, W, y) t(X) %*% W %*% y, 
                             X = X, W = W, y = y, SIMPLIFY = FALSE)
  sumXWJWX         <- mapply(function(X, W, J) t(X) %*% W %*% J %*% W %*% X, 
                             X = X, W = W, J = J, SIMPLIFY = FALSE)
  sumXWVWX         <- mapply(function(X, W, V) t(X) %*% W %*% V %*% W %*% X, 
                             X = X, W = W, V = vee, SIMPLIFY = FALSE)
  sumXW.sig.m.v.WX <- mapply(function(X, W, V) t(X) %*% W %*% V %*% W %*% X, 
                             X = X, W = W, V = SigmV, SIMPLIFY = FALSE)
  sumXWX           <- Reduce("+", sumXWX)
  sumXWy           <- Reduce("+", sumXWy)
  sumXWJWX         <- Reduce("+", sumXWJWX)
  sumXWVWX         <- Reduce("+", sumXWVWX)
  sumXW.sig.m.v.WX <- Reduce("+", sumXW.sig.m.v.WX)
  
  switch(modelweights,
    
    HIER = { # Values needed for hierarchical effects model.
        
        tr.sumJJ <- mapply(function(J) sum(diag(J %*% J)), 
                           J = J, SIMPLIFY = FALSE) 
        sumXJX   <- mapply(function(X, J) t(X) %*% J %*% X, 
                           X = X, J = J, SIMPLIFY = FALSE)
        sumXWJJX <- mapply(function(X, W, J) t(X) %*% W %*% J %*% J %*% X, 
                           X = X, W = W, J = J, SIMPLIFY = FALSE)
        sumXJJWX <- mapply(function(X, W, J) t(X) %*% J %*% J%*% W %*% X, 
                           X = X, W = W, J = J, SIMPLIFY = FALSE)
        sumXWWX  <- mapply(function(X, W) t(X) %*% W %*% W %*% X, 
                           X = X, W = W, SIMPLIFY = FALSE)
        sumXJWX  <- mapply(function(X, W, J) t(X) %*% J %*% W %*% X, 
                           X = X, W = W, J = J, SIMPLIFY = FALSE)
        sumXWJX  <- mapply(function(X, W, J) t(X) %*% W %*% J %*% X, 
                           X = X, W = W, J = J, SIMPLIFY = FALSE)
        tr.sumJJ <- Reduce("+", tr.sumJJ)   
        sumXJX   <- Reduce("+", sumXJX) 
        sumXWJJX <- Reduce("+", sumXWJJX) 
        sumXJJWX <- Reduce("+", sumXJJWX) 
        sumXWWX  <- Reduce("+", sumXWWX) 
        sumXJWX  <- Reduce("+", sumXJWX) 
        sumXWJX  <- Reduce("+", sumXWJX) 
    }
  ) 
  
  b              <- solve(sumXWX)%*%sumXWy 
  Xreg           <- data.matrix(X.full[-c(1)])
  dimnames(Xreg) <- NULL
  data.full$pred <- Xreg%*%b 
  data.full$e    <- data.full$effect.size - data.full$pred 
  
  
  if (!user.weighting) { 
    
  switch(modelweights,
    
    HIER = { # Hierarchical Effects
      # Sigma_aj = tau.sq * J_j + omega.sq * I_j + V_j 
     
      # Qe is sum of squares 1
      # Qe = Sigma(T'WT)-(Sigma(T'WX)(Sigma(X'WX))^-1(Sigma(X'WT)
      # where W = V^(-1) and V = data.full$var.eff.size
      # Also, Qe = (y-xb)' W (y-xb)
      sumV <- sum(data.full$var.eff.size)
      W    <- diag(1/data.full$var.eff.size) 
      sumW <- sum(W)
      Qe   <- t(data.full$e) %*% W %*% data.full$e
     
      # Qa is sum of squares 2
      # Qa = sum(T-XB.hat)'J(T-XB.hat)
      # where B.hat = (X'WX)^-1(X'WT)
      # Also, Qa = (y-xb)'A (y-xb), A=diag(J)
      e      <- by(data.full$e, data.full$study, function(x) matrix(x))
      sumEJE <- mapply(function(e, J) 
                       t(e) %*% J %*% e, 
                       e = e, J = J, SIMPLIFY = FALSE)
      sumEJE <- Reduce("+", sumEJE) 
      Qa     <- sumEJE 
      
      # MoM estimators for tau.sq and omega.sq can be written as
      # omega.sq.h = A2(Qa-C1)-A1(Qe-C2) / B1A2-B2A1
      # tau.sq.h = Qe-C2/A2 - omega.sq.h(B2/A2) where
      
      # Vi = (t(X)WX)^-1
      V.i    <- solve(sumXWX)
      
      # A1 = Sigma(kj^2) - tr(V*Sigma(kj*t(Xj)*Jj*Wj*Xj)) - 
      #                    tr(V*Sigma(kj*t(Xj)*Jj*Wj*Xj)) +
      #                    tr(V*[Sigma(t(Xj)*Jj*Xj)]*V*Sigma(t(Xj)*Wj*Jj*Wj*Xj))
      # B1 = Sigma(kj)   - tr(V Sigma(t(Xj)*Jj*Wj*Xj)) -
      #                    tr(V Sigma(t(Xj)*Wj*Jj*Xj)) +
      #                    tr(V*[Sigma(t(Xj)*Jj*Xj)]*V*Sigma(t(Xj)*Wj^2*Xj)) 
      # C1 = tr(W^-1)    - tr(V*Sigma(t(X)*Jj*Xj))
      
      A1    <- tr.sumJJ  - sum(diag(V.i%*%sumXJJWX)) - 
                           sum(diag(V.i%*%sumXWJJX)) + 
                           sum(diag(V.i%*%sumXJX%*%V.i%*%sumXWJWX))
      B1    <- length(data.full$study) -
                           sum(diag(V.i%*%sumXWJX)) -
                           sum(diag(V.i%*%sumXJWX)) +
                           sum(diag(V.i%*%sumXJX%*%V.i%*%sumXWWX))

      C1    <- sumV      - sum(diag(V.i%*%sumXJX))
     
      # A2 = tr(W) - tr(V*Sigma(t(X)*Wj*Jj*Wj*Xj))
      # B2 = tr(W) - tr(V*Sigma(t(X)*Wj^2*Xj))
      # C2 = Sigma(kj-p)
      #
      A2   <- sumW - sum(diag(V.i%*%sumXWJWX)) 
      B2   <- sumW - sum(diag(V.i%*%sumXWWX)) 
      C2   <- length(data.full$study) - (p+1) 
      #
      # MoM estimator for omega.sq.h = A2(Qa-C1)-A1(Qe-C2) / B1A2-B2A1
      # Estimate of between-studies-wthin-cluster variance component
      omega.sq1  <- ((Qa-C1) * A2 - (Qe - C2) * A1)/(B1 * A2 - B2 * A1)
      omega.sq   <- ifelse(omega.sq1 < 0, 0, omega.sq1)
      #
      # MoM estimators for tau.sq: Qe-C2/A2 - omega.sq.h(B2/A2)
      # Estimate of between-clusters variance component 
      tau.sq1  <- ((Qe - C2)/A2) - omega.sq  *(B2/A2)
      tau.sq   <- ifelse(tau.sq1 < 0, 0, tau.sq1)

      # Approximate inverse variance weights
      data.full$r.weights <- (1/(data.full$var.eff.size + tau.sq + omega.sq))

    }, # End HIER
         
    CORR = { # Correlated Effects
      
      W    <- diag (data.full$weights) 
      sumW <- sum(data.full$weights) # Sum (k.j*w.j)
      Qe   <- t(data.full$e)%*%W%*%data.full$e 
      
      # The following components (denom, termA, termB, term1, term2)
      # are used in the calculation of the estimate of the residual 
      # variance component tau.sq.hat. 
      # Note: The effect of correlation on the estimates occurs entirely 
      # through the rho*term2 component.
      denom   <- sumW - sum(diag(solve(sumXWX)%*%sumXWJWX)) 
      termA   <- sum(diag(solve(sumXWX)%*%sumXWVWX))
      termB   <- sum(diag(solve(sumXWX)%*%sumXW.sig.m.v.WX ))
      term1   <- (Qe - N + termA) /denom 
      term2   <- termB / denom 
      tau.sq1 <- term1 + rho*term2 
      tau.sq  <- ifelse(tau.sq1 < 0, 0, tau.sq1)
      df      <- N - termA - rho*(termB) # Fixed
      I.2.1 <- ((Qe - df)/Qe) * 100
      I.2 <- ifelse(I.2.1 < 0, 0, I.2.1)
      
      # Approximate inverse variance weights
      data.full$r.weights <- 1/(data.full$k * 
                             (data.full$avg.var.eff.size + tau.sq))
      
    }    
  ) # End Switch
  
  } else {
  
    data.full$r.weights <- data.full$userweights
    
  }
  
  W.r              <- by(data.full$r.weights, data.full$study, 
                         function(x) diag(x, nrow = length(x)))
  sumXWX.r         <- mapply(function(X,W) t(X) %*% W %*% X, 
                             X=X, W=W.r, 
                             SIMPLIFY = FALSE)
  sumXWy.r         <- mapply(function(X,W,y) t(X) %*% W %*% y, 
                             X=X, W=W.r, y=y, 
                             SIMPLIFY = FALSE)
  sumXWX.r         <- Reduce("+", sumXWX.r)
  sumXWy.r         <- Reduce("+", sumXWy.r)
  b.r              <- solve(sumXWX.r)%*%sumXWy.r # Robust betas
  
  data.full$pred.r <- Xreg%*%b.r
  data.full$e.r    <- cbind(data.full$effect.size) - data.full$pred.r
  data.full$e.r    <- as.numeric(data.full$e.r)
  sigma.hat.r      <- by(data.full$e.r, data.full$study, 
                         function(x) tcrossprod(x))
  
  if (!small) {    # Begin Large-Sample (Hedges, et al., 2010)
    
    adjusted    <- 0
    sumXWeeWX.r <- mapply(function(X,W,V) t(X) %*% W %*% V %*% W %*% X, 
                          X = X, W = W.r, V = sigma.hat.r, 
                          SIMPLIFY = FALSE)
    sumXWeeWX.r <- Reduce("+", sumXWeeWX.r)   
    VR.r        <- solve(sumXWX.r) %*% sumXWeeWX.r %*% solve(sumXWX.r)  

    SE          <- sqrt(diag(VR.r)) * sqrt(N/(N-(p+1)))
    t           <- b.r/SE
    prob        <- 2 * (1 - pt(abs(t), df = N-(p+1)))
    CI.L        <- b.r - qt(.975, N-(p+1)) * SE
    CI.U        <- b.r + qt(.975, N-(p+1)) * SE
    
  
  } else { # Begin Small-Sample Corrections (Tipton, 2013)

    adjusted <- 1 # Fix this.
    Q        <- solve(sumXWX.r)
    W.r.des  <- diag(data.full$r.weights)
    ImH      <- diag(c(1), dim(Xreg)[1], dim(Xreg)[1]) - 
                Xreg %*% Q %*% t(Xreg) %*% W.r.des
    dfS      <- c(rep(0,p+1))
    diagOnes <- by(rep(1,nrow(X.full)), X.full$study, 
                   function(x) diag(x, nrow = length(x)))
    Q.list   <- rep(list(Q), N)
    ImHii    <- mapply(function(X, Q, W, D) D - X %*% Q %*% t(X) %*% W,
                    X = X, Q = Q.list, W = W.r, D = diagOnes, SIMPLIFY = FALSE)
    
    
    if (!user.weighting){
    
    switch(modelweights,
      
      HIER = { # Begin HIER
        
        inside   <- mapply(function(W, I) 
                           solve(sqrt(W)) %*% I %*% solve(sqrt(W)^3),
                           I = ImHii, W = W.r, SIMPLIFY = FALSE)
        I        <- inside
        eigenvec <- lapply(inside, function(x) eigen(x)$vectors) 
        eigenval <- lapply(inside, function(x) eigen(x)$values)
        
      }, # End HIER
      
      CORR = { # Begin CORR
        
        eigenvec <- lapply(ImHii, function(x) eigen(x)$vectors) 
        eigenval <- lapply(ImHii, function(x) eigen(x)$values)
        I        <- ImHii
        
      } # End CORR
           
    ) # End switch
    
    } else {

        V.big     <- diag(c(1), dim(Xreg)[1], dim(Xreg)[1]) %*% 
                     diag(data.full$avg.var.eff.size)
        v.j       <- by(data.full$avg.var.eff.size, data.full$study, 
                       function(x) diag(x, nrow = length(x)))
        v.j.sqrt  <- lapply(v.j, function (x) sqrt(x))
        V.j       <- mapply(function(V, D) V %*% V,
                            D = diagOnes, V = v.j, SIMPLIFY = FALSE)
        
        inside    <- mapply(function(V, I) I  %*%V %*% I,
                            I = ImHii, V = V.j, SIMPLIFY = FALSE)

        eigenvec <- lapply(inside, function(x) eigen(x)$vectors) 
        eigenval <- lapply(inside, function(x) eigen(x)$values)
        I        <- inside
        
      }
    
    A.MBB    <- mapply(function (eigenvec, eigenval, kl) 
                       eigenvec %*% diag(1/sqrt(eigenval), kl, kl) %*% 
                       t(eigenvec),
                       eigenvec = eigenvec, eigenval = eigenval, kl = kl, 
                       SIMPLIFY = FALSE)
    
    A.MBB1   <- mapply(function(K, A, I) if (K > 1) A 
                      else matrix(sqrt(solve(I))), 
                      K = kl, I = I, A = A.MBB, 
                      SIMPLIFY = FALSE)
    
    if (!user.weighting){
      
    switch(modelweights,
           
     HIER = { # Begin HIER
        
        A.MBB2                <- mapply(function(W, A) 
                                        solve(sqrt(W)) %*% A %*% solve(sqrt(W)),
                                        A = A.MBB1, W = W.r, SIMPLIFY = FALSE) 

        sumXWA.MBBeeA.MBBWX.r <- mapply(function(X,W,A,S) 
                                        t(X) %*% W %*% A %*% S %*% A %*% W %*%X, 
                                        X = X, W = W.r, A = A.MBB2, 
                                        S = sigma.hat.r, SIMPLIFY = FALSE)
      }, # End HIER
      
      CORR = { # Begin CORR
        
        A.MBB2                <- A.MBB1 
        sumXWA.MBBeeA.MBBWX.r <- mapply(function(X,W,A,S) 
                                        t(X) %*% W %*% A %*% S %*% A %*% W %*%X, 
                                        X = X, W = W.r, A = A.MBB2, 
                                        S = sigma.hat.r, 
                                        SIMPLIFY = FALSE)
      } # End CORR
           
    ) # End switch 
    
    } else {
        
        A.MBB2                <- mapply(function(V, A) 
                                        V %*% A,
                                        A = A.MBB1, V = v.j.sqrt, 
                                        SIMPLIFY = FALSE) 
        
        sumXWA.MBBeeA.MBBWX.r <- mapply(function(X,W,A,S) 
                                        t(X) %*% W %*% A %*% S %*% A %*% W %*%X, 
                                        X = X, W = W.r, A = A.MBB2, 
                                        S = sigma.hat.r, SIMPLIFY = FALSE)
      }
    
    sumXWA.MBBeeA.MBBWX.r <- Reduce("+", sumXWA.MBBeeA.MBBWX.r) 
    data.full$ImH         <- ImH
    ImH                   <- lapply(split(data.full$ImH, data.full$study), 
                                    matrix, ncol=nrow(data.full))
    giTemp                <- mapply(function(I, A, W, X, Q)
                                    t(I) %*% A %*% W %*% X %*% Q, 
                                    I = ImH, A = A.MBB2, W = W.r, X = X, 
                                    Q = Q.list, 
                                    SIMPLIFY = FALSE) 
   
    dfs <- c(rep(0,p+1))
      for (i in 1:(p+1)) { 
        L      <- c(rep(0,p+1))
        L[i]   <- 1
        Ll     <- rep(list(L), N)
        G      <- 0
        gi     <- mapply(function(G,L) G %*% cbind(L), 
                         G = giTemp, L = Ll, SIMPLIFY = FALSE)
        G      <- lapply(gi, function(x) tcrossprod(x))
        G      <- Reduce("+", G)
        if (!user.weighting){
          switch(modelweights,
            HIER = {B <- solve(sqrt(W.r.des))%*% G %*% solve(sqrt(W.r.des))},
            CORR = {B <- solve(sqrt(W.r.des))%*% G %*% solve(sqrt(W.r.des))}
          )
        } else {
                    B <- solve(sqrt(V.big))%*% G %*% solve(sqrt(V.big)) 
        }
        e.val2 <- eigen(B)
        dfs[i] <- sum(e.val2$values)^2/sum(e.val2$values^2)
      }
    
    VR.MBB1 <- solve(sumXWX.r)%*%sumXWA.MBBeeA.MBBWX.r%*%solve(sumXWX.r)
    VR.r    <- VR.MBB1

    SE      <- sqrt(diag(VR.r))
    t       <- b.r/SE
    prob    <- 2 * (1 - pt(abs(t), df = dfs)) 
    CI.L    <- b.r - qt(.975, dfs) * SE
    CI.U    <- b.r + qt(.975, dfs) * SE
  }
  
    names(X.full)[2] <- "intercept"
    labels           <- c(colnames(X.full[2:length(X.full)]))
    labels           <- format(labels, justify = "left")
    b.r              <- format(b.r,digits=3, justify="centre")
    SE               <- format(SE, digits=3, justify="centre")
    t                <- format(t, digits=3, justify="centre")
    prob             <- format(prob, digits=3, 
                               scientific = FALSE, justify="centre")
    sig              <- ifelse(prob < .01, "***", 
                        ifelse(prob > .01 & prob < .05, "**",
                        ifelse(prob > .05 & prob < .10, "*", "")))
    CI.L             <- format(CI.L, digits=3, 
                               scientific = FALSE, justify="centre")
    CI.U             <- format(CI.U, digits=3, 
                               scientific = FALSE, justify="centre")
  
  if (!small) { # Begin Large Sample (Hedges et al., 2010)
    
    
    output            <- data.frame(cbind(labels,b.r,SE,t,prob,CI.L,CI.U, sig))
    colnames(output)  <- c("", "Estimate","StdErr", "t-value", "P(|t|>)", 
                          "95% CI.L","95% CI.U", "Sig")
    model.lab2        <- ""
    notice            <- ""
    
  
  } else { # Begin Small-Samples Correction Tipton (2013)
    
    dfs.SW           <- format(dfs, digits=3, 
                               scientific=FALSE, justify = "centre")
    output           <- data.frame(cbind(labels, b.r, SE, t, dfs.SW, prob, 
                                         CI.L, CI.U, sig))
    colnames(output) <- c("", "Estimate","StdErr","t-value","df","P(|t|>)",
                          "95% CI.L","95% CI.U", "Sig")
    model.lab2       <- "with Small-Sample Corrections"            
    notice           <- "Note: If df < 3, do not trust the results"
    
  }

  k        <- unlist(kl)
  min.k    <- min(k)
  max.k    <- max(k)
  mean.k   <- mean(k)
  mean.k   <- format(mean.k, digits=3)
  median.k <- median(k)
  
  if (!user.weighting) {
  
  switch(modelweights,
         
    HIER = {
      I.2   <- NA
      term1 <- NA
      term2 <- NA
      model.lab1 <- "RVE: Hierarchical Effects Model"
      model.lab  <- c(model.lab1, model.lab2)
    },
         
    CORR = {
      omega.sq   <- NA
      model.lab1 <- "RVE: Correlated Effects Model"
      model.lab  <- c(model.lab1, model.lab2)
    }
  )
  } else {
    omega.sq <- NA
    tau.sq   <- NA
    I.2      <- NA
    term1    <- NA
    term2    <- NA
    Qe       <- NA
    term1    <- NA
    term2    <- NA
    model.lab1 <- "RVE: User Specified Weights"
    model.lab  <- c(model.lab1, model.lab2)
  }

  res <- list( data.full = data.full, X.full = X.full, output = output, 
               rho = rho, min.k = min.k, mean.k = mean.k, median.k = 
               median.k, max.k = max.k, I.2 = I.2, tau.sq = tau.sq, 
               omega.sq = omega.sq, coefficients = b, predicted = 
               data.full$pred, residuals = data.full$e, p = p, effect.sizes = 
               data.full$effect.size, robust.coefficients = b.r, 
               robust.predicted = data.full$pred.r, n = N, k = sum(k), 
               QE = Qe, robust.residuals = data.full$e.r, robust.varcov = VR.r, 
               ml = ml, weights = data.full$weights, r.weights = 
               data.full$r.weights, studynum = data.full$studynum, 
               model.lab = model.lab, notice = notice, modelweights = 
               modelweights, kl = kl, avg.var = data.full$avg.var.eff.size,
               X = X, y = y, Xreg = Xreg, term1 = term1, term2 = term2, 
               labels = labels, SE = SE, adjusted = adjusted, mf = mf,
               user.weighting = user.weighting, data = data, model.lab1 = 
               model.lab1, call = sys.call(), CI.L = CI.L, CI.U = CI.U)

  class(res) <- "robu"
  res
}