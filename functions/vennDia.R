# grabbed Thomas Girke's Venn Diagram functions from
# source("http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/vennDia.R")

venndiagram <- function(x=x, y=y, z=z, w=w, unique=T, title="Venn Diagram", labels=c("x", "y", "z", "w"), 
         lines=1, lcol=1, tcol=1, diacol=1, plot=T, type="3", printsub=TRUE, ...) {
  ## Remove duplicates and NA fields in x, y, z and w
  if(unique==T) {
    x <- unique(x); x <- as.vector(na.omit(x))
    y <- unique(y); y <- as.vector(na.omit(y))
    if(!missing("z")) {
      z <- unique(z); z <- as.vector(na.omit(z))
    }
    if(!missing("w")) {
      w <- unique(w); w <- as.vector(na.omit(w))
    }
  }
  
  ## Check valid type selection
  if(!type %in% c("2", "2map", "3", "3map", "4", "4map", "4el", "4elmap")) {
    return("Error: the 'type' argument can only be set to one of these values: 2, 2map, 3, 3map, 4, 4map, 4el, 4elmap.")	
  }
  
  ## Plot a 2-way venn diagram
  if(type=="2") {
    # Define ovelap queries 
    q1 <- x[x %in% y]
    q2 <- x[!x %in% y]
    q3 <- y[!y %in% x]
    
    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3)
    
    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(5.0, 3.1, 7.0), y=c(6.1, 6.1, 6.1), count=countDF$count)
    if(printsub==TRUE) {mysub <- paste(paste("N unique: xy =", length(unique(c(x,y)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), sep="")} else {mysub <- ""}		
    if(plot==T) {
      ## Plot the 2-way venn diagram
      symbols(x=c(4, 6), y = c(6, 6), circles=c(2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
      text(olDF$x, olDF$y, olDF$count, col=tcol, ...); text(c(2.0, 8.0), c(8.8, 8.8), labels[1:2], col=lcol, ...)
    }
    
    ## Return query list 
    return(qlist)
  }
  
  ## Plot 2-way mapping venn diagram
  if(type=="2map") {
    olDFdebug <- data.frame(x=c(5.0, 3.1, 7.0), y=c(6.1, 6.1, 6.1), count=paste("q", 1:3, sep=""), ...)
    symbols(x=c(4, 6), y = c(6, 6), circles=c(2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
    text(olDFdebug$x, olDFdebug$y, olDFdebug$count, col=tcol, ...); text(c(2.0, 8.0), c(8.8, 8.8), paste(labels[1:2], "=", c("x","y")), col=lcol, ...)
  }
  
  ## Plot a 3-way venn diagram
  if(type=="3") {
    ## Define ovelap queries 
    q1 <- x[x %in% y & x %in% z]
    q2 <- x[x %in% z]; q2 <- q2[!q2 %in% y]
    q3 <- y[y %in% z]; q3 <- q3[!q3 %in% x]
    q4 <- x[x %in% y]; q4 <- q4[!q4 %in% z]
    q5 <- x[!x %in% y]; q5 <- q5[!q5 %in% z]
    q6 <- y[!y %in% z]; q6 <- q6[!q6 %in% x]
    q7 <- z[!z %in% x]; q7 <- q7[!q7 %in% y]
    
    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3, q4=q4, q5=q5, q6=q6, q7=q7)
    
    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(5.0, 3.8, 6.3, 5.0, 3.0, 7.0, 5.0), y=c(5.6, 4.6, 4.6, 6.9, 6.5, 6.5, 3.0), count=countDF$count)
    if(printsub==TRUE) {mysub <- paste(paste("N unique: xyz =", length(unique(c(x,y,z)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), paste("; z =", length(unique(z))), sep="")} else { mysub <- "" }
    if(plot==T) {
      ## Plot the 3-way venn diagram
      symbols(x=c(4, 6, 5), y = c(6, 6, 4), circles=c(2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
      text(olDF$x, olDF$y, olDF$count, col=tcol, ...); text(c(2.0, 8.0, 6.0), c(8.8, 8.8, 1.1), labels[1:3], col=lcol, ...)
    }
    
    ## Return query list 
    return(qlist)
  }
  
  ## Plot 3-way mapping venn diagram
  if(type=="3map") {
    olDFdebug <- data.frame(x=c(5.0, 3.8, 6.3, 5.0, 3.0, 7.0, 5.0), y=c(5.6, 4.6, 4.6, 6.9, 6.5, 6.5, 3.0), count=paste("q", 1:7, sep=""), ...)
    symbols(x=c(4, 6, 5), y = c(6, 6, 4), circles=c(2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
    text(olDFdebug$x, olDFdebug$y, olDFdebug$count, col=tcol, ...); text(c(2.0, 8.0, 6.0), c(8.8, 8.8, 1.1), paste(labels[1:3], "=", c("x","y","z")), col=lcol, ...)
  }
  
  ## Overlap queries for 4-way venn diagram
  if(type=="4" | type=="4el" | type=="4elmap") {
    ## Define ovelap queries 
    xy <- x[x %in% y]; xz <-x[x %in% z]; xw <- x[x %in% w]; yz <- y[y %in% z]; yw <- y[y %in% w]; zw <- z[z %in% w]
    q1 <- xy[xy %in% zw]
    q2 <- xw[xw %in% z]; q2 <- q2[!q2 %in% y]
    q3 <- yz[yz %in% w]; q3 <- q3[!q3 %in% x]
    q4 <- yz[yz %in% x]; q4 <- q4[!q4 %in% w]
    q5 <- xw[xw %in% y]; q5 <- q5[!q5 %in% z]
    q6 <- xy[!xy %in% z]; q6 <- q6[!q6 %in% w]
    q7 <- zw[!zw %in% x]; q7 <- q7[!q7 %in% y]
    q8 <- xz[!xz %in% y]; q8 <- q8[!q8 %in% w] 
    q9 <- yw[!yw %in% x]; q9 <- q9[!q9 %in% z]
    q10 <- x[!x %in% c(y,z,w)]
    q11 <- y[!y %in% c(x,z,w)]
    q12 <- z[!z %in% c(x,y,w)]
    q13 <- w[!w %in% c(x,y,z)]
    q14 <- xw[!xw %in% y]; q14 <- q14[!q14 %in% z]
    q15 <- yz[!yz %in% x]; q15 <- q15[!q15 %in% w]
    
    ## Store query vectors in list
    qlist <- list(q1=q1, q2=q2, q3=q3, q4=q4, q5=q5, q6=q6, q7=q7, q8=q8, q9=q9, q10=q10, q11=q11, q12=q12, q13=q13, q14=q14, q15=q15)
    
    ## Perfom query counts
    count <- unlist(lapply(qlist, length))
    countDF <- data.frame(query=names(count) , count=as.vector(count))
    olDF <- data.frame(x=c(4.8, 3.9, 5.7, 3.9, 5.7, 4.8, 4.8, 3.0, 6.5, 3.0, 6.5, 3.0, 6.5, 4.8, 4.8), y=c(5.2, 4.2, 4.2, 6.3, 6.3, 7.2, 3.2, 5.2, 5.2, 7.2, 7.2, 3.2, 3.2, 1.0, 0.4), count=countDF$count)
    
    if(printsub==TRUE) {mysub <- paste(paste("N unique: xyzw =", length(unique(c(x,y,z,w)))), paste("; x =", length(unique(x))), paste("; y =", length(unique(y))), paste("; z =", length(unique(z))), paste("; w =", length(unique(w))), sep="") } else { mysub <- "" }
    
    ## Plot 4-way venn diagram as circles
    if(plot==T & type=="4") {
      symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main=title, sub=mysub, xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
      text(olDF$x[1:13], olDF$y[1:13], olDF$count[1:13], col=tcol, ...) # rows 14-15 of olDF are printed in last step
      text(c(2.0, 7.5, 2.0, 7.5), c(8.3, 8.3, 2.0, 2.0), labels, col=lcol, ...)
      text(c(3.8, 3.8), c(1.0, 0.4), c(paste("Only in ", labels[1], " & ", labels[4], ": ", olDF$count[14], sep=""), paste("Only in ", labels[2], " & ", labels[3], ": ", olDF$count[15], sep="")), col=diacol, ...)
    }
    
    ## Plot 4-way venn diagram as ellipses
    if(plot==T & (type=="4el" | type=="4elmap")) {
      olDF <- data.frame(x=c(5.0, 4.2, 6.4, 3.6, 5.8, 2.9, 7.1, 3.1, 6.9, 1.5, 3.5, 6.5, 8.5, 5.0, 5.0), y=c(2.8, 1.4, 4.0, 4.0, 1.4, 5.9, 5.9, 2.2, 2.2, 4.8, 7.2, 7.2, 4.8, 0.7, 6.0), count=countDF$count)
      ## Plot ellipse
      plotellipse <- function (center=c(1,1), radius=c(1,2), rotate=1, segments=360, xlab="", ylab="", ...) {
        angles <- (0:segments) * 2 * pi/segments  
        rotate <- rotate*pi/180
        ellipse <- cbind(radius[1] * cos(angles), radius[2] * sin(angles))
        ellipse <- cbind( ellipse[,1]*cos(rotate) + ellipse[,2]*sin(rotate), ellipse[,2]*cos(rotate) - ellipse[,1]*sin(rotate) )
        ellipse <- cbind(center[1]+ellipse[,1], center[2]+ellipse[,2])	
        plot(ellipse, type = "l", xlim = c(0, 10), ylim = c(0, 10), xlab = "", ylab = "", ...)
      }
      ## Plot ellipse as 4-way venn diagram
      ellipseVenn <- function(lines=lines, olDF, title=title, labels=labels, sub=mysub, main, lcol=lcol, tcex=1.3, ...) {
        split.screen(c(1,1))
        plotellipse(center=c(3.5,3.6), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[1], axes=FALSE, main=title, sub=mysub, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(4.7,4.4), radius=c(2,4), rotate=-35, segments=360, xlab="", ylab="", col=lines[2], axes=FALSE, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(5.3,4.4), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[3], axes=FALSE, ...)
        screen(1, new=FALSE)
        plotellipse(center=c(6.5,3.6), radius=c(2,4), rotate=35, segments=360, xlab="", ylab="", col=lines[4], axes=FALSE, ...)
        text(olDF[1:15,1], olDF[1:15,2], olDF[1:15,3], col=tcol, ...)
        text(c(0.4, 2.8, 7.5, 9.4), c(7.3, 8.3, 8.3, 7.3), labels, col=lcol, ...)
        close.screen(all=TRUE) 
      }
      ## Plot 4-way ellipse venn diagram
      if(type=="4el") {
        ellipseVenn(olDF=olDF, lcol=lcol, lines=lines, labels=labels, title=title, ...)
      }
      
      ## Plot 4-way ellipse mapping venn diagram
      if(type=="4elmap") {
        olDFdebug <- data.frame(x=c(5.0, 4.2, 6.4, 3.6, 5.8, 2.9, 7.1, 3.1, 6.9, 1.5, 3.5, 6.5, 8.5, 5.0, 5.0), y=c(2.8, 1.4, 4.0, 4.0, 1.4, 5.9, 5.9, 2.2, 2.2, 4.8, 7.2, 7.2, 4.8, 0.7, 6.0), count=paste("q", 1:15, sep=""), ...)
        ellipseVenn(olDF=olDFdebug, lcol=lcol, lines=lines, labels=paste(labels, "=", c("x","y","z","w")), title="Mapping Venn Diagram", ...)
      }
    }
    
    ## Return query list 
    return(qlist)
  }
  
  ## Plot 4-way circle mapping venn diagram
  if(type=="4map") {
    olDFdebug <- data.frame(x=c(4.8, 3.9, 5.7, 3.9, 5.7, 4.8, 4.8, 3.0, 6.5, 3.0, 6.5, 3.0, 6.5, 4.8, 4.8), y=c(5.2, 4.2, 4.2, 6.3, 6.3, 7.2, 3.2, 5.2, 5.2, 7.2, 7.2, 3.2, 3.2, 1.0, 0.4), count=paste("q", 1:15, sep=""), ...)
    symbols(x=c(4, 5.5, 4, 5.5), y = c(6, 6, 4.5, 4.5), circles=c(2, 2, 2, 2), xlim=c(0, 10), ylim=c(0, 10), inches=F, main="Mapping Venn Diagram", xlab="", ylab="",  xaxt="n", yaxt="n", bty="n", fg=lines, ...); 
    text(olDFdebug$x[1:13], olDFdebug$y[1:13], olDFdebug$count[1:13], col=tcol, ...); text(c(2.0, 7.5, 2.0, 7.5), c(8.3, 8.3, 2.0, 2.0), paste(labels, "=", c("x","y","z","w")), col=lcol, ...)
    text(c(3.8, 3.8), c(0.97, 0.36), c(paste("Only in ", labels[1], " & ", labels[4], ": ", olDFdebug$count[14], sep=""), paste("Only in ", labels[2], " & ", labels[3], ": ", olDFdebug$count[15], sep="")), col=tcol, ...)
  }
  
}
