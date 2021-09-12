### find regulations based on target etc.
### G.Fay
### last modded: 2019/08/09
#needs to go at top of script for FORTRAN code
args = commandArgs(trailingOnly=TRUE)

find.regs <- function(lookup, regs, target, recadj, type) {

  mround <- function(x,base){
    base*round(x/base)
  }


  #  lookup <- as_tibble(res)
#  regs <- c(4,15,150)
#  recadj <- 0.75
#  target <- recadj*31659
  regs[3] <- mround(regs[3],15)
  new_regs <- regs
  if (type == 2) {
    target <- dplyr::filter(lookup, Bag == regs[1],
                            MinLen == regs[2],
                            SeasonLen == regs[3])$land * recadj
  }
  lookup$dist <- lookup$land-target
  #if (recadj>1) lookup$dist <- lookup$dist*-1
  dir <- c(1,-1,1)
  if (recadj<=1) {
    if (recadj<=0.8) {
    tempdat <- dplyr::filter(lookup,
                             Bag==regs[1],
                             SeasonLen==regs[3],
                             MinLen>=regs[2],
                             dist <=0)
    if (nrow(tempdat)>0) {
     result <- tempdat[which.max(tempdat$dist),]
     if (abs(result$dist)<(0.02*target)) return(result)
    }
    }
    tempdat <- dplyr::filter(lookup,
                             Bag==regs[1],
                             SeasonLen<=regs[3],
                             MinLen>=regs[2],
                             dist <=0)
    if (nrow(tempdat)>0) {
      result <- tempdat[which.max(tempdat$dist),]
      if (abs(result$dist)<(0.02*target)) return(result)
    }
    tempdat <- dplyr::filter(lookup,
                             Bag<=regs[1],
                             SeasonLen<=regs[3],
                             MinLen>=regs[2],
                             dist <=0)
    if (nrow(tempdat)>0) {
      result <- tempdat[which.max(tempdat$dist),]
      return(result)
    }
    result <- NULL
    return(result)
  }
  if (recadj>1) {
    if (recadj>=1.2) {
      tempdat <- dplyr::filter(lookup,
                               Bag==regs[1],
                               SeasonLen==regs[3],
                               MinLen<=regs[2],
                               dist <=0)
      if (nrow(tempdat)>0) {
        result <- tempdat[which.max(tempdat$dist),]
        if (abs(result$dist)<(0.02*target)) return(result)
      }
    }
    tempdat <- dplyr::filter(lookup,
                             Bag==regs[1],
                             SeasonLen>=regs[3],
                             MinLen<=regs[2],
                             dist <=0)
    if (nrow(tempdat)>0) {
      result <- tempdat[which.max(tempdat$dist),]
      if (abs(result$dist)<(0.02*target)) return(result)
    }
    tempdat <- dplyr::filter(lookup,
                             Bag>=regs[1],
                             SeasonLen>=regs[3],
                             MinLen<=regs[2],
                             dist <=0)
    if (nrow(tempdat)>0) {
      result <- tempdat[which.max(tempdat$dist),]
      return(result)
    }
    result <- NULL
    return(result)
  }

}


print(args)
# generate realized catch based on new regulations

lookup <- tibble::as_tibble(readRDS(file = "lookup.rds"))
#  regs <- c(4,15,150)
#  recadj <- 0.75
#  target <- recadj*31659
regs <- as.numeric(args[1:3])
recadj <- as.numeric(args[4])
target <- as.numeric(args[5])
new_regs <- regs
pred <- target
dist <- -999999
flag <- 0
mgmt <- find.regs(lookup, regs, target, recadj, as.integer(args[6]))
print(mgmt)
if (length(mgmt)>0) {
  new_regs <- as.numeric(mgmt[1,1:3])
  pred <- mgmt$land
  flag <- 1
  dist <- mgmt$dist
}
write.table(c(new_regs,pred,flag,dist),file = "mgmt_regs.out",row.names = FALSE, col.names = FALSE)


print("done!")


lookup <- tibble::as_tibble(readRDS(file = "statelookup.rds"))
  regs <- c(4,15,150)
  recadj <- 0.75
  target <- recadj*31659
  state <- "MA"
# regs <- as.numeric(args[1:3])
# recadj <- as.numeric(args[4])
# target <- as.numeric(args[5])
# state <- as.character(args[7])
new_regs <- regs
pred <- target
dist <- -999999
flag <- 0
#mgmt <- find.regs(lookup, regs, target, recadj, as.integer(args[6]))
mgmt <- find.regs(filter(lookup, State==state), regs, target, recadj, as.integer(args[6]))
print(mgmt)
if (length(mgmt)>0) {
  new_regs <- as.numeric(mgmt[1,1:3])
  pred <- mgmt$land
  flag <- 1
  dist <- mgmt$dist
}
write.table(c(new_regs,pred,flag,dist),file = "mgmt_regs.out",row.names = FALSE, col.names = FALSE)


