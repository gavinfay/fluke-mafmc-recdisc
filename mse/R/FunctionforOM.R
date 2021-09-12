#####################################################
#####################################################
##    Function for calling in gam object           ##
##      and predicting with it                     ##
##      J. McNamee   2/17/19                       ##
#####################################################
#####################################################
args = commandArgs(trailingOnly=TRUE)

#Switch argument is: 1 = mean prediction, 2 = observation sampled from uncertainty
#library(mgcv)
pred.land <- function(State, MinLen, Bag, SeasonLen, RHL, Year, switch) {

  #profvis::profvis({
    #
  #load("C:/Z Drive stuff/ASMFC/TCs/Fluke Scup BCB info/Summer Flounder/2019/Fluke_MSE/gam_obj.RData")
  #load("data/gam_obj.RData")
  load("~/research/fluke/data/gam_obj.RData")
  gam_obj <- mdl.gam
  len = seq(10,30,0.5)
  seas = 61 - (306-SeasonLen)
###Dataframe
  dat2 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(3, length(len)), SeasonLen = rep(61, length(len)), RHL = rep(RHL, length(len)))
  dat3 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(4, length(len)), SeasonLen = rep(62, length(len)), RHL = rep(RHL, length(len)))
  dat4 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(5, length(len)), SeasonLen = rep(61, length(len)), RHL = rep(RHL, length(len)))
  dat5 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(6, length(len)), SeasonLen = rep(seas, length(len)), RHL = rep(RHL, length(len)))

  dat.all = data.frame(rbind(dat2, dat3, dat4, dat5))   #add this if wave 2 added in to prediction: dat1,

###Season conditional statements
  if (seas<0) {
    seas = 61-(seas*-1)
    dat4 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(5, length(len)), SeasonLen = rep(seas, length(len)), RHL = rep(RHL, length(len)))

    dat.all = data.frame(rbind(dat2, dat3, dat4))   #add this if wave 2 added in to prediction: dat1,

    }  #end of season length condition 1

  if (SeasonLen>245) {
    dat1 = data.frame(State = rep(State, length(len)), Year = as.factor(rep(Year, length(len))), Length = len, MinLen = rep(MinLen, length(len)), Bag = rep(Bag, length(len)), Wave = rep(2, length(len)), SeasonLen = rep(61, length(len)), RHL = rep(RHL, length(len)))

    dat.all = data.frame(rbind(dat1, dat2, dat3, dat4, dat5))

  }  #end of season length condition 2


###Output

  if (switch ==1) {

    land = exp(mgcv::predict.gam(gam_obj, newdata = dat.all))
    output = cbind(dat.all, land)
    return(output)

  }  #end of switch condition 1

  if (switch ==2) {

    ##Bayesian covariance matrix of the model coefficients
    Vb <- mgcv::vcov.gam(gam_obj)

    #Number of draws from posterior
    #N <- 1000

    #Container
    land = rep(0,length(dat.all[,1]))
    output = cbind(dat.all, land)

    #Now we calculate fhat(x)-f(x), which is evaluated at the reg specs in dat.all, by row
    #for (i in 1:length(dat.all[,1])) {
      Cg <- mgcv::predict.gam(gam_obj, dat.all, type = "lpmatrix")
      #evaluates the basis function

      ##N samps frm bias in mdl coeff, follows multivariate nrml dist w/ mean vec 0 and covar matrix Vb
      sims <- as.matrix(mgcv::rmvn(1, mu = coef(gam_obj), V = Vb),nrow=1)
      #sims <- mgcv::rmvn(N, mu = coef(gam_obj), V = Vb)

      #computes the deviations between the fitted and true parameters
      #fits <- Cg %*% t(sims)
      fits <- Cg %*% sims

      ##Randomly sample 1 point from fits
      #nrnd <- 1  #number of random samples
      #land.1 <- exp(sample(fits, nrnd))
      output$land = exp(fits) #land.1

#    }  #end of for loop
    return(output)

  }  #end of switch condition 2
#})

}  #end of function


#####################################################
#     Test                                          #
#####################################################

#pred.land("NJ", 18, 3, 246, 3.77, 2017, 2)
profvis::profvis({x = pred.land("NJ", 18, 3, 246, 3.77, 2017, 2)
print(sum(x$land))
})

#####################################################


# x <- pred.land("NJ", 18, 3, 246, 3.77, 2017, 2)

#args(pred.land)
#function (State, MinLen, Bag, SeasonLen, RHL, Year, switch)

State <- "NJ"
MinLen <- 18
Bag <- 3
SeasonLen <- 246
RHL <- 3.77
Year <- 2017
switch <- 2

#vector gam
#discard model, going to do a two model approach and a vector gam approach
#call Gary about bsb assess
print(args)
#x <- pred.land("NJ", as.numeric(args[2]), 3, 246, 3.77, 2017, 2)
x <- pred.land(args[1], as.numeric(args[2]), 3, 246, 3.77, 2017, 2)
#write(sum(x),file="recland.out")
print(sum(x$land))
write(sum(x$land),file="recland.out")
write.table(t(tapply(x$land,x$Length,sum,na.rm=TRUE)),file="recland.out",
            append = TRUE, col.names = FALSE, row.names = FALSE)
print("done!")


# head(dat)
# dat_mung <- filter(dat, State == 'NJ', Year == 2017) %>%
#   group_by(Length) %>% summarise(n = sum(x))
#
# x %>%
#   group_by(Length) %>%
#   summarize(n = sum(land)) %>%
# ggplot() +
#   geom_line(aes(x=Length,y=n)) +
#   geom_point(data = dat_mung, aes(x=Length,y=n))
#
#

#
# bob <- NULL
# for (i in 1:100) bob <- rbind(bob,rnorm(205,fitted,sqrt(gam_obj$sig2)))
# bob2 <- as.data.frame(bob) %>%
#   gather(key = ipt, value = yhat, 1:205)
# bob2 <- mutate(bob2, ipt = as.numeric(gsub("V","",ipt)))
# bob2 <- mutate(bob2, fit = fitted[bob2$ipt])
# ggplot(bob2) +
#   geom_point(aes(x=ipt,y=yhat),col="gray",alpha=0.5) +
#   geom_line(aes(x=ipt,y=fit),col="red")
#
# fitted2 <- mgcv::predict.gam(gam_obj)
# bob <- NULL
# for (i in 1:100) bob <- rbind(bob,rnorm(length(fitted2),fitted2,sqrt(gam_obj$sig2)))
# bob2 <- as.data.frame(bob) %>%
#   gather(key = ipt, value = yhat, 1:ncol(bob))
# bob2 <- mutate(bob2, ipt = as.numeric(gsub("V","",ipt)))
# bob2 <- mutate(bob2, fit = fitted2[bob2$ipt])
# ggplot(bob2) +
#   geom_boxplot(aes(y=yhat,group=ipt)) +
#   coord_flip()
#   geom_point(aes(x=ipt,y=yhat),col="gray",alpha=0.5) +
#   #geom_line(aes(x=ipt,y=fit),col="red")
#   geom_point(aes(x=ipt,y=fit),col="red")
#
#
#

# dat$log_x <- log(dat$x)
# gam2 <- gam(formula = log_x ~ s(Length, MinLen, k=40) + State +
#       RHL + factor(Wave) + s(SeasonLen, k=3) + s(Bag, k=3) - 1,
#     data = dat)
#
# gam2 <- gam(formula = log_x ~ s(Length, MinLen, k=40) +
#               RHL + factor(Wave) + s(SeasonLen, k=3) + s(Bag, k=3) - 1,
#             data = filter(dat, State == "NJ"))
#
# dat2 <- dat
# dat2$Length <- scale(dat2$Length)
# dat2$MinLen <- scale(dat2$MinLen)
# dat2$SeasonLen <- scale(dat2$SeasonLen)
# dat2$log_x <- scale(dat2$log_x)
# gam2 <- gam(formula = log_x ~ s(Length, MinLen, k=100) + State +
#               RHL + factor(Wave) + s(SeasonLen, k=3) + s(Bag, k=3) - 1,
#             data = dat2)
