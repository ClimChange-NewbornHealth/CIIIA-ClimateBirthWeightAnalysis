# 10 2 Stage ---

rm(list=(ls()))
options(scipen=999)
source("Code/0.2 Settings.R")

# Optimal use power computation 
options(future.globals.maxSize = 3000 * 1024^2)  
plan(multisession, workers = detectCores() - 4) # Parallelization

# Paths
data_path <- "Data/Output/"

## Prepare data  ----

data_temp <- "births_2011_2020_weeks_temp_analysis.RData"
load(paste0(data_path, data_temp))
glimpse(births_weeks_temp)

## 2Stage  ----

# El objetivo del diseño de dos etapas es manejar la complejidad de los datos
# de series temporales que provienen de múltiples ubicaciones. 
# Este enfoque permite estimar asociaciones específicas de cada ubicación y 
# luego combinar estas estimaciones para obtener una visión general más robusta y generalizable.

# Etapa 1: Estimación de asociaciones específicas de cada ubicación entre la exposición y el resultado de interés.

# Save results
clim_zone <- unique(births_weeks_temp$clim_zone)
clim_zone <- clim_zone[-c(4)] # Remove Cold steppe

datalist <- lapply(clim_zone, function(loc) births_weeks_temp[births_weeks_temp$clim_zone==loc,])
names(datalist) <- clim_zone
glimpse(datalist)
#rm(births_weeks_temp)

theta_hat <- se_theta_hat <- numeric(length(clim_zone))

for (loc in seq_along(clim_zone)) {
  # Filter location data 
  data_loc <- datalist[[loc]]
  # Estimate the model
  gam_model <- gam(tbw ~ temp_mean + sex + 
                     age_mom + factor(educ_mom) + factor(job_mom) +
                     age_dad + factor(educ_dad) + factor(job_dad) +
                     s(year_week1) + s(month_week1),  
                   data = data_loc,
                   family=gaussian())
  # Save results
  theta_hat[loc] <- coef(gam_model)["temp_mean"]
  se_theta_hat[loc] <- sqrt(vcov(gam_model)["temp_mean", "temp_mean"])
}

# See results
theta_hat
se_theta_hat 
uni <- rma(yi=theta_hat, sei=se_theta_hat, slab=clim_zone, measure="GEN")
uni
ci.lin(uni)

forest(uni, refline=0, pch=23, bg=2, col=1,
       main="Mean temperature and tBW")


uniblup <- blup(uni)
plot(theta_hat, cex=max(se_theta_hat)/se_theta_hat*2, main="First-stage and BLUPs",
     xaxt="n", xlab="Region")
axis(1, at=c(1:6), labels=clim_zone, cex.axis=0.8)
points(uniblup$pred, cex=1/(uniblup$se/max(se_theta_hat))*2, col=2)
abline(h=coef(uni))
legend("topleft",c("First-stage","BLUPs"), col=1:2, pch=1)



# Etapa 2: Combinar las estimaciones obtenidas en la primera etapa utilizando métodos meta-analíticos para obtener una estimación conjunta que represente el efecto global.

coef <- matrix(NA,length(datalist), 4, dimnames=list(clim_zone, paste0("b",seq(4))))
S <- vector("list",length(datalist))
names(S) <- clim_zone

bound <- rowMeans(sapply(datalist, function(x) range(x$temp_mean))) #of avg temp distribution 
varknots <- equalknots(bound, fun="bs", degree=2, df=4) #knots for exposure, 
lagknots <- equalknots(x = c(1, 37), nk = 2, fun = "ns")
argvar <- list(fun="bs", degree=2, knots=varknots, Bound=bound) #for exposure variable, bs, quadratic
arglag <- list(fun="ns", knots=lagknots) #lag, natural spline with 2 internal 

for(i in seq(datalist)) {
  cb <- crossbasis(datalist[[i]]$temp_mean, argvar=argvar, arglag=arglag)
  m <- gam(tbw ~ cb + sex + 
             age_group_mom + educ_group_mom + job_group_mom +
             age_group_dad + educ_group_dad + job_group_dad +
             s(year_week1) + s(month_week1) + sovi, 
           na.action = na.exclude,
           family=gaussian(),
           data=datalist[[i]])
  cr <- crossreduce(cb, m, cen=17) #
  coef[i,] <- coef(cr)
  S[[i]] <- vcov(cr)
}

coef
S

mix <- mixmeta(coef~1, S, method="reml")
print(summary(mix), digits=3)
mixblup <- mixmeta::blup(mix, vcov=TRUE)

xvar <- seq(bound[1], bound[2], by=0.1)
bvar <- do.call("onebasis", c(list(x=xvar), argvar))

predpool <- crosspred(bvar, coef=coef(mix), vcov=vcov(mix),
                      by=0.1, cen=17)
predreg <- lapply(seq(nrow(coef)),function(i) crosspred(bvar, coef=coef[i,],
                                                        vcov=S[[i]],  cen=17))
predblup <- lapply(seq(nrow(coef)),function(i) crosspred(bvar,
                                                         coef=mixblup[[i]]$blup, vcov=mixblup[[i]]$vcov,  cen=17))

layout(t(1:2))
plot(predpool, type="l", ci="n", ylab="tBW", ylim=c(-60,60), lwd=2,
     xlab="Temperature (C)", main="Pooled and first-stage")
for(i in seq(datalist)) lines(predreg[[i]], col=alpha(4,0.3))
plot(predpool, type="l", ci="n", ylab="tBW",  ylim=c(-60,60), lwd=2,
     xlab="Temperature (C)", main="Pooled and BLUPs")
for(i in seq(datalist)) lines(predblup[[i]], col=alpha(2,0.3))
















