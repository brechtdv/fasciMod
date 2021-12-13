### Fasciola ABM
### last update: 13/12/2021

## required packages

## set up dataframes

n_hhd <- 20
n_man_hhd <- 5
n_ctl_hhd <- 5
n_man <- rpois(n_hhd, n_man_hhd)
n_ctl <- rpois(n_hhd, n_ctl_hhd)
nn_man <- sum(n_man)
nn_ctl <- sum(n_ctl)

df_hhd <-
data.frame(
  ID_HHD = seq(n_hhd),
  YN_MAN_ACCESS = 0,
  YN_CTL_ACCESS = 0,
  YN_SNAIL_PRESENT = 0,
  YN_SNAIL_INFECTED = 0,
  CT_SNAIL_TIME_SINCE_INFECTION = 0, # incubation period 10-12 days
                                     # duration of infectiousness
  YN_PLANT_PRESENT = 0,
  YN_PLANT_INFECTED = 0,
  CT_PLANT_TIME_SINCE_INFECTION = 0, # incubation period 5-7 wk
                                     # duration of infectiousness
  PR_PLANT_CONSUMED = 0) # prob consuming infected plants per month

df_man <-
data.frame(
  ID_HHD = factor(rep(seq(n_hhd), times = n_man), levels = seq(n_hhd)),
  #ID_MAN = seq(nn_man),
  #CT_AGE = round(runif(nn_man, 0, 80) * 12),
  YN_INFECTED = 0,
  CT_TIME_SINCE_INFECTION = 0)  # incubation period 8-12 wk

df_ctl <-
data.frame(
  ID_HHD = factor(rep(seq(n_hhd), times = n_ctl), levels = seq(n_hhd)),
  YN_INFECTED = 0,
  CT_TIME_SINCE_INFECTION = 0)

## random probabilities
pm2s <- 0.001
pc2s <- 0.002
ps2p <- 0.050
pp2m <- 0.100
pp2c <- 0.500

## set up transitions

## increment snail time since infection
df_hhd$CT_SNAIL_TIME_SINCE_INFECTION[df_hhd$YN_SNAIL_INFECTED == 1] <-
  df_hhd$CT_SNAIL_TIME_SINCE_INFECTION[df_hhd$YN_SNAIL_INFECTED == 1] + 1

## loss of infection of snails
df_hhd$YN_SNAIL_INFECTED[df_hhd$CT_SNAIL_TIME_SINCE_INFECTION > 2] <- 0
df_hhd$CT_SNAIL_TIME_SINCE_INFECTION[df_hhd$YN_SNAIL_INFECTED == 0] <- 0

## infection of snails
n_man_infected <-
  with(df_man, tapply(YN_INFECTED, ID_HHD, sum)) * df_hhd$YN_MAN_ACCESS
n_ctl_infected <-
  with(df_ctl, tapply(YN_INFECTED, ID_HHD, sum)) * df_hhd$YN_CTL_ACCESS

p2s <- 1 - (1 - (1 - (1 - pm2s) ^ n_man_infected)) *
           (1 - (1 - (1 - pc2s) ^ n_ctl_infected))

hhd_snail_susc <-
  which(df_hhd$YN_SNAIL_PRESENT == 0 & df_hhd$YN_SNAIL_INFECTED == 0) #!
hhd_snail_infect_new <-
  rbinom(length(p2s), 1, p2s)

df_hhd$YN_SNAIL_INFECTED[hhd_snail_susc] <-
  hhd_snail_infect_new[hhd_snail_susc]


## increment plant time since infection
df_hhd$CT_PLANT_TIME_SINCE_INFECTION[df_hhd$YN_PLANT_INFECTED == 1] <-
  df_hhd$CT_PLANT_TIME_SINCE_INFECTION[df_hhd$YN_PLANT_INFECTED == 1] + 1

## loss of infection of plants
df_hhd$YN_PLANT_INFECTED[df_hhd$CT_PLANT_TIME_SINCE_INFECTION > 2] <- 0
df_hhd$CT_PLANT_TIME_SINCE_INFECTION[df_hhd$YN_PLANT_INFECTED == 0] <- 0

## infection of plants
hhd_plant_susc <- which(df_hhd$YN_PLANT_PRESENT == 0)
hhd_plant_infect_new <-
  rbinom(nrow(df_hhd), 1, ps2p * df_hhd$YN_SNAIL_INFECTED)

df_hhd$YN_PLANT_INFECTED[hhd_plant_susc] <-
  hhd_plant_infect_new[hhd_plant_susc]


## infection of humans
man_susc <- which(df_man$YN_INFECTED == 0) #!

man_infect_new <-
  rbinom(nrow(df_man), 1,
    rep(pp2m * df_hhd$YN_PLANT_INFECTED * df_hhd$PR_PLANT_CONSUMED,
        times = n_man))
df_man$YN_INFECTED[man_susc] <-
  man_infect_new[man_susc]

## increment human time since infection
## loss of infection of humans

## infection of cattle
ctl_susc <- which(df_ctl$YN_INFECTED == 0) #!

ctl_infect_new <-
  rbinom(nrow(df_ctl), 1,
    rep(pp2c * df_hhd$YN_PLANT_INFECTED * df_hhd$YN_CTL_ACCESS,
        times = n_ctl))
df_ctl$YN_INFECTED[ctl_susc] <-
  ctl_infect_new[ctl_susc]


## increment cattle time since infection
## loss of infection of cattle
