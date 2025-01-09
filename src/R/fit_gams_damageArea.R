pacman::p_load(arrow, data.table,tidyverse,patchwork,mgcv)


da0 <- arrow::read_parquet("data/parquets/damageArea13_evi2_gmd_prism.parquet")
sa0 <- arrow::read_parquet("data/parquets/surveyArea13_evi2_gmd_prism.parquet")

da0[damage_type=="Mortality"][survey_year==2023][percent_affected=="Very Severe (>50%)"]

da <- da[damage_type == "Mortality"] %>% mutate(mort = 1)
sa <- sa %>% mutate(mort = 0)
da <- da %>% select(names(sa))

dat1 <- rbindlist(list(da, sa))
dat1$evi2 %>% summary


dat1[,`:=`(id_pix = .GRP), by=.(x,y)]
dat1[,`:=`(year=year(time),
           month=month(time))]
dat1[,`:=`(evi2_ltmax = max(evi2,na.rm=T), 
         evi2_ltmin = min(evi2,na.rm=T), 
         evi2_u = mean(evi2,na.rm=T),
         evi2_sd = sd(evi2,na.rm=T)),
     by=.(x,y,month)]



da1 <- dat1[mort==1][month==8][year==survey_year]
vec_time <- da1$time %>% unique %>% sort

#!!!
fn_p <- approxfun(density(da1$evi2_ltmax)$x, 
       density(da1$evi2_ltmax)$y, 
       rule=2)

sa1 <- dat1[mort==0][evi2>0][time %in% vec_time][,`:=`(
  prob_match = fn_p(evi2_ltmax))][is.na(prob_match)==F]


# sample.int(n = 1e5, 
#            size = 1e5, 
#            replace = F, 
#            prob = runif(1e5))

# sa2 <- sa1[sample(.N, nrow(da1)*5, prob = prob_match)] # oddly slow

summary(sa2$evi2_ltmax)
summary(da1$evi2_ltmax)



mdat1 <- rbindlist(list(da1, sa2 %>% select(-prob_match)))

mdat1[sample(.N, 5e4)][order(-mort)] %>% 
  ggplot(aes(x,y,color=factor(mort)))+
  geom_point(size = 0.25, alpha = 0.5) +
  coord_sf() + 
  scale_color_viridis_d(end = 0.7) +
  theme_linedraw()

mdat1 %>% 
  ggplot(aes(fill = factor(mort), evi2_ltmax))+
  geom_density(alpha = 0.5)

m0 <- bam(mort ~ te(x,y), 
          # family = binomial,
          data = mdat1, 
          select = T, 
          discrete = T)
summary(m0)
plot(m0,scheme=2)


m1 <- bam(mort ~ 
            s(spei90d, k=5, bs='ts') +
            s(evi2_ltmin, evi2_ltmax, k=5) + 
            s(I(evi2/evi2_ltmax)) +
            te(x,y), 
          family = binomial(link = 'logit'),
          data = mdat1, 
          select = T, 
          discrete = T)
summary(m1)
plot(m1,scheme=2, pages =1)


m2 <- bam(mort ~ 
            # s(I((evi2-evi2_u)/evi2_ltmax), k=5 ) + 
            s(spei5y, spei1y, k=5, bs='ts') +
            # s(spei5y, k=5, bs='ts') + 
            # s(spei90d, k=5, bs='ts') +
            s(evi2_ltmin, evi2_ltmax, k=5),
          family = binomial(link = 'logit'),
          data = mdat1, 
          select = T, 
          discrete = T)
summary(m2)
plot(m2,scheme=2, pages =1)


