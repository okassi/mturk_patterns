rm(list=ls())
gc()

library(data.table)
library(lfe)
library(stringr)
library(stargazer)

  # ##                                         ## #
 # # Table 2: pooled cross sectional analyses  # #
# ##                                         ## #

d1 <- fread('data_withoutOutliers_withTurkopticon.csv') 
d1 <- data.frame(sapply(d1, function(x) str_replace(x,',','.') ))
# fix numeric columns
for( j in c('comm','speed','pay','fair','fast','reviews','tos_flags','hprice','price','time','max','maxlog','meanspeed','varspeed','ldesc','ltitle') ){
	d1[, names(d1) == j] <- as.numeric(as.character(d1[, names(d1) == j]))
}

d1_model1.n <- lm(log(speed) ~ (log(max))  + (log(ltitle)) + (log(ldesc)), data=d1)
d1_model2.n <- lm(log(speed) ~ (log(price)) + (log(ltitle)) + (log(ldesc)), data=d1)
d1_model3.n <- lm(log(speed) ~ (log(max)) + (log(price)) + (log(ltitle)) + (log(ldesc)), data=d1)
d1_model4.n <- lm(log(speed) ~ (log(max)) + (log(price)) + (log(ltitle)) + (log(ldesc)) + comm + pay + fair + fast + log(reviews), data=d1, x=T, y=T)

stargazer(d1_model1.n, d1_model2.n, d1_model3.n, d1_model4.n, out='./quant_study1_results.tex', type='latex',align = FALSE, 
	               omit.stat = c("aic", "f", "bic", "ser", 'adj.rsq'),
                 dep.var.labels='Throughput (log)',
                 covariate.labels = c('MaxHitsAvailable (log)',
                                      'Reward (log)',
                                      'TitleLength (log)',
                                      'DesrcLength (log)',
                                      'CommunicationScore',
                                      'PaymentScore',
                                      'FairnessScore',
                                      'QuicknessScore',
                                      'Reviews (log)'))


  # ##                                   ## #
 # #  Table 3: Panel data specifications # #
# ##                                   ## #


d2 <- fread('multi-price-best-part2.csv') 
d2$id <- paste(d2$group, d2$qualif, d2$title, d2$description, sep='_')

d2_model1.n <- felm(log(speed) ~ (log(max)) | id  | 0 | id, data=d2[d2$speed > 0 & d2$price > 0,] )
d2_model2.n <- felm(log(speed) ~ (log(price)) | id | 0 | id, data=d2[d2$speed > 0 & d2$price > 0,] )
d2_model3.n <- felm(log(speed) ~ (log(max)) + (log(price))  | id | 0 | id, data=d2[d2$speed > 0 & d2$price > 0,] )

stargazer(d2_model1.n, d2_model2.n, d2_model3.n,  out='./quant_study2_results.tex', type='latex',align = FALSE, 
	               omit.stat = c("aic", "f", "bic", "ser", 'adj.rsq'),  covariate.labels=c('MaxHitsAvailable (log)','Reward (log)'),
                 add.lines=list(c("HIT Group FE\'s", 'Yes', 'Yes')),
                 dep.var.labels='Throughput' 
                 )                


d2_model_split1 <- felm(log(speed) ~ log(max) + log(price) | id | 0 | id, data=d2[d2$speed > 0 & d2$price > 0 & d2$max < 1800,] )
d2_model_split2 <- felm(log(speed) ~ log(max) + log(price) | id | 0 | id, data=d2[d2$speed > 0 & d2$price > 0 & d2$max >= 1800,] )

stargazer(d2_model_split1, d2_model_split2,  out='./quant_study2_results_split.tex', type='text',align = FALSE, 
                omit.stat = c("aic", "f", "bic", "ser", 'adj.rsq'),  covariate.labels=c('MaxHitsAvailable (log)','Reward (log)'),
                add.lines=list(c("HIT Group FE\'s", 'Yes', 'Yes'), c('Data filter', 'MaxHitsAvailable < mean', 'MaxHitsAvailable >= mean')),
                dep.var.labels='Throughput (log)'
                )


stargazer(d2_model1, d2_model2, d2_model3,  out='./study2_results.txt', type='text')

