install.packages("presize")
library("presize")

# anxiety about 17% of haemodialysis patients: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6572629/
# about 22%: https://www.ajkd.org/article/S0272-6386(19)30619-5/fulltext

# CALCUL RETENU POUR L'APRI
prec_prop(p = 0.17, 
          n = NULL, conf.width = 0.1, conf.level = 0.99, method = "exact")

#sample size for a proportion with exact confidence interval. 

#p      padj    n       conf.width   conf.level       lwr     upr
#0.17   NA 234.8403        0.1       0.95        0.1242729 0.2242729

prec_prop(p = 0.22, 
          n = NULL, conf.width = 0.1, conf.level = 0.95, method = "exact")

# p     padj   n     conf.width    conf.level       lwr       upr
# 0.22   NA 281.4165        0.1       0.95      0.1730013 0.2730013

prec_prop(p = 0.195, 
          n = NULL, conf.width = 0.1, conf.level = 0.95, method = "exact")
