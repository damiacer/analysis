library("presize")

prec_prop(p = 0.17, 
n = NULL, conf.width = 0.1, conf.level = 0.95, method = "exact")


#     sample size for a proportion with exact confidence interval. 
#
#     p padj        n conf.width conf.level       lwr       upr
#1 0.17   NA 234.8403        0.1       0.95 0.1242729 0.2242729
#
#NOTE: padj is the adjusted proportion, from which the ci is calculated.

#234.8403*0.10
#234.8403+23.48403=258.3243

#234.8403*0.20
#234.8403+46.96806=281.8084
