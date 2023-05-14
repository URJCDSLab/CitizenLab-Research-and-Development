library(ClusterR)

opt_gmm = Optimal_Clusters_GMM(dfz, max_clusters = 10, criterion = "BIC",

                               dist_mode = "maha_dist", seed_mode = "random_subset",

                               km_iter = 10, em_iter = 10, var_floor = 1e-10,

                               plot_data = T)

res = external_validation(dietary_survey_IBS$class, pr$cluster_labels,

                          method = "adjusted_rand_index", summary_stats = T)

res

gmm <- GMM(dfz, 3, dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10,
          em_iter = 10, verbose = F)