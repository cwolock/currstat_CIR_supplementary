library(tidyverse)
library(extrafont)
library(dichromat)
pal <- function(col, ...) image(seq_along(col), 1, matrix(seq_along(col), ncol = 1), col = col, axes = FALSE, ...)
blue_palette <- colorschemes$LightBluetoDarkBlue.7[c(3,5,7)]
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/")
dat <- readRDS("CIR_stability_021225")

messed_up <- dat %>% filter(is.na(cdf_estimate) | is.na(cil) | is.na(ciu))

dat <- dat %>% filter(!(is.na(cdf_estimate) | is.na(cil) | is.na(ciu)))

dat <- dat %>% mutate(err = cdf_estimate - truth,
                      cov = ifelse(cil <= truth & ciu >= truth, 1, 0),
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(method = factor(method, levels = c("glm_parametric", "glm_HAL", "gam_parametric", "gam_HAL",
                                            "xgboost_parametric", "xgboost_HAL", "SL4_parametric", "SL4_HAL",
                                            "known_known", "known_parametric", "known_HAL",
                                            "SL4_known", "glm_known", "gam_known", "xgboost_known"),
                         labels = c("GLM_parametric", "GLM_haldensify", "GAM_parametric", "GAM_haldensify",
                                    "xgboost_parametric", "xgboost_haldensify", "Super Learner_parametric", "Super Learner_haldensify",
                                    "known_known", "known_parametric", "known_haldensify",
                                    "Super Learner_known", "GLM_known", "GAM_known", "xgboost_known")),
         n = factor(n))

summ <- dat %>% group_by(n, method, truth, interaction) %>%
  summarize(nreps = n(),
            sample_size = mean(n_actual),
            bias = mean(err),
            variance = var(cdf_estimate),
            coverage = mean(cov),
            runtime = mean(runtime),
            MSE_mu = mean(MSE_mu),
            MSE_g = mean(MSE_g)) %>%
  filter(nreps > 100)

timeaverage_summ <- summ %>% group_by(n, method, interaction) %>%
  summarize(bias = mean(abs(bias)),
            coverage = mean(coverage))
timeaverage_summ$mu_nuisance <- stringr::str_split_fixed(timeaverage_summ$method, "_", 2)[,1]
timeaverage_summ$g_nuisance <- stringr::str_split_fixed(timeaverage_summ$method, "_", 2)[,2]
summ$mu_nuisance <- stringr::str_split_fixed(summ$method, "_", 2)[,1]
summ$g_nuisance <- stringr::str_split_fixed(summ$method, "_", 2)[,2]

summ <- summ %>% mutate(mu_nuisance = factor(mu_nuisance, levels = c("known", "GLM", "GAM", "xgboost", "Super Learner"),
                                             labels = c("true form (known)", "GLM", "GAM", "xgboost", "Super Learner")),
                        g_nuisance = factor(g_nuisance, levels = c("known", "parametric", "haldensify"),
                                            labels = c("true form (known)", "parametric", "haldensify")))

p_bias <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = bias, linetype = n, color = n)) +
  facet_grid(mu_nuisance ~ g_nuisance) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle("Conditional density estimator") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  scale_y_continuous(breaks = c(-0.01, 0, 0.01, 0.02, 0.03),
                     labels = c("-0.01", "0.00", "0.01", "0.02", "0.03"),
                     limits = c(-0.011, 0.031),
                     sec.axis = sec_axis(~ . , name = "Outcome regression estimator",
                                         labels = NULL, breaks = NULL)) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman", size = 12),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank())

p_variance <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = variance, linetype = n, color = n)) +
  facet_grid(mu_nuisance ~ g_nuisance) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Variance") +
  xlab("Symptom resolution probability") +
  ggtitle("Conditional density estimator") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  scale_y_continuous(breaks = c(0, 0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007),
                     labels = c("0.000", "0.001", "0.002", "0.003", "0.004", "0.005", "0.006", "0.007"),
                     limits = c(-0.0001, 0.0071),
                     sec.axis = sec_axis(~ . , name = "Outcome regression estimator",
                                         labels = NULL, breaks = NULL)) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman", size = 12),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank())

dat <- readRDS("CIR_stability_031025_topperfomers.rds")
dat <- dat %>% mutate(err = cdf_estimate - truth,
                      cov = ifelse(cil <= truth & ciu >= truth, 1, 0),
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(method = factor(method, levels = c("glm_parametric", "glm_HAL", "gam_parametric", "gam_HAL",
                                            "xgboost_parametric", "xgboost_HAL", "SL4_parametric", "SL4_HAL",
                                            "known_known", "known_parametric", "known_HAL",
                                            "SL4_known", "glm_known", "gam_known", "xgboost_known"),
                         labels = c("GLM_parametric", "GLM_haldensify", "GAM_parametric", "GAM_haldensify",
                                    "xgboost_parametric", "xgboost_haldensify", "Super Learner_parametric", "Super Learner_haldensify",
                                    "known_known", "known_parametric", "known_haldensify",
                                    "Super Learner_known", "GLM_known", "GAM_known", "xgboost_known")),
         n = factor(n))

# look at stability
dat <- dat %>% group_by(n, method, truth, runtime, interaction) %>%
  summarize(bias = mean(err)) %>%
  filter(method == "known_known" | method == "Super Learner_haldensify" | method == "xgboost_haldensify")
dat <- dat %>% pivot_wider(names_from = method, values_from = bias) %>%
  mutate(stability1 = abs(known_known - `Super Learner_haldensify`),
         stability2 = abs(xgboost_haldensify - `Super Learner_haldensify`))
summ <- dat %>% group_by(n, truth,interaction) %>%
  summarize(nreps = n(),
            median_1 = median(stability1, na.rm = TRUE),
            q95_1 = quantile(stability1, probs = 0.95, na.rm = TRUE),
            q5_1 = quantile(stability1, probs = 0.05, na.rm = TRUE),
            median_2 = median(stability2, na.rm = TRUE),
            q95_2 = quantile(stability2, probs = 0.95, na.rm = TRUE),
            q5_2 = quantile(stability2, probs = 0.05, na.rm = TRUE)) %>%
  filter(nreps > 100)
blah <- summ %>% filter(n == 5000)

p_stability_1 <- summ %>%
  ggplot(aes(x = truth)) +
  geom_line(aes(y = median_1, color = "median")) +
  geom_line(aes(y = q95_1, color="95th percentile")) +
  geom_line(aes(y = q5_1, color = "5th percentile")) +
  facet_wrap(~ n, nrow = 1) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylim(c(0, 0.06)) +
  ylab("Absolute difference in point estimates") +
  xlab("Symptom resolution probability") +
  ggtitle("Sample size") +
  scale_color_manual("", breaks = c("5th percentile", "median", "95th percentile"),
                     values = c(blue_palette[1], blue_palette[2], blue_palette[3])) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman"),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank())

p_stability_2 <- summ %>%
  ggplot(aes(x = truth)) +
  geom_line(aes(y = median_2, color = "median")) +
  geom_line(aes(y = q95_2, color="95th percentile")) +
  geom_line(aes(y = q5_2, color = "5th percentile")) +
  facet_wrap(~ n, nrow = 1) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Absolute difference in point estimates") +
  xlab("Symptom resolution probability") +
  ggtitle("Sample size") +
  ylim(c(0, 0.06)) +
  scale_color_manual("", breaks = c("5th percentile", "median", "95th percentile"),
                     values = c(blue_palette[1], blue_palette[2], blue_palette[3])) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman"),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.grid.minor.y = element_blank())


ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/stability_bias_plot_021525.pdf",
       plot = p_bias,
       device = "pdf",
       width = 7,
       height = 7,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/stability_var_plot_021525.pdf",
       plot = p_variance,
       device = "pdf",
       width = 7,
       height = 7,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/stability_plot_021525_known.pdf",
       plot = p_stability_1,
       device = "pdf",
       width = 7,
       height = 4,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/stability_plot_021525_ML.pdf",
       plot = p_stability_2,
       device = "pdf",
       width = 7,
       height = 4,
       dpi = 300,
       units = "in")


