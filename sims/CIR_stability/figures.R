library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_stability/")
dat <- readRDS("CIR_stability_021025_multi_lessYint_bigN_moreHALbins.rds")

messed_up <- dat %>% filter(is.na(cdf_estimate) | is.na(cil) | is.na(ciu))

dat <- dat %>% filter(!(is.na(cdf_estimate) | is.na(cil) | is.na(ciu)))

dat <- dat %>% mutate(err = cdf_estimate - truth,
                      cov = ifelse(cil <= truth & ciu >= truth, 1, 0),
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(#method = factor(method, levels = c("cc", "extended", "npmle", "npmle_survival", "extended_smalllib"),
        #                 labels = c("Complete case CIR", "Extended CIR", "NPMLE (manual)", "NPMLE", "Extended (glm)")),
         n = factor(n))

summ <- dat %>% group_by(n, method, truth) %>%
  summarize(nreps = n(),
            sample_size = mean(n_actual),
            bias = mean(err),
            variance = var(cdf_estimate),
            coverage = mean(cov),
            runtime = mean(runtime),
            MSE_mu = mean(MSE_mu),
            MSE_g = mean(MSE_g)) %>%
  filter(nreps > 100)


p_bias <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = bias, linetype = n, color = n)) +
  facet_wrap(~method) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle("Nonresponse scenario") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  # scale_y_continuous(breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
                     # labels = c("-0.03", "-0.02", "-0.01", "0.00", "0.01", "0.02", "0.03"),
                     # limits = c(-0.03, 0.03),
                     # sec.axis = sec_axis(~ . , name = "Method",
                                         # labels = NULL, breaks = NULL)) +
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

p_variance <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = variance, linetype = n, color = n)) +
  facet_wrap(~method) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle("Nonresponse scenario") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  # scale_y_continuous(breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
  # labels = c("-0.03", "-0.02", "-0.01", "0.00", "0.01", "0.02", "0.03"),
  # limits = c(-0.03, 0.03),
  # sec.axis = sec_axis(~ . , name = "Method",
  # labels = NULL, breaks = NULL)) +
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

# look at stability
dat <- dat %>% group_by(n, method, truth, runtime) %>%
  summarize(bias = mean(err)) %>%
  filter(method == "xgboost_HAL" | method == "SL4_HAL")
dat <- dat %>% pivot_wider(names_from = method, values_from = bias) %>%
  mutate(stability = abs(xgboost_HAL - SL4_HAL))
summ <- dat %>% group_by(n, truth) %>%
  summarize(nreps = n(),
            median = median(stability, na.rm = TRUE),
            q95 = quantile(stability, probs = 0.95, na.rm = TRUE),
            q5 = quantile(stability, probs = 0.05, na.rm = TRUE)) %>%
  filter(nreps > 100)

p_stability <- summ %>%
  ggplot(aes(x = truth)) +
  geom_line(aes(y = median, color = "red")) +
  geom_line(aes(y = q95), color= "blue") +
  geom_line(aes(y = q5), color = "black") +
  facet_wrap(~ n) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle("Nonresponse scenario") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  # scale_y_continuous(breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
  # labels = c("-0.03", "-0.02", "-0.01", "0.00", "0.01", "0.02", "0.03"),
  # limits = c(-0.03, 0.03),
  # sec.axis = sec_axis(~ . , name = "Method",
  # labels = NULL, breaks = NULL)) +
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


ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_testing/bias_plot_013025.pdf",
       plot = p_bias,
       device = "pdf",
       width = 7,
       height = 6,
       dpi = 300,
       units = "in")

