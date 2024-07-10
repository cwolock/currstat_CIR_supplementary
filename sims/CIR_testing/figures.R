library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/")
# dat <- readRDS("CIR_testing_bigN.rds")
dat <- readRDS("CIR_testing_srongerconfounding.rds")
dat <- dat %>% mutate(err = cdf_estimate - truth,
                      cov = ifelse(cil <= truth & ciu >= truth, 1, 0),
                      width = ciu - cil,
                      sq_err = (cdf_estimate - truth)^2,
                      scaled_err = (n^(1/3))*(cdf_estimate - truth),
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(method = factor(method, levels = c("cc", "extended"),
                         labels = c("Complete case CIR", "Extended CIR")),
         n = factor(n),
         missing_bound = factor(missing_bound,
                                levels = c(-100, 2.1, 1.8, 1.65),
                                labels = c("Scenario 0", "Scenario 1", "Scenario 2", "Scenario 3")),
         eval_upper_bound = factor(eval_upper_bound)) %>%
  filter(missing_bound != "Scenario 0")

summ <- dat %>% group_by(n, method, truth, missing_bound, eval_upper_bound) %>%
  summarize(nreps = n(),
            sample_size = mean(n_actual),
            bias = mean(err),
            scaled_bias = mean(scaled_err),
            bias_mc_se = sqrt(mean((err - bias)^2)/nreps),
            scaled_bias_mc_se = sqrt(mean((scaled_err - scaled_bias)^2)/nreps),
            coverage = mean(cov),
            coverage_mc_se = sqrt(coverage*(1-coverage)/nreps),
            width = mean(width),
            mse = mean(sq_err)) %>%
  mutate(bias_mc_cil = bias - 1.96 * bias_mc_se,
         bias_mc_ciu = bias + 1.96 * bias_mc_se,
         scaled_bias_mc_cil = scaled_bias - 1.96 * scaled_bias_mc_se,
         scaled_bias_mc_ciu = scaled_bias + 1.96 * scaled_bias_mc_se,
         coverage_mc_cil = coverage - 1.96 * coverage_mc_se,
         coverage_mc_ciu = coverage + 1.96 * coverage_mc_se) %>%
  filter(nreps > 100)

timeaverage_summ <- summ %>% group_by(n, method, missing_bound, eval_upper_bound) %>%
  summarize(bias = mean(abs(bias)),
            coverage = mean(coverage))

p_timeaverage_bias <- timeaverage_summ %>% 
  ggplot(aes(x = n, group = method)) + 
  geom_line(aes(y = bias, linetype = method, color = method)) + 
  facet_wrap(~ missing_bound, nrow = 1) + 
  theme_bw() +  
  ylab("Absolute bias") +
  xlab("Sample size") + 
  ggtitle("Nonresponse scenario") + 
  scale_linetype_manual(name = "Method", values = c("dashed", "solid")) +
  scale_color_discrete(name = "Method") +
  ylim(c(0, 0.013)) + 
  geom_hline(yintercept = 0, color = "black") + 
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
                       strip.background = element_blank(),
                       legend.position = "bottom",
                       axis.text = element_text(family = "Times New Roman"),
                       axis.title = element_text(family = "Times New Roman"),
                       strip.text = element_text(family = "Times New Roman"),
                       legend.title = element_text(family = "Times New Roman"),
                       legend.text = element_text(family = "Times New Roman"))
p_timeaverage_cov <- timeaverage_summ %>% 
  ggplot(aes(x = n, group = method)) + 
  geom_line(aes(y = coverage, linetype = method, color = method)) + 
  facet_wrap(~ missing_bound, nrow = 1) + 
  theme_bw() +  
  ylab("Coverage") +
  xlab("Sample size") + 
  ggtitle("Nonresponse scenario") + 
  geom_hline(yintercept = 0.95, color = "black") +
  scale_linetype_manual(name = "Method", values = c("dashed", "solid")) +
  scale_color_discrete(name = "Method") +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman"),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"))

p_bias <- summ %>% 
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = bias, linetype = n, color = n)) +
  facet_grid(method~missing_bound) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle("Nonresponse scenario") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  scale_y_continuous(breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
                     labels = c("-0.03", "-0.02", "-0.01", "0.00", "0.01", "0.02", "0.03"),
                     limits = c(-0.03, 0.03),
                     sec.axis = sec_axis(~ . , name = "Method",
                                         labels = NULL, breaks = NULL)) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman"),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"))

p_coverage <- summ %>% 
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = coverage, linetype = n, color = n)) +
  facet_grid(method~missing_bound) +
  theme_bw() +
  ylab("Coverage") +
  xlab("Symptom resolution probability") +
  ggtitle("Nonresponse scenario") +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  geom_hline(yintercept = 0.95, color = "black") +
  scale_y_continuous(breaks = c(0.7, 0.8, 0.9, 1.0),
                     labels = c("0.7", "0.8", "0.9", "1.0"),
                     limits = c(0.7, 1),
                     sec.axis = sec_axis(~ . , name = "Method",
                                         labels = NULL, breaks = NULL)) +
  theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
         strip.background = element_blank(),
         legend.position = "bottom",
         axis.text = element_text(family = "Times New Roman"),
         axis.title = element_text(family = "Times New Roman"),
         strip.text = element_text(family = "Times New Roman"),
         legend.title = element_text(family = "Times New Roman"),
         legend.text = element_text(family = "Times New Roman"))

# p_scaled_bias <- summ %>% ggplot(aes(x = truth, group = n)) +
#   geom_line(aes(y = scaled_bias, linetype = n, color = n)) +
#   facet_grid(method~nonresponse) +
#   theme_bw() +
#   ylab(expression(paste(n^{1/3}, "x bias"))) +
#   xlab("Symptom resolution probability") +
#   ggtitle("Missingness mechanism") +
#   scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
#   scale_color_discrete(name = "Sample size") +
#   geom_hline(yintercept = 0, color = "black") +
#   scale_y_continuous(breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3),
#                      labels = c("-0.3", "-0.2", "-0.1", "0.0", "0.1", "0.2", "0.3"),
#                      limits = c(-0.3, 0.3),
#                      sec.axis = sec_axis(~ . , name = "Method",
#                                          labels = NULL, breaks = NULL)) +
#   theme( plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
#          strip.background = element_blank(),
#          legend.position = "bottom",
#          axis.text = element_text(family = "Times New Roman"),
#          axis.title = element_text(family = "Times New Roman"),
#          strip.text = element_text(family = "Times New Roman"),
#          legend.title = element_text(family = "Times New Roman"),
#          legend.text = element_text(family = "Times New Roman"))


ggsave(filename = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/bias_plot_061724.pdf",
       plot = p_bias,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/coverage_plot_061724.pdf",
       plot = p_coverage,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/timeave_bias_plot_061724.pdf",
       plot = p_timeaverage_bias,
       device = "pdf",
       width = 7,
       height = 3.5,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/DISSERTATION/chu_lab/susan/code/CIR_testing/timeave_coverage_plot_061724.pdf",
       plot = p_timeaverage_cov,
       device = "pdf",
       width = 7,
       height = 3.5,
       dpi = 300,
       units = "in")
