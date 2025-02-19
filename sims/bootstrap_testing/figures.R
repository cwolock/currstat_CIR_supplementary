library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/chu_lab/susan/code/bootstrap_testing/")
# dat <- readRDS("icenReg_bootstrap_011324.rds")
dat <- readRDS("icenReg_bootstrap.rds")
dat <- dat %>% pivot_longer(cols = c(x1_est, x1_se, x1_025, x1_975,
                                     x2_est, x2_se, x2_025, x2_975,
                                     x3_est, x3_se, x3_025, x3_975),
                            names_to = c("variable", "statistic"),
                            names_sep = "_",
                            values_to = "estimate")
dat$id <- rep(1:(nrow(dat)/4), each = 4)
dat <- dat %>% pivot_wider(names_from = "statistic", values_from = "estimate")
dat <- dat %>% select(-c(id))
truth <- data.frame(variable = c("x1", "x2", "x3"),
                    parameter = c(-0.4*0.75, 0.2*0.75, 0))
dat <- left_join(dat, truth, by = "variable")
names(dat) <- c("runtime", "n", "n_actual", "B", "missing_bound", "variable", "est", "se", "perc_025", "perc_975", "parameter")
dat <- dat %>% mutate(err = est - parameter,
                      cov_norm = ifelse(est - 1.96*se <= parameter & est + 1.96*se >= parameter, 1, 0),
                      cov_perc = ifelse(perc_025 <= parameter & perc_975 >= parameter, 1, 0))

summ <- dat %>% group_by(n, B, variable, missing_bound) %>%
  summarize(nreps = n(),
            bias = mean(err),
            bias_mc_se = bias/sqrt(nreps),
            coverage_norm = mean(cov_norm),
            coverage_norm_mc_se = sqrt(coverage_norm*(1-coverage_norm)/nreps),
            coverage_perc = mean(cov_perc),
            coverage_perc_mc_se = sqrt(coverage_perc*(1-coverage_perc)/nreps)) %>%
  mutate(bias_mc_cil = bias - 1.96 * bias_mc_se,
         bias_mc_ciu = bias + 1.96 * bias_mc_se,
         coverage_norm_mc_cil = coverage_norm - 1.96 * coverage_norm_mc_se,
         coverage_norm_mc_ciu = coverage_norm + 1.96 * coverage_norm_mc_se,
         coverage_perc_mc_cil = coverage_perc - 1.96 * coverage_perc_mc_se,
         coverage_perc_mc_ciu = coverage_perc + 1.96 * coverage_perc_mc_se)

summ <- summ %>% select(n, B, missing_bound, variable, coverage_norm, coverage_norm_mc_cil, coverage_norm_mc_ciu,
                        coverage_perc, coverage_perc_mc_cil, coverage_perc_mc_ciu)
names(summ) <- c("n", "B", "missing_bound", "variable", "norm_est", "norm_cil", "norm_ciu", "perc_est", "perc_cil", "perc_ciu")
summ <- summ %>% pivot_longer(cols = -c(n, B, missing_bound, variable),
                              names_to = c("type", ".value"),
                              names_sep = "_") %>%
  mutate(B = factor(B, levels = c(100, 250, 500, 1000),
                    labels = c("100", "200", "500", "1000")),
         variable = factor(variable, levels = c("x1", "x2", "x3"),
                           labels = c("W[1]", "W[2]", "W[3]")),
         type = factor(type, levels = c("norm", "perc"),
                       labels = c("Normal", "Percentile")))

p_175 <- summ %>% filter(missing_bound == 1.65) %>%
  ggplot(aes(x = n, group = type)) +
  geom_line(aes(y = est, linetype = type)) +
  geom_point(aes(y = est), size = 0.75) +
  geom_errorbar(aes(ymin=cil,
                    ymax=ciu),
                width=.1) +
  facet_grid(variable~ B, labeller = label_parsed) +
  theme_bw() +
  geom_hline(yintercept = 0.95, color = "red") +
  ylab("Empirical coverage") +
  xlab("Sample size") +
  labs(linetype = "Interval type") +
  ggtitle("Number of bootstrap replicates") +
  scale_y_continuous(breaks = c(0.8, 0.85, 0.9, 0.95, 1, 1.05),
                     labels = c("0.8", "0.85", "0.9", "0.95", "1", "1.05"),
                     limits = c(0.75, 1),
                     sec.axis = sec_axis(~ . , name = "Coefficient",
                                         labels = NULL, breaks = NULL)) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"))

p_190 <- summ %>% filter(missing_bound == 1.8) %>%
  ggplot(aes(x = n, group = type)) +
  geom_line(aes(y = est, linetype = type)) +
  geom_point(aes(y = est), size = 0.75) +
  geom_errorbar(aes(ymin=cil,
                    ymax=ciu),
                width=.1) +
  facet_grid(variable~ B, labeller = label_parsed) +
  theme_bw() +
  geom_hline(yintercept = 0.95, color = "red") +
  ylab("Empirical coverage") +
  xlab("Sample size") +
  labs(linetype = "Interval type") +
  ggtitle("Number of bootstrap replicates") +
  scale_y_continuous(breaks = c(0.8, 0.85, 0.9, 0.95, 1, 1.05),
                     labels = c("0.8", "0.85", "0.9", "0.95", "1", "1.05"),
                     limits = c(0.75, 1),
                     sec.axis = sec_axis(~ . , name = "Coefficient",
                                         labels = NULL, breaks = NULL)) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"))

p_210 <- summ %>% filter(missing_bound == 2.10) %>%
  ggplot(aes(x = n, group = type)) +
  geom_line(aes(y = est, linetype = type)) +
  geom_point(aes(y = est), size = 0.75) +
  geom_errorbar(aes(ymin=cil,
                    ymax=ciu),
                width=.1) +
  facet_grid(variable~ B, labeller = label_parsed) +
  theme_bw() +
  geom_hline(yintercept = 0.95, color = "red") +
  ylab("Empirical coverage") +
  xlab("Sample size") +
  labs(linetype = "Interval type") +
  ggtitle("Number of bootstrap replicates") +
  scale_y_continuous(breaks = c(0.8, 0.85, 0.9, 0.95, 1, 1.05),
                     labels = c("0.8", "0.85", "0.9", "0.95", "1", "1.05"),
                     limits = c(0.75, 1),
                     sec.axis = sec_axis(~ . , name = "Coefficient",
                                         labels = NULL, breaks = NULL)) +
  theme(strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 12, family = "Times New Roman"),
        legend.position="bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, family = "Times New Roman"),
        axis.text.y = element_text(family = "Times New Roman"),
        axis.title = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman"),
        legend.title = element_text(family = "Times New Roman"),
        legend.text = element_text(family = "Times New Roman"))

ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/bootstrap_testing/icenReg_boot_test_scen1_021525.pdf",
       plot = p_175,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/bootstrap_testing/icenReg_boot_test_scen2_021525.pdf",
       plot = p_190,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/bootstrap_testing/icenReg_boot_test_scen3_021525.pdf",
       plot = p_210,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")


# summ %>% ggplot(aes(x = n, y = bias, color = as.factor(B))) +
#   geom_point() +
#   facet_wrap(~variable)
#
# summ %>% ggplot(aes(x = n, y = coverage_norm, color = as.factor(B))) +
#   geom_point() +
#   facet_wrap(~variable)
#
# summ %>% ggplot(aes(x = n, y = coverage_perc, color = as.factor(B))) +
#   geom_point() +
#   facet_wrap(~variable)
