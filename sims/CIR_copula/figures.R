library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_copula/")
dat <- readRDS("CIR_copula_030525_sped_up_old_dens_estimation_stackF.rds")
dat2 <- readRDS("CIR_copula_030525_sped_up_old_dens_estimation_stackF_postheta.rds")
dat <- bind_rows(dat, dat2)

dat <- readRDS("CIR_copula_031325_frank_biggerN.rds")
# dat <- dat %>% filter(missing_bound != -100) %>% select(-y_quant)

# dat2 <- readRDS("CIR_testing_012925_npmlesurvfit.rds")

# dat <- rbind(dat, dat2)

messed_up <- dat %>% filter(is.na(cdf_estimate))

dat <- dat %>% filter(!(is.na(cdf_estimate))) %>%
  filter(theo_kendall != -0.025 & theo_kendall != 0.025 & theo_kendall != 0.1 & theo_kendall != -0.1)

dat <- dat %>% mutate(err = cdf_estimate - truth,
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(method = factor(method, levels = c("copula", "nocopula"),
                         labels = c("Copula extended CIR", "Baseline extended CIR")),
         n = factor(n),
         theta = factor(theta))
         #missing_bound = factor(missing_bound,
         #                       levels = c(2.1, 1.8, 1.65),
        #                        labels = c("Scenario 1", "Scenario 2", "Scenario 3")),
        # eval_upper_bound = factor(eval_upper_bound))



summ <- dat %>% group_by(n, method, truth, theo_kendall) %>%
  summarize(nreps = n(),
            tau_bias = mean(emp_kendall - theo_kendall),
            sample_size = mean(n_actual),
            bias = mean(err)) %>%
  filter(nreps > 100)

timeaverage_summ <- summ %>% group_by(n, method, theo_kendall) %>%
  summarize(bias = mean(abs(bias)),
            tau_bias = mean(tau_bias))

p_bias <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = bias, linetype = n, color = n)) +
  facet_grid(method~theo_kendall) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle(expression("Kendall's "*tau)) +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
  # scale_y_continuous(breaks = c(-0.03, -0.02, -0.01, 0, 0.01, 0.02, 0.03),
  #                    labels = c("-0.03", "-0.02", "-0.01", "0.00", "0.01", "0.02", "0.03"),
  #                    limits = c(-0.03, 0.03),
  #                    sec.axis = sec_axis(~ . , name = "Method",
  #                                        labels = NULL, breaks = NULL)) +
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


ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_copula/copula_bias_plot_031325_frank.pdf",
       plot = p_bias,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
