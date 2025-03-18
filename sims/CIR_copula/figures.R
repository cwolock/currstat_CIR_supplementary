library(tidyverse)
library(extrafont)
# font_import(prompt = FALSE)
loadfonts(device = "all")
setwd("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/sims/CIR_copula/")
dat <- readRDS("CIR_copula_031725.rds")

messed_up <- dat %>% filter(is.na(cdf_estimate))

dat <- dat %>% filter(!(is.na(cdf_estimate)))

dat <- dat %>% mutate(err = cdf_estimate - truth,
                      truth = round(truth, digits = 2))

dat <- dat %>% filter(round(truth, digits = 2) <= 0.95 & round(truth, digits = 2) >= 0.05) %>%
  mutate(method = factor(method, levels = c("copula", "nocopula"),
                         labels = c("Copula extended CIR", "Baseline extended CIR")),
         n = factor(n),
         tau = factor(tau))

summ <- dat %>% group_by(n, method, truth, tau) %>%
  summarize(nreps = n(),
            sample_size = mean(n_actual),
            bias = mean(err)) %>%
  filter(nreps > 100)

p_bias <- summ %>%
  ggplot(aes(x = truth, group = n)) +
  geom_line(aes(y = bias, linetype = n, color = n)) +
  facet_grid(method~tau) +
  theme_bw() +
  geom_hline(yintercept = 0, color = "black") +
  ylab("Bias") +
  xlab("Symptom resolution probability") +
  ggtitle(expression("Kendall's "*tau)) +
  scale_linetype_manual(name = "Sample size", values = c("dotted", "dashed", "longdash", "solid")) +
  scale_color_discrete(name = "Sample size") +
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


ggsave(filename = "copula_bias_plot_031725.pdf",
       plot = p_bias,
       device = "pdf",
       width = 7,
       height = 5,
       dpi = 300,
       units = "in")
