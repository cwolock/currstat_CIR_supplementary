library(icenReg)
library(tidyverse)
library(extrafont)
library(gridExtra)
library(ggpubr)
# font_import(prompt = FALSE)
loadfonts(device = "all")
dat <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/long_covid_truncated_120_022425_noinconclusives_fixedexpdates_keepinvitedbothyears_wlag.rds")

# hisotgram of response times
p <- dat %>%
  filter(time_event < 120) %>%
  ggplot(aes(x = time_event)) +
  geom_histogram(binwidth = 2, color = "white", fill = "gray70") +
  theme_bw() +
  scale_x_continuous(breaks = c(25, 50, 75,  100, 125)) +
  xlab("Survey response time (days since positive test)") +
  ylab("Number of respondents") +
  theme(axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"))
ggsave(plot = p, filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/response_times_histogram_022425.pdf",
       device = "pdf",
       dpi = 300,
       width = 7,
       height = 4,
       units = "in")

sum(dat$invite_lag > 120)
p <- dat %>%
  filter(invite_lag <= 120) %>%
  ggplot(aes(x = invite_lag)) +
  geom_histogram(binwidth = 2, color = "white", fill = "gray70") +
  theme_bw() +
  scale_x_continuous(breaks = c(25, 50, 75,  100, 125)) +
  xlab("Survey invitation time (days since positive test)") +
  ylab("Number of invitees") +
  theme(axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"))
ggsave(plot = p, filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/invite_lag_times_histogram_022425.pdf",
       device = "pdf",
       dpi = 300,
       width = 7,
       height = 4,
       units = "in")


# CIR results
# fit <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/chu_lab/susan/data/CIR_results_correct_063024_trunc120_window115rds")
fit <- readRDS("/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/CIR_results_022425_trunc120_window115_noinconclusives_fixedexpdates_keepinvitedbothyears.rds")

# fit <- fit %>% mutate(x = as.numeric(x), #x_quants = as.numeric(x_quants),
#                       y = as.numeric(y), y_low = as.numeric(y_low),
#                       y_hi = as.numeric(y_hi))
# # fit <- fit %>% select(-x_quants)
# # isotonize confidence bands?
# fit$y_low <- Iso::pava(fit$y_low)
# fit$y_hi <- Iso::pava(fit$y_hi)
# fit$y <- 1 - fit$y
# fit$y_low <- 1 - fit$y_low
# fit$y_hi <- 1 - fit$y_hi


lims <- c(27, 95)
breaks <- c(30, 45, 60, 75, 90)
p1 <- fit %>% filter(t <= 90 & t>= 30) %>%
  ggplot(aes(x = t)) +
  geom_vline(xintercept = 30, color = "gray") +
  geom_vline(xintercept = 60, color = "gray") +
  geom_vline(xintercept = 90, color = "gray") +
  geom_point(aes(x = 30, y= 0.1918571), color = "black") +
  geom_point(aes(x = 60, y= 0.1291988), color = "black") +
  geom_point(aes(x = 90, y= 0.06809619), color = "black") +
  geom_step(aes(y = S_hat_est)) +
  geom_step(aes(y = S_hat_cil), linetype = "dashed") +
  geom_step(aes(y = S_hat_ciu), linetype = "dashed") +
  theme_bw() +
  ylab("Proportion with symptoms") +
  scale_y_continuous(limits = c(0, 0.3),
                     labels = c("0", "0.05", "0.1", "0.15", "0.2", "0.25", "0.3"),
                     breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3))+
  theme(axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        strip.text = element_text(family = "Times New Roman"),
        strip.background = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())
p2 <- dat %>%
  filter(time_event < 120) %>%
  ggplot(aes(x = time_event)) +
  geom_histogram(binwidth = 1, color = "white", fill = "gray70") +
  geom_vline(xintercept = 30, color = "gray") +
  geom_vline(xintercept = 60, color = "gray") +
  geom_vline(xintercept = 90, color = "gray") +
  theme_bw() +
  scale_y_reverse() +
  xlab("Survey response time (days since positive test)") +
  ylab("Number of respondents") +
  theme(axis.title = element_text(family = "Times New Roman"),
        axis.text = element_text(family = "Times New Roman"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())


p1.common.x <- p1 + scale_x_continuous(limits=lims, breaks=breaks)
p2.common.x <- p2 + scale_x_continuous(limits=lims, breaks=breaks)
p1.common.x <- ggplot_gtable(ggplot_build(p1.common.x))
p2.common.x <- ggplot_gtable(ggplot_build(p2.common.x))

# copy the plot height from p1 to p2
p2.common.x$widths <- p1.common.x$widths

p_both <- grid.arrange(p1.common.x,
                       p2.common.x,
                       ncol=1,
                       nrow = 2,
                       heights = c(1, 0.5))

# p_both <- ggarrange(p, p2, nrow = 2)
ggsave(filename = "/Users/cwolock/Dropbox/UW/RESEARCH/paper_supplements/currstat_CIR_supplementary/data_analysis/CIR_results_022425_noinconclusives_fixedexpdates_keepinvitedbothyears.pdf",
       plot = p_both,
       device="pdf", width = 9,
       height=5, units="in", dpi=300)
