#' @title ggsurvplot_jama.
#' @description  Making KM survival curves and risk table in JAMA publication-ready format.
#' @param fit an object of class survfit.
#' @param x_label the title of x axis.
#' @param y_label the title of y axis.
#' @export


ggsurvplot_jama <- function (surv_fit, x_label = "Time", y_label)
{
  km1 <- survminer::ggsurvplot(surv_fit, risk.table = TRUE, size = 0.8, legend = 'none',
                               risk.table.y.text.col = FALSE, risk.table.height = 0.15, risk.table.fontsize = 3.8,
                               ggtheme = theme_bw() + theme(panel.border = element_blank(),
                                                            axis.line = element_line(colour = "black", size = 0.3),
                                                            panel.grid.major.x = element_blank(),
                                                            panel.grid.minor.x = element_blank(),
                                                            panel.grid.minor.y = element_blank(),
                                                            axis.text.x = element_text(size = 12),
                                                            axis.text.y = element_text(size = 12),
                                                            axis.title.y = element_text(margin = margin(0,-4,0,0,'cm')),
                                                            plot.margin = unit(c(0.5,0.5,0,1),'cm')),
                               xlab = x_label, ylab = y_label, palette = c("black", "orange"))
  km1$plot <- km1$plot + scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 1.0, 0.2), labels = c("0","0.2","0.4","0.6","0.8","1.0"))
  km1$table <- km1$table + ggtitle('No. at risk') +
    theme(axis.line = element_blank(), panel.grid.major.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = 10, color = "black", hjust = 0,
                                     margin = margin(0,1.1,0,0,'cm')),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = 10, hjust = -0.26,
                                    margin = margin(0,0,0,0,'cm')),
          plot.margin = unit(c(0,-0.3,0,0.3),'cm'))

  result <- km1
}
