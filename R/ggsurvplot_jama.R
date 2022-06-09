#' @title ggsurvplot_jama.
#' @description  Making KM survival curves and risk table in JAMA publication-ready format.
#' @param fit an object of class survfit.
#' @param fontsize text font size, the default value is 10.
#' @param risk_table_fontsize the font size of risk table content, the default value is 3.5.
#' @param x_label the title of x axis, the default value is 'Time'.
#' @param y_label the title of y axis.
#' @param plot_yaxis_title_position the distance between yaxis and its title, the default value is -4.
#' @param table_title_position, the indent of risk table's title.
#' @param annotation an object of list; the element of the list should be vectors of length 3 that contain the x coordinate, y coordinate, and the content of annotation.
#' @export


ggsurvplot_jama <- function (surv_fit, 
                             fontsize = 10, 
                             risk_table_fontsize = 3.5, 
                             x_label = "Time", 
                             y_label, 
                             plot_yaxis_title_position = -4, 
                             table_title_position = -0.26, 
                             annotation = NULL)
{
  km1 <- survminer::ggsurvplot(surv_fit, risk.table = TRUE, legend = 'none', conf.int = FALSE,
                               risk.table.y.text.col = FALSE, risk.table.height = 0.15, fontsize = risk_table_fontsize,
                               ggtheme = theme_bw() + theme(panel.border = element_blank(),
                                                            axis.line = element_line(colour = "black", size = 0.3),
                                                            panel.grid.major.x = element_blank(),
                                                            panel.grid.minor.x = element_blank(),
                                                            panel.grid.minor.y = element_blank(),
                                                            axis.text.x = element_text(size = fontsize),
                                                            axis.text.y = element_text(size = fontsize),
                                                            axis.title.x = element_text(size = fontsize),
                                                            axis.title.y = element_text(size = fontsize, margin = margin(0,plot_yaxis_title_position,0,0,'cm')),
                                                            plot.margin = unit(c(0.5,0.5,0,1),'cm')),
                               xlab = x_label, ylab = y_label, palette = c("black", "orange"))
  km1$plot <- km1$plot + scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), breaks = seq(0, 1.0, 0.2), labels = c("0","0.2","0.4","0.6","0.8","1.0"))
  if (class(annotation) == "list"){
    for (i in 1:length(annotation)){
      km1$plot <- km1$plot + ggplot2::annotate("text", x = as.numeric(annotation[[i]][1]), y = as.numeric(annotation[[i]][2]), label = annotation[[i]][3])
    }
  }
  km1$table <- km1$table + ggtitle('No. at risk') +
    theme(axis.line = element_blank(), panel.grid.major.y = element_blank(),
          axis.title.y = element_blank(), axis.title.x = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_text(size = fontsize, color = "black", hjust = 0,
                                     margin = margin(0,1.1,0,0,'cm')),
          axis.ticks.y = element_blank(),
          plot.title = element_text(size = fontsize, hjust = table_title_position,
                                    margin = margin(0,0,0,0,'cm')),
          plot.margin = unit(c(0,-0.3,0,0.3),'cm'))
  
  result <- km1
}
