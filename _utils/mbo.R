# Utility function for MBO


# Function that creates 3D plots of loss surface
plot_loss_surface <- function(mbo, zlim = NULL, file_suffix = "", to_disk = TRUE,show_legend=TRUE){
  
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  pars_w <- mbo[[1]]$archive %>% mutate(loss = oob_error)
  
  w_id_opt_grid <- pars_w %>% filter(is.na(.already_evaluated)) %>% nrow()
  col_w <- ifelse(is.na(pars_w$.already_evaluated), "init", "proposed (EI)")
  col_w[which.min(pars_w$loss)] <- "Solution BO"
  col_w[w_id_opt_grid] <- "Solution Grid-Search"
  # cv_prop <- which(pars_w$mtry == cv_opt_w$mtry & pars_w$min.node.size == cv_opt_w$min.node.size)  
  col_w <- as.factor(col_w)
  
  
  opt_w <- (pars_w %>% arrange(loss) %>% select(min.node.size,mtry,loss))[1,]
  main_w <- paste0("RF",str_extract(form,"\\d"), " MBO: ", ifelse(var == "w","Wealth","Income")," \n optimal (mtry, min.node.size, loss) = (",
                   paste(opt_w$mtry,opt_w$min.node.size,round(opt_w$loss,3),sep = ", "),")")
  
  colvar <- as.numeric(as.factor(col_w))
  col <- c("black","blue","green", "red")#[colvar]
  # col <- 1:4
  pch <- c(19,19,18,18)[colvar]
  cex <- c(0.5,0.5,2,2)[colvar]
  if(to_disk){
    pdf(paste0(fig_path,var,"_RF",str_extract(form,"\\d"),"_mbo_3d",file_suffix,".pdf"))
  }
  with(pars_w,
       
       scatter3D(x = mtry, y = min.node.size, z = loss, colvar = colvar,
                 col = col, xlab = "mtry", zlim = zlim,
                 ylab = "min.node.size", zlab = "MSE", theta = 40,
                 phi = 0, bty = "g",  type = "h", colkey = FALSE,
                 ticktype = "detailed", pch = pch, cex = cex, main = main_w)
       
  )
  if(show_legend){
    legend("right", c("Initial Design", "Proposal by EI", "BO Solution", "Grid-Search Solution"), 
           col = col, pch = c(19,19,18,18), title = "Proposal")
  }
  if(to_disk){
    dev.off()
  }
  
  
  
}

# Function that converts colors from names (e.g. 'red') to hex
col2hex <- function(x, alpha = FALSE) {
  args <- as.data.frame(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}

# Function that plots evolution of loss over iterations of MBO
plot_loss_path <- function(mbo, zlim = NULL, file_suffix = "", to_disk = TRUE){
  
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  pars_w <- mbo[[1]]$archive %>% mutate(loss = oob_error)
  
  w_id_opt_grid <- pars_w %>% filter(is.na(.already_evaluated)) %>% nrow()
  col_w <- ifelse(is.na(pars_w$.already_evaluated), "init", "proposed (EI)")
  col_w[which.min(pars_w$loss)] <- "Solution BO"
  col_w[w_id_opt_grid] <- "Solution Grid-Search"
  # cv_prop <- which(pars_w$mtry == cv_opt_w$mtry & pars_w$min.node.size == cv_opt_w$min.node.size)  
  col_w <- as.factor(col_w)
  
  
  opt_w <- (pars_w %>% arrange(loss) %>% select(min.node.size,mtry,loss))[1,]
  
  colvar <- as.numeric(as.factor(col_w))
  cols <- c("black","blue","green", "red")#[colvar]
  
  
  
  col_groups <- sapply(cols, col2hex)
  names(col_groups) <- c("init","proposed (EI)","Solution BO","Solution Grid-Search")
  
  p <- pars_w %>% mutate(n = row_number(), col = col_w) %>% 
    pivot_longer(cols = c(oob_error, acq_ei)) %>% 
    mutate(name = ifelse(name == "oob_error", "OOB Error", "Expected Improvement")) %>% 
    ggplot(aes(x = n, y = value)) +
    geom_line() +
    geom_point(aes(color = col)) +
    facet_wrap(~name, ncol = 1, scales = "free") +
    scale_y_log10() +
    theme(legend.position = "bottom") +
    ggtitle(paste0("RF",str_extract(form,"\\d")," ", ifelse(var == "w","Wealth","Income"),": BO Progress - Expected Improvement & OOB Error")) +
    xlab("Iteration") +
    scale_color_manual(values = col_groups, 
                       labels = c("Initial Design", "Proposal by EI", "BO Solution", "Grid-Search Solution")) +
    guides(color = guide_legend(""))
  
  if(to_disk){
    pdf(paste0(fig_path,var,"_RF",str_extract(form,"\\d"),"_EI_error_path",file_suffix,".pdf"))
    print(p)
    dev.off()
  } else {
    p
  }
  
}
