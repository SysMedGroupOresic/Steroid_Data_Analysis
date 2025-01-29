
the_fun_figs=function(tv_all, Group, hopiu, aa, bb, fn) {
  # Filter data based on the specified group
  if (Group == 'male') {
    NAFLDo = tv_all[tv_all[,'Gender'] == max(tv_all[,'Gender']),]
  } else if (Group == 'female') {
    NAFLDo = tv_all[tv_all[,'Gender'] == min(tv_all[,'Gender']),]
  } else if (Group == 'All') {
    NAFLDo = tv_all
  }
  
  # Select relevant columns from the filtered data
  SG0 = NAFLDo[, c(2:dim(tv_all)[2])]
  
  # Fix column names by replacing spaces and special characters
  oknames = colnames(SG0)
  SG0 = data.frame(SG0)
  colnames(SG0[, 8:27]) <- gsub("-", ".", colnames(SG0[, 8:27]))
  colnames(SG0[, 8:27]) <- gsub("/", ".", colnames(SG0[, 8:27]))
  
  hesh = c()
  xnam = colnames(SG0)[c(4:7)]
  Treatment = colnames(tv_all)[52:58]
  y = Treatment
  TreatmentN = Treatment
  
  # Loop through each combination of xnam and y to calculate correlations
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      # Extract relevant rows from hopiu based on conditions
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      
      # Prepare data for plotting
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      
      # Clean up Mediator names
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      
      # Set up directory for saving plots
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      
      # Save plots as JPEG files
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted_alle", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      # Plot correlations using ggplot2
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      
      print(a)
      dev.off()
    }
  }
  
  # Repeat the process for different combinations of xnam and y
  xnam <- colnames(SG0)[c(2)]
  y <- TreatmentN
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 4))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Repeat the process for different combinations of xnam and y
  xnam <- colnames(SG0)[c(3)]
  y <- TreatmentN
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted_alle", Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }
  }
  
  # Repeat the process for different combinations of xnam and y
  xnam <- TreatmentN
  y = colnames(SG0[, 8:27])
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 4))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
    }
  }
  
  # Additional analysis for specific combinations of xnam and y
  xnam <- c(x3, x6)
  y <- c(colnames(SG0[, 8:27]))
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Final analysis for specific combinations of xnam and y
  xnam <- c('AGE', 'BMI', colnames(SG0)[c(4:7)])
  y <- c(colnames(SG0[, 8:27]))
  for (i in 1:length(xnam)) {
    for (j in 1:length(y)) {
      hösh = hopiu[hopiu[, 1] == y[j] & hopiu[, 2] == xnam[i] & hopiu[, 3] == Group,]
      jeps = SG0
      r = as.numeric(hösh[4][1,])
      p.val = as.numeric(hösh[5][1,])
      rsadj = as.numeric(hösh[6][1,])
      colnames(jeps) = colnames(tv_all)[2:dim(tv_all)[2]]
      Treatment = as.character(hösh[2][1,])
      Mediator = as.character(hösh[1][1,])
      Mediator <- gsub("\\.", "-", Mediator)
      Mediator <- gsub("X", "", Mediator)
      if (Mediator == "T-Epi-T") {
        Mediator[Mediator == "T-Epi-T"] = "T/Epi-T"
      }
      Treatment2 = Treatment
      colnames(jeps)[colnames(jeps) == 'HOMA-IR'] = 'HOMA.IR'
      colnames(jeps) <- gsub(" ", "\\.", colnames(jeps))
      main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn, Group, "/"), collapse = "")
      sub_dir <- Treatment
      if (file.exists(file.path(main_dir, sub_dir))) {
        setwd(file.path(main_dir, sub_dir))
      } else {
        dir.create(file.path(main_dir, sub_dir))
        setwd(file.path(main_dir, sub_dir))
      }
      if (Mediator != "T/Epi-T") {
        jpeg(paste("Correlations plotted", Group, Treatment2, Mediator, ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      } else {
        jpeg(paste("Correlations plotted_", Treatment2, 'T.Epi.T', ".jpg"), width = 1000, height = 1000, quality = 100, pointsize = 20, res = 300)
      }
      xcx = jeps[, Treatment]
      ycy = jeps[, Mediator]
      väh = round(sd(ycy) / 2, 2)
      a = ggplot(jeps, aes(y = ycy, x = xcx)) +
        geom_point() +
        xlab(Treatment) +
        ylab(Mediator) +
        geom_smooth(method = "lm", col = "black") +
        annotate("text", x = min(xcx), y = max(ycy), hjust = 0, vjust = 0, label = paste0("r = ", round(r, 5))) +
        annotate("text", x = min(xcx), y = max(ycy) - väh, hjust = 0, vjust = 0, label = paste0("p = ", round(p.val, 5))) +
        theme_classic()
      print(a)
      dev.off()
      hesh = rbind(hesh, c(y[j], xnam[i], Group, r, p.val, rsadj))
    }
  }
  
  # Convert results to a data frame and return
  hesa = hesh
  hoi = as.data.frame(hesh)
  main_dir <- paste0(c("C://Users//patati//Desktop//Turku//R//", fn), collapse = "")
  setwd(main_dir)
  
  return(list(hopiu))
}

      