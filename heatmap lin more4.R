
# Add the R function code to the document
the_funal <- function(tv, Group, ok, fn, adj, sig.level, sick, sick_group, joo) {
  tv <- tv_covscl
  if (Group == 'male') {
    NAFLDo <- tv[tv[, 'Gender'] == max(tv[, 'Gender']), ]
  } else if (Group == 'female') {
    NAFLDo <- tv[tv[, 'Gender'] == min(tv[, 'Gender']), ]
  } else if (Group == 'All') {
    NAFLDo <- tv
  }
  
  SG0 <- NAFLDo[, c(2:dim(tv)[2])]
  SG0 <- data.frame(SG0)
  colnames(SG0[, 8:27]) <- gsub("-", ".", colnames(SG0[, 8:27]))
  colnames(SG0[, 8:27]) <- gsub("/", ".", colnames(SG0[, 8:27]))
  hesh <- c()
  
  Treatment <- colnames(tv_all)[52:58]
  Mediator <- colnames(tv_all)[9:28]
  Outcome <- colnames(tv_all)[c(29:51, 59:71)]
  
  xnam_sets <- list(
    colnames(SG0)[c(4:7)],
    colnames(SG0)[c(2)],
    colnames(SG0)[c(3)],
    Treatment,
    c(x3, x6),
    c('AGE', 'BMI', colnames(SG0)[c(4:7)]),
    c(x3, x6)
  )
  
  y_sets <- list(
    Treatment,
    Treatment,
    Treatment,
    colnames(SG0[, 8:27]),
    colnames(SG0[, 8:27]),
    colnames(SG0[, 8:27]),
    colnames(tv_all)[52:58]
  )
  
  perform_analysis <- function(xnam, y, Group, SG0, tv) {
    for (i in 1:length(xnam)) {
      for (j in 1:length(y)) {
        # Construct the formula based on the Group
        if (Group != 'All') {
          fmla <- as.formula(paste(y[j], " ~ ", paste(c(xnam[i], 'BMI', 'AGE'), collapse = "+")))
        } else {
          fmla <- as.formula(paste(y[j], " ~ ", paste(c(xnam[i], 'BMI', 'AGE', 'Gender'), collapse = "+")))
        }
        
        # Fit the linear model
        poissone <- lm(fmla, data = SG0)
        
        # Extract p-values and summary statistics
        p.val <- anova(poissone)$'Pr(>F)'[1]
        ps <- summary(poissone)
        pss <- ps[[4]]
        
        # Collect results
        hoesh <- c(y[j], xnam[i], Group, pss[2, 1], pss[2, 4], pss[2, 2])
        r <- as.numeric(hoesh[4])
        p <- as.numeric(hoesh[5])
        rsadj <- as.numeric(hoesh[6])
        
        # Update column names and other variables
        colnames(SG0) <- colnames(tv)[2:dim(tv)[2]]
        Treatment <- hoesh[2]
        Mediator <- hoesh[1]
        
        # Append results to hesh
        hesh <<- rbind(hesh, c(y[j], xnam[i], Group, r, pss[2, 4], rsadj))
      }
    }
  }
  
  # Loop through the sets and perform the analysis
  for (k in 1:length(xnam_sets)) {
    xnam <- xnam_sets[[k]]
    y <- y_sets[[k]]
    perform_analysis(xnam, y, Group, SG0, tv)
  }
  
  hesa <- hesh
  hoi <- as.data.frame(hesh)
  hopiu <- hoi
  colnames(hopiu) <- c('y', 'x', 'Gender', 'r', 'p', 'var_x')
  colnames(hoi) <- c('y', 'x', 'Gender', 'r', 'p', 'var_x')
  
  main_dir <- paste0(c("C://Users//patati//Documents//GitHub//Steroid_Data_Analysis//"), collapse = "")
  setwd(main_dir)
  
  meds <- names(table(hoi[, 1]))[!names(table(hoi[, 1])) %in% c(x3, x5, x6)]
  covas <- c('Steatosis.Grade', 'Fibrosis.Stage', 'Necroinflammation', 'HOMA.IR', 'AGE', 'BMI')
  
  if (adj == 'ok') {
    hoi[, 5] <- p.adjust(p = hopiu[, 5], method = 'BH', n = length(hopiu[, 5]))
    hopiu[, 5] <- p.adjust(p = hopiu[, 5], method = 'BH', n = length(hopiu[, 5]))
  }
  
  if (ok == 'big') {
    rsa <- c()
    joi <- c()
    
    meds <- names(table(hoi[, 1]))[!names(table(hoi[, 1])) %in% c(x3, x5, x6)]
    covas <- c('Steatosis.Grade', 'Fibrosis.Stage', 'Necroinflammation', 'HOMA.IR', 'AGE', 'BMI')
    
    c1 <- hoi[, 2] %in% covas
    c2 <- hoi[, 1] %in% meds
    hyy <- c1 & c2
    m1 <- hoi[hyy, ]
    colnames(m1) <- c('y', 'x', 'Gender', 'r', 'p', 'var_x')
    c1 <- hoi[, 2] %in% c(x3, x6)
    c2 <- hoi[, 1] %in% meds
    hyy <- c1 & c2
    m2 <- hoi[hyy, ]
    colnames(m2) <- c('y', 'x', 'Gender', 'r', 'p', 'var_x')
    joi <- rbind(m1, m2)
    i <- 4
    rs <- c()
    
    for (i in 4:5) {
      rs <- joi[, c(1, 2, i)]
      rs <- reshape(rs, idvar = "x", timevar = "y", direction = "wide")
      rownames(rs) <- rs[, 1]
      rs <- rs[, -1]
      
      library(stringr)
      colnames(rs) <- str_sub(colnames(rs), 3, -1)
      
      colnames(rs) <- gsub("\\.", "-", colnames(rs))
      colnames(rs) <- gsub("X11", "11", colnames(rs))
      colnames(rs) <- gsub("X17", "17", colnames(rs))
      colnames(rs)[colnames(rs) == "T-Epi-T"] <- "T/Epi-T"
      
      rownames(rs)[rownames(rs) == "Steatosis.Grade"] <- "Steatosis Grade"
      rownames(rs)[rownames(rs) == "Fibrosis.Stage"] <- "Fibrosis Stage"
      rownames(rs)[rownames(rs) == "HOMA.IR"] <- "HOMA-IR"
      covas[covas == "Steatosis.Grade"] <- "Steatosis Grade"
      covas[covas == "Fibrosis.Stage"] <- "Fibrosis Stage"
      covas[covas == "HOMA.IR"] <- "HOMA-IR"
      
      heps <- c(groups[, 2])
      heps[heps == "17aOH-P4"] <- "17a-OHP4"
      cme1 <- match(heps, colnames(rs))
      cme2 <- match(c(covas, x3, x6), rownames(rs))
      rs <- rs[cme2, cme1]
      rsa <- rbind(rsa, rs)
    }
    
    rs1a <- rsa[1:dim(rs)[1], ]
    rs2a <- rsa[(dim(rs1a)[1] + 1):(dim(rs1a)[1] + dim(rs1a)[1]), ]
    
    rs1 <- rs1a
    rs2 <- rs2a
    rownames(rs2) <- str_sub(rownames(rs2), end = -2)
    rownames(rs1) <- gsub("\\.", " ", rownames(rs1))
    rownames(rs2) <- gsub("\\.", " ", rownames(rs2))
    rownames(rs1)[rownames(rs1) == "HOMA IR"] <- "HOMA-IR"
    rownames(rs2)[rownames(rs2) == "HOMA IR"] <- "HOMA-IR"
    
    rownames(rs1)[rownames(rs1) == "Gender"] <- "HOMA-IR"
    rownames(rs2)[rownames(rs2) == "HOMA IR"] <- "HOMA-IR"

rango <- function(x, mi, ma) {
  (ma - mi) / (max(x) - min(x)) * (x - min(x)) + mi
}
rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))

rs1 <- as.matrix(rs1)
rs2 <- as.matrix(rs2)

rs1[rs1 > 0.5] <- 0.5
rs1[rs1 < -0.5] <- -0.5

width <- 2500
height <- 4400
order <- "original"
range <- 'orig'
corre <- 'no_renorm'
type <- 'full'
method <- 'color'
cl.offset <- 1.0
cl.length <- 15
cl.cex <- 1.09
pch.cex <- 1.09
pch <- 11
cl.pos <- 'n'
ho <- Group
hip1 <- 'BAs_lipids_as_y vs. steroids_as_x'

jpeg(paste("Linear Model Estimate Plot ofees5", hip1, Group, ".jpg"), width = width, height = height, quality = 100, pointsize = 16, res = 300)

hepio <- colorRampPalette(c('blue', 'white', 'orange'), alpha = TRUE)(150)

corrplot(rs1, type = type, order = order, method = method, p.mat = rs2, tl.col = "black", cl.cex = cl.cex, pch.cex = pch.cex, pch.col = 'black', pch = pch, sig.level = c(.001, .01, .05), cl.pos = cl.pos, 
         insig = "label_sig", cl.offset = cl.offset, cl.length = cl.length, tl.cex = 0.5, tl.srt = 90, diag = TRUE, col = colorRampPalette(c('blue', 'white', 'orange'), alpha = TRUE)(100), is.corr = FALSE, col.lim = c(-0.5, 0.5))

dev.off()
eoh <- paste("Linear Model Estimate Plot ofees5", hip1, Group, ".jpg")
daiR::image_to_pdf(eoh, pdf_name = paste0(eoh, '.pdf'))
my_image <- image_read(eoh)
my_svg <- image_convert(my_image, format = "svg")
image_write(my_svg, paste(eoh, ".svg"))

} else {
  
  rsa <- c()
  rs1 <- c()
  rs2 <- c()
  
  c1 <- hoi[, 1] %in% x5
  hoi[c1, ]
  c2 <- hoi[, 1] %in% names(table(hoi[, 1]))[!names(table(hoi[, 1])) %in% c(x3, x5, x6)]
  hyy <- c1 & c2
  hoi2 <- hoi[c2 | c1, ]
  rownames(hoi2) <- 1:dim(hoi2)[1]
  hoi2 <- hoi2[1:182, ]
  a <- hoi2[1:42, 1]
  b <- hoi2[1:42, 2]
  hoi2[1:42, ] <- cbind(b, a, hoi2[1:42, 3:5])
  hoi2 <- hoi2[, c(2, 1, 3:5)]
  
  i <- 4
  rse <- c()
  for (i in 4:5) {
    
    rse <- hoi2[, c(1, 2, i)]
    rse <- rse[order(rse[, 1]), ]
    rs <- reshape(rse, idvar = "x", timevar = "y", direction = "wide")
    rownames(rs) <- rs[, 1]
    rs <- rs[, -1]
    
    library(stringr)
    colnames(rs) <- str_sub(colnames(rs), 3, -1)
    
    colnames(rs) <- gsub("\\.", "-", colnames(rs))
    colnames(rs) <- gsub("X11", "11", colnames(rs))
    colnames(rs) <- gsub("X17", "17", colnames(rs))
    colnames(rs)[colnames(rs) == "T-Epi-T"] <- "T/Epi-T"
    colnames(rs)[colnames(rs) == "T-E-T"] <- "T/Epi-T"
    colnames(rs)[colnames(rs) == "Steatosis-Grade"] <- "Steatosis Grade"
    colnames(rs)[colnames(rs) == "Fibrosis-Stage"] <- "Fibrosis Stage"
    colnames(rs)[colnames(rs) == "17aOH-P4"] <- "17a-OHP4"
    heps <- c(covas, groups[, 2])
    heps <- gsub("\\.", " ", heps)
    heps[heps == "HOMA IR"] <- "HOMA-IR"
    heps[heps == "17aOH-P4"] <- "17a-OHP4"
    ccc <- match(heps, colnames(rs))
    
    rs <- rs[, ccc]
    rsa <- rbind(rsa, rs)
  }
  
  rs1a <- rsa[1:7, ]
  rs2a <- rsa[8:14, ]
  rs1 <- rs1a
  rs2 <- rs2a
  
  rs1 <- mutate_all(rs1, function(x) as.numeric(as.character(x)))
  rs2 <- mutate_all(rs2, function(x) as.numeric(as.character(x)))
  
  rs1 <- as.matrix(rs1)
  rs2 <- as.matrix(rs2)
  
  rs1[rs1 > 0.4] <- 0.4
  rs1[rs1 < -0.4] <- -0.4
  
  order <- "original"
  range <- 'orig'
  corre <- 'no_renorm'
  type <- 'full'
  method <- 'color'
  cl.offset <- 1.0
  cl.length <- 11
  cl.cex <- 1.4
  pch.cex <- 1.5
  pch <- 20
  cl.pos <- 'n'
  ho <- Group
  hip1 <- 'Steroids_y vs. PFAS_as_x'
  width <- 5500
  height <- 1800
  
  jpeg(paste("Linear Model Estimate Plot of_sa5", hip1, Group, ".jpg"), width = width, height = height, quality = 100, pointsize = 16, res = 300)
  corrplot(rs1, type = type, order = order, method = method, p.mat = rs2, tl.col = "black", cl.cex = cl.cex, pch.cex = pch.cex, pch.col = 'black', pch = pch,
           sig.level = c(.001, .01, .05), cl.pos = cl.pos, insig = "label_sig", cl.offset = cl.offset, cl.length = cl.length, tl.cex = 0.8,
           tl.srt = 90, diag = TRUE, col = colorRampPalette(c('blue', 'white', 'orange'), alpha = TRUE)(100), is.corr = FALSE, col.lim = c(-0.4, 0.4))
  
  dev.off()
  eoh <- paste("Linear Model Estimate Plot of_sa5", hip1, Group, ".jpg")
  daiR::image_to_pdf(eoh, pdf_name = paste0(eoh, '.pdf'))
  my_image <- image_read(eoh)
  my_svg <- image_convert(my_image, format = "svg")
  image_write(my_svg, paste(eoh, ".svg"))
}

return(list(hopiu))
}