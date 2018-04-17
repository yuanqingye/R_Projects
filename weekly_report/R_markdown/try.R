colour <- c('G', 'G', 'R', 'Y', 'G', 'Y', 'Y', 'R', 'Y')
col <- factor(colour)
col1 <- factor(colour, levels = c('G', 'R', 'Y'), labels = c('Green', 'Red', 'Yellow'))

col_vec <- as.vector(col1)
ls()