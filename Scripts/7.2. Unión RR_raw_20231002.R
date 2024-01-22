## Unión de gráficos en pdf ----
install.packages("magick");install.packages("pdftools")
library(magick); library(pdftools); library(cowplot); library(grid)

# Read the PDF file
input_pdf <- "FIG/RR_HGGB_CCP.pdf"
output_png <- "FIG/RR_HGGB_CCP.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/RR_HSJ.pdf"
output_png <- "FIG/RR_HSJ.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/RR_HSJD.pdf"
output_png <- "FIG/RR_HSJD.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/RR_HUAP.pdf"
output_png <- "FIG/RR_HUAP.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/RR_HSJD_LS.pdf"
output_png <- "FIG/RR_HSJD_LS.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

## Load images together ----
image_files <- c("FIG/RR_HGGB_CCP.png", "FIG/RR_HSJD_LS.png",
                 "FIG/RR_HSJ.png","FIG/RR_HSJD.png", 
                 "FIG/RR_HUAP.png")

images <- lapply(image_files, function(file) {
  image_read(file)
})

ggplots <- lapply(images, function(img) {
  ggplot() + 
    annotation_custom(rasterGrob(img)) + 
    theme_void() # This theme removes axes and labels
})

combined_plot <- plot_grid(plotlist = ggplots, ncol = 2) 

ggsave("FIG/RR_Totales.pdf", combined_plot)
ggsave("FIG/RR_Totales.png", combined_plot)