## Unión de gráficos en pdf ----
install.packages("magick");install.packages("pdftools")
library(magick); library(pdftools); library(cowplot); library(grid)

# Read the PDF file
input_pdf <- "FIG/STConsultasTemp_Concepción.pdf"
output_png <- "FIG/STConsultasTemp_Concepción.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/STConsultasTemp_HSJ.pdf"
output_png <- "FIG/STConsultasTemp_HSJ.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/STConsultasTemp_HSJD.pdf"
output_png <- "FIG/STConsultasTemp_HSJD.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/STConsultasTemp_HUAP.pdf"
output_png <- "FIG/STConsultasTemp_HUAP.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

input_pdf <- "FIG/STConsultasTemp_LaSerena.pdf"
output_png <- "FIG/STConsultasTemp_LaSerena.png"

# Read the PDF
pdf_image <- image_read_pdf(input_pdf, density = 300)  # You can change density if needed

# Write the PNG
image_write(pdf_image, path = output_png)

## Load images together ----
image_files <- c("FIG/STConsultasTemp_Concepción.png", "FIG/STConsultasTemp_LaSerena.png", 
                 "FIG/STConsultasTemp_HSJ.png", "FIG/STConsultasTemp_HSJD.png",
                 "FIG/STConsultasTemp_HUAP.png")

images <- lapply(image_files, function(file) {
  image_read(file)
})

ggplots <- lapply(images, function(img) {
  ggplot() + 
    annotation_custom(rasterGrob(img)) + 
    theme_void() # This theme removes axes and labels
})

combined_plot <- plot_grid(plotlist = ggplots, ncol = 2) 

ggsave("FIG/STtotales.pdf", combined_plot)
ggsave("FIG/STtotales.png", combined_plot)