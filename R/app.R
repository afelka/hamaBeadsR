# Load required packages
library(shiny)
library(magick)
library(ggplot2)
library(tidyr)
library(dplyr)

# Define UI for the Shiny App
ui <- fluidPage(
  titlePanel("Image to Hama Beads Pattern"),

  # Sidebar for image upload
  sidebarLayout(
    sidebarPanel(
      fileInput("upload", "Upload an image (png/jpg/jpeg)", accept = c('image/png', 'image/jpeg', 'image/jpg')),
      actionButton("process", "Generate Hama Bead Pattern"),
      downloadButton("download_plot", "Download Plot")
    ),

    # Main panel to display output image
    mainPanel(
      plotOutput("hama_plot", width = "600px", height = "600px")
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Define Hama Colors
  hama_colors <- data.frame(
    name = c("black", "white", "red", "blue", "yellow", "green", 'darkgreen', "brown", "cyan", "beige",
             "pink", "orchid", "orange", "purple", "lightblue", "lightgreen", "grey", "darkgrey", "darkred",
             "azure", "plum", "gold","turquoise" , "ivory", "lightsteelblue"),
    r = c(0, 255, 255, 0 , 255, 0, 0, 165, 0 , 245, 255, 218, 255, 160, 173, 144, 190 ,
          169, 139, 240 , 221, 255, 64,255,  176),
    g = c(0, 255, 0, 0 , 255, 255, 100, 42, 255 , 245, 192, 112, 165, 32, 216, 238,
          190 , 169, 0, 255, 160 , 215, 224, 255 ,  196),
    b = c(0, 255, 0, 255, 0, 0, 0, 42 , 255 ,220, 203, 214, 0, 240, 230 ,
          144, 190 , 169, 0, 255, 221, 0, 208,240 , 222)
  )

  # Function to find the closest Hama bead color
  closest_color <- function(r, g, b, palette) {
    distances <- sqrt((r - palette$r)^2 + (g - palette$g)^2 + (b - palette$b)^2)
    palette$name[which.min(distances)]
  }

  # Reactively process the uploaded image
  observeEvent(input$process, {
    req(input$upload)

    # Read and resize the uploaded image to 29x29
    img <- image_read(input$upload$datapath) %>%
      image_resize("29x29!")

    # Convert the image to an array
    img_array <- image_data(img)

    # Convert raw array data to numeric RGB values
    r_channel <- as.numeric(img_array[1,,])
    g_channel <- as.numeric(img_array[2,,])
    b_channel <- as.numeric(img_array[3,,])

    # Create a data frame with RGB values and pixel positions
    img_df <- expand.grid(X = 1:dim(img_array)[3], Y = 1:dim(img_array)[2]) %>%
      mutate(
        R = r_channel,
        G = g_channel,
        B = b_channel,
        # Find closest Hama bead color for each pixel
        ClosestColor = mapply(closest_color, R, G, B, MoreArgs = list(palette = hama_colors))
      )

    # Render the plot
    output$hama_plot <- renderPlot({
      ggplot(img_df, aes(x = X, y = Y)) +
        geom_point(shape = 21, size = 3, stroke = 3, color = img_df$ClosestColor) +  # Shape 21 for filled circles
        scale_y_reverse() +       # Flip the y-axis to match image orientation
        coord_fixed() +           # Keep aspect ratio fixed
        theme_void() +
        theme(legend.position = "none") +  # Remove the legend
        ggtitle("29x29 with Closest Hama Colors")
    })

    # Download handler for the plot
    output$download_plot <- downloadHandler(
      filename = function() {
        # Generate the filename based on the uploaded file name
        # Remove the file extension from the uploaded file name
        base_name <- tools::file_path_sans_ext(basename(input$upload$name))
        # Append the current date and ".png" to create a unique filename
        paste0(base_name, "_hama_bead_pattern_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
