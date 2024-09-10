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
      selectInput("size", "Select Image Size", choices = c("29x29", "57x57"), selected = "29x29"),
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

  # Get R G B values for all colors
  all_colors_rgb <-  data.frame(name = colors()) %>% rowwise() %>%
    mutate(
      r = col2rgb(name)[1],
      g = col2rgb(name)[2],
      b = col2rgb(name)[3]
    ) %>%
    ungroup()

  # Define Hama Colors
  hama_colors <- data.frame(
    name = c("black", "white", "red", "blue", "yellow", "green", 'darkgreen', "brown", "cyan", "beige",
             "pink", "orchid", "orange", "purple", "lightblue", "lightgreen", "grey", "darkgrey", "darkred",
             "azure", "plum", "gold","turquoise" , "ivory", "lightsteelblue", "lavender", "darkolivegreen",
             "mintcream","lightyellow","peachpuff", "mistyrose","ghostwhite")
  )

  # hama color rgb
  hama_colors_rgb <- all_colors_rgb %>% filter(name %in% hama_colors$name)

  # Function to find the closest Hama bead color
  closest_color <- function(r, g, b, palette) {
    distances <- sqrt((r - palette$r)^2 + (g - palette$g)^2 + (b - palette$b)^2)
    palette$name[which.min(distances)]
  }

  # Reactively process the uploaded image
  observeEvent(input$process, {
    req(input$upload)

    # Get the selected size
    selected_size <- input$size

    # Read and resize the uploaded image based on the selected size
    img <- image_read(input$upload$datapath) %>%
      image_resize(paste0(selected_size,"!"))


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
        ClosestColor = mapply(closest_color, R, G, B, MoreArgs = list(palette = hama_colors_rgb))
      )

    # Set stroke size based on image size
    stroke_size <- ifelse(selected_size == "57x57", 1, 3)

    # Render the plot
    output$hama_plot <- renderPlot({
      ggplot(img_df, aes(x = X, y = Y)) +
        geom_point(shape = 21, size = 3, stroke = stroke_size, color = img_df$ClosestColor) +  # Shape 21 for filled circles
        scale_y_reverse() +       # Flip the y-axis to match image orientation
        coord_fixed() +           # Keep aspect ratio fixed
        theme_void() +
        theme(legend.position = "none") +  # Remove the legend
        ggtitle(paste(selected_size, "with Closest Hama Colors"))
    })

    # Download handler for the plot
    output$download_plot <- downloadHandler(
      filename = function() {
        # Generate the filename based on the uploaded file name
        # Remove the file extension from the uploaded file name
        base_name <- tools::file_path_sans_ext(basename(input$upload$name))
        # Append the current date and ".png" to create a unique filename
        paste0(base_name, "_hama_bead_pattern_", Sys.Date(),"_",selected_size, ".png")
      },
      content = function(file) {
        ggsave(file, plot = last_plot(), width = 8, height = 8, units = "in", dpi = 300)
      }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
