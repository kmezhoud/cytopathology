
#' get segmented image using k-means cluter method
#'
#' @param file The image that with be segmented as jpg or png file
#' @param k    The number of classes or labels that with image will be segmented
#'
#' @return A list of k segmented images and a list of k arrays corresponding to the k segmented images.
#' @export
#'
#' @examples
get_kseg_image <- function(file, k){
  
  im <- EBImage::readImage(file)
  # reshape image into a data frame
  df <- data.frame(
    red = matrix(im[,,1], ncol=1),
    green = matrix(im[,,2], ncol=1),
    blue = matrix(im[,,3], ncol=1)
  )
  
  ### compute the k-means clustering
  set.seed(1234)
  K = kmeans(df,k)
  df$label = K$cluster
  
  ### Replace the color of each pixel in the image with the mean 
  ### R,G, and B values of the cluster in which the pixel resides:
  
  # get the coloring
  colors = data.frame(
    label = 1:nrow(K$centers), 
    R = K$centers[,"red"],
    G = K$centers[,"green"],
    B = K$centers[,"blue"]
  )
  
  # merge color codes on to df
  # IMPORTANT: we must maintain the original order of the df after the merge!
  
  df$order <- 1:nrow(df)
  df <- merge(df, colors)
  
  ## define a list of element where we will store segmented matrices
  M <- vector("list", k)
  IM <- vector("list", k)
  for(i in seq(k)){
    
    m <- df
    
    ## split image into two dataframe df.x contain 1 label, df.y contain the remain labels
    m.x <- m[m$label == i,]
    m.y <- m[!m$label == i,]
    
    ## replace value in df.y  to 255 (white)
    m.y[,c('red','green', 'blue', 'R', 'G', 'B')] <- 255
    m <- rbind(m.x, m.y)
    
    # reorder the matrix (image)
    m <- m[order(m$order),]
    m$order = NULL
    
    # get mean color channel values for each row of the df.
    R <- matrix(m$R, nrow=dim(im)[1])
    G <- matrix(m$G, nrow=dim(im)[1])
    B <- matrix(m$B, nrow=dim(im)[1])
    
    # reconstitute the segmented image in the same shape as the input image
    im.segmented <- array(dim=dim(im))
    im.segmented[,,1] = R
    im.segmented[,,2] = G
    im.segmented[,,3] = B
    
    IM[[paste0("cluster",i, sep="")]] <- im.segmented
    # convert array to cimg for imager package from dim(x,y,channel) to dim(x, y, 1, channels)
    # ignore warning message if necessary
    #suppressWarnings(im.segmented_cimg <- imager::as.cimg(im.segmented))
    M[[paste0("cluster",i, sep="")]] <- EBImage::Image(im.segmented, colormode=Color)
    
    im.segmented <- EBImage::Image(im.segmented, colormode=Color)
   # plot(im.segmented, title= paste0("Cluster: " ,i, sep= "~"))
   # text(60, 5, paste0("Cluster: " ,i, sep= "~"), cex = 1.5)
  }
  return(list(seg_array = IM, seg_image = M))
}



#' Merge two images (supeimpose)
#'
#' @param img1 image with or without 3 channels (RGB color)
#' @param img2 image with or without 3 channels (RGB color)
#'
#' @return     Grayscale image
#' @export
#'
#' @examples
merge2img <- function(img1, img2){
  
  img1 <- EBImage::channel(img1, mode= "gray")
  img2 <- EBImage::channel(img2, mode= "gray")
  norImg1 <- EBImage::normalize(img1)
  norImg2 <- EBImage::normalize(img2)
  IMG <- EBImage::stackObjects(norImg1, norImg2)

  return(IMG)
  
}


