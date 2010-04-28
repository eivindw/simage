import simage.io.SImageIO._
import simage.structs._
import simage.structs.StrElType._

val img = loadImageFile("cell.jpg")
val se = StrEl(Square, 3)

saveImage(img.avg(se), "cell_avg.jpg")
saveImage(img.erode(se), "cell_erode.jpg")
saveImage(img.dilate(se), "cell_dilate.jpg")
saveImage(img.close(se), "cell_close.jpg")
saveImage(img.open(se), "cell_open.jpg")
saveImage(img.topHat(se), "cell_tophat.jpg")
saveImage(img.botHat(se), "cell_bothat.jpg")
