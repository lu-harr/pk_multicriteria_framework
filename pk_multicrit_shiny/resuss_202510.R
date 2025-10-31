# multicrit_shiny_resuss
malaysia_shp = malariaAtlas::getShp(country="Malaysia", admin_level = c("admin1"))
brunei_shp = malariaAtlas::getShp(country="Brunei Darussalam")
malaysia_shp = malaysia_shp[malaysia_shp$name_1 %in% c("Sarawak","Sabah"),]
borneo_remainder = rbind(malaysia_shp, brunei_shp)
borneo_sf = st_as_sf(borneo_remainder)

st_write(borneo_sf, "borneo_MB.shp")

row.match <- function (x, table, nomatch = NA) 
{
  if (inherits(table, "matrix")) 
    table <- as.data.frame(table)
  if (is.null(dim(x))) 
    x <- as.data.frame(matrix(x, nrow = 1))
  cx <- do.call("paste", c(x[, , drop = FALSE], sep = "\r"))
  ct <- do.call("paste", c(table[, , drop = FALSE], sep = "\r"))
  match(cx, ct, nomatch = nomatch)
}
