# multicrit_shiny_resuss
malaysia_shp = malariaAtlas::getShp(country="Malaysia", admin_level = c("admin1"))
brunei_shp = malariaAtlas::getShp(country="Brunei Darussalam")
malaysia_shp = malaysia_shp[malaysia_shp$name_1 %in% c("Sarawak","Sabah"),]
borneo_remainder = rbind(malaysia_shp, brunei_shp)
borneo_sf = st_as_sf(borneo_remainder)

st_write(borneo_sf, "borneo_MB.shp")
