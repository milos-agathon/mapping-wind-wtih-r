#############################################
# MAPPING WIND DIRECTION & SPEED WITH R
# Milos Popovic 2023/08/04
#############################################

# 1. INSTALL & LOAD LIBRARIES
#----------------------------

# Please install the latest version
remotes::install_github("bluegreen-labs/ecmwfr")

libs <- c(
    "ecmwfr", "tidyverse", "metR",
    "terra", "sf", "giscoR", "classInt",
    "lubridate", "hms"
)

installed_libs <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libs == F)){
    install.packages(
        libs[!installed_libs]
    )
}

invisible(lapply(libs, library, character.only = T))

# 1. QUERY WIND DATA
#--------------------

# my_api <- "*****" # NOT NEEDED
my_key <- "************" # PLEASE INSERT YOUR API KEY

ecmwfr::wf_set_key(
    # user = my_api,
    key = my_key
)

time <- seq(
    from = lubridate::ymd_hms(
        paste(2023, 7, 29, 00, 00, 00,
        sep = "-"
        )
    ), 
    to = lubridate::ymd_hms(
        paste(2023, 7, 29, 23, 00, 00,
        sep = "-"
        )
    ),
    by = "1 hours"
)

hours <- hms::as_hms(time)

# 32.002078,-25.048828,72.180823,48.164063

ymin <- 32
xmin <- -25
ymax <- 72
xmax <- 48

request <- list(
    product_type = "reanalysis",
    format = "netcdf",
    variable = c(
        "10m_u_component_of_wind",
        "10m_v_component_of_wind"
    ),
    year = "2023",
    month = "7",
    day = "29",
    time = hours,
    area = c(
        ymax, xmin, ymin, xmax
    ),
    dataset_short_name = "reanalysis-era5-single-levels",
    target = "europe-wind.nc"
)

ecmwfr::wf_request(
    request = request,
    transfer = TRUE,
    #user = my_api, NOT NEEDED
    path = getwd()
)

# 2. LOAD WIND DATA
#--------------------

europe_wind <- terra::rast(
    "europe-wind.nc"
)

europe_wind_df <- europe_wind |>
    as.data.frame(
        xy = T, na.rm = T
    )

head(europe_wind_df)

# 3. GET AVERAGE U & V COMPONENT
#-------------------------------

u <- europe_wind_df |>
    dplyr::select(
        x, y, 
        dplyr::starts_with(
            "u"
        )
    ) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "time",
        values_to = "u10"
    ) |>
    dplyr::group_by(x, y, .groups = "keep") |>
    dplyr::summarise(
        u = mean(u10)
    ) |>
    dplyr::rename(
        "lon" = "x",
        "lat" = "y"
    ) |>
    dplyr::select(
        lon, lat, u
    )

head(u)

v <- europe_wind_df |>
    dplyr::select(
        x, y, 
        dplyr::starts_with(
            "v"
        )
    ) |>
    tidyr::pivot_longer(
        !c("x", "y"),
        names_to = "time",
        values_to = "v10"
    ) |>
    dplyr::group_by(x, y, .groups = "keep") |>
    dplyr::summarise(
        v = mean(v10)
    ) |>
    dplyr::rename(
        "lon" = "x",
        "lat" = "y"
    ) |>
    dplyr::select(
        lon, lat, v
    )

head(v)

# 4. MERGE U & V COMPONENT
#-------------------------

europe_wind_stream <- dplyr::inner_join(
    u, v, by = c("lon", "lat"),
    relationship = "many-to-many"
) |>
dplyr::as_tibble()

# 5. EUROPE SHAPEFILE
#--------------------

get_europe_sf <- function(){
    europe_sf <- giscoR::gisco_get_countries(
        region = c("Europe", "Asia"),
        resolution = "3"
    )

    return(europe_sf)
}

europe_sf <- get_europe_sf()

# 6. BREAKS
#----------

europe_wind_stream$speed <- 
sqrt(europe_wind_stream$u^2 + europe_wind_stream$v^2)

breaks <- classInt::classIntervals(
    europe_wind_stream$speed,
    n = 6,
    style = "equal"
)$brks

# 6. MAP
#-------

p <- ggplot(data = europe_wind_stream) +
    geom_sf(
        data = europe_sf,
        fill = NA,
        color = "grey40",
        size = .25,
        alpha = .99
    ) +
    metR::geom_streamline(
        aes(
            x = lon,
            y = lat,
            dx = u,
            dy = v,
            color = sqrt(..dx..^2 + ..dy..^2),
            alpha = sqrt(..dx..^2 + ..dy..^2),
            linewidth = sqrt(..dx..^2 + ..dy..^2),
        ),
        L = 2,
        res = 10,
        n = 60,
        arrow = NULL,
        lineend = "round",
        inherit.aes = F
    ) +
    scale_color_gradientn(
        name = "Average speed (m/s)",
        colours = hcl.colors(
            12, "Plasma"
        ),
        breaks = round(breaks, 0)
    ) +
    scale_alpha(
        range = c(.2, 1)
    ) +
    scale_linewidth(
        range = c(.1, .5)
    ) +
    coord_sf(
        crs = 4326,
        xlim = c(xmin, xmax),
        ylim = c(ymin, ymax)
    ) +
    guides(
        alpha = "none",
        linewidth = "none",
        color = guide_colorbar(
            direction = "horizontal",
            title.position = "top",
            label.position = "bottom",
            title.hjust = .5,
            label.hjust = 0,
            nrow = 1
        )
    ) + theme_void() +
    theme(
        legend.position = c(.85, 1.01),
        legend.title = element_text(
            size = 11,
            color = "white"
        ),
        legend.text = element_text(
            size = 9,
            color = "white"
        ),
        plot.title = element_text(
            size = 16,
            color = "white",
            hjust = .1,
            vjust = -1
        ),
        plot.subtitle = element_text(
            size = 9,
            color = "white",
            hjust = .2,
            vjust = -1
        ),
        plot.background = element_rect(
            fill = "black",
            color = NA
        ),
        plot.margin = unit(
            c(
                t = 0, r = -3,
                b = -3, l = -3
            ), "lines"
        )
    ) +
    labs(
        title = "Wind speed on 29 July 2023",
        subtitle = "Source: Climate Change Service, ERA5 hourly data on single levels from 1940 to present",
        x = "",
        y = ""
    )
