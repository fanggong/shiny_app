set.seed(2414)
dat <- data.frame(
  long = rnorm(100), 
  lat = rnorm(100),
  facet_x = sample(c("East", "West"), 100, replace = TRUE),
  facet_y = sample(c("Sourth", "North"), 100, replace = TRUE),
  shape = sample(c("ShapeX", "ShapeY", "ShapeZ"), 100, replace = TRUE),
  color = sample(c("SizeX", "SizeY", "SizeZ"), 100, replace = TRUE)
)
ggplot(dat, aes(x = long, y = lat, shape = shape, color = color)) + 
  geom_point() +
  labs(
    title = "this is TITLE",
    subtitle = "this is SUBTITLE",
    caption = "this is CAPTION",
    tag = "this is TAG"
  ) +
  facet_grid(rows = vars(facet_y), cols = vars(facet_x)) + 
  theme(
    line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"),
    rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
    text = element_text(family = "", face = "plain", colour = "black", size = 11, lineheight = 0.9, 
                        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
    title = element_text(),
    aspect.ratio = NULL,
    
    axis.title = element_text(),
    axis.title.x = element_text(margin = margin(t = 5.5/2), vjust = 1),
    axis.title.x.top = element_text(margin = margin(b = 5.5/2), vjust = 0),
    # axis.title.x.bottom = element_text(),
    axis.title.y = element_text(angle = 90, margin = margin(r = 5.5/2), vjust = 1),
    # axis.title.y.left = element_text(),
    axis.title.y.right = element_text(angle = 90, margin = margin(r = 5.5/2), vjust = 1),
    
    axis.text = element_text(size = rel(0.8), colour = "grey30"),
    axis.text.x = element_text(margin = margin(t = 0.8 * 5.5 / 2), vjust = 1),
    axis.text.x.top = element_text(margin = margin(b = 0.8 * 5.5 / 2), vjust = 0),
    # axis.text.x.bottom = element_text(),
    axis.text.y = element_text(margin = margin(r = 0.8 * 5.5 / 2), hjust = 1),
    # axis.text.y.left = element_text(),
    axis.text.y.right = element_text(margin = margin(l = 0.8 * 5.5 / 2), hjust = 0),
    
    axis.ticks = element_line(colour = "grey20"),
    # axis.ticks.x = element_line(),
    # axis.ticks.x.top = element_line(colour = "black"),
    # axis.ticks.x.bottom = element_line(),
    # axis.ticks.y = element_line(),
    # axis.ticks.y.left = element_line(),
    # axis.ticks.y.right = element_line(),
    
    axis.ticks.length = unit(5.5 / 2, "pt"),
    # axis.ticks.length.x = unit(10, "pt"),
    # axis.ticks.length.x.top = unit(20, "pt"),
    # axis.ticks.length.x.bottom = unit(1, "pt"),
    # axis.ticks.length.y = unit(1, "pt"),
    # axis.ticks.length.y.left = unit(1, "pt"),
    # axis.ticks.length.y.right = unit(1, "pt"),
    axis.ticks.length.x = NULL,
    axis.ticks.length.x.top = NULL,
    axis.ticks.length.x.bottom = NULL,
    axis.ticks.length.y = NULL,
    axis.ticks.length.y.left = NULL,
    axis.ticks.length.y.right = NULL,

    # axis.line = element_line(),
    axis.line = element_blank(),
    # axis.line.x = element_line(),
    # axis.line.x.top = element_line(),
    # axis.line.x.bottom = element_line(),
    axis.line.x = NULL,
    # axis.line.y = element_line(),
    # axis.line.y.left = element_line(),
    # axis.line.y.right = element_line(),
    axis.line.y = NULL,
    
    legend.background = element_rect(colour = NA),
    
    legend.margin = margin(5.5, 5.5, 5.5, 5.5, "pt"),
    
    legend.spacing = unit(2 * 5.5, "pt"),
    # legend.spacing.x = unit(1, "pt"),
    # legend.spacing.y = unit(1, "pt"),
    legend.spacing.x = NULL,
    legend.spacing.y = NULL,
    
    legend.key = element_rect(fill = "grey95", colour = NA),
    legend.key.size = unit(1.2, "lines"),
    # legend.key.height = unit(1, "pt"),
    # legend.key.width = unit(1, "pt"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    
    legend.text = element_text(size = rel(0.8)),
    # legend.text.align = 0.5,
    legend.text.align = NULL,
    
    legend.title = element_text(hjust = 0),
    legend.title.align = 0.5,
    
    legend.position = "right",
  
    legend.direction = "vertical",
    
    legend.justification = "center",
    
    legend.box = NULL,
    legend.box.just = NULL,
    
    legend.box.margin = margin(0, 0, 0, 0, "cm"),
    # legend.box.background = element_rect(),
    legend.box.background = element_blank(),
    legend.box.spacing = unit(2 * 5.5, "pt"),
    
    panel.background = element_rect(fill = "grey92", colour = NA),
    # panel.border = element_rect(),
    panel.border = element_blank(),
    
    panel.spacing = unit(5.5, "pt"),
    # panel.spacing.x = unit(1, "pt"),
    # panel.spacing.y = unit(1, "pt"),
    panel.spacing.x = NULL,
    panel.spacing.y = NULL,
    
    panel.grid = element_line(colour = "white"),
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(size = rel(0.5)),
    panel.grid.major.x = element_line(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.x = element_line(),
    panel.grid.minor.y = element_line(),
    
    panel.ontop = FALSE,
    
    plot.background = element_rect(colour = "white"),
    plot.title = element_text(size = rel(1.2), hjust = 0, vjust = 1, margin = margin(b = 5.5)),
    plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(b = 5.5)),
    plot.caption = element_text(size = rel(0.8), hjust = 1, vjust = 1, margin = margin(t = 5.5)),
    plot.tag = element_text(size = rel(1.2), hjust = 0.5, vjust = 0.5),
    plot.title.position = "panel",
    plot.caption.position = "panel",
    plot.tag.position = "topleft",
    
    plot.margin = margin(5.5, 5.5, 5.5, 5.5, "pt"),
    
    strip.background = element_rect(fill = "grey85", colour = NA),
    strip.background.x = element_rect(),
    strip.background.y = element_rect(),
    
    strip.placement = "inside",
    strip.placement.x = NULL, 
    strip.placement.y = NULL,
    
    strip.text = element_text(colour = "grey10", size = rel(0.8), 
                              margin = margin(0.8 * 5.5,0.8 * 5.5, 0.8 * 5.5, 0.8 * 5.5)),
    # strip.text.x = element_text(),
    strip.text.x = NULL,
    strip.text.y = element_text(angle = -90),
    strip.text.y.left = element_text(angle = 90),
    
    strip.switch.pad.grid = unit(5.5/2, "pt"),
    strip.switch.pad.wrap = unit(5.5/2, "pt"),
    complete = TRUE
  )

