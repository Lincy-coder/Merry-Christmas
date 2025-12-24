# 安装和加载必要的包
required_packages <- c("ggplot2", "gganimate", "dplyr", "tidyr", "showtext", "gifski", "av")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# 加载字体
font_add_google("Dancing Script", "cursive_font")  
showtext_auto()

# ================= 风格配置 (保持不变) =================
styles <- list(
  "1" = list(bg_col = "#0f0a19", tree_cols = c("#0B3D0B", "#144514", "#006400", "#228B22"), trunk_col = "#3e2723", decor_cols = c("#FFD700", "#FFA500", "#FF8C00", "#FF4500"), star_col = "#FFD700", text_col = "#FFD700", snow_col = "white", ribbon = TRUE, ribbon_col = "#D4AF37", ribbon_width = 1.5, light_cols = c("#FFD700", "#FFA500", "#FF8C00")),
  "2" = list(bg_col = "#0a1220", tree_cols = c("#2F4F4F", "#5F9EA0", "#708090", "#87CEEB"), trunk_col = "#696969", decor_cols = c("#B0C4DE", "#E6E6FA", "#F0F8FF", "#F8F8FF"), star_col = "#B0C4DE", text_col = "#B0C4DE", snow_col = "#E0FFFF", ribbon = TRUE, ribbon_col = "#C0C0C0", ribbon_width = 1.5, light_cols = c("#B0C4DE", "#E6E6FA", "#F0F8FF")),
  "3" = list(bg_col = "#1a0f0a", tree_cols = c("#556B2F", "#6B8E23", "#8B7355", "#A0522D"), trunk_col = "#5D4037", decor_cols = c("#CD853F", "#DAA520", "#B8860B", "#8B4513"), star_col = "#FFCC00", text_col = "#DEB887", snow_col = "#FAF0E6", ribbon = FALSE, ribbon_col = NA, ribbon_width = 0, light_cols = c("#CD853F", "#DAA520", "#B8860B")),
  "4" = list(bg_col = "#050814", tree_cols = c("#006400", "#228B22", "#32CD32", "#7CFC00"), trunk_col = "#4E342E", decor_cols = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#FFFF00"), star_col = "#FFD700", text_col = "#FF6347", snow_col = "white", ribbon = TRUE, ribbon_col = "#9370DB", ribbon_width = 1.0, light_cols = c("#FF0000", "#00FF00", "#0000FF", "#FF00FF")),
  "5" = list(bg_col = "#000000", tree_cols = c("#111111", "#222222", "#333333", "#444444"), trunk_col = "#222222", decor_cols = c("#FFFFFF", "#F0F0F0", "#E0E0E0", "#D0D0D0"), star_col = "#FFFFFF", text_col = "#FFFFFF", snow_col = "#888888", ribbon = TRUE, ribbon_col = "#FFFFFF", ribbon_width = 0.8, light_cols = c("#FFFFFF", "#F0F0F0", "#E0E0E0")),
  "6" = list(bg_col = "#1F0F12", tree_cols = c("#D87093", "#FF69B4", "#FFB6C1", "#FFC0CB"), trunk_col = "#4A3728", decor_cols = c("#FFFFFF", "#FFD700", "#FF1493", "#FFB6C1"), star_col = "#FFD700", text_col = "#FFC0CB", snow_col = "#FFF0F5", ribbon = TRUE, ribbon_col = "#F8F8FF", ribbon_width = 0.8, light_cols = c("#FFD700", "#FF1493", "#FFB6C1"))
)

set.seed(2025)

# ================= 辅助函数 =================
get_star_polygon <- function(x_center, y_center, radius) {
  angles <- seq(pi/2, 2.5 * pi, length.out = 11)[-11]
  radii <- rep(c(radius, radius * 0.4), 5)
  data.frame(
    x = x_center + radii * cos(angles),
    y = y_center + radii * sin(angles)
  )
}

generate_tree_data <- function(cfg, style_id) {
  # 减少每棵树的点数以优化性能（总体点数是原来的6倍）
  n_leaves <- 4000 
  # ... (保持树生成逻辑一致)
  h <- runif(n_leaves, 0, 1)
  base_r <- (1 - h)
  layer_cycle <- (h * 8) %% 1
  r <- base_r * 0.6 * (0.5 + 0.5 * (1 - layer_cycle)^0.5)
  theta <- runif(n_leaves, 0, 2 * pi)
  
  df_tree <- data.frame(
    x=r*cos(theta), y=h-0.5, z=r*sin(theta),
    col=sample(cfg$tree_cols, n_leaves, replace=TRUE),
    size=runif(n_leaves, 0.4, 1.2), type="tree", alpha=0.8, style=style_id
  )
  
  # 树干
  n_trunk <- 300
  df_trunk <- data.frame(
    x=0.1*cos(runif(n_trunk,0,2*pi)), y=runif(n_trunk,-0.7,-0.45), z=0.1*sin(runif(n_trunk,0,2*pi)),
    col=cfg$trunk_col, size=0.8, type="trunk", alpha=1, style=style_id
  )
  
  # 装饰
  n_decor <- 200
  h_dec <- runif(n_decor, 0.1, 0.95)
  r_dec <- (1-h_dec)*0.58*(0.6+0.4*runif(n_decor))
  theta_dec <- runif(n_decor,0,2*pi)
  df_decor <- data.frame(
    x=r_dec*cos(theta_dec), y=h_dec-0.5, z=r_dec*sin(theta_dec),
    col=sample(cfg$decor_cols, n_decor, replace=TRUE),
    size=runif(n_decor, 1.5, 2.5), type="decor", alpha=1, style=style_id
  )
  
  # 丝带
  df_ribbon <- NULL
  if (cfg$ribbon) {
    n_rib <- 1000
    h_rib <- seq(0.1, 0.95, length.out=n_rib)
    theta_rib <- 8*pi*h_rib
    df_ribbon <- data.frame(
      x=(1-h_rib)*0.58*cos(theta_rib), y=h_rib-0.5, z=(1-h_rib)*0.58*sin(theta_rib),
      col=cfg$ribbon_col, size=cfg$ribbon_width, type="ribbon", alpha=0.7, style=style_id
    )
  }
  
  # 灯光
  n_lights <- 150
  h_l <- runif(n_lights, 0.15, 0.9)
  r_l <- (1-h_l)*0.62*(0.7+0.3*runif(n_lights))
  th_l <- runif(n_lights,0,2*pi)
  df_lights <- data.frame(
    x=r_l*cos(th_l), y=h_l-0.5, z=r_l*sin(th_l),
    col=sample(cfg$light_cols, n_lights, replace=TRUE),
    size=runif(n_lights, 1.5, 2.5), type="light", alpha=1, style=style_id
  )
  
  bind_rows(df_trunk, df_tree, df_decor, df_ribbon, df_lights)
}

# ================= 数据汇总 =================

# 定义网格坐标 (2行3列)
grid_offsets <- data.frame(
  style = as.character(1:6),
  off_x = c(-1.8, 0, 1.8, -1.8, 0, 1.8),
  off_y = c(0.8, 0.8, 0.8, -0.8, -0.8, -0.8)
)

all_static_data <- list()
all_stars_data <- list()
all_snow_data <- list()

for(i in 1:6) {
  cfg <- styles[[as.character(i)]]
  off <- grid_offsets[i,]
  
  # 树主体数据
  all_static_data[[i]] <- generate_tree_data(cfg, i) %>%
    mutate(off_x = off$off_x, off_y = off$off_y)
  
  # 星星数据 (直接偏移)
  star_poly <- get_star_polygon(off$off_x, off$off_y + 0.45, 0.08)
  star_poly$style <- i
  star_poly$col <- cfg$star_col
  all_stars_data[[i]] <- star_poly
  
  # 雪花数据 (针对每棵树局部生成)
  n_snow <- 100
  all_snow_data[[i]] <- data.frame(
    x = runif(n_snow, off$off_x - 0.8, off$off_x + 0.8),
    y = runif(n_snow, off$off_y - 0.8, off$off_y + 0.8),
    z = runif(n_snow, -1, 1),
    col = cfg$snow_col, size = runif(n_snow, 0.3, 1.2),
    type = "snow", alpha = 0.8, speed = runif(n_snow, 0.01, 0.03), style = i,
    off_x = off$off_x, off_y = off$off_y
  )
}

merged_static <- bind_rows(all_static_data)
merged_snow <- bind_rows(all_snow_data)
merged_stars <- bind_rows(all_stars_data)

# ================= 动画每一帧处理 =================
n_frames <- 100
fps_val <- 20

process_frame <- function(frame_id) {
  angle <- 2 * pi * (frame_id / n_frames)
  
  # 旋转并投影静态物体（树）
  tree_proj <- merged_static %>%
    mutate(
      x_rot = x * cos(angle) - z * sin(angle),
      z_rot = z * cos(angle) + x * sin(angle),
      depth = 1 / (3.5 - z_rot),
      x_proj = off_x + x_rot * depth * 2.2,
      y_proj = off_y + y * depth * 2.2,
      size_vis = size * depth * 1.5,
      alpha_vis = alpha * ((z_rot + 1.5)/3)
    )
  
  # 雪花逻辑
  snow_proj <- merged_snow %>%
    mutate(
      y_curr = off_y - 0.8 + (y - frame_id * speed - (off_y - 0.8)) %% 1.6,
      x_swing = x + 0.05 * sin(frame_id * 0.1 + y * 5),
      depth = 1 / (3.5 - z),
      x_proj = x_swing, 
      y_proj = y_curr,
      size_vis = size,
      alpha_vis = alpha
    )
  
  bind_rows(tree_proj, snow_proj) %>%
    mutate(frame = frame_id)
}

all_frames <- lapply(1:n_frames, process_frame) %>% bind_rows()

# ================= 绘制动画 =================
p <- ggplot() +
  # 绘制六棵树及其雪花
  geom_point(
    data = all_frames,
    aes(x = x_proj, y = y_proj, color = I(col), size = I(size_vis), alpha = I(alpha_vis)),
    shape = 19
  ) +
  # 绘制六颗星星
  geom_polygon(
    data = merged_stars,
    aes(x = x, y = y, group = style, fill = I(col)),
    color = "white", size = 0.2
  ) +
  # 添加标题
  annotate("text", x = 0, y = 1.6, label = "Merry Christmas!", 
           family = "cursive_font", color = "white", size = 12, fontface = "bold") +
  # 限制坐标系展示所有树
  coord_fixed(xlim = c(-3, 3), ylim = c(-2, 2)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#050510", color = NA), # 统一采用极深背景
    panel.background = element_rect(fill = "#050510", color = NA)
  ) +
  transition_manual(frame)

# ================= 渲染保存 =================
animation <- animate(
  p,
  nframes = n_frames, fps = fps_val,
  width = 1000, height = 700,
  renderer = gifski_renderer(loop = TRUE)
)

anim_save("six_christmas_trees.gif", animation)
# 保存动画为MP4格式，适合在视频平台分享
library(av)
av::av_encode_video(input = animation, output = "six_christmas_trees.mp4", framerate = 30)
animation
