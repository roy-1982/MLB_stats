pacman::p_load(rvest, dplyr, tidyr, showtext,tibble,factoextra)

font_add_google("Dela Gothic One", "dela")
font_add_google("M PLUS 1p", "mplus")
showtext_auto() 

df <- read.csv("~/Desktop/R Studio/CSV/MLB_stats.csv", check.names = FALSE)

# colnames(df)
# colSums(is.na(df))
df <- df %>% 
  filter(pa > 500) %>% 
  arrange(desc(xwoba)) %>%  #打席数500以上でxwobaがtop50
  head(50) %>% 
  select(-c(player_id, year, pa)) %>% 
  column_to_rownames(var = "last_name, first_name") 
  

fviz_nbclust(scale(df), kmeans, method = "silhouette") 

set.seed(123)
km_res <- kmeans(scale(df), centers = 6, nstart = 25) 
km_res$centers

res_pca <- prcomp(df, scale. = TRUE) 
summary(res_pca)
res_pca$rotation[, 1:2] 
# 因子負荷量
loadings <- res_pca$rotation %*% diag(res_pca$sdev)
colnames(loadings) <- paste0("PC", 1:ncol(loadings))
print(loadings) 


if (res_pca$rotation["barrel_batted_rate", 1] < 0) {
  res_pca$x[, 1] <- -res_pca$x[, 1]
  res_pca$rotation[, 1] <- -res_pca$rotation[, 1]
}

cluster_labels <- c(
  "1" = "俊足・積極アタッカー",
  "2" = "剛腕・爆速ライナー",
  "3" = "精密コンタクト職人",
  "4" = "高次元バランス・巧打者",
  "5" = "絶対的規格外（ユニコーン）",
  "6" = "ベテラン主軸・パワー専科"  
)

fviz_pca_biplot(res_pca,
                label = c("ind", "var"),             
                habillage = as.factor(km_res$cluster),
                addEllipses = F,          
                repel = TRUE,
                pointsize = 1.5,           
                labelsize = 3,
                col.var = "orange", 
                alpha.var = 0.3,
                arrowsize = 0.8,
) +
  scale_color_discrete(labels = cluster_labels) +
  scale_fill_discrete(labels = cluster_labels) +
  labs(
    title = "MLB 選手別スタッツマップ (2025)",
    subtitle = "抽出条件：規定打席(PA>500)かつ期待加重出塁率(xwOBA)上位50名",
    x = "PC1: 打撃の破壊力（← 技巧派・コンタクト重視 | 怪物・パワーヒッター →）",
    y = "PC2: プレースタイル（← 足・積極性重視 | 出塁・選球眼重視 →）"
  ) +
  theme_minimal(base_family = "dela") +
  theme(
    plot.background = element_rect(fill = "#F8F9FA", color = NA),
    panel.background = element_rect(fill = "#F8F9FA", color = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 10, family = 'mplus', hjust = 0.5),
    plot.title = element_text(family = "dela", size = 24, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 18)),
    axis.title = element_text(family = "", size = 14),
    plot.margin = margin(20, 20, 10, 15)
  ) +
  guides(
    color = guide_legend(title = NULL), 
    fill = guide_legend(title = NULL),
    shape = "none" 
  )
# ggsave("~/Desktop/MLB2025.png", width = 1150, height = 950, units = "px", dpi = 96)



df_clusters <- data.frame(
  Player = rownames(df),
  Cluster_No = km_res$cluster
) %>%
  mutate(Cluster_Name = cluster_labels[as.character(Cluster_No)]) %>%
  arrange(Cluster_No, Player)
cat("\n--- クラスター別 選手リスト ---\n")
print.data.frame(df_clusters, row.names = FALSE)

