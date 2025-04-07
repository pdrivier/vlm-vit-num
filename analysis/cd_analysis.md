---
title: "Multimodal Models and Numerosity"
date: "September 12, 2024"
output:
  # pdf_document: 
  #    fig_caption: yes
  #    keep_md: yes
  #    keep_tex: yes
  html_document:
     keep_md: yes
     toc: yes
     toc_float: yes

---






# Load data


```r
# setwd("/Users/seantrott/Dropbox/UCSD/Research/NLMs/vlm-vit-num/analysis")
directory_path <- "../results"
csv_files <- list.files(path = directory_path, pattern = "*.csv", full.names = TRUE)
csv_list <- csv_files %>%
  map(~ read_csv(.))
```

```
## New names:
## Rows: 3380 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 8320 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 12740 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 31360 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 10660 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 26240 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 8580 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 21120 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 6500 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 16000 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 3380 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 8320 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 8580 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 21120 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 6500 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 16000 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 6500 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## New names:
## Rows: 16000 Columns: 13
## ── Column specification
## ──────────────────────────────────────────────────────── Delimiter: "," chr
## (5): model_name, image_type, image_1, image_2, numerosity_comparison_type dbl
## (8): ...1, cosine_similarity, numerosity_1, numerosity_2, area_diff, lay...
## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
## Specify the column types or set `show_col_types = FALSE` to quiet this message.
## • `` -> `...1`
```

```r
df_hf_models <- bind_rows(csv_list)


df_hf_models = df_hf_models %>%
  mutate(numerosity_diff = abs(numerosity_2 - numerosity_1)) %>%
  mutate(log_params = log10(n_params)) %>%
  group_by(model_name) %>%
  mutate(max_layer = max(layer)) %>%
  mutate(model_type = case_when(
    str_detect(model_name, "clip") == TRUE ~ "VLM",
    TRUE ~ "ViT"))  %>%
  mutate(cosine_similarity_z = scale(cosine_similarity),
         numerosity_diff_z = scale(numerosity_diff),
         area_diff_z = scale(area_diff)) %>%
  mutate(model_name2 = str_extract(model_name, "^(?:[^-]*-?){1,3}[^-]*"))


table(df_hf_models$model_name)
```

```
## 
##  clip-base-patch32    clip-bg-patch14 clip-giant-patch14  clip-huge-patch14 
##              11700              44100              36900              29700 
## clip-large-patch14   vit-base-patch16   vit-huge-patch14  vit-large-patch16 
##              22500              11700              29700              22500 
##  vit-large-patch32 
##              22500
```

```r
table(df_hf_models$model_name, df_hf_models$numerosity_comparison_type)
```

```
##                     
##                      different  same
##   clip-base-patch32       5850  5850
##   clip-bg-patch14        22050 22050
##   clip-giant-patch14     18450 18450
##   clip-huge-patch14      14850 14850
##   clip-large-patch14     11250 11250
##   vit-base-patch16        5850  5850
##   vit-huge-patch14       14850 14850
##   vit-large-patch16      11250 11250
##   vit-large-patch32      11250 11250
```

```r
table(df_hf_models$model_name, df_hf_models$image_type)
```

```
##                     
##                       dots rectangles
##   clip-base-patch32   3380       8320
##   clip-bg-patch14    12740      31360
##   clip-giant-patch14 10660      26240
##   clip-huge-patch14   8580      21120
##   clip-large-patch14  6500      16000
##   vit-base-patch16    3380       8320
##   vit-huge-patch14    8580      21120
##   vit-large-patch16   6500      16000
##   vit-large-patch32   6500      16000
```


# Descriptive analyses


```r
df_hf_models %>%
  filter(layer == max_layer) %>%
  ggplot(aes(x = cosine_similarity)) +
  geom_histogram(alpha = .6) +
  theme_minimal() +
  labs(x = "Cosine Similarity") +
  theme(text = element_text(size = 15)) +
  facet_wrap(~model_name)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](cd_analysis_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

# Analyses

## Same Vs. Different Numerosity


```r
df_summary <- df_hf_models %>%
  group_by(model_name2, numerosity_comparison_type, layer, max_layer, image_type) %>%
  summarize(avg_similarity = mean(cosine_similarity, na.rm = TRUE),
            se_similarity = sd(cosine_similarity, na.rm = TRUE) / sqrt(n()))
```

```
## `summarise()` has grouped output by 'model_name2',
## 'numerosity_comparison_type', 'layer', 'max_layer'. You can override using the
## `.groups` argument.
```

```r
df_summary %>%
  filter(layer == max_layer) %>%
  ggplot(aes(x = model_name2,
             y = avg_similarity,
             fill = numerosity_comparison_type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.5), width = .6) +  
  # geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = avg_similarity - se_similarity, 
                    ymax = avg_similarity + se_similarity), 
                width = 0.2,
                position = position_dodge(width = 0.5)) + 
  labs(# title = "",
       x = "Model",
       y = "Average Cosine Similarity",
       fill = "",
       color = "") +
  theme_minimal() +
  coord_flip() +
  scale_fill_viridis(discrete=TRUE) +
  # scale_color_viridis_d() +
  scale_y_continuous(n.breaks = 3) +
  theme(axis.title = element_text(size=rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        # legend.title = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = rel(1.2)),
        legend.position = "bottom") +
  facet_wrap(~image_type)
```

![](cd_analysis_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
df_hf_models %>%
  filter(layer == max_layer) %>%
  ggplot(aes(x = cosine_similarity,
             y = model_name,
             fill = numerosity_comparison_type)) +
  geom_density_ridges2(aes(height = ..density..), 
                       color=gray(0.25), 
                       alpha = .7, 
                       scale=.85, 
                       # size=1, 
                       size = 0,
                       stat="density") +
  labs(x = "Cosine Similarity",
       y = "",
       fill = "Comparison Type") +
  theme_minimal() +
  scale_fill_viridis(discrete=TRUE) +
  theme(text = element_text(size = 15),
        legend.position="bottom") +
  facet_wrap(~image_type)
```

```
## Warning in geom_density_ridges2(aes(height = ..density..), color = gray(0.25),
## : Ignoring unknown parameters: `size`
```

```
## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(density)` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](cd_analysis_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

```r
### Overall sensitivity
m1 = lmer(data = df_hf_models,
          cosine_similarity ~ area_diff + numerosity_comparison_type + 
            (1 | image_1) + (1 | image_2) + 
            (1 |model_name) + (1|image_type),
          control=lmerControl(optimizer="bobyqa"))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff + numerosity_comparison_type +  
##     (1 | image_1) + (1 | image_2) + (1 | model_name) + (1 | image_type)
##    Data: df_hf_models
## Control: lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: -648964.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9772 -0.3122  0.1044  0.5046  3.8459 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0004657 0.02158 
##  image_2    (Intercept) 0.0004479 0.02116 
##  model_name (Intercept) 0.0003195 0.01787 
##  image_type (Intercept) 0.0002491 0.01578 
##  Residual               0.0034547 0.05878 
## Number of obs: 231300, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                                  Estimate Std. Error         df t value
## (Intercept)                     9.559e-01  1.270e-02  1.634e+00  75.242
## area_diff                      -4.949e-06  1.049e-06  2.488e+03  -4.717
## numerosity_comparison_typesame  1.658e-02  8.549e-04  2.646e+03  19.388
##                                Pr(>|t|)    
## (Intercept)                    0.000693 ***
## area_diff                      2.52e-06 ***
## numerosity_comparison_typesame  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff
## area_diff   -0.046       
## nmrsty_cmp_ -0.046  0.210
```

```r
### Interaction with layer
m1 = lmer(data = df_hf_models,
          cosine_similarity ~ area_diff *layer + numerosity_comparison_type * layer + 
            (1 | image_1) + (1 | image_2) + 
            (1 |model_name) + (1|image_type))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff * layer + numerosity_comparison_type *  
##     layer + (1 | image_1) + (1 | image_2) + (1 | model_name) +  
##     (1 | image_type)
##    Data: df_hf_models
## 
## REML criterion at convergence: -814492.9
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -11.4532  -0.3767   0.0956   0.4923   4.8630 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0004855 0.02203 
##  image_2    (Intercept) 0.0004651 0.02157 
##  model_name (Intercept) 0.0003162 0.01778 
##  image_type (Intercept) 0.0002612 0.01616 
##  Residual               0.0016785 0.04097 
## Number of obs: 231300, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                                        Estimate Std. Error         df  t value
## (Intercept)                           1.028e+00  1.293e-02  1.615e+00   79.500
## area_diff                            -1.445e-05  1.027e-06  2.521e+03  -14.069
## layer                                -5.100e-03  1.267e-05  2.305e+05 -402.446
## numerosity_comparison_typesame       -8.050e-03  8.407e-04  2.871e+03   -9.575
## area_diff:layer                       6.593e-07  1.552e-08  2.313e+05   42.473
## layer:numerosity_comparison_typesame  1.623e-03  1.535e-05  2.309e+05  105.736
##                                      Pr(>|t|)    
## (Intercept)                           0.00068 ***
## area_diff                             < 2e-16 ***
## layer                                 < 2e-16 ***
## numerosity_comparison_typesame        < 2e-16 ***
## area_diff:layer                       < 2e-16 ***
## layer:numerosity_comparison_typesame  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff layer  nmrs__ ar_df:
## area_diff   -0.044                            
## layer       -0.014  0.092                     
## nmrsty_cmp_ -0.044  0.214  0.170              
## are_dff:lyr  0.006 -0.206 -0.430 -0.021       
## lyr:nmrst__  0.009 -0.021 -0.635 -0.268  0.076
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
## Plot differences by layer
summary_df <- df_hf_models %>%
  group_by(model_name, layer, numerosity_comparison_type) %>%
  summarize(mean_cosine = mean(cosine_similarity, na.rm = TRUE), .groups = 'drop')

difference_df <- summary_df %>%
  pivot_wider(names_from = numerosity_comparison_type, values_from = mean_cosine) %>%
  mutate(difference = same - different) %>%
  group_by(model_name) %>%
  mutate(max_layer = max(layer),
         prop_layer = layer / max_layer) %>%
  mutate(binned_prop_layer = ntile(prop_layer, 10)) %>%
  mutate(prop_binned = binned_prop_layer / 10) 


smoothed_data <- difference_df %>%
  group_by(prop_binned) %>%
  summarize(avg_difference = mean(difference, na.rm = TRUE)) %>%
  mutate(model_name = "Average")

# Create the plot
difference_df %>%
  ggplot(aes(x = prop_binned, y = difference, 
             color = model_name, group = model_name)) +
  geom_line(alpha = 0.5, size=2) + # Individual lines for each model_name
  geom_line(data = smoothed_data, aes(x = prop_binned, y = avg_difference), 
             color = "black", size = 3) + # Smoothed average
  labs(
    title = "",
    x = "Layer Depth Ratio",
    y = "Difference (Same - Different)",
    color = ""
  ) +
  theme_minimal() +
  scale_color_manual(values = viridisLite::viridis(9, option = "mako", begin = 0, end = 0.8))  +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "none")
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](cd_analysis_files/figure-html/unnamed-chunk-4-3.png)<!-- -->


## Continuous function of numerosity



```r
df_hf_models <- df_hf_models %>%
  mutate(model_name2 = str_extract(model_name, "^(?:[^-]*-?){1,3}[^-]*"))

df_hf_models %>%
  filter(layer == max_layer) %>%
  ggplot(aes(x = numerosity_diff,
             y = cosine_similarity,
             color = model_type)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  scale_color_manual(values = viridisLite::viridis(2, option = "mako", begin = 0, end = 0.8)) +
  labs(# title = "",
       x = "Numerosity Difference",
       y = "Cosine Similarity",
       fill = "",
       color = "Model Type") +
  theme(text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~reorder(model_name2, n_params))
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](cd_analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
### Overall sensitivity
m1 = lmer(data = df_hf_models,
          cosine_similarity ~ area_diff + numerosity_diff + 
            (1 | image_1) + (1 | image_2) + 
            (1 |model_name) + (1|image_type),
          control=lmerControl(optimizer="bobyqa"))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff + numerosity_diff + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name) + (1 | image_type)
##    Data: df_hf_models
## Control: lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: -649512.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -8.9699 -0.3114  0.1038  0.5043  3.8516 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0003325 0.01823 
##  image_2    (Intercept) 0.0003363 0.01834 
##  model_name (Intercept) 0.0003178 0.01783 
##  image_type (Intercept) 0.0001416 0.01190 
##  Residual               0.0034544 0.05877 
## Number of obs: 231300, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)      9.725e-01  1.034e-02  2.173e+00  94.060  5.8e-05 ***
## area_diff       -1.082e-06  9.392e-07  2.569e+03  -1.152    0.249    
## numerosity_diff -2.145e-03  6.362e-05  2.371e+03 -33.707  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff
## area_diff   -0.035       
## numrsty_dff -0.015 -0.258
```

```r
### Overall sensitivity
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff + numerosity_diff + 
            (1 | image_1) + (1 | image_2) + 
            (1 |model_name) + (1|image_type),
          control=lmerControl(optimizer="bobyqa"))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff + numerosity_diff + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name) + (1 | image_type)
##    Data: filter(df_hf_models, layer == max_layer)
## Control: lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: -22460
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.0533 -0.3717  0.0626  0.4907  3.3163 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0022208 0.04713 
##  image_2    (Intercept) 0.0023204 0.04817 
##  model_name (Intercept) 0.0007499 0.02738 
##  image_type (Intercept) 0.0012261 0.03502 
##  Residual               0.0023580 0.04856 
## Number of obs: 8100, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                   Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)      9.118e-01  2.651e-02  1.287e+00  34.400 0.007257 ** 
## area_diff       -1.022e-05  2.778e-06  2.918e+03  -3.679 0.000238 ***
## numerosity_diff -5.892e-03  1.864e-04  2.579e+03 -31.614  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff
## area_diff   -0.041       
## numrsty_dff -0.019 -0.236
```

```r
### Interaction
m1 = lmer(data = df_hf_models,
          cosine_similarity ~ area_diff * layer +  numerosity_diff * layer +
            (1 | image_1) + (1 | image_2) + 
            (1 |model_name) + (1|image_type),
          control=lmerControl(optimizer="bobyqa"))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff * layer + numerosity_diff * layer +  
##     (1 | image_1) + (1 | image_2) + (1 | model_name) + (1 | image_type)
##    Data: df_hf_models
## Control: lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: -834543.9
## 
## Scaled residuals: 
##      Min       1Q   Median       3Q      Max 
## -11.9861  -0.3765   0.1007   0.4890   5.4463 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0003409 0.01846 
##  image_2    (Intercept) 0.0003473 0.01864 
##  model_name (Intercept) 0.0003246 0.01802 
##  image_type (Intercept) 0.0001639 0.01280 
##  Residual               0.0015419 0.03927 
## Number of obs: 231300, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                         Estimate Std. Error         df t value Pr(>|t|)    
## (Intercept)            1.018e+00  1.090e-02  2.019e+00   93.35 0.000107 ***
## area_diff             -1.003e-05  9.047e-07  2.623e+03  -11.09  < 2e-16 ***
## layer                 -3.329e-03  1.068e-05  2.304e+05 -311.79  < 2e-16 ***
## numerosity_diff        6.035e-04  6.215e-05  2.589e+03    9.71  < 2e-16 ***
## area_diff:layer        6.084e-07  1.482e-08  2.312e+05   41.04  < 2e-16 ***
## layer:numerosity_diff -1.797e-04  9.967e-07  2.304e+05 -180.32  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff layer  nmrst_ ar_df:
## area_diff   -0.031                            
## layer       -0.014  0.092                     
## numrsty_dff -0.014 -0.263  0.115              
## are_dff:lyr  0.006 -0.223 -0.422  0.006       
## lyr:nmrsty_  0.007  0.012 -0.478 -0.239 -0.028
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```


# VLM vs. ViT

## Same vs. Different


```r
df_hf_models = df_hf_models %>%
  mutate(model_type = case_when(
    str_detect(model_name, "clip") == TRUE ~ "VLM",
    TRUE ~ "ViT")) %>%
  group_by(model_name) %>%
  mutate(max_layer = max(layer))

df_summary <- df_hf_models %>%
  filter(layer == max_layer) %>%
  group_by(model_type, numerosity_comparison_type) %>%
  summarize(avg_similarity = mean(cosine_similarity, na.rm = TRUE),
            se_similarity = sd(cosine_similarity, na.rm = TRUE) / sqrt(n()))
```

```
## `summarise()` has grouped output by 'model_type'. You can override using the
## `.groups` argument.
```

```r
df_summary %>%
  ggplot(aes(x = factor(model_type),
             y = avg_similarity,
             color = numerosity_comparison_type)) +
  geom_point(position = position_dodge(width = 0.5), size = 6) +  
  geom_errorbar(aes(ymin = avg_similarity - 2 * se_similarity, 
                    ymax = avg_similarity + 2 * se_similarity), 
                width = 0.6,
                size = 2,
                position = position_dodge(width = 0.5)) + 
  labs(# title = "",
       x = "Model Type",
       y = "Average Cosine Similarity",
       color = "") +
  theme_minimal() +
  scale_color_manual(values = viridisLite::viridis(2, option = "mako", begin = 0, end = 0.8)) +
  theme(axis.title = element_text(size=rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        legend.text = element_text(size = rel(1.2)),
        # legend.title = element_text(size = rel(1.5)),
        strip.text.x = element_text(size = rel(1.2)),
        legend.position = "bottom") 
```

![](cd_analysis_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff * model_type + 
            numerosity_comparison_type * model_type + 
            patch_size * numerosity_comparison_type  + 
            log_params * numerosity_comparison_type + 
            (1 | image_1) + (1 | image_2) + (1|image_type) +
            (1|model_name),
          control=lmerControl(optimizer="bobyqa"))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## cosine_similarity ~ area_diff * model_type + numerosity_comparison_type *  
##     model_type + patch_size * numerosity_comparison_type + log_params *  
##     numerosity_comparison_type + (1 | image_1) + (1 | image_2) +  
##     (1 | image_type) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## Control: lmerControl(optimizer = "bobyqa")
## 
## REML criterion at convergence: -21943.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.1766 -0.3656  0.0602  0.4837  3.2152 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  image_1    (Intercept) 0.003002 0.05479 
##  image_2    (Intercept) 0.002978 0.05457 
##  model_name (Intercept) 0.001008 0.03175 
##  image_type (Intercept) 0.002041 0.04518 
##  Residual               0.002368 0.04866 
## Number of obs: 8100, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                                                Estimate Std. Error         df
## (Intercept)                                   1.075e+00  2.916e-01  5.151e+00
## area_diff                                    -2.115e-05  3.628e-06  2.999e+03
## model_typeVLM                                -3.427e-03  2.501e-02  5.041e+00
## numerosity_comparison_typesame                7.160e-02  3.180e-02  7.024e+03
## patch_size                                   -1.822e-03  1.690e-03  5.024e+00
## log_params                                   -2.038e-02  3.224e-02  5.031e+00
## area_diff:model_typeVLM                       4.980e-06  4.643e-06  2.642e+03
## model_typeVLM:numerosity_comparison_typesame  6.231e-03  2.811e-03  7.139e+03
## numerosity_comparison_typesame:patch_size     5.462e-04  1.775e-04  6.559e+03
## numerosity_comparison_typesame:log_params    -4.035e-03  3.549e-03  7.020e+03
##                                              t value Pr(>|t|)    
## (Intercept)                                    3.687   0.0135 *  
## area_diff                                     -5.830 6.12e-09 ***
## model_typeVLM                                 -0.137   0.8963    
## numerosity_comparison_typesame                 2.251   0.0244 *  
## patch_size                                    -1.078   0.3299    
## log_params                                    -0.632   0.5549    
## area_diff:model_typeVLM                        1.073   0.2835    
## model_typeVLM:numerosity_comparison_typesame   2.217   0.0267 *  
## numerosity_comparison_typesame:patch_size      3.077   0.0021 ** 
## numerosity_comparison_typesame:log_params     -1.137   0.2556    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##                        (Intr) ar_dff md_VLM nmrs__ ptch_s lg_prm a_:_VL m_VLM:
## area_diff              -0.005                                                 
## modl_typVLM             0.455  0.016                                          
## nmrsty_cmp_            -0.055  0.010 -0.026                                   
## patch_size             -0.574  0.000 -0.154  0.030                            
## log_params             -0.988 -0.001 -0.507  0.055  0.495                     
## ar_dff:_VLM             0.001 -0.555 -0.027  0.007 -0.001  0.001              
## mdl_tVLM:__            -0.025 -0.046 -0.058  0.455  0.008  0.028  0.063       
## nmrsty_cmprsn_typsm:p_  0.031  0.009  0.009 -0.563 -0.053 -0.027 -0.003 -0.159
## nmrsty_cmprsn_typsm:l_  0.054  0.004  0.029 -0.992 -0.026 -0.055 -0.010 -0.502
##                        nmrsty_cmprsn_typsm:p_
## area_diff                                    
## modl_typVLM                                  
## nmrsty_cmp_                                  
## patch_size                                   
## log_params                                   
## ar_dff:_VLM                                  
## mdl_tVLM:__                                  
## nmrsty_cmprsn_typsm:p_                       
## nmrsty_cmprsn_typsm:l_  0.485                
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

### Multiverse analysis

Significant interaction does not always hold up under alternative model specifications, though it is generally trending / positive.


```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ # area_diff * model_type + 
            numerosity_comparison_type * model_type + 
            patch_size * numerosity_comparison_type  + 
            log_params * numerosity_comparison_type +
            (1 | image_1) + (1 | image_2) + # (1|image_type) +
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ numerosity_comparison_type * model_type +  
##     patch_size * numerosity_comparison_type + log_params * numerosity_comparison_type +  
##     (1 | image_1) + (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -21848.1
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.1417 -0.3575  0.0635  0.4815  3.2175 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  image_1    (Intercept) 0.003308 0.05752 
##  image_2    (Intercept) 0.003264 0.05713 
##  model_name (Intercept) 0.001020 0.03193 
##  Residual               0.002367 0.04865 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                                Estimate Std. Error         df
## (Intercept)                                   1.076e+00  2.915e-01  5.027e+00
## numerosity_comparison_typesame                7.565e-02  3.187e-02  7.031e+03
## model_typeVLM                                -2.669e-03  2.514e-02  5.029e+00
## patch_size                                   -1.837e-03  1.700e-03  5.020e+00
## log_params                                   -2.080e-02  3.243e-02  5.027e+00
## numerosity_comparison_typesame:model_typeVLM  5.811e-03  2.813e-03  7.153e+03
## numerosity_comparison_typesame:patch_size     5.504e-04  1.776e-04  6.585e+03
## numerosity_comparison_typesame:log_params    -4.216e-03  3.557e-03  7.025e+03
##                                              t value Pr(>|t|)   
## (Intercept)                                    3.689  0.01402 * 
## numerosity_comparison_typesame                 2.373  0.01765 * 
## model_typeVLM                                 -0.106  0.91958   
## patch_size                                    -1.081  0.32883   
## log_params                                    -0.642  0.54924   
## numerosity_comparison_typesame:model_typeVLM   2.066  0.03888 * 
## numerosity_comparison_typesame:patch_size      3.099  0.00195 **
## numerosity_comparison_typesame:log_params     -1.185  0.23600   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##                        (Intr) nmrs__ md_VLM ptch_s lg_prm n__:_V
## nmrsty_cmp_            -0.055                                   
## modl_typVLM             0.457 -0.026                            
## patch_size             -0.577  0.030 -0.154                     
## log_params             -0.994  0.055 -0.507  0.495              
## nmrs__:_VLM            -0.025  0.455 -0.056  0.008  0.028       
## nmrsty_cmprsn_typsm:p_  0.031 -0.563  0.009 -0.052 -0.027 -0.159
## nmrsty_cmprsn_typsm:l_  0.054 -0.992  0.028 -0.025 -0.055 -0.502
##                        nmrsty_cmprsn_typsm:p_
## nmrsty_cmp_                                  
## modl_typVLM                                  
## patch_size                                   
## log_params                                   
## nmrs__:_VLM                                  
## nmrsty_cmprsn_typsm:p_                       
## nmrsty_cmprsn_typsm:l_  0.485
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff * model_type + 
            numerosity_comparison_type * model_type + 
            # patch_size * numerosity_comparison_type  + 
            log_params * numerosity_comparison_type +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## cosine_similarity ~ area_diff * model_type + numerosity_comparison_type *  
##     model_type + log_params * numerosity_comparison_type + (1 |  
##     image_1) + (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -21820.2
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.1007 -0.3596  0.0635  0.4840  3.2601 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0033258 0.05767 
##  image_2    (Intercept) 0.0032966 0.05742 
##  model_name (Intercept) 0.0009949 0.03154 
##  Residual               0.0023659 0.04864 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                                Estimate Std. Error         df
## (Intercept)                                   8.951e-01  2.351e-01  6.041e+00
## area_diff                                    -5.481e-06  3.485e-06  2.819e+03
## model_typeVLM                                -7.777e-03  2.455e-02  6.050e+00
## numerosity_comparison_typesame                1.309e-01  2.635e-02  7.073e+03
## log_params                                   -3.349e-03  2.783e-02  6.039e+00
## area_diff:model_typeVLM                       5.871e-06  4.811e-06  2.657e+03
## model_typeVLM:numerosity_comparison_typesame  7.439e-03  2.783e-03  7.119e+03
## numerosity_comparison_typesame:log_params    -9.591e-03  3.111e-03  7.010e+03
##                                              t value Pr(>|t|)    
## (Intercept)                                    3.807  0.00878 ** 
## area_diff                                     -1.573  0.11586    
## model_typeVLM                                 -0.317  0.76206    
## numerosity_comparison_typesame                 4.968 6.93e-07 ***
## log_params                                    -0.120  0.90813    
## area_diff:model_typeVLM                        1.220  0.22246    
## model_typeVLM:numerosity_comparison_typesame   2.673  0.00753 ** 
## numerosity_comparison_typesame:log_params     -3.083  0.00206 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff md_VLM nmrs__ lg_prm a_:_VL m_VLM:
## area_diff   -0.007                                          
## modl_typVLM  0.457  0.018                                   
## nmrsty_cmp_ -0.056  0.014 -0.026                            
## log_params  -0.998 -0.001 -0.501  0.056                     
## ar_dff:_VLM  0.000 -0.591 -0.028  0.007  0.001              
## mdl_tVLM:__ -0.025 -0.046 -0.058  0.447  0.028  0.062       
## nmrsty_c_:_  0.056  0.002  0.028 -0.994 -0.056 -0.009 -0.491
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ # area_diff * model_type + 
            numerosity_comparison_type * model_type + 
            # patch_size * numerosity_comparison_type  +
            log_params * numerosity_comparison_type +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ numerosity_comparison_type * model_type +  
##     log_params * numerosity_comparison_type + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -21864
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.1045 -0.3604  0.0638  0.4828  3.2530 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  image_1    (Intercept) 0.003310 0.05753 
##  image_2    (Intercept) 0.003266 0.05715 
##  model_name (Intercept) 0.000993 0.03151 
##  Residual               0.002370 0.04868 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                                Estimate Std. Error         df
## (Intercept)                                   8.929e-01  2.349e-01  6.040e+00
## numerosity_comparison_typesame                1.313e-01  2.636e-02  7.089e+03
## model_typeVLM                                -6.917e-03  2.452e-02  6.041e+00
## log_params                                   -3.379e-03  2.781e-02  6.039e+00
## numerosity_comparison_typesame:model_typeVLM  7.198e-03  2.779e-03  7.155e+03
## numerosity_comparison_typesame:log_params    -9.562e-03  3.113e-03  7.032e+03
##                                              t value Pr(>|t|)    
## (Intercept)                                    3.802  0.00884 ** 
## numerosity_comparison_typesame                 4.981 6.49e-07 ***
## model_typeVLM                                 -0.282  0.78727    
## log_params                                    -0.122  0.90722    
## numerosity_comparison_typesame:model_typeVLM   2.590  0.00961 ** 
## numerosity_comparison_typesame:log_params     -3.072  0.00214 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) nmrs__ md_VLM lg_prm n__:_V
## nmrsty_cmp_ -0.056                            
## modl_typVLM  0.457 -0.026                     
## log_params  -0.998  0.056 -0.502              
## nmrs__:_VLM -0.025  0.448 -0.057  0.028       
## nmrsty_c_:_  0.056 -0.994  0.028 -0.056 -0.492
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff * model_type + 
            numerosity_comparison_type * model_type + 
            patch_size * numerosity_comparison_type  +
            # log_params * numerosity_comparison_type +
            (1 | image_1) + (1 | image_2) + # (1|image_type) +
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: 
## cosine_similarity ~ area_diff * model_type + numerosity_comparison_type *  
##     model_type + patch_size * numerosity_comparison_type + (1 |  
##     image_1) + (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -21816.7
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.1512 -0.3617  0.0638  0.4831  3.2151 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0033244 0.05766 
##  image_2    (Intercept) 0.0032917 0.05737 
##  model_name (Intercept) 0.0009358 0.03059 
##  Residual               0.0023635 0.04862 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                                Estimate Std. Error         df
## (Intercept)                                   8.920e-01  3.174e-02  6.159e+00
## area_diff                                    -5.367e-06  3.483e-06  2.820e+03
## model_typeVLM                                -1.167e-02  2.078e-02  6.056e+00
## numerosity_comparison_typesame                3.760e-02  4.151e-03  7.729e+03
## patch_size                                   -1.296e-03  1.415e-03  6.032e+00
## area_diff:model_typeVLM                       5.776e-06  4.809e-06  2.658e+03
## model_typeVLM:numerosity_comparison_typesame  4.361e-03  2.437e-03  7.144e+03
## numerosity_comparison_typesame:patch_size     6.514e-04  1.552e-04  6.565e+03
##                                              t value Pr(>|t|)    
## (Intercept)                                   28.102 9.75e-08 ***
## area_diff                                     -1.541   0.1235    
## model_typeVLM                                 -0.562   0.5946    
## numerosity_comparison_typesame                 9.059  < 2e-16 ***
## patch_size                                    -0.915   0.3951    
## area_diff:model_typeVLM                        1.201   0.2298    
## model_typeVLM:numerosity_comparison_typesame   1.789   0.0736 .  
## numerosity_comparison_typesame:patch_size      4.196 2.75e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff md_VLM nmrs__ ptch_s a_:_VL m_VLM:
## area_diff   -0.054                                          
## modl_typVLM -0.470  0.021                                   
## nmrsty_cmp_ -0.074  0.102  0.023                            
## patch_size  -0.869  0.001  0.129  0.040                     
## ar_dff:_VLM  0.011 -0.591 -0.032 -0.017 -0.001              
## mdl_tVLM:__  0.026 -0.050 -0.061 -0.378 -0.006  0.066       
## nmrsty_c_:_  0.047  0.006 -0.007 -0.720 -0.055  0.002  0.111
```


## Continuous


```r
df_hf_models %>%
  filter(layer == max_layer) %>%
  ggplot(aes(x = numerosity_diff,
             y = cosine_similarity,
             color = model_type)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Numerosity Difference",
       y = "Cosine Similarity",
       color = "") +
    scale_color_manual(values = viridisLite::viridis(2, option = "mako", begin = 0, end = 0.8)) +
  theme(text = element_text(size = 15),
        legend.position = "bottom") +
  facet_wrap(~model_type)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](cd_analysis_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
df_hf_models_last = df_hf_models %>%
  filter(layer == max_layer) 

m1 = lmer(data = df_hf_models_last,
          cosine_similarity ~ area_diff * model_type + numerosity_diff * model_type + 
            patch_size * numerosity_diff + log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + (1|image_type) + 
            (1|model_name))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff * model_type + numerosity_diff *  
##     model_type + patch_size * numerosity_diff + log_params *  
##     numerosity_diff + (1 | image_1) + (1 | image_2) + (1 | image_type) +  
##     (1 | model_name)
##    Data: df_hf_models_last
## 
## REML criterion at convergence: -22402.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.0044 -0.3755  0.0581  0.4949  3.1493 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0022289 0.04721 
##  image_2    (Intercept) 0.0023297 0.04827 
##  model_name (Intercept) 0.0009963 0.03156 
##  image_type (Intercept) 0.0012337 0.03512 
##  Residual               0.0023438 0.04841 
## Number of obs: 8100, groups:  
## image_1, 1412; image_2, 1318; model_name, 9; image_type, 2
## 
## Fixed effects:
##                                 Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                    1.138e+00  2.890e-01  5.090e+00   3.937 0.010614
## area_diff                     -1.119e-05  3.342e-06  3.180e+03  -3.349 0.000821
## model_typeVLM                  4.039e-03  2.484e-02  5.023e+00   0.163 0.877154
## numerosity_diff               -6.662e-03  2.105e-03  6.830e+03  -3.165 0.001558
## patch_size                    -1.304e-03  1.679e-03  5.011e+00  -0.777 0.472269
## log_params                    -2.352e-02  3.203e-02  5.017e+00  -0.734 0.495577
## area_diff:model_typeVLM        2.051e-06  4.219e-06  2.762e+03   0.486 0.626890
## model_typeVLM:numerosity_diff -7.682e-04  1.832e-04  6.880e+03  -4.193 2.79e-05
## numerosity_diff:patch_size    -4.463e-05  1.194e-05  6.527e+03  -3.737 0.000188
## numerosity_diff:log_params     2.310e-04  2.342e-04  6.792e+03   0.986 0.324042
##                                  
## (Intercept)                   *  
## area_diff                     ***
## model_typeVLM                    
## numerosity_diff               ** 
## patch_size                       
## log_params                       
## area_diff:model_typeVLM          
## model_typeVLM:numerosity_diff ***
## numerosity_diff:patch_size    ***
## numerosity_diff:log_params       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##               (Intr) ar_dff md_VLM nmrst_ ptch_s lg_prm a_:_VL m_VLM:
## area_diff     -0.004                                                 
## modl_typVLM    0.456  0.012                                          
## numrsty_dff   -0.040 -0.011 -0.018                                   
## patch_size    -0.575  0.001 -0.154  0.022                            
## log_params    -0.990  0.000 -0.507  0.040  0.495                     
## ar_dff:_VLM    0.001 -0.555 -0.021 -0.009 -0.001  0.000              
## mdl_tyVLM:_   -0.019  0.032 -0.041  0.454  0.006  0.020 -0.039       
## nmrsty_dff:p_  0.023 -0.007  0.006 -0.569 -0.039 -0.020  0.005 -0.158
## nmrsty_dff:l_  0.040 -0.008  0.020 -0.991 -0.019 -0.041  0.010 -0.502
##               nmrsty_dff:p_
## area_diff                  
## modl_typVLM                
## numrsty_dff                
## patch_size                 
## log_params                 
## ar_dff:_VLM                
## mdl_tyVLM:_                
## nmrsty_dff:p_              
## nmrsty_dff:l_  0.490       
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
### For visualizing, remove key factors from model 
m_reduced = lmer(data = df_hf_models_last,
          cosine_similarity ~ area_diff + # numerosity_diff * model_type + 
            patch_size + # log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + (1|image_type) + 
            (1|model_name))

df_hf_models_last$resid = residuals(m_reduced)

# Get the slopes for the line describing residuals v. numerosity difference


slopes <- df_hf_models_last %>%
  group_by(model_type) %>%
  nest() %>%
  mutate(
    model = map(data, ~ lm(resid ~ numerosity_diff, data = .x)),
    tidied = map(model, tidy)
  ) %>%
  unnest(tidied) %>%
  filter(term == "numerosity_diff") %>%
  select(model_type, estimate) %>%
  mutate(label = paste0("B = ", round(estimate, 5)))

slopes <- slopes %>%
  mutate(x = 0,    # Adjust as needed
         y = max(df_hf_models_last$resid, na.rm = TRUE))  # Top of panel


df_hf_models_last %>%
  ggplot(aes(x = numerosity_diff,
             y = resid,
             color = model_type)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "Numerosity Difference",
       y = "Residuals",
       color = "") +
  scale_color_manual(values = viridisLite::viridis(2, option = "mako", begin = 0, end = 0.8)) +
  theme(text = element_text(size = 15),
        legend.position = "bottom") +
  facet_wrap(~model_type) + 
  geom_text(data = slopes,
            aes(x = x, y = y, label = label),
            hjust = -1,
            inherit.aes = FALSE,
            size = 5)
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](cd_analysis_files/figure-html/unnamed-chunk-8-2.png)<!-- -->


### Multiverse analysis

Significant interaction is preserved under alternative specifications.


```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ # area_diff * model_type + 
            numerosity_diff * model_type + 
            patch_size * numerosity_diff + 
            log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ numerosity_diff * model_type + patch_size *  
##     numerosity_diff + log_params * numerosity_diff + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -22342.4
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.9782 -0.3711  0.0698  0.4923  3.1558 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  image_1    (Intercept) 0.002404 0.04903 
##  image_2    (Intercept) 0.002524 0.05024 
##  model_name (Intercept) 0.001008 0.03175 
##  Residual               0.002342 0.04839 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                 Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                    1.145e+00  2.896e-01  5.017e+00   3.952 0.010758
## numerosity_diff               -7.033e-03  2.106e-03  6.840e+03  -3.339 0.000844
## model_typeVLM                  4.123e-03  2.498e-02  5.019e+00   0.165 0.875330
## patch_size                    -1.319e-03  1.689e-03  5.011e+00  -0.781 0.469870
## log_params                    -2.410e-02  3.222e-02  5.017e+00  -0.748 0.488036
## numerosity_diff:model_typeVLM -7.543e-04  1.832e-04  6.891e+03  -4.117 3.89e-05
## numerosity_diff:patch_size    -4.440e-05  1.194e-05  6.548e+03  -3.717 0.000203
## numerosity_diff:log_params     2.454e-04  2.344e-04  6.803e+03   1.047 0.295081
##                                  
## (Intercept)                   *  
## numerosity_diff               ***
## model_typeVLM                    
## patch_size                       
## log_params                       
## numerosity_diff:model_typeVLM ***
## numerosity_diff:patch_size    ***
## numerosity_diff:log_params       
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##               (Intr) nmrst_ md_VLM ptch_s lg_prm n_:_VL nmrsty_dff:p_
## numrsty_dff   -0.040                                                 
## modl_typVLM    0.457 -0.019                                          
## patch_size    -0.577  0.022 -0.154                                   
## log_params    -0.994  0.040 -0.507  0.495                            
## nmrst_:_VLM   -0.018  0.454 -0.041  0.006  0.020                     
## nmrsty_dff:p_  0.023 -0.569  0.006 -0.038 -0.019 -0.158              
## nmrsty_dff:l_  0.040 -0.991  0.021 -0.019 -0.041 -0.502  0.490
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff * model_type + 
            numerosity_diff * model_type + 
            # patch_size * numerosity_diff + 
            log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))
```

```
## Warning: Some predictor variables are on very different scales: consider
## rescaling

## Warning: Some predictor variables are on very different scales: consider
## rescaling
```

```r
summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff * model_type + numerosity_diff *  
##     model_type + log_params * numerosity_diff + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -22314.8
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.8539 -0.3745  0.0684  0.4907  3.2035 
## 
## Random effects:
##  Groups     Name        Variance Std.Dev.
##  image_1    (Intercept) 0.002408 0.04908 
##  image_2    (Intercept) 0.002528 0.05028 
##  model_name (Intercept) 0.000984 0.03137 
##  Residual               0.002345 0.04843 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                 Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                    1.013e+00  2.336e-01  6.023e+00   4.336  0.00486
## area_diff                      2.127e-06  3.156e-06  2.963e+03   0.674  0.50036
## model_typeVLM                  8.856e-04  2.439e-02  6.029e+00   0.036  0.97221
## numerosity_diff               -1.154e-02  1.734e-03  6.880e+03  -6.655 3.05e-11
## log_params                    -1.171e-02  2.766e-02  6.022e+00  -0.423  0.68683
## area_diff:model_typeVLM        2.592e-06  4.333e-06  2.764e+03   0.598  0.54970
## model_typeVLM:numerosity_diff -8.637e-04  1.812e-04  6.871e+03  -4.766 1.92e-06
## numerosity_diff:log_params     6.727e-04  2.045e-04  6.788e+03   3.290  0.00101
##                                  
## (Intercept)                   ** 
## area_diff                        
## model_typeVLM                    
## numerosity_diff               ***
## log_params                       
## area_diff:model_typeVLM          
## model_typeVLM:numerosity_diff ***
## numerosity_diff:log_params    ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff md_VLM nmrst_ lg_prm a_:_VL m_VLM:
## area_diff   -0.005                                          
## modl_typVLM  0.457  0.014                                   
## numrsty_dff -0.041 -0.011 -0.018                            
## log_params  -0.998  0.000 -0.501  0.041                     
## ar_dff:_VLM  0.001 -0.597 -0.022 -0.008  0.000              
## mdl_tyVLM:_ -0.019  0.030 -0.041  0.449  0.020 -0.039       
## nmrsty_df:_  0.041 -0.007  0.020 -0.993 -0.041  0.009 -0.493
## fit warnings:
## Some predictor variables are on very different scales: consider rescaling
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ # area_diff * model_type + 
            numerosity_diff * model_type + 
            # patch_size * numerosity_diff + 
            log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ numerosity_diff * model_type + log_params *  
##     numerosity_diff + (1 | image_1) + (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -22359.5
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -5.8604 -0.3717  0.0688  0.4922  3.2012 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0024038 0.04903 
##  image_2    (Intercept) 0.0025260 0.05026 
##  model_name (Intercept) 0.0009841 0.03137 
##  Residual               0.0023459 0.04843 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                 Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                    1.014e+00  2.336e-01  6.022e+00   4.341  0.00483
## numerosity_diff               -1.149e-02  1.734e-03  6.895e+03  -6.625 3.73e-11
## model_typeVLM                  1.172e-03  2.439e-02  6.023e+00   0.048  0.96324
## log_params                    -1.170e-02  2.766e-02  6.022e+00  -0.423  0.68696
## numerosity_diff:model_typeVLM -8.616e-04  1.811e-04  6.894e+03  -4.757 2.00e-06
## numerosity_diff:log_params     6.719e-04  2.045e-04  6.809e+03   3.285  0.00102
##                                  
## (Intercept)                   ** 
## numerosity_diff               ***
## model_typeVLM                    
## log_params                       
## numerosity_diff:model_typeVLM ***
## numerosity_diff:log_params    ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) nmrst_ md_VLM lg_prm n_:_VL
## numrsty_dff -0.041                            
## modl_typVLM  0.457 -0.019                     
## log_params  -0.998  0.041 -0.502              
## nmrst_:_VLM -0.019  0.449 -0.042  0.020       
## nmrsty_df:_  0.041 -0.993  0.020 -0.041 -0.493
```

```r
m1 = lmer(data = filter(df_hf_models, layer == max_layer),
          cosine_similarity ~ area_diff * model_type + 
            numerosity_diff * model_type + 
             patch_size * numerosity_diff + 
            # log_params * numerosity_diff +
            (1 | image_1) + (1 | image_2) + # (1|image_type) + 
            (1|model_name))

summary(m1)
```

```
## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
## lmerModLmerTest]
## Formula: cosine_similarity ~ area_diff * model_type + numerosity_diff *  
##     model_type + patch_size * numerosity_diff + (1 | image_1) +  
##     (1 | image_2) + (1 | model_name)
##    Data: filter(df_hf_models, layer == max_layer)
## 
## REML criterion at convergence: -22316
## 
## Scaled residuals: 
##     Min      1Q  Median      3Q     Max 
## -6.0046 -0.3743  0.0683  0.4907  3.1546 
## 
## Random effects:
##  Groups     Name        Variance  Std.Dev.
##  image_1    (Intercept) 0.0024078 0.04907 
##  image_2    (Intercept) 0.0025246 0.05025 
##  model_name (Intercept) 0.0009234 0.03039 
##  Residual               0.0023411 0.04838 
## Number of obs: 8100, groups:  image_1, 1412; image_2, 1318; model_name, 9
## 
## Fixed effects:
##                                 Estimate Std. Error         df t value Pr(>|t|)
## (Intercept)                    9.279e-01  3.144e-02  6.091e+00  29.515 8.31e-08
## area_diff                      2.245e-06  3.154e-06  2.964e+03   0.712    0.477
## model_typeVLM                 -5.619e-03  2.062e-02  6.034e+00  -0.272    0.794
## numerosity_diff               -4.895e-03  2.888e-04  7.203e+03 -16.946  < 2e-16
## patch_size                    -6.959e-04  1.405e-03  6.016e+00  -0.495    0.638
## area_diff:model_typeVLM        2.487e-06  4.330e-06  2.765e+03   0.574    0.566
## model_typeVLM:numerosity_diff -6.597e-04  1.586e-04  6.872e+03  -4.160 3.22e-05
## numerosity_diff:patch_size    -5.059e-05  1.041e-05  6.531e+03  -4.859 1.21e-06
##                                  
## (Intercept)                   ***
## area_diff                        
## model_typeVLM                    
## numerosity_diff               ***
## patch_size                       
## area_diff:model_typeVLM          
## model_typeVLM:numerosity_diff ***
## numerosity_diff:patch_size    ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Correlation of Fixed Effects:
##             (Intr) ar_dff md_VLM nmrst_ ptch_s a_:_VL m_VLM:
## area_diff   -0.038                                          
## modl_typVLM -0.471  0.016                                   
## numrsty_dff -0.039 -0.106  0.015                            
## patch_size  -0.871  0.001  0.129  0.028                     
## ar_dff:_VLM  0.009 -0.597 -0.026  0.008 -0.001              
## mdl_tyVLM:_  0.018  0.030 -0.042 -0.364 -0.005 -0.039       
## nmrsty_df:_  0.035 -0.003 -0.005 -0.699 -0.040 -0.001  0.117
```


