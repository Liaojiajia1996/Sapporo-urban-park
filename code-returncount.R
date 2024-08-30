install.packages("writexl")
install.packages("ggplot2")
install.packages("readr")
install.packages("sf")
install.packages("readr")
install.packages("dplyr")


library(sf)
library(dplyr)
library(ggplot2)
library(writexl)
library(tidyr)
library(readr)

# 读取和处理公园边界数据
park_boundary <- st_read("/Users/liaojiajia/博士研究/paper/滞留时间重新计算/geourbanpark.geojson")
utm_crs <- 32654
park_boundary <- st_transform(park_boundary, crs = utm_crs)

# 读取访客数据并转换为空间对象
visitor_data <- read_csv("/Users/liaojiajia/博士研究/paper/滞留时间重新计算/提取原数据ID/ID2020070600.csv")
visitor_data_sf <- st_as_sf(visitor_data, coords = c("longitude", "latitude"), crs = 4326)
visitor_data_sf <- st_transform(visitor_data_sf, crs = st_crs(park_boundary))


# 判断访客是否在公园边界内
visitor_data_sf <- visitor_data_sf %>%
  mutate(in_park = lengths(st_within(geometry, park_boundary)) > 0)

# 按时间排序并标记访客的公园内外时间点
visitor_data_sf <- visitor_data_sf %>%
  arrange(dailyid, timestamp) %>%
  group_by(dailyid) %>%
  mutate(
    prev_in_park = lag(in_park, default = first(in_park)),
    left_park = prev_in_park & !in_park,
    returned_park = !in_park & lead(in_park, default = last(in_park))
  )

# 标记离开公园的时间和重新进入公园前的最后一个时间点
visitor_data_sf <- visitor_data_sf %>%
  mutate(
    exit_time = if_else(left_park, lag(timestamp), as.POSIXct(NA)),
    reentry_time = if_else(returned_park, timestamp, as.POSIXct(NA)),
    time_outside = difftime(reentry_time, exit_time, units = "mins")
  ) %>%
  fill(exit_time, .direction = "down") %>%
  ungroup()

# 筛选每次在公园外部滞留超过30分钟后再返回的访客
valid_visits <- visitor_data_sf %>%
  filter(!is.na(time_outside) & time_outside >= 30 & returned_park) %>%
  group_by(dailyid) %>%
  mutate(return_visit_count = n()) %>%
  ungroup()

# 统计符合条件的访客数量
unique_visitors_count <- valid_visits %>%
  distinct(dailyid) %>%
  nrow()

print(paste("Number of unique visitors who left the park for more than 30 minutes and returned:", unique_visitors_count))



# 统计每个访客的进出次数
visit_counts <- valid_visits %>%
  group_by(dailyid) %>%
  summarize(visit_count = n(),
            avg_stay_duration = mean(time_outside, na.rm = TRUE)) %>%
  ungroup()

# 输出每个访客的进出次数
print("Visitors' park re-entry counts and their average stay duration:")
print(visit_counts)


# 统计在公园中滞留过的访客数量
visitors_in_park <- visitor_data_sf %>%
  filter(in_park) %>%
  distinct(dailyid) %>%
  nrow()

print(paste("Number of unique visitors who stayed in the park:", visitors_in_park))


# 计算多次往返公园的访客的平均滞留时间
average_stay_duration <- valid_visits %>%
  group_by(dailyid) %>%
  summarize(avg_stay_duration = mean(time_outside, na.rm = TRUE)) %>%
  summarize(overall_avg_stay_duration = mean(avg_stay_duration, na.rm = TRUE))

print(paste("Average stay duration for visitors with multiple visits:", 
            round(average_stay_duration$overall_avg_stay_duration, 2), "minutes"))

#######以上，得出特殊游客的进出数据


###### 结果！！！
# 计算每个访客点是否在某个公园内，并获得公园的名称
visitor_data_sf <- visitor_data_sf %>%
  mutate(park_name = apply(st_within(geometry, park_boundary, sparse = FALSE), 1, 
                           function(x) ifelse(any(x), park_boundary$parkname[which(x)], NA)))

# 检查前几行数据，确认 park_name 列是否存在
head(visitor_data_sf$park_name)
print(head(visitor_data_sf))
print(head(park_boundary))
print(st_crs(visitor_data_sf))
print(st_crs(park_boundary))


# 为了生成更详细的进入和离开时间信息，我们展开 visit_times 和 exit_times 列
visitor_details <- visitor_details %>%
  mutate(visit_times = sapply(visit_times, paste, collapse = ", "),
         exit_times = sapply(exit_times, paste, collapse = ", "))

# 打印结果以确认
print(visitor_details)




