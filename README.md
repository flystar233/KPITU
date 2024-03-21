# KPITU
This is the KPITU (Knee Point Identification Based on Trade-Off Utility) algorithm writing by R
这个R脚本主要用于计算数据的最佳拐点。
代码参考于：https://github.com/COLA-Laboratory/kpi, 90% 的代码由AI生成,并进行人工检查，原代码功能已复刻且通过测试。
## AI 代码解释
1. 定义 solution 和 reference_point 类：这两个类被用来表示解和参考点。每个解包含一个目标值，邻居列表，贡献度，关联的参考点等信息。参考点包含一个方向，邻居列表和关联的解。
2. 定义 transfer 和 select 函数：transfer 函数用于判断两个解之间是否存在转移，select 函数用于计算两个解的目标值之差。
3. 定义 Associate 函数：用于关联解和参考点，即确定每个解最接近的参考点。
4. main_function 函数：这是主函数，执行以下操作：

   4.1 数据归一化：将数据的每一列（即每一个目标）缩放到 [0, 1] 区间。
   
   4.2 计算参考点：根据给定的维度和数据点数量计算参考点。

   4.3 关联解和参考点：使用 Associate 函数关联解和参考点。
   
   4.4 创建邻居列表：对每个解，找出与其关联的其他解和参考点的邻居。
   
   4.5 划分解：将解分为内部解（不需要移动的解），外围解（可能需要添加到内部解的解）和预留（需要移动的解）。
   
   4.6 调整内部解：根据需要，从外围解中添加解或从内部解中移除解，使内部解的数量达到预设值K。
   
   4.7返回内部解：返回内部解对应的原始数据点。
   
这个R脚本的主要应用场景是在多目标优化问题中找到最佳的拐点，也就是那些在目标之间达到最佳平衡的解。

## 示例
```r
library(ggplot2)
data1<- read.table('PMOP1_M2_A4.out',header=F)
knee_point1<- main_function(as.matrix(data1),1)

ggplot() +
geom_point(data = data1, aes(x = V1, y = V2), color = "black") +
geom_point(data = knee_point1, aes(x = V1, y = V2,color = "red"), size = 4) +
labs(colour = "Knee point") +
guides(size = FALSE)


data2<- read.table('PMOP1_M2_A2.out',header=F)
knee_point2<- main_function(as.matrix(data2),1)

ggplot() +
geom_point(data = data2, aes(x = V1, y = V2), color = "black") +
geom_point(aes(x = knee_point2[1], y = knee_point2[2],color = "red"), size = 4) +
labs(colour = "Knee point") +
guides(size = FALSE)
```
![PMOP1_M2_A4](https://github.com/flystar233/KPITU/assets/20431495/5022e926-2dbe-4dda-84b9-2d3d900bfc24)
![PMOP1_M2_A2](https://github.com/flystar233/KPITU/assets/20431495/59110a51-ecb4-42db-8bd5-900f202c855c)
