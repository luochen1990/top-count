Top Count
=========

编译:

```
stack build
```

安装可执行文件:

```
stack install
```

可执行文件:

- gen-test-file: 生成测试文件, 如 `u100` 表示生成100个URL, 文件名形如 `u100.in`
- ext-sort: 外部排序, 输入文件名, 输出结果会写到相应文件, 文件名形如 `u100.in.sort.out`
- top-count: 找出现次数最多的100项, 输入文件名, 输出结果写到文件, 文件名形如 `u100.in.top-count.out`

Tips:

- 命令行参数后面加 `+RTS -s` 可以查看用时及性能分析数据, 如 `top-count u100.in +RTS -s`
- 命令行参数后面加 `+RTS -Nx` 可以指定CPU核心数, 如 `top-count u100.in +RTS -s -N4`

运行单元测试:

```
stack test
```

复杂度分析
----------

设文件共有 `n` 行, 每行平均大小为 `l`, 需要取出现次数最多的 Top `m` 项.
记 `k` 为所有行的去重计数, 即不同行的总数量 (该值可能无法预先知道).

另有以下记号:

- `Time` 表示CPU时间代价
- `Mem` 表示内存峰值
- `Read` 表示连续读盘代价
- `Read?` 表示不连续读盘代价, 通过适当的缓存策略可优化为连续读盘代价
- `Write` 表示写盘代价
- `X + Y` 先后发生的资源占用
- `X | Y` 同时发生的对不同设备时间的占用, 在有适当缓存策略的情况下可以重叠, 此时两者取最大值.

### 阶段1

- 分块读取所有数据到内存 `(n * l) Read` (其中 `n` 为总行数, `l` 为每行的平均大小)
- 对每块进行内存中排序 `(b * log b * l * a) Time; (b * l) Mem` (其中 `b` 为分块大小, `a` 为块数量, 可知 `n = a * b`)
- 将每块的排序结果落盘 `(n * l) Write`

小计: `( (n * l) * (Read + Write) | (b * log b * l * a) Time ) ; (b * l) Mem`

### 阶段2

- 对已排序块执行合并有序流 (会有一次读盘) 并进行连续项计数 `(n * l) Read? | (n * log a * l) Time`
- 对计数结果 (设记录数为 `k`, 按 `kb` 分块, 一共 `ka` 块) 进行分块并在内存中排序 `(kb * log kb * l * ka) Time ; (kb * l) Mem`
- 将每块的排序结果落盘 `(k * l) Write`

小计: `((n * l) Read? + (k * l) Write) | (n * log a * l + kb * log kb * l * ka) Time ; (kb * l) Mem`

### 阶段3

- 对已排序块执行合并有序流 (会有一次读盘) 并取 Top m 项 `(m * l) Read? + (m * log ka * l) Time`
- 将最后结果落盘 `(m * l) Write`

小计: `(m * l) * (Read? + Write) | (m * log ka * l) Time`

### 总计

#### 内存

内存占用主要取决于两次外排序的分块大小, 忽略常数为 `b * l`

#### 时间

```
( (n * l) * (Read + Write) | (b * log b * l * a) Time )
+
( ( (n * l) Read? + (k * l) Write ) | (n * log a * l + kb * log kb * l * ka) Time )
+
( (m * l) * (Read? + Write) | (m * log ka * l) Time )
```

若假设磁盘时间远大于CPU时间, 则以上结果可简化为

```
(n * l) * (Read + Read? + Write) + (k * l) Write + (m * l) * (Read? + Write)
```

其中 `n >= k >= m`

极端情况下有 `n = k = m`, 则以上可简化为

```
(n * l) * (Read + Read? * 2 + Write * 3)
```

较好情况下, 若数据中存在大量重复项, 即有 `n >> k` (n 远大于 k), 则以上可简化为

```
(n * l) * (Read + Read? + Write)
```

一般情况下有 `n ~= k >> m` (k 与 n 接近, 并远大于 m), 则以上可简化为

```
(n * l) * (Read + Read? + Write * 2)
```

该算法实现及优化的关键是:

- 使用流式操作, 确保数据不会堆积在内存中导致OOM
- 尽量对多个阶段进行融合, 减少磁盘读写次数
- 确保用适当的缓存策略将磁盘随机读 (Read?) 优化到磁盘连续读 (Read)

