# Робота 3 — Кореляційний аналіз

Нижче наведено основні графіки, які створює скрипт `3_звіт.R` (збережені у `plots/report3/`).

**Графіки:**

![Corrplot (mixed)](../plots/report3/corrplot_pearson_mixed.png)

![Corrplot (hclust)](../plots/report3/corrplot_pearson_hclust.png)

![qgraph network](../plots/report3/qgraph_pearson.png)

![Pairs plot (group 1)](../plots/report3/pairs_group_01.png)

![Scatter for top pair](../plots/report3/scatter_TOP1_TOP2.png)

> Примітка: назви останньої картинки (`scatter_TOP1_TOP2.png`) — це заповнювач; при запуску `3_звіт.R` файл для вибраної пари матиме назву `scatter_<ticker1>_<ticker2>.png`.

Результати (матриці кореляцій, список топ-пар та груп) збережено у `outputs/` (`corr_pearson.csv`, `top_correlated_partners.csv`, `ticker_groups_hclust.csv`, тощо).