# 📊 CancerRCDShiny

<p align="center">
  <img src="https://github.com/CancerRCD/CancerRCDShiny/blob/bbb0be5495097c0bedfc711565c8279061b50306/www/Figure%203_HRF.png" width="1000">
</p>

**CancerRCDShiny** is an R Shiny application designed for researchers and clinicians to explore the molecular mechanisms of cancer through the lens of regulated cell death (RCD).  

The platform integrates a comprehensive database covering **25 distinct forms of RCD** and **32 cancer types**, enabling users to investigate the complex relationships between molecular signatures and cancer phenotypes. By applying stringent genome-wide significance filters, the app ensures access to high-confidence signatures and highlights those with the strongest correlation scores, helping users focus on the most biologically relevant features.  

With an intuitive interface and interactive visualization tools, **CancerRCDShiny** allows users to:  
- Explore gene-level and phenotypic attributes in detail.  
- Customize plots and generate detailed reports.  
- Perform targeted queries based on specific RCD types, cancer types, or molecular features.  

This resource is designed to support precision oncology by uncovering novel insights into cancer-associated cell death processes and advancing translational cancer research.  

---

## 🔗 Useful Links
- 🔥 [Online App](https://cancerrcdshiny.shinyapps.io/cancerrcdshiny/)  
- 🧪 [Published Paper](https://doi.org/10.3389/fbinf.2025.1630518)  

---

### ▶️ Run Locally

```r
library(shiny)
setwd("/path/to/parent/dir/of/source/")
runApp("CancerRCDShiny")
```

---

## 🐞 Bug Reports

Please open an **issue** on GitHub or contact:  
📧 **[Enrique Medina-Acosta](mailto:quique@uenf.br)**  

---

## ⚙️ Tested Environment

```
R version 4.3.1 (2023-06-16)
Platform: x86_64-w64-mingw32 (64-bit)
```
