# Quick Start - R Language Studio in VS Code Server

## 🚀 First Time Setup Complete!

Your R environment is **fully configured** with all features available in VS Code Server.

## ✨ What You Now Have

### 1. **Full R Language Support**
- IntelliSense and autocomplete
- Real-time error checking
- Function signatures on hover
- Parameter hints

### 2. **Debugging Tools**
- Set breakpoints by clicking line numbers
- Step through code with F10/F11
- Inspect variables in debug console
- Watch expressions

### 3. **Code Quality Tools**
- Automatic formatting on save
- Linting with real-time diagnostics
- Code style suggestions
- Refactoring options

### 4. **Productivity Features**
- Go to definition (F12)
- Find references (Shift+Alt+F12)
- Rename symbols (F2)
- Search workspace (Ctrl+Shift+F)

### 5. **Visualization**
- Interactive plots with httpgd
- Plot history
- Integrated graphics viewer

---

## 🎯 Quick Commands to Try

### Open a Terminal
```
Ctrl+` (backtick)
```

### Run R Script
```
1. Open any .R file
2. Press Ctrl+Shift+Enter on a line
3. Results show in R Terminal
```

### Access Help
```
1. Click any function name
2. Press F1 or Ctrl+K Ctrl+I
3. Documentation appears instantly
```

### Format Code
```
Ctrl+Shift+P → "Format Document"
Or auto-formats on save
```

---

## 📋 Most Used Keyboard Shortcuts

| Shortcut | What It Does |
|----------|-------------|
| `Ctrl+Shift+Enter` | Run current line |
| `Ctrl+Shift+S` | Run selected code |
| `Alt+-` | Type: `<-` |
| `Ctrl+Shift+M` | Type: `\|>` (pipe) |
| `F12` | Go to definition |
| `Ctrl+/` | Toggle comment |
| `F5` | Start debugging |
| `Ctrl+F` | Find text |
| `Ctrl+H` | Find & Replace |

---

## 🧪 Test It Out

### 1. Create a Test File
```bash
# In VS Code terminal
cd Workshops/"Week 1"
cat > test.R << 'EOF'
# Test R functionality
x <- c(1, 2, 3, 4, 5)
mean(x)

# Try typing library( and see autocomplete
# Try hovering over mean
# Try Ctrl+Shift+Enter on each line
EOF
```

### 2. Run It
- Open `test.R`
- Click on any line
- Press `Ctrl+Shift+Enter`
- See output in terminal

### 3. Debug It
- Set breakpoint on a line (click line number)
- Press `F5` to start debugging
- Step through code

---

## 📚 Available Packages

You have installed:
- **Data Science**: dplyr, tidyr, ggplot2, data.table, plotly
- **Development**: devtools, roxygen2, testthat
- **Documents**: rmarkdown, knitr
- **Quality**: styler, lintr
- **Language Server**: languageserver, jsonlite

### Example Usage
```r
# Load packages
library(dplyr)
library(ggplot2)

# Your code here
data <- tibble(x = 1:10, y = x^2)
ggplot(data, aes(x, y)) + geom_point()
```

---

## 🔄 Workflow Tips

### For Data Analysis
```r
# 1. Load data
df <- read.csv("data.csv")

# 2. Type df$ and get autocomplete
# 3. Explore with View, head, etc
# 4. Create plots and see in viewer
# 5. Save results
```

### For Development
```r
# 1. Create functions in .R files
# 2. Use Ctrl+Shift+Enter to test
# 3. Set breakpoints to debug
# 4. View help with F1
# 5. Document with roxygen2
```

### For Reports
```r
# 1. Create .Rmd file
# 2. Mix R code and markdown
# 3. Press Ctrl+Shift+K to preview
# 4. Export to HTML/PDF/Word
```

---

## 🆘 Troubleshooting

**Nothing happens when I run code?**
- Make sure you have an R terminal open
- If not: Ctrl+Shift+P → "R: Create R Terminal"

**Autocomplete not working?**
- Wait a moment for language server to start
- Check console for errors
- Try: Ctrl+Shift+P → "R: Restart R Session"

**Can't find a function?**
- Make sure package is loaded with `library()`
- Try: `?function_name` in console

**Graphics not showing?**
- Try: `library(httpgd)` in console
- Or disable httpgd in settings if issues persist

---

## 📖 Learn More

- **Data wrangling**: https://dplyr.tidyverse.org
- **Visualization**: https://ggplot2.tidyverse.org
- **Reports**: https://rmarkdown.rstudio.com
- **Testing**: https://testthat.r-lib.org

---

## ✅ You're Ready!

1. ✓ R Language Server installed
2. ✓ VS Code extensions configured
3. ✓ Debugging enabled
4. ✓ Keyboard shortcuts set up
5. ✓ Packages installed
6. ✓ Custom R profile activated

**Start coding! Open any `.R` file and press `Ctrl+Shift+Enter` to run.**

---

*Last configured: December 4, 2025*
*R Version: 4.5.1*
*VS Code Server: Ready*
