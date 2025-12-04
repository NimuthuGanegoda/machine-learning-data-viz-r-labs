# R Language Studio - Complete Setup Index

## 🎯 For First-Time Users

Start here:
1. **[QUICKSTART.md](QUICKSTART.md)** - 5-minute setup guide
2. **[FEATURES_AVAILABLE.txt](FEATURES_AVAILABLE.txt)** - Complete feature list

## 📖 Detailed Documentation

- **[R_STUDIO_SETUP.md](R_STUDIO_SETUP.md)** - Comprehensive setup guide
- **[CONFIG_CHECKLIST.md](CONFIG_CHECKLIST.md)** - Verification checklist

## 🚀 Essential Keyboard Shortcuts

| Action | Shortcut |
|--------|----------|
| Run current line | `Ctrl+Shift+Enter` |
| Run selection | `Ctrl+Shift+S` |
| Insert `<-` | `Alt+-` |
| Insert `\|>` (pipe) | `Ctrl+Shift+M` |
| Go to definition | `F12` |
| Find references | `Shift+Alt+F12` |
| Rename symbol | `F2` |
| Debug (F5) | Set breakpoints first |
| Format document | `Ctrl+Shift+P` → Format |

## 📁 Configuration Files

### VS Code Settings
- `.vscode/settings.json` - Main configuration (LSP, debugging, formatting)
- `.vscode/launch.json` - Debugging configurations
- `.vscode/keybindings.json` - Custom keyboard shortcuts
- `.vscode/extensions.json` - Recommended extensions

### R Configuration
- `.Rprofile` - R startup configuration with utilities and welcome message
- `renv/` - Project-specific R package library

## ✅ What's Installed

### VS Code Extensions (3)
- REditorSupport.r - R language support
- RDebugger.r-debugger - Debugging tools
- REditorSupport.r-syntax - Syntax highlighting

### R Language Server
- languageserver - LSP protocol
- jsonlite - JSON parsing

### Key R Packages
- **Data Science**: dplyr, tidyr, ggplot2, plotly, data.table
- **Development**: devtools, roxygen2, testthat
- **Documents**: rmarkdown, knitr
- **Quality**: lintr, styler

## 🎯 Quick Commands

### Start R Terminal
```
Ctrl+` or Ctrl+Shift+P → "R: Create R Terminal"
```

### Run Code Interactively
```r
# Place cursor on line and press Ctrl+Shift+Enter
x <- c(1, 2, 3, 4, 5)
mean(x)  # ← Run this
```

### Use Autocomplete
```r
# Type and press Ctrl+Space
library(  # ← Press Ctrl+Space to see packages
dp       # ← Press Ctrl+Space to see dplyr functions
```

### Debug Code
```
1. Click line number to set breakpoint
2. Press F5 to start debugging
3. Use F10 to step over, F11 to step into
4. Hover over variables to inspect
```

## 🔧 Custom R Functions

Available in R console:

```r
# Load packages silently
lib(dplyr, ggplot2, ggplot2)

# Get package info
pkginfo("plot")  # Find plotting packages

# Check installation
is_installed("tidyverse")

# Install if missing
install_if_missing(shiny, plotly)
```

## 🆘 Troubleshooting

### Code won't run
- Open R Terminal: `Ctrl+Shift+P` → "R: Create R Terminal"

### Autocomplete not working
- Language server starting up (wait a moment)
- Or restart: `Ctrl+Shift+P` → "R: Restart R Session"

### Debugging won't start
- Set breakpoints first (click line numbers)
- Then press F5

### Can't find package
- Load it: `library(packagename)`
- Or check: `installed.packages()`

## 📚 Resource Links

- [R Language Server GitHub](https://github.com/REditorSupport/languageserver)
- [tidyverse Documentation](https://www.tidyverse.org)
- [ggplot2 Reference](https://ggplot2.tidyverse.org)
- [RMarkdown Guide](https://rmarkdown.rstudio.com)

## 📊 System Information

- **R Version**: 4.5.1
- **R Path**: /usr/bin/R
- **Platform**: Linux
- **Package Manager**: renv
- **IDE**: VS Code Server

## ✨ Features Enabled

- ✅ IntelliSense & Autocomplete
- ✅ Real-time Diagnostics
- ✅ Hover Documentation
- ✅ Go to Definition
- ✅ Find References
- ✅ Debugging with Breakpoints
- ✅ Code Formatting
- ✅ Code Linting
- ✅ Interactive Graphics
- ✅ Terminal Integration

## 🚀 Next Steps

1. **Open a file**: `Ctrl+P` and search for a `.R` file
2. **Run code**: `Ctrl+Shift+Enter` on any line
3. **Use autocomplete**: `Ctrl+Space` while typing
4. **Explore features**: Use F12 to jump to definitions
5. **Set up debugging**: Click line numbers for breakpoints

---

**All features are ready to use in VS Code Server (both local and remote).**

Happy coding! 🎉
