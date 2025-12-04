# R Language Studio - VS Code Configuration Guide

## Setup Overview

Your VS Code environment is now configured with **full R language support** including:

### ✅ Installed Components

#### 1. **VS Code Extensions**
- **REditorSupport.r** (v2.8.6) - Main R language support
- **RDebugger.r-debugger** (v0.5.6) - Debugging capabilities
- **REditorSupport.r-syntax** (v0.1.3) - Syntax highlighting

#### 2. **R Language Server**
- **languageserver** - Protocol for VS Code communication
- **jsonlite** - JSON parsing for language server
- **httpgd** - Web-based graphics device

#### 3. **Core R Packages**
- **devtools** - Package development tools
- **rmarkdown** - Report generation
- **knitr** - Dynamic documents
- **testthat** - Unit testing framework
- **styler** - Code formatting
- **lintr** - Code linting
- **roxygen2** - Documentation generation

#### 4. **Data Science Stack**
- **tidyverse** - Data manipulation ecosystem
- **dplyr** - Data manipulation
- **tidyr** - Data tidying
- **ggplot2** - Visualization
- **plotly** - Interactive plots
- **data.table** - Fast data operations

---

## 🚀 Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| `Ctrl+Shift+Enter` | Run current line |
| `Ctrl+Shift+S` | Run selection |
| `Ctrl+Shift+A` | Run all lines |
| `Alt+-` | Insert `<-` operator |
| `Ctrl+Shift+M` | Insert `\|>` pipe operator |
| `Ctrl+L` | Clear console |
| `F2` | Rename symbol |
| `Ctrl+K Ctrl+0` | Fold all regions |
| `Ctrl+Shift+P` | Command palette |

---

## 💻 VS Code Features Enabled

### 1. **IntelliSense & Autocomplete**
- Function signatures and parameter hints
- Package namespace completion (e.g., `dplyr::`)
- Object and variable completion
- Snippet suggestions

### 2. **Code Navigation**
- Go to Definition (`F12`)
- Find All References (`Shift+Alt+F12`)
- Document Symbols (`Ctrl+Shift+O`)
- Workspace Symbols (`Ctrl+T`)

### 3. **Debugging**
- Breakpoints
- Step through code
- Watch variables
- Debug mode: `F5` or `Ctrl+Shift+F10`

### 4. **Code Quality**
- Real-time diagnostics
- Linting with **lintr**
- Formatting with **styler**
- Code actions for fixes

### 5. **Documentation**
- Hover for function documentation
- Inline help (`F1`)
- HTML help pages (clickable)
- Parameter documentation

### 6. **Visualization**
- Graphics preview in sidebar
- **httpgd** web-based plotting
- Interactive graphics support
- Plot window integrated into editor

### 7. **Project Management**
- File explorer
- Git integration
- Package structure recognition
- Workspace symbols

---

## 📝 Useful Commands in VS Code

Open Command Palette (`Ctrl+Shift+P`) and type:

| Command | Description |
|---------|-------------|
| `R: Create R Terminal` | Start R in terminal |
| `R: Run Selection` | Execute selected code |
| `R: Run Entire File` | Run whole script |
| `R: Clear Console` | Clear R console |
| `R: Help on Topic` | Get help for current selection |
| `R: View Plot History` | Show plot history |
| `R: Show Plot Device` | Display graphics device |

---

## 🔧 Custom R Functions

In R console, you have access to quick utilities:

```r
# Load packages silently
lib(dplyr, ggplot2, data.table)

# Check package information
pkginfo("dplyr")  # Find packages matching pattern

# Check if installed
is_installed("tidyverse")

# Install if missing
install_if_missing(shiny, plotly)
```

---

## 📁 File Structure

```
.vscode/
├── settings.json        # VS Code R configuration
├── launch.json          # Debugger configuration
├── keybindings.json     # Custom keyboard shortcuts
└── extensions.json      # Recommended extensions

.Rprofile              # R environment initialization
renv/                  # Project dependency management
├── activate.R
└── library/
```

---

## ⚙️ Configuration Details

### Settings Enabled

**Language Server Protocol (LSP)**
- ✅ Diagnostics enabled
- ✅ Hover information
- ✅ Completion
- ✅ Definition lookup
- ✅ Document symbols
- ✅ Code actions
- ✅ Formatting

**Plotting**
- ✅ httpgd graphics device
- ✅ 800x600 default resolution
- ✅ Web-based rendering

**Debugging**
- ✅ Break on error enabled
- ✅ Console evaluation
- ✅ Step debugging

**Code Quality**
- ✅ Format on save
- ✅ Auto-indentation
- ✅ Bracket matching
- ✅ Diagnostics: always shown

---

## 🎯 Recommended Workflow

### 1. **Development**
```r
# Create a new script in Workshops/
# Use Ctrl+Shift+Enter to run lines interactively
# Use F12 to jump to function definitions
# Use Ctrl+Shift+R to run selection
```

### 2. **Debugging**
```r
# Set breakpoint: click line number
# Press F5 to debug current file
# Use Debug console to inspect variables
# Step through with F10/F11
```

### 3. **Formatting**
```r
# Auto-format on save (configured)
# Or: Ctrl+Shift+P → "Format Document"
# Uses styler for consistent style
```

### 4. **Testing**
```r
# Create tests with testthat
# Run with Ctrl+Shift+T in test file
# View results in console
```

---

## 📚 Documentation

### Access Help
- **Hover over function**: Shows signature
- **F1 on function**: Opens help page
- **?function_name**: In console shows help
- **??pattern**: Searches all documentation

### Inline Documentation
- Press `Ctrl+K Ctrl+I` for quick info
- Hover over parameters to see types
- Function signatures auto-complete

---

## 🐛 Troubleshooting

### Issue: Language Server not responding
```r
# Solution: Restart R session
Sys.getenv("R_PROFILE_USER")  # Verify .Rprofile location
# Then: Ctrl+Shift+P → "R: Restart R Session"
```

### Issue: Packages not found
```r
# Solution: Check library paths
.libPaths()
# Install to user library:
install.packages("package_name")
```

### Issue: Plots not displaying
```r
# Solution: Ensure httpgd is available
library(httpgd)
# If issues persist, switch to default graphics
# in settings: "r.plot.useHttpgd": false
```

---

## 🔗 Useful Resources

- **R Language Server**: https://github.com/REditorSupport/languageserver
- **tidyverse**: https://www.tidyverse.org
- **ggplot2**: https://ggplot2.tidyverse.org
- **rmarkdown**: https://rmarkdown.rstudio.com

---

## ✨ Next Steps

1. **Test the setup**: Open a `.R` file and run `Ctrl+Shift+Enter` on a line
2. **Create a project**: Use the Python script `run_labs.py` to organize materials
3. **Explore features**: Try hovering over functions, using autocomplete, setting breakpoints
4. **Customize**: Modify `.vscode/settings.json` for your preferences

---

**Happy Coding! 🚀**
