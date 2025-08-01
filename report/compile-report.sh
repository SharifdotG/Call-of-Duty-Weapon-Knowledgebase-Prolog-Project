#!/bin/bash

# LaTeX Report Compilation Script
# Compiles the lab report from LaTeX source to PDF

echo "=== Call of Duty Weapon Knowledgebase Report Compilation ==="
echo ""

# Check if pdflatex is available
if ! command -v pdflatex &> /dev/null; then
    echo "Error: pdflatex not found. Please install a LaTeX distribution."
    echo "Recommended: MiKTeX (Windows) or TeX Live (Linux/Mac)"
    exit 1
fi

echo "Compiling lab-report.tex to PDF..."
echo ""

# Compile the LaTeX document
# Run twice to ensure proper cross-references and table of contents
pdflatex -interaction=nonstopmode lab-report.tex
pdflatex -interaction=nonstopmode lab-report.tex

# Check if compilation was successful
if [ -f "lab-report.pdf" ]; then
    echo "✅ Compilation successful!"
    echo "📄 Output: lab-report.pdf"
    echo ""
    echo "The report includes:"
    echo "  - Beautiful front page matching the template"
    echo "  - Complete project documentation"
    echo "  - Sample input/output examples"
    echo "  - Technical analysis and conclusions"
    echo ""
    echo "Ready for submission! 🎉"
else
    echo "❌ Compilation failed!"
    echo "Please check the LaTeX source for errors."
    exit 1
fi

# Clean up auxiliary files (optional)
read -p "Clean up auxiliary files? (y/n): " cleanup
if [ "$cleanup" = "y" ] || [ "$cleanup" = "Y" ]; then
    rm -f *.aux *.log *.out *.toc *.fls *.fdb_latexmk
    echo "🧹 Auxiliary files cleaned up."
fi
