#!/usr/bin/env bash
# Compile GLSL shaders to SPIR-V
# Requires: glslangValidator (from Vulkan SDK) or glslc (from shaderc)
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

compile() {
    local src="$1"
    local base="${src%.*}"
    local ext="${src##*.}"
    # Output: name_vert.spv, name_frag.spv, etc.
    # Exception: vertex shaders also get a plain name.spv for backwards compat
    local dst="${base}_${ext}.spv"

    if command -v glslangValidator &>/dev/null; then
        glslangValidator -V "$src" -o "$dst"
    elif command -v glslc &>/dev/null; then
        glslc "$src" -o "$dst"
    else
        echo "ERROR: No SPIR-V compiler found. Install Vulkan SDK or shaderc."
        exit 1
    fi
    echo "Compiled: $src -> $dst"
}

cd "$SCRIPT_DIR"

for shader in *.vert *.frag *.comp *.geom *.tese *.tesc; do
    [ -f "$shader" ] && compile "$shader"
done

echo "All shaders compiled."
