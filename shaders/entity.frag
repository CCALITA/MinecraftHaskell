#version 450

layout(location = 0) in vec4 fragColor;
layout(location = 0) out vec4 outColor;

void main() {
    // Discard fully transparent fragments
    if (fragColor.a < 0.01) {
        discard;
    }
    outColor = fragColor;
}
