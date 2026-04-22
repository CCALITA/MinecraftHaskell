#version 450

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 projection;
    vec4 sunDirection;
    float ambientLight;
    float time;
    float _pad2;
    float _pad3;
} ubo;

layout(location = 0) in vec3 inPosition;   // world-space billboard vertex
layout(location = 1) in vec4 inColor;       // RGBA color

layout(location = 0) out vec4 fragColor;

void main() {
    gl_Position = ubo.projection * ubo.view * ubo.model * vec4(inPosition, 1.0);
    // Apply ambient light to entity color
    float light = max(ubo.ambientLight, 0.15);
    fragColor = vec4(inColor.rgb * light, inColor.a);
}
