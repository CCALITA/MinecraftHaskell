#version 450

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 projection;
    vec4 sunDirection;
    float ambientLight;
    float _pad1;
    float _pad2;
    float _pad3;
} ubo;

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec2 fragTexCoord;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in float fragAO;
layout(location = 3) in vec3 fragWorldPos;
layout(location = 4) in float fragAmbient;

layout(location = 0) out vec4 outColor;

void main() {
    // Sample texture atlas
    vec4 texColor = texture(texSampler, fragTexCoord);

    // Directional light from sun (uses actual sun direction from UBO)
    vec3 lightDir = normalize(ubo.sunDirection.xyz);
    float diffuse = max(dot(normalize(fragNormal), lightDir), 0.0);

    // Ambient + diffuse lighting, scaled by day/night ambient level
    float ambient = 0.3 * fragAmbient;
    float light = ambient + diffuse * 0.7 * fragAmbient;

    // Apply per-vertex light level (from block/sky light propagation)
    light *= fragAO;

    // Minimum visibility so player can always see something
    light = max(light, 0.05);

    outColor = vec4(texColor.rgb * light, texColor.a);
}
