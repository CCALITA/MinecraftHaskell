#version 450

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec2 fragTexCoord;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in float fragAO;
layout(location = 3) in vec3 fragWorldPos;

layout(location = 0) out vec4 outColor;

void main() {
    // Sample texture atlas
    vec4 texColor = texture(texSampler, fragTexCoord);

    // Simple directional light (sun from above-right)
    vec3 lightDir = normalize(vec3(0.3, 1.0, 0.5));
    float diffuse = max(dot(normalize(fragNormal), lightDir), 0.0);

    // Ambient + diffuse lighting
    float ambient = 0.4;
    float light = ambient + diffuse * 0.6;

    // Apply ambient occlusion
    light *= fragAO;

    outColor = vec4(texColor.rgb * light, texColor.a);
}
