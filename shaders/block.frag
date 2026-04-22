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

layout(binding = 1) uniform sampler2D texSampler;

layout(location = 0) in vec2 fragTexCoord;
layout(location = 1) in vec3 fragNormal;
layout(location = 2) in float fragAO;
layout(location = 3) in vec3 fragWorldPos;
layout(location = 4) in float fragAmbient;

layout(location = 0) out vec4 outColor;

void main() {
    // Animated texture offset for water and lava
    // Atlas layout: 16x16 tiles, each tile = 1/16 (0.0625) of atlas
    // Water tile: column 13, row 0 -> u in [0.8125, 0.875]
    // Lava  tile: column 14, row 0 -> u in [0.875, 0.9375]
    vec2 tc = fragTexCoord;
    float tileSize = 1.0 / 16.0;

    // Detect water tile
    bool isWater = tc.x >= 13.0 * tileSize && tc.x < 14.0 * tileSize
                && tc.y >= 0.0 && tc.y < tileSize;

    // Detect lava tile
    bool isLava = tc.x >= 14.0 * tileSize && tc.x < 15.0 * tileSize
               && tc.y >= 0.0 && tc.y < tileSize;

    if (isWater) {
        // Gentle flowing offset that wraps within the tile
        float localU = tc.x - 13.0 * tileSize;
        float localV = tc.y;
        localU += sin(ubo.time * 1.2 + localV * 12.0) * 0.008;
        localV += sin(ubo.time * 0.8 + localU * 10.0) * 0.006;
        // Wrap within tile boundaries
        localU = fract(localU / tileSize) * tileSize;
        localV = fract(localV / tileSize) * tileSize;
        tc = vec2(13.0 * tileSize + localU, localV);
    } else if (isLava) {
        // Slower, more turbulent offset for lava
        float localU = tc.x - 14.0 * tileSize;
        float localV = tc.y;
        localU += sin(ubo.time * 0.6 + localV * 8.0) * 0.012;
        localV += sin(ubo.time * 0.4 + localU * 6.0) * 0.010;
        // Wrap within tile boundaries
        localU = fract(localU / tileSize) * tileSize;
        localV = fract(localV / tileSize) * tileSize;
        tc = vec2(14.0 * tileSize + localU, localV);
    }

    // Sample texture atlas with (possibly animated) coordinates
    vec4 texColor = texture(texSampler, tc);

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
