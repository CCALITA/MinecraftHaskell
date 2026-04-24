#version 450

layout(binding = 0) uniform UniformBufferObject {
    mat4 model;
    mat4 view;
    mat4 projection;
    vec4 sunDirection;    // xyz = sun direction, w = unused
    float ambientLight;   // 0.0-1.0 overall brightness
    float time;           // elapsed time in seconds
    float fogStart;       // distance where fog begins
    float fogEnd;         // distance where fog is fully opaque
    vec4 fogColor;        // fog / horizon color
    float underwater;     // 1.0 when camera is submerged
} ubo;

layout(location = 0) in vec3 fragDir;

layout(location = 0) out vec4 outColor;

void main() {
    // Reconstruct world-space view direction from inverse view-projection
    mat4 invViewProj = inverse(ubo.projection * ubo.view);
    vec4 worldDir = invViewProj * vec4(fragDir.xy, 1.0, 1.0);
    vec3 dir = normalize(worldDir.xyz / worldDir.w);

    // Elevation angle: positive = up, negative = below horizon
    // Vulkan NDC has Y flipped, so dir.y from inverse VP is already correct
    float elevation = dir.y;

    // Horizon color comes from fogColor (already computed from day/night cycle)
    vec3 horizon = ubo.fogColor.rgb;

    // Zenith color: brighter / more saturated version of the horizon
    // Shift toward blue sky and increase brightness
    vec3 zenith = horizon * 0.7 + vec3(0.15, 0.25, 0.45);
    zenith = min(zenith, vec3(1.0));

    // Smooth blend from horizon to zenith based on elevation
    // clamp to [0,1] — below horizon stays at horizon color
    float t = clamp(elevation, 0.0, 1.0);
    // Use smoothstep for a gentle transition
    t = smoothstep(0.0, 0.6, t);

    vec3 skyColor = mix(horizon, zenith, t);

    // Below-horizon fade: darken slightly to suggest ground
    if (elevation < 0.0) {
        float belowT = clamp(-elevation * 3.0, 0.0, 1.0);
        vec3 groundColor = horizon * 0.6;
        skyColor = mix(horizon, groundColor, belowT);
    }

    outColor = vec4(skyColor, 1.0);
}
