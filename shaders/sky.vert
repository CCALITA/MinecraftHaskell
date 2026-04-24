#version 450
layout(location = 0) out vec3 fragDir;
void main() {
    vec2 positions[3] = vec2[3](vec2(-1,-1), vec2(3,-1), vec2(-1,3));
    gl_Position = vec4(positions[gl_VertexIndex], 0.999, 1.0);
    fragDir = vec3(positions[gl_VertexIndex], 1.0);
}
