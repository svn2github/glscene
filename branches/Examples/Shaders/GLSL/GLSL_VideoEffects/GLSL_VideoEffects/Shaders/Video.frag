const vec3 lumCoeff = vec3 (0.2125, 0.7154, 0.0721);
const vec3 AvgLuminance = vec3 (0.5, 0.5, 0.5);

uniform sampler2D u_texture;
uniform float brightness;
uniform float saturation;
uniform float contrast;

void main(void)
{
    vec3 color = vec3(texture2D(u_texture, gl_TexCoord[0].st));
    vec3 intensity = vec3(dot(color, lumCoeff));

    color = mix(intensity, color, saturation);
    color = color * brightness;
    color = mix(AvgLuminance, color, contrast);

    gl_FragColor.rgb = color;
    gl_FragColor.a = 1.0;

}

