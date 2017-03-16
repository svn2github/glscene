uniform sampler2D decalMap;
uniform sampler2D normalMap;

varying vec2 uv;
varying vec3 lightVec;
varying vec3 eyeVec;

const float diffuseCoeff = 0.7;
const float specularCoeff = 0.5;

void main()
{
    // fetch normal from normal map and normalize
    vec3 normal = normalize(texture2D(normalMap, uv).rgb - 0.5);

    // compute diffuse lighting
    float diffuse = max(dot(lightVec, normal), 0.0) * diffuseCoeff;
    vec3 decalColor = texture2D(decalMap, uv).rgb;

    // compute specular lighting
    vec3 eye = normalize(eyeVec);
    float specular = clamp(dot(eye, normal), 0.0, 1.0);
    specular = pow(specular, 32.0) * specularCoeff;

    // output final color
    gl_FragColor = vec4(vec3(diffuse) * decalColor + vec3(specular), 1.0);
}
