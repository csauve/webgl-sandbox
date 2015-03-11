precision highp float;
const float EPSILON = 0.0001;
const float GRADIENT_STEP = 0.01;
const float MAX_DISTANCE = 1000.0;
const int MAX_REFLECTIONS = 3;
const int MAX_STEPS = 256;
const vec4 BACKGROUND_COLOUR = vec4(0);
const vec4 AMBIENT_COLOUR = vec4(0.25, 0.35, 0.45, 1.0);
const vec4 BLACK = vec4(vec3(0.0), 1.0);
const vec4 LIGHT_COLOUR = vec4(0.9, 0.8, 0.6, 1.0);
const vec3 LIGHT_POS = vec3(2.0, 5.0, 4.0);

uniform vec2 canvasSize;
uniform float timeSec;

vec3 hash3(float n) {
  return fract(sin(vec3(n, n + 1.0, n + 2.0)) * 43758.5453123);
}

//operations
vec3 translate(vec3 point, vec3 offset) {
  return point - offset;
}

vec3 repeat(vec3 point, vec3 c) {
  return mod(point, c) - 0.5 * c;
}

//distance functions
float sphere(vec3 point, float radius) {
  return max(0.0, length(point) - radius);
}

float box(vec3 point, vec3 size) {
  vec3 d = abs(point) - size;
  return min(max(d.x, max(d.y, d.z)), 0.0) + length(max(d, 0.0));
}

vec3 circlePath(float y, float radius, float time) {
  return vec3(sin(time) * radius, y, cos(time) * radius);
}

float sceneDistance(vec3 point) {
  vec3 spherePos = circlePath(1.0, 2.0, timeSec / 2.0);
  vec3 miniPos = translate(circlePath(0.0, 2.3, timeSec), -spherePos);
  vec3 wavePos = translate(point, vec3(0.0, 0.1 * sin((timeSec + point.z) * 2.0), 0.0));
  float boxGapSize = sin(timeSec) * 0.04;
  float planets = min(
    sphere(translate(point, spherePos), 1.0),
    sphere(translate(point, miniPos), 0.3)
  );
  float boxes = max(
    box(translate(wavePos, vec3(0.0, -1.0, 0.0)), vec3(5.0, 1.0, 5.0)),
    box(repeat(wavePos, vec3(0.25 + boxGapSize)), vec3(0.1))
  );
  return min(
    planets,
    boxes
  );
}

float march(vec3 rayOrigin, vec3 rayDir) {
  float rayLength = 0.0;
  for (int i = 0; i < MAX_STEPS; i++) {
    float dist = sceneDistance(rayOrigin + rayLength * rayDir);
    rayLength += dist;
    if (dist < EPSILON) return rayLength;
    if (dist >= MAX_DISTANCE) break;
  }
  return -1.0;
}

float rayOcclusion(vec3 rayOrigin, vec3 rayDest, float k) {
  vec3 rayDir = normalize(rayDest - rayOrigin);
  float rayLength = EPSILON * 100.0;
  float maxRayLength = length(rayDest - rayOrigin);
  float occlusion = 1.0;

  for (int i = 0; i < 512; i++) {
    float distToScene = sceneDistance(rayOrigin + rayDir * rayLength);
    if (distToScene < EPSILON) return 0.0;

    occlusion = min(occlusion, k * distToScene / rayLength);
    rayLength += distToScene;

    if (rayLength > maxRayLength) break;
  }
  return occlusion;
}

float ambientOcclusion(vec3 surfacePos, vec3 surfaceNormal, float stepSize) {
  float rayLength = stepSize;
  float occlusion = 0.0;
  for (int i = 0; i < 10; i++) {
    float distToScene = sceneDistance(surfacePos + surfaceNormal * rayLength);
    occlusion += rayLength - distToScene;
    rayLength += stepSize;
  }
  return 1.0 - clamp(occlusion, 0.0, 1.0);
}

vec3 gradient(vec3 point, float scale) {
  return normalize(vec3(
    sceneDistance(point + vec3(scale, 0, 0)) - sceneDistance(point - vec3(scale, 0, 0)),
    sceneDistance(point + vec3(0, scale, 0)) - sceneDistance(point - vec3(0, scale, 0)),
    sceneDistance(point + vec3(0, 0, scale)) - sceneDistance(point - vec3(0, 0, scale))
  ));
}

vec4 shade(vec3 surfacePos, vec3 cameraPos, int numReflections) {
  vec3 surfaceNormal = gradient(surfacePos, EPSILON * 50.0);
  float lightDist = length(LIGHT_POS - surfacePos);
  float lightPower = clamp(10.0 / (lightDist * lightDist), 0.0, 1.0);
  float lightOcclusion = rayOcclusion(surfacePos, LIGHT_POS, 12.0);
  float ambientOcclusion = ambientOcclusion(surfacePos, surfaceNormal, 0.02);

  float lightIntensity = 0.0;
  if (lightOcclusion > 0.0) {
    vec3 lightDir = normalize(LIGHT_POS - surfacePos);
    lightIntensity = lightPower * lightOcclusion * clamp(dot(surfaceNormal, lightDir), 0.0, 1.0);
  }

  vec4 reflectionColour = BLACK;
  if (numReflections > 0) {
    vec3 incidentRay = normalize(surfacePos - cameraPos);
    //reflectionColour = render(surfacePos, reflect(incidentRay, surfaceNormal), numReflections - 1);
  }

  vec4 surfaceColour = mix(AMBIENT_COLOUR * ambientOcclusion, LIGHT_COLOUR, lightIntensity);

  return mix(surfaceColour, reflectionColour, 0.1);
}

vec4 render(vec3 rayOrigin, vec3 rayDir, int numReflections) {
  float rayLength = march(rayOrigin, rayDir);
  vec4 surfaceColour = BACKGROUND_COLOUR;
  if (rayLength != -1.0) {
    vec3 surfacePos = rayOrigin + rayDir * rayLength;
    surfaceColour = shade(surfacePos, rayOrigin, numReflections);
  }
  return surfaceColour;
}

void main() {
  //camera setup
  vec3 cameraPos = circlePath(2.0, 4.0, -timeSec / 5.0);
  vec3 cameraLookAtPos = vec3(0, 0, 0);
  vec3 cameraUpDir = vec3(0, 1, 0);
  vec3 cameraLookDir = normalize(cameraLookAtPos - cameraPos);

  //screen setup
  vec2 screenCoord = gl_FragCoord.xy / canvasSize.xy - 0.5;
  vec3 screenXDir = normalize(cross(cameraUpDir, cameraLookDir));
  vec3 screenYDir = cross(cameraLookDir, screenXDir);
  vec3 screenOrigin = cameraPos + cameraLookDir;

  //ray setup
  vec3 rayOrigin = screenOrigin +
      screenCoord.x * screenXDir * canvasSize.x / canvasSize.y +
      screenCoord.y * screenYDir;
  vec3 rayDir = normalize(rayOrigin - cameraPos);

  vec3 noise = (10.0 / 255.0) * hash3(screenCoord.x + 13.0 * screenCoord.y + timeSec);
  gl_FragColor = render(rayOrigin, rayDir, MAX_REFLECTIONS) + vec4(noise, 0.0);
}
