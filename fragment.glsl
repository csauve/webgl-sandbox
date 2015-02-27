precision highp float;
float EPSILON = 0.00001;
float MAX_DISTANCE = 100.0;
vec4 BACKGROUND_COLOUR = vec4(vec3(0.3), 1);

uniform vec2 canvasSize;
uniform float timeMs;

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

float plane(vec3 point, float y) {
  return point.y - y;
}

float sceneDistance(vec3 point) {
  vec3 ballPos = translate(point, vec3(0, 1.0 + 0.5 * sin(timeMs / 2000.0), 0));
  vec3 repeatedBallPos = repeat(ballPos, vec3(3));
  float ballsDist = sphere(repeatedBallPos, 0.5);
  float ceilingDist = plane(point, 0.0);
  float floorDist = plane(point, -2.0);

  vec3 pitPos = translate(point, vec3(
    sin(timeMs / 1000.0) * 0.3 * sin(point.z),
    -2.0,
    sin(timeMs / 2000.0) * 0.3 * sin(point.x)));
  vec3 repeatedPitPos = repeat(pitPos, vec3(0.2));
  float pitsDist = sphere(repeatedPitPos, 0.1);

  return min(
    max(floorDist, pitsDist),
    max(ceilingDist, ballsDist)
  );
}

vec2 march(vec3 rayOrigin, vec3 rayDir, float k) {
  float rayLength = 1000.0 * EPSILON;
  float occlusion = 1.0;
  for (int i = 0; i < 256; i++) {
    float dist = sceneDistance(rayOrigin + rayLength * rayDir);
    occlusion = min(occlusion, 1.0 * min(1.0, dist / rayLength));
    rayLength += dist;
    if (dist < EPSILON || rayLength > MAX_DISTANCE) break;
  }
  return vec2(rayLength, occlusion);
}

vec3 circlePath(float y, float radius, float time) {
  return vec3(sin(time) * radius, y, cos(time) * radius);
}

void main() {
  //lighting
  vec3 lightPos = circlePath(1.0, 2.0, timeMs / 1000.0);

  //camera setup
  vec3 cameraPos = circlePath(1.0, 3.0, -timeMs / 10000.0);
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

  //march ray to surface
  float rayLength = march(rayOrigin, rayDir, 1.0).x;
  vec3 surfacePos = rayOrigin + rayDir * rayLength;

  //colour the surface
  vec4 surfaceColour = BACKGROUND_COLOUR;
  if (rayLength <= MAX_DISTANCE) {
    float lightDistActual = length(lightPos - surfacePos);
    float lightPower = min(1.0, 20.0 / (lightDistActual * lightDistActual));

    vec2 lightMarch = march(surfacePos, normalize(lightPos - surfacePos), 1.0);

    float tone = lightMarch.y;

    surfaceColour = vec4(min(vec3(tone), 1.0), 1.0);
  }

  gl_FragColor = surfaceColour;
}