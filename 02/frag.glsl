#version 320 es
out lowp vec4 FragColor;

in lowp vec3 ourColor;

void
main (void)
  {
    FragColor = vec4 (ourColor, 1.0);
  }
