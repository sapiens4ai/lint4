export const unquote = (s) => s.replace(/"/g, "");
export const unslash = (s) => s.replace(/\/\//g, "/");
export const half = (n) => n / 2;
export const later = (f) => () => setTimeout(f, 100);
