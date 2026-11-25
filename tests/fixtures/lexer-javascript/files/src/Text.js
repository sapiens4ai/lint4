export const unquote = (s) => s.replace(/"/g, "");
export const unslash = (s) => s.replace(/\/\//g, "/");
export const half = (n) => n / 2;
export const later = (f) => () => setTimeout(f, 100);
export const f = `${setTimeout(g, 100)}`;
export const h = `a${ {k: `${setInterval(t, 5)}`}.k }b`;
export const note = `setImmediate(x) ${"}"} done`;
