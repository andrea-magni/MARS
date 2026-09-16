// tools/tailwind.config.js
module.exports = {
  content: [
    "./templates/**/*.html",
    // Tailwind only emits the classes it can actually see, and it scans any file
    // as plain text - so every source that names a utility class has to be listed:
    "./www/js/ui.js",          // classes toggled at run time
    "./Server.Web.Models.pas", // NAV_* constants returned to the templates
  ],
  theme: {
    extend: {
      colors: {
        brand: {
          50: "#eff6ff",
          500: "#2563eb",
          600: "#1d4ed8",
          700: "#1d4ed8"
        }
      }
    }
  },
  plugins: []
};