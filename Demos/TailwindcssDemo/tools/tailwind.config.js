// tools/tailwind.config.js
module.exports = {
  content: [
    "./templates/**/*.html",
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