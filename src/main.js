import { Elm } from "./App/Main.elm";

function toggleDarkMode(darkMode) {
  const { documentElement } = document;
  if (darkMode) {
    documentElement.classList.add("dark");
  } else {
    documentElement.classList.remove("dark");
  }
}

const settings = JSON.parse(localStorage.getItem("settings")) || {
  darkMode: false,
  language: "en"
};

const app = Elm.Main.init({
  node: document.getElementById("app"),
  flags: {
    settings
  }
});

toggleDarkMode(settings.darkMode);

app.ports.settings.subscribe(function(settings) {
  toggleDarkMode(settings.darkMode);
  localStorage.setItem("settings", JSON.stringify(settings));
});
